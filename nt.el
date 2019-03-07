;;; nt.el --- Program with Personalized Notation -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/ekaschalk/nt
;; Version: 0.1
;; Keywords: indentation, display, notes, major-modes
;; Package-Requires: ((cl "1.0") (dash "2.14.1") (dash-functional "1.2.0") (s "1.12.0") (emacs "26.1"))

;;; Commentary:

;; Notate your programs with indentation-correcting visual replacements of
;; symbols with other symbols.

;; Ligature's generalization, known to Emacs as ~prettified-symbols~, causes
;; alignment and indentation issues, preventing one from rendering ~for~ as ~∀~
;; and ~int~ as ~ℤ~ in their code

;; With notate, you can selectively APL-ize your code.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-bounds)
(require 'nt-mask)
(require 'nt-note)
(require 'nt-ov)
(require 'nt-tree)

;;; Configuration
;;;; Core

(defvar nt-notes (nt-notes--make '(("hello" "∧") ("bye" "!∨")))
  "plist of note specifications resulting from `nt-notes--make'.")

(defvar-local nt-bound-fn #'nt-bounds--lisps
  "A function that should return line boundaries [a b) given a NOTE.")

(defvar-local nt-bound?-fn #'nt-bounds?--lisps
  "A function that should return whether a given NOTE modifies indentation.")

(defvar nt-normalize-height? nil
  "Force notated buffer's line heights to match true buffer?

For notated/true-buffer to be viewable side-by-side, this must be
true. However depending on font for note, it might be rendered
smaller than normal and widths might not match (so visual
indentation might be slightly off even with correct spaces).

A (possible) solution is to use 'specified spaces' (see emacs manual).

The problem is complex enough as is, so while a significant
upgrade, not the #1 priority at time of writing.

For development purposes, it is recommended to have false to
visually compare indentation more reliably.

For screenshot purposes, it is recommended to have true for
side-by-side comparisons to be aligned.")

;;;; Debugging

(defvar nt-display-prefixes? t
  "Whether to add the 'line-prefix property to indentation overlays.")

(defvar nt-display-render-status? t
  "Whether to add a 'face property to rendered indentation overlays.")

(defvar nt-render-masks? t
  "Should masks render? Note that line-prefixes, if set to, still display.")

;;;; Managed
;;;;; Core

(defvar-local nt-tree nil
  "Manage notes in a `hierarchy' tree based on interval-containment.

---
Roots are non-overlapping line-intervals with P-C relationship defined as:
  - A note n_p is a parent of note n_c iff bound(n_p) contains bound(n_c).
  - If bound(n_p) == bound(n_c), the first note by buffer position is the parent.

---
Notes are sorted as follows (see the cmp fn `nt-tree--note<'):
  - A child is smaller than its parents.
  - Comparing two separate subtrees: the first occurring note is smaller.

---
So the ordering looks like:
  - Sort roots by buffer position.
  - Insert before each root its children ordered by increasing size.

This ordering is maintained for optimized parent/child lookup.")

;; NOTE transfer to tree based implementation in-progress
(defvar nt-note-list nil
  "List of note overlays currently managed.")

;; NOTE This will be converted into a vector soon for constant-time idxing
(defvar nt-mask-list nil
  "List of indent overlays currently managed.

This an ordered list s.t. nt-mask-list[i] = mask-at-line-i+1.

Accessing this should be done through `nt-mask--at' and friends to avoid
confusing indexings.")

;;;;; Transitory

(defvar nt-mask--wait-for-refresh nil
  "Let-bind true to hold off on refreshing masks during batch modifications.")

;;; Note-Mask Interactions

(defun nt--masks-for (note)
  "Return all masks NOTE contributes to."
  (-some->>
   note
   (funcall (symbol-value #'nt-bound?-fn))
   (funcall (symbol-value #'nt-bound-fn))
   (apply #'nt-masks--in)
   (-remove (-partial #'nt-mask--contains? note))))

(defun nt--map-over-masks (fn note)
  "Map FN partially applied on NOTE over masks for NOTE."
  (->> note nt--masks-for (-map (-partial fn note))))

(defun nt--add-note-to-mask (note mask)
  "Add NOTE to a MASK, possibly refresh mask, and return back mask."
  (push note (overlay-get mask 'nt-notes))
  (nt-mask--refresh-maybe mask))

(defun nt--remove-note-from-mask (note mask)
  "Remove NOTE from MASK, possibly refresh mask, and return back mask."
  (delq note (overlay-get mask 'nt-notes))
  (nt-mask--refresh-maybe mask))

(defun nt--add-note-to-masks (note)
  "Add NOTE to all masks it contributes to and return them."
  (nt--map-over-masks #'nt--add-note-to-mask note))

(defun nt--remove-note-from-masks (note)
  "Remove NOTE from all masks it contributes to."
  (nt--map-over-masks #'nt--remove-note-from-mask note))

(defun nt--add-notes-to-masks (notes)
  "Batch add NOTES to their masks refreshing upon completion."
  ;; (let ((nt-mask--wait-for-refresh t))
  ;;   (-each notes #'nt--add-note-to-masks))

  ;; (nt-masks--refresh-buffer)

  ;; TODO Test this implementation compared to simpler version above
  (let ((masks))
    (let ((nt-mask--wait-for-refresh t))
      (setq masks (-mapcat #'nt--add-note-to-masks notes)))
    (-> masks -distinct nt-masks--refresh)))

(defun nt--delete-notes (notes)
  "Delete NOTES and refresh the masks they contributed to."
  ;; SEE `nt-alg', implementation is substanitally more complex when avoiding
  ;; running boundary functions and batch deleting notes
  (-doto note
    (nt--remove-note-from-masks)
    (nt-note--delete)))

;;; Change Functions
;;;; Utils

(defun nt-change--line-diff ()
  "Lines added: +x, removed: -x, otherwise 0 since mask list last updated."
  ;; NOTE 1- point-max easier than calling skip-line on last-line's mask
  (- (line-number-at-pos (1- (point-max)))
     (length nt-mask-list)))

(defun nt-change--new-lines? ()
  "Return count of lines added since last update or nil."
  (let ((line-change (nt-change--line-diff)))
    (when (> line-change 0)
      line-change)))

(defun nt-change--removed-lines? ()
  "Return count of lines removed since last update or nil."
  (let ((line-change (nt-change--line-diff)))
    (when (< line-change 0)
      (- line-change))))

;;;; Insertion

(defun nt-change--insertion (start end)
  "Change function specialized for insertion, in START and END."
  (-when-let (new-lines (nt-change--new-lines?))
    (let* ((end-line (1+ (line-number-at-pos)))
           (start-line (- end-line new-lines))
           (line-before-change (1- start-line))

           ;; Must init masks ASAP for `nt-mask-list' integrity
           (masks
            ;; (nt-masks--init start-line end-line)
            (-map #'nt-mask--init
                  (number-sequence start-line (1- end-line)))
            )

           (mask-before-change (nt-mask--at line-before-change))
           (notes-before-change (overlay-get mask-before-change 'nt-notes))

           (notes (-union notes-before-change
                          (nt-notes--at line-before-change))))

      ;; The note is being added to mask but it takes 2 insertions to "kick in"
      ;; for some reason?
      ;; (nt--add-notes-to-masks notes)
      (-each notes #'nt--add-note-to-masks)

      ;; (message "%s %s" start end)
      )))

;;;; Deletion

(defun nt-change--deletion (pos chars-deleted)
  "Change function specialized for deletion, number CHARS-DELETED at POS."
  ;; (message "Deleting at pos %s, %s characters" pos chars-deleted)


  )

;;;; Hook

(defun nt-after-change-function (start end chars-deleted)
  "See `after-change-functions'."
  (if (= 0 chars-deleted)
      (nt-change--insertion start end)
    (nt-change--deletion start chars-deleted)))

;;; Interactive
;;;; Setup
;;;;; Proper

(defun nt-setup--agnostic ()
  "Setup all *major-mode-agnostic* components."
  (nt-tree--init)
  (nt-masks--init)
  (nt-masks--refresh nt-mask-list))

;;;;; Development

(defun nt-setup--quick-dirty ()
  "DEV UTIL - Setup components that will need to be redone more generally."
  (add-hook 'lisp-mode-hook #'nt-note--kwds-add)
  (add-hook 'after-change-functions #'nt-after-change-function nil 'local)

  (let ((nt-mask--wait-for-refresh t))
    (lisp-mode)
    (font-lock-ensure))
  (nt-masks--refresh-buffer))

(defun nt-disable--quick-dirty ()
  "DEV UTIL - Disable components that will need to be redone more generally."
  (remove-hook 'lisp-mode-hook #'nt-note--kwds--add)
  (remove-hook 'after-change-functions #'nt-after-change-function 'local)
  (setq font-lock-keywords nil)
  ;; todo remove all instances of 'nt-note--face
  )

(defun nt-disable--just-in-case ()
  "DEV UTIL - Disable components that /should/ be handled by other methods."
  (setq nt-mask--wait-for-refresh nil))

;;;; Commands

(defun nt-disable ()
  "Delete overlays managed by nt."
  (interactive)
  (nt-ov--remove-all)
  (nt-disable--quick-dirty)
  (nt-disable--just-in-case))

;;;###autoload
(defun nt-enable ()
  "Enable nt and cleanup previous instance if running."
  (interactive)
  (nt-disable)
  (nt-setup--agnostic)
  (nt-setup--quick-dirty))

;;; Provide

(provide 'nt)

;;; nt.el ends here
