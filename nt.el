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



;;; Configuration
;;;; Core

(defconst nt-notes (nt-notes--make '(("hello" "∧") ("bye" "!∨")))
  "plist of note specifications resulting from `nt-notes--make'.")

(defvar-local nt-bound-fn #'nt-bounds--lisps
  "A function that should return line boundaries [a b) given a NOTE.")

(defvar-local nt-bound?-fn #'nt-bounds?--lisps
  "A subset of `nt-bound-fn', whether NOTE has a boundary.")

(defvar nt-normalize-height? nil
  "Force notated buffer's line heights to match true buffer?

For notated/true-buffer to be viewable side-by-side, this must be
true. However depending on font for note, it might be rendered
smaller than normal and widths might not match (so visual
indentation might be slightly off even with correct spaces).

A (possible) solution is to use 'specified spaces':
https://www.gnu.org/software/emacs/manual/html_node/elisp/Specified-Space.html

The problem is complex enough as is, so while a significant
upgrade, not the #1 priority at time of writing.

For development purposes, it is recommended to have false to
evaluate indenation more reliably.

For screenshot purposes, it is recommended to have true for
side-by-side comparisons.")

;;;; Debugging

(defvar nt-display-prefixes? t
  "Whether to add the 'line-prefix property to indentation overlays.")

(defvar nt-display-render-status? t
  "Whether to add a 'face property to rendered indentation overlays.")

(defvar nt-render-masks? t
  "Should masks render? Note that line-prefixes, if set to, still display.")

;;;; Managed

(defvar nt-note-list nil
  "List of note overlays currently managed.")

(defvar nt-mask-list nil
  "List of indent overlays currently managed.

This an ordered list s.t. nt-mask-list[i] = mask-at-line-i+1.

Accessing this should be done through `nt-mask--at' and friends to avoid
confusing indexings.")

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

;; NOTE DELETION ALGORITHM rough draft

;; Given notes n_i ordered descending by indent, let m_i be the mask at line(n_i)
;; with the masks notes denoted n_m_i.

;; Intersect n_m_0 with each n_m_1.. Add n_0 and call it note-chain C_0 maintaining order.
;; Repeat above for next note not contained in C_0 and call it C_1.
;; Repeat until each n_i is a member of some chain.

;; For each chain C_i, let l_i be the line of C_i[0] and then:
;; 1. Delete notes in C_i
;; 2. Goto line 1+l_i
;; 3. Remove any deleted notes from mask at line
;; 4. Forward-line and repeat step 3 until mask at line has no deleted notes

(defun nt--delete-notes (notes)
  "Delete NOTES and refresh the masks they contributed to."

  ;; (->> notes
  ;;    (-group-by #'nt-note->chain)
  ;;    (-map (-partial #'-sort
  ;;                    (-on #'< #'nt-note->indent))))
  ;; (nt-note--delete note)

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

(defun nt-setup--agnostic ()
  "Setup all *major-mode-agnostic* components."
  (nt-masks--init)
  (nt-masks--refresh nt-mask-list))

;;;; Commands

(defun nt-disable ()
  "Delete overlays managed by nt."
  (interactive)

  (nt-ov--remove-all)
  (remove-hook 'lisp-mode-hook #'nt-kwds--add)
  (remove-hook 'after-change-functions #'nt-after-change-function 'local)

  ;; TODO remove all 'nt-note--face

  ;; just-in-case stuff
  (setq nt-mask--wait-for-refresh nil)
  (setq font-lock-keywords nil))

;;;###autoload
(defun nt-enable ()
  "Enable nt and cleanup previous instance if running."
  (interactive)

  (nt-disable)
  (nt-setup--agnostic)

  (add-hook 'lisp-mode-hook #'nt-note--kwds-add)
  (let ((nt-mask--wait-for-refresh t))
    (lisp-mode)
    (font-lock-ensure))

  (nt-masks--refresh-buffer)
  (add-hook 'after-change-functions #'nt-after-change-function nil 'local)

  ;; This isn't working yet, not highest priority
  ;; (advice-remove 'undo-tree-undo #'nt-masks--unrender-buffer)
  ;; (advice-remove 'undo-tree-undo #'nt-masks--render-buffer)
  ;; (advice-add 'undo-tree-undo :before #'nt-masks--unrender-buffer)
  ;; (advice-add 'undo-tree-undo :after #'nt-masks--render-buffer)
  )

(defun nt-remove-note-at-point ()
  "Delete note at point if it exists and update masks."
  (interactive)

  (nt--delete-note (nt-note--at-point)))



(provide 'nt)



;;; nt.el ends here
