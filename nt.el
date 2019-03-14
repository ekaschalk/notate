;;; nt.el --- Program with Personalized Notation -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/ekaschalk/nt
;; Version: 0.1
;; Keywords: indentation, display, notes, major-modes
;; Package-Requires: ((cl "1.0") (dash "2.14.1") (dash-functional "1.2.0") (hierarchy "0.2.0") (s "1.12.0") (smartparens "1.11.0") (emacs "26.1"))

;;; Commentary:
;;;; Header

;; Notate your programs with indentation-correcting visual replacements of
;; symbols with other symbols.

;; Ligature's generalization, known to Emacs as ~prettified-symbols~, causes
;; alignment and indentation issues, preventing one from rendering ~for~ as ~∀~
;; and ~int~ as ~ℤ~ in their code

;; With notate, you can selectively APL-ize your code.

;;;; nt.el

;; `nt.el' exposes user configuration and enable/disable functionality.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-bounds)
(require 'nt-change)
(require 'nt-mask)
(require 'nt-note)
(require 'nt-ov)
;; (require 'nt-tree)

;;; Configuration
;;;; Core

(defvar nt-notes (nt-notes--make '(("hello" "∧") ("bye" "!∨")))
  "plist of note specifications resulting from `nt-notes--make'.")


(defvar-local nt-bound-fn #'nt-bounds--lisps
  "A function that should return line boundaries [a b) given a NOTE.")


(defvar-local nt-bound?-fn #'nt-bounds?--lisps
  "A function that should return whether a given NOTE modifies indentation.")


(defvar nt-ignore-notes '("defun")
  "List of strings identifying notes never contributing to indentation.

There are better (ie. automated) ways to implement blacklisting, tbd.")


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
  "Manage note overlays in a `hierarchy' tree, ordered on interval-containment.

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

---
There are better tree structures we can use, which will be
explored nearing project completion.")


(defvar nt-note-list nil
  "List of note overlays currently managed.

NOTE - transfer to tree based implementation in-progress.")


(defvar nt-mask-list nil
  "List of indent overlays currently managed.

This an ordered list s.t. nt-mask-list[i] = mask-at-line-i+1.

Accessing this should be done through `nt-mask--at' and friends to avoid
confusing indexings.

NOTE - This will be converted into a vector soon^tm for constant-time idxing.")

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

(defun nt--delete-region (start end)
  "Delete NOTES within START and END and refresh their masks."
  (nt--delete-notes (nt-notes<-region start end)))

(defun nt--delete-notes (notes)
  "Delete NOTES and refresh their masks."
  (let* ((roots (nt-notes->roots notes))
         (bounds (-map nt-note->bound roots)))
    (-each notes #'nt-note--delete)
    (-each bounds #'nt-mask--refresh-region)))

;;; Interactive
;;;; Setup
;;;;; Proper

(defun nt-enable--agnostic ()
  "Setup all *major-mode-agnostic* components."
  ;; (nt-tree--init)
  (nt-masks--init)
  (nt-masks--refresh nt-mask-list))

(defun nt-disable--agnostic ()
  "Disable all *major-mode-agnostic* components."
  (nt-ov--remove-all))

;;;;; Development

(defun nt-enable--temp ()
  "DEV UTIL - Setup components that will need to be redone more generally."
  (add-hook 'lisp-mode-hook #'nt-note--kwds-add)
  (add-hook 'after-change-functions #'nt-change--after-change-function nil 'local)

  (let ((nt-mask--wait-for-refresh t))
    (lisp-mode)
    (font-lock-ensure))
  (nt-masks--refresh-buffer))

(defun nt-disable--temp ()
  "DEV UTIL - Disable components that will need to be redone more generally."
  ;; todo remove all instances of 'nt-note--face
  (remove-hook 'lisp-mode-hook #'nt-note--kwds--add)
  (remove-hook 'after-change-functions #'nt-after-change-function 'local)
  (setq font-lock-keywords nil))

(defun nt-disable--just-in-case ()
  "DEV UTIL - Disable components that /should/ be handled by other methods."
  (setq nt-mask--wait-for-refresh nil))

;;;; Commands

(defun nt-disable ()
  "Delete overlays managed by nt."
  (interactive)

  (nt-disable--agnostic)
  (nt-disable--temp)
  (nt-disable--just-in-case))

;;;###autoload
(defun nt-enable ()
  "Enable nt and cleanup previous instance if running."
  (interactive)

  (nt-disable)

  (nt-enable--agnostic)
  (nt-enable--temp))

;;; Provide

(provide 'nt)

;;; nt.el ends here
