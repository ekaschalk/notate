;;; nt.el --- Program with Personalized Notation -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/ekaschalk/notate
;; Version: 0.1
;; Keywords: indentation, display, notes, major-modes
;; Package-Requires: ((cl "1.0") (dash "2.14.1") (dash-functional "1.2.0") (s "1.12.0") (smartparens "1.11.0") (emacs "26.1"))

;;; Commentary:
;;;; Copy of README Header

;; Notate your programs with indentation-correcting visual replacements of
;; symbols with other symbols.

;; Ligature's generalization, known to Emacs as ~prettified-symbols~, causes
;; alignment and indentation issues, preventing one from rendering ~for~ as ~∀~
;; and ~int~ as ~ℤ~ in their code

;; With notate, you can selectively APL-ize your code.

;;;; Further Commentary

;; Collect `nt' modules and expose the user API.

;; Majority of code is contained with `nt-note' and `nt-mask'

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-bounds)
(require 'nt-change)
(require 'nt-kwds)
(require 'nt-mask)
(require 'nt-note)
(require 'nt-ov)

;;; Configuration
;;;; Core

(defvar nt-defs '(("hello" "∧")
                  ("bye" "!∨"))
  "String->replacement alist specifying visual replacements.

Accepts an optional third element, a custom RX for the note. Otherwise defaults
to matching the specified string.")
;; TODO Support extra options for tailoring the RX without full-on providing it.
;; Probably make optional third element a plist where you can eg. tack-on a
;; symbol match if it sees like :symbol or something.


(defvar-local nt-bound-fn #'nt-bounds--lisps
  "A function that should return line boundaries [a b) given a NOTE.")


(defvar-local nt-bound?-fn #'nt-bounds?--lisps
  "A function that should return whether a given NOTE modifies indentation.")


(defvar nt-ignore-notes nil
  "List of strings identifying notes never contributing to indentation.

Atm, provides way to handle specially-indented symbols without delving into
special indentation rules and introspection.

There are better (ie. automated) ways to implement this, tbd.")


(defvar nt-normalize-height? nil
  "Force notated buffer's line heights to match true buffer?

For notated/true-buffer to be viewable side-by-side, this must be
true. However depending on font for note, it might be rendered
smaller than normal and widths might not match (so visual
indentation might be slightly off even with correct spaces).

A (possible) solution is to use 'specified spaces' (see emacs manual).

The problem is complex enough as is, so while a significant
upgrade, not the #1 priority at time of writing.

For development purposes, it is recommended to have false so
your code is monospaced.

For screenshot purposes, it is recommended to have true for
side-by-side comparisons to be aligned.")

;;;; Debugging

(defvar nt-display-prefixes? nil
  "Whether to add the 'line-prefix property to indentation overlays.")


(defvar nt-display-render-status? t
  "Whether to add a 'face property to rendered indentation overlays.")


(defvar nt-render-masks? t
  "Should masks render? Note that line-prefixes, if set to, still display.")

;;; Note-Mask Interactions

;; TODO Section is half-legacy, half-still-in-use

(defun nt--masks-for (note)
  "Return all masks NOTE contributes to."
  (-some->>
   note
   (funcall (symbol-value #'nt-bound?-fn))
   nt-note->bound
   (apply #'nt-masks<-lines)))

(defun nt--add-note-to-mask (note mask)
  "Add NOTE to a MASK, possibly refresh mask, and return back mask."
  (push note (overlay-get mask 'nt-notes))
  (nt-mask--refresh mask))

(defun nt--remove-note-from-mask (note mask)
  "Remove NOTE from MASK, possibly refresh mask, and return back mask."
  (delq note (overlay-get mask 'nt-notes))
  (nt-mask--refresh mask))

(defun nt--add-note-to-masks (note)
  "Add NOTE to all masks it contributes to and return them."
  (->> note
     nt--masks-for
     (-remove (-partial #'nt-mask--contains? note))
     (-map (-partial #'nt--add-note-to-mask note))))

(defun nt--remove-note-from-masks (note)
  "Remove NOTE from all masks it contributes to."
  (->> note
     nt--masks-for
     (-map (-partial #'nt--remove-note-to-mask note))))

(defun nt--add-notes-to-masks (notes)
  "Batch add NOTES to their masks refreshing upon completion."
  ;; (let ((nt-mask--wait-for-refresh? t))
  ;;   (-each notes #'nt--add-note-to-masks))

  ;; (nt-masks--refresh-buffer)

  ;; TODO Test this implementation compared to simpler version above
  (let ((masks))
    (let ((nt-mask--wait-for-refresh? t))
      (setq masks (-mapcat #'nt--add-note-to-masks notes)))
    (-> masks -distinct nt-masks--refresh)))

;;; Advice

;; SECTION IN DEVELOPMENT

(defun nt-masks--indent-difference (start-line end-line)
  "Get difference of END-LINE and START-LINE masked indents."
  (- (nt-mask->width (nt-mask<-line end-line))
     (nt-mask->width (nt-mask<-line start-line))))

;; (defun nt-masks--col-offset (start-line end-line)
;;   "Get masked column offset from START-LINE to END-LINE."
;;   (if (nt-mask--render? (nt-mask<-line end-line))
;;       (nt-masks--indent-difference start-line end-line)
;;     0))

(defun nt--advise-line-movement-of-masked-indent (line-fn &rest args)
  "Account for masked-indent column offsets in line up-down movement."
  ;; advises `next-line' (and later also `previous-line')

  ;; This may seem like a complete mess, but it actually is working.
  ;; in almost all cases. Only case I've found to still resolve
  ;; if goal-col is past the lines end AND moving into an unrendered mask

  (-let* (((line-count)
           args)
          (start-line
           (line-number-at-pos))
          (end-line
           (+ start-line line-count))
          (end-mask
           (nt-mask<-line end-line))
          (end-line-indent-masked?
           (nt-mask--render? end-mask))
          (col-offset
           (nt-masks--indent-difference start-line end-line))

          ;; Will make these 2 clearer asap
          (intersect
           (-intersection
            (nt-notes<-region (line-beginning-position) (point))
            (nt-mask->notes end-mask)))
          ;; What do i do if moving into a note? Anything?
          (masked-before-col
           (nt-notes<-region
            (nt-line->start end-line)
            (+ (nt-line->start end-line)
               (current-column))))
          )

    ;; ALG:
    ;; 1. Check if mask(end-line).notes contains notes occuring
    ;;    on start-line after current-column
    ;; 2. Subtract sum of such note's widths from the col-offset

    ;; CASE TO HANDLE:
    ;; Move forward past a note on the line we are moving into
    ;; eg. put point far in "test-bed for notate" header
    ;;     moving downwards with point past the first note will go <-
    ;; It's essentially the mirror of the intersect above

    ;; (message "INTERSECT %s" intersect)

    (setq col-offset (+ (- col-offset
                           (nt-notes->width intersect))
                        (nt-notes->width masked-before-col)))

    (message "temporary-goal-column %s before" temporary-goal-column)
    (message "cur-col %s before line-next" (current-column))

    (apply line-fn args)

    (message "temporary-goal-column %s after" temporary-goal-column)
    (message "cur-col %s after line-next" (current-column))
    (message "offset %s" col-offset)

    ;; Simplified version
    ;; (setq temporary-goal-column
    ;;       (+ (if end-line-indent-masked?
    ;;              (current-column)
    ;;            temporary-goal-column)
    ;;          col-offset))

    ;; FOUND ISSUE:
    ;; It matters whether we are going up/down before/after the
    ;; note on the current line...
    ;; If moving down with point past note -> dont need to offset
    ;;   as it is already offset
    ;; If moving before note -> need to offset

    ;; So:
    ;; Count the width of notes occurring before POINT in:
    ;;   (nt-mask->notes (nt-mask<-line start-line))
    ;; not the entire note set

    (unless end-line-indent-masked?
      ;; TODO need similar check to below on whether to
      ;; modify the goal column if past end of line
      (setq temporary-goal-column (+ temporary-goal-column
                                     col-offset)))

    (when (and (> 0 col-offset)
               end-line-indent-masked?)
      (forward-char col-offset)

      ;; Really sneaky emacs developers...

      ;; TODO We can lose our temporary-goal-column if:
      ;; moving down to a smaller mask + at line end already
      ;; so this should check if we are past end-column

      (unless (> temporary-goal-column
                 (save-excursion (end-of-line) (current-column)))
        (setq temporary-goal-column (current-column))))

    ;; (message "cur-col %s after offset" (current-column))
    ;; (message "---")
    ))

;; (remove-function #'next-line
;;                  #'nt--advise-line-movement-of-masked-indent)
;; (advice-remove #'previous-line #'nt--advise-line-movement-of-masked-indent)
;; (add-function :around '(local next-line)
;;               #'nt--advise-line-movement-of-masked-indent)
;; (advice-add #'previous-line :around #'nt--advise-line-movement-of-masked-indent)

;;; Setup
;;;; Solid

(defun nt-enable--agnostic ()
  "Setup all *major-mode-agnostic* components."
  (nt-masks--init)
  (nt-masks--refresh nt-masks))

(defun nt-disable--agnostic ()
  "Disable all *major-mode-agnostic* components."
  (nt-ov--remove-all))

;;;; Temporary

(defun nt-enable--temp ()
  "TEMP Setup components that will need to be redone more generally."
  (add-hook 'lisp-mode-hook #'nt-kwds--add)
  (add-hook 'after-change-functions #'nt-change--after-change-function nil 'local)

  (advice-add #'next-line :around #'nt--advise-line-movement-of-masked-indent)

  (let ((nt-mask--wait-for-refresh? t))
    (lisp-mode)
    (nt-notes--init))
  (nt-masks--refresh-buffer))

(defun nt-disable--temp ()
  "TEMP Disable components that will need to be redone more generally."
  (remove-hook 'lisp-mode-hook #'nt-kwds--add)
  (remove-hook 'after-change-functions #'nt-after-change-function 'local)

  ;; This working in some, not all cases yet
  (advice-add #'next-line :around #'nt--advise-line-movement-of-masked-indent)
  (advice-remove #'next-line #'nt--advise-line-movement-of-masked-indent)

  ;; (advice-add #'previous-line :around #'nt--advise-line-movement-of-masked-indent)
  ;; (advice-remove #'previous-line #'nt--advise-line-movement-of-masked-indent)


  (setq font-lock-keywords nil))

(defun nt-disable--just-in-case ()
  "TEMP Reset vars that _should_ never need to be reset."
  (setq nt-mask--wait-for-refresh? nil)
  (setq nt-note--init-in-progress nil)
  (setq nt-mask--init-in-progress nil))

;;; Interactive

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
