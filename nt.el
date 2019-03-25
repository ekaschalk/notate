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
  "Get difference of START-LINE and END-LINE's masked indents."
  (- (nt-mask->width (nt-mask<-line start-line))
     (nt-mask->width (nt-mask<-line end-line))))

(defun nt-masks--col-offset (start-line end-line)
  "Get masked column offset from START-LINE to END-LINE."
  (if (nt-mask--render? (nt-mask<-line end-line))
      (nt-masks--indent-difference start-line end-line)
    0))

(defun nt--advise-line-movement-of-masked-indent (line-fn &rest args)
  "Account for masked-indent column offsets in line up-down movement."
  ;; advises `next-line' and `previous-line'
  (-let* (((line-count)
           args)
          (start-line
           (line-number-at-pos))
          (end-line
           (+ start-line line-count))
          (col-offset
           (nt-masks--col-offset start-line end-line)))

    ;; (message "temporary-goal-column %s before" temporary-goal-column)
    ;; (message "cur-col %s before line-next" (current-column))

    (apply line-fn args)

    ;; (message "temporary-goal-column %s after" temporary-goal-column)
    ;; (message "cur-col %s after line-next" (current-column))

    (forward-char (- 0 col-offset))

    ;; Really sneaky emacs developers...
    ;; This demonstrates better than any other instance I've seen why
    ;; global mutations can cause very confusing behavior...
    (setq temporary-goal-column (current-column))

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

  ;; (advice-add #'next-line :around #'nt--advise-line-movement-of-masked-indent)
  ;; (advice-remove #'next-line #'nt--advise-line-movement-of-masked-indent)

  ;; This working in some, not all cases yet
  (advice-add #'evil-line-move :around
              #'nt--advise-line-movement-of-masked-indent)
  (advice-remove #'evil-line-move
                 #'nt--advise-line-movement-of-masked-indent)


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
