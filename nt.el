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

;; Collect `nt' modules and expose settings/API to users.

;; Majority of code is contained within the other modules.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-bounds)
(require 'nt-change)
(require 'nt-kwds)
(require 'nt-mask)
(require 'nt-note)
(require 'nt-ov)
(require 'nt-vis)

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

;;;; Managed

(defvar nt-vis--temporary-goal-column nil
  "A visual-column version of `temporary-goal-column' for masking line mvmnt.

It is implemented in terms of `temporary-goal-column' so it doesn't share
semantics quite the same. Used for tracking goal columns when moving lines
interacts with EOLs.")

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


  ;; ~~
  ;; Working..
  (advice-add #'next-line :around #'nt-vis--mask-line-movement)
  (advice-add #'previous-line :around #'nt-vis--mask-line-movement)
  ;; ~~

  (let ((nt-mask--wait-for-refresh? t))
    (lisp-mode)
    (nt-notes--init))
  (nt-masks--refresh-buffer))

(defun nt-disable--temp ()
  "TEMP Disable components that will need to be redone more generally."
  (remove-hook 'lisp-mode-hook #'nt-kwds--add)
  (remove-hook 'after-change-functions #'nt-after-change-function 'local)

  ;; ~~
  ;; Mostly working..
  (advice-add #'next-line :around #'nt-vis--mask-line-movement)
  (advice-remove #'next-line #'nt-vis--mask-line-movement)
  (advice-add #'previous-line :around #'nt-vis--mask-line-movement)
  (advice-remove #'previous-line #'nt-vis--mask-line-movement)
  ;; ~~
  )

(defun nt-disable--just-in-case ()
  "TEMP Reset vars that _should_ never need to be reset."
  (setq nt-vis--temporary-goal-column nil)
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
