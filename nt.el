;;; nt.el --- Program with Personalized Notation -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/ekaschalk/notate
;; Version: 0.1
;; Keywords: indentation, display, notes, major-modes
;; Package-Requires: ((cl "1.0") (dash "2.14.1") (dash-functional "1.2.0") (s "1.12.0") (smartparens "1.11.0") (emacs "26.1"))

;;; Commentary:

;; Notate - Program with Personalized Notation

;; `nt.el' exposes user API and configurations
;; Developers, see `nt-dev' which adds additional debugging and developing cmds.

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


(defvar-local nt-bound-fn
  ;; #'nt-bounds--general
  #'nt-bounds--lisps
  "A function that should return line boundaries [a b) given a NOTE.")


;; Predicate version can't be generalized without performance costs I think
(defvar-local nt-bound?-fn #'nt-bounds?--lisps
  "A function that should return whether a given NOTE modifies indentation.")


(defvar nt-ignore-notes nil
  "List of strings identifying notes you never want to modify indentation.

Note that emacs-lisp indent declarations /are/ supported. For
example, notating defun or let as script f and l, won't mask
indentation, as the indent rules decide the indent.")


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
  "Add 'line-prefix property to all indentation overlays?

Prefixes prefix will prefix each line with:

  xx:yy:+z
   with values true-indent:masked-indent:notes-contributing-to-masks

To allow identifying what Notate is doing at a glance.")


(defvar nt-display-render-status? t
  "Underline first char of every line with masked indentation?")


(defvar nt-render-masks? t
  "Actually mask the indentation? Line prefixes may still be used and seen.")

;;; Setup
;;;; Solid

(defun nt-enable--agnostic ()
  "Setup all *major-mode-agnostic* components."
  (nt-masks--init)
  (nt-notes--init))

(defun nt-disable--agnostic ()
  "Disable all *major-mode-agnostic* components."
  (nt-ov--remove-all))

;;;; Temporary

(defun nt-enable--temp ()
  "TEMP Setup components that will need to be redone more generally."
  (add-hook 'lisp-mode-hook #'nt-kwds--add)
  (lisp-mode))

(defun nt-disable--temp ()
  "TEMP Disable components that will need to be redone more generally."
  (remove-hook 'lisp-mode-hook #'nt-kwds--add))

(defun nt-disable--just-in-case ()
  "TEMP Reset vars that _should_ never need to be reset."
  (setq nt-mask--wait-for-refresh? nil)
  (setq nt-mask--init-in-progress nil)
  (setq nt-note--init-in-progress nil))

;;;; In-Progress

(defun nt-enable--in-progress ()
  "TEMP Enable Notate components that are in active development"
  (add-hook 'after-change-functions #'nt-change--after-change-function nil 'local)
  )

(defun nt-disable--in-progress ()
  "TEMP Disable Notate components that are in active development."
  (remove-hook 'after-change-functions #'nt-change--after-change-function 'local)
  )

;;; Interactive

(defun nt-disable ()
  "Delete overlays managed by nt."
  (interactive)

  (nt-disable--agnostic)
  (nt-disable--temp)
  (nt-disable--just-in-case)
  (nt-disable--in-progress))

;;;###autoload
(defun nt-enable ()
  "Enable nt and cleanup previous instance if running."
  (interactive)

  (nt-disable)

  (nt-enable--temp)  ; This should happen before agnostic atm
  (nt-enable--agnostic)
  (nt-enable--in-progress))

;;; Provide

(provide 'nt)

;;; nt.el ends here
