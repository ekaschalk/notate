;;; nt-base.el --- Common requires and utilities -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Collect pkg-wide imports and utils

;;; Code:
;;;; Requires

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 'hierarchy)
(require 's)
(require 'smartparens)

;;; General Purpose

(defun nt-base--s-diff (s1 s2)
  "Return difference of lengths of strings S1 and S2."
  (- (length s1) (length s2)))

(defun nt-base--range (from &optional to inc)
  "Open RHS variation of `number-sequence', see its documentation."
  (number-sequence from (and to (1- to)) inc))

;;; Line Methods

(defun nt-base--goto-line (line)
  "Non-interactive version of `goto-line'."
  (forward-line (- line (line-number-at-pos))))

(defun nt-base--line-start (line)
  "Return pos at start of LINE."
  (save-excursion (nt-base--goto-line line) (line-beginning-position)))

(defun nt-base--line-end (line)
  "Return pos at end of LINE."
  (save-excursion (nt-base--goto-line line) (line-end-position)))

(defun nt-base--line-bounds (line)
  "Return [start end] points of LINE."
  (save-excursion
    (nt-base--goto-line line)
    (list (line-beginning-position)
          (line-end-position))))

(defun nt-base--lines-bounds (start-line end-line)
  "Return [start end] points with start of START-LINE, end of 1-END-LINE."
  (save-excursion
    (nt-base--goto-line start-line)
    (list (line-beginning-position)
          (progn (nt-base--goto-line (1- end-line))
                 (line-end-position)))))

(defun nt-base--line-size (line)
  "Return number of characters composing LINE."
  (apply #'- (-> line nt-base--line-bounds reverse)))

(defun nt-base--indent-col (&optional n)
  "Get indentation col, of line forward N-1 times if given."
  (save-excursion (end-of-line n) (back-to-indentation) (current-column)))

(defun nt-base--indent-at (line)
  "Get indentation col of LINE."
  (save-excursion (nt-base--goto-line line) (nt-base--indent-col)))

;;; Provide

(provide 'nt-base)

;;; nt-base.el ends here
