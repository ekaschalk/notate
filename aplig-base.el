;;; aplig-base.el --- Common requires and utilities -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Collect pkg-wide imports and utils



;;; Code:
;;;; Requires

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'smartparens)

;;; Utils

(defun aplig-base--s-diff (s1 s2)
  "Return difference of lengths of strings S1 and S2."
  (- (length s1) (length s2)))

(defun aplig-base--goto-line (line)
  "Non-interactive version of `goto-line'."
  (forward-line (- line (line-number-at-pos))))

(defun aplig-base--line-start (line)
  "Return pos at start of LINE."
  (save-excursion (aplig-base--goto-line line) (line-beginning-position)))

(defun aplig-base--line-end (line)
  "Return pos at end of LINE."
  (save-excursion (aplig-base--goto-line line) (line-end-position)))



(provide 'aplig-base)



;;; aplig-base.el ends here
