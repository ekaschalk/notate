;;; virtual-indent.el --- Personal Indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Trying to resolve key issues with ligature adoption, namely indentation.
;; A general implementation has interesting applications elsewhere, like
;; opening a 2-indent Python file and seeing it as if it was 4-indent, and
;; making the `:nameless' emacs-lisp library more valuable.

;;; Code:
;;;; Requires

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)

;;; Virtual-Indent
