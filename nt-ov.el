;;; nt-ov.el --- Overlay management -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Methods acting on yet-to-be-specialized or regardless-of-specialization
;; nt overlays.

;;; Code:
;;;; Requires

(require 'nt-base)

;;; Predicates

(defun nt-ov? (ov)
  "Is OV made by `nt'? Get it."
  (-> ov (overlay-get 'nt?) (and ov)))

(defun nt-ov--note? (ov)
  "Is OV a note? Get it."
  (-> ov (overlay-get 'nt-note?) (and ov)))

(defun nt-ov--mask? (ov)
  "Is OV a mask? Get it."
  (-> ov (overlay-get 'nt-mask?) (and ov)))

(defun nt-ov--in? (ov start end)
  "Is OV contained within START and END? Get it."
  (and ov start end
       (<= start (overlay-start ov) (overlay-end ov) end)
       ov))

(defun nt-ov--deleted? (ov)
  "Has OV been deleted?"
  (-> ov overlay-buffer null))

;;; Utils

(defun nt-ov--goto (ov)
  "Goto start of OV."
  (goto-char (overlay-start ov)))

(defun nt-ov--at (pos)
  "Return first nt overlay at POS."
  (-any #'nt-ov? (overlays-at pos)))

(defun nt-ov--at-point ()
  "Execute `nt-ov--at' point."
  (nt-ov--at (point)))

(defun nt-ov--region (ov)
  "Return OV's region."
  (list (overlay-start ov)
        (overlay-end ov)))

;;; Management

(defun nt-ov--remove-all ()
  "Remove all `nt' overlays from current buffer."
  (remove-overlays nil nil 'nt? t)
  (setq nt-mask-list nil)
  (setq nt-note-list nil))

;;; Provide

(provide 'nt-ov)

;;; nt-ov.el ends here
