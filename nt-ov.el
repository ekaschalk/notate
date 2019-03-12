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
  (when (overlayp ov)
    (-> ov (overlay-get 'nt?) (and ov))))

(defun nt-ov--note? (ov)
  "Is OV a note? Get it."
  (-some-> ov nt-ov? (overlay-get 'nt-note?) (and ov)))

(defun nt-ov--mask? (ov)
  "Is OV a mask? Get it."
  (-some-> ov nt-ov? (overlay-get 'nt-mask?) (and ov)))

(defun nt-ov--deleted? (ov)
  "Has OV been deleted?"
  (when (nt-ov? ov)
    (-> ov overlay-buffer null)))

(defun nt-ov--in? (ov start end)
  "Is OV contained within START and END? Get it."
  (when (nt-ov? ov)
    (and start end
         (<= start (overlay-start ov) (overlay-end ov) end)
         ov)))

;;; Utils

(defun nt-ov--goto (ov)
  "Goto start of OV."
  (-> ov overlay-start goto-char))

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
