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
  (and (overlay-get ov 'nt?)
       ov))

(defun nt-ov--note? (ov)
  "Is OV a note? Get it."
  (and (overlay-get ov 'nt-note?)
       ov))

(defun nt-ov--mask? (ov)
  "Is OV a mask? Get it."
  (and (overlay-get ov 'nt-mask?)
       ov))

(defun nt-ov--in? (ov start end)
  "Is OV contained within START and END? Get it."
  (and ov start end
       (<= start
          (overlay-start ov)
          (overlay-end ov)
          end)
       ov))



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



;;; Management

(defun nt-ov--remove-all ()
  "Remove all `nt' overlays from current buffer."
  (remove-overlays nil nil 'nt? t)
  (setq nt-mask-list nil)
  (setq nt-note-list nil))



(provide 'nt-ov)



;;; nt-ov.el ends here
