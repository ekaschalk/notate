;;; aplig-ov.el --- Overlay management -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Methods acting on yet-to-be specialized or regardless-of specialization
;; aplig overlays.



;;; Code:
;;;; Requires

(require 'aplig-base)



;;; Predicates

(defun aplig-ov? (ov)
  "Is OV made by `aplig'? Get it."
  (and (overlay-get ov 'aplig?)
       ov))

(defun aplig-ov--lig? (ov)
  "Is OV a ligature? Get it."
  (and (overlay-get ov 'aplig-lig?)
       ov))

(defun aplig-ov--mask? (ov)
  "Is OV a mask? Get it."
  (and (overlay-get ov 'aplig-mask?)
       ov))

(defun aplig-ov--in? (ov start end)
  "Is OV contained within START and END? Get it."
  (and ov start end
       (<= start
          (overlay-start ov)
          (overlay-end ov)
          end)
       ov))



;;; Utils

(defun aplig-ov--goto (ov)
  "Goto start of OV."
  (goto-char (overlay-start ov)))

(defun aplig-ov--at (pos)
  "Return first aplig overlay at POS."
  (-any #'aplig-ov? (overlays-at pos)))

(defun aplig-ov--at-point ()
  "Execute `aplig-ov--at' point."
  (aplig-ov--at (point)))



;;; Management

(defun aplig-ov--remove-all ()
  "Remove all `aplig' overlays from current buffer."
  (remove-overlays nil nil 'aplig? t)
  (setq aplig-mask-list nil)
  (setq aplig-lig-list nil))



(provide 'aplig-ov)



;;; aplig-ov.el ends here
