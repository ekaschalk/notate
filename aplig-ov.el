;;; aplig-ov.el --- Overlay management -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Methods acting on yet-to-be specialized overlays.



;;; Code:
;;;; Requires

(require 'aplig-base)



;;; Predicates

(defun aplig-ov--lig? (ov)
  "Is OV a ligature?"
  (overlay-get ov 'aplig-lig?))

(defun aplig-ov--mask? (ov)
  "Is OV a mask?"
  (overlay-get ov 'aplig-mask?))

(defun aplig-ov--in? (ov start end)
  "Is OV contained within START and END?"
  (and ov start end
       (<= start
          (overlay-start ov)
          (overlay-end ov)
          end)))

(defun aplig-ov--in-match? (ov subexp)
  "Is OV contained in the SUBEXP matching group?"
  (aplig--ov-in-bound? ov (match-beginning subexp) (match-end subexp)))



;;; Utils

(defun aplig-ovs--prop (ovs prop)
  "Return list of each OVS PROP."
  (--map (overlay-get it prop) ovs))

(defun aplig-ov--at (pos)
  "Return first aplig overlay at POS."
  (let ((ovs (overlays-at pos)))
    (--any (when (overlay-get it 'aplig?) it) ovs)))

(defun aplig-ov--goto (ov)
  "Goto start of OV."
  (goto-char (overlay-start ov)))

(defun aplig-ov--at-point ()
  "Execute `aplig-ov--at' point."
  (aplig-ov--at (point)))



(provide 'aplig-ov)



;;; aplig-ov.el ends here
