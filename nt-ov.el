;;; nt-ov.el --- Overlay management -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Methods acting on yet-to-be-specialized, or, regardless-of-specialization
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

;;; Transforms

(defun nt-ov->region (ov)
  "Return OV's region."
  (when (nt-ov? ov)
    (list (overlay-start ov)
          (overlay-end ov))))

(defun nt-ov->line (ov)
  "Return OV's line. Note that all nt ovs don't span lines, so this is ok."
  (-> ov overlay-start line-number-at-pos))

(defun nt-ov->length (ov)
  "Return number of chars covered by OV."
  (-let (((start end) (nt-ov->region ov)))
    (- end start)))

(defun nt-ov->string (ov)
  "Return string covered by OV."
  (-some->> ov nt-ov->region (apply #'buffer-substring-no-properties)))

(defun nt-ov->width (ov)
  "Return difference of OV's length and its 'display properties length."
  (- (nt-ov->length ov)
     (length (or (overlay-get ov 'display)
                 '(nil-should-be-length-1)))))

(defun nt-ovs->width (ovs)
  "Return sum of OVS widths."
  (->> ovs (-map #'nt-ov->width) -sum))

;;; Access

(defun nt-ov<-pos (pos)
  "Return first nt overlay at POS."
  (-any #'nt-ov? (overlays-at pos)))

(defun nt-ov--at-point ()
  "Execute `nt-ov<-pos' point."
  (nt-ov<-pos (point)))

(defun nt-ovs<-region (start end)
  (-filter #'nt-ov? (overlays-in start end)))

;; Fill these out if I find a point to doing so, pun-intended
;; (defun nt-ovs<-line (line))
;; (defun nt-ovs<-lines (start-line end-line))

;;; Management

(defun nt-ov--goto (ov)
  "Goto start of OV."
  (-> ov overlay-start goto-char))

(defun nt-ov--extend (ov end)
  "Execute `move-overlay' for only an END."
  (move-overlay ov (overlay-start ov) end))

(defun nt-ov--remove-all ()
  "Remove all `nt' overlays from current buffer, no questions asked."
  (remove-overlays nil nil 'nt? t)
  (setq nt-masks nil)
  (setq nt-notes nil))

;;; Provide

(provide 'nt-ov)

;;; nt-ov.el ends here
