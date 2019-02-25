;;; aplig-lig.el --- Ligature Overlays -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Ligature overlay management, instantiation, etc.



;;; Code:
;;;; Requires

(require 'aplig-base)

(require 'aplig-ov)



;;; Overlays

(defun aplig-ligs--present? (&optional start end)
  "Are ligs present within START and END, defaulting to `match-data'? Get them."
  (let ((start (or start (match-beginning 1)))
        (end   (or end (match-end 1))))
    (and start end
         (-filter #'aplig-ov--lig?
                  (overlays-in start end)))))

(defun aplig-lig--at (pos)
  "Get lig at POS."
  (-first-item (aplig-ligs--present? pos (1+ pos))))

(defun aplig-lig--at-point ()
  "Get lig at point."
  (aplig-lig--at (point)))

(defun aplig-ligs--at (line)
  "Return all ligs on LINE."
  (apply #'aplig-ligs--present? (aplig-base--line-boundary line)))

(defun aplig-ligs--in (start-line end-line)
  "Return all ligs in [START-LINE END-LINE)."
  (-mapcat (-applify #'aplig-ligs--at) (aplig-base--range start-line end-line)))

(defun aplig-lig--delete (lig)
  "Delete LIG."
  (delq lig aplig-lig-list)
  (delete-overlay lig))

(defun aplig-lig--decompose-hook (lig post-modification? start end &optional _)
  "Decompose LIG upon modification as a modification-hook."
  (when post-modification?
    (aplig--remove-lig-from-masks lig)
    (aplig-lig--delete lig)))



;;; Transforms

(defun aplig-lig->width (lig)
  "Wrapper to access width of LIG."
  (overlay-get lig 'aplig-width))

(defun aplig-ligs->width (ligs)
  "Sum widths of LIGS."
  (->> ligs (-map #'aplig-lig->width) -sum))



;;; Init

(defun aplig-lig--init-ov (ov string replacement)
  "Put lig text properties into OV."
  (-doto ov
    (overlay-put 'aplig?      t)
    (overlay-put 'aplig-lig?  t)
    (overlay-put 'aplig-width (aplig-base--s-diff string replacement))

    (overlay-put 'display replacement)
    (overlay-put 'modification-hooks '(aplig-lig--decompose-hook))))

(defun aplig-lig--init (string replacement &optional start end)
  "Build ligature overlay, defaulting to `match-data' for START and END."
  (setq start (or start (match-beginning 1)))
  (setq end   (or end   (match-end 1)))

  (unless (and start end)
    (error "Initiatializing ligature without match-data set."))

  (let* ((ov    (make-overlay start end))
         (lig   (aplig-lig--init-ov ov string replacement)))
    (push lig aplig-lig-list)
    (aplig--add-lig-to-masks lig)
    lig))



(provide 'aplig-lig)



;;; aplig-lig.el ends here
