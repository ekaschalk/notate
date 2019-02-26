;;; nt-lig.el --- Ligature Overlays -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Ligature overlay management, instantiation, etc.



;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)



;;; Overlays

(defun nt-ligs--present? (&optional start end)
  "Are ligs present within START and END, defaulting to `match-data'? Get them."
  (let ((start (or start (match-beginning 1)))
        (end   (or end (match-end 1))))
    (and start end
         (-filter #'nt-ov--lig?
                  (overlays-in start end)))))

(defun nt-lig--at (pos)
  "Get lig at POS."
  (-first-item (nt-ligs--present? pos (1+ pos))))

(defun nt-lig--at-point ()
  "Get lig at point."
  (nt-lig--at (point)))

(defun nt-ligs--at (line)
  "Return all ligs on LINE."
  (apply #'nt-ligs--present? (nt-base--line-boundary line)))

(defun nt-ligs--in (start-line end-line)
  "Return all ligs in [START-LINE END-LINE)."
  (-mapcat (-applify #'nt-ligs--at) (nt-base--range start-line end-line)))

(defun nt-lig--delete (lig)
  "Delete LIG."
  (delq lig nt-lig-list)
  (delete-overlay lig))

(defun nt-lig--decompose-hook (lig post-modification? start end &optional _)
  "Decompose LIG upon modification as a modification-hook."
  (when post-modification?
    (nt--remove-lig-from-masks lig)
    (nt-lig--delete lig)))



;;; Transforms

(defun nt-lig->width (lig)
  "Wrapper to access width of LIG."
  (overlay-get lig 'nt-width))

(defun nt-ligs->width (ligs)
  "Sum widths of LIGS."
  (->> ligs (-map #'nt-lig->width) -sum))



;;; Init

(defun nt-lig--init-ov (ov string replacement)
  "Put lig text properties into OV."
  (-doto ov
    (overlay-put 'nt?      t)
    (overlay-put 'nt-lig?  t)
    (overlay-put 'nt-width (nt-base--s-diff string replacement))

    (overlay-put 'display replacement)
    (overlay-put 'modification-hooks '(nt-lig--decompose-hook))))

(defun nt-lig--init (string replacement &optional start end)
  "Build ligature overlay, defaulting to `match-data' for START and END."
  (setq start (or start (match-beginning 1)))
  (setq end   (or end   (match-end 1)))

  (unless (and start end)
    (error "Initiatializing ligature without match-data set."))

  (let* ((ov    (make-overlay start end))
         (lig   (nt-lig--init-ov ov string replacement)))
    (push lig nt-lig-list)
    (nt--add-lig-to-masks lig)
    lig))



(provide 'nt-lig)



;;; nt-lig.el ends here
