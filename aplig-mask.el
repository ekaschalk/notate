;;; aplig-mask.el --- Indentation Masks -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Indentation mask management, instantiation, transforms, etc.



;;; Code:
;;;; Requires

(require 'aplig-base)

(require 'aplig-ov)



;;; Lines

(defun aplig-mask--indent-col (&optional n)
  "Get indentation col, of line forward N-1 times if given."
  (save-excursion (end-of-line n) (back-to-indentation) (current-column)))

(defun aplig-mask--indent-at (line)
  "Get indentation col of LINE."
  (save-excursion (aplig-base--goto-line line) (aplig-mask--indent-col)))

(defun aplig-mask--at (line)
  "Retrieve mask at LINE."
  (nth (1- line) aplig-mask-list))

(defun aplig-masks--at (lines)
  "Retrieve masks at LINES."
  (-select-by-indices (-map #'1- lines) aplig-mask-list))

(defun aplig-masks--in (start-line end-line)
  "Retrieve masks within START-LINE and END-LINE."
  (-slice aplig-mask-list (1- start-line) (1- end-line)))

(defun aplig-mask--insert-at (mask line)
  "Insert MASK at LINE into `aplig-mask-list'."
  (setq aplig-mask-list (-insert-at (1- line) mask aplig-mask-list)))



;;; Transforms

(defun aplig-mask->indent (mask)
  "Return true indent of line containing MASK."
  (save-excursion (aplig-ov--goto mask) (aplig-mask--indent-col)))

(defun aplig-mask->ligs (mask)
  "Wrapper to access ligs contributing to MASK."
  (overlay-get mask 'aplig-ligs))

(defun aplig-mask->width (mask)
  "Calculate width of MASK's ligs."
  (-> mask aplig-mask->ligs aplig-ligs->width))

(defun aplig-mask->line (mask)
  "Return MASK's line."
  (-> mask overlay-start line-number-at-pos))



;;; Predicates

(defun aplig-mask--empty? (mask)
  "Is MASK currently empty of ligatures?"
  (= 0 (aplig-mask->width mask)))

(defun aplig-mask--enough-space? (mask)
  "Does MASK's line contain enough space for rendering? If so get MASK."
  (let ((line-size (-> mask aplig-mask->line aplig-base--line-size))
        (mask-size (aplig-mask->width mask)))
    ;; (message "%s mask ov" mask)
    ;; (message "%s line" line-size)
    ;; (message "%s mask" mask-size)

    ;; If this fails -> mask unrenderes -> mask has display nil

    (and (<= line-size mask-size)
         mask)))

(defun aplig-mask--contains? (lig mask)
  "Does MASK already contain LIG? If so get MASK."
  (and (-> mask aplig-mask->ligs (-contains? lig))
       mask))

(defun aplig-mask--render? (mask)
  "Should MASK be rendered?"
  (and aplig-render-masks?
       (aplig-mask--enough-space? mask)))



;;; Overlays

(defun aplig-mask--delete (mask)
  "Delete MASK."
  (delq mask aplig-mask-list)
  (delete-overlay mask))

(defun aplig-mask--decompose-hook (mask post-mod? start end &optional _)
  "Overlay modification hook to delete indent ov upon modification within it."
  ;; NOTE probably need to handle deleting forward differently
  ;; NOTE probably need to handle visual deletion differently
  ;; NOTE nearly certain this is interacting in a bad way with `undo-tree-undo'
  (when post-mod?
    (let* ((inhibit-modification-hooks t)
           (width                      (aplig-mask->width mask))
           (invis-spaces-to-delete     (1+ width)))
      (aplig-mask--delete mask)
      (evil-with-single-undo
        (delete-char (- invis-spaces-to-delete))))))

(defun aplig-mask--format-prefix (mask)
  "Format the `line-prefix' overlay text property for MASK."
  (->>
   (list (-> "%02d" (format (aplig-mask->indent mask)))
         (-> "%02d" (format (aplig-mask->width mask)))
         (-> "+%d " (format (length (aplig-mask->ligs mask)))))
   (-interpose "|")
   (apply #'s-concat)))

(defun aplig-mask--reset-prefix (mask)
  "Reset the `line-prefix' text property for MASK."
  (when aplig-display-prefixes?
    (->> mask aplig-mask--format-prefix (overlay-put mask 'line-prefix))))

(defun aplig-mask--recenter (mask)
  "Recenter MASK, ie. reset its end position based on ligs widths."
  (let* ((start (overlay-start mask))
         (width (aplig-mask->width mask))
         (end   (+ 1 start width)))
    (when (= (line-number-at-pos start)
             (line-number-at-pos end))
      (move-overlay mask start end))))

(defun aplig-mask--render (mask)
  "Set display-based overlay properties for MASK."
  (-doto mask
    (overlay-put 'face    'underline)
    (overlay-put 'display " ")))

(defun aplig-mask--unrender (mask)
  "Remove display-based overlay properties for MASK."
  (-doto mask
    (overlay-put 'face    nil)
    (overlay-put 'display nil)))

(defun aplig-masks--render (masks)
  "Set display-based overlay properties for MASKS."
  (-each masks #'aplig-mask--render))

(defun aplig-masks--unrender (masks)
  "Remove display-based overlay properties for MASKS."
  (-each masks #'aplig-mask--unrender))

(defun aplig-masks--render-buffer (&rest _)
  "Set display-based overlay properties for masks in buffer (as a hook)."
  (aplig-masks--render aplig-mask-list))

(defun aplig-masks--unrender-buffer (&rest _)
  "Remove display-based overlay properties for masks in buffer (as a hook)."
  (aplig-masks--unrender aplig-mask-list))

(defun aplig-mask--reset-display (mask)
  "Reset display and face text properties of MASK."
  (if (aplig-mask--render? mask)
      (aplig-mask--render mask)
    (aplig-mask--unrender mask)))

(defun aplig-mask--refresh (mask)
  "Reset bounds and boundary-dependent properties of MASK based on its ligs."
  (-doto mask
    (aplig-mask--recenter)
    (aplig-mask--reset-prefix)
    (aplig-mask--reset-display)))

(defun aplig-mask--refresh-maybe (mask)
  "Perform `aplig-mask--refresh' when we should and return back MASK."
  (unless aplig-mask--wait-for-refresh
    (aplig-mask--refresh mask))
  mask)

(defun aplig-masks--refresh (masks)
  "Refresh MASKS."
  (-map #'aplig-mask--refresh-maybe masks))

(defun aplig-masks--refresh-buffer ()
  "Refresh `aplig-mask-list'."
  (aplig-masks--refresh aplig-mask-list))



;;; Init

(defun aplig-mask--init-ov (ov)
  "Put always-on text properties for masks into OV."
  (-doto ov
    (overlay-put 'aplig?      t)
    (overlay-put 'aplig-mask? t)
    (overlay-put 'aplig-ligs  nil)

    (overlay-put 'modification-hooks '(aplig-mask--decompose-hook))))

(defun aplig-mask--init (&optional line)
  "Create empty mask for LINE, otherwise current line."
  (save-excursion
    (when line (aplig-base--goto-line line))

    (let* ((line  (line-number-at-pos))
           (start (line-beginning-position))
           (end   (1+ start))
           (mask  (aplig-mask--init-ov (make-overlay start end))))
      (aplig-mask--insert-at mask line)
      mask)))

(defun aplig-masks--init (&optional start-line end-line)
  "Line-by-line buildup `aplig-mask-list', optionally [a b) bounded start/end."
  (save-excursion
    (aplig-base--goto-line (or start-line 1))

    (while (and (not (eobp))
                (if end-line (< (line-number-at-pos) end-line) t))
      (aplig-mask--init)
      (forward-line))))



(provide 'aplig-mask)



;;; aplig-mask.el ends here
