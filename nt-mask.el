;;; nt-mask.el --- Indentation Masks -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Indentation mask management, instantiation, transforms, etc.



;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)



;;; Lines

(defun nt-mask--indent-col (&optional n)
  "Get indentation col, of line forward N-1 times if given."
  (save-excursion (end-of-line n) (back-to-indentation) (current-column)))

(defun nt-mask--indent-at (line)
  "Get indentation col of LINE."
  (save-excursion (nt-base--goto-line line) (nt-mask--indent-col)))

(defun nt-mask--at (line)
  "Retrieve mask at LINE."
  (nth (1- line) nt-mask-list))

(defun nt-masks--at (lines)
  "Retrieve masks at LINES."
  (-select-by-indices (-map #'1- lines) nt-mask-list))

(defun nt-masks--in (start-line end-line)
  "Retrieve masks within START-LINE and END-LINE."
  (-slice nt-mask-list (1- start-line) (1- end-line)))

(defun nt-mask--insert-at (mask line)
  "Insert MASK at LINE into `nt-mask-list'."
  (setq nt-mask-list (-insert-at (1- line) mask nt-mask-list)))



;;; Transforms
;;;; Aliases

(defun nt-mask->ligs (mask)
  "Wrapper to access ligs contributing to MASK."
  (overlay-get mask 'nt-ligs))

(defun nt-mask->opaque-end (mask)
  "Return MASK's 'opaque-end text property."
  (overlay-get mask 'nt-opaque-end))

;;;; Methods

(defun nt-mask->indent (mask)
  "Return true indent of line containing MASK."
  (save-excursion (nt-ov--goto mask) (nt-mask--indent-col)))

(defun nt-mask->width (mask)
  "Calculate width of MASK's ligs."
  (-> mask nt-mask->ligs nt-ligs->width))

(defun nt-mask->line (mask)
  "Return MASK's line."
  (-> mask overlay-start line-number-at-pos))



;;; Predicates

(defun nt-mask--empty? (mask)
  "Is MASK currently empty of ligatures?"
  (= 0 (nt-mask->width mask)))

(defun nt-mask--enough-space? (mask)
  "Does MASK's line contain enough space for rendering?"
  (= (nt-mask->line mask)
     (-> mask nt-mask->opaque-end line-number-at-pos)))

(defun nt-mask--contains? (lig mask)
  "Does MASK already contain LIG?"
  (-> mask nt-mask->ligs (-contains? lig)))

(defun nt-mask--ends-agree? (mask)
  "Does MASK's opaque-end and actual end agree?"
  (= (overlay-end mask)
     (nt-mask->opaque-end mask)))

(defun nt-mask--render? (mask)
  "Should MASK be rendered?"
  (and nt-render-masks?
       (not (nt-mask--empty? mask))
       (nt-mask--ends-agree? mask)))



;;; Overlays

(defun nt-mask--delete (mask)
  "Delete MASK."
  (delq mask nt-mask-list)
  (delete-overlay mask))

(defun nt-mask--decompose-hook (mask post-mod? start end &optional _)
  "Overlay modification hook to delete indent ov upon modification within it."
  ;; NOTE probably need to handle deleting forward differently
  ;; NOTE probably need to handle visual deletion differently
  ;; NOTE nearly certain this is interacting in a bad way with `undo-tree-undo'
  (when post-mod?
    (let* ((inhibit-modification-hooks t)
           (width                      (nt-mask->width mask))
           (invis-spaces-to-delete     (1+ width)))
      (nt-mask--delete mask)
      (evil-with-single-undo
        (delete-char (- invis-spaces-to-delete))))))

(defun nt-mask--format-prefix (mask)
  "Format the `line-prefix' overlay text property for MASK."
  (->>
   (list (-> "%02d" (format (nt-mask->indent mask)))
         (-> "%02d" (format (nt-mask->width mask)))
         (-> "+%d " (format (length (nt-mask->ligs mask)))))
   (-interpose "|")
   (apply #'s-concat)))

(defun nt-mask--reset-prefix (mask)
  "Reset the `line-prefix' text property for MASK."
  (when nt-display-prefixes?
    (->> mask nt-mask--format-prefix (overlay-put mask 'line-prefix))))

(defun nt-mask--recenter-maybe (mask)
  "Recenter MASK if it won't cross lines doing so."
  (when (nt-mask--enough-space? mask)
    (move-overlay mask
                  (overlay-start mask)
                  (nt-mask->opaque-end mask))))

(defun nt-mask--render (mask)
  "Set display-based overlay properties for MASK."
  (-doto mask
    (overlay-put 'face    'underline)
    (overlay-put 'display " ")))

(defun nt-mask--unrender (mask)
  "Remove display-based overlay properties for MASK."
  (-doto mask
    (overlay-put 'face    nil)
    (overlay-put 'display nil)))

(defun nt-masks--render (masks)
  "Set display-based overlay properties for MASKS."
  (-each masks #'nt-mask--render))

(defun nt-masks--unrender (masks)
  "Remove display-based overlay properties for MASKS."
  (-each masks #'nt-mask--unrender))

(defun nt-masks--render-buffer (&rest _)
  "Set display-based overlay properties for masks in buffer (as a hook)."
  (nt-masks--render nt-mask-list))

(defun nt-masks--unrender-buffer (&rest _)
  "Remove display-based overlay properties for masks in buffer (as a hook)."
  (nt-masks--unrender nt-mask-list))

(defun nt-mask--reset-display (mask)
  "Reset display and face text properties of MASK."
  (if (nt-mask--render? mask)
      (nt-mask--render mask)
    (nt-mask--unrender mask)))

(defun nt-mask--reset-opaque-end (mask)
  "Update MASK's 'opaque-end based on contributing ligatures."
  (overlay-put mask 'nt-opaque-end
               (+ 1 (overlay-start mask) (nt-mask->width mask))))

(defun nt-mask--refresh (mask)
  "Reset bounds and boundary-dependent properties of MASK based on its ligs."
  ;; These mutations must occur in the order presented
  (-doto mask
    (nt-mask--reset-opaque-end)
    (nt-mask--recenter-maybe)
    (nt-mask--reset-prefix)
    (nt-mask--reset-display)))

(defun nt-mask--refresh-maybe (mask)
  "Perform `nt-mask--refresh' when we should and return back MASK."
  (unless nt-mask--wait-for-refresh
    (nt-mask--refresh mask))
  mask)

(defun nt-masks--refresh (masks)
  "Refresh MASKS."
  (-map #'nt-mask--refresh-maybe masks))

(defun nt-masks--refresh-buffer ()
  "Refresh `nt-mask-list'."
  (nt-masks--refresh nt-mask-list))



;;; Init

(defun nt-mask--init-ov (ov)
  "Put always-on text properties for masks into OV."
  (-doto ov
    (overlay-put 'nt?      t)
    (overlay-put 'nt-mask? t)
    (overlay-put 'nt-ligs  nil)
    (overlay-put 'nt-opaque-end (overlay-end ov))

    (overlay-put 'modification-hooks '(nt-mask--decompose-hook))))

(defun nt-mask--init (&optional line)
  "Create empty mask for LINE, otherwise current line."
  (save-excursion
    (when line (nt-base--goto-line line))

    (let* ((line  (line-number-at-pos))
           (start (line-beginning-position))
           (end   (1+ start))
           (mask  (nt-mask--init-ov (make-overlay start end))))
      (nt-mask--insert-at mask line)
      mask)))

(defun nt-masks--init (&optional start-line end-line)
  "Line-by-line buildup `nt-mask-list', optionally [a b) bounded start/end."
  (save-excursion
    (nt-base--goto-line (or start-line 1))

    (while (and (not (eobp))
                (if end-line (< (line-number-at-pos) end-line) t))
      (nt-mask--init)
      (forward-line))))



(provide 'nt-mask)



;;; nt-mask.el ends here
