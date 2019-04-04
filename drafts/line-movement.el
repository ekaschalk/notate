;; SECTION IN-DEVELOPMENT

;; (defun nt--mask-line-movement (line-fn &rest args)
;;   "Advises line movement to preserve visual rather than true column.

;; There are several challenges to overcome:
;; 1. Overlays with 'display length shorter than the covered region (notes).
;; 2. Masked indentation satisfies 1. but has additional considerations.
;; 3. Must notify `temporary-goal-column'.
;;    - Deals with columns when moving touches EOLs.
;;    - Normally exposed via `goal-column', but that doesn't quite fit here.

;; Advising `next-line' also updates `evil-line-move' as appropriate."
;;   (-let* (;; Lines
;;           ((line-count)
;;            args)
;;           (start-line
;;            (line-number-at-pos))
;;           (end-line
;;            (+ start-line line-count))
;;           (end-line-start
;;            (nt-line->start end-line))

;;           ;; Masks
;;           (start-mask
;;            (nt-mask<-line start-line))
;;           (end-mask
;;            (nt-mask<-line end-line))
;;           (end-mask-rendered?
;;            (nt-mask--render? end-mask))

;;           ;; Notes
;;           (notes-masking-cols-at-start
;;            ;; (nt-notes<-region (line-beginning-position) (point))
;;            (-intersection (nt-notes<-region (line-beginning-position) (point))
;;                           (nt-mask->notes end-mask)))
;;           (notes-masking-cols-at-end
;;            (nt-notes<-region end-line-start (+ end-line-start (current-column))))

;;           ;; Column Offset Calculations
;;           (indent-offset
;;            (- (nt-mask->width end-mask)
;;               (nt-mask->width start-mask)))
;;           (note-offset
;;            (- (nt-notes->width notes-masking-cols-at-end)
;;               (nt-notes->width notes-masking-cols-at-start)))
;;           (col-offset
;;            (+ indent-offset note-offset)))
;;     ;; Call `next-line'
;;     (apply line-fn args)

;;     ;; Apply column offset to point and `temporary-goal-column'
;;     (cond
;;      ;; Tasks on this case:
;;      ;; 1. Need similar eol goal-col check here
;;      ;; 2. store render status so don't have to recalculate
;;      ((not end-mask-rendered?)
;;       (cl-incf temporary-goal-column col-offset))

;;      (
;;       ;; (> 0 col-offset)
;;       t
;;       (progn
;;         (forward-char col-offset)
;;         (when (and (> 0 col-offset)
;;                    (<= temporary-goal-column
;;                        (nt-line->end-col (line-number-at-pos))))
;;           (setq temporary-goal-column (current-column))))))))


;; (message "true-goal %s vis-goal %s vis-col %s"
;;          temporary-goal-column
;;          nt--temporary-goal-column-vis
;;          (nt--pos->vis-col start))



;; (col-limit (nt-line->end-col end-line))
;; (vis-col-limit (nt--pos->vis-col (line-end-position end-line)))

;; (defun nt--eol-tracking? ()
;;   "Is `temporary-goal-column' referencing arbitrary EOLs?"
;;   (= most-positive-fixnum temporary-goal-column))

;; (let ((vis-col (nt--goto-temporary-vis-col)))
;;   (if (= vis-col nt--temporary-goal-column-vis)
;;       (progn
;;         (setq nt--temporary-goal-column-vis nil)
;;         (setq temporary-goal-column (current-column)))
;;     (when (= most-positive-fixnum temporary-goal-column)
;;       (setq nt--temporary-goal-column-vis temporary-goal-column)
;;       )))

;; (defun nt--goto-temporary-vis-col ()
;;   (when (= nt--temporary-goal-column-vis
;;            (nt--goto-vis-col nt--temporary-goal-column-vis))
;;     (setq nt--temporary-goal-column-vis nil)
;;     (setq temporary-goal-column (current-column))))

;; Apply column offset to point and `temporary-goal-column'
;; (cond
;;  ;; Tasks on this case:
;;  ;; 1. Need similar eol goal-col check here
;;  ;; 2. store render status so don't have to recalculate
;;  ((not end-mask-rendered?)
;;   (cl-incf temporary-goal-column col-offset))

;;  (
;;   ;; (> 0 col-offset)
;;   t
;;   (progn
;;     (forward-char col-offset)
;;     (when (and (> 0 col-offset)
;;                (<= temporary-goal-column
;;                   (nt-line->end-col (line-number-at-pos))))
;;       (setq temporary-goal-column (current-column))))))

;; (end (nt--col+line->pos col end-line))
;; (vis-col-end (nt--pos->vis-col end))
;; (vis-col-offset (- vis-col-end vis-col-start))
;; (end-line-start (nt-line->start end-line))
;; (-let* ((max-col (nt-line->end-col end-line))))

;; (setq nt--temporary-goal-column-vis
;;       (or nt--temporary-goal-column-vis temporary-goal-column))
;; this right here (was) the culprit
;; the col/vis-col are equal -> temp vis goal becomes true goal

;; When we enter first contained empty line
;; vis-goal should be set to 2
;; instead it is being set to 6 (the temporary-goal-column)

;; (setq nt--temporary-goal-column-vis
;;       (- temporary-goal-column
;;          (- (nt--pos->col (line-end-position))
;;             (nt--pos->vis-col (line-end-position)))))
