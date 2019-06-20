;;; nt-mask.el --- Indentation Masks -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Indentation mask management, instantiation, transforms, etc.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)

;;; Configuration
;;;; Managed

(defvar nt-masks nil
  "Line-ordered list of indent overlays, covering the buffer.

Always access through `nt-mask<-line' and friends to not confuse 0 vs 1-idxing.
Eventually rewrite with vector for constant-time idxing.")


(defvar-local nt-mask--wait-for-refresh? nil
  "Let-bind true to hold off on refreshing masks during batch note updates.")


(defvar-local nt-mask--init-in-progress? nil
  "Are we instantiating the initial masks?")

;;; Access
;;;; Fundamentals

(defun nt-mask<-line (line)
  "Get mask at LINE."
  (-some-> line nt-line->idx (nth nt-masks)))

(defun nt-mask<-line-raw (line)
  "Get mask at LINE, via overlay methods for when `nt-masks' unreliable."
  (-> line
     nt-line->start
     nt-ov<-pos
     nt-ov--mask?))

(defun nt-masks<-lines (start-line end-line)
  "Get masks in [START-LINE END-LINE)."
  (let ((start (nt-line->idx start-line))
        (end (nt-line->idx end-line)))
    (-slice nt-masks (or start 0) end)))

;;;; Extensions

(defun nt-masks<-region (start end)
  "Get masks in lines containing START through END."
  (-some->> (nt-lines<-region start end) (apply #'nt-masks<-lines)))

;;; Transforms
;;;; Overlay Wrappers

(defun nt-mask->line (mask)
  "Access MASK's line."
  (-some-> mask overlay-start line-number-at-pos))

(defun nt-mask->notes (mask)
  "Access MASK's notes."
  (-some-> mask (overlay-get 'nt-notes)))

(defun nt-mask->opaque-end (mask)
  "Access MASK's opaque-end."
  (-some-> mask (overlay-get 'nt-opaque-end)))

(defun nt-mask->true-end (mask)
  "Alias for `overlay-end'."
  (and mask (overlay-end mask)))

;;;; Misc

(defun nt-mask->opaque-line (mask)
  "Get line of MASK's opaque-end."
  (-some-> mask nt-mask->opaque-end line-number-at-pos))

(defun nt-mask->indent (mask)
  "Get true indent of MASK's line."
  (save-excursion (nt-ov--goto mask) (nt-line->indent-col)))

(defun nt-mask->width (mask)
  "Calculate width of MASK's notes."
  (-> mask nt-mask->notes nt-notes->width))

(defun nt-mask->idx (mask)
  "Get index of insertion of new MASK into `nt-masks'."
  (-> mask nt-mask->line nt-line->idx))

(defun nt-mask->line-prefix (mask)
  "Format the `line-prefix' overlay text property for MASK."
  (->>
   (list (-> "%02d" (format (nt-mask->indent mask)))
         (-> "%02d" (format (nt-mask->width mask)))
         (-> "+%d " (format (length (nt-mask->notes mask)))))
   (-interpose "|")
   (apply #'s-concat)))

;;; Predicates
;;;; General Purpose

(defun nt-mask--empty? (mask)
  "Does MASK have no notes?"
  (-> mask nt-mask->notes null))

(defun nt-mask--nonempty? (mask)
  "Does MASK contain notes?"
  (-> mask nt-mask--empty? null))

(defun nt-mask--contains? (note mask)
  "Does MASK contain NOTE?"
  (-> mask nt-mask->notes (-contains? note)))

;;;; Rendering

(defun nt-mask--enough-space? (mask)
  "Does MASK's line contain enough width for rendering?"
  (= (nt-mask->line mask)
     (nt-mask->opaque-line mask)))

;; Potentially this just replaces `nt-mask--enough-space?' entirely
(defun nt-mask--enough-whitespace? (mask)
  "Does MASK's line contain enough whitespace for rendering?"
  (<= (nt-mask->width mask) (nt-mask->indent mask)))

(defun nt-mask--ends-agree? (mask)
  "Does MASK's opaque-end and actual end agree?"
  (= (nt-mask->true-end mask)
     (nt-mask->opaque-end mask)))

(defun nt-mask--render? (mask)
  "Should MASK be rendered?"
  (and nt-render-masks?
       (nt-mask--nonempty? mask)
       (nt-mask--ends-agree? mask)))

;;; Management
;;;; Insertion

(defun nt-mask--insert-sorted (mask)
  "Get `nt-masks' with MASK inserted maintaining order."
  (-> mask nt-mask->idx (-insert-at mask nt-masks)))

(defun nt-mask--insert (mask)
  "Insert MASK into `nt-masks' according to the current context."
  (if nt-mask--init-in-progress?
      (!cons mask nt-masks)
    (setq nt-masks (nt-mask--insert-sorted mask))))

;;;; Deletion

(defun nt-mask--delete (mask)
  "Delete MASK."
  (delq mask nt-masks)
  (delete-overlay mask))

(defun nt-mask--delete-lines (start-line end-line)
  "Delete masks in [START-LINE END-LINE)."
  ;; Later do a filter to optimize batch mask deletion.
  ;; Just like how note deletions are handled.
  (-each #'nt-mask--delete (nt-masks<-lines start-line end-line)))

;;; Decomposition

;; Could one decompose only a part of the mask? Things like iedit won't ever
;; touch masks (because spaces). Maybe multi-cursors might? Not sure how though.
;; The decomposition can be cleaned up abit later.

(defun nt-mask--decompose-hook (mask post-mod? start end &optional _)
  "Decompose MASK upon modification as a modification-hook."
  (let* ((deletion-length
          (- end start))
         (mask-length
          (nt-ov->length mask))
         (decompose-length
          (- mask-length deletion-length))
         (whole-mask-deleted?  ; Still need to check edge-cases, but working atm
          (<= decompose-length 0)))
    (when post-mod?
      (nt-mask--delete mask)

      (unless whole-mask-deleted?
        (let* ((line-start
                (line-beginning-position))
               (masked-indent
                (+ line-start (current-column))))
          (delete-region line-start masked-indent))))))

;;; Refreshing
;;;; Internal

;; These internal components of refreshing are organized in the order they
;; should take place, see the composition `nt-mask--refresh-internal'.

;;;;; Notes

(defun nt-mask--refresh-notes (mask)
  "Remove deleted notes from MASK."
  (setf (overlay-get mask 'nt-notes)
        (->> mask nt-mask->notes (-remove #'nt-ov--deleted?))))

;;;;; End Positions

(defun nt-mask--refresh-opaque-end-internal (mask)
  "Reset MASK's 'opaque-end based on its notes."
  (let ((mask-width (nt-mask->width mask))
        (start (overlay-start mask)))
    (overlay-put mask 'nt-opaque-end (+ 1 start mask-width))))

(defun nt-mask--refresh-end-internal (mask)
  "Reset MASK's true end based on its 'opaque-end."
  (-some->> mask nt-mask->opaque-end (nt-ov--extend mask)))

(defun nt-mask--refresh-ends (mask)
  "Reset MASK's 'opaque-end, and possibly true-end if there is space to do so."
  (nt-mask--refresh-opaque-end-internal mask)
  (when (and (nt-mask--enough-space? mask)
             (nt-mask--enough-whitespace? mask))
    (nt-mask--refresh-end-internal mask)))

;;;;; Prefixes

(defun nt-mask--refresh-prefix (mask)
  "Reset the `line-prefix' text property for MASK."
  (when nt-display-prefixes?
    (->> mask nt-mask->line-prefix (overlay-put mask 'line-prefix))))

;;;;; Rendering

(defun nt-mask--refresh-render (mask)
  "Reset rendering status of MASK."
  (let ((render? (nt-mask--render? mask)))
    (-doto mask
      (overlay-put 'display (and render? " "))
      (overlay-put 'face (and render? nt-display-render-status? 'underline)))))

;;;; Commands

(defun nt-mask--refresh-internal (mask)
  "Reset bounds and boundary-dependent properties of MASK based on its notes."
  (-doto mask  ; Refreshing ordering here not arbitrary
    (nt-mask--refresh-notes)
    (nt-mask--refresh-ends)
    (nt-mask--refresh-prefix)
    (nt-mask--refresh-render)))

(defun nt-mask--refresh (mask)
  "Refresh and give back MASK."
  (unless nt-mask--wait-for-refresh?
    (nt-mask--refresh-internal mask))
  mask)

(defun nt-masks--refresh (masks)
  "Refresh and give back MASKS."
  (-map #'nt-mask--refresh masks))

(defun nt-masks--refresh-region (start end)
  "Refresh and give back masks in START and END."
  (nt-masks--refresh (nt-masks<-region start end)))

(defun nt-masks--refresh-lines (start-line end-line)
  "Refresh and give back masks in [START-LINE END-LINE)."
  (nt-masks--refresh (nt-masks<-lines start-line end-line)))

(defun nt-masks--refresh-buffer ()
  "Refresh `nt-masks'."
  (nt-masks--refresh nt-masks))

;;; Init

(defun nt-mask--init-ov (start end)
  "Instantiate mask overlay and its properties for `nt-mask--init'.

Notate Text Properties
  'nt?:           A notate overlay.
  'nt-mask?:      A mask overlay.
  'nt-notes:      List of notes currently contributing to the mask.
  'nt-opaque-end: End takes value nt-opaque-end only when we won't cross lines.
"
  (-doto (make-overlay start end)
    (overlay-put 'nt?      t)
    (overlay-put 'nt-mask? t)
    (overlay-put 'nt-notes nil)
    (overlay-put 'nt-opaque-end end)
    ;; TODO Save 'nt-rendered? property as an optimization

    ;; TODO Investigate effects of 'intangible (emacs docs says be careful)
    (overlay-put 'modification-hooks '(nt-mask--decompose-hook))))

(defun nt-mask--init (&optional line)
  "Build empty mask for LINE, otherwise current line."
  (let* ((line
          (or line (line-number-at-pos)))
         (start
          (nt-line->start line))
         (mask
          (nt-mask--init-ov start (1+ start))))
    (nt-mask--insert mask)
    mask))

(defun nt-masks--init (&optional start-line end-line)
  "Line-by-line buildup `nt-masks', optionally [START-LINE END-LINE) bounded."
  (let ((nt-mask--init-in-progress? t))
    (nt-lines--foreach start-line end-line
      (nt-mask--init))

    ;; Later just build up nt-masks by backward-lining to avoid this call
    (setq nt-masks (reverse nt-masks))))

;;; Provide

(provide 'nt-mask)

;;; nt-mask.el ends here
