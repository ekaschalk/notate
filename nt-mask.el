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


(defvar nt-mask--wait-for-refresh? nil
  "Let-bind true to hold off on refreshing masks during batch updates.")


(defvar nt-mask--init-in-progress? nil
  "Are we instantiating the initial masks?")

;;; Access
;;;; Fundamentals

(defun nt-mask<-line (line)
  "Get mask at LINE."
  (-some-> line 1- (nth nt-masks)))

(defun nt-masks<-lines (start-line end-line)
  "Get masks in [START-LINE END-LINE)."
  (-slice nt-masks (1- start-line) (1- end-line)))

;;;; Extensions

(defun nt-masks<-region (start end)
  "Get masks in START and END."
  (apply #'nt-masks<-lines (nt-lines<-region start end)))

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

;;; Predicates
;;;; General Purpose

(defun nt-mask--empty? (mask)
  "Does MASK have no notes?"
  (-> mask nt-mask->notes null))

(defun nt-mask--contains? (note mask)
  "Does MASK contain NOTE?"
  (-> mask nt-mask->notes (-contains? note)))

;;;; Rendering

(defun nt-mask--enough-space? (mask)
  "Does MASK's line contain enough space for rendering?"
  (= (nt-mask->line mask)
     (nt-mask->opaque-line mask)))

(defun nt-mask--ends-agree? (mask)
  "Does MASK's opaque-end and actual end agree?"
  (= (overlay-end mask)
     (nt-mask->opaque-end mask)))

(defun nt-mask--render? (mask)
  "Should MASK be rendered?"
  (and nt-render-masks?
       (not (nt-mask--empty? mask))
       (nt-mask--ends-agree? mask)))

;;; Management
;;;; Insertion

(defun nt-mask--insert-sorted (mask)
  "Insert MASK into `nt-masks' maintaining order."
  (setq nt-masks
        (-some-> mask nt-mask->line 1- (-insert-at mask nt-masks))))

(defun nt-mask--insert (mask)
  "Insert MASK into `nt-masks' according to the current context."
  (if nt-mask--init-in-progress?
      (!cons mask nt-masks)
    (nt-mask--insert-sorted mask)))

;;;; Deletion

(defun nt-mask--delete (mask)
  "Delete MASK."
  (delq mask nt-masks)
  (delete-overlay mask))

;; TODO test first
(defun nt-mask--delete-lines (start-line end-line)
  "Delete masks in [START-LINE END-LINE)."
  ;; (-each #'delete-overlay (nt-masks<-lines start-line end-line))
  ;; (setq nt-masks
  ;;       (append (nt-masks<-lines 0 start-line)
  ;;               (nt-masks<-lines end-line (length nt-masks))))
  )

;;; Decomposition

(defun nt-mask--decompose (mask)
  "Workhorse of `nt-mask--decompose-hook'."
  ;; TODO This function will likely be hard to implement perfectly, probably
  ;; harder than note's decomposition hook...

  ;; Few observations:
  ;; 1. probably need to handle deleting forward differently
  ;; 2. probably need to handle visual deletion differently
  ;; 3. probably interacting in a bad way with `undo-tree-undo'

  (let* ((inhibit-modification-hooks t)
         (width                      (nt-mask->width mask))
         (invis-spaces-to-delete     (1+ width)))
    (nt-mask--delete mask)
    (evil-with-single-undo
      (delete-char (- invis-spaces-to-delete)))))

(defun nt-mask--decompose-hook (mask post-mod? start end &optional _)
  "Decompose MASK upon modification as a modification-hook."
  (when post-mod?
    (nt-mask--decompose mask)))

;;; Prefixes

(defun nt-mask--format-prefix (mask)
  "Format the `line-prefix' overlay text property for MASK."
  (->>
   (list (-> "%02d" (format (nt-mask->indent mask)))
         (-> "%02d" (format (nt-mask->width mask)))
         (-> "+%d " (format (length (nt-mask->notes mask)))))
   (-interpose "|")
   (apply #'s-concat)))

;;; Refreshing
;;;; Internal
;;;;; Rendering

(defun nt-mask--render (mask)
  "Set display-based overlay properties for MASK."
  (-doto mask
    (overlay-put 'face    (if nt-display-render-status? 'underline nil))
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
  (nt-masks--render nt-masks))

(defun nt-masks--unrender-buffer (&rest _)
  "Remove display-based overlay properties for masks in buffer (as a hook)."
  (nt-masks--unrender nt-masks))

;;;;; Components

(defun nt-mask--reset-opaque-end (mask)
  "Update MASK's 'opaque-end based on contributing notes."
  (overlay-put mask 'nt-opaque-end
               (+ 1 (overlay-start mask) (nt-mask->width mask))))

(defun nt-mask--recenter-maybe (mask)
  "Recenter MASK if it won't cross lines doing so."
  (when (nt-mask--enough-space? mask)
    (move-overlay mask
                  (overlay-start mask)
                  (nt-mask->opaque-end mask))))

(defun nt-mask--reset-display (mask)
  "Reset display and face text properties of MASK."
  (if (nt-mask--render? mask)
      (nt-mask--render mask)
    (nt-mask--unrender mask)))

(defun nt-mask--reset-prefix (mask)
  "Reset the `line-prefix' text property for MASK."
  (when nt-display-prefixes?
    (->> mask nt-mask--format-prefix (overlay-put mask 'line-prefix))))

;; TODO Not in use yet, to test
;; (defun nt-mask--refresh-notes (mask)
;;   "Remove deleted notes from MASK."
;;   (setf (overlay-get mask 'nt-notes)
;;         (->> mask nt-mask->notes (-filter #'nt-ov--deleted?))))

;;;; Exposes

(defun nt-mask--refresh (mask)
  "Reset bounds and boundary-dependent properties of MASK based on its notes."
  ;; The ordering here is _not_ arbitrary
  (-doto mask
    (nt-mask--reset-opaque-end)
    (nt-mask--recenter-maybe)
    (nt-mask--reset-prefix)
    (nt-mask--reset-display)))

(defun nt-mask--refresh-maybe (mask)
  "Perform `nt-mask--refresh' when we should and return back MASK."
  (unless nt-mask--wait-for-refresh?
    (nt-mask--refresh mask))
  mask)

(defun nt-masks--refresh (masks)
  "Refresh MASKS."
  (-map #'nt-mask--refresh-maybe masks))

(defun nt-masks--refresh-region (start end)
  "Regresh masks in START and END."
  (nt-masks--refresh (nt-masks<-region start end)))

(defun nt-masks--refresh-buffer ()
  "Refresh `nt-masks'."
  (nt-masks--refresh nt-masks))

;;; Init

(defun nt-mask--init-ov (ov)
  "Put always-on text properties for masks into OV."
  (-doto ov
    (overlay-put 'nt?      t)
    (overlay-put 'nt-mask? t)
    (overlay-put 'nt-notes  nil)
    (overlay-put 'nt-opaque-end (overlay-end ov))

    (overlay-put 'modification-hooks '(nt-mask--decompose-hook))))

(defun nt-mask--init (&optional line)
  "Create empty mask for LINE, otherwise current line."
  (save-excursion
    (when line (nt-line--goto line))

    (let* ((line  (line-number-at-pos))
           (start (line-beginning-position))
           (end   (1+ start))
           (mask  (nt-mask--init-ov (make-overlay start end))))
      (nt-mask--insert mask)
      mask)))

(defun nt-masks--init (&optional start-line end-line)
  "Line-by-line buildup `nt-masks', optionally [a b) bounded start/end."
  (let ((nt-mask--init-in-progress? t))
    (save-excursion
      (nt-line--goto (or start-line 1))

      (while (and (not (eobp))
                  (if end-line (< (line-number-at-pos) end-line) t))
        (nt-mask--init)
        (forward-line)))

    (setq nt-masks (reverse nt-masks))))

;;; Provide

(provide 'nt-mask)

;;; nt-mask.el ends here
