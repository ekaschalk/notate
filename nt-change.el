;;; nt-change.el --- Buffer Modification Support -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; UNDER DEVELOPMENT

;; Support for buffer modifications, ie. text-editing.

;;; Code:
;;;; Requires

(require 'nt-base)

;;; Hook

(defun nt-change--after-change-function (start end chars-deleted)
  "See `after-change-functions', dispatches on insertion or deletion."
  ;; (let ((inhibit-modification-hooks t))  ; Don't believe I will need this
  (if (= 0 chars-deleted)
      (nt-change--insertion start end)
    (nt-change--deletion start chars-deleted)))

;;; Notes

;; ASSUMING BALANCED DELETION CURRENTLY
;; If I do end up needing a tree
;;  can recursively call the `nt-notes->roots' on each `-drop-while' section

;; A better version would trim-out every note in separate subtrees
;; but taking everything contained in the root is still far better than nothing
;; and likely to be close to optimized anyway in most situations

;; Further I can just store the roots and point to their children which
;; should be fine in most cases

;;; Utilities
;;;; Lines

(defun nt-change--lines-deleted? ()
  "Have lines been deleted (since last time updating masks)?"
  (let ((count (- (length nt-masks)
                  (line-number-at-pos (point-max)))))
    (and (< 0 count) count)))

(defun nt-change--lines-added? ()
  "Have lines been added (since last time updating masks)?"
  (let ((count (- (line-number-at-pos (point-max))
                  (length nt-masks))))
    (and (< 0 count) count)))

;;;; Outer Regions

(defun nt-change--pos->outer-region (pos)
  "Get region of the outermost form's start/end containing POS."
  (save-excursion
    (-when-let* ((syntax (syntax-ppss pos))
                 ((outer-parens-start) (nth 9 syntax)))
      (goto-char outer-parens-start)
      (list outer-parens-start
            (progn (forward-char)
                   (and (sp-up-sexp)  ; Goes to point-max if no closing pair
                        (point)))))))

(defun nt-change--region->outer-region (start end)
  "Get region of outermost forms containing START to containing END."
  (-let (((outer-start) (nt-change--pos->outer-region start))
         ((_ outer-end) (nt-change--pos->outer-region end)))
    (list (or outer-start (point-min))
          (or outer-end (point-max)))))

(defun nt-change--update-bounded-outer-region (start end)
  "Perform `nt-notes--update-bounded-region' on the maximal outer-region."
  ;; This function can be replaced (but will be potentially much slower) with:
  (nt-notes--update-bounded-buffer)
  ;; (let ((outer-region (nt-change--region->outer-region start end)))
  ;;   (apply #'nt-notes--update-bounded-region outer-region))
  )

;;; Mask Interaction

(defun nt-change--init-masks-in-region (start end)
  "Init masks for newly made lines within START and END."
  (let* ((start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (mask-at-start? (nt-mask<-line-raw start-line))
         (mask-at-end? (nt-mask<-line-raw end-line)))
    (when mask-at-start?
      (cl-incf start-line))
    (when mask-at-end?
      (cl-decf end-line))

    (-each (number-sequence start-line end-line) #'nt-mask--init)))

;;; Insertion

(defun nt-change--insertion (start end)
  "Change func specialized for insertion, in START and END."
  (nt-change--init-masks-in-region start end)
  (nt-change--update-bounded-outer-region start end))

;;; Deletion

(defun nt-change--deletion (pos chars-deleted)
  "Change function specialized for deletion, number CHARS-DELETED at POS.

Note that the 'modification-hook text property handles decomposing note and mask
overlays. Change functions update mask lengths and rendering status."
  (nt-notes--update-bounded-buffer)
  ;; (nt-change--update-bounded-outer-region pos pos)
  )

;;; Provide

(provide 'nt-change)

;;; nt-change.el ends here
