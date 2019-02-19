;;; aplig-lig.el --- Ligature Overlays -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Ligature overlay management, instantiation, etc.



;;; Code:
;;;; Requires

(require 'aplig-base)

(require 'aplig-ov)



;;; Boundary Functions

(defun aplig-lig--boundary--lisps (lig)
  "Calculate line boundary [a b) for LIG's masks."
  (let* ((start (overlay-start lig))
         (line (line-number-at-pos start))
         (max-line (line-number-at-pos (point-max))))
    (list (min line max-line)
          (save-excursion
            (goto-char start)
            (sp-end-of-sexp)
            (line-number-at-pos)))))

(defun aplig-lig--boundary?-fn--heuristic--lisps (lig)
  "Hueristic version of boundary predicate."
  (save-excursion
    (aplig-ov--goto lig)

    (let* ((at-form-opener? (null (ignore-errors (backward-sexp) t)))

           ;; (declare indent)
           in-specially-indented-body?

           ;; (foo lig (foo foo
           ;;               foo))
           another-form-opener-same-line?

           ;; (lig
           ;;  foo)
           only-sexp-on-its-line?)
      (and at-form-opener?
           (not in-specially-indented-body?)
           (not another-form-opener-same-line?)
           (not only-sexp-on-its-line?)))))

(defun aplig-lig--boundary?--lisps (lig)
  "Does LIG have an indentation boundary? Return back LIG if it does."
  ;; Always-correct boundary-fn much more involved than a good-enough heuristic
  ;; It must correspond to `lisp-indent-function', but given its calling
  ;; convention, likely difficult to emulate in a non-temp-buffer-based impl.
  (and (aplig-lig--boundary?-fn--heuristic--lisps lig)
       lig))



;;; Overlays

(defun aplig-ligs--present? (&optional start end)
  "Are ligs present within START and END, defaulting to `match-data'? Get it."
  (let ((start (or start (match-beginning 1)))
        (end   (or end (match-end 1))))
    (and start end
         (-filter #'aplig-ov--lig?
                  (overlays-in start end)))))

(defun aplig-ligs--at (line)
  "Return all ligs on LINE."
  (aplig-ligs--present?
   (save-excursion (goto-line line) (line-beginning-position))
   (line-end-position)))

(defun aplig-lig--delete (lig)
  "Delete LIG."
  (delq lig aplig-lig-list)
  (delete-overlay lig))

(defun aplig-lig--decompose-hook (lig post-modification? start end &optional _)
  "Decompose LIG upon modification as a modification-hook."
  (when post-modification?
    (aplig-lig-mask--remove-lig-from-masks lig)
    (aplig-lig--delete lig)))



;;; Transforms

(defun aplig-ligs->width (ligs)
  "Sum widths of LIGS."
  (-> ligs (aplig-ovs--prop 'aplig-width) -sum))



;;; Init

(defun aplig-lig--init-ov (ov replacement width)
  "Put lig text properties into OV."
  (-doto ov
    (overlay-put 'aplig?      t)
    (overlay-put 'aplig-lig?  t)
    (overlay-put 'aplig-width width)

    (overlay-put 'display replacement)
    (overlay-put 'modification-hooks '(aplig-lig--decompose-hook))))

(defun aplig-lig--init (replacement width &optional start end)
  "Build ligature overlay, defaulting to `match-data' for START and END."
  (unless (or (or start (match-beginning 1))
              (or end   (match-end 1)))
    (error "Initiatializing ligature without match-data set."))

  (let* ((start (or start (match-beginning 1)))
         (end   (or end (match-end 1)))
         (ov    (make-overlay start end))
         (lig   (aplig-lig--init-ov ov replacement width)))
    (push lig aplig-lig-list)
    (aplig-lig-mask--add-lig-to-masks lig)
    lig))



(provide 'aplig-lig)



;;; aplig-lig.el ends here