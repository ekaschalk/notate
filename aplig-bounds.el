;;; aplig-bounds.el --- Indent Boundaries -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Major-mode-dependent functions for calculating boundaries of ligature's
;; effects on indentation masks.



;;; Code:
;;;; Requires

(require 'aplig-base)

(require 'aplig-ov)



;;; Lisps

(defun aplig-bounds?--lisps-heuristic (lig)
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

(defun aplig-bounds--lisps (lig)
  "Calculate line boundary [a b) for LIG's masks."
  (let* ((start (overlay-start lig))
         (line (line-number-at-pos start))
         (max-line (line-number-at-pos (point-max))))
    (list (min line max-line)
          (save-excursion
            (goto-char start)
            (sp-end-of-sexp)
            (line-number-at-pos)))))

(defun aplig-bounds?--lisps (lig)
  "Does LIG have an indentation boundary? Return back LIG if it does."
  ;; Always-correct boundary-fn much more involved than a good-enough heuristic
  ;; It must correspond to `lisp-indent-function', but given its calling
  ;; convention, likely difficult to emulate in a non-temp-buffer-based impl.
  (and (aplig-bounds?--lisps-heuristic lig)
       lig))



(provide 'aplig-bounds)



;;; aplig-bounds.el ends here
