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
;;;; Predicates
;;;;; Conditions

(defun aplig-bounds?--lisps-form-opener? (lig)
  "Does LIG open a form?"
  (save-excursion
    (aplig-ov--goto lig)
    (null (ignore-errors (backward-sexp) t))))

;; TODO Straightforward (descend and check line)
(defun aplig-bounds?--lisps-another-form-opener-same-line? (lig)
  "Does LIG have another form opener on the same line?

(foo lig (foo foo
              foo))

Has LIG contributing to indentation masks even though it is not a form opener."
  nil)

;; TODO Straightforward (next and check line)
(defun aplig-bounds?--lisps-terminal-sexp? (lig)
  "Is LIG the terminal sexp on its line?

(lig
 foo)

Does not have LIG contributing to indentation masks though it is a form opener."
  nil)

;; TODO Not sure where to start on this one Might have to learn how to inspect
;; function properties and how (declare indent) works in-depth
(defun aplig-bounds?--lisps-specially-indented? (lig)
  "Do we have to account for indentation declarations?"
  nil)

;;;;; Composition

(defun aplig-bounds?--lisps (lig)
  "Does LIG have an indentation boundary? If so give LIG."
  (funcall
   (-andfn (-orfn #'aplig-bounds?--lisps-another-form-opener-same-line?
                  #'aplig-bounds?--lisps-form-opener?)
           (-not #'aplig-bounds?--lisps-terminal-sexp?)
           (-not #'aplig-bounds?--lisps-specially-indented?)
           #'identity)
   lig))

;;;; Range

(defun aplig-bounds--lisps (lig)
  "Calculate line boundary [a b) for LIG's masks."
  (let* ((start (overlay-start lig))
         (line (1+ (line-number-at-pos start)))
         (max-line (line-number-at-pos (point-max))))
    (list (min line max-line)
          (save-excursion
            (goto-char start)
            (sp-end-of-sexp)
            (1+ (line-number-at-pos))))))



(provide 'aplig-bounds)



;;; aplig-bounds.el ends here
