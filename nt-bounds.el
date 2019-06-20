;;; nt-bounds.el --- Indent Boundaries -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Calculate boundaries of notes effects on indentation masks. Major-mode
;; dependent functions are implemented here.

;; Other modules should only be interested in `nt-bound' and `nt-bound?'

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)

;;; Exposes

(defun nt-bound (note)
  "Call `nt-bound-fn' on NOTE."
  (funcall nt-bound-fn note))

(defun nt-bound? (note)
  "Call `nt-bound?-fn' on NOTE."
  (funcall nt-bound?-fn note))

;;; Language Agnostic

(defun nt-bounds?--in-string-or-comment? (note)
  "Is NOTE contained within a string or comment?"
  (-> note nt-ov->syntax nt-syntax->string-or-comment))

(defun nt-bounds?--ignore? (note)
  "Should NOTE, identified by `nt-ignore-notes', never modify the indent?

Provides an entry point for users to modify indentation masking behavior
of particular notes. I do not believe there is a reason for the converse
to be implemented."
  (-contains? nt-ignore-notes (nt-ov->string note)))

;;; Lisps - Emacs Lisp Only

(defun nt-bounds?--elisp-indent-declared? (note)
  "Returns non-nil if indendation declarations are attached to NOTE."
  (-some-> note nt-ov->symbol (function-get 'lisp-indent-function)))

;;; Lisps - General
;;;; Predicates
;;;;; Components

(defun nt-bounds?--lisps-terminal-sexp? (note)
  "Is NOTE the terminal sexp on its line?

  (note
   foo)

Does not have NOTE contributing to indentation masks though it is a form opener."
  (save-excursion
    (nt-ov--goto note)

    (let ((line-start (line-number-at-pos)))
      (ignore-errors (forward-sexp))
      (nt-syntax--line-empty-after-point))))

(defun nt-bounds?--lisps-another-form-opener-on-line? (note)
  "Does NOTE have another form opener on the same line?

  (foo note (foo foo
                 foo))

Has NOTE contributing to indentation masks even though it is not a form opener."
  (save-excursion
    (nt-ov--goto note)

    (let ((line-start (line-number-at-pos))
          (depth (nt-syntax--depth-at-point)))
      (sp-down-sexp)

      (and (> (nt-syntax--depth-at-point) depth)
           (= (line-number-at-pos) line-start)))))

(defun nt-bounds?--lisps-form-opener? (note)
  "Does NOTE open a form?

  (note foo
        bar)

Simplest case that has NOTE contributing to indentation masks."
  (save-excursion
    (nt-ov--goto note)
    (null (ignore-errors (backward-sexp) t))))

;;;;; Composed

(defun nt-bounds?--lisps (note)
  "Render NOTE's indentation boundary? If so give NOTE."
  ;; About as un-optimized as you can get... But clean and easy to test!
  (let* ((fail-predicates '(nt-bounds?--ignore?
                            nt-bounds?--in-string-or-comment?
                            nt-bounds?--elisp-indent-declared?
                            nt-bounds?--lisps-terminal-sexp?))
         (pass-predicates '(nt-bounds?--lisps-another-form-opener-on-line?
                            nt-bounds?--lisps-form-opener?))

         (any-fail? (apply #'-orfn fail-predicates))
         (any-pass? (apply #'-orfn pass-predicates))

         (none-failed? (not (funcall any-fail? note)))
         (some-passed? (funcall any-pass? note)))

    (when (and none-failed? some-passed?)
      note)))

;;;; Boundary

(defun nt-bounds--lisps (note)
  "Calculate line boundary [a b) for NOTE's masks."
  (save-excursion
    (nt-ov--goto note)

    ;; Handles the case ~(note (foo bar)~ where note opens form that isnt closed
    (if (eq t (sp-end-of-sexp))
        (1+ (line-number-at-pos (point-max)))
      (1+ (line-number-at-pos)))
    ))

;;; Generalized
;;;; Commentary

;; Special indent rules, indent blocks, etc. will be handled by
;; major-mode-dependent predicate. I don't think the predicate can be made
;; major-mode-agnostic...

;; Still thinking about the bound is nil case
;;   ie. all lines are empty/before indent and so are bound
;; An option is to signal that the bound is being completed still.
;; Depending on how change functions are implemented, I may use this idea

;;;; Implementation

;; TODO Not compatabile yet with `after-change-functions' yet
;; however, it works without them enabled.
(defun nt-bounds--general (note)
  "Generalized visual-line based bounds finding for NOTE."
  (save-excursion
    (nt-ov--goto note)

    (let ((start-line (line-number-at-pos))
          (bound (line-number-at-pos (point-max))))
      (nt-line-move-visual-while (or (nt-line--empty? line)
                                     (nt--before-indent?))
        (when (nt-line--nonempty? line)
          (setq bound line)))

      (1+ bound))))

;;; Provide

(provide 'nt-bounds)

;;; nt-bounds.el ends here
