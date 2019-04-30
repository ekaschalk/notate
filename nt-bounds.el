;;; nt-bounds.el --- Indent Boundaries -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Calculate boundaries of notes effects on indentation masks. Major-mode
;; dependent functions are implemented here.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)

;;; Exposes

(defun nt-bound (note)
  "Call `nt-bound-fn' on NOTE."
  (funcall (symbol-value #'nt-bound-fn) note))

(defun nt-bound? (note)
  "Call `nt-bound?-fn' on NOTE."
  (funcall (symbol-value #'nt-bound?-fn) note))

;;; General
;;;; Overlay-Based

(defun nt-bounds?--in-string-or-comment? (note)
  "Is NOTE contained within a string or comment?"
  (let ((state (save-excursion
                 (syntax-ppss (overlay-start note)))))
    (or (nth 3 state) (nth 4 state))))

;;; Lisps
;;;; Predicates
;;;;; Conditions

(defun nt-bounds?--ignore? (note)
  "Should NOTE never contribute to indentation?"
  (-contains? nt-ignore-notes (nt-ov->string note)))

;; TODO If the replacement covers a form-opener only partially, should return t
(defun nt-bounds?--lisps-form-opener? (note)
  "Does NOTE open a form?

(note foo
      bar)

Simplest case that has NOTE contributing to indentation masks."
  (save-excursion
    (nt-ov--goto note)
    (null (ignore-errors (backward-sexp) t))))

;; TODO Straightforward (descend and check line)
(defun nt-bounds?--lisps-another-form-opener-same-line? (note)
  "Does NOTE have another form opener on the same line?

(foo note (foo foo
               foo))

Has NOTE contributing to indentation masks even though it is not a form opener."
  nil)

(defun nt-bounds?--lisps-terminal-sexp? (note)
  "Is NOTE the terminal sexp on its line?

(note
 foo)

Does not have NOTE contributing to indentation masks though it is a form opener."
  (save-excursion
    (nt-ov--goto note)

    (let ((line-start (line-number-at-pos)))
      (ignore-errors (forward-sexp) (forward-sexp))
      (< line-start (line-number-at-pos)))))

;; TODO Not sure where to start on this one Might have to learn how to inspect
;; function properties and how (declare indent) works in-depth
(defun nt-bounds?--lisps-specially-indented? (note)
  "Do we have to account for indentation declarations?"
  nil)

;;;;; Composition

(defun nt-bounds?--lisps (note)
  "Render NOTE's indentation boundary? If so give NOTE."
  ;; Not optimized obviously but clear.
  (when (funcall (-andfn (-not #'nt-bounds?--ignore?)
                         (-not #'nt-bounds?--in-string-or-comment?)
                         (-not #'nt-bounds?--lisps-specially-indented?)
                         (-not #'nt-bounds?--lisps-terminal-sexp?)
                         (-orfn #'nt-bounds?--lisps-another-form-opener-same-line?
                                #'nt-bounds?--lisps-form-opener?))
                 note)
    note))

;;;; Range

(defun nt-bounds--lisps (note)
  "Calculate line boundary [a b) for NOTE's masks."
  (save-excursion
    (nt-ov--goto note)
    (sp-end-of-sexp)
    (1+ (line-number-at-pos))))

;;; Generalized
;;;; COMMENTARY

;; Special indent rules, indent blocks, etc. will be handled by
;; major-mode-dependent predicate. I don't think the predicate can be made
;; major-mode-agnostic...

;; Still thinking about the bound is nil case
;;   ie. all lines are empty/before indent and so are bound
;; An option is to signal that the bound is being completed still.
;; Depending on how change functions are implemented, I may use this idea

;;;; Implementation

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
