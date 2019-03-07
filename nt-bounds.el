;;; nt-bounds.el --- Indent Boundaries -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Calculate boundaries of notes effects on indentation masks. Major-mode
;; dependent functions are implemented here.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)

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

;; TODO Straightforward (next and check line)
(defun nt-bounds?--lisps-terminal-sexp? (note)
  "Is NOTE the terminal sexp on its line?

(note
 foo)

Does not have NOTE contributing to indentation masks though it is a form opener."
  nil)

;; TODO Not sure where to start on this one Might have to learn how to inspect
;; function properties and how (declare indent) works in-depth
(defun nt-bounds?--lisps-specially-indented? (note)
  "Do we have to account for indentation declarations?"
  nil)

;;;;; Composition

(defun nt-bounds?--lisps (note)
  "Render NOTE's indentation boundary? If so give NOTE."
  ;; This may or may not be exhaustive. Exhausting cases is lower priority than
  ;; getting this subset working. Same for performance optimizations.
  (and
   (funcall
    (-andfn (-not #'nt-bounds?--in-string-or-comment?)
            (-not #'nt-bounds?--lisps-specially-indented?)
            (-not #'nt-bounds?--lisps-terminal-sexp?)
            (-orfn #'nt-bounds?--lisps-another-form-opener-same-line?
                   #'nt-bounds?--lisps-form-opener?))
    note)
   note))

;;;; Range

(defun nt-bounds--lisps (note)
  "Calculate line boundary [a b) for NOTE's masks."
  ;; It is potentially more involved than this, but this /should/ work unless
  ;; you are going out of your way to format in a breaking manner
  (let* ((start (overlay-start note))
         (line (1+ (line-number-at-pos start)))
         (max-line (line-number-at-pos (point-max))))
    (list (min line max-line)
          (save-excursion
            (goto-char start)
            (sp-end-of-sexp)
            (1+ (line-number-at-pos))))))

;;; Provide

(provide 'nt-bounds)

;;; nt-bounds.el ends here
