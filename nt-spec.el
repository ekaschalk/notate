;;; nt-spec.el --- Specs -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Define and validate ligatures and add to `font-lock-mode'



;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-lig)



;;; Validation

(defun nt-spec--validate (string replacement)
  "Throw error on egregious inputs."
  (cond
   ((or (s-contains? "\n" string)
        (s-contains? "\n" replacement))
    (error "Newlines anywhere in spec components cause ambiguity."))

   ((= 0
       (length replacement))
    (error "Zero-width replacements can be done natively with 'invisible."))

   ((> (length replacement)
       (length string))
    (error "Indentation expansions not supported yet, but I would like to."))))

(defun nt-specs--validate (specs)
  "Throw error on egregious combinations of inputs."
  ;; TODO Make sure no two string's can have RX's overlap, easier to have happen
  ;; then you might first think, because we aren't matching symbols, we are
  ;; matching strings. Eg. '(("foo" "x") ("foo2" "x")) -> got me twisted.
  ;; Note that replacements and string-replacements can overlap all they want.
  )

;;; Construction

(defun nt-spec--string->rx (string)
  "Convert string to an expected nt-spec RX."
  (rx-to-string `(group ,string)
                'no-shy-group))

(defun nt-spec--make (string replacement &optional rx)
  "Create spec plist for STRING to REPLACEMENT optionally with custom RX.

Without a RX given, default to matching entire STRING.
The RX, if given, should set the first group for the match to replace."
  (nt-spec--validate string replacement)

  `(:string
    ,string
    :rx          ,(or rx (nt-spec--string->rx string))
    :replacement ,replacement))

(defun nt-specs--make (specs)
  "Apply `nt-spec--make' to each SPEC."
  (nt-specs--validate specs)

  (-map (-applify #'nt-spec--make) specs))



;;; Font Locks

(defun nt-spec--kwd-match (string replacement)
  "The form for FACENAME in font-lock-keyword's MATCH-HIGHLIGHT."
  (unless (nt-ligs--present?)
    (nt-lig--init string replacement)))

(defun nt-spec--kwd-build (spec)
  "Compose the font-lock-keyword for SPEC in `nt-specs'."
  (-let (((&plist :string string
                  :replacement replacement
                  :rx rx)
          spec))
    `(,rx (0 (prog1 nil (nt-spec--kwd-match ,string ,replacement))))))

(defun nt-spec--kwds-add ()
  "Build kwds from `nt-specs' and add to `font-lock-keywords'."
  (->> nt-specs
     (-map #'nt-spec--kwd-build)
     (font-lock-add-keywords nil)))



(provide 'nt-spec)



;;; nt-spec.el ends here
