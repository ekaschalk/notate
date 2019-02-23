;;; aplig-spec.el --- Specs -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Define and validate ligatures and add to `font-lock-mode'



;;; Code:
;;;; Requires

(require 'aplig-base)

(require 'aplig-lig)



;;; Validation

(defun aplig-spec--validate (string replacement)
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
    (error "Indentation expansions not supported yet."))))

(defun aplig-specs--validate (specs)
  "Throw error on egregious combinations of inputs."
  ;; TODO Make sure no two string's can have RX's overlap, easier to have happen
  ;; then you might first think, because we aren't matching symbols, we are
  ;; matching strings. Eg. '(("foo" "x") ("foo2" "x")) -> got me twisted.
  ;; Note that replacements and string-replacements can overlap all they want.
  )

;;; Construction

(defun aplig-spec--string->rx (string)
  "Convert string to an expected aplig-spec RX."
  (rx-to-string `(group ,string)
                'no-shy-group))

(defun aplig-spec--make (string replacement &optional rx)
  "Create spec plist for STRING to REPLACEMENT optionally with custom RX.

Without a RX given, default to matching entire STRING.
The RX, if given, should set the first group for the match to replace."
  (aplig-spec--validate string replacement)
  `(:string
    ,string
    :rx          ,(or rx (aplig-spec--string->rx string))
    :replacement ,replacement))

(defun aplig-specs--make (specs)
  "Apply `aplig-spec--make' to each SPEC."
  (-map (-applify #'aplig-spec--make) specs))



;;; Font Locks

(defun aplig-spec--kwd-match (string replacement)
  "The form for FACENAME in font-lock-keyword's MATCH-HIGHLIGHT."
  (unless (aplig-ligs--present?)
    (aplig-lig--init string replacement)))

(defun aplig-spec--kwd-build (spec)
  "Compose the font-lock-keyword for SPEC in `aplig-specs'."
  (-let (((&plist :string string
                  :replacement replacement
                  :rx rx)
          spec))
    `(,rx (0 (prog1 nil (aplig-spec--kwd-match ,string ,replacement))))))

(defun aplig-spec--kwds-add ()
  "Build kwds from `aplig-specs' and add to `font-lock-keywords'."
  (->> aplig-specs
     (-map #'aplig-spec--kwd-build)
     (font-lock-add-keywords nil)))



(provide 'aplig-spec)



;;; aplig-spec.el ends here
