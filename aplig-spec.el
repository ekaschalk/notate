;;; aplig-spec.el --- Specs -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Hi



;;; Code:
;;;; Requires

(require 'aplig-base)



;;; Validation

(defun aplig-spec--validate (string replacement)
  "Throw error on egregious inputs."
  (cond
   ((or (s-contains? "\n" string)
        (s-contains? "\n" replacement))
    (error "Newlines anywhere in spec components cause ambiguity."))

   ((> (length replacement)
       (length string))
    (error "Indentation expansions not supported yet."))))

;;; Construction

(defun aplig-spec--make (string replacement &optional rx)
  "Create spec plist for STRING to REPLACEMENT optionally with custom RX.

Without a RX given, default to matching entire STRING.
The RX, if given, should set the first group for the match to replace."
  (aplig-spec--validate string replacement)
  `(:string
    ,string
    :rx          ,(or rx
                      `,(rx-to-string `(group ,string)
                                      'no-shy-group))
    :replacement ,replacement
    :width       ,(- (length string)
                     (length replacement))))

(defun aplig-specs--make (specs)
  "Apply `aplig-spec--make' to each SPEC."
  (-map (-applify #'aplig-spec--make) specs))



(provide 'aplig-spec)



;;; aplig-spec.el ends here
