;;; nt-kwds.el --- Notate's Font-Lock Interface -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Interface between `nt-note' and `font-lock-mode' and expose functions for
;; defining notes.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-note)

;;; Specs

(defun nt-note--validate (string replacement)
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

(defun nt-note--string->rx (string)
  "Convert string to an expected nt-note RX."
  (rx-to-string `(group ,string)
                'no-shy-group))

(defun nt-note--make (string replacement &optional rx)
  "Create spec plist for STRING to REPLACEMENT optionally with custom RX.

Without a RX given, default to matching entire STRING.
The RX, if given, should set the first group for the match to replace."
  (nt-note--validate string replacement)

  `(:string
    ,string
    :rx          ,(or rx (nt-note--string->rx string))
    :replacement ,replacement))

(defun nt-notes--make (specs)
  "Apply `nt-note--make' to each SPEC."
  (-map (-applify #'nt-note--make) specs))

;;; Matching

(defun nt-note--kwd-match (string replacement)
  "The form for FACENAME in font-lock-keyword's MATCH-HIGHNOTEHT."
  (-let* (((start end)
           (match-data 1))
          (note-already-present?
           (nt-notes<-region start end)))
    (unless note-already-present?
      (nt-note--init string replacement start end))))

;;; Construction

(defun nt-note--kwd-build (spec)
  "Compose the font-lock-keyword for SPEC in `nt-notes'."
  (-let (((&plist :string string
                  :replacement replacement
                  :rx rx)
          spec))
    `(,rx (0 (prog1 `,(and nt-normalize-height?
                           'nt-note--face)
               (nt-note--kwd-match ,string ,replacement))))))

(defun nt-note--kwds-add ()
  "Build kwds from `nt-notes' and add to `font-lock-keywords'."
  (->> nt-notes
     (-map #'nt-note--kwd-build)
     (font-lock-add-keywords nil)))

;;; Provide

(provide 'nt-kwds)

;;; nt-kwds.el ends here
