;;; nt-kwds.el --- Notate's Font-Lock Interface -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Interface between `nt-note', `font-lock-mode', and user note definitions.
;; Such DEFS are string-replacement pairs with optional custom regex.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-note)

;;; Configuration
;;;; Debugging

(defface nt-kwd--normalize-height-face
  `((t (:height 1)))
  "See `nt-normalize-height?' for commentary.")

;;; Definitions
;;;; Validation

(defun nt-kwd--def-validate (string replacement)
  "Throw error on some egregious note definitions."
  (cond
   ((or (s-contains? "\n" string)
        (s-contains? "\n" replacement))
    (error "Newlines anywhere in note definitions are ambiguous."))

   ((> (length replacement)
       (length string))
    (error "Indentation expansions not supported yet, but I would like to."))

   ((s-blank? replacement)
    (error "Zero-width replacements can be done natively, see 'invisible."))))

;;;; Utilities

(defun nt-kwd--string->rx (string)
  "Construct a regex matching STRING in the first subexp."
  (rx-to-string `(group ,string) 'no-shy-group))

;;;; Matching

(defun nt-kwd--def->matcher (string replacement)
  "Construct FACENAME form in MATCH-HIGHLIGHT for a def."
  ;; FIXME Must handle eg. -> and --> (two overlapping note definitions)
  ;; Something like:
  ;;   (when (string strict-contains note-in-region) (delete note-in-region))
  ;; might work. MIGHT work.

  (-let* (((start end) (match-data 1)))
    (unless (-some-> start nt-note<-pos nt-note->string (s-equals? string))
      (nt-note--init string replacement start end))))

;;; Keywords

(defun nt-kwd<-def (string replacement &optional rx)
  "Compose the kwd for STRING to REPLACEMENT, optionally matching custom RX.

The translation of this kwd in `font-lock-add-keywords' documentation is not
totally obvious. It exploits the following rule:

  (MATCHER=rx . HIGHLIGHT=MATCH-HIGHLIGHT=(SUBEXP=0 FACENAME=expression)).

The expression is an arbitrary form, namely notate's note overlay instantiation.

If the expression returns a face, the matched region will have that face set.
See the variable `nt-normalize-height?' for information about the face.
"
  (nt-kwd--def-validate string replacement)
  `(,(or rx
         (nt-kwd--string->rx string))
    (0 (prog1 `,(and nt-normalize-height?
                     'nt-kwd--normalize-height-face)
         (nt-kwd--def->matcher ,string ,replacement)))))

(defun nt-kwds<-defs (defs)
  "Construct keywords for `font-lock-keywords' given DEFS."
  (-map (-applify #'nt-kwd<-def) defs))

(defun nt-kwds--add ()
  "Add to `font-lock-keywords' the kwds resulting from `nt-defs'."
  (font-lock-add-keywords nil (nt-kwds<-defs nt-defs)))

;;; Provide

(provide 'nt-kwds)

;;; nt-kwds.el ends here
