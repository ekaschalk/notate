;;; nt-change.el --- Buffer Modification Support -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; UNDER DEVELOPMENT

;; Support for buffer modifications, ie. text-editing.

;;; Code:
;;;; Requires

(require 'nt-base)

;;; Hook

(defun nt-change--after-change-function (start end chars-deleted)
  "See `after-change-functions', dispatches on insertion or deletion."
  (if (= 0 chars-deleted)
      (nt-change--insertion start end)
    (nt-change--deletion start chars-deleted)))

;;; Scratch

;; (let* ((roots (nt-notes->roots nt-notes))
;;        (line (line-number-at-pos pos))  ; works only if balanced i think
;;        (root (nt-change--root-containing line roots)))
;;   (when root
;;     (let ((children (nt-notes--children-of root)))
;;       (nt-notes--update-bound children))))

;; (defun nt-change--root-containing (line roots)
;;   "Get first root in ROOTS whose interval contains LINE."
;;   (when roots
;;     (-let* (((root . rest) roots)
;;             ((start end) root))
;;       (if (<= start line (1+ end))
;;           root
;;         (nt-change--root-containing line rest)))))

;; (defun nt-notes--children-of (root)
;;   (-let (((_ max-bound)
;;           (nt-note->interval root)))
;;     (->> nt-notes
;;        (-drop-while (lambda (note)
;;                       (not (equal it root))))
;;        (-take-while (lambda (note)
;;                       (<= (nt-ov->line note) max-bound))))))

;;; Insertion

;; Not started

(defun nt-change--insertion-same-line (start end)
  "Change func specialized for insertion not crossing lines in START and END."
  )

(defun nt-change--insertion (start end)
  "Change func specialized for insertion, in START and END."
  )

;;; Deletion
;;;;; Notes

;; ASSUMING BALANCED DELETION CURRENTLY
;; If I do end up needing a tree
;;  can recursively call the `nt-notes->roots' on each `-drop-while' section

;; A better version would trim-out every note in separate subtrees
;; but taking everything contained in the root is still far better than nothing
;; and likely to be close to optimized anyway in most situations

;; Further I can just store the roots and point to their children which
;; should be fine in most cases

;;;; Utilities

(defun nt-change--lines-deleted? ()
  "Have lines been deleted?"
  (let ((count (- (length nt-masks) (line-number-at-pos (point-max)))))
    (and (< 0 count) count)))

;;;; Implementation

(defun nt-change--deletion (pos chars-deleted)
  "Change function specialized for deletion, number CHARS-DELETED at POS.

Note that the 'modification-hook text property handles deletion of notes
and masks themselves."
  (when (nt-change--lines-deleted?)
    (let* (;; TODO I'm cheating! setting these to start/end of buffer atm
           ;; to get this off the ground
           (start (point-min))
           (end (point-max))
           (notes (nt-notes<-region start end)))
      (nt-notes--update-bound notes))))

;;; Provide

(provide 'nt-change)

;;; nt-change.el ends here
