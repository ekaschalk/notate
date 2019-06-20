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

;;; Notes

;; ASSUMING BALANCED DELETION CURRENTLY
;; If I do end up needing a tree
;;  can recursively call the `nt-notes->roots' on each `-drop-while' section

;; A better version would trim-out every note in separate subtrees
;; but taking everything contained in the root is still far better than nothing
;; and likely to be close to optimized anyway in most situations

;; Further I can just store the roots and point to their children which
;; should be fine in most cases

;;; Utilities

(defun nt-change--lines-deleted? ()
  "Have lines been deleted?"
  (let ((count (- (length nt-masks) (line-number-at-pos (point-max)))))
    (and (< 0 count) count)))

(defun nt-change--lines-added? ()
  "Have lines been deleted?"
  (let ((count (- (line-number-at-pos (point-max)) (length nt-masks))))
    (and (< 0 count) count)))

;;; Insertion

(defun nt-change--insertion (start end)
  "Change func specialized for insertion, in START and END."
  (let ((inhibit-modification-hooks t))

    ;; Build new masks if needed
    (-when-let (new-lines (nt-change--lines-added?))
      (let ((start-line (line-number-at-pos start))
            (end-line (line-number-at-pos end)))

        (when (nt-mask<-line-raw start-line)
          (cl-incf start-line))
        (when (nt-mask<-line-raw end-line)
          (cl-decf end-line))

        (unless (< end-line start-line)
          (-each (number-sequence start-line end-line) #'nt-mask--init))))

    ;; TODO setting these to start/end of buffer atm to get off ground
    (let* ((start (point-min))
           (end (point-max))
           (notes (nt-notes<-region start end)))
      (nt-notes--update-bounded notes))))

;;; Deletion

(defun nt-change--deletion (pos chars-deleted)
  "Change function specialized for deletion, number CHARS-DELETED at POS.

Note that the 'modification-hook text property handles deletion of notes
and masks themselves."
  (when (nt-change--lines-deleted?)

    ;; TODO setting these to start/end of buffer atm to get off ground
    (let* ((start (point-min))
           (end (point-max))
           (notes (nt-notes<-region start end)))
      (nt-notes--update-bounded notes))))

;;; Provide

(provide 'nt-change)

;;; nt-change.el ends here
