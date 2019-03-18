;;; nt-change.el --- After Change Functions -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Support for buffer modifications, ie. text-editing, for Notate.

;;; Code:
;;;; Requires

(require 'nt-base)

;;; Change Functions
;;;; Utils

(defun nt-change--line-diff ()
  "Lines added: +x, removed: -x, otherwise 0 since mask list last updated."
  ;; NOTE 1- point-max easier than calling skip-line on last-line's mask
  (- (line-number-at-pos (1- (point-max)))
     (length nt-masks)))

(defun nt-change--new-lines? ()
  "Return count of lines added since last update or nil."
  (let ((line-change (nt-change--line-diff)))
    (when (> line-change 0)
      line-change)))

(defun nt-change--removed-lines? ()
  "Return count of lines removed since last update or nil."
  (let ((line-change (nt-change--line-diff)))
    (when (< line-change 0)
      (- line-change))))

;;;; Insertion

(defun nt-change--insertion (start end)
  "Change function specialized for insertion, in START and END."
  (-when-let (new-lines (nt-change--new-lines?))
    (let* ((end-line (1+ (line-number-at-pos)))
           (start-line (- end-line new-lines))
           (line-before-change (1- start-line))

           ;; Must init masks ASAP for `nt-masks' integrity
           (masks
            ;; (nt-masks--init start-line end-line)
            (-map #'nt-mask--init
                  (number-sequence start-line (1- end-line)))
            )

           (mask-before-change (nt-mask<-line line-before-change))
           (notes-before-change (overlay-get mask-before-change 'nt-notes))

           (notes (-union notes-before-change
                          (nt-notes<-line line-before-change))))

      ;; The note is being added to mask but it takes 2 insertions to "kick in"
      ;; for some reason?
      ;; (nt--add-notes-to-masks notes)
      (-each notes #'nt--add-note-to-masks)

      ;; (message "%s %s" start end)
      )))

;;;; Deletion

(defun nt-change--deletion (pos chars-deleted)
  "Change function specialized for deletion, number CHARS-DELETED at POS."
  ;; (message "Deleting at pos %s, %s characters" pos chars-deleted)
  )

;;;; Hook

(defun nt-change--after-change-function (start end chars-deleted)
  "See `after-change-functions', dispatches the correct change function."
  (if (= 0 chars-deleted)
      (nt-change--insertion start end)
    (nt-change--deletion start chars-deleted)))

;;; Provide

(provide 'nt-change)

;;; nt-change.el ends here
