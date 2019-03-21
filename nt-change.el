;;; nt-change.el --- Buffer Modification Support -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Support for buffer modifications, ie. text-editing.

;;; Code:
;;;; Requires

(require 'nt-base)

;;; Utilities

(defun nt-change--line-diff ()
  "Get diff of current number of lines and lines since last `nt-masks' refresh."
  (let ((prev-lines (length nt-masks))
        (cur-lines (- (1+ (line-number-at-pos (point-max)))
                      (line-number-at-pos (point-min)))))
    (- cur-lines prev-lines)))

(defun nt-change--new-lines? ()
  "Get count of new lines since last `nt-masks', if lines were added."
  (let ((line-diff (nt-change--line-diff)))
    (and (> line-diff 0) line-diff)))

(defun nt-change--removed-lines? ()
  "Get count of removed lines since last `nt-masks', if lines were removed."
  (let ((line-diff (nt-change--line-diff)))
    (and (< line-diff 0) line-diff)))

;;; Insertion

(defun nt-change--insertion-lines (start end count)
  "Change function specialized for insertions changing line-count."
  ;; This is a hard function, much harder than I originally expected.
  ;; Because insertion (both atm and on prev notes) need not be balanced.

  ;; STEPS
  ;; 1. Init a mask at each newline within start and end
  ;; 2. Check mask at line before START and mask after END
  ;;    call these mask-start and mask-end
  ;; 3a. If mask-start.notes == mask-end.notes := notes
  ;;     each new mask inherits notes
  ;;     THEN font-lock-mode /should/ handle any new notes within start and end
  ;; 3b. If mask-start.notes != mask-end.notes
  ;;     each new mask inherits mask-end.notes (I think??)

  ;; x. Get root containing point
  ;;    increment all bounds' ends of notes contained by root's line boundaries
  ;; x. Could also be completing a bound...
  ;;    first note backwards from start (might need extra conditions too)
  ;;    possibly update that notes bound
  ;;      if we do -> update all notes bounds contained within that notes root
  )

(defun nt-change--insertion (start end)
  "Change function specialized for insertion, in START and END."
  (-when-let (new-lines (nt-change--new-lines?))
    (nt-change--insertion-lines start end new-lines))

  ;; Below is old code, doesnt work
  ;; (let* ((end-line (1+ (line-number-at-pos)))
  ;;        (start-line (- end-line new-lines))
  ;;        (line-before-change (1- start-line))

  ;;        ;; Must init masks ASAP for `nt-masks' integrity
  ;;        (masks
  ;;         ;; (nt-masks--init start-line end-line)
  ;;         (-map #'nt-mask--init
  ;;               (number-sequence start-line (1- end-line)))
  ;;         )

  ;;        (mask-before-change (nt-mask<-line line-before-change))
  ;;        (notes-before-change (overlay-get mask-before-change 'nt-notes))

  ;;        (notes (-union notes-before-change
  ;;                       (nt-notes<-line line-before-change))))

  ;;   ;; The note is being added to mask but it takes 2 insertions to "kick in"
  ;;   ;; for some reason?
  ;;   ;; (nt--add-notes-to-masks notes)
  ;;   (-each notes #'nt--add-note-to-masks)

  ;;   ;; (message "%s %s" start end)
  ;;   )
  )

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
