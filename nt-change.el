;;; nt-change.el --- Buffer Modification Support -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; UNDER DEVELOPMENT

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

(defun nt-change--insertion-line-restricted (line)
  "Change function specialized for insertions of regions not crossing lines."
  ;; I /think/ it is this simple. Need to enumerate assumptions on this.

  ;; This relies on 'nt-bound not changing upon insertion of text in just line
  ;; Is this assumption valid?
  ;; It isn't valid if `sp-end-of-sexp' jumps to a different line.
  ;; Inserting a ")" maybe does it?

  ;; I think 'nt-bound's end could only ever decrease here.

  ;; What if: nt-bounds saves if the sexp isn't being completed
  ;; then this operates on notes backward from point up until root that
  ;; are being "completed" still

  (-some-> line nt-notes<-line nt-notes--update-bounded))

(defun nt-change--insertion (start end)
  "Change function specialized for insertion, in START and END."
  (-if-let (new-lines (nt-change--new-lines?))
      (nt-change--insertion-lines start end new-lines)
    (nt-change--insertion-line-restricted (line-number-at-pos start))))

;;; Deletion

(defun nt-change--deletion (pos chars-deleted)
  "Change function specialized for deletion, number CHARS-DELETED at POS."
  ;; (message "Deleting at pos %s, %s characters" pos chars-deleted)
  )

;;; Hook

(defun nt-change--after-change-function (start end chars-deleted)
  "See `after-change-functions', dispatches the correct change function."
  (if (= 0 chars-deleted)
      (nt-change--insertion start end)
    (nt-change--deletion start chars-deleted)))

;;; Scratch - Stream of Thought

;; (let ((line (line-number-at-pos start))
;;       (bounds (nt-notes->maximal-bounds nt-notes)))
;;   (-when-let ((start-line end-line)
;;               (nt-bounds--contains? line bounds))
;;     (let ((notes (nt-notes<-lines start-line end-line)))
;;       ;; Check if note boundaries need to be extended

;;       )))

;;; Provide

(provide 'nt-change)

;;; nt-change.el ends here
