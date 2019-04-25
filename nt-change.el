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

;;; OLD
;;;; Utilities

;; (defun nt-change--line-diff ()
;;   "Get diff of current number of lines and lines since last `nt-masks' refresh."
;;   (let ((prev-lines (length nt-masks))
;;         (cur-lines (- (1+ (line-number-at-pos (point-max)))
;;                       (line-number-at-pos (point-min)))))
;;     (- cur-lines prev-lines)))

;; (defun nt-change--new-lines? ()
;;   "Get count of new lines since last `nt-masks', if lines were added."
;;   (let ((line-diff (nt-change--line-diff)))
;;     (and (> line-diff 0) line-diff)))

;;;; Insertion

;; (-if-let (new-lines (nt-change--new-lines?))
;;     (nt-change--insertion-lines start end new-lines)
;;   (nt-change--insertion-line-restricted (line-number-at-pos start)))

;; (defun nt-change--insertion-lines (start end count)
;;   "Change function specialized for insertions changing line-count."
;;   ;; This is a hard function, much harder than I originally expected.
;;   ;; Because insertion (both atm and on prev notes) need not be balanced.

;;   ;; STEPS
;;   ;; 1. Init a mask at each newline within start and end
;;   ;; 2. Check mask at line before START and mask after END
;;   ;;    call these mask-start and mask-end
;;   ;; 3a. If mask-start.notes == mask-end.notes := notes
;;   ;;     each new mask inherits notes
;;   ;;     THEN font-lock-mode /should/ handle any new notes within start and end
;;   ;; 3b. If mask-start.notes != mask-end.notes
;;   ;;     each new mask inherits mask-end.notes (I think??)

;;   ;; x. Get root containing point
;;   ;;    increment all bounds' ends of notes contained by root's line boundaries
;;   ;; x. Could also be completing a bound...
;;   ;;    first note backwards from start (might need extra conditions too)
;;   ;;    possibly update that notes bound
;;   ;;      if we do -> update all notes bounds contained within that notes root
;;   )

;; (defun nt-change--insertion-line-restricted (line)
;;   "Change function specialized for insertions of regions not crossing lines."
;;   ;; I /think/ it is this simple. Need to enumerate assumptions on this.

;;   ;; This relies on 'nt-bound not changing upon insertion of text in just line
;;   ;; Is this assumption valid?
;;   ;; It isn't valid if `sp-end-of-sexp' jumps to a different line.
;;   ;; Inserting a ")" maybe does it?

;;   ;; I think 'nt-bound's end could only ever decrease here.

;;   ;; What if: nt-bounds saves if the sexp isn't being completed
;;   ;; then this operates on notes backward from point up until root that
;;   ;; are being "completed" still

;;   (-some-> line nt-notes<-line nt-notes--update-bounded))

;; (defun nt-change--insertion (start end)
;;   "Change function specialized for insertion, in START and END."
;;   (-if-let (new-lines (nt-change--new-lines?))
;;       (nt-change--insertion-lines start end new-lines)
;;     (nt-change--insertion-line-restricted (line-number-at-pos start))))

;;; NEW IMPLEMENTATIONS
;;;; Unbalanced

(defun nt-change--unbalanced (pos chars-deleted)
  "PANIC - Unbalanced is hard and being worked through still.

Requires some more advanced ideas to implement in a user-transparent way.
Think it is possible and I have an idea on how to accomplish it. Getting
balanced working first.")

;;;; Insertion

(defun nt-change--insertion-same-line (start end)
  "Change func specialized for insertion not crossing lines in START and END."
  )

(defun nt-change--insertion (start end)
  "Change func specialized for insertion, in START and END."
  )

;;;; Deletion
;;;;; Utilities

(defun nt-change--lines-deleted? ()
  "Have lines been deleted?"
  (let ((count (- (length nt-masks) (line-number-at-pos (point-max)))))
    (and (< 0 count) count)))

(defun nt-change--root-containing (line roots)
  "Get first root in ROOTS whose interval contains LINE."
  (when roots
    (-let* (((root . rest) roots)
            ((start end) root))
      (if (<= start line (1+ end))
          root
        (nt-change--root-containing line rest)))))

(defun nt-notes--children-of (root)
  (-let (((_ max-bound)
          (nt-note->interval root)))
    (->> nt-notes
       (-drop-while (lambda (note)
                      (not (equal it root))))
       (-take-while (lambda (note)
                      (<= (nt-ov->line note) max-bound))))))

(defun nt-change--update-bounded (note)
  ;; TODO Must manage new text prop `nt-in-effect?'
  ;; Could check next lines mask if it contains a note, but that is bad i think

  (let ((last-bound (nt-note->last-bound note))
        (was-in-effect? (nt-note--in-effect? note))
        (bound (nt-bound note))
        (in-effect? (nt-bound? note)))

    (cond
     ((and was-in-effect? in-effect?)
      ;; delete note in masks [bound last-bound)
      ;; add note to masks [last-bound bound)
      )
     (was-in-effect?
      ;; delete note from all its masks, ie. [note-line+1 bound)
      )
     (in-effect?
      ;; add note to all its masks, ie. [note-line+1 bound)
      ))

    (-doto note
      (overlay-put 'nt-last-bound bound)
      (overlay-put 'nt-in-effect? in-effect?))))

;;;;; Implementation

;; ASSUMING BALANCED DELETION CURRENTLY
;; If I do end up needing a tree
;;  can recursively call the `nt-notes->roots' on each `-drop-while' section

(defun nt-change--deletion (pos chars-deleted)
  "Change function specialized for deletion, number CHARS-DELETED at POS."
  (when (nt-change--lines-deleted?)
    (let* ((roots (nt-notes->roots nt-notes))
           (line (line-number-at-pos pos))  ; works only if balanced i think
           (root (nt-change--root-containing line roots)))
      (when root
        (let ((children (nt-notes--children-of root)))
          (-each children #'nt-change--update-bounded))))))

;;; Provide

(provide 'nt-change)

;;; nt-change.el ends here
