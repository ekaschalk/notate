;;; nt-note.el --- Notation Overlays -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Notation overlay management, instantiation, etc.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)


;;; Configuration
;;;; Managed

(defvar nt-notes nil
  "Start-position-ordered list of note overlays.")


(defvar-local nt-note--init-in-progress? nil
  "Are we instantiating the initial notes?")

;;; Access
;;;; Fundamentals

(defun nt-note<-pos (pos)
  "Get note at POS."
  (-some->> (overlays-at pos) (-filter #'nt-ov--note?) car))

(defun nt-notes<-region (start end)
  "Get notes in START and END."
  (-some->> (overlays-in start end) (-filter #'nt-ov--note?) nt-notes--sort))

(defun nt-notes<-line (line)
  "Get notes on LINE."
  (-some->> line nt-line->region (apply #'nt-notes<-region)))

;;;; Extensions

(defun nt-note--at-point ()
  "Get note at point."
  (-> (point) nt-note<-pos))

(defun nt-notes<-lines (start-line end-line)
  "Get notes in [START-LINE END-LINE)."
  (-some->> (nt-lines->region start-line end-line) (apply #'nt-notes<-region)))

;;; Transforms
;;;; Wrappers

(defun nt-note->width (note)
  "Access NOTE's width."
  (-some-> note (overlay-get 'nt-width)))

(defun nt-note->bound (note)
  "Access NOTE's bound."
  (-some-> note (overlay-get 'nt-bound)))

(defun nt-note->replacement (note)
  "Access NOTE's display."
  (-some-> note (overlay-get 'display)))

;;;; Misc

(defun nt-note->string (note)
  "Access NOTE's string."
  (-some->> note nt-ov->region (apply #'buffer-substring-no-properties)))

(defun nt-notes->width (notes)
  "Sum NOTES' widths."
  (->> notes (-map #'nt-note->width) -sum))

(defun nt-note->indent (note)
  "Get indent of NOTE's line."
  (-some-> note nt-ov->line nt-line->indent))

(defun nt-note->idx (note)
  "Get index of insertion of new NOTE into `nt-notes'."
  (or (-find-index (-partial #'nt-notes--lt note) nt-notes)
      (length nt-notes)))

;;; Relationships
;;;; Comparisons

(defun nt-notes--lt (self other)
  "Compare lt two notes."
  (funcall (-on #'< #'overlay-start) self other))

(defun nt-notes--sort (notes)
  "Return NOTES sorted according to start position."
  (-sort #'nt-notes--lt notes))

;;;; Roots

(defun nt-notes->roots-1 (notes roots)
  "Internal, mutually-recursive component of `nt-notes->roots'."
  (-let* (((root)
           roots)
          ((_ root-end)
           (nt-note->bound root))
          (next
           (-drop-while (-compose (-partial #'> root-end)
                                  #'car
                                  #'nt-note->bound)
                        notes)))
    (nt-notes->roots next roots)))

(defun nt-notes->roots (notes &optional roots)
  "Return ROOTS of NOTES, ie. the set of notes with largest disjoint intervals.

If 2+ roots have equiv. bounds, the first by buffer position is the only root."
  (-let (((root . rest) notes)
         (-compare-fn (-on #'equal #'nt-note->bound)))
    (cond (rest (nt-notes->roots-1 rest (cons root roots)))
          (root (nt-notes->roots   rest (cons root roots)))
          ((-distinct (reverse roots))))))

(defun nt-notes->maximal-regions (notes)
  "Return maximal disjoint intervals of NOTES."
  (->> notes nt-notes->roots (-map #'nt-note->bound)))

;;; Management
;;;; Insertion

(defun nt-note--insert-sorted (note)
  "Get `nt-notes' with NOTE inserted maintaining order."
  (-> note nt-note->idx (-insert-at note nt-notes)))

(defun nt-note--insert (note)
  "Insert NOTE into `nt-notes' according to the current context."
  (if nt-note--init-in-progress?
      (!cons note nt-notes)
    (setq nt-notes (nt-note--insert-sorted note))))

;;;; Deletion
;;;;; Internal

(defun nt-notes--delete-internal (notes)
  "Delete NOTES overlays. Internal-use by other deletion methods only."
  (-each notes #'delete-overlay)
  (setq nt-notes (-remove #'nt-ov--deleted? nt-notes)))

(defun nt-note--delete-internal (note)
  "Delete a single NOTE overlay. Internal-use by other deletion methods only."
  (nt-notes--delete-internal (list note)))

;;;;; Commands

;; TODO Test
(defun nt-notes--delete (notes)
  "Delete NOTES, updating masks."
  (let ((regions (nt-notes->maximal-regions notes)))
    (-each notes #'nt-note--delete-internal)
    (-each regions (-applify #'nt-mask--refresh-region))))

(defun nt-note--delete (note)
  "Delete a single NOTE, updating masks."
  (nt-notes--delete (list note)))

(defun nt-notes--delete-region (start end)
  "Delete notes in START and END, updating masks."
  (nt-notes--delete (nt-notes<-region start end)))

;;; Decomposition

;; TODO probably much more involved than this...
(defun nt-note--decompose (note)
  "Workhorse of `nt-note--decompose-hook'."
  (nt-note--delete note))

(defun nt-note--decompose-hook (note post-modification? start end &optional _)
  "Decompose NOTE upon modification as a modification-hook."
  (when post-modification?
    (nt-note--decompose note)))

;;; Init

(defun nt-note--init-ov (string replacement start end)
  "Instantiate note overlay and its properties for `nt-note--init'."
  (-doto (make-overlay start end)
    (overlay-put 'nt?      t)
    (overlay-put 'nt-note? t)
    (overlay-put 'nt-width (- (length string) (length replacement)))

    (overlay-put 'display replacement)
    (overlay-put 'modification-hooks '(nt-note--decompose-hook))))

(defun nt-note--init (string replacement start end)
  "Build note overlay for STRING to REPLACEMENT between START and END."
  (let* ((note (nt-note--init-ov string replacement start end))
         (bound (funcall (symbol-value #'nt-bound-fn) note)))
    (-doto note
      (nt-note--insert)
      (nt--add-note-to-masks)
      (overlay-put 'nt-bound bound))))

(defun nt-notes--init ()
  "Instantiate `nt-notes', ie. wrap `font-lock-ensure' with optimizations."
  (let ((nt-note--init-in-progress t))
    (font-lock-ensure)

    ;; During init we don't rely on the ordering of `nt-notes'
    ;; So we can !cons, reverse upon completion, and insert-sorted from thereon
    (setq nt-notes (reverse nt-notes))))

;;; Provide

(provide 'nt-note)

;;; nt-note.el ends here
