;;; nt-note.el --- Notation Overlays -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Notation overlay management, instantiation, etc.

;; `hl-todo' users, remove highlighting of NOTE in docstrings with:
;;   (setq hl-todo-keyword-faces (--remove (s-equals? (car it) "NOTE") hl-todo-keyword-faces))

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)


;;; Configuration
;;;; Managed

(defvar-local nt-note--init-in-progress? nil
  "Are we instantiating the initial notes?")

;;;; Debugging

(defface nt-note--face
  `((t (:height 1)))
  "Face applied to notes.")

;;; Access
;;;; Fundamentals

(defun nt-note<-pos (pos)
  "Get note at POS."
  (-some->> (overlays-at pos) (-filter #'nt-ov--note?) car))

(defun nt-notes<-region (start end)
  "Get notes in START and END."
  (-some->> (overlays-in start end) (-filter #'nt-ov--note?) reverse))

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
;;;; Overlay Wrappers

(defun nt-note->width (note)
  "Access NOTE's width."
  (-some-> note (overlay-get 'nt-width)))

(defun nt-note->bound (note)
  "Access NOTE's bound."
  (-some-> note (overlay-get 'nt-bound)))

(defun nt-note->replacement (note)
  "Access NOTE's display."
  (-some-> note (overlay-get 'display)))

(defun nt-note->string (note)
  "Access NOTE's string."
  (-some->> note nt-ov->region (apply #'buffer-substring-no-properties)))

;;;; Misc

(defun nt-notes->width (notes)
  "Sum NOTES' widths."
  (->> notes (-map #'nt-note->width) -sum))

(defun nt-note->indent (note)
  "Get indent of NOTE's line."
  (-some-> note nt-ov->line nt-line->indent))

;;; Relationships
;;;; Comparisons

(defun nt-notes--lt (self other)
  "Compare lt two notes."
  (funcall (-on #'< #'overlay-start) self other))

(defun nt-notes--sort (notes &optional in-place?)
  "Return NOTES sorted according to start position, optionally IN-PLACE?."
  (-sort #'nt-notes--lt notes))

;;;; Root-Finding

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
  "Return ROOTS of NOTES, ie. the set of notes with largest disjoint intervals."
  (-let (((root . rest) notes))
    (cond (rest (nt-notes->roots-1 rest (cons root roots)))
          (root (nt-notes->roots   rest (cons root roots)))
          ((reverse roots)))))

;;; Unorganized

(defun nt-note--delete (note)
  "Delete NOTE."
  (delq note nt-note-list)
  (delete-overlay note))

(defun nt--delete-region (start end)
  "Delete NOTES in START and END then refresh the masks they cover."
  (let* ((notes (nt-notes<-region start end))
         (roots (nt-notes->roots notes))
         (bounds (-map #'nt-note->bound roots)))
    (-each notes
      #'nt-note--delete)
    (-each bounds
      (-applify #'nt-mask--refresh-region))))

(defun nt-note--insert-sorted (note)
  "Insert NOTE into `nt-note-list' maintaining sorted order."
  (setq nt-note-list
        (-if-let (idx (-find-index (-partial #'nt-notes--lt note)
                                   nt-note-list))
            (-insert-at idx note nt-note-list)
          (-snoc nt-note-list note))))

(defun nt-note--insert (note)
  "Insert NOTE into `nt-note-list' according to the current context."
  (if nt-note--init-in-progress?
      (!cons note nt-note-list)
    (nt-note--insert-sorted note)))

(defun nt-note--decompose-hook (note post-modification? start end &optional _)
  "Decompose NOTE upon modification as a modification-hook."
  (when post-modification?
    ;; TODO Below will be replaced with the new deletion implementation that
    ;; doesn't require calling masks-for.
    (nt--remove-note-from-masks note)
    (nt-note--delete note)))

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
  ;; TODO Think about making nt-bound-fn act on a region instead of a NOTE
  ;; TODO Rename `nt-note-list' to `nt-notes'
  (let* ((note (nt-note--init-ov string replacement start end))
         (bound (funcall (symbol-value #'nt-bound-fn) note)))
    (-doto note
      (nt-note--insert)
      (nt--add-note-to-masks)
      (overlay-put 'nt-bound bound))))

(defun nt-notes--init ()
  "Instantiate `nt-note-list', ie. wrap `font-lock-ensure' with optimizations."
  (let ((nt-note--init-in-progress t))
    (font-lock-ensure)

    ;; During init we don't rely on the ordering of `nt-note-list'
    ;; So we !cons, reverse upon completion, and insert-sorted thereon
    (setq nt-note-list (reverse nt-note-list))))

;;; Provide

(provide 'nt-note)

;;; nt-note.el ends here
