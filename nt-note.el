;;; nt-note.el --- Notation Overlays -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Notation overlay management, instantiation, etc.

;; For `hl-todo' users, consider:
;; (setq hl-todo-keyword-faces (--remove (s-equals? (car it) "NOTE") hl-todo-keyword-faces))
;; To remove highlighting of NOTE in documentation strings



;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)



;;; Overlays

(defun nt-notes--present? (&optional start end)
  "Are notes present within START and END, defaulting to `match-data'? Get them."
  (let ((start (or start (match-beginning 1)))
        (end   (or end (match-end 1))))
    (and start end
         (-filter #'nt-ov--note?
                  (overlays-in start end)))))

(defun nt-note--at (pos)
  "Get note at POS."
  (-first-item (nt-notes--present? pos (1+ pos))))

(defun nt-note--at-point ()
  "Get note at point."
  (nt-note--at (point)))

(defun nt-notes--at (line)
  "Return all notes on LINE."
  (apply #'nt-notes--present? (nt-base--line-boundary line)))

(defun nt-notes--in (start-line end-line)
  "Return all notes in [START-LINE END-LINE)."
  (-mapcat (-applify #'nt-notes--at) (nt-base--range start-line end-line)))

(defun nt-note--delete (note)
  "Delete NOTE."
  (delq note nt-note-list)
  (delete-overlay note))

(defun nt-note--decompose-hook (note post-modification? start end &optional _)
  "Decompose NOTE upon modification as a modification-hook."
  (when post-modification?
    (nt--remove-note-from-masks note)
    (nt-note--delete note)))



;;; Transforms

(defun nt-note->width (note)
  "Wrapper to access width of NOTE."
  (overlay-get note 'nt-width))

(defun nt-notes->width (notes)
  "Sum widths of NOTES."
  (->> notes (-map #'nt-note->width) -sum))



;;; Init

(defun nt-note--init-ov (ov string replacement)
  "Put note text properties into OV."
  (-doto ov
    (overlay-put 'nt?      t)
    (overlay-put 'nt-note?  t)
    (overlay-put 'nt-width (nt-base--s-diff string replacement))

    (overlay-put 'display replacement)
    (overlay-put 'modification-hooks '(nt-note--decompose-hook))))

(defun nt-note--init (string replacement &optional start end)
  "Build note overlay, defaulting to `match-data' for START and END."
  (setq start (or start (match-beginning 1)))
  (setq end   (or end   (match-end 1)))

  (unless (and start end)
    (error "Initiatializing note without match-data set."))

  (let* ((ov    (make-overlay start end))
         (note   (nt-note--init-ov ov string replacement)))
    (push note nt-note-list)
    (nt--add-note-to-masks note)
    note))



(provide 'nt-note)



;;; nt-note.el ends here
