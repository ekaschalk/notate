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

;;;; Roots

(defun nt-notes--sort (notes)
  "Return NOTES sorted according to start position."
  (-sort (-on #'< #'overlay-start) notes))

(defun nt-notes->roots-1 (notes roots)
  "Internal, see `nt-notes->roots'."
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

(defun nt-note--decompose-hook (note post-modification? start end &optional _)
  "Decompose NOTE upon modification as a modification-hook."
  (when post-modification?
    ;; TODO Below will be replaced with the new deletion implementation that
    ;; doesn't require calling masks-for.
    (nt--remove-note-from-masks note)
    (nt-note--delete note)))

;;; Init

(defun nt-note--init-ov (ov string replacement)
  "Put note text properties into OV."
  (-doto ov
    (overlay-put 'nt?      t)
    (overlay-put 'nt-note? t)
    (overlay-put 'nt-width (nt-base--s-diff string replacement))

    (overlay-put 'display replacement)
    (overlay-put 'modification-hooks '(nt-note--decompose-hook))))

(defun nt-note--init (string replacement &optional start end)
  "Build note overlay, defaulting to `match-data' for START and END."
  (setq start (or start (match-beginning 1)))
  (setq end   (or end   (match-end 1)))

  (unless (and start end)
    (error "Initiatializing note without match-data set."))

  (let* ((ov   (make-overlay start end))
         (note (nt-note--init-ov ov string replacement)))
    (push note nt-note-list)
    (nt--add-note-to-masks note)

    ;; New tree stuff
    (overlay-put note 'nt-bound (funcall (symbol-value #'nt-bound-fn) note))
    ;; end tree stuff

    note))

;;; Font Locks
;;;; Spec Handling

(defun nt-note--validate (string replacement)
  "Throw error on egregious inputs."
  (cond
   ((or (s-contains? "\n" string)
        (s-contains? "\n" replacement))
    (error "Newlines anywhere in spec components cause ambiguity."))

   ((= 0
       (length replacement))
    (error "Zero-width replacements can be done natively with 'invisible."))

   ((> (length replacement)
       (length string))
    (error "Indentation expansions not supported yet, but I would like to."))))

(defun nt-note--string->rx (string)
  "Convert string to an expected nt-note RX."
  (rx-to-string `(group ,string)
                'no-shy-group))

(defun nt-note--make (string replacement &optional rx)
  "Create spec plist for STRING to REPLACEMENT optionally with custom RX.

Without a RX given, default to matching entire STRING.
The RX, if given, should set the first group for the match to replace."
  (nt-note--validate string replacement)

  `(:string
    ,string
    :rx          ,(or rx (nt-note--string->rx string))
    :replacement ,replacement))

(defun nt-notes--make (specs)
  "Apply `nt-note--make' to each SPEC."
  (-map (-applify #'nt-note--make) specs))

;;;; Keyword Construction

(defun nt-note--kwd-match (string replacement)
  "The form for FACENAME in font-lock-keyword's MATCH-HIGHNOTEHT."
  (unless (nt-notes<-region (match-beginning 1) (match-end 1))
    (nt-note--init string replacement)))

(defun nt-note--kwd-build (spec)
  "Compose the font-lock-keyword for SPEC in `nt-notes'."
  (-let (((&plist :string string
                  :replacement replacement
                  :rx rx)
          spec))
    `(,rx (0 (prog1 `,(and nt-normalize-height?
                           'nt-note--face)
               (nt-note--kwd-match ,string ,replacement))))))

(defun nt-note--kwds-add ()
  "Build kwds from `nt-notes' and add to `font-lock-keywords'."
  (->> nt-notes
     (-map #'nt-note--kwd-build)
     (font-lock-add-keywords nil)))

;;; Provide

(provide 'nt-note)

;;; nt-note.el ends here
