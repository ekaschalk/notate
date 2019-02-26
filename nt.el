;;; nt.el --- Program with Personalized Notation -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/ekaschalk/nt
;; Version: 0.1
;; Keywords: indentation, display, notes, major-modes
;; Package-Requires: ((cl "1.0") (dash "2.14.1") (dash-functional "1.2.0") (s "1.12.0") (emacs "26.1"))



;;; Commentary:

;; Anotenment and indentation issues hamper ligature's generalization, known as
;; prettified-symbols in Emacs, from adoption. nt attempts to bring the joy
;; and readability of apl to every language!



;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-bounds)
(require 'nt-note)
(require 'nt-mask)
(require 'nt-ov)
(require 'nt-spec)



;;; Configuration
;;;; Core

(defconst nt-specs (nt-specs--make '(("hello" "∧") ("bye" "!∨")))
  "plist of note specifications resulting from `nt-specs--make'.")

(defvar-local nt-bound-fn #'nt-bounds--lisps
  "A function that should return line boundaries [a b) given a NOTE.")

(defvar-local nt-bound?-fn #'nt-bounds?--lisps
  "A subset of `nt-bound-fn', whether NOTE has a boundary.")

;;;; Debugging

(defvar nt-display-prefixes? t
  "Whether to add the `line-prefix' property to indentation overlays.")

(defvar nt-render-masks? t
  "Should masks render? Note that line-prefixes, if set to, still display.")

;;;; Managed

(defvar nt-note-list nil
  "List of note overlays currently managed.")

(defvar nt-mask-list nil
  "List of indent overlays currently managed.

This an ordered list s.t. nt-mask-list[i] = mask-at-line-i+1.

Accessing this should be done through `nt-mask--at' and friends to avoid
confusing indexings.")

(defvar nt-mask--wait-for-refresh nil
  "Let-bind true to hold off on refreshing masks during batch modifications.")



;;; Note-Mask Interactions

(defun nt--masks-for (note)
  "Return all masks NOTE contributes to."
  (-some->>
   note
   (funcall (symbol-value #'nt-bound?-fn))
   (funcall (symbol-value #'nt-bound-fn))
   (apply #'nt-masks--in)
   (-remove (-partial #'nt-mask--contains? note))))

(defun nt--add-note-to-mask (note mask)
  "Add NOTE to a MASK, possibly refresh mask, and return back mask."
  (push note (overlay-get mask 'nt-notes))
  (nt-mask--refresh-maybe mask))

(defun nt--remove-note-from-mask (note mask)
  "Remove NOTE from MASK, possibly refresh mask, and return back mask."
  (delq note (overlay-get mask 'nt-notes))
  (nt-mask--refresh-maybe mask))

(defun nt--map-over-masks (fn note)
  "Map FN over NOTE's masks."
  (->> note nt--masks-for (-map (-partial fn note))))

(defun nt--add-note-to-masks (note)
  "Add NOTE to all masks it contributes to and return them."
  (nt--map-over-masks #'nt--add-note-to-mask note))

(defun nt--remove-note-from-masks (note)
  "Remove NOTE from all masks it contributes to."
  (nt--map-over-masks #'nt--remove-note-from-mask note))

(defun nt--add-notes-to-masks (notes)
  "Batch add NOTES to their masks refreshing upon completion."
  ;; (let ((nt-mask--wait-for-refresh t))
  ;;   (-each notes #'nt--add-note-to-masks))

  ;; (nt-masks--refresh-buffer)

  ;; TODO Test this implementation compared to simpler version above
  (let ((masks))
    (let ((nt-mask--wait-for-refresh t))
      (setq masks (-mapcat #'nt--add-note-to-masks notes)))
    (-> masks -distinct nt-masks--refresh)))

(defun nt--delete-note (note)
  "Delete NOTE and refresh masks it contributed to."
  (when note

    ;; FIXME we skip masks that note already is placed in in masks-for
    ;; so when we remove-note-from-masks, it will skip the ones it needs
    ;; When we delete the note ov, the notes are still contained in the masks,
    ;; but are for no buffer now.

    ;; Perhaps as an optimization (and simpler implementation) we do this:
    ;; 1. Delete the note
    ;; 2. forward line deleting all overlays for no buffers
    ;; 3. once we hit a line without a no buffer, we got to the end

    ;; Note this also allows for efficient batch removal as if we remove many
    ;; notes in some contiguous region, we don't do any more work than if
    ;; if deleting a single note

    (-doto note
      (nt--remove-note-from-masks)
      (nt-note--delete))))



;;; Change Functions
;;;; Utils

(defun nt-change--line-diff ()
  "Lines added: +x, removed: -x, otherwise 0 since mask list last updated."
  ;; NOTE 1- point-max easier than calling skip-line on last-line's mask
  (- (line-number-at-pos (1- (point-max)))
     (length nt-mask-list)))

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

           ;; Must init masks ASAP for `nt-mask-list' integrity
           (masks
            ;; (nt-masks--init start-line end-line)
            (-map #'nt-mask--init
                  (number-sequence start-line (1- end-line)))
            )

           (mask-before-change (nt-mask--at line-before-change))
           (notes-before-change (overlay-get mask-before-change 'nt-notes))

           (notes (-union notes-before-change
                          (nt-notes--at line-before-change))))

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

(defun nt-after-change-function (start end chars-deleted)
  "See `after-change-functions'."
  (if (= 0 chars-deleted)
      (nt-change--insertion start end)
    (nt-change--deletion start chars-deleted)))



;;; Interactive
;;;; Setup

(defun nt-setup--agnostic ()
  "Setup all *major-mode-agnostic* components."
  (nt-masks--init)
  (nt-masks--refresh nt-mask-list))

;;;; Commands

(defun nt-disable ()
  "Delete overlays managed by nt."
  (interactive)

  (nt-ov--remove-all)
  (remove-hook 'lisp-mode-hook #'nt-kwds--add)
  (remove-hook 'after-change-functions #'nt-after-change-function 'local)

  ;; just-in-case stuff
  (setq nt-mask--wait-for-refresh nil)
  (setq font-lock-keywords nil))

;;;###autoload
(defun nt-enable ()
  "Enable nt and cleanup previous instance if running."
  (interactive)

  (nt-disable)
  (nt-setup--agnostic)

  (add-hook 'lisp-mode-hook #'nt-spec--kwds-add)
  (let ((nt-mask--wait-for-refresh t))
    (lisp-mode)
    (font-lock-ensure))

  (nt-masks--refresh-buffer)
  (add-hook 'after-change-functions #'nt-after-change-function nil 'local)

  ;; This isn't working yet, not highest priority
  ;; (advice-remove 'undo-tree-undo #'nt-masks--unrender-buffer)
  ;; (advice-remove 'undo-tree-undo #'nt-masks--render-buffer)
  ;; (advice-add 'undo-tree-undo :before #'nt-masks--unrender-buffer)
  ;; (advice-add 'undo-tree-undo :after #'nt-masks--render-buffer)
  )

(defun nt-remove-note-at-point ()
  "Delete note at point if it exists and update masks."
  (interactive)

  (nt--delete-note (nt-note--at-point)))



(provide 'nt)



;;; nt.el ends here
