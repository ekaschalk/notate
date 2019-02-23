;;; aplig.el --- Always Program with Ligatures -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/ekaschalk/aplig
;; Version: 0.1
;; Keywords: indentation, display, ligatures, major-modes
;; Package-Requires: ((cl "1.0") (dash "2.14.1") (dash-functional "1.2.0") (s "1.12.0") (emacs "26.1"))



;;; Commentary:

;; Alignment and indentation issues hamper ligature's generalization, known as
;; prettified-symbols in Emacs, from adoption. aplig attempts to bring the joy
;; and readability of apl to every language!



;;; Code:
;;;; Requires

(require 'aplig-base)

(require 'aplig-bounds)
(require 'aplig-lig)
(require 'aplig-mask)
(require 'aplig-ov)
(require 'aplig-spec)



;;; Configuration

(defconst aplig-specs (aplig-specs--make '(("hello" "∧") ("bye" "!∨")))
  "Collection of specs from `aplig-spec--make'.")

(defvar-local aplig-bound-fn #'aplig-bounds--lisps
  "A function that should return line boundaries [a b) given a LIG.")

(defvar-local aplig-bound?-fn #'aplig-bounds?--lisps
  "A subset of `aplig-bound-fn', whether LIG has a boundary.")

;;;; Debugging

(defconst aplig-display-prefixes? t
  "Whether to add the `line-prefix' property to indentation overlays.")

(defconst aplig-render-masks? t
  "Should masks render? Note that line-prefixes, if set to, still display.")

;;;; Managed

(defconst aplig-lig-list nil
  "List of ligature overlays currently managed.")

(defconst aplig-mask-list nil
  "List of indent overlays currently managed.

This an ordered list s.t. aplig-mask-list[i] = mask-at-line-i+1.

Accessing this should be done through `aplig-mask--at' and friends to avoid
confusing indexings.")

(defconst aplig-mask--wait-for-refresh nil
  "Let-bind true to hold off on refreshing masks during batch modifications.")



;;; Lig-Mask Interface

(defun aplig-lig-mask--masks-for (lig)
  "Return all masks LIG contributes to."
  (-some->>
   lig
   (funcall (symbol-value #'aplig-bound?-fn))
   (funcall (symbol-value #'aplig-bound-fn))
   (apply #'aplig-masks--in)
   (-remove (-partial #'aplig-lig-mask--skip? lig))))

(defun aplig-lig-mask--skip? (lig mask)
  "Should MASK in boundary of LIG be skipped when adding LIG to its masks?"
  (save-excursion
    (aplig-ov--goto mask)

    (let* ((line-width (- (line-end-position)
                          (line-beginning-position)))
           (mask-width (aplig-mask->width mask))
           (mask-potential-width (-> lig aplig-lig->width (+ mask-width)))

           (lig-already-in-mask? (-> mask aplig-mask->ligs (-contains? lig)))
           (line-too-small? (<= line-width mask-potential-width)))

      ;; (message "Looking at lig %s mask %s. Potential: %s. Has: %s"
      ;;          lig mask mask-potential-width lig-already-in-mask?)

      (or lig-already-in-mask? line-too-small?))))

(defun aplig-lig-mask--add-lig-to-mask (lig mask)
  "Add LIG to a MASK and possibly refresh it."
  (push lig (overlay-get mask 'aplig-ligs))
  (aplig-mask--refresh-maybe mask))

(defun aplig-lig-mask--remove-lig-from-mask (lig mask)
  "Remove LIG from MASK."
  (delq lig (aplig-mask->ligs mask))
  (aplig-mask--refresh-maybe mask))

(defun aplig-lig-mask--add-lig-to-masks (lig)
  "Add LIG to all masks it contributes to."
  (-each (aplig-lig-mask--masks-for lig)
    (-partial #'aplig-lig-mask--add-lig-to-mask lig)))

(defun aplig-lig-mask--remove-lig-from-masks (lig)
  "Remove LIG from all masks it contributes to."
  (-each (aplig-lig-mask--masks-for lig)
    (-partial #'aplig-lig-mask--remove-lig-from-mask lig)))

(defun aplig-lig-mask--add-ligs-to-masks (ligs)
  "Batch add LIGS to their masks refreshing upon completion."
  (let ((aplig-mask--wait-for-refresh t))
    (-each ligs #'aplig-lig-mask--add-lig-to-masks))

  ;; NOTE Easier atm to just refresh the buffer than the correct intervals
  (aplig-masks--refresh-buffer))

(defun aplig-lig-mask--delete-lig (lig)
  "Delete LIG and refresh masks it contributed to."
  (when lig

    ;; FIXME we skip masks that lig already is placed in in masks-for
    ;; so when we remove-lig-from-masks, it will skip the ones it needs
    ;; When we delete the lig ov, the ligs are still contained in the masks,
    ;; but are for no buffer now.

    ;; Perhaps as an optimization (and simpler implementation) we do this:
    ;; 1. Delete the lig
    ;; 2. forward line deleting all overlays for no buffers
    ;; 3. once we hit a line without a no buffer, we got to the end

    ;; Note this also allows for efficient batch removal as if we remove many
    ;; ligs in some contiguous region, we don't do any more work than if
    ;; if deleting a single lig

    (-doto lig
      (aplig-lig-mask--remove-lig-from-masks)
      (aplig-lig--delete))))



;;; Change Functions
;;;; Utils

(defun aplig-change--line-diff ()
  "Lines added: +x, removed: -x, otherwise 0 since mask list last updated."
  ;; NOTE 1- point-max easier than calling skip-line on last-line's mask
  (- (line-number-at-pos (1- (point-max)))
     (length aplig-mask-list)))

(defun aplig-change--new-lines? ()
  "Return count of lines added since last update or nil."
  (let ((line-change (aplig-change--line-diff)))
    (when (> line-change 0)
      line-change)))

(defun aplig-change--removed-lines? ()
  "Return count of lines removed since last update or nil."
  (let ((line-change (aplig-change--line-diff)))
    (when (< line-change 0)
      (- line-change))))

;;;; Insertion

(defun aplig-change--insertion (start end)
  "Change function specialized for insertion, in START and END."
  (-when-let (new-lines (aplig-change--new-lines?))
    (let* ((end-line (1+ (line-number-at-pos)))
           (start-line (- end-line new-lines))
           (line-before-change (1- start-line))

           ;; Must init masks ASAP for `aplig-mask-list' integrity
           (masks
            ;; (aplig-masks--init start-line end-line)
            (-map #'aplig-mask--init
                  (number-sequence start-line (1- end-line)))
            )

           (mask-before-change (aplig-mask--at line-before-change))
           (ligs-before-change (overlay-get mask-before-change 'aplig-ligs))

           (ligs (-union ligs-before-change
                         (aplig-ligs--at line-before-change))))

      ;; The lig is being added to mask but it takes 2 insertions to "kick in"
      ;; for some reason?
      ;; (aplig-lig-mask--add-ligs-to-masks ligs)
      (-each ligs #'aplig-lig-mask--add-lig-to-masks)

      (message "%s %s" start end)
      )))

;;;; Deletion

(defun aplig-change--deletion (pos chars-deleted)
  "Change function specialized for deletion, number CHARS-DELETED at POS."
  ;; (message "Deleting at pos %s, %s characters" pos chars-deleted)


  )

;;;; Hook

(defun aplig-after-change-function (start end chars-deleted)
  "See `after-change-functions'."
  (if (= 0 chars-deleted)
      (aplig-change--insertion start end)
    (aplig-change--deletion start chars-deleted)))



;;; Interactive
;;;; Setup

(defun aplig-setup--agnostic ()
  "Setup all *major-mode-agnostic* components."
  (aplig-masks--init)
  (aplig-masks--refresh aplig-mask-list))

;;;; Toggling

(defun aplig-disable ()
  "Delete overlays managed by aplig."
  (interactive)

  (aplig-ov--remove-all)
  (remove-hook 'lisp-mode-hook #'aplig-kwds--add)
  (remove-hook 'after-change-functions #'aplig-after-change-function 'local)

  ;; just-in-case stuff
  (setq aplig-mask--wait-for-refresh nil)
  (setq font-lock-keywords nil))

;;;###autoload
(defun aplig-enable ()
  "Enable aplig and cleanup previous instance if running."
  (interactive)

  (aplig-disable)
  (aplig-setup--agnostic)

  (add-hook 'lisp-mode-hook #'aplig-spec--kwds-add)
  (let ((aplig-mask--wait-for-refresh t))
    (lisp-mode)
    (font-lock-ensure))

  (aplig-masks--refresh-buffer)
  (add-hook 'after-change-functions #'aplig-after-change-function nil 'local))

;;;; Commands

(defun aplig-remove-lig-at-point ()
  "Delete lig at point if it exists and update masks."
  (interactive) (aplig-lig-mask--delete-lig (aplig-lig--at-point)))



(provide 'aplig)



;;; aplig.el ends here
