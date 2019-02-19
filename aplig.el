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
;; prettified-symbols in Emacs. aplig attempts to bring the joy and readability
;; of apl to every language!



;;; Code:
;;;; Requires

(require 'aplig-base)

(require 'aplig-ov)
(require 'aplig-spec)



;;; Configuration

(defconst aplig-specs (aplig-specs--make '(("hello" "∧") ("bye" "!∨")))
  "Collection of specs from `aplig-spec--make'.")

(defconst aplig-lig--boundary-fn #'aplig-lig--boundary--lisps
  "A function that should return line boundaries given a LIG.")

(defconst aplig-lig--boundary?-fn #'aplig-lig--boundary?--lisps
  "A subset of `aplig-lig--boundary-fn', whether LIG has a boundary.")

;;;; Debugging

(defconst aplig-display-prefixes? t
  "Whether to add the `line-prefix' property to indentation overlays.")

(defconst aplig-render-masks? t
  "Should masks render? Note that line-prefixes, if set to, still display.")

;;;; Managed

(defconst aplig-lig-list nil
  "List of ligature overlays currently managed.")

(defconst aplig-mask-list nil
  "List of indent overlays currently managed.")

(defconst aplig-mask--wait-for-refresh nil
  "Let-bind true to hold off on refreshing masks during batch modifications.")



;;; Ligs
;;;; Boundary Functions

(defun aplig-lig--boundary--lisps (lig)
  "Calculate line boundary [a b) for LIG's masks."
  (let* ((start (overlay-start lig))
         (line (line-number-at-pos start))
         (max-line (line-number-at-pos (point-max))))
    (list (min line max-line)
          (save-excursion
            (goto-char start)
            (sp-end-of-sexp)
            (line-number-at-pos)))))

(defun aplig-lig--boundary?-fn--heuristic--lisps (lig)
  "Hueristic version of boundary predicate."
  (save-excursion
    (aplig-ov--goto lig)

    (let* ((at-form-opener? (null (ignore-errors (backward-sexp) t)))

           ;; (declare indent)
           in-specially-indented-body?

           ;; (foo lig (foo foo
           ;;               foo))
           another-form-opener-same-line?

           ;; (lig
           ;;  foo)
           only-sexp-on-its-line?)
      (and at-form-opener?
           (not in-specially-indented-body?)
           (not another-form-opener-same-line?)
           (not only-sexp-on-its-line?)))))

(defun aplig-lig--boundary?--lisps (lig)
  "Does LIG have an indentation boundary? Return back LIG if it does."
  ;; Always-correct boundary-fn much more involved than a good-enough heuristic
  ;; It must correspond to `lisp-indent-function', but given its calling
  ;; convention, likely difficult to emulate in a non-temp-buffer-based impl.
  (and (aplig-lig--boundary?-fn--heuristic--lisps lig)
       lig))

;;;; Overlays

(defun aplig-ligs--present? (&optional start end)
  "Are ligs present within START and END, defaulting to `match-data'? Get it."
  (let ((start (or start (match-beginning 1)))
        (end   (or end (match-end 1))))
    (and start end
         (-filter #'aplig-ov--lig?
                  (overlays-in start end)))))

(defun aplig-ligs--at (line)
  "Return all ligs on LINE."
  (aplig-ligs--present?
   (save-excursion (goto-line line) (line-beginning-position))
   (line-end-position)))

(defun aplig-lig--delete (lig)
  "Delete LIG."
  (delq lig aplig-lig-list)
  (delete-overlay lig))

(defun aplig-lig--decompose-hook (lig post-modification? start end &optional _)
  "Decompose LIG upon modification as a modification-hook."
  (when post-modification?
    (aplig-lig-mask--remove-lig-from-masks lig)
    (aplig-lig--delete lig)))

(defun aplig-lig--init-ov (ov replacement width)
  "Put lig text properties into OV."
  (-doto ov
    (overlay-put 'aplig?      t)
    (overlay-put 'aplig-lig?  t)
    (overlay-put 'aplig-width width)

    (overlay-put 'display replacement)
    (overlay-put 'modification-hooks '(aplig-lig--decompose-hook))))

;;;; Methods

(defun aplig-ligs->width (ligs)
  "Sum widths of LIGS."
  (-> ligs (aplig-ovs--prop 'aplig-width) -sum))

(defun aplig-lig--init (replacement width &optional start end)
  "Build ligature overlay, defaulting to `match-data' for START and END."
  (unless (or (or start (match-beginning 1))
              (or end   (match-end 1)))
    (error "Initiatializing ligature without match-data set."))

  (let* ((start (or start (match-beginning 1)))
         (end   (or end (match-end 1)))
         (ov    (make-overlay start end))
         (lig   (aplig-lig--init-ov ov replacement width)))
    (push lig aplig-lig-list)
    (aplig-lig-mask--add-lig-to-masks lig)
    lig))



;;; Masks
;;;; Lines

(defun aplig-mask--indent-col (&optional n)
  "Get indentation col, of line forward N-1 times if given."
  ;; NOTE For lisps `calculate-lisp-indent', though that does alot of extra work
  (save-excursion (end-of-line n) (back-to-indentation) (current-column)))

(defun aplig-mask--at (line)
  "Retrieve mask at LINE."
  (nth line aplig-mask-list))

(defun aplig-masks--at (lines)
  "Retrieve masks at LINES."
  (-select-by-indices lines aplig-mask-list))

(defun aplig-masks--in (start-line end-line)
  "Retrieve masks within START-LINE and END-LINE."
  (-slice aplig-mask-list start-line end-line))

(defun aplig-mask--insert-at (mask line)
  "Insert MASK at LINE into `aplig-mask-list'."
  (setq aplig-mask-list (-insert-at line mask aplig-mask-list)))

;;;; Overlays

(defun aplig-mask--delete (mask)
  "Delete MASK."
  (delq mask aplig-mask-list)
  (delete-overlay mask))

(defun aplig-mask--decompose-hook (mask post-mod? start end &optional _)
  "Overlay modification hook to delete indent ov upon modification within it."
  ;; NOTE probably need to handle deleting forward differently
  ;; NOTE probably need to handle visual deletion differently
  (when post-mod?
    (let* ((inhibit-modification-hooks t)
           (width                      (aplig-mask->width mask))
           (invis-spaces-to-delete     (1+ width)))
      (aplig-mask--delete mask)
      (evil-with-single-undo
        (delete-char (- invis-spaces-to-delete))))))

(defun aplig-mask--format-prefix (mask)
  "Format the `line-prefix' overlay text property for MASK."
  (let* ((sep         "|")
         (true-indent (save-excursion
                        (aplig-ov--goto mask)
                        (aplig-mask--indent-col)))
         (width       (aplig-mask->width mask))
         (ligs        (overlay-get mask 'aplig-ligs))
         (sections    (list (-> "%02d" (format true-indent))
                            (-> "%02d" (format width))
                            (-> "+%d " (format (length ligs))))))
    (->> sections (-interpose sep) (apply #'s-concat))))

(defun aplig-mask--reset-prefix (mask)
  "Reset the `line-prefix' overlay text property for MASK."
  (when aplig-display-prefixes?
    (->> mask aplig-mask--format-prefix (overlay-put mask 'line-prefix))))

(defun aplig-mask--init-ov (ov)
  "Put always-on text properties for masks into OV."
  (-doto ov
    (overlay-put 'aplig?      t)
    (overlay-put 'aplig-mask? t)
    (overlay-put 'aplig-ligs  nil)

    (overlay-put 'modification-hooks '(aplig-mask--decompose-hook))))

(defun aplig-mask--recenter (mask)
  "Recenter MASK, ie. reset its end position based on ligs widths."
  (let* ((start (overlay-start mask))
         (width (aplig-mask->width mask))
         (end   (1+ (+ start width))))  ; 1+ opens RHS to match overlay defs
    (move-overlay mask start end)))

(defun aplig-mask--render? (mask)
  "Should MASK be rendered?"
  (and aplig-render-masks?
       (> (aplig-mask->width mask) 0)))

(defun aplig-mask--render (mask)
  "Set display-based overlay properties for MASK."
  (-doto mask
    (overlay-put 'face    'underline)
    (overlay-put 'display " ")))

(defun aplig-mask--unrender (mask)
  "Remove display-based overlay properties for MASK."
  ;; For line-prefix no need to nil, it is not rendered when display is nil
  (-doto mask
    (overlay-put 'face    nil)
    (overlay-put 'display nil)))

(defun aplig-mask--reset-display (mask)
  "Reset display-affecting text properties of MASK."
  (if (aplig-mask--render? mask)
      (aplig-mask--render mask)
    (aplig-mask--unrender mask)))

(defun aplig-mask--refresh (mask)
  "Reset bounds and boundary-dependent properties of MASK based on cur ligs."
  (-doto mask
    (aplig-mask--recenter)
    (aplig-mask--reset-prefix)
    (aplig-mask--reset-display)))

(defun aplig-mask--refresh-maybe (mask)
  "Perform `aplig-mask--refresh' when we should."
  (unless aplig-mask--wait-for-refresh
    (aplig-mask--refresh mask)))

(defun aplig-masks--refresh (masks)
  "Refresh MASKS."
  (-each masks #'aplig-mask--refresh-maybe))

(defun aplig-masks--refresh-buffer ()
  "Refresh `aplig-mask-list'."
  (aplig-masks--refresh aplig-mask-list))

;;;; Methods

(defun aplig-mask->width (mask)
  "Calculate width of MASK's ligs."
  (-> mask (overlay-get 'aplig-ligs) aplig-ligs->width))

(defun aplig-mask--init (&optional line)
  "Create empty mask for LINE, otherwise current line."
  (save-excursion
    (when line (goto-line line))

    (let* ((line  (line-number-at-pos))
           (start (line-beginning-position))
           (end   (1+ start))
           (mask  (aplig-mask--init-ov (make-overlay start end))))
      (aplig-mask--insert-at mask line)
      mask)))

(defun aplig-masks--init ()
  "Line-by-line buildup `aplig-mask-list'."
  (save-excursion
    (goto-char (point-min))

    (while (not (eobp))
      (aplig-mask--init)
      (forward-line))))



;;; Lig-Mask Interface

(defun aplig-lig-mask--masks-for (lig)
  "Return all masks LIG contributes to."
  (-some->>
   lig
   (funcall (symbol-value #'aplig-lig--boundary?-fn))
   (funcall (symbol-value #'aplig-lig--boundary-fn))
   (apply #'aplig-masks--in)
   (-remove (-partial #'aplig-lig-mask--skip? lig))))

(defun aplig-lig-mask--skip? (lig mask)
  "Should MASK in boundary of LIG be skipped when adding LIG to its masks?"
  (save-excursion
    (aplig-ov--goto mask)
    (let* ((line-width (- (line-end-position)
                          (line-beginning-position)))
           (mask-width (aplig-mask->width mask))
           (mask-potential-width (+ mask-width (overlay-get lig 'aplig-width)))
           (lig-already-in-mask? (-contains? (overlay-get mask 'aplig-ligs) lig)))
      ;; (message "Looking at lig %s mask %s. Potential: %s. Has: %s"
      ;;          lig mask mask-potential-width lig-already-in-mask?)

      ;; have to insert two characters for the mask to kick in, why?

      (or lig-already-in-mask?
          (<= line-width mask-potential-width)))))

(defun aplig-lig-mask--add-lig-to-mask (lig mask)
  "Add LIG to a MASK and possibly refresh it."
  (push lig (overlay-get mask 'aplig-ligs))
  (aplig-mask--refresh-maybe mask))

(defun aplig-lig-mask--remove-lig-from-mask (lig mask)
  "Remove LIG from MASK."
  (delq lig (overlay-get mask 'aplig-ligs))
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
           (masks (-map #'aplig-mask--init
                        (number-sequence start-line (1- end-line))))  ; incl.

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



;;; Font Locks

(defun aplig-kwd--match (replacement width)
  "The form for FACENAME in font-lock-keyword's MATCH-HIGHLIGHT."
  (unless (aplig-ligs--present?)
    (aplig-lig--init replacement width)))

(defun aplig-kwd--build (spec)
  "Compose the font-lock-keyword for SPEC in `aplig-specs'."
  (-let (((&plist :replacement replacement
                  :rx rx
                  :width width)
          spec))
    `(,rx (0 (prog1 nil (aplig-kwd--match ,replacement ,width))))))

(defun aplig-kwds--add ()
  "Build kwds from `aplig-specs' and add to `font-lock-keywords'."
  (let ((kwds (-map #'aplig-kwd--build aplig-specs)))
    (font-lock-add-keywords nil kwds)))



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

  (-each aplig-mask-list #'aplig-mask--delete)
  (-each aplig-lig-list #'aplig-lig--delete)
  (setq aplig-mask-list nil)
  (setq aplig-lig-list nil)

  ;; Below aren't needed in normal use, resets buffer just in case
  (setq aplig-mask--wait-for-refresh nil)
  (remove-overlays nil nil 'aplig? t)

  (setq font-lock-keywords nil)
  (remove-hook 'lisp-mode-hook #'aplig-kwds--add)
  (remove-hook 'after-change-functions #'aplig-after-change-function 'local))

(defun aplig-enable ()
  "Enable aplig and cleanup previous instance if running."
  (interactive)

  (aplig-disable)
  (aplig-setup--agnostic)

  (add-hook 'lisp-mode-hook #'aplig-kwds--add)
  (let ((aplig-mask--wait-for-refresh t))
    (lisp-mode)
    (font-lock-ensure))

  (aplig-masks--refresh-buffer)
  (add-hook 'after-change-functions #'aplig-after-change-function nil 'local))



(provide 'aplig)



;;; aplig.el ends here
