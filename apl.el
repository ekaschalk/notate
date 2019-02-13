;;; apl.el --- Always Program in Ligatures -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/ekaschalk/apl
;; Version: 0.1
;; Keywords: indentation, display, ligatures, major-modes
;; Package-Requires: ((cl "1.0") (dash "2.14.1") (dash-functional "1.2.0") (s "1.12.0") (emacs "26.1"))



;;; Commentary:

;; Alignment and specifically indentation issues hamper generalized ligatures,
;; known as prettified-symbols in Emacs. APL attempts to bring the joy and
;; readability of APLang to every language!



;;; Code:
;;;; Requires

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'smartparens)



;;; Configuration
;;;; Utils

(defun apl-make-spec (name string replacement &optional rx)
  "Create spec plist NAME for STRING to REPLACEMENT optionally with custom RX.

Without a RX given, default to matching entire STRING.
The RX, if given, should set the first group for the match to replace."
  `(:name
    ,name
    :string      ,string
    :rx          ,(or rx
                      `,(rx-to-string `(group ,string)
                                      'no-shy-group))
    :replacement ,replacement
    :width       ,(- (length string)
                     (length replacement))))

(defun apl-make-specs (specs)
  "Apply `apl-make-spec' to each SPEC."
  (-map (-applify #'apl-make-spec) specs))

;;;; Configured

(defconst apl-specs
  (apl-make-specs '(("Hello Lig"   "hello"     "")
                    ("0-space Lig" "0-space"   "")
                    ("1-space Lig" "1-space"   " ")
                    ("2-space Lig" "2-space"   "  ")
                    ("tab Lig"     "tab-space" "	")))
  "Collection of specs from `apl-make-spec'.")

(defconst apl-display-prefixes? t
  "Whether to add the `line-prefix' property to indentation overlays.")

(defconst apl-lig--boundary-fn #'apl-lig--boundary-fn--lisps
  "A function that should return a cons of line boundaries given a LIG.")

;; Below fn not used yet
(defconst apl-lig--boundary?-fn #'apl-lig--boundary-fn--lisps
  "A subset of `apl-lig--boundary-fn', whether LIG has a boundary.")

;;;; Managed

(defconst apl-lig-list nil
  "List of ligature overlays currently managed.")

(defconst apl-mask-list nil
  "List of indent overlays currently managed.")

(defconst apl-mask--wait-for-refresh nil
  "Let-bind true to hold off on refreshing masks during batch modifications.")



;;; Overlays
;;;; Predicates

(defun apl-ov--lig? (ov)
  "Is OV a ligature?"
  (overlay-get ov 'apl-lig?))

(defun apl-ov--mask? (ov)
  "Is OV a mask?"
  (overlay-get ov 'apl-mask?))

(defun apl-ov--in? (ov start end)
  "Is OV contained within START and END?"
  (and ov start end
       (<= start (overlay-start ov) (overlay-end ov) end)))

(defun apl-ov--in-match? (ov subexp)
  "Is OV contained in the SUBEXP matching group?"
  (apl--ov-in-bound? ov (match-beginning subexp) (match-end subexp)))

;;;; Utils

(defun apl-ovs--map (ovs prop fn)
  "Map FN over OVS PROP."
  (--map (overlay-get it prop) fn))

(defun apl-ov--present? (start end prop)
  "Is a lig present within START and END with non-nil PROP? Return it."
  (and start end
       (-any (-cut #'overlay-get <> prop)
             (overlays-in start end))))



;;; Ligs
;;;; Boundary Functions

(defun apl-lig--boundary-fn--lisps (lig)
  "Calculate line boundary cons for LIG's masks."
  (let ((lig-line (-> ov overlay-start line-number-at-pos)))
    (cons (1+ lig-line)
          (save-excursion
            (goto-line lig-line)
            (sp-end-of-sexp)
            (line-number-at-pos)))))

(defun apl-lig--boundary?--lisps (lig)
  "Does LIG have an indentation boundary? A weaker version of boundary-fn."
  ;; 1. Is the lig a form opener?
  ;; 2. Is the lig already modifying indentation?
  ;; 3. Are there more lines?
  ;; Note that we should store the reason why it fails, as an optimization
  ;; we can utilize when modifications are performed on a line containing LIG
  t)

;;;; Unorganized

(defun apl-lig--delete (lig)
  "Delete LIG."
  (delq lig apl-lig-list)
  (delete-overlay lig))

(defun apl-ligs->width (ligs)
  "Sum widths of LIGS."
  (apl-ovs--map ligs 'apl-width #'sum))

(defun apl-lig--decompose-hook (lig post-modification? start end &optional _)
  "Decompose LIG upon modification as a modification-hook."
  (when post-modification?
    (apl-lig-mask--remove-lig-from-masks lig)
    (apl-lig--delete lig)))

(defun apl-lig--init-lig-ov (ov replacement width)
  "Put lig text properties into OV."
  (-doto ov
    (overlay-put 'apl?      t)
    (overlay-put 'apl-lig?  t)
    (overlay-put 'apl-width width)

    (overlay-put 'display replacement)
    (overlay-put 'modification-hooks '(apl-lig--decompose-hook))))

(defun apl-lig--init-lig (replacement width)
  "Build ligature overlay for current `match-data'."
  (let* ((start (match-beginning 1))
         (end   (match-end 1))
         (lig   (apl-lig--init-lig-ov (make-overlay start end) replacement width)))
    (push lig apl-lig-list)
    (apl-lig-mask--add-lig-to-masks lig)))



;;; Masks

(defun apl-mask--at (line)
  "Retrieve mask at LINE."
  (nth line apl-mask-list))

(defun apl-masks--at (lines)
  "Retrieve masks at LINES."
  (-select-by-indices lines apl-masks))

(defun apl-masks--in (start-line end-line)
  "Retrieve masks within START-LINE and END-LINE."
  (-slice apl-mask-list start-line end-line))

(defun apl-mask--insert-at (mask line)
  "Insert MASK at LINE."
  (-insert-at line ov apl-masks))

(defun apl-mask--line-count-modified? ()
  "Positive if lines have been added, negative if removed, otherwise zero."
  (- (line-number-at-pos (point-max))
     (length apl-masks)))

(defun apl-mask--indent-col (&optional n)
  "Get indentation col, of line forward N-1 times if given."
  (save-excursion (end-of-line n) (back-to-indentation) (current-column)))

(defun apl-mask--delete (mask)
  "Delete MASK."
  (delq lig apl-lig-list)
  (delete-overlay mask))

(defun apl-mask--decompose-hook (mask post-mod? start end &optional _)
  "Overlay modification hook to delete indent ov upon modification within it."
  (when post-mod?
    (let* ((inhibit-modification-hooks t)
           (width                      (apl-mask->width mask))
           (invis-spaces-to-delete     (1+ width)))
      (apl-mask--delete mask)
      (evil-with-single-undo
        (delete-char (- invis-spaces-to-delete))))))

(defun apl-mask--set-prefix (mask)
  "Format and set the `line-prefix' overlay text property for MASK."
  (let* ((sep         "|")
         (true-indent (apl-mask--indent-col))
         (width       (apl-mask->width))
         (num-parents (length (overlay-get mask 'apl-ligs)))
         (sections    (list (-> "%02d" (format true-indent))
                            (-> "%02d" (format width))
                            (-> "#%d:" (format num-parents)))))
    (->> sections (-interpose sep) (apply #'s-concat))))

(defun apl-mask--init-mask-ov (ov)
  "Put mask text properties into OV."
  (-doto ov
    (overlay-put 'apl?      t)
    (overlay-put 'apl-mask? t)
    (overlay-put 'apl-ligs  nil)

    (overlay-put 'face               'underline)
    (overlay-put 'display            " ")
    (overlay-put 'line-prefix        (apl--format-prefix 1 0))
    (overlay-put 'modification-hooks '(apl-mask--decompose-hook))))

(defun apl-mask--init-mask (&optional line)
  "Create empty mask for LINE, otherwise current line."
  (save-excursion
    (when line (goto-line line))

    (let* ((line  (line-number-at-pos))
           (start (line-beginning-position))
           (end   (1+ start))
           (mask  (apl-mask--init-mask-ov (make-overlay start end))))
      (apl-mask--insert-at mask line))))

(defun apl-mask--init-masks ()
  "Line-by-line buildup `apl-masks'."
  (save-excursion
    (goto-char (point-min))

    (while (not (eobp))
      (apl-mask--init-mask)
      (forward-line))))

(defun apl-mask->width (mask)
  "Calculate width of MASK's ligs."
  (-> mask (overlay-get 'apl-ligs) apl-ligs->width))

(defun apl-mask--recenter (mask)
  "Recenter MASK, ie. reset its end position based on ligs widths."
  (let ((start (overlay-start mask))
        (width (apl-mask->width mask)))
    (move-overlay mask start (+ start width))))

(defun apl-mask--refresh (mask)
  "Reset bounds and boundary-dependent properties of MASK based on cur ligs."
  (-doto mask
    (apl-mask--recenter)
    (apl-mask--set-prefix)))

(defun apl-mask--refresh-maybe (mask)
  "Perform `apl-mask--refresh' when we should."
  (unless apl-mask--wait-for-refresh
    (apl-mask--refresh mask)))

(defun apl-masks--refresh (masks)
  "Refresh MASKS."
  (-each masks #'apl-mask--refresh-maybe))



;;; Mask-Lig Interface

(defun apl-lig-mask--masks-for (lig)
  "Return all masks LIG contributes to."
  (->> lig (funcall #'apl-lig--boundary-fn) (apply #'apl-masks--in)))

(defun apl-lig-mask--add-lig-to-mask (lig mask)
  (push lig (overlay-get mask 'apl-ligs))
  (apl-mask--refresh-maybe mask))

(defun apl-lig-mask--remove-lig-from-mask (lig mask)
  "Remove LIG from MASK."
  (delq lig (overlay-get mask 'apl-ligs))
  (apl-mask--refresh-maybe mask))

(defun apl-lig-mask--add-lig-to-masks (lig)
  "Add LIG to all masks it contributes to."
  (-each (apl-lig-mask--masks-for lig)
    (-partial #'apl-lig-mask--add-lig-to-mask lig)))

(defun apl-lig-mask--remove-lig-from-masks (lig)
  "Remove LIG from all masks it contributes to."
  (-each (apl-lig-mask--masks-for lig)
    (-partial #'apl-lig-mask--remove-lig-from-mask lig)))



;;; Font Locks

(defun apl-kwd--match (replacement width)
  "The form for FACENAME in font-lock-keyword's MATCH-HIGHLIGHT."
  (unless (apl-ov--present? (match-beginning 1) (match-end 1) 'apl-lig?)
    (apl-lig--init replacement width)))

(defun apl-kwd--build (spec)
  "Compose the font-lock-keyword for SPEC in `apl-specs'."
  (-let (((&plist :name name
                  :replacement replacement
                  :rx rx
                  :width width)
          spec))
    `(,rx (0 (prog1 nil (apl-kwd--match ,replacement ,width))))))

(defun apl-kwds--build ()
  "Build kwds from `apl-specs' and add to `font-lock-keywords'."
  (let ((kwds (-map #'apl-kwd--build apl-specs)))
    (font-lock-add-keywords nil kwds)))



;;; Interactive

(defun apl-disable ()
  "Delete overlays managed by apl."
  (interactive)

  (-each apl-mask-list #'apl-mask--delete)
  (-each apl-lig-list #'apl-lig--delete)
  (setq font-lock-keywords nil)
  (remove-hook 'lisp-mode-hook #'apl-add-kwds))

(defun apl-enable ()
  "Enable apl and cleanup previous instance if running."
  (interactive)

  (apl-disable)
  (apl-init-masks)
  (add-hook 'lisp-mode-hook #'apl-add-kwds nil 'local)
  (lisp-mode))



;;; Scratch

(when nil
  (spacemacs/declare-prefix "d" "dev")
  (spacemacs/set-leader-keys "de" #'apl-enable)
  (spacemacs/set-leader-keys "dd" #'apl-disable))



;;; Footer

(provide 'apl)



;;; apl.el ends here
