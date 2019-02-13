;;; aplig.el --- Always Program with Ligatures -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/ekaschalk/aplig
;; Version: 0.1
;; Keywords: indentation, display, ligatures, major-modes
;; Package-Requires: ((cl "1.0") (dash "2.14.1") (dash-functional "1.2.0") (s "1.12.0") (emacs "26.1"))



;;; Commentary:

;; Alignment and specifically indentation issues hamper generalized ligatures,
;; known as prettified-symbols in Emacs. aplig attempts to bring the joy and
;; readability of APL to every language!



;;; Code:
;;;; Requires

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'smartparens)



;;; Configuration
;;;; Utils

(defun aplig-make-spec (name string replacement &optional rx)
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

(defun aplig-make-specs (specs)
  "Apply `aplig-make-spec' to each SPEC."
  (-map (-applify #'aplig-make-spec) specs))

;;;; Configured

(defconst aplig-specs
  (aplig-make-specs '(("Hello Lig"   "hello"     "")
                      ("0-space Lig" "0-space"   "")
                      ("1-space Lig" "1-space"   " ")
                      ("2-space Lig" "2-space"   "  ")
                      ("tab Lig"     "tab-space" "	")))
  "Collection of specs from `aplig-make-spec'.")

(defconst aplig-display-prefixes? t
  "Whether to add the `line-prefix' property to indentation overlays.")

(defconst aplig-lig--boundary-fn #'aplig-lig--boundary-fn--lisps
  "A function that should return a cons of line boundaries given a LIG.")

;; Below fn not used yet
(defconst aplig-lig--boundary?-fn #'aplig-lig--boundary-fn--lisps
  "A subset of `aplig-lig--boundary-fn', whether LIG has a boundary.")

;;;; Managed

(defconst aplig-lig-list nil
  "List of ligature overlays currently managed.")

(defconst aplig-mask-list nil
  "List of indent overlays currently managed.")

(defconst aplig-mask--wait-for-refresh nil
  "Let-bind true to hold off on refreshing masks during batch modifications.")



;;; Overlays
;;;; Predicates

(defun aplig-ov--lig? (ov)
  "Is OV a ligature?"
  (overlay-get ov 'aplig-lig?))

(defun aplig-ov--mask? (ov)
  "Is OV a mask?"
  (overlay-get ov 'aplig-mask?))

(defun aplig-ov--in? (ov start end)
  "Is OV contained within START and END?"
  (and ov start end
       (<= start (overlay-start ov) (overlay-end ov) end)))

(defun aplig-ov--in-match? (ov subexp)
  "Is OV contained in the SUBEXP matching group?"
  (aplig--ov-in-bound? ov (match-beginning subexp) (match-end subexp)))

;;;; Utils

(defun aplig-ovs--map (ovs prop fn)
  "Map FN over OVS PROP."
  (--map (overlay-get it prop) fn))



;;; Ligs
;;;; Boundary Functions

(defun aplig-lig--boundary-fn--lisps (lig)
  "Calculate line boundary cons for LIG's masks."
  (let ((lig-line (-> ov overlay-start line-number-at-pos)))
    (cons (1+ lig-line)
          (save-excursion
            (goto-line lig-line)
            (sp-end-of-sexp)
            (line-number-at-pos)))))

(defun aplig-lig--boundary?--lisps (lig)
  "Does LIG have an indentation boundary? A weaker version of boundary-fn."
  ;; 1. Is the lig a form opener?
  ;; 2. Is the lig already modifying indentation?
  ;; 3. Are there more lines?
  ;; Note that we should store the reason why it fails, as an optimization
  ;; we can utilize when modifications are performed on a line containing LIG
  t)

;;;; Unorganized

(defun aplig-lig--present? (start end)
  "Is a lig present within START and END? Return it."
  (and start end
       (-any #'aplig-ov--lig?
             (overlays-in start end))))

(defun aplig-lig--delete (lig)
  "Delete LIG."
  (delq lig aplig-lig-list)
  (delete-overlay lig))

(defun aplig-ligs->width (ligs)
  "Sum widths of LIGS."
  (aplig-ovs--map ligs 'aplig-width #'sum))

(defun aplig-lig--decompose-hook (lig post-modification? start end &optional _)
  "Decompose LIG upon modification as a modification-hook."
  (when post-modification?
    (aplig-lig-mask--remove-lig-from-masks lig)
    (aplig-lig--delete lig)))

(defun aplig-lig--init-lig-ov (ov replacement width)
  "Put lig text properties into OV."
  (-doto ov
    (overlay-put 'apl?      t)
    (overlay-put 'aplig-lig?  t)
    (overlay-put 'aplig-width width)

    (overlay-put 'display replacement)
    (overlay-put 'modification-hooks '(aplig-lig--decompose-hook))))

(defun aplig-lig--init-lig (replacement width)
  "Build ligature overlay for current `match-data'."
  (let* ((start (match-beginning 1))
         (end   (match-end 1))
         (lig   (aplig-lig--init-lig-ov (make-overlay start end) replacement width)))
    (push lig aplig-lig-list)
    (aplig-lig-mask--add-lig-to-masks lig)))



;;; Masks

(defun aplig-mask--at (line)
  "Retrieve mask at LINE."
  (nth line aplig-mask-list))

(defun aplig-masks--at (lines)
  "Retrieve masks at LINES."
  (-select-by-indices lines aplig-masks))

(defun aplig-masks--in (start-line end-line)
  "Retrieve masks within START-LINE and END-LINE."
  (-slice aplig-mask-list start-line end-line))

(defun aplig-mask--insert-at (mask line)
  "Insert MASK at LINE."
  (-insert-at line ov aplig-masks))

(defun aplig-mask--line-count-modified? ()
  "Positive if lines have been added, negative if removed, otherwise zero."
  (- (line-number-at-pos (point-max))
     (length aplig-masks)))

(defun aplig-mask--indent-col (&optional n)
  "Get indentation col, of line forward N-1 times if given."
  (save-excursion (end-of-line n) (back-to-indentation) (current-column)))

(defun aplig-mask--delete (mask)
  "Delete MASK."
  (delq lig aplig-lig-list)
  (delete-overlay mask))

(defun aplig-mask--decompose-hook (mask post-mod? start end &optional _)
  "Overlay modification hook to delete indent ov upon modification within it."
  (when post-mod?
    (let* ((inhibit-modification-hooks t)
           (width                      (aplig-mask->width mask))
           (invis-spaces-to-delete     (1+ width)))
      (aplig-mask--delete mask)
      (evil-with-single-undo
        (delete-char (- invis-spaces-to-delete))))))

(defun aplig-mask--set-prefix (mask)
  "Format and set the `line-prefix' overlay text property for MASK."
  (let* ((sep         "|")
         (true-indent (aplig-mask--indent-col))
         (width       (aplig-mask->width))
         (num-parents (length (overlay-get mask 'aplig-ligs)))
         (sections    (list (-> "%02d" (format true-indent))
                            (-> "%02d" (format width))
                            (-> "#%d:" (format num-parents)))))
    (->> sections (-interpose sep) (apply #'s-concat))))

(defun aplig-mask--init-mask-ov (ov)
  "Put mask text properties into OV."
  (-doto ov
    (overlay-put 'apl?      t)
    (overlay-put 'aplig-mask? t)
    (overlay-put 'aplig-ligs  nil)

    (overlay-put 'face               'underline)
    (overlay-put 'display            " ")
    (overlay-put 'line-prefix        (aplig--format-prefix 1 0))
    (overlay-put 'modification-hooks '(aplig-mask--decompose-hook))))

(defun aplig-mask--init-mask (&optional line)
  "Create empty mask for LINE, otherwise current line."
  (save-excursion
    (when line (goto-line line))

    (let* ((line  (line-number-at-pos))
           (start (line-beginning-position))
           (end   (1+ start))
           (mask  (aplig-mask--init-mask-ov (make-overlay start end))))
      (aplig-mask--insert-at mask line))))

(defun aplig-mask--init-masks ()
  "Line-by-line buildup `aplig-masks'."
  (save-excursion
    (goto-char (point-min))

    (while (not (eobp))
      (aplig-mask--init-mask)
      (forward-line))))

(defun aplig-mask->width (mask)
  "Calculate width of MASK's ligs."
  (-> mask (overlay-get 'aplig-ligs) aplig-ligs->width))

(defun aplig-mask--recenter (mask)
  "Recenter MASK, ie. reset its end position based on ligs widths."
  (let ((start (overlay-start mask))
        (width (aplig-mask->width mask)))
    (move-overlay mask start (+ start width))))

(defun aplig-mask--refresh (mask)
  "Reset bounds and boundary-dependent properties of MASK based on cur ligs."
  (-doto mask
    (aplig-mask--recenter)
    (aplig-mask--set-prefix)))

(defun aplig-mask--refresh-maybe (mask)
  "Perform `aplig-mask--refresh' when we should."
  (unless aplig-mask--wait-for-refresh
    (aplig-mask--refresh mask)))

(defun aplig-masks--refresh (masks)
  "Refresh MASKS."
  (-each masks #'aplig-mask--refresh-maybe))



;;; Mask-Lig Interface

(defun aplig-lig-mask--masks-for (lig)
  "Return all masks LIG contributes to."
  (->> lig (funcall #'aplig-lig--boundary-fn) (apply #'aplig-masks--in)))

(defun aplig-lig-mask--add-lig-to-mask (lig mask)
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



;;; Font Locks

(defun aplig-kwd--match (replacement width)
  "The form for FACENAME in font-lock-keyword's MATCH-HIGHLIGHT."
  (unless (aplig-lig--present? (match-beginning 1) (match-end 1))
    (aplig-lig--init replacement width)))

(defun aplig-kwd--build (spec)
  "Compose the font-lock-keyword for SPEC in `aplig-specs'."
  (-let (((&plist :name name
                  :replacement replacement
                  :rx rx
                  :width width)
          spec))
    `(,rx (0 (prog1 nil (aplig-kwd--match ,replacement ,width))))))

(defun aplig-kwds--build ()
  "Build kwds from `aplig-specs' and add to `font-lock-keywords'."
  (let ((kwds (-map #'aplig-kwd--build aplig-specs)))
    (font-lock-add-keywords nil kwds)))



;;; Interactive

(defun aplig-disable ()
  "Delete overlays managed by apl."
  (interactive)

  (-each aplig-mask-list #'aplig-mask--delete)
  (-each aplig-lig-list #'aplig-lig--delete)
  (setq font-lock-keywords nil)
  (remove-hook 'lisp-mode-hook #'aplig-add-kwds))

(defun aplig-enable ()
  "Enable apl and cleanup previous instance if running."
  (interactive)

  (aplig-disable)
  (aplig-init-masks)
  (add-hook 'lisp-mode-hook #'aplig-add-kwds nil 'local)
  (lisp-mode))



;;; Scratch

(when nil
  (spacemacs/declare-prefix "d" "dev")
  (spacemacs/set-leader-keys "de" #'aplig-enable)
  (spacemacs/set-leader-keys "dd" #'aplig-disable))



;;; Footer

(provide 'aplig)



;;; aplig.el ends here
