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
  (aplig-validate-spec name string replacement)
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

(defun aplig-validate-spec (name string replacement)
  "Throw error on egregious inputs."
  (cond
   ((or (s-contains? "\n" string)
        (s-contains? "\n" replacement))
    (error "Newlines anywhere in spec are confusing, will not be supported."))

   ((> (length replacement)
       (length string))
    (error "Indentation expansions are not supported yet."))))

;;;; Configured

(defconst aplig-specs
  (aplig-make-specs '(("Hello Lig"   "hello"     "")
                      ;; ("0-space Lig" "0-space"   "")
                      ;; ("1-space Lig" "1-space"   " ")
                      ;; ("2-space Lig" "2-space"   "  ")
                      ;; ("tab Lig"     "tab-space" "	")
                      ))
  "Collection of specs from `aplig-make-spec'.")

(defconst aplig-display-prefixes? t
  "Whether to add the `line-prefix' property to indentation overlays.")

(defconst aplig-lig--boundary-fn #'aplig-lig--boundary--lisps
  "A function that should return line boundaries given a LIG.")

(defconst aplig-lig--boundary?-fn #'aplig-lig--boundary?--lisps
  "A subset of `aplig-lig--boundary-fn', whether LIG has a boundary.")

;; Eventually support indentation heuristic optimizations
(defconst aplig-lig--boundary-fn--heuristic nil)
(defconst aplig-lig--boundary?-fn--heuristic nil)

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

(defun aplig-ovs--prop (ovs prop)
  "Return list of each OVS PROP."
  (--map (overlay-get it prop) ovs))

(defun aplig--ov-at (pos)
  "Return first aplig overlay at POS."
  (let ((ovs (overlays-at pos)))
    (--any (when (overlay-get it 'aplig?) it) ovs)))

(defun aplig-ov--print (ov)
  "Dispatch pprint on OV."
  (cond ((null ov)
         (print "No ov found."))
        ((aplig-ov--lig? ov)
         (aplig-lig--print ov))
        ((aplig-ov--mask? ov)
         (aplig-mask--print ov))))



;;; Ligs
;;;; Boundary Functions

(defun aplig-lig--boundary--lisps (lig)
  "Calculate line boundary for LIG's masks."
  (let ((lig-line (-> lig overlay-start line-number-at-pos)))
    (list (min (line-number-at-pos (point-max))
               (1+ lig-line))
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

;;;; Overlays

(defun aplig-lig--present? (&optional start end)
  "Is a lig present within START and END, defaulting to `match-data'? Get it."
  (let ((start (or start (match-beginning 1)))
        (end   (or end (match-end 1))))
    (and start end
         (-any #'aplig-ov--lig?
               (overlays-in start end)))))

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

  (message "INIT")

  (let* ((start (or start (match-beginning 1)))
         (end   (or end (match-end 1)))
         (ov    (make-overlay start end))
         (lig   (aplig-lig--init-ov ov replacement width)))
    (push lig aplig-lig-list)
    (aplig-lig-mask--add-lig-to-masks lig)
    lig))



;;; Masks
;;;; Lines

(defun aplig-mask--line-count-modified? ()
  "Positive if lines have been added, negative if removed, otherwise zero."
  (- (line-number-at-pos (point-max))
     (length aplig-mask-list)))

(defun aplig-mask--indent-col (&optional n)
  "Get indentation col, of line forward N-1 times if given."
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
  "Insert MASK at LINE."
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
         (true-indent (aplig-mask--indent-col))
         (width       (aplig-mask->width mask))
         (num-ligs    (length (overlay-get mask 'aplig-ligs)))
         (sections    (list (-> "%02d" (format true-indent))
                            (-> "%02d" (format width))
                            (-> "#%d:" (format num-ligs)))))
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
         (end   (+ start width)))
    (move-overlay mask start end)))

(defun aplig-mask--render? (mask)
  "Should MASK be rendered?"
  (> (aplig-mask->width mask) 0))

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
      (aplig-mask--insert-at mask line))))

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
  (when (funcall (symbol-value #'aplig-lig--boundary?-fn) lig)
    (->> lig
       (funcall (symbol-value #'aplig-lig--boundary-fn))
       (apply #'aplig-masks--in))))

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

(defun aplig-lig-mask--add-ligs-to-masks (ligs)
  "Batch add LIGS to their masks refreshing upon completion."
  (let ((aplig-mask--wait-for-refresh t))
    (-each ligs #'aplig-lig-mask--add-lig-to-masks))

  ;; NOTE Easier atm to just refresh the buffer than the correct intervals
  (aplig-masks--refresh-buffer))



;;; Font Locks

(defun aplig-kwd--match (replacement width)
  "The form for FACENAME in font-lock-keyword's MATCH-HIGHLIGHT."
  (unless (aplig-lig--present?)
    (aplig-lig--init replacement width)))

(defun aplig-kwd--build (spec)
  "Compose the font-lock-keyword for SPEC in `aplig-specs'."
  (-let (((&plist :name name
                  :replacement replacement
                  :rx rx
                  :width width)
          spec))
    `(,rx (0 (prog1 nil (aplig-kwd--match ,replacement ,width))))))

(defun aplig-kwds--add ()
  "Build kwds from `aplig-specs' and add to `font-lock-keywords'."
  (let ((kwds (-map #'aplig-kwd--build aplig-specs)))
    (font-lock-add-keywords nil kwds)))



;;; Interactive
;;;; Utils

(defun aplig-print-at-point ()
  "Pprint aplig OV at point."
  (interactive)

  (-> (point) aplig--ov-at aplig-ov--print))

;;;; Setup

(defun aplig-setup--agnostic ()
  "Setup all *major-mode-agnostic* components."
  (aplig-masks--init)
  (aplig-masks--refresh aplig-mask-list))

;;;; Toggling

(defun aplig-disable ()
  "Delete overlays managed by apl."
  (interactive)

  (-each aplig-mask-list #'aplig-mask--delete)
  (-each aplig-lig-list #'aplig-lig--delete)
  (setq font-lock-keywords nil)
  (remove-hook 'lisp-mode-hook #'aplig-kwds--add))

(defun aplig-enable ()
  "Enable apl and cleanup previous instance if running."
  (interactive)

  (aplig-disable)
  (aplig-setup--agnostic)
  (add-hook 'lisp-mode-hook #'aplig-kwds--add)
  ;; (add-hook 'lisp-mode-hook #'aplig-kwds--add nil 'local)

  (let ((aplig-mask--wait-for-refresh t))
    (lisp-mode)
    (font-lock-ensure))
  (aplig-masks--refresh-buffer))



;;; Dev

(defun aplig-lig--print (lig)
  "Pprint a ligature."
  (let* ((start (overlay-start lig))
         (end (overlay-end lig))
         (line (line-number-at-pos start))
         (string (buffer-substring-no-properties start end))
         (replacement (overlay-get lig 'display))
         (masks (aplig-lig-mask--masks-for lig)))
    (message "Lig overlay:
start: %s
end: %s
line: %s
string: %s
replacement: %s
masks: %s
"
             start end line string replacement masks)))

(defun aplig-mask--print (mask)
  "Pprint a mask."
  (let* ((start (overlay-start lig))
         (end (overlay-end lig))
         (line (line-number-at-pos start))
         (ligs (overlay-get mask 'aplig-ligs)))
    (message "Mask overlay:
start: %s
end: %s
line: %s
ligs: %s
"
             start end line ligs)))

(when nil
  (spacemacs/declare-prefix "d" "dev")
  (spacemacs/set-leader-keys "de" #'aplig-enable)
  (spacemacs/set-leader-keys "dd" #'aplig-disable)
  (spacemacs/set-leader-keys "dp" #'aplig-print-at-point))



;;; Footer

(provide 'aplig)



;;; aplig.el ends here
