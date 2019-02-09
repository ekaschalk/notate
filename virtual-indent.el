;;; virtual-indent.el --- Personal Indentation -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/ekaschalk/virtual-indent
;; Version: 0.1
;; Keywords: indentation, display, ligatures, major-modes
;; Package-Requires: ((cl "1.0") (dash "2.14.1") (dash-functional "1.2.0") (s "1.12.0") (emacs "26.1"))

;; virtual-indent is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; virtual-indent is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with virtual-indent.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.



;;; Commentary:

;; Exploring concept of "personalized indentation"

;; Several use-cases:
;; 1. Rendering and editing a 2-indent python file as if it was a 4-indent,
;;    and the reverse.
;; 2. Multi-character ligature replacements, like lambda -> lambda-symbol will
;;    not modify indentation in programming modes.
;; 3. The `nameless-mode' library will not require a choice of which
;;    indentation to keep correct, the true or your view of the file.



;;; Code:
;;;; Requires

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)



;;; Configuration
;;;; Utils

(defun virtual-indent-make-spec (name string replacement &optional rx)
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

(defun virtual-indent-make-specs (specs)
  "Apply `virtual-indent-make-spec' to each SPEC."
  (-map (-applify #'virtual-indent-make-spec) specs))

;;;; Configured

(defconst virtual-indent-specs
  (virtual-indent-make-specs '(("Hello Lig"   "hello"     "")
                               ("0-space Lig" "0-space"   "")
                               ("1-space Lig" "1-space"   " ")
                               ("2-space Lig" "2-space"   "  ")
                               ("tab Lig"     "tab-space" "	")))
  "Collection of specs from `virtual-indent-make-spec'.")

(defconst virtual-indent-display-prefixes? t
  "Whether to add the `line-prefix' property to indentation overlays.")

;;;; Constants

(defconst virtual-indent--lig-subexp 1
  "Alias the SUBEXP for ligatures in `match-data'.")

;;;; Managed

(defconst virtual-indent-ovs nil
  "List of ligature overlays currently managed.")

(defconst virtual-indent-masks nil
  "List of indent overlays currently managed.")



;;; Overlays
;;;; Utils
;;;;; General

(defun virtual-indent--make-ov-for-match (subexp)
  "Specialized `make-overlay' with start and end taking `match-data' at SUBEXP."
  (make-overlay (match-beginning subexp) (match-end subexp)))

(defun virtual-indent--ovs-in-match (subexp)
  "Specialized `overlays-in' with start and end taking `match-data' at SUBEXP."
  (overlays-in (match-beginning subexp) (match-end subexp)))

(defun virtual-indent--ov-in-bound? (ov start end)
  "Is overlay OV contained within START and END?"
  (and ov start end
       (<= start (overlay-start ov) (overlay-end ov) end)))

(defun virtual-indent--ov-in-match? (ov subexp)
  "Is overlay OV contained in the SUBEXP'th matching group?"
  (virtual-indent--ov-in-bound? ov (match-beginning subexp) (match-end subexp)))

;;;;; Specialized

(defun virtual-indent--lig-in-match? (ov)
  "Specialize `virtual-indent--ov-in?' for ligatures."
  (and (overlay-get ov 'virtual-indent-lig?)
       (virtual-indent--ov-in-match? ov virtual-indent--lig-subexp)))

(defun virtual-indent--any-lig-in-match? ()
  "Is any ligature overlay in the current match?"
  (-any #'virtual-indent--lig-in-match?
        (virtual-indent--ovs-in-match virtual-indent--lig-subexp)))

;;;; Deletion

(defun virtual-indent--delete-lig (ov)
  "Delete a ligature overlay."
  (setq virtual-indent-ovs (delq ov virtual-indent-ovs))
  (delete-overlay ov))

(defun virtual-indent--delete-mask (ov)
  "Delete an indent mask."
  (setq virtual-indent-masks (delq ov virtual-indent-masks))
  (delete-overlay ov))

(defun virtual-indent-delete-ligs ()
  "Delete ligature overlays."
  (-each virtual-indent-ovs #'virtual-indent--delete-lig))

(defun virtual-indent-delete-masks ()
  "Delete indent overlays."
  (-each virtual-indent-masks #'virtual-indent--delete-mask))

(defun virtual-indent-delete-ovs ()
  "Delete overlays managed by virtual-indent."
  (interactive)

  (virtual-indent-delete-ligs)
  (virtual-indent-delete-masks))

;;;; Components

(defun virtual-indent-remove-lig-as-parent (lig)
  "Remove ov LIG as a parent in each mask in its range."
  ;; NOTE Depending on how outlines are implemented,
  ;; deleting the lig OV and refreshing might be sufficient rather
  ;; than explicitly delqing the mask.
  (let ((masks (virtual-indent-masks-with lig)))
    (-each masks
      (lambda (mask)
        (->>
         (overlay-get mask 'virtual-indent-parents)
         (delq mask)
         (overlay-put mask 'virtual-indent-parents))
        (virtual-indent-refresh-mask mask)))))

(defun virtual-indent--lig-decompose-hook (lig post-mod? start end &optional _)
  "Overlay modification hook to delete lig ov upon modification within the ov."
  (when post-mod?
    (virtual-indent-remove-lig-as-parent)
    (virtual-indent--delete-lig lig)))

(defun virtual-indent--mask-decompose-hook (mask post-mod? start end &optional _)
  "Overlay modification hook to delete indent ov upon modification within it."
  (when post-mod?
    (let* ((inhibit-modification-hooks t)
           (width                      (overlay-get mask 'width))
           (invis-spaces-to-delete     (1+ width)))
      (virtual-indent--delete-mask mask)
      (delete-char (- invis-spaces-to-delete)))))

(defun virtual-indent--format-prefix (width num-parents)
  "Format the `line-prefix' overlay text property."
  (let ((sep "|")
        (true-indent (virtual-indent-indent-col))
        (sections (list (-> "%02d"  (format true-indent))
                        (-> "%02d" (format width))
                        (-> "#%d:"  (format num-parents)))))
    (->> sections (-interpose sep) (apply #'s-concat))))

;;;; Builders

(defun virtual-indent-build-lig (replacement width)
  "Build ligature overlay for current `match-data'."
  (let ((ov (virtual-indent--make-ov-for-match virtual-indent--lig-subexp)))
    (-doto ov
      (overlay-put 'virtual-indent?      t)
      (overlay-put 'virtual-indent-lig?  t)
      (overlay-put 'virtual-indent-width width)

      (overlay-put 'display replacement)
      (overlay-put 'modification-hooks '(virtual-indent--lig-decompose-hook))

      (push virtual-indent-ovs))

    (virtual-indent-add-parent-maybe ov)))



;;; Masks
;;;; Utils

(defun virtual-indent-indent-col (&optional n)
  "Get indentation col, of line forward N-1 times if given."
  (save-excursion (end-of-line n) (back-to-indentation) (current-column)))

(defun virtual-indent-line-begin-pos (line)
  "Return point at start of LINE."
  (save-excursion (goto-line line) (line-beginning-position)))

(defun virtual-indent-line-end-pos (line)
  "Return point at end of LINE."
  (save-excursion (goto-line line) (line-end-position)))

(defun virtual-indent-line-count-modified? ()
  "Positive if lines have been added, negative if removed, otherwise zero."
  (- (line-number-at-pos (point-max))
     (length virtual-indent-masks)))

;;;; Aliases

(defun virtual-indent-mask-at (line)
  "Indent mask at line."
  (nth line virtual-indent-masks))

(defun virtual-indent-masks-at (lines)
  "Indent masks at lines."
  (-select-by-indices lines virtual-indent-masks))

(defun virtual-indent-masks-in (start-line end-line)
  "Indent masks in slice."
  (-slice virtual-indent-masks start-line end-line))

(defun virtual-indent-masks-with (lig)
  "Retrieve masks that (should) have LIG as a parent."
  (-let (((start-line . end-line)
          (virtual-indent-lig-indent-interval lig)))
    (virtual-indent-masks-in start-line end-line)))

;;;; Construction

(defun virtual-indent--init-mask ()
  "Create initial mask, without parents, for current line."
  (let* ((line  (line-number-at-pos))
         (start (line-beginning-position))
         (ov    (make-overlay start (1+ start))))
    (-doto ov
      (overlay-put 'virtual-indent?        t)
      (overlay-put 'virtual-indent-mask?   t)
      (overlay-put 'virtual-indent-parents nil)

      (overlay-put 'face 'underline)
      (overlay-put 'display " ")
      (overlay-put 'line-prefix (virtual-indent--format-prefix 1 0))
      (overlay-put 'modification-hooks '(virtual-indent--mask-decompose-hook)))

    (-insert-at line ov virtual-indent-masks)))

(defun virtual-indent-init-masks ()
  "Line-by-line buildup empty `virtual-indent-masks'."
  (save-excursion
    (goto-char (point-min))
    (while (not eobp)
      (virtual-indent--init-mask)
      (forward-line))))

;;;; Management

(defun virtual-indent-refresh-mask (mask)
  "Recalculate and set bounds and properties of MASK."
  (let* ((parents     (overlay-get mask 'virtual-indent-parents))
         (width       (virtual-indent-parents-width parents))
         (line-prefix (virtual-indent--format-prefix width (length parents))))
    (overlay-put mask 'line-prefix line-prefix)
    (move-overlay mask (overlay-start mask) width)))



;;; Ligs
;;;; Objects

(defun virtual-indent-lig-modifies-indent? (lig)
  "Does the ov LIG modify following lines' indentations?" ; TODO
  ;; 1. Is the lig a form opener?
  ;; 2. Is the lig already modifying indentation?
  ;; 3. Are there more lines?
  t)

(defun virtual-indent-lig-indent-interval (lig)
  "Find the limiting lines cons for ov LIG's affect on indentation."
  (save-excursion
    (goto-char (overlay-start lig))
    (cons (1+ (line-number-at-pos))
          (progn (sp-end-of-sexp) (line-number-at-pos)))))

(defun virtual-indent-add-parent (lig)
  "Add LIG ov to the appropriate masks and refresh them."
  (let ((masks (virtual-indent-masks-with lig)))
    (-each masks
      (lambda (mask)
        (->>
         (overlay-get mask 'virtual-indent-parents)
         (cons lig)
         (overlay-put mask 'virtual-indent-parents))
        (virtual-indent-refresh-mask mask)))))

(defun virtual-indent-add-parent-maybe (lig)
  "Determine whether LIG effects indentation before adding it as a parent."
  (when (virtual-indent-lig-modifies-indent? lig)
    (virtual-indent-add-parent)))

;;;; Collections

(defun virtual-indent-parents-width (parents)
  "Sum PARENTS's lig ovs widths."
  (->> parents (--map (overlay-get it 'virtual-indent-width)) -sum))



;;; Font-Locks

(defun virtual-indent-match (replacement width)
  "The form for FACENAME in font-lock-keyword's MATCH-HIGHLIGHT.

Identify a ligature has been found and dispatch ov builders as required."
  (unless (virtual-indent--any-lig-in-match?)
    (virtual-indent-build-lig replacement width)))

(defun virtual-indent--build-kwd (spec)
  "Compose the font-lock-keyword for SPEC in `virtual-indent-specs'."
  (-let (((&plist :name name
                  :replacement replacement
                  :rx rx
                  :width width)
          spec))
    `(,rx (0 (prog1 virtual-indent-lig-face
               (virtual-indent-match ,replacement ,width))))))

(defun virtual-indent-add-kwds ()
  "Translate spec into keywords and add to `font-lock-keywords'."
  (->> virtual-indent-specs
     (-map #'virtual-indent--build-kwd)
     (font-lock-add-keywords nil)))



;;; Setup

(defun virtual-indent-setup ()
  "Handle virtual-indent instantiation to take place before font-locks turn on."
  (virtual-indent-init-masks))



;;; Interactive

(defun virtual-indent-disable ()
  "Disable and cleanup virtual-indent."
  (interactive)

  (setq font-lock-keywords nil)
  (remove-hook 'lisp-mode-hook #'virtual-indent-add-kwds)
  (virtual-indent-delete-ovs))

(defun virtual-indent-enable ()
  "Enable virtual-indent and cleanup previous instance if running."
  (interactive)

  (virtual-indent-disable)
  (virtual-indent-setup)
  (add-hook 'lisp-mode-hook #'virtual-indent-add-kwds nil 'local)
  (lisp-mode)

  ;; Change Functions to use later...
  ;; before-change-functions
  ;; after-change-functions
  )



;;; Development Stuff

(when nil
  (spacemacs/declare-prefix "d" "dev")
  (spacemacs/set-leader-keys "de" #'virtual-indent-enable)
  (spacemacs/set-leader-keys "dd" #'virtual-indent-disable))

(defconst virtual-indent-lig-face font-lock-function-name-face
  "Make it easier to tell when a ligature is found.")



(provide 'virtual-indent)

;;; virtual-indent.el ends here
