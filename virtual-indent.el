;;; virtual-indent.el --- Personal Indentation -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>
;;
;; Authors: Eric Kaschalk <ekaschalk@gmail.com>
;; URL: http://github.com/ekaschalk/virtual-indent
;; Version: 1.0
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
;;;; Utilities

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

;;;; Setup

(defconst virtual-indent-specs
  (virtual-indent-make-specs '(("Hello Lig"   "hello"     "")
                               ("0-space Lig" "0-space"   "")
                               ("1-space Lig" "1-space"   " ")
                               ("2-space Lig" "2-space"   "  ")
                               ("tab Lig"     "tab-space" "	")))
  "Collection of specs from `virtual-indent-make-spec'.")

;;;; Managed
;;;;; Constants

(defconst virtual-indent--lig-subexp 1
  "Alias the SUBEXP for ligatures in `match-data'.")

;;;;; Variable

(defconst virtual-indent-lig-ovs nil
  "List of ligature overlays currently managed.")

(defconst virtual-indent-indent-ovs nil
  "List of indent overlays currently managed.")


;;; Overlays
;;;; Fundamentals
;;;;; General

(defun virtual-indent--make-ov (subexp)
  "`make-overlay' with start and end taking `match-data' at SUBEXP."
  (make-overlay (match-beginning subexp)
                (match-end subexp)))

(defun virtual-indent--ovs-in (subexp)
  "`overlays-in' with start and end taking `match-data' at SUBEXP."
  (overlays-in (match-beginning subexp)
               (match-end subexp)))

(defun virtual-indent--ov-in? (ov subexp)
  "Is overlay OV contained in overlays for `match-data' at SUBEXP?"
  (-contains? (virtual-indent--ovs-in subexp) ov))

;;;;; Specialized

(defun virtual-indent--ov-lig? (ov)
  "Is OV a ligature overlay?"
  (-contains? virtual-indent-lig-ovs ov))

(defun virtual-indent--lig-ov-in? (ov)
  "Specialize `virtual-indent--ov-in?' for ligatures."
  (and (virtual-indent--ov-lig? ov)
       (virtual-indent--ov-in? ov virtual-indent--lig-subexp)))

(defun virtual-indent--lig-ov-present? ()
  "Is a ligature overlay present for current match?"
  (-any #'virtual-indent--lig-ov-in?
        (virtual-indent--ovs-in virtual-indent--lig-subexp)))

(defun virtual-indent--make-lig-ov ()
  "Make a ligature overlay."
  (virtual-indent--make-ov virtual-indent--lig-subexp))

(defun virtual-indent--trim-lig-ovs ()
  "Remove deleted lig overlays from `virtual-indent-lig-ovs'"
  (setq virtual-indent-lig-ovs
        (-filter #'overlay-buffer virtual-indent-lig-ovs)))

(defun virtual-indent--delete-lig-ov (ov)
  "Delete a ligature overlay."
  (delete-overlay ov)
  (virtual-indent--trim-lig-ovs))

;; indenters
(defun virtual-indent--make-indent-ov (start end)
  "Make an indent overlay."
  (make-overlay start end))

(defun virtual-indent--trim-indent-ovs ()
  (setq virtual-indent-indent-ovs
        (-filter #'overlay-buffer virtual-indent-indent-ovs)))

(defun virtual-indent--delete-indent-ov (ov)
  "Delete a ligature overlay."
  (delete-overlay ov)
  (virtual-indent--trim-indent-ovs))

;;;; Management
;;;;; Cleanup

(defun virtual-indent-cleanup-lig-ovs ()
  "Delete and cleanup all `virtual-indent-lig-ovs'."
  (-doto virtual-indent-lig-ovs
    (-each #'delete-overlay)
    (setq nil)))

(defun virtual-indent-cleanup-indent-ovs ()
  "Delete and cleanup all `virtual-indent-indent-ovs'."
  (-doto virtual-indent-indent-ovs
    (-each #'delete-overlay)
    (setq nil)))

(defun virtual-indent-cleanup-ovs ()
  "Delete and cleanup all overlays managed by virtual-indent."
  (virtual-indent-cleanup-lig-ovs)
  (virtual-indent-cleanup-indent-ovs))

;;;; Functions

(defun virtual-indent--ov-mod-hook (ov post-modification? start end &optional _)
  "Overlay modification hook to force evaporation upon modification within ov."
  (when post-modification?
    (virtual-indent--delete-lig-ov ov)))

(defun virtual-indent-build-lig-ov (replacement)
  "Build ligature overlay for current `match-data'."
  (let ((ov (virtual-indent--make-lig-ov)))
    (-doto ov
      (overlay-put 'display replacement)
      (overlay-put 'modification-hooks '(virtual-indent--ov-mod-hook)))
    (add-to-list 'virtual-indent-lig-ovs ov)))


;;; Indent

(defun virtual-indent-build-indent-ov (width)
  (let* ((start (line-beginning-position 2))
         (end (+ start width))
         (ov (virtual-indent--make-indent-ov start end)))
    (-doto ov
      ;; (overlay-put 'line-prefix "---")
      ;; (overlay-put 'display `(space . (:align-to 2)))

      (overlay-put 'display (number-to-string width))
      (overlay-put 'face underline)
      ))
  (add-to-list 'virtual-indent-indent-ovs ov))


;;; Font-Locks

(defun virtual-indent-match (replacement width)
  "The form for FACENAME in font-lock-keyword's MATCH-HIGHLIGHT."
  (unless (virtual-indent--lig-ov-present?)
    (virtual-indent-build-lig-ov replacement)
    (virtual-indent-build-indent-ov width)))

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

(defun virtual-indent-hook-fn ()
  "Add as hook to enable virtual-indent.

Currently:
1. Just an alias on `virtual-indent-add-kwds'.
2. Works for just `lisp-mode'."
  (virtual-indent-add-kwds))

;;; Interactive

(defun virtual-indent-disable ()
  "Disable and cleanup virtual-indent."
  (interactive)

  (setq font-lock-keywords nil)
  (remove-hook 'lisp-mode-hook #'virtual-indent-hook-fn)
  (virtual-indent-cleanup-ovs))

(defun virtual-indent-enable ()
  "Enable virtual-indent and cleanup previous instance if running."
  (interactive)

  (virtual-indent-disable)
  (add-hook 'lisp-mode-hook #'virtual-indent-hook-fn)
  (lisp-mode))


;;; Development Stuff

(when nil
  (spacemacs/declare-prefix "d" "dev")
  (spacemacs/set-leader-keys "de" #'virtual-indent-enable)
  (spacemacs/set-leader-keys "dd" #'virtual-indent-disable))

(defconst virtual-indent-lig-face font-lock-function-name-face
  "Make it easier to tell when a ligature is found.")



(provide 'virtual-indent)

;;; virtual-indent.el ends here
