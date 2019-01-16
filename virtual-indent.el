;;; virtual-indent.el --- Personal Indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Exploring generalized concept of "personalized indentation": virtual-indent.

;; Several use-cases:
;; 1. Rendering and editing a 2-indent python file as if it was a 4-indent,
;;    and the reverse.
;; 2. Multi-character ligature replacements, like lambda -> lambda-symbol will
;;    not modify indentation in programming modes.
;; 3. The `nameless-mode' library will not require a choice of which
;;    indentation to keep correct, the true or your view of the file.

;; Exciting stuff if an implementation can be successful.

;;; Code:
;;;; Requires

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)

;;; Spec

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

(defconst virtual-indent-spec
  (list (virtual-indent-make-spec "Test Ligature" "hello" "")
        ;; more ligatures...
        )
  "Collection of `virtual-indent-make-spec' specifying ligature replacements.")

;;; Overlays
;;;; Basics

(defun virtual-indent--make-ov (subexp)
  "`make-overlay' with start and end taking `match-data' at SUBEXP."
  (make-overlay (match-beginning subexp)
                (match-end       subexp)))

(defun virtual-indent--ovs-in (subexp)
  "`overlays-in' with start and end taking `match-data' at SUBEXP."
  (overlays-in (match-beginning subexp)
               (match-end       subexp)))

(defun virtual-indent--ov-in (ov subexp)
  "Is overlay OV contained in overlays for `match-data' at SUBEXP?"
  (-> subexp virtual-indent--ovs-in (-contains? ov)))


(defun virtual-indent--ov-mod-hook (ov post-modification? start end &optional _)
  "Overlay modification hook to force evaporation upon modification within ov."
  (when post-modification?
    (-doto ov
      (overlay-put 'display nil)
      (overlay-put 'modification-hooks nil))))

;;; Font-Locks

(defun virtual-indent--build-kwd (rgx string)
  `(,rgx (0 (prog1 nil
              ;; (print "found")

              (unless (-contains?)
                (overlays-at))

              (setq ov (virtual-indent--make-ov 1))
              (-doto
                  (overlay-put 'display string)
                (overlay-put 'evaporate t)
                (overlay-put 'modification-hooks '(lig-mod-hook)))
              ;; run in the indent recalculations
              ))))

(defun virtual-indent-add-kwds (rgx-string-alist)
  (->> rgx-string-alist
     (-map (-applify #'virtual-indent--build-kwd))
     (font-lock-add-keywords nil)))

(defun virtual-indent-hook-fn ()
  (virtual-indent-add-kwds virtual-indent-rgx-string-alist))

(defun virtual-indent-disable ()
  (interactive)
  (remove-hook 'lisp-mode-hook #'virtual-indent-hook-fn))

(defun virtual-indent-enable ()
  (interactive)
  (virtual-indent-disable)
  (setq font-lock-keywords nil)
  (add-hook    'lisp-mode-hook #'virtual-indent-hook-fn)
  (lisp-mode))

;;; Indentation
