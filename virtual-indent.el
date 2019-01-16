;;; virtual-indent.el --- Personal Indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Trying to resolve key issues with ligature adoption, namely indentation.
;; A general implementation has interesting applications elsewhere, like
;; opening a 2-indent Python file and seeing it as if it was 4-indent, and
;; making the `:nameless' emacs-lisp library more valuable.

;;; Code:
;;;; Requires

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)

;;; Configuration

(defun virtual-indent-make-spec (name string replacement &optional rx)
  "Create spec plist NAME for STRING to REPLACEMENT optionally with custom RX."
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

(defun virtual-indent--make-ov (subexp)
  "Create overlay with start and end taking `match-data' at SUBEXP."
  (make-overlay (match-beginning subexp)
                (match-end       subexp)))

(defun virtual-indent--ov-in (subexp)
  (-when-let* ((overlays (overlays-in (match-beginning subexp)
                                      (match-end       subexp)))
               (-contains? overlays)
               )))

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
                (overlays-at)
                )

              (-doto (virtual-indent--make-ov 1)
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
