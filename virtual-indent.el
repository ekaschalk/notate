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

;;;; Constants

(defconst virtual-indent--lig-subexp 1
  "Alias the SUBEXP for ligatures in `match-data'.")

;;;; Managed

(defconst virtual-indent-lig-ovs nil
  "List of ligature overlays currently managed.")

(defconst virtual-indent-indent-ovs nil
  "List of indent overlays currently managed.")

;;;; Setup

(defconst virtual-indent-spec
  (list (virtual-indent-make-spec "Test Ligature" "hello" "")
        ;; more ligatures...
        )
  "Collection of `virtual-indent-make-spec' specifying ligature replacements.")

;;; Overlays
;;;; Fundamentals

(defun virtual-indent--make-ov (subexp)
  "`make-overlay' with start and end taking `match-data' at SUBEXP."
  (apply #'make-overlay
         (match-data subexp)))

(defun virtual-indent--ovs-in (subexp)
  "`overlays-in' with start and end taking `match-data' at SUBEXP."
  (apply #'overlays-in
         (match-data subexp)))

(defun virtual-indent--ov-in? (ov subexp)
  "Is overlay OV contained in overlays for `match-data' at SUBEXP?"
  (-> subexp
     virtual-indent--ovs-in
     (-contains? ov)))

(defun virtual-indent--lig-ov-in? (ov)
  "Specialize `virtual-indent--ov-in?' for ligatures."
  (virtual-indent--ov-in? ov virtual-indent--lig-subexp))

(defun virtual-indent--lig-ov-present? ()
  (-any #'virtual-indent--lig-ov-in?
        (virtual-indent--ovs-in virtual-indent--lig-subexp)))

;;;; Management

(defun virtual-indent-cleanup-lig-ovs ()
  "Delete all `virtual-indent-lig-ovs'."
  (-doto virtual-indent-lig-ovs
    (-each #'delete-overlay)
    (setq nil)))

(defun virtual-indent-cleanup-indent-ovs ()
  "Delete all `virtual-indent-indent-ovs'."
  (-doto virtual-indent-indent-ovs
    (-each #'delete-overlay)
    (setq nil)))

(defun virtual-indent-cleanup-ovs ()
  "Delete all overlays managed by virtual-indent."
  (virtual-indent-cleanup-lig-ovs)
  (virtual-indent-cleanup-indent-ovs))

;;;; Other

(defun virtual-indent--ov-mod-hook (ov post-modification? start end &optional _)
  "Overlay modification hook to force evaporation upon modification within ov."
  (when post-modification?
    (-doto ov
      (overlay-put 'display nil)
      (overlay-put 'modification-hooks nil))))

;;; Font-Locks

(defun virtual-indent-build-lig-ov ()
  "Build ligature overlay for current `match-data'."
  (-doto (virtual-indent--make-ov virtual-indent--lig-subexp)
    (overlay-put 'display string)
    (overlay-put 'evaporate t)
    (overlay-put 'modification-hooks '(lig-mod-hook))
    (add-to-list 'virtual-indent-lig-ovs)))

(defun virtual-indent--build-kwd (rgx string)
  `(,rgx (0 (prog1 nil
              (unless virtual-indent--lig-ov-present?
                (virtual-indent-build-lig-ov))
              ;; indent-stuff
              ))))

(defun virtual-indent-add-kwds (rgx-string-alist)
  "Translate spec into and add to `font-lock-keywords'."
  (->> rgx-string-alist
     (-map (-applify #'virtual-indent--build-kwd))
     (font-lock-add-keywords nil)))

(defun virtual-indent-hook-fn ()
  "Add as hook to (currently just `lisp-mode') to enable virtual-indent."
  (virtual-indent-add-kwds virtual-indent-rgx-string-alist))

;;; Indentation

;; todo

;;; Interactive

(defun virtual-indent-disable ()
  (interactive)
  (remove-hook 'lisp-mode-hook #'virtual-indent-hook-fn)
  (virtual-indent-cleanup-ovs))

(defun virtual-indent-enable ()
  (interactive)
  (virtual-indent-disable)
  (setq font-lock-keywords nil)
  (add-hook 'lisp-mode-hook #'virtual-indent-hook-fn)
  (lisp-mode))
