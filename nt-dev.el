;;; nt-dev.el --- Commands helping nt development -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Extend `nt' with development and debugging utilities like pprints,
;; toggles, keybindings, and testing buffer commands.

;;; Code:
;;;; Requires

(require 'nt)

;;; Configuration
;;;; Development

(defconst nt-dev--modules
  '("nt-dev.el" "nt.el"

    ;; Components
    "nt-base.el" "nt-bounds.el" "nt-kwds.el" "nt-ov.el"
    "nt-mask.el" "nt-note.el"
    "nt-change.el")
  ;; Yes I'll make this automatic later
  "A list of nt's elisp file names, for reloading dev utility.")

(defconst nt-dev--test-buffer "*nt-development*"
  "Buffer name for notate experiments.")

(defconst nt-dev--test-buffer-indirect "*nt-development-no-notes*"
  "Buffer name for notate experiments indirect counterpart.")

;;;; Test Buffer

(defconst nt-dev--test-defs
  '(("hello" "∧")
    ("bye" "!∨"))
  "Notes for testing.")

(defconst nt-dev--test-text
  ";; Test-bed for notate

(hello hello
       (bye foo

            bar
            baz)
       foo
       bar

       bar
       baz)

;; End of testing
"
  "Text to for ad-hoc testing.")

;;;; Screenshot Buffer

(defconst nt-dev--screenshot-defs
  `(
    ("and" "∧")
    ;; ("or" "∨")
    ;; ("or" "||")
    ;; ("int" "ℤ")

    ;; Yes I will make the symbol capture nicer to define later.
    ("and" "&&")
    ("loop" "∀" ,(rx (: symbol-start "loop" symbol-end)))
    ("compose" "∘" ,(rx (: symbol-start "compose" symbol-end)))
    ("everyone" "")
    ("great" "")
    ("in"   "∈" ,(rx (: symbol-start "in" symbol-end)))

    ;; ("not" "¬" ,(rx (: symbol-start "not" symbol-end)))
    )
  "Notes for screenshots.")

(defconst nt-dev--screenshot-text
  "
(and foo
     (and foo
          (and foo
               bar)
          bar)
     bar)
"
  ;;   ";; Notate - Indentation-Aware Visual Replacements

  ;; (loop symbols in (and math our-dreams)
  ;;       (compose be possible in

  ;;                programming)

  ;;       without annoying
  ;;       (everyone right?
  ;;                 great))
  ;; "
  "Text for screenshots.")

;;; Printing

(defun nt-dev--print (ov)
  "Dispatch formater for OV and message."
  (message
   (cond ((null ov)
          "No ov found.")
         ((nt-ov--note? ov)
          (nt-note--format ov))
         ((nt-ov--mask? ov)
          (nt-mask--format ov)))))

(defun nt-note--format (note)
  "Format NOTE for pprinting."
  (let* ((start (overlay-start note))
         (end (overlay-end note))
         (line (line-number-at-pos start))
         (string (buffer-substring-no-properties start end))
         (replacement (overlay-get note 'display))
         (width (- (length string) (length replacement)))
         (masks (nt-note->masks note)))
    (format "Note overlay:
start: %s
end: %s
line: %s
width: %s
string: %s
replacement: %s
masks: %s
"
            start end line width string replacement masks)))

(defun nt-mask--format (mask)
  "Format MASK for pprinting."
  (let* ((start (overlay-start mask))
         (end (overlay-end mask))
         (line (line-number-at-pos start))
         (notes (nt-mask->notes mask))
         (opaque-end (nt-mask->opaque-end mask)))
    (format "Mask overlay:
start: %s
end: %s
line: %s
notes: %s
opaque-end: %s
"
            start end line notes opaque-end)))

;;; Commands

(defun nt-dev--print-at-point ()
  "Print nt OV at point."
  (interactive) (nt-dev--print (nt-ov--at-point)))

(defun nt-dev--switch-to-test-buffer-internal (text)
  "Switch to a buffer for experimenting with notate."
  (switch-to-buffer-other-window (get-buffer-create nt-dev--test-buffer))

  (delete-region (point-min) (point-max))
  (insert text)
  (goto-char (point-min))

  (local-set-key "q" 'quit-window)
  (when (fboundp 'evil-local-set-key)
    (evil-local-set-key 'normal "q" 'quit-window)))

;;;###autoload
(defun nt-dev--switch-to-test-buffer ()
  "Perform `nt-dev--switch-to-test-buffer-internal' experiment friendly settings."
  (interactive)

  (let ((nt-defs
         nt-dev--test-defs))
    (nt-dev--switch-to-test-buffer-internal nt-dev--test-text)

    (setq nt-normalize-height? nil)
    (setq nt-display-prefixes? nil)
    (setq nt-display-render-status? t)
    (setq nt-render-masks? t)

    (nt-enable)))

;;;###autoload
(defun nt-dev--switch-to-screenshot-buffer ()
  "Perform `nt-dev--switch-to-test-buffer-internal' screenshot friendly settings."
  (interactive)

  (let ((nt-defs
         nt-dev--screenshot-defs))
    (nt-dev--switch-to-test-buffer-internal nt-dev--screenshot-text)

    ;; I go back-and-forth on whether to have true or nil. Both are useful.
    (setq nt-normalize-height? nil)
    ;; (setq nt-normalize-height? t)

    ;; same for these two
    ;; (setq nt-display-render-status? t)
    (setq nt-display-render-status? nil)

    (setq nt-display-prefixes? nil)
    (setq nt-render-masks? t)

    (make-indirect-buffer nt-dev--test-buffer nt-dev--test-buffer-indirect 'clone)
    (nt-enable)
    (switch-to-buffer-other-window (get-buffer nt-dev--test-buffer-indirect))))

;;;###autoload
(defun nt-dev--reload ()
  "(Re)load all `nt-dev--modules'."
  (interactive) (-each nt-dev--modules #'load-file))

;;;; Bind Keys

(when (and (boundp 'eric?) eric?)
  (spacemacs/declare-prefix
    "d" "dev")
  (spacemacs/set-leader-keys
    ;; Toggles
    "de" #'nt-enable
    "dd" #'nt-disable

    ;; Develop Commmands
    "dr" #'nt-dev--reload
    "db" #'nt-dev--switch-to-test-buffer
    "dB" #'nt-dev--switch-to-screenshot-buffer

    ;; Debug Commands
    "dp" #'nt-dev--print-at-point))

;;; Provide

(provide 'nt-dev)

;;; nt-dev.el ends here
