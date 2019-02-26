;;; nt-dev.el --- Commands helping nt development -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Extend `nt' with development and debugging utilities like pprints,
;; toggles, keybindings, and testing buffer commands.



;;; Code:
;;;; Requires

(require 'nt)



;;; Configuration

(defconst nt-dev--modules
  '("nt-dev.el"

    ;; Modules
    "nt-bounds.el"
    "nt-mask.el"
    "nt-note.el"
    "nt-ov.el"

    ;; Core
    "nt.el"
    "nt-base.el")
  "A list of nt's elisp file names, for reloading dev utility.")

(defconst nt-dev--test-buffer "*nt-development*"
  "Buffer name to contain `nt-dev--test-text' for experimenting with nt.")

(defconst nt-dev--test-text
  ";; Test-bed for nt

(hello hello
       (bye foo

            bar
            baz)
       foo
       baz)

;; End of testing
"
  "Text to use for ad-hoc testing.")



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
         (masks (nt--masks-for note)))
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
;;;; Definitions

;;;###autoload
(defun nt-dev--switch-to-test-buffer ()
  "Get or create an nt test buffer with `nt-dev--test-text' inserted.

We can easily find ourselves with the error:
  'Changes to be undone are outside visible portion of buffer'.

When working on nt, necessitating a transient testing buffer."
  (interactive)

  (switch-to-buffer-other-window (get-buffer-create nt-dev--test-buffer))

  (delete-region (point-min) (point-max))
  (insert nt-dev--test-text)
  (goto-char (point-min))

  (local-set-key "q" 'quit-window)
  (when (fboundp 'evil-local-set-key)
    (evil-local-set-key 'normal "q" 'quit-window)))

(defun nt-dev--print-at-point ()
  "Print nt OV at point."
  (interactive) (nt-dev--print (nt-ov--at-point)))

;;;###autoload
(defun nt-dev--reload ()
  "(Re)load all `nt-dev--modules'."
  (interactive) (-each nt-dev--modules #'load-file))

;;;; Bind Keys

(when eric?
  (spacemacs/declare-prefix
    "d" "dev")
  (spacemacs/set-leader-keys
    ;; Toggles
    "de" #'nt-enable
    "dd" #'nt-disable

    ;; Develop Commmands
    "dr" #'nt-dev--reload
    "db" #'nt-dev--switch-to-test-buffer

    ;; Debug Commands
    "dp" #'nt-dev--print-at-point))



(provide 'nt-dev)



;;; nt-dev.el ends here
