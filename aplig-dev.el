;;; aplig-dev.el --- Commands helping aplig development -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Extend `aplig' with development and debugging utilities like pprints,
;; toggles, keybindings, and testing buffer commands.



;;; Code:
;;;; Requires

(require 'aplig)



;;; Configuration

(defconst aplig-dev--modules
  '("aplig-dev.el"

    ;; Core
    "aplig.el"
    "aplig-base.el"

    ;; Modules
    "aplig-spec.el"
    "aplig-ov.el")
  "A list of aplig's elisp file names, for reloading dev utility.")

(defconst aplig-dev--test-buffer "*aplig-development*"
  "Buffer name to contain `aplig-dev--test-text' for experimenting with aplig.")

(defconst aplig-dev--test-text
  ";; Test-bed for aplig

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

(defun aplig-dev--print (ov)
  "Dispatch formater for OV and message."
  (message
   (cond ((null ov)
          "No ov found.")
         ((aplig-ov--lig? ov)
          (aplig-lig--format ov))
         ((aplig-ov--mask? ov)
          (aplig-mask--format ov)))))

(defun aplig-lig--format (lig)
  "Format LIG for pprinting."
  (let* ((start (overlay-start lig))
         (end (overlay-end lig))
         (line (line-number-at-pos start))
         (string (buffer-substring-no-properties start end))
         (replacement (overlay-get lig 'display))
         (width (- (length string) (length replacement)))
         (masks (aplig-lig-mask--masks-for lig)))
    (format "Lig overlay:
start: %s
end: %s
line: %s
width: %s
string: %s
replacement: %s
masks: %s
"
            start end line width string replacement masks)))

(defun aplig-mask--format (mask)
  "Format MASK for pprinting."
  (let* ((start (overlay-start mask))
         (end (overlay-end mask))
         (line (line-number-at-pos start))
         (ligs (overlay-get mask 'aplig-ligs)))
    (format "Mask overlay:
start: %s
end: %s
line: %s
ligs: %s
"
            start end line ligs)))



;;; Commands
;;;; Definitions

(defun aplig-dev--switch-to-test-buffer ()
  "Get or create an aplig test buffer with `aplig-dev--test-text' inserted.

We can easily find ourselves with the error:
  'Changes to be undone are outside visible portion of buffer'.

When working on aplig, necessitating a transient testing buffer."
  (interactive)

  (when (switch-to-buffer-other-window
         (get-buffer-create aplig-dev--test-buffer))
    (delete-region (point-min) (point-max))
    (insert aplig-dev--test-text)
    (goto-char (point-min))

    (local-set-key "q" 'quit-window)
    (when (fboundp 'evil-local-set-key)
      (evil-local-set-key 'normal "q" 'quit-window))))

(defun aplig-dev--print-at-point ()
  "Print aplig OV at point."
  (interactive) (aplig-dev--print (aplig-ov--at-point)))

(defun aplig-dev--reload ()
  "(Re)load all `aplig-dev--modules'."
  (interactive) (-map #'load-file aplig-dev--modules))

;;;; Bind Keys

(when eric?
  (spacemacs/declare-prefix
    "d" "dev")
  (spacemacs/set-leader-keys
    ;; Toggles
    "de" #'aplig-enable
    "dd" #'aplig-disable

    ;; Develop Commmands
    "db" #'aplig-dev--reload
    "dp" #'aplig-dev--print-at-point
    "dr" #'aplig-dev--switch-to-test-buffer))



(provide 'aplig-dev)



;;; aplig-dev.el ends here
