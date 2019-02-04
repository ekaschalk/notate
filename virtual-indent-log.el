;;; virtual-indent-log.el --- logging module -*- lexical-binding: t; -*-

;;; Commentary:

;; Extra logging features to track what's happening when for both development
;; and future debug messages since `font-lock-mode' inhibits `edebug'.
;; Currently not using this module.

;;; Code:
;;;; Requires

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)

;;; Configuration
;;;; Configure

(defconst virtual-indent-log-enabled? t
  "Enable virtual-indent logging facilities?")

(defconst virtual-indent-log-verbose? t
  "Enable verbose logging?")

(defconst virtual-indent-log-persists? nil
  "Whether to persist logs when restarting virtual-indent.")

;;;; Managed

(defconst virtual-indent-logging-buffer-name "*virtual-indent-log*"
  "The buffer name for virtual-indent-logs.")

;;; Log Management

(defun virtual-indent-log-get-or-create-buffer ()
  "Get or create the logging buffer for virtual-indent."
  (setq virtual-indent-logging-buffer
        (get-buffer-create virtual-indent-logging-buffer-name)))

(defun virtual-indent-log-cleanup ()
  "Kill virtual-indent's logging buffer."
  (kill-buffer (virtual-indent-log-get-or-create-buffer)))

(defun virtual-indent-log-cleanup-maybe ()
  "Cleanup logs if configured to with `virtual-indent-log-cleanup?'."
  (unless virtual-indent-log-persists?
    (virtual-indent-log-cleanup)))

;;; Messaging

(defun virtual-indent-log-format (msg)
  "Format MSG for virtual-indent logs."
  msg)

(defun virtual-indent-log (msg)
  "Log MSG in virtual-indent's log."
  (with-current-buffer (virtual-indent-log-get-or-create-buffer)
    (goto-char (point-max))
    (insert (virtual-indent-log-format msg))
    (newline)))

(defun virtual-indent-log-maybe (msg)
  "Run `virtual-indent-log' only when logs enabled."
  (when virtual-indent-log-enabled?
    (virtual-indent-log msg)))

;;; Specialized

(defun virtual-indent-log-indent-ov-creation-maybe (ov)
  "Log indent overlay creation."
  (virtual-indent-log-maybe (format "Created indent overlay %s" ov)))

(defun virtual-indent-log-indent-ov-deletion-maybe (ov)
  "Log indent overlay deletion."
  (virtual-indent-log-maybe (format "Deleting indent overlay %s" ov)))

(defun virtual-indent-log-lig-ov-creation-maybe (ov)
  "Log ligature overlay creation."
  (virtual-indent-log-maybe (format "Created lig overlay %s" ov)))

(defun virtual-indent-log-lig-ov-deletion-maybe (ov)
  "Log ligature overlay creation."
  (virtual-indent-log-maybe (format "Deleting lig overlay %s" ov)))

;;; Interactive

(defun virtual-indent-log-switch-to ()
  (interactive)
  (switch-to-buffer-other-window (virtual-indent-log-get-or-create-buffer)))

(spacemacs/set-leader-keys "dl" #'virtual-indent-log-switch-to)
