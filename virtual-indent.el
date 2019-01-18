;;; virtual-indent.el --- Personal Indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Exploring concept of "personalized indentation"

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

;;;; Setup

(defconst virtual-indent-specs
  (list (virtual-indent-make-spec "Hello Lig"   "hello"     "")
        (virtual-indent-make-spec "0-space Lig" "0-space"   "")
        (virtual-indent-make-spec "1-space Lig" "1-space"   " ")
        (virtual-indent-make-spec "2-space Lig" "2-space"   "  ")
        (virtual-indent-make-spec "tab Lig"     "tab-space" "	")
        )
  "Collection of `virtual-indent-make-spec' specifying ligature replacements.")

;;;; Managed
;;;;; Constants

(defconst virtual-indent--lig-subexp 1
  "Alias the SUBEXP for ligatures in `match-data'.")

;;;;; Variable

(defconst virtual-indent-lig-ovs nil
  "List of ligature overlays currently managed.")

(defconst virtual-indent-indent-ovs nil
  "List of indent overlays currently managed.")

;;; Logging
;;;; Configuration

(defconst virtual-indent-logging-buffer-name "*virtual-indent-log*"
  "The buffer name for virtual-indent-logs.")

(defconst virtual-indent-enable-logs? t
  "Enable virtual-indent logging facilities?")

(defconst virtual-indent-log-verbose? t
  "Enable verbose logging?")

(defconst virtual-indent-persist-logs? nil
  "Whether to persist logs when restarting virtual-indent.")

;;;; Management

(defun virtual-indent-get-or-create-log-buffer ()
  "Get or create the logging buffer for virtual-indent."
  (setq virtual-indent-logging-buffer
        (get-buffer-create virtual-indent-logging-buffer-name)))

(defun virtual-indent-cleanup-logs ()
  "Kill virtual-indent's logging buffer."
  (kill-buffer (virtual-indent-get-or-create-log-buffer)))

(defun virtual-indent-cleanup-logs-maybe ()
  "Cleanup logs if configured to with `virtual-indent-cleanup-logs?'."
  (unless virtual-indent-persist-logs?
    (virtual-indent-cleanup-logs)))

;;;; Messaging

(defun virtual-indent-format-log (msg)
  "Format MSG for virtual-indent logs."
  msg)

(defun virtual-indent-log (msg)
  "Log MSG in virtual-indent's log."
  (with-current-buffer (virtual-indent-get-or-create-log-buffer)
    (goto-char (point-max))
    (insert (virtual-indent-format-log msg))
    (newline)))

(defun virtual-indent-log-maybe (msg)
  "Run `virtual-indent-log' only when logs enabled."
  (when virtual-indent-enable-logs?
    (virtual-indent-log msg)))

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
  (-> subexp
     virtual-indent--ovs-in
     (-contains? ov)))

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
  "Remove deleted lig overlauys from `virtual-indent-lig-ovs'"
  (setq virtual-indent-lig-ovs
        (-filter #'overlay-buffer virtual-indent-lig-ovs)))

(defun virtual-indent--delete-lig-ov (ov)
  "Delete a ligature overlay."
  (-doto ov
    (virtual-indent-log-lig-ov-deletion-maybe)

    (delete-overlay))
  (virtual-indent--trim-lig-ovs))

;; indenters
(defun virtual-indent--trim-indent-ovs ()
  (setq virtual-indent-indent-ovs
        (-filter #'overlay-buffer virtual-indent-indent-ovs)))

(defun virtual-indent--delete-indent-ov (ov)
  "Delete a ligature overlay."
  (-doto ov
    (virtual-indent-log-indent-ov-deletion-maybe)

    (delete-overlay))
  (virtual-indent--trim-indent-ovs))

;;;; Logs

(defun virtual-indent-log-lig-ov-creation-maybe (ov)
  "Log ligature overlay creation."
  (virtual-indent-log-maybe (format "Created lig overlay %s" ov)))

(defun virtual-indent-log-lig-ov-deletion-maybe (ov)
  "Log ligature overlay creation."
  (virtual-indent-log-maybe (format "Deleting lig overlay %s" ov)))

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
  (virtual-indent-log-maybe "Cleaning up ligature overlays...")

  (virtual-indent-cleanup-lig-ovs)
  (virtual-indent-cleanup-indent-ovs)

  (virtual-indent-log-maybe "Finished cleaning up ligature overlays..."))

;;;; Functions

(defun virtual-indent--ov-mod-hook (ov post-modification? start end &optional _)
  "Overlay modification hook to force evaporation upon modification within ov."
  (when post-modification?
    (virtual-indent--delete-lig-ov ov)))

(defun virtual-indent-build-lig-ov (replacement)
  "Build ligature overlay for current `match-data'."
  (let ((ov (virtual-indent--make-lig-ov)))
    (-doto ov
      (virtual-indent-log-lig-ov-creation-maybe)

      (overlay-put 'display replacement)
      (overlay-put 'modification-hooks '(virtual-indent--ov-mod-hook)))
    (add-to-list 'virtual-indent-lig-ovs ov)))

;;; Indent

(defun virtual-indent-log-indent-ov-creation-maybe (ov)
  "Log indent overlay creation."
  (virtual-indent-log-maybe (format "Created indent overlay %s" ov)))

(defun virtual-indent-log-indent-ov-deletion-maybe (ov)
  "Log indent overlay deletion."
  (virtual-indent-log-maybe (format "Deleting indent overlay %s" ov)))

(defun virtual-indent--delete-indent-ov (ov)
  "Delete an indent overlay."
  (-doto ov
    (virtual-indent-log-indent-ov-deletion-maybe)

    (delete-overlay))
  (virtual-indent--trim-lig-ovs))

(defun virtual-indent-build-indent-ov (width)
  (let* ((start (line-beginning-position 2))
         (end (+ start width))
         (ov (make-overlay start end)))
    (-doto ov
      (virtual-indent-log-indent-ov-creation-maybe)

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
    (virtual-indent-log-maybe (format "Building kwd %s" name))

    `(,rx (0 (prog1 virtual-indent-lig-face
               (virtual-indent-match ,replacement ,width))))))

(defun virtual-indent-add-kwds ()
  "Translate spec into keywords and add to `font-lock-keywords'."
  (let ((kwds (-map #'virtual-indent--build-kwd virtual-indent-specs)))
    (virtual-indent-log-maybe "Finished building kwds")

    (font-lock-add-keywords nil kwds)))

(defun virtual-indent-hook-fn ()
  "Add as hook to enable virtual-indent.

Currently:
1. Just an alias on `virtual-indent-add-kwds'.
2. Works for just `lisp-mode'."
  (virtual-indent-add-kwds))

;;; Interactive

(defun virtual-indent-log-switch-to ()
  (interactive)
  (switch-to-buffer-other-window (virtual-indent-get-or-create-log-buffer)))

(defun virtual-indent-disable ()
  "Disable and cleanup virtual-indent."
  (interactive)
  (virtual-indent-log-maybe "Disabling virtual-indent...")

  (setq font-lock-keywords nil)
  (remove-hook 'lisp-mode-hook #'virtual-indent-hook-fn)
  (virtual-indent-cleanup-ovs)
  (virtual-indent-cleanup-logs))

(defun virtual-indent-enable ()
  "Enable virtual-indent and cleanup previous instance if running."
  (interactive)
  (virtual-indent-disable)

  (virtual-indent-log-maybe "Enabling virtual-indent...")

  (add-hook 'lisp-mode-hook #'virtual-indent-hook-fn)
  (lisp-mode))

;;; Development Stuff

(when eric?
  (spacemacs/declare-prefix "d" "dev")
  (spacemacs/set-leader-keys "de" #'virtual-indent-enable)
  (spacemacs/set-leader-keys "dd" #'virtual-indent-disable)
  (spacemacs/set-leader-keys "dl" #'virtual-indent-log-switch-to))

(defconst virtual-indent-lig-face (if virtual-indent-log-verbose?
                                      font-lock-function-name-face
                                    nil)
  "Make it easier to tell when a ligature is found.")
