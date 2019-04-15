;;; test-helper.el --- Testing Macros -*- lexical-binding: t -*-

;;; Buttercup Extensions
;;;; Macros

(defalias 'xnt-describe #'xdescribe)

(defmacro nt-describe (description &rest body)
  "Equivalent to buttercup's `describe' but uses `-let*' on `:var' bindings."
  (declare (indent 1) (debug (&define sexp def-body)))
  (let ((new-body (if (eq (elt body 0) :var)
                      `((-let* ,(elt body 1)
                          ,@(cddr body)))
                    body)))
    `(buttercup-describe ,description (lambda () ,@new-body))))

;;;; Matchers

(buttercup-define-matcher-for-unary-function :nil null)

(buttercup-define-matcher :size (obj size)
  (let ((obj (funcall obj))
        (size (funcall size)))
    (if (= (length obj) size)
        t
      `(nil . ,(format "Expected %s of size %s to have size %s"
                     obj (length obj) size)))))

;;; Notate Testing Contexts
;;;; Setup

(defun nt-test--setup (context text &optional notes)
  "Run BODY with buffer containing trimmed TEXT and CONTEXT.

CONTEXT is a symbol identifying how notes will contribute to masks:

   'minimal: Notes do not have bounds.

   'simple: Notes are always bounded by the next line.

   'simple-2: Notes are always bounded by the next next line.

   'lispy: Notes use lispy boundaries and inherit `lisp-mode-syntax-table'.

   'no-setup: Notes do not have bounds AND do not run `nt-enable--agnostic'.
"
  (declare (indent 2))

  (insert (s-trim text))

  (cl-case context
    ((minimal no-setup)
     (setq nt-bound?-fn (-const nil)
           ;; TODO the new 'nt-bound prop is still reached even
           ;; with the (-const nil) above. So need better way
           ;; to nil this out.
           nt-bound-fn (-juxt (-compose #'1+
                                        #'line-number-at-pos
                                        #'overlay-start)
                              (-compose #'1+
                                        #'line-number-at-pos
                                        #'overlay-start))))

    (simple
     (setq nt-bound?-fn #'identity
           nt-bound-fn (-juxt (-compose #'1+
                                        #'line-number-at-pos
                                        #'overlay-start)
                              (-compose #'1+ #'1+
                                        #'line-number-at-pos
                                        #'overlay-start))))

    (simple-2
     (setq nt-bound?-fn #'identity
           nt-bound-fn (-juxt (-compose #'1+
                                        #'line-number-at-pos
                                        #'overlay-start)
                              (-compose #'1+ #'1+ #'1+
                                        #'line-number-at-pos
                                        #'overlay-start))))

    (lispy
     (progn
       (setq nt-bound?-fn #'nt-bounds?--lisps
             nt-bound-fn #'nt-bounds--lisps)
       (set-syntax-table lisp-mode-syntax-table)))

    (otherwise
     (error "Supplied testing CONTEXT '%s' not implemented" context)))

  ;; Useful for holding off on things like ov initiation
  (unless (eq 'no-setup context)
    (nt-enable--agnostic))

  (when notes
    (nt-test--mock-notes notes)))

;;;; Teardown

(defun nt-test--teardown ()
  "Disable notate and clear the buffer."
  (nt-disable)
  (delete-region (point-min) (point-max)))

;;; Mocks
;;;; Notes

;; Mocked notes are just notes except that they are built manually instead of
;; through font lock keywords.

(defun nt-test--mock-notes-internal (string replacement)
  "Mock notes for STRING to REPLACEMENT."
  (save-excursion
    (goto-char (point-min))

    (let (notes
          (rx (nt-kwd--string->rx string)))
      (while (re-search-forward rx nil 'noerror)
        (-let* (((start end) (match-data 1))
                (note (nt-note--init string replacement start end)))
          (push note notes)))
      notes)))

(defun nt-test--mock-notes (defs)
  "Mock all notes for DEFS (a string-replacement-alist) and sort."
  (->> defs (-mapcat (-applify #'nt-test--mock-notes-internal)) nt-notes--sort))

;;; Provide

(provide 'test-helper)
