;;; test-helper.el --- Testing Macros -*- lexical-binding: t -*-

;; (require 'buttercup)
;; (require 'ert)
;; (require 'f)

;; (progn (add-to-list 'load-path (-> (f-this-file) (f-parent) (f-parent)))
;;        (require 'nt))

;;; Buttercup Extensions

;; https://github.com/jorgenschaefer/emacs-buttercup/blob/master/docs/writing-tests.md

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

;;; Buttercup-Rewrite

(defun nt-test--setup (kind text notes)
  (apply (-partial #'nt-test--setup* kind text) notes))

(defun nt-test--setup* (kind text &rest notes)
  "Run BODY in context KIND in temp-buffer with (`s-trim'med) text.

KIND is a symbol identifying how notes will contribute to masks:

   'minimal: Notes do not have bounds.

   'simple: Notes are always bounded by the next line.

   'simple-2: Notes are always bounded by the next next line.

   'lispy: Notes use lispy boundaries and inherit `lisp-mode-syntax-table'.

   'no-setup: Notes do not have bounds AND do not run `nt-enable--agnostic'.

   'any: Execute BODY once for each following value of KIND:
           minimal, simple, lispy
"
  (declare (indent 2))

  (insert (s-trim text))
  (nt-test--kind->context kind)
  (unless (eq 'no-setup kind)
    (nt-enable--agnostic))
  (when notes
    (nt-test--mock-notes notes)))

(defun nt-test--teardown ()
  "Disable notate and clear the buffer."
  (nt-disable)
  (delete-region (point-min) (point-max)))

;;; Testing Macro
;;;; Exposes

(defmacro nt-test--with-context (kind buffer-contents &rest body)
  "Run BODY in context KIND in temp-buffer with (`s-trim'med) BUFFER-CONTENTS.

KIND is a symbol identifying how notes will contribute to masks:

   'minimal: Notes do not have bounds.

   'simple: Notes are always bounded by the next line.

   'simple-2: Notes are always bounded by the next next line.

   'lispy: Notes use lispy boundaries and inherit `lisp-mode-syntax-table'.

   'no-setup: Notes do not have bounds AND do not run `nt-enable--agnostic'.

   'any: Execute BODY once for each following value of KIND:
           minimal, simple, lispy
"
  (declare (indent 2))

  (if (eval `(equal 'any ,kind))
      `(nt-test--with-contexts ('minimal 'simple 'lispy) ,buffer-contents ,@body)
    `(with-temp-buffer
       (nt-disable)  ; just-in-case, preceding disable shouldn't ever be needed

       (nt-test--kind->context ,kind)

       (insert (s-trim ,buffer-contents))
       (unless (eq 'no-setup ,kind)
         (nt-enable--agnostic))

       ,@body

       (nt-disable))))

(defmacro nt-test--with-contexts (kinds buffer-contents &rest body)
  "Perform `nt-test--with-context' for all KINDS."
  (when kinds
    `(progn (nt-test--with-context ,(car kinds) ,buffer-contents ,@body)
            (nt-test--with-contexts ,(cdr kinds) ,buffer-contents ,@body))))

;;;; Contexts

(defmacro nt-test--kind->context (kind)
  "See `nt-test--with-context' for documentation on KIND."
  `(cl-case ,kind
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
      (error "Supplied testing context KIND '%s' not implemented" ,kind))))

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

;;; Expanded Shoulds

(defmacro should* (&rest fi)
  "Expands to (progn (should f1) (should f2) ...) for forms FI."
  (when fi
    `(progn (should ,(car fi))
            (should* ,@(cdr fi)))))

(defmacro should= (f1 &rest fi) `(should (= ,f1 ,@fi)))
(defmacro should-equal (f1 f2) `(should (equal ,f1 ,f2)))
(defmacro should-s= (f1 f2) `(should (s-equals? ,f1 ,f2)))
(defmacro should-size (coll size) `(should= (length ,coll) ,size))


(provide 'test-helper)
