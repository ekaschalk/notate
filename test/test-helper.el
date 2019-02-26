;;; test-helper.el --- Testing Macros -*- lexical-binding: t -*-



(require 'ert)
(require 'faceup)
(require 'f)

(progn (add-to-list 'load-path (-> (f-this-file) (f-parent) (f-parent)))
       (require 'nt))



;;; Macros
;;;; Contexts

(defmacro nt-test--kind->context (kind)
  "See `nt-test--with-context' for documentation on KIND."
  `(cl-case ,kind
     ((minimal no-setup)
      (setq nt-bound?-fn (-const nil)))  ; `nt-bound-fn' won't be reached

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
      (error "Supplied testing context KIND %s not implemented" ,kind))))

(defmacro nt-test--with-context (kind buffer-contents &rest body)
  "Run BODY in context KIND in temp-buffer with (`s-trim'med) BUFFER-CONTENTS.

KIND is a symbol identifying how notes will contribute to masks:

   'minimal: Notes will not contribute to any mask.

   'simple: Notes will always contribute to following line's mask.

   'simple-2: Notes will always contribute to following two line's masks.

   'lispy: Notes use lisp boundary functions to contribute to masks
           and inherit `lisp-mode-syntax-table'.

   'no-setup: Same as 'minimal but do not execute `nt-setup--agnostic'.

   'any: Execute BODY for each of the following values of KIND:
           minimal, simple and lispy

         Useful when mask-note interaction is present but doesn't matter.

After setting the context, `nt-setup--agnostic' is executed. At the time of
writing, it instantiates empty masks for the buffer and sets up managed vars."
  (declare (indent 2))

  `(when (eq 'any ,kind)
     (nt-test--with-context 'minimal buffer-contents ,@body)
     (nt-test--with-context 'simple buffer-contents ,@body)
     (nt-test--with-context 'lispy buffer-contents ,@body))

  `(unless (eq 'any ,kind)
     (with-temp-buffer
       (nt-disable)  ; just-in-case reset managed vars
       (nt-test--kind->context ,kind)

       (insert (s-trim ,buffer-contents))  ; so test lines 1-idxed not 2-idxed

       (unless (eq 'no-setup ,kind)
         (nt-setup--agnostic))

       ,@body

       (nt-disable))))



;;; Mocks
;;;; Notes

(defun nt-test--mock-note (string replacement)
  "Mock notes for STRING to REPLACEMENT."
  (save-excursion
    (goto-char (point-min))

    (let ((rx (nt-note--string->rx string))
          notes)
      (while (re-search-forward rx nil 'noerror)
        (push (nt-note--init string replacement) notes))
      notes)))

(defun nt-test--mock-notes (string-replacement-alist)
  "Map `nt-test--mock-note' over list STRING-REPLACEMENT-ALIST."
  (-mapcat (-applify #'nt-test--mock-note) string-replacement-alist))



;;; Expanded Shoulds

(defmacro should* (&rest fi)
  "Expands to (progn (should f1) (should f2) ...) for forms in FI."
  (when fi
    `(progn (should ,(car fi))
            (should* ,@(cdr fi)))))

(defmacro should= (f1 &rest fi) `(should (= ,f1 ,@fi)))
(defmacro should/= (f1 f2) `(should (/= ,f1 ,f2)))
(defmacro should-eq (f1 f2) `(should (eq ,f1 ,f2)))
(defmacro should-neq (f1 f2) `(should-not (eq ,f1 ,f2)))
(defmacro should-s= (s1 s2) `(should (s-equals? ,s1 ,s2)))
(defmacro should-size (coll size) `(should= (length ,coll) ,size))
(defmacro should-size= (coll1 coll2) `(should= (length ,coll1) (length ,coll2)))
