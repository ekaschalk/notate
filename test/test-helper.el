;;; test-helper.el --- Testing Macros -*- lexical-binding: t -*-



(require 'ert)
(require 'faceup)
(require 'f)

(progn (add-to-list 'load-path (-> (f-this-file) (f-parent) (f-parent)))
       (require 'aplig))



;;; Macros
;;;; Contexts

(defmacro aplig-test--kind->context (kind)
  "See `aplig-test--with-context' for documentation on KIND."
  `(cl-case ,kind
     ((minimal no-setup)
      (setq aplig-bound?-fn (-const nil)))  ; `aplig-bound-fn' won't be reached

     (simple
      (setq aplig-bound?-fn #'identity
            aplig-bound-fn (-juxt (-compose #'1+
                                            #'line-number-at-pos
                                            #'overlay-start)
                                  (-compose #'1+ #'1+
                                            #'line-number-at-pos
                                            #'overlay-start))))

     (simple-2
      (setq aplig-bound?-fn #'identity
            aplig-bound-fn (-juxt (-compose #'1+
                                            #'line-number-at-pos
                                            #'overlay-start)
                                  (-compose #'1+ #'1+ #'1+
                                            #'line-number-at-pos
                                            #'overlay-start))))

     (lispy
      (progn
        (setq aplig-bound?-fn #'aplig-bounds?--lisps
              aplig-bound-fn #'aplig-bounds--lisps)
        (set-syntax-table lisp-mode-syntax-table)))

     (otherwise
      (error "Supplied testing context KIND %s not implemented" kind))))

(defmacro aplig-test--with-context (kind buffer-contents &rest body)
  "Run BODY in context KIND in temp-buffer with (`s-trim'med) BUFFER-CONTENTS.

KIND is a symbol identifying how ligs will contribute to masks:

   'minimal: Ligs will not contribute to any mask.

   'simple: Ligs will always contribute to following line's mask.

   'simple-2: Ligs will always contribute to following two line's masks.

   'lispy: Ligs use lisp boundary functions to contribute to masks
           and inherit `lisp-mode-syntax-table'.

   'no-setup: Same as 'minimal but do not execute `aplig-setup--agnostic'.

   'any: Execute BODY for each of the following values of KIND:
           minimal, simple and lispy

         Useful when mask-lig interaction is present but doesn't matter.

After setting the context, `aplig-setup--agnostic' is executed. At the time of
writing, it instantiates empty masks for the buffer and sets up managed vars."
  (declare (indent 2))

  `(when (eq 'any ,kind)
     (aplig-test--with-context 'minimal buffer-contents ,@body)
     (aplig-test--with-context 'simple buffer-contents ,@body)
     (aplig-test--with-context 'lispy buffer-contents ,@body))

  `(unless (eq 'any ,kind)
     (with-temp-buffer
       (aplig-disable)  ; just-in-case reset managed vars
       (aplig-test--kind->context ,kind)

       (insert (s-trim ,buffer-contents))  ; so test lines 1-idxed not 2-idxed

       (unless (eq 'no-setup ,kind)
         (aplig-setup--agnostic))

       ,@body

       (aplig-disable))))



;;; Mocks
;;;; Ligs

(defun aplig-test--mock-lig (string replacement)
  "Mock ligs for STRING to REPLACEMENT."
  (save-excursion
    (goto-char (point-min))

    (let ((rx (aplig-spec--string->rx string))
          ligs)
      (while (re-search-forward rx nil 'noerror)
        (push (aplig-lig--init string replacement) ligs))
      ligs)))

(defun aplig-test--mock-ligs (string-replacement-alist)
  "Map `aplig-test--mock-lig' over list STRING-REPLACEMENT-ALIST."
  (-mapcat (-applify #'aplig-test--mock-lig) string-replacement-alist))



;;; Asserts

;; Aliases for `should' variations. Not required, but I like it, as it makes
;; assertions captured in threads slightly cleaner.

(defmacro assert= (f1 f2) `(should (= ,f1 ,f2)))
(defmacro assert/= (f1 f2) `(should (/= ,f1 ,f2)))
(defmacro assert-eq (f1 f2) `(should (eq ,f1 ,f2)))
(defmacro assert-neq (f1 f2) `(should-not (eq ,f1 ,f2)))
(defmacro assert-s= (s1 s2) `(should (s-equals? ,s1 ,s2)))
(defmacro assert-size (coll size) `(assert= (length ,coll) ,size))
(defmacro assert-size= (coll1 coll2) `(assert= (length ,coll1) (length ,coll2)))
