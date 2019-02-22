(require 'ert)
(require 'faceup)
(require 'f)

(progn (add-to-list 'load-path (-> (f-this-file) (f-parent) (f-parent)))
       (require 'aplig))



;;; Asserts

;; Aliases for (compose `should' predicate). Not required, but I like it.

(defmacro assert= (f1 f2) `(should (= ,f1 ,f2)))
(defmacro assert/= (f1 f2) `(should (/= ,f1 ,f2)))
(defmacro assert-eq (f1 f2) `(should (eq ,f1 ,f2)))
(defmacro assert-neq (f1 f2) `(should (neq ,f1 ,f2)))
(defmacro assert-s= (s1 s2) `(should (s-equals? ,s1 ,s2)))
(defmacro assert-size (coll size) `(assert= (length ,coll) ,size))
(defmacro assert-size= (coll1 coll2) `(should (= (length ,coll1) (length ,coll2))))



;;; Macros

(defmacro aplig-test--with-context (kind buffer-contents &rest body)
  "Run BODY in context KIND in temp-buffer with (`s-trim'med) BUFFER-CONTENTS.

KIND is a symbol identifying how local variables should be set:

   'minimal: Ligs will not contribute to any mask.

After setting the context, `aplig-setup--agnostic' is executed. At the time of
writing, it instantiates empty masks for the buffer and sets up managed vars."
  (declare (indent 2))
  `(with-temp-buffer
     (aplig-disable)

     (cl-case ,kind
       ('minimal (setq-local aplig-lig--boundary?-fn (-const nil)))
       (else (error "Supplied testing context KIND %s not implemented" kind)))

     (aplig-setup--agnostic)
     (insert (s-trim ,buffer-contents))
     ,@body
     (aplig-disable)))



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
