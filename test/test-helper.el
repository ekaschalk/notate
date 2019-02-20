(require 'ert)
(require 'faceup)
(require 'f)

(progn (add-to-list 'load-path (-> (f-this-file) (f-parent) (f-parent)))
       (require 'aplig))



;;; Asserts

;; Aliases for (compose `should' predicate). Not required, I just like it.

(defmacro assert= (o1 o2) `(should (= ,o1 ,o2)))
(defmacro assert/= (o1 o2) `(should (/= ,o1 ,o2)))
(defmacro assert-s= (s1 s2) `(should (s-equals? ,s1 ,s2)))



;;; Macros

(defmacro aplig-test--with-context (kind buffer-contents &rest body)
  "Execute BODY with aplig context KIND in temp-buffer with BUFFER-CONTENTS.

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
     (insert ,buffer-contents)
     ,@body
     (aplig-disable)))



;;; Mocks
;;;; Ligs

(defun aplig-test--mock-lig (string replacement)
  "Mock lig for STRING to REPLACEMENT. STRING should exist in buffer!"
  ;; set match data and error if you mock a lig that doesn't exist
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (aplig-spec--string->rx string)))

  (aplig-lig--init string replacement))

(defun aplig-test--mock-ligs (string-replacement-pairs)
  "Map `aplig-test--mock-lig' over list STRING-REPLACEMENT-PAIRS."
  (-map (-applify #'aplig-test--mock-lig) string-replacement-pairs))
