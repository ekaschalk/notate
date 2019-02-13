(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'smartparens)

(require 'ert)
(require 'faceup)
(require 'f)

(progn (add-to-list 'load-path (-> (f-this-file) (f-parent) (f-parent)))
       (require 'aplig))



;;; Asserts

(defun s-assert (s1 s2)
  "Combine `should' and `s-equals?'."
  (should (s-equals? s1 s2)))



;;; Macros

(defmacro aplig--with-context--nil (&rest body)
  `(let ((aplig-lig-list nil)
         (aplig-mask-list nil))
     ,@body))



;;; Overlays

(defun aplig-test--mock-lig (width)
  (let ((replacement (s-repeat width "a")))
    (aplig-lig--init-lig replacement width)))
