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

(defmacro aplig--with-context (buffer-contents &rest body)
  `(let (;; Configurations
         (aplig-lig--boundary?-fn (-const nil))

         ;; Managed Objects
         (aplig-lig-list nil)
         (aplig-mask-list nil))
     (with-temp-buffer
       (insert ,buffer-contents)
       (aplig-setup--agnostic)

       ,@body

       (aplig-disable))))


(defmacro aplig--with-context--simple (&rest body)
  (let ((simple-buffer-contents
         "
(string foo
        bar)
"
         ))
    `(aplig--with-context ,simple-buffer-contents ,@body)))



;;; Overlays

(defun aplig-test--mock-lig (string replacement)
  (let* ((width       (- (length string) (length replacement)))
         (start       0)
         (end         (+ start (length string))))
    (aplig-lig--init-lig replacement width start end)))
