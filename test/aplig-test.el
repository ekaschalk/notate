;;; aplig-test.el --- Tests -*- lexical-binding: t -*-



;;; Ligs
;;;; Widths

(ert-deftest ligs::widths::none ()
  (should (= 0 (aplig-ligs->width nil))))

(ert-deftest ligs::widths::one ()
  (aplig--with-context--simple
   (let* ((s "string")
          (r "lig")
          (ligs (list (aplig-test--mock-lig s r)))

          (exp (- (length s) (length r)))
          (ret (aplig-ligs->width ligs)))

     (should (= exp ret)))))

(ert-deftest ligs::widths::some ()
  (aplig--with-context--simple
   (let* ((s-1 "string")
          (s-2 "fooo")
          (r-1 "lig")
          (r-2 "bar")
          (ligs (list (aplig-test--mock-lig s-1 r-1)
                      (aplig-test--mock-lig s-2 r-2)))

          (exp (- (+ (length s-1) (length s-2))
                  (+ (length r-1) (length r-2))))
          (ret (aplig-ligs->width ligs)))

     (should (= exp ret)))))
