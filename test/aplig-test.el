;;; aplig-test.el --- Tests -*- lexical-binding: t -*-



;;; Ligs
;;;; Widths

(ert-deftest ligs::widths::none ()
  (should (= 0
             (aplig-ligs->width nil))))

(ert-deftest ligs::widths::one ()
  (aplig--with-context--nil
   (let* ((width 1)
          (lig   (aplig-test--mock-lig width))
          (ligs  (list lig)))
     (should (= width
                (aplig-ligs->width ligs))))))

(ert-deftest ligs::widths::some ()
  (aplig--with-context--nil
   (let* ((width-1 2)
          (width-2 3)
          (width   5)
          (lig-1   (aplig-test--mock-lig width-1))
          (lig-2   (aplig-test--mock-lig width-2))
          (ligs    (list lig-1 lig-2)))
     (should (= width
                (aplig-ligs->width ligs))))))
