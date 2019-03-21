;;; nt-mask-test.el --- Tests -*- lexical-binding: t -*-

;; ~ TEST COVERAGE ~

;; Covered:
;; - Access
;; - Transforms
;; - Initiation
;; - Insertion

;; Implicitly Covered:
;; - Notes added to masks

;; Not Covered:
;; - Decomposition

;;; Access
;;;; Fundamentals

(ert-deftest masks:access:fundamentals:line ()
  (nt-test--with-context 'minimal "
1 foo
2 foo
"
    (should* (not (nt-mask<-line 0))
             (nt-mask<-line 1)
             (nt-mask<-line 2)
             (not (nt-mask<-line 3)))))

(ert-deftest masks:access:fundamentals:lines ()
  (nt-test--with-context 'minimal "
1 foo
2 foo
"
    (should* (not (nt-masks<-lines 0 1))
             (nt-masks<-lines 1 2)
             (nt-masks<-lines 1 3)
             (nt-masks<-lines 0 3)
             (not (nt-masks<-lines 3 4)))))

;;;; Extensions

(ert-deftest masks:access:extensions:region ()
  (nt-test--with-context 'minimal "
123
678
"
    (should-size (nt-masks<-region 1 3)
                 1)
    (should-size (nt-masks<-region 1 6)
                 2)
    (should-size (nt-masks<-region 6 8)
                 1)))

;;; Transforms
;;;; Misc
;;;;; Indent

(ert-deftest masks:transforms:misc:indent ()
  (nt-test--with-context 'minimal "
(foo bar
12345
     note)
"
    (should= (-> 3 nt-mask<-line nt-mask->indent)
             (-> 3 nt-line->indent)
             5)))

;;;;; Widths

(ert-deftest masks:transforms:misc:widths:simplest ()
  (nt-test--with-context 'simple "
(foo bar
     bar
     bar)
"
    (nt-test--mock-notes '(("foo" "f")))

    (should* (-> 1 nt-mask<-line nt-mask--empty?)
             (= (-> 2 nt-mask<-line nt-mask->width)
                (- 3 1))
             (-> 3 nt-mask<-line nt-mask--empty?))))

(ert-deftest masks:transforms:misc:widths:simplest-many-lines ()
  (nt-test--with-context 'simple-2 "
(foo bar
     bar
     bar)
"
    (nt-test--mock-notes '(("foo" "f")))

    (should* (-> 1 nt-mask<-line nt-mask--empty?)
             (= (-> 2 nt-mask<-line nt-mask->width)
                (-> 3 nt-mask<-line nt-mask->width))
             (- 3 1))))

(ert-deftest masks:transforms:misc:widths:complex ()
  "Multiple notes updating multiple masks"
  (nt-test--with-context 'simple-2 "
(foo bazz
     foo
     bar
     bar
     bar)
"
    (nt-test--mock-notes '(("foo" "f") ("bazz" "bob")))

    (let ((note-1-width (- 3 1))
          (note-2-width (- 4 3)))
      (should* (-> 1 nt-mask<-line nt-mask--empty?)
               (= (-> 2 nt-mask<-line nt-mask->width)
                  (+ note-1-width
                     note-2-width))
               (= (-> 3 nt-mask<-line nt-mask->width)
                  (+ note-1-width
                     note-2-width
                     note-1-width))
               (= (-> 4 nt-mask<-line nt-mask->width)
                  note-1-width)
               (-> 5 nt-mask<-line nt-mask--empty?)))))

;;; Init

;; The 'no-setup context won't instantiate `nt-masks', allowing testing
;; insertion of masks upon initiation. We cannot add assertions that use
;; accessors like `nt-mask<-line' as they require `nt-masks' to be fully
;; instantiated. Could be done manually via `nt-ov' methods, but not ideal.

;;;; Single

(ert-deftest masks:init:single ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-masks)
    (nt-mask--init 2)
    (should-size nt-masks 1)))

;;;; Multiple

(ert-deftest masks:init:buffer ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-masks)
    (nt-masks--init)
    (should-size nt-masks 4)))

(ert-deftest masks:init:ranges:start-only ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-masks)
    (let ((start 2) end)
      (nt-masks--init start end))
    (should-size nt-masks (- 4 1))))

(ert-deftest masks:init:ranges:end-only ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-masks)
    (let (start (end 4))
      (nt-masks--init start end))
    (should-size nt-masks (- 4 1))))

(ert-deftest masks:init:ranges ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-masks)
    (let ((start 2) (end 4))
      (nt-masks--init start end))
    (should-size nt-masks (- 4 1 1))))

;;; Refreshing

(ert-deftest masks:refreshing:internal:notes ()
  (nt-test--with-context 'simple-2 "
1 (foo bar
2      bar
3
4      baz)
"
    ;; - Test is a bit verbose, but very straightforward as a result.
    ;; - `nt-note--delete' implicitly calls `nt-mask--refresh-notes'
    ;; - Beware I intentionally chose context s.t. only 1 root is present.

    (-let (((foo bar1 bar2)
            (nt-test--mock-notes '(("foo" "f") ("bar" "b")))))
      (should-size (-> 1 nt-mask<-line nt-mask->notes)
                   0)
      (should-size (-> 2 nt-mask<-line nt-mask->notes)
                   2)
      (should-size (-> 3 nt-mask<-line nt-mask->notes)
                   3)
      (should-size (-> 4 nt-mask<-line nt-mask->notes)
                   1)

      (nt-note--delete bar1)

      (should-size (-> 1 nt-mask<-line nt-mask->notes)
                   0)
      (should-size (-> 2 nt-mask<-line nt-mask->notes)
                   1)
      (should-size (-> 3 nt-mask<-line nt-mask->notes)
                   2)
      (should-size (-> 4 nt-mask<-line nt-mask->notes)
                   1)

      (nt-note--delete foo)

      (should-size (-> 1 nt-mask<-line nt-mask->notes)
                   0)
      (should-size (-> 2 nt-mask<-line nt-mask->notes)
                   0)
      (should-size (-> 3 nt-mask<-line nt-mask->notes)
                   1)
      (should-size (-> 4 nt-mask<-line nt-mask->notes)
                   1)

      (nt-note--delete bar2)

      (should* (null (-> 1 nt-mask<-line nt-mask->notes))
               (null (-> 2 nt-mask<-line nt-mask->notes))
               (null (-> 3 nt-mask<-line nt-mask->notes))
               (null (-> 4 nt-mask<-line nt-mask->notes))))))
