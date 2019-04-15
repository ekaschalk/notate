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

;;; Load Test Helper

;; There has to be a cleaner way to do this

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'nt)
       (add-to-list 'load-path (f-parent (f-this-file)))
       (require 'test-helper))

;;; Access

(nt-describe "Accessing masks"
  ;; Numbers = positions
  :var ((text "
123
678
"))

  (before-each (nt-test--setup-no-notes text))
  (after-each (nt-test--teardown))

  (describe "by line"
    (it "nothing if outside buffer"
      (expect (nt-mask<-line 0) :nil)
      (expect (nt-mask<-line 3) :nil))
    (it "finds the mask"
      (expect (nt-mask<-line 1) :to-be-truthy)
      (expect (nt-mask<-line 2) :to-be-truthy)))

  (describe "by lines"
    (it "has RHS closed"
      (expect (nt-masks<-lines 0 1) :nil)
      (expect (nt-masks<-lines 2 2) :nil)
      (expect (nt-masks<-lines 3 4) :nil)
      (expect (nt-masks<-lines 0 2) :size 1)
      (expect (nt-masks<-lines 1 2) :size 1)
      (expect (nt-masks<-lines 1 3) :size 2)))

  (describe "by region"
    (it "finds one"
      (expect (nt-masks<-region 1 3) :size 1)
      (expect (nt-masks<-region 6 8) :size 1))
    (it "finds many"
      (expect (nt-masks<-region 1 6) :size 2))
    (it "finds none"
      (expect (nt-masks<-region 9 20) :nil))))

;;; Widths

(nt-describe "Notes contribute widths to appropriate masks"
  :var ((text "
(note1 note2
       note1
       foo
       foo
       foo)
")
        (notes '(("note1" "n") ("note2" "noo")))
        (note-1-width (- 5 1))
        (note-2-width (- 5 3)))

  ;; Pay attention to the CONTEXT
  (before-each (nt-test--setup 'simple-2 text notes))
  (after-each (nt-test--teardown))

  (it "no contribution to same line as note"
    (expect (-> 1 nt-mask<-line nt-mask--empty?)))
  (it "line takes contributions from many notes from one line"
    (expect (-> 2 nt-mask<-line nt-mask->width)
            :to-be (+ note-1-width note-2-width)))
  (it "line takes contributions from many notes from many lines"
    (expect (-> 3 nt-mask<-line nt-mask->width)
            :to-be (+ note-1-width note-2-width note-1-width)))
  (it "don't extend past bounds"
    (expect (-> 4 nt-mask<-line nt-mask->width)
            :to-be note-1-width)
    (expect (-> 5 nt-mask<-line nt-mask--empty?))))

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
