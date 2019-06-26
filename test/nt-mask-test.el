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

;;; Load Files

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'nt-test))

;;; Access

(nt-describe "Accessing masks"
  ;; Numbers = positions
  :var ((text "
123
678
"))

  (before-all (nt-test--setup 'minimal text))
  (after-all (nt-test--teardown))

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
  (before-all (nt-test--setup 'simple-2 text notes))
  (after-all (nt-test--teardown))

  (describe "lie within bounds"
    (it "at start"
      (expect (-> 1 nt-mask<-line nt-mask--empty?)))
    (it "at end"
      (expect (-> 5 nt-mask<-line nt-mask--empty?))))

  (it "line takes contributions from many notes from one line"
    (expect (-> 2 nt-mask<-line nt-mask->width)
            :to-be
            (+ note-1-width note-2-width)))

  (it "line takes contributions from many notes from many lines"
    (expect (-> 3 nt-mask<-line nt-mask->width)
            :to-be
            (+ note-1-width note-2-width note-1-width)))

  (it "line takes contributions from one note from one line"
    (expect (-> 4 nt-mask<-line nt-mask->width)
            :to-be
            note-1-width)))

;;; Init

(nt-describe "Mask initiation"
  :var ((text "
1 foo
2 foo
3 foo
4 foo
"))

  (before-each (nt-test--setup 'no-setup text))
  (after-each (nt-test--teardown))

  (it "makes a single mask"
    (expect nt-masks :nil)
    (nt-mask--init 2)

    (expect (nt-mask<-line-raw 1) :nil)
    (expect (nt-mask<-line-raw 2))
    (expect (nt-mask<-line-raw 3) :nil)
    (expect (nt-mask<-line-raw 4) :nil))

  (it "makes masks for buffer"
    (expect nt-masks :nil)
    (nt-masks--init)

    (expect (nt-mask<-line-raw 1))
    (expect (nt-mask<-line-raw 2))
    (expect (nt-mask<-line-raw 3))
    (expect (nt-mask<-line-raw 4)))

  (it "makes masks past a given line"
    (expect nt-masks :nil)
    (let ((start 2) end)
      (nt-masks--init start end))

    (expect (nt-mask<-line-raw 1) :nil)
    (expect (nt-mask<-line-raw 2))
    (expect (nt-mask<-line-raw 3))
    (expect (nt-mask<-line-raw 4)))

  (it "makes masks limited by a given line"
    (expect nt-masks :nil)
    (let (start (end 4))
      (nt-masks--init start end))

    (expect (nt-mask<-line-raw 1))
    (expect (nt-mask<-line-raw 2))
    (expect (nt-mask<-line-raw 3))
    (expect (nt-mask<-line-raw 4) :nil))

  (it "makes masks within a given line range"
    (expect nt-masks :nil)
    (let ((start 2) (end 4))
      (nt-masks--init start end))

    (expect (nt-mask<-line-raw 1) :nil)
    (expect (nt-mask<-line-raw 2))
    (expect (nt-mask<-line-raw 3))
    (expect (nt-mask<-line-raw 4) :nil)))
