;;; nt-note-test.el --- Tests -*- lexical-binding: t -*-

;; ~ TEST COVERAGE ~

;; Covered:
;; - Access
;; - Transforms
;; - Sorting
;; - Root-Finding
;; - Deletion (internal-only)

;; Implicitly Covered:
;; - Initiation (note mocks bypass font-lock-mode, not inititation methods)
;; - Insertion

;; Not Covered:
;; - Decomposition

;;; Load Test Helper

;; There has to be a cleaner way to do this

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'nt)
       (add-to-list 'load-path (f-parent (f-this-file)))
       (require 'test-helper))

;;; Template
;;;; One Note

(nt-describe "foo bar"
  :var ((text "
(note foo
      bar)
(foo bar)
")
        (notes '(("note" "n")))
        mocked-note)
  (before-all (setq mocked-note (nt-test--setup 'simple text notes)))
  (after-all (nt-test--teardown))

  )

;;;; Many Notes

(nt-describe "foo bar"
  :var ((text "
(note1 note2
       note3)
(foo bar)
")
        (notes '(("note1" "n") ("note2" "n") ("note3" "n")))
        )
  (before-all (nt-test--setup 'simple text notes))
  (after-all (nt-test--teardown))

  )

;;; Access

(nt-describe "Accessing notes"
  :var ((text "
(note1 note2
       note3)
(foo bar)
")
        (notes '(("note1" "n") ("note2" "n") ("note3" "n")))
        (note-start 3)
        (note-width 5)
        (point-without-note 18)
        ((line-with-no-notes line-with-one-note line-with-two-notes) '(3 2 1))
        (region-with-no-notes `(,point-without-note
                                ,(1+ point-without-note)))
        (region-with-one-note `(,note-start
                                ,(1+ note-start)))
        (region-with-two-notes `(,note-start
                                 ,(+ note-start note-width note-width)))
        (region-before-notes `(,(- point-without-note 2)
                               ,(- point-without-note 1))))

  (before-all (nt-test--setup 'simple text notes))
  (after-all (nt-test--teardown))

  (describe "by position"
    (describe "standard cases"
      (it "there"
        (expect (nt-note<-pos note-start)))
      (it "not there"
        (expect (nt-note<-pos point-without-note)
                :nil)))

    (describe "edge cases"
      (it "nil"
        (expect (nt-note<-pos nil)
                :nil))
      (it "outside buffer"
        (expect (nt-note<-pos -1)
                :nil)
        (expect (nt-note<-pos 1000)
                :nil))))

  (describe "by line"
    (it "found none"
      (expect (nt-notes<-line line-with-no-notes)
              :nil))
    (it "found one"
      (expect (nt-notes<-line line-with-one-note)
              :size 1))
    (it "found many"
      (expect (nt-notes<-line line-with-two-notes)
              :size 2)))

  (describe "by lines"
    (it "returns nil if start and end lines are equal"
      (expect (nt-notes<-lines line-with-one-note line-with-one-note)
              :nil))
    (it "found one"
      (expect (nt-notes<-lines line-with-one-note (1+ line-with-one-note))
              :size 1)))

  (describe "by region"
    (it "found none"
      (expect (apply #'nt-notes<-region region-before-notes)
              :nil))
    (it "found one"
      (expect (apply #'nt-notes<-region region-with-one-note)
              :size 1))
    (it "found many"
      (expect (apply #'nt-notes<-region region-with-two-notes)
              :size 2))))

;;; Transforms

(nt-describe "Transforming notes"
  :var ((text "
(note foo
      bar)
(foo bar)
")
        (notes '(("note" "n")))
        ((true-width display-width) '(4 1))
        mocked-note)

  (before-all (setq mocked-note (car (nt-test--setup 'simple text notes))))
  (after-all (nt-test--teardown))

  (it "back to its actual string"
    (expect (nt-ov->string mocked-note)
            :to-equal "note"))

  (it "calculates width as difference of true and displayed lengths"
    (expect (nt-ov->width mocked-note)
            :to-be (- true-width display-width))))

;;; Sorting

(nt-describe "Sorting notes by buffer position"
  :var ((text "
(note1 note2
       note3)
(foo bar)
")
        (notes '(("note1" "n") ("note2" "n") ("note3" "n")))
        ov-at-buffer-start ov-at-buffer-end
        mocked-notes)

  (before-all (setq mocked-notes (nt-test--setup 'simple text notes))
              (setq ov-at-buffer-start (make-overlay (point-min) (point-min)))
              (setq ov-at-buffer-end (make-overlay (point-max) (point-max))))
  (after-all (nt-test--teardown))

  (describe "finds idx of insertion"
    (it "of notes"
      (-let (((note-1 note-2 note-3) mocked-notes))
        (expect (nt-note->idx note-1)
                :to-be 1)
        (expect (nt-note->idx note-2)
                :to-be 2)
        (expect (nt-note->idx note-3)
                :to-be 3)))
    (it "at start"
      (expect (nt-note->idx ov-at-buffer-start)
              :to-be 0))
    (it "at end"
      (expect (nt-note->idx ov-at-buffer-end)
              :to-be 3))))

;;; Relationships
;;;; Comparisons

(ert-deftest notes:relationships:comparisons:sorting ()
  (nt-test--with-context 'any "
1 (note1 foo
2        bar)
3
4 (note2 foo
5        bar)
6
7 (note1 note2
8        foo
9        bar)
"
    ;; The mock itself sorts for easier destructuring, so dont need to
    ;; explicitly call `nt-notes--sort'
    (-let ((notes
            (nt-test--mock-notes '(("note1" "n") ("note2" "n")))))
      (should-equal notes
                    `(,@(nt-notes<-line 1)
                      ,@(nt-notes<-line 4)
                      ,@(nt-notes<-line 7))))))

;;;; Roots

;; Notice that the CONTEXT does matter for root tests.
;; These cases can be made more granular down the road, but good enough atm.

(ert-deftest notes:relationships:roots:no-children ()
  (nt-test--with-context 'lispy "
(note foo
      bar)

(note foo
      bar)
"
    (-let ((notes
            (nt-test--mock-notes '(("note" "n")))))
      (should-equal (nt-notes->roots notes)
                    notes))))

(ert-deftest notes:relationships:roots:one-root-with-children ()
  (nt-test--with-context 'lispy "
(note foo
      (note foo
            bar)
      bar)
"
    (-let ((notes
            (nt-test--mock-notes '(("note" "n")))))
      (should-equal (nt-notes->roots notes)
                    `(,(car notes))))))

(ert-deftest notes:relationships:roots:no-duplicates ()
  ;; Under specific circumstances duplicate roots were present in past
  (nt-test--with-context 'lispy "
(note foo bar)

(note (note foo bar) bar)

(note foo bar)
"
    (-let ((notes
            (nt-test--mock-notes '(("note" "n")))))
      (should-equal (nt-notes->roots notes)
                    `(,(car notes)
                      ,(cadr notes)
                      ,(cadddr notes))))))

(ert-deftest notes:relationships:roots:full-complexity ()
  (nt-test--with-context 'lispy "
(note foo
      bar)

(note foo (note foo
                bar))

(note foo
      (note foo (note foo bar)
            (note foo
                  foo bar)
            bar)
      (note foo bar))
"
    (-let ((notes
            (nt-test--mock-notes '(("note" "n")))))
      (should-equal (nt-notes->roots notes)
                    `(,(car notes)
                      ,(cadr notes)
                      ,(cadddr notes))))))

;;; Management
;;;; Deletion
;;;;; Internal

(ert-deftest notes:management:deletion:internal ()
  (nt-test--with-context 'any
      "
(note foo
      bar)
"
    (-let (((note foo)
            (nt-test--mock-notes '(("note" "n") ("foo" "fo")))))

      (should-equal nt-notes
                    `(,note ,foo))

      (nt-note--delete-internal foo)
      (should-equal nt-notes
                    `(,note))

      (nt-note--delete-internal foo)  ; Check no error thrown
      (should-equal nt-notes
                    `(,note))

      (nt-note--delete-internal note)
      (should-not nt-notes))))

;;;;; Commands

;; tbd
