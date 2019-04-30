;;; nt-bounds-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; - General
;;   - comments/strings
;; - Lisps
;;   - Form opener?

;; Not Covered:
;; - Alot (restricting to simple cases atm until change functions finished)

;;; Load Files

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'nt-test))

;;; General

(nt-describe "Bounds generally applicable checks"
  :var ((text "
(foo ; skip-comment
    \"skip-string\"
     baz)
")
        (notes '(("skip-comment" "n") ("skip-string" "n")))
        mocked-notes)

  (before-all (setq mocked-notes (nt-test--setup 'lispy text notes))
              (setq skip-comment (car mocked-notes))
              (setq skip-string (cadr mocked-notes)))
  (after-all (nt-test--teardown))

  (it "strings"
    (expect (nt-bounds?--in-string-or-comment? skip-string)))
  (it "comments"
    (expect (nt-bounds?--in-string-or-comment? skip-comment))))

;;; Lisps

(nt-describe "Lispy bounds checks"
  :var ((text "
(note1 note2
       baz)
")
        (notes '(("note1" "n") ("note2" "n")))
        mocked-notes)

  (before-all (setq mocked-notes (nt-test--setup 'lispy text notes))
              (setq mocked-note-1 (car mocked-notes))
              (setq mocked-note-2 (cadr mocked-notes)))
  (after-all (nt-test--teardown))

  (it "form openers"
    (expect (nt-bounds?--lisps-form-opener? mocked-note-1))
    (expect (nt-bounds?--lisps-form-opener? mocked-note-2) :nil)))

;;; Lisp Integration Tests

(nt-describe "Lispy bounds - integration tests"
  :var ((mocked-notes))

  (nt-describe "single form cases"
    :var ((text "
(note note2
      note3
      foo

      bar)

(note4  ; foo
 foo
 bar)
")
          (notes '(("note" "n") ("note2" "n") ("note3" "n") ("note4" "n")))
          (bound 6)
          mocked-note-1 mocked-note-2 mocked-note-3 mocked-note-4)

    (before-all (setq mocked-notes (nt-test--setup 'lispy text notes))
                (setq mocked-note-1 (car mocked-notes))
                (setq mocked-note-2 (cadr mocked-notes))
                (setq mocked-note-3 (caddr mocked-notes))
                (setq mocked-note-4 (cadddr mocked-notes)))
    (after-all (nt-test--teardown))

    (describe text
      (describe "bounds"
        (it "start bounds form"
          (expect (nt-bound mocked-note-1) :to-equal bound))

        (it "not at start bounds form"
          (expect (nt-bound mocked-note-2) :to-equal bound))

        (it "not on same line bounds form"
          (expect (nt-bound mocked-note-3) :to-equal bound)))

      (describe "predicates"
        (it "opener effects indent"
          (expect (nt-bounds?--lisps-form-opener? mocked-note-1))
          (expect (nt-bound? mocked-note-1)))

        (it "non-opening sexp first line doesnt modify indent"
          (expect (nt-bounds?--lisps-form-opener? mocked-note-2) :nil)
          (expect (nt-bound? mocked-note-2) :nil))

        (it "other line sexps dont modify indent"
          (expect (nt-bounds?--lisps-form-opener? mocked-note-3) :nil)
          (expect (nt-bound? mocked-note-3) :nil))

        (it "opener that is a terminal sexp does not effect indent"
          (expect (nt-bounds?--lisps-terminal-sexp? mocked-note-4))
          (expect (nt-bound? mocked-note-4) :nil))))))

;;; Generalized Integration Tests

(xnt-describe "Generalized bounds - integration tests"
  :var ((mocked-notes))

  (nt-describe "single form"
    :var ((text "
(note note2
      note3
      foo

      bar)
")
          (notes '(("note" "n") ("note2" "n") ("note3" "n")))
          (bound 6)
          mocked-note-1 mocked-note-2 mocked-note-3)

    (before-all (setq mocked-notes (nt-test--setup 'generalized text notes))
                (setq mocked-note-1 (car mocked-notes))
                (setq mocked-note-2 (cadr mocked-notes))
                (setq mocked-note-3 (caddr mocked-notes)))
    (after-all (nt-test--teardown))

    (nt-describe text
      (it "start bounds form"
        (expect (nt-bound mocked-note-1) :to-equal bound))
      (it "not at start bounds form"
        (expect (nt-bound mocked-note-2) :to-equal bound))
      (it "not on same line bounds form"
        (expect (nt-bound mocked-note-3) :to-equal bound))

      (it "opener effects indent"
        (expect (nt-bound? mocked-note-1)))
      (it "non-openeing sexp first line doesnt modify indent"
        (expect (nt-bound? mocked-note-2) :nil))
      (it "other line sexps dont modify indent"
        (expect (nt-bound? mocked-note-3) :nil))))
  )
