;;; nt-[module]-test.el --- Tests -*- lexical-binding: t -*-

;; ~ TEST COVERAGE ~

;; Covered:
;; -

;; Implicitly Covered:
;; -

;; Not Covered:
;; -

;;; Commentary

;; File provides some simple templates for bootstrapping some test setup
;; useful generally.

;;; Load Test Helper

;; There has to be a cleaner way to load everything

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'nt)
       (add-to-list 'load-path (f-parent (f-this-file)))
       (require 'test-helper))

;;; Tests on 1 Note

(nt-describe "foo bar"
  :var ((text "
(note foo
      bar)
(foo bar)
")
        (notes '(("note" "n")))
        mocked-note)
  (before-all (setq mocked-note (car (nt-test--setup 'simple text notes))))
  (after-all (nt-test--teardown))

  )

;;; Tests on 2+ Notes

(nt-describe "foo bar"
  :var ((text "
(note1 note2
       note3)
(foo bar)
")
        (notes '(("note1" "n") ("note2" "n") ("note3" "n")))
        mocked-notes)

  (before-all (setq mocked-notes (nt-test--setup 'simple text notes)))
  (after-all (nt-test--teardown))

  )
