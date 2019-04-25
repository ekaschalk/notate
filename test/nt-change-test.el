;; CURRENTLY ENUMERATING CASES

;; slup-unbalanced - deleting a (
;; barf-unbalanced - deleting a )

;;; Deletion (single-line)
;;;; Slurp-Unbalanced

;; CASES:
;; 1 (note bar
;; 2       X
;; 3       (foo note foo
;; 4            foo)
;; 5       bar)

;; CHANGE: Delete the "(" under X
;; RESULT: Mask 5 has note removed
;; IMPL:
;; 1. Extract root containing line of deletion, if such a root exists.
;; 2. Update bounds of notes contained in root up until X.
;; 3. For each note captured:
;;      Delete note in masks in [bound last-bound)
;;      Add note in masks in [last-bound bound)

;;;; Barf-Unbalanced

;; CASES:
;; (note bar
;;       (note foo
;;                X
;;             bar)
;;       (foo note foo
;;            foo)
;;       bar)
;; =>
;; (note bar
;;       (note foo
;;                X
;;             bar
;;             (foo note foo
;;                  foo)
;;             bar)

;; CHANGE: Delete the ")" under X
;; RESULT: 2nd note added to masks up until final line
;; IMPL:
;; 1. Extract root containing line of deletion, if such a root exists.
;; 2. From root up until smallest child s.t. bound < line of deletion:
;;    Update bounds of notes contained in root up until X.
;; then what...

;;;; Barf-Unbalanced that eats roots
;;;;; Hungry

;; Also possible deletion makes a new root.
;; Though, since I'm not storing roots as it is, this might not be necessary
;; to think about in a special way.

;; But, I'm using 'nt-last-bound to find the roots right. So why am I not
;; storing the roots?

;; CASE:
;; (note foo
;;          X
;;       bar)
;; (note foo
;;       bar)
;; (note foo
;;       bar)

;; CHANGE: Delete the ")" under X
;; RESULT:
;; Same as before but we are checking the first-note-after-the-root==next-root
;; In that case - we eat the next root... and possibly more roots afterwards...

;;;;; not that hungry

;; CASE:

;; CASE:
;; (note foo
;;          X
;;       bar)
;; (note foo
;;       bar))
;; (note foo
;;       bar)

;; CHANGE: Delete the ")" under X
;; RESULT:
;; We only eat the first root this time.


;; Deleting the second "(" also causes root eating

;;; GENERAL NOTES

;; In
;; (note foo
;;          X
;;       bar)
;; (note foo
;;       bar))

;; When we delete ")", the boundary checking will fail because we haven't
;; indented that line yet!

;; maybe it has to be tied to indent here...
;; maybe I mark the line(s) visually that it will be rendered when it can...
;;   though this requires calling indent-line...
;;   or does it? if I eat the root, I might already know the lines to mark...

;; What does marking mean?
;; Maybe - the first space we insert will actually insert mask-size spaces!
;; now the line wont "collapse" when it has enough space to render
;; when spaces are being added...
