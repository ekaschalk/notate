;; CURRENTLY ENUMERATING CASES

;; slup-unbalanced - deleting a (
;; barf-unbalanced - deleting a )

;;; Deletion
;;;; Slurp-Unbalanced

;; CASES:
;; 1 (note bar
;; 2       X
;; 3       (foo note foo
;; 4            foo)
;; 5       bar)

;; CHANGE: Delete the "(" under X
;; RESULT: Mask 5 has note removed
;; IMPL: Extract root containing X.
;;       Update bounds of notes contained in root up until X.

;;;; Barf-Unbalanced
