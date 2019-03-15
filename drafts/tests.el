;;; Scratch

;; (defun nt-test--mock-tree-note (string replacement)
;;   "Mock notes for STRING to REPLACEMENT."
;;   (save-excursion
;;     (goto-char (point-min))

;;     (let ((rx (nt-note--string->rx string))
;;           notes)
;;       (while (re-search-forward rx nil 'noerror)
;;         (push (nt-note--init string replacement) notes))
;;       (prog1 notes
;;         (nt-tree--buildup notes)))))

;; (defun nt-test--mock-tree (string-replacement-alist)
;;   "Construct mocked `nt-tree', ret notes as /list/ sorted by /buffer position/.

;; Now we can destructure notes for test-cases, nicer than always searching first."
;;   (->> string-replacement-alist
;;      (-mapcat (-applify #'nt-test--mock-tree-note))
;;      (-sort (-on #'< #'overlay-start))))

;; Below variations not used yet, undecided on including
;; (defmacro should/= (f1 f2) `(should (/= ,f1 ,f2)))
;; (defmacro should-eq (f1 f2) `(should (eq ,f1 ,f2)))
;; (defmacro should-neq (f1 f2) `(should-not (eq ,f1 ,f2)))
;; (defmacro should-size= (coll1 coll2) `(should= (length ,coll1) (length ,coll2)))
