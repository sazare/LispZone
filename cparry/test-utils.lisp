;(load "test.lisp")
;(load "utils.lisp")

(test-set
  "test for make-arc"
  (test "all atom" '((a b c)) (make-arc 'a 'b 'c)) 
  (test "all targets list" '((a b 1)(a b 2)(a b 3)) (make-arc 'a 'b '(1 2 3)))
  )

(test-set
  "test for make-arcs"
  (test "plist nil" () (make-arcs 'k ()))
  (test "plist variety" '((K A 12)(K B 1)(K B 2)(K B 3)) (make-arcs 'k '(A 12 B (1 2 3) C ((EQ 1 2) (EQ 2 2)))))
  )

(test-set
  "test for make-edge"
  (test "full" '((K A 12)(K B 1)(K B 2)(K B 3)) (make-edge '(K A 12 B (1 2 3) C ((EQ 1 2) (EQ 2 2)))))
)

(test-set
  "test for make-edges"
  (test "full" '((A B C)(K A 12)(K B 1)(K B 2)(K B 3))
	(make-edges '((A B C)(K A 12 B (1 2 3) C ((EQ 1 2) (EQ 2 2)))))
	)
  )

