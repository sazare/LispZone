(test-set "member3"
 
  (test "case atom"  nil (member3 'b '((a . b) (c . d))) )

  (test "atom and not hit"  nil (member3 'c '((a . b) (c . d))) )
  (setf (get 'd 'USED) T)
  (test "atom and hit"  '(c . d) (member3 'c '((a . b) (c . d))) )

  (setf (get 'y 'USED) T)
  (test "case cons" '(a . y)  (member3 '(x a) '((H . L)(A . x)(B . y)(C . z)) )

  (setf (get 'x 'USED) T)
  (test "case cons" '(a . y)  (member3 '(x a) '((H . L)(A . x)(B . y)(C . z)) )
  )
