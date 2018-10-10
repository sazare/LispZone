(load "test.lisp")
(load "pmem.lisp")

(deftest test-lambdaname ()
	 "lambdaname"
	 (test "lambdaname is @@<atom>" t (lambdaname '@@abc)) 
	 (test "lambdaname is @@<atom>" t (lambdaname '@@ab)) 
	 (test "lambdaname is @@<atom>" t (lambdaname '@@a)) 
	 (test "not lambdaname " nil (lambdaname 'abc))
;	 (test "not lambdaname " nil (lambdaname 'ab))
;	 (test "not lambdaname " nil (lambdaname 'a))
;	 (test "not lambdaname " nil (lambdaname '@a))
;	 (test "not lambdaname " nil (lambdaname '@@))
	 )

(test-lambdaname)

