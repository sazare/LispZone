(load "test.lisp")
(load "diaapp.lisp")

(test-set
	 "test for carn"
	 (test "when atom" 'a (carn 'a)) 
	 (test "when list" 'a (carn '(a b c)))
	 )

