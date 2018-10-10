(load "test.lisp")
(load "diaapp.lisp")

(deftest test-carn ()
	 "carn"
	 (test "when atom" 'a (carn 'a)) 
	 (test "when list" 'a (carn '(a b c)))
	 )

(test-carn)

