(load "test.lisp")

(deftest test-a ()
	 "about plus"
	 (test "three1" 3 (+ 1 2 ) )
	 (test "three2" 3 (- 4 1 ) )
	 (test "four" 9 (+ 2 2 ) )
	 (test "five" 9 (+ 4 2 ) )
	 )
(deftest test-b ()
	 "about multi"
	 (test "four"  4 (* 2 2) )
	 (test "two"   2 (* 1 1) )
	 )
(deftest test-car (x) ;; what this parameter's meaning?
	 "about car"
	 (test "car is abc1" 'abc (car x))
	 (test "car is abc2" 'bbb (car x))
	 (test "car is abc3" 'abc (car x))
	 )

(deftest test-all ()
	 (test-case "testset"
	   (test-a)
	   (test-b)
	   (test-car '(abc ss))
	   ))

(test-all)


