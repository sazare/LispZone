;(load "test.lisp")
;(load "pmem4.lisp")

(test-set "test for BL"
	  (test "not atom" NIL (bl '(a b c)))
	  (setf (get 'b1 'class) '(a b))
	  (setf (get 'b2 'class) 'th2)
	  (setf (get 'b2 'ntruth) 1)
	  (setf (get 'b31 'class) 'inn)
	  (setf (get 'b31 'ntruth) 6)
	  (setf (get 'b32 'class)  'inn)
	  (setf (get 'b32 'ntruth) 5)
	  (setf (get 'b33 'class) 'inn)
	  (setf (get 'b33 'ntruth) 4)
	  (setf (get 'b2 'truth)  t)
	  (setf (get 'b31 'truth) 4)
	  (setf (get 'b32 'truth) 5)
	  (setf (get 'b33 'truth) 6)

	  (test "class not inn" t (bl 'b2))
	  (test "inn < 5" nil (bl 'b33))
	  (test "inn = 5" t (bl 'b32))
	  (test "inn > 5" t (bl 'b31))
)


