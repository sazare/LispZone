(load "test.lisp")
(load "readdb.lisp")

(deftest test-bel-base ()
	 "base"
	 (make-bel '(hiro 9 jump))
	 (test "ntruth" 9 (get 'hiro 'ntruth))
	 (test "oppos" nil  (get 'hiro 'oppos))
	 (test "class" nil (get 'hiro 'class))
)
(deftest test-bel-inn ()
	 "INN"
	 (make-bel '(dalia 4 INN))
	 (test "ntruth" 4 (get 'dalia 'ntruth))
	 (test "class" 'INN (get 'dalia 'class))
)
(deftest test-bel-oppos ()
	 "oppos"
	 (make-bel '(becca 2 DOC *becca 8))
	 (test "ntruth" 2 (get 'becca 'ntruth))
	 (test "oppos of becca" '*becca (get 'becca 'oppos))
	 (test "class of becca" nil (get 'becca 'class))
	 (test "ntruth of oppos" 8 (get '*becca 'ntruth))
	 (test "oppos os oppos" 'becca (get '*becca 'oppos))
	 (test "class of oppos" nil (get '*becca 'class))
)

(deftest test-bel ()
	 (test-case "test for reading BEL"
	   (test-bel-base)
	   (test-bel-inn)
	   (test-bel-oppos)
	 )
)

(test-bel)


