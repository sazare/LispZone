(load "test.lisp")
(load "readdb.lisp")

(deftest test-bel ()
	 "base"
	 (make-bel '(hiro 9 jump))
	 (test "ntruth" (equal (get 'hiro 'ntruth) 9))
	 (test "oppos" (null (get 'hiro 'oppos)))
	 (test "class" (null (get 'hiro 'class)))
)
(deftest test-bel ()
	 "INN"
	 (make-bel '(dalia 4 INN))
	 (test "ntruth" (equal (get 'dalia 'ntruth) 4))
	 (test "class" (equal (get 'dalia 'class) 'INN))
)
(deftest test-bel ()
	 "oppos"
	 (make-bel '(becca 2 DOC *becca 8))
	 (test "ntruth" (equal (get 'becca 'ntruth) 2))
	 (test "oppos of becca" (equal (get 'becca 'oppos) '*becca))
	 (test "class of becca" (null (get 'becca 'class)))
	 (test "ntruth of oppos" (equal (get '*becca 'ntruth) 8))
	 (test "oppos os oppos" (equal (get '*becca 'oppos) 'becca))
	 (test "class of oppos" (null (get '*becca 'class)))
)

(test-bel)

