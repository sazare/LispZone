(load "test.lisp")
(load "pmem.lisp")
(load "pmem4.lisp")

(test-set
  "test for make-bel"
  (test-set
  	 " base"
  	 (make-bel '(hiro 9 jump))
  	 (test "ntruth" 9 (get 'hiro 'ntruth))
  	 (test "oppos" nil  (get 'hiro 'oppos))
  	 (test "class" nil (get 'hiro 'class))
  )
  (test-set
  	 " INN"
  	 (make-bel '(dalia 4 INN))
  	 (test "ntruth" 4 (get 'dalia 'ntruth))
  	 (test "class" 'INN (get 'dalia 'class))
  )
  (test-set
  	 " oppos"
  	 (make-bel '(becca 2 DOC *becca 8))
  	 (test "ntruth" 2 (get 'becca 'ntruth))
  	 (test "oppos of becca" '*becca (get 'becca 'oppos))
  	 (test "class of becca" nil (get 'becca 'class))
  	 (test "ntruth of oppos" 8 (get '*becca 'ntruth))
  	 (test "oppos os oppos" 'becca (get '*becca 'oppos))
  	 (test "class of oppos" nil (get '*becca 'class))
  )
)

(test-set
  "test for make-bels"
  (make-bels '((hiro2 5 jump) (dalia2 3 INN) (becca2 4 DOC *becca2 7)))
   (test "ntruth"          5        (get 'hiro2 'ntruth))
   (test "oppos"           nil      (get 'hiro2 'oppos))
   (test "class"           nil      (get 'hiro2 'class))
   (test "ntruth"          3        (get 'dalia2 'ntruth))
   (test "class"          'INN      (get 'dalia2 'class))
   (test "ntruth"          4        (get 'becca2 'ntruth))
   (test "oppos of becca"  '*becca2 (get 'becca2 'oppos))
   (test "class of becca"  nil      (get 'becca2 'class))
   (test "ntruth of oppos" 7        (get '*becca2 'ntruth))
   (test "oppos os oppos"  'becca2  (get '*becca2 'oppos))
   (test "class of oppos"  nil      (get '*becca2 'class))
  )
  
;(defvar *testbel* '((CEATB 2) @@4962))

;(defvar *testinf* '((TH2 (NOMONEY 2) @@0492)(EMOTE )(IF0001

(test-set
  "test for make-infs" 
  (test-set 
    "case TH2" 
    (make-infs '((TH2 (ABC 2) I1 I2) (TH2 (DDD 3) I1 I3) 
		  (EMOTE (FJUMP 0.9) L1)(EMOTE (FJUMP 0.4) L1 L2 L3)(EMOTE (AJUMP 0.9) L3)))
    (test "th2-i1" '((DDD 3) (ABC 2)) (get 'I1 'TH2))
    (test "th2-i2" '((ABC 2)) (get 'I2 'TH2))
    (test "th2-i3" '((DDD 3)) (get 'I3 'TH2))

    (make-infs '((TH2(BCD 2) ((MEASURE 1 2) KKK) FFF)))
    (test "th2-form" nil (get 'KKK 'TH2)) ;; not ((BCD 2)). When TH2, a list of form of cddr should ignored.
    (test "th2-form" '((BCD 2)) (get 'FFF 'TH2)) ;; this is usual
  )
  (test-set 
    "case EMOTE"

    (test "emo-L1" '((FJUMP 0.4)(FJUMP 0.9)) (get 'L1 'EMOTE))
    (test "emo-L2" '((FJUMP 0.4)) (get 'L2 'EMOTE))
    (test "emo-L3" '((AJUMP 0.9)(FJUMP 0.4)) (get 'L3 'EMOTE))


  )
  (test-set
    "case THEOREM"
    (make-bels '((BBB 2 HUM)(CCC 5 HUM)(DDD 9 HUM)))

    (make-infs '((IF110 BBB (@@123 CCC))(IF114 CCC (@@123))))
    (test "init theorem" '(BBB @@123 CCC) (get 'IF110 'THEOREM))
    (test "init theorem" '(CCC @@123) (get 'IF114 'THEOREM))
    (test "lambda gets" '(IF114 IF110) (get '@@123 'TH))
    (test "lambda gets" '(IF110) (get 'CCC 'TH))

    (make-infs '((IF111 (BBB 2) (CCC DDD (EQ 1 2))) (IF112 (BBB 3)(CCC EEE))))
    (test "theorem list 111" '((BBB 2) CCC DDD(EQ 1 2) ) (get 'IF111 'THEOREM))
    (test "theorem list 112" '((BBB 3) CCC EEE) (get 'IF112 'THEOREM))
    (test "theorem list th0" nil (get 'BBB 'TH))
    (test "theorem list th1" '(IF111 ) (get 'DDD 'TH))
    (test "theorem list th2" '(IF112 ) (get 'EEE 'TH))
    (test "theorem list th3" '(IF112 IF111 IF110) (get 'CCC 'TH))

    
    (test-set
      "theorem message"
      (setf (get 'AAA 'THEOREM) 'XXX)
;      (make-infs '((AAA (three 2))))
      (make-infs '((IF400 (CCC 1) '(@@EEE))(AAA (BBB 2) (CCC))))
      )
  )
)
