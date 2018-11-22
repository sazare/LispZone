;(load "test.lisp")
;(load "loadall.lisp")

(test-set "numed"
  (test "0" "00.00" (numed 0.0) )
  (test "1.0" "01.00" (numed 1.0))
  (test "50.0" "50.01" (numed 50.0))
  (test "12.34" "12.35" (numed 12.34))
  (test "99.99" "100.00" (numed 99.99))
  )

(test-set "silencer"
  (setf SILENCENO 0)
  (setf ENDE nil)
  (setf AJUMP 0.5)
  (silencer)
  (test "1st" 1 SILENCENO)
  (test "1st" NIL ENDE)
  (test "1st" 0.1 AJUMP)
  (silencer)
  (silencer)
  (silencer)
  (silencer)
  (silencer)
  (silencer)
  (silencer)
  (silencer)
  (silencer)
  (silencer)
  (test "11st" 11 SILENCENO)
  (test "11st" T ENDE)
  (test "11st" 0.1 AJUMP)
  )

(test-set "exhauster"
  (setf EXHAUSTNO 0)
  (setf AJUMP 0.0)
  (setf ENDE nil)
  (exhauster)
  (test "exhaust 1st" t (and (null ende) (= EXHAUSTNO 1)(= ajump 0.15)))
  (exhauster)
  (exhauster)
  (exhauster)
  (exhauster)
  (exhauster)
  (exhauster)
  (exhauster)
  (test "exhaust 8th" t (and (= EXHAUSTNO 8)(= ajump 0.15)))
  (exhauster)
  (test "exhaust 9th" t (and ende (eq chosen 'MADEXIT)))
)

(test-set "swearer"
  (setf SWEARNO 0)
  (setf AJUMP 0.0)
  (setf CHOSEN 'NON)
  (setf ende nil)
  (setf swearno 0)
  (swearer)
  (test "swear 1st" T (and (null ende) (= swearno 1) (= ajump 0.3)(eq chosen 'NON)))
  (swearer)
  (swearer)
  (swearer)
  (swearer)
  (test "swear 5th" T (and ende (= swearno 5) (= ajump 0.3)(eq chosen 'MADEXIT)))
)


(test-set "q"
  (test "null stat" T (eq (q nil) 'D))
  (test "last qm" T (eq (q '(a b qm)) 'Q))
  (test "1st words" T (eq (q '(is this x)) 'Q))
  (test "1st words" T (eq (q '(are you x))'Q))
  (test "1st words" T (eq (q '(were you x)) 'Q))
  (test "1st words" T (eq (q '(am I x)) 'Q))
  (test "1st words" T (eq (q '(did you x)) 'Q))
  (test "1st words" T (eq (q '(does it x)) 'Q))
  (test "1st words" T (eq (q '(do you x)) 'Q))
  (test "1st words" T (eq (q '(have you x)) 'Q))
  (test "1st words" T (eq (q '(has he x)) 'Q))
  (test "1st words" T (eq (q '(had you x)) 'Q))
  (test "1st words" T (eq (q '(who are x)) 'Q))
  (test "1st words" T (eq (q '(whom it x)) 'Q))
  (test "1st words" T (eq (q '(what are you x)) 'Q))
  (test "1st words" T (eq (q '(when is it x)) 'Q))
  (test "1st words" T (eq (q '(where is there  x)) 'Q))
  (test "1st words" T (eq (q '(how do you do )) 'Q))
  (test "1st words" T (eq (q '(why did you x)) 'Q))
  (test "1st words" T (eq (q '(can I  x)) 'Q))
  (test "1st words" T (eq (q '(could you x)) 'Q))
  (test "1st words" T (eq (q '(would you   x)) 'Q))
  (test "1st words" T (eq (q '(should I  x)) 'Q))
  (test "1st words" T (eq (q '(will you  x)) 'Q))
  (test "1st words" T (eq (q '(may I  x)) 'Q))
  (test "1st words" T (eq (q '(I have x)) 'D))
  (test "1st words" T (eq (q '(You can  x)) 'D))
  (test "1st words" T (eq (q '(There are x)) 'D))
  (test "1st words" T (eq (q '(These are x)) 'D))
 ) 

