;% THIS FILE HAS FUNCTIONS FOR THE WINDOW FEATURE      %

;;; stub
(defun begin (x) x)
(defun winsup (x) x)
(defun winopn (x y z w h) (list x y z w h))
(defun winlbl (x y) (list x y))
(defun windis (&rest x) x)
(defun winsiz (x y) (list x y))
(defun winsil (x &rest y) (list x y))
(defun winrel (x) x)
(defun winxit ())1
(defun strp (x) x)
(defun str (x) x)
(defun intest ())
(defun winout (x y z) (list x y z))

;;;;

(defun PMCLR()
  (prog ()
	(WINSUP T)
	(loop for I from 1 to 29 do 
	      (unless (= I 9) (WINREL I)))
	(WINDIS) (WINSUP NIL)
	)
  )

(defun MMCLR ()
  (prog ()
	(WINSUP T)
	(loop for i from 30 to 64 do (WINREL I))
	(WINDIS) ( WINSUP NIL)
	)
  )

(defun WINDOW (N FLAG L) 
"this is trial window"
  (format t "stub window(~a ~a ~a)~%"
            N FLAG L)
)

;(defun WINDOW (N FLAG L)
;  (prog (M)
;	(when (eq TRACEV 'ALL) (TWINDOW N FLAG L) (RETURN L))
;	(unless WINDOWS (RETURN L))
;	(setf M (IF (STRP L) L (STR L)))
;	(unless (SPECWIN N) (INTEST))
;	(WINOUT N FLAG M)
;	(when (or (= N 9) (= N 49)) (WINSUP NIL) (WINDIS))
;	(return L)
;	)
;  )

(defun SPECWIN(N)(or  (= N 1) (= N 31)))

(defun WININIT()
  (format t "stub wininit()~%")

;(defun WININIT()
;  (progn 
;    (setf WINDOWS  T)
;    (WINHELP) (WINDOW 2 T NIL)
;    (WINXIT) (WINSIL 30) (WINSIZ 4 2) (WINDIS)
;    (setf TRACEV NIL)
;    )
;  )
)

(defun WINHELP()
 	(PRINTSTR
"NEW EASIER WINDOW COMMANDS:
    ESC I   IS A BINARY SWITCH TO HALT AND CONTINUE PROCESSING 
   YOU ARE NOW HALTED. TYPE  ESC I  TO CONTINUE.  " )
   )

(defun WINDOWSET(N)
  (prog ()
    (unless WINDOWS (RETURN NIL))
    (when (= N 1) (MMCLR) (PMWIN))
    (when (= N 2) (SLEEP 1) (PMCLR)
      (MMWIN2)(WINDOW 32 T PMINPUT) (WINDOW 32 NIL (GET PMINPUT 'BONDVALUE))
      (WINDIS)(WINREL 9)(WINDIS)
      (MMWIN)
      )
    )
  )


(defun TWINDOW (N FLAG L) ;% PRINTS OUT WINDOW STUFF FOR A TELETYPE %
  (prog (A)
	(setf A (ASSOC  N '(	( 2 . "Input:	" )
			( 3 . "Respelled:  " )
			( 4 . "Canonical form:  " )
			( 5 . "Segmented:  " )
			( 7 . "Simple patterns:  " )
			( 9 . "Result:  " )
			( 33 . "Preprocess:  " )
			( 36 . "Inferences succeeded:  " )
			( 37 . "New beliefs:  " )
			( 40 . "Emotions:  " )
			( 42 . "Intentions:  " )
			( 44 . "Action:  " )		) ))
	(when (and A (or FLAG (= N 36) (= N 42))) (PRINC (CDR A)) (PRINTSTR L))
	)
  )

;% INITIALIZING WINDOWS %

(defun PMWIN()
  (prog (SENTL)
	(WINSUP T)
	(setf SENTL 76 )(TEN)
	(WINSIL 30)
	(WINSIZ 4 2)

	(WINOPN 1 9 0 16 3)
	(WINLBL 1 'PROCESS)
	(WINOPN 2 0 3 SENTL 3)
	(WINLBL 2 "INPUT SENTENCE")
	(WINDIS T)

	(WINOPN 11 12 6 16 3)
	(WINLBL 11 "WORD")
	(WINOPN 12 42 6 22 3)
	(WINLBL 12 "RESPELLED")
	(WINOPN 3 0 9 SENTL 3)
	(WINLBL 3 "RESPELLED INPUT")
	(WINOPN 13  6 12 16 3)
	(WINLBL 13 "WORD")
	(WINOPN 14 27 12 22 3)
	(WINLBL 14 "IDIOMS FOUND")
	(WINOPN 15 54 12 16 3)
	(WINLBL 15 "ANAPHS FOUND")

	(WINOPN 4 0 15 SENTL 3)
	(WINLBL 4 "CANONIZED INPUT")
	(WINOPN 5 0 18 SENTL 3)
	(WINLBL 5 "SEGMENTED INPUT")

	(WINOPN 16 5 21 33 3)
	(WINLBL 16 "SIMPLE PATTERN")
	(WINOPN 17 42 21 33 3)
	(WINLBL 17 "PATTERNS TRIED")
	(WINOPN 7 3 24 60 3)
	(WINLBL 7 "MATCHED SIMPLE PATTERNS")
	(WINOPN 8 0 27 30 3)
	(WINLBL 8 "COMPOUND PATTERN")
	(WINDIS )

	(WINOPN 9 37 27 38 3)
	(WINLBL 9 "INPUT RECOGNIZED")
	(WINDIS )

	(WINSUP NIL)
	)
  )

(defun MMWIN()
  (prog (SENTL HSENTL)
	(WINSUP T)
	(setf SENTL 76)
	(setf HSENTL 36)
	(TEN)

	(WINOPN 51 41 3 19 3)
	(WINLBL 51 "TOPIC")
	(WINOPN 52 62 3 14 3)
	(WINLBL 52 "ANAPHORA")

	(WINOPN 33 4 6 (- SENTL 8) 3)
	(WINLBL 33 "PREPROCESS RESULTS")

	(WINOPN 35 0 9 HSENTL 6)
	(WINLBL 35 "INFERENCES TRIED")
	(WINOPN 36 38 9 HSENTL 6)
	(WINLBL 36 "INFERENCES SUCCEEDED")
	(WINOPN 37 0 15 (+ HSENTL 17) 4)
	(WINLBL 37 "NEW BELIEFS")

	(WINOPN 40  5 19 43 5)
	(WINLBL 40 "AFFECT CHANGES")
	(WINOPN 41 55 17 22 6)
	(WINLBL 41 "CURRENT AFFECTS")
	(WINOPN 42 0 24 (+ HSENTL 17) 3)
	(WINLBL 42 "INTENTIONS")

	(WINOPN 44 58 24 16 3)
	(WINLBL 44 "ACTIONS")

	(WINDIS )

	(WINOPN 49 0 27 SENTL 3)
	(WINLBL 49 "OUTPUT SENTENCES")

	(WINDIS )
	(WINSUP NIL)
	)
	)

(defun MMWIN2();
	(BEGIN  (WINSUP T))
	(WINOPN 31 9 0 16 3)
	(WINLBL 31 'PROCESS)
	(WINOPN 32 0 3 38 3)
	(WINLBL 32 "INPUT STRUCTURE")
	)

