;; PMEM2 

(defun initf () ;% INITIALIZES GLOBAL VARIABLES %
  (setf EXHAUSTNO 0)
  (setf SILENCENO 0)
  (setf SWEARNO 0)
  (setf ?!ALLANAPHS '( (WHO ) ;% THEY HE SHE WE %  
                       (THEY  HE SHE WE)
                       (HE )  (SHE )  (WE )
                       (THERE WHERE)  (HERE THERE WHERE)  (WHERE)
                       (THEN WHEN)  (WHEN HOW_LONG)    (HOW_LONG)
                       (IT) (WHAT) (YOU_DO) (THEY_DO) (HOW_MUCH) (HOW_KNOW)
                       (GO_ON) (ELAB) (WHY) (HOW) (YES) (NO)  ))
)

(defun flaresent ()
;        % FLARESENT IS A SEMANTIC FUNCTION CALLED BY A FLARE INPUT %
;        % IT CALLS THE OLD PARRY ROUTINES FOR FLARE TOPICS %
;; % DEACTIVATE NEW FLARE WORDS AND COMPUTE FJUMP  %
  (prog ()
    (flareref INPUTQUES)
    (return nil)
  )
)

(defun delnsent()
;        % FLARESENT IS A SEMANTIC FUNCTION CALLED BY A FLARE INPUT %
;        % IT CALLS THE OLD PARRY ROUTINES FOR FLARE TOPICS %

  (prog (a b)
    (setf a (delcheck INPUTQUES))
    (setf b (delref a))
    (return b)
    )
  )

; % *** RAISE, PRINTVARS, WPRINTVARS %
(defun raise ()
  (progn
    (when HJUMP 
      (when WEAK (setf HJUMP (* 0.5 HJUMP)))
      (setf HURT (+ HURT (* HJUMP (- 20 HURT))))
      (setf MISTRUST (+ MISTRUST (* (* 0.5 HJUMP) (- 20 MISTRUST))))
      (setf MISTRUST0 (+ MISTRUST0 (* 0.1 HJUMP (- 20 - MISTRUST0))))
      (setf HURT0 (max (/ HURT 2) HURT0))
      ;; % SET HIGHER FLOOR ON FEAR AND ANGER DUE TO HURT %
      (setf FEAR0 (max FEAR0 (/ HURT0 2)))
      (setf FEAR (max FEAR FEAR0))
      (setf ANGER0 (max ANGER0 (/ HURT0 2)))
      (setf ANGER (max ANGER ANGER0))
      )
    (when FJUMP 
      (setf FJUMP (+ FJUMP (/ HURT 50))) ;;  % MAKES FEAR VOLATILE ON HIGH HURT %
      (when WEAK (setf FJUMP (* 0.3 FEAR)))
      (setf FEAR (+ FEAR (* FJUMP (- 20 MISTRUST))))
      (setf MISTRUST (+ MISTRUST (* 0.5 FJUMP) (- 20 MISTRUST)))
      (setf MISTRUST0 (+ MISTRUST0 (* 0.1 FJUMP (- 20 MISTRUST0)))) ;;OPERATOR PRED IS CORRECT?
      )
    (when AJUMP
      (setf AJUMP (+ AJUMP (/ HURT 50))) ;; % MAKES ANGER VOLATILE ON HIGH HURT %
      (when WEAK (setf AJUMP (* 0.7 ANGER)))
      (setf ANGER (+ ANGER (* AJUMP (- 20 ANGER))))
      (setf MISTRUST (+ MISTRUST (* (* 0.5 AJUMP) (- 20 MISTRUST))))
      (setf MISTRUST0 ( + MISTRUST0 (* 0.1 AJUMP (- 20 MISTRUST0))))
      )
    )
  )

(defun numed (n) ;; % 0.00 <= N <= 99.99, RETURNS STRING "12.34" %
  (nedit (fix (+ (* n 100) 0.5)))
  )

; % SKEYWD, KEYWD, SILENCER, EXHAUSTER, SWEARER, ENDROUTINE  %

; % SKEYWD CHECKS FOR FLARE AND DELN WORDS USING OLD PARRY ROUTINES %
; % SKEYWD AND KEYWD ARE ONLY USED WHEN NOTHING IS RECOGNIZED BY THE PATTERN MATCHER %

(defun skeywd (type sent)
  (prog (found r)
        ;;    % CHECK FOR DELN OR FLARE WDS IN INPUT %
        (when (and DELFLAG  (setf r (delcheck SENT)))
                (if (setf r (delref r)) 
		  (setf found R)
		  (setf found (DELSTMT))))
        (when (and (not found) (eq FLARE 'INIT) (setf R (flareref SENT)))
	  (setf found (flstmt r)))
        (unless found (setf found (keywd SENT SETLIST)))
        (unless found (setf found (SPECCONCEPT NIL)))
        (return found)
        )
  )

;% KEYWD CHECKS FOR KEY WORDS FROM SPECIAL TOPICS %
;% KEYWD IS ONLY USED IF THE PATTERN MATCHER FOUND NOTHING, AND THE NEW KEYWORD
;        IS ON THE SAME TOPIC AS THE PREVIOUS INPUT %

(defun keywd(inp setlist)
  (prog (result a)
    (loop for set in (get 'SETLIST 'SETS) do 
	  (loop for word in (get set 'words) do
		(when (assoc word inp) (setf result set)) 
		until result)
	  until result)
    (unless result (return nil))
    (setf set result)
    (unless (setf result (get result 'STORY))(return nil))
    (setf a (if (eq STOPIC 'ANAPH) 
	      OLDTOPIC 
	      STOPIC))
    (when (equal (synnym a) (synnym set)) (return (car result)))
    ;; % ONLY RETURN ANSWER IF TOPIC SAME AS PREVIOUS TOPIC %
    )
  )

(defun  silencer() ; % SEMANTIC FUNCTION CALLED BY SILENCE INPUT %
  (prog () 
    (setf SILENCENO (+ 1 SILENCENO)) 
    (when (= SILENCENO 11)  (setf ENDE  T))
    (setf AJUMP  0.1)
    (return nil)
    )
  )

(defun  exhauster () ; % SEMANTIC FUNCTION CALLED BY EXHAUST OUTPUT SELECTION %
  (prog ()
    (setf EXHAUSTNO (+ 1  EXHAUSTNO))
    (setf AJUMP 0.15)
    (when (= EXHAUSTNO 9) 
      (setf ENDE T)
      (return (CHOOSE 'MADEXIT))
      )
    )
  )

(defun  swearer() ; % SEMANTIC FUNCTION CALLED BY SWEAR INPUT %
  (prog ()
    (setf SWEARNO (+ 1  SWEARNO))
    (setf AJUMP  0.3)
    (when (= SWEARNO 5)
      (setf ENDE T)
      (return (choose 'MADEXIT))
      )
    )
  )

(defun  endroutine() ; % SEMANTIC FUNCTION CALLED BY EXIT OUTPUT SELECTION %
  (prog ()
    (setf ENDE  T)
    (when (and (>= FEAR 18.4) (or DELFLAG  (eq  FLARE 'INIT)))
      (setf AJUMP  0.1)
      (return (choose 'BYEOFF)))
    (return (CHOOSE 'BYE))
    )
  )

; % Q, CANONA, MEMFIND, INITPARAMS, STRINGATE, ANALYZE    %

(defun  Q(L) ; % RETURNS T IF THE INPUT L IS A QUESTION %
  (if (not L) 'D 
    (if (eq (car (last L)) 'QM) 'Q
      (if (member (car L) '(IS ARE WAS WERE AM DID DOES DO HAVE HAS HAD
                WHO WHOM WHAT WHEN WHERE HOW WHY CAN COULD WOULD SHOULD WILL MAY ))
        'Q 
	'D)
      )
    )
  )

;% CANONIZE CANONIZES L USING THE PATTERN MATCHER %
; % USED FOR RUNNING THE OUTPUT BACK THRU AN INPUT SCAN FOR DELUSIONAL WORDS %
(defun  canona(L)
  (prog (a b c) 
    (setf a INPUTQUES)
    (setf c DO_SPELL)
    (setf DO_SPELL nil)
    (setf b (CANONIZE (if (and L (not (atom (car L)))) (cdr L) L)))
    (setf b INPUTQUES)
    (setf DO_SPELL c)
    (setf INPUTQUES a)
    (return b)
    )
  )

(defun  MEMFIND(struc)  struc)

(defun initparams () ; % INITIALIZED PROGRAM PARAMETERS %
  (prog (a)
    ;; SPECIAL NOTSAVED;
    (setf EOF PERCENT)
    (terpri nil)
    (printstr "END INPUT PARAMETERS WITH CARRIAGE RETURN OR ALTMODE")
    (terpri nil)
    (printstr "PRINT NON VERBAL FEATURE? [Y,N]")
    (setf a (read))
    (when (or (equal a NOTSAVED) (eq a 'FILE))
      (setf SUPPRESS nil)
      (setf ANGER 0)
      (setf ANGER0 0)
      (setf FEAR 0) 
      (setf FEAR0 0)
      (setf MISTRUST 0) 
      (setf MISTRUST0 0) 
      (setf HURT 0) 
      (setf HURT0 0) 
      (setf TRACEV T)
      (setf SAVE_FILE  NIL)
      (INITFN NIL)
      (progn (printstr "WINDOWS? ") (setf WINDOWS (eq (READ) 'Y) ))
      (when (eq  A 'FILE) 
	(printstr "FILE=") 
	(setf A (READ))
	(setf INPUTFILE A)
	(BILLP) )
      (terpri nil) 
      (return nil)
     )
     (setf EXPERIMENT a)
     (setf A (chrval A))
     (setf SUPPRESS (not (or (eq A (chrval 'Y)) (eq A (chrval 'y) )))) 
     (terpri nil)
     (printstr "VERSION [WEAK, MILD, STRONG]")
     (setf a (charval (read)))
     (if (or (eq A (CHRVAL 'W)) (eq A (CHRVAL 'w)))
        (progn (setf WEAK  T) (setf VERSION 'WEAK))
        (setf ANGER (setf ANGER0 (setf FEAR (setf FEAR0 
	 (setf MISTRUST (setf MISTRUST0 (setf HURT (setf HURT0 
	  (if (or (eq A (CHRVAL 'S)) (eq A (CHRVAL 's))) 
	    (progn (setf VERSION 'STRONG) 5)
	    (progn (setf VERSION 'MILD) 0))
	  ))))))))
	)
      (when (and (ddjob) (not SUMEX))
	(terpri nil)
        (printstr "DISPLAY WINDOWS? [Y,N] ")
	(setf A (CHRVAL (READ)))
        (setf WINDOWS (or (eq A (CHRVAL 'Y)) (eq A (CHRVAL 'y) )))
	)
      (when (and  TRACEVFLAG (not WINDOWS))
	(terpri nil)
	(printstr "TRACE INTERNAL PROCESSES? [Y,N] ")
	(setf A (chrval (READ)))
	(when (or (eq A (CHRVAL 'Y)) (eq A (CHRVAL 'y)))  (setf TRACEV 'ALL))
	(when TRACEV (printstr "APPROX 15 LINES OF OUTPUT PER I/O PAIR"))
	)

      (when (and (not TRACEV) (not WINDOWS))
	(terpri nil) 
	(printstr "TRACE EMOTION VARIABLES? [Y,N]")
        (setf  A (CHRVAL (READ)))
        (when (or (eq A (CHRVAL 'Y)) (eq A (CHRVAL 'y))) (setf TRACEV T))
	)
      (when (and (not SUMEX)(not PTYJOB))
	(terpri nil) 
	(printstr "DO YOU WANT THE CORE DUMPED? [Y,N]") ;  % FOR SAVING CORE IF SYSTEM CRASHES %
	(if (eq (READ) 'Y)
	  (progn 
	    (terpri nil) 
	    (printstr "NAME FOR THE DUMP FILE?[6 CHARS]") 
	    (setf A (READ))
	    (setf SAVE_DUMP  A) 
	    (setf  A (EXPLODE A))
	    (if (and (eq (car A) 'H) (eq (cadr A) 'A)  (eq (caddr A) 'R))
	      NIL 
	      (setf SAVE_DUMP NIL))
	    )
	  (setf SAVE_DUMP NIL)
	)
       )
      (setf SAVE_FILE T)
      (INITPARAMS2)
    )
  )

(defun  initparams2()
  (let ()
    (printstr " 
END INPUT WITH A PERIOD OR QUESTION MARK, 
   FOLLOWED BY CARRIAGE RETURN. 
TO INDICATE SILENCE, TYPE   .   
   WHEN FINISHED, TYPE   BYE. 
USE PERIODS ONLY AT THE ENDS OF SENTENCES,
   NOT IN ABBREVIATIONS.
")
    (when  (PTYJOB) (printstr "
IF YOU ARE NOT AT STANFORD, YOUR BACKSPACE OR RUBOUT KEY 
   PROBABLY DOESNT WORK CORRECTLY.
")
    )
  )
 )

(defun  stringate(l) ;% RETURNS A STRING WITH THE QUOTE MARKS, FASTER THAN STR %
  (prog (a b)
    (setf a (explodec L))
    (setf a (cdr (stringate2 a)))
    (setf a (cons '\" a))
    (setf b (readlist a))
    (return b)
    )
  )

(defun  stringate2(a) 
  (cond ((null a) nil)
	((null (cdr a)) (list '\?))
	(t (cons (car a) (stringate2 (cdr a) ))))
  )

(defun  analyze(flag) ; % FOR ANALYZING TIME AND GARBAGE COLLECTION %
        ; SPECIAL OLDTIME, OLDSPEAK, ANALFLAG; 
  (prog (a b)
    (unless flag 
      (setf OLDTIME (time))
      (setf OLDSPEAK (speak))
      (setf ANALFLAG NIL)
      (RETURN nil)
      )
    (unless ANALFLAG (RETURN nil))
    (setf a (time))
    (setf b (car (divide (* 10 (- A OLDTIME)) 166 )))
    (princ b) (printstr " TICS") (setf  OLDTIME a)
    (setf a (speak))(setf b OLDSPEAK)
    (PRINC b) (printstr " CONSES")(setf OLDSPEAK a)
    )
  )

;%       GET_DATE, GET_TIME, GETDOCNAME  %

(defun  date(n)
  (prog (a b c yr mo date day)
    (setf A (dateuu))
    (ten) (setf A (+ A N))  ; % -1 IS YESTERDAY, 0 TODAY, + 1 TOMORROW %
    (setf B (divide A 31)) (setf date (cdr (+ 1 B))) 
    (setf B (divide (car B) 12))(setf mo (cdr (+ 1 B)))
    (setf YR (car (+ B 1964)))
    (setf C '((31 .JANUARY)(28 .FEBRUARY)(31 .MARCH)(30 .APRIL)(31 .MAY)(30 .JUNE)
         (31 .JULY)(31 .AUGUST)(30 .SEPTEMBER)(31 .OCTOBER)(30 .NOVEMBER)(31 .DECEMBER)))
    (setf A 0)
    (loop for i from 1 to (- MO 1) do (setf A (+ A (car (nth i C)))))
    (loop for i from 1973 to YR do (setf A (+ A 365)))
    (setf a (+ a date -1))
    (setf a (+ (cdr (DIVIDE A 7)) 1)) ; % MINUS 2 AND PLUS 1 TO ORIENT FOR 1973 AND BEYOND%
	  ; % PREVIOUS LINE SHOULD BE FIXED ON FEB 29, 1976 %
    (setf b '(SUNDAY MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY))
    (setf day (nth a b))
    (setf mo (cdr (nth mo c)))
    (return (list yr mo date day))
    )
  )

(defun  getdocname()
  (prog (a b c doc test name)
    (setf a SSENT)
    (setf a (DELETE 'PD a))(setf a (DELETE 'COMMA a))
    (setf b a)
    (loop do 
	  (when (and (eq (car b) 'MY) (eq (cadr b) 'NAME) (eq (caddr b) 'IS))
	    (setf c (cddr b)))
	    until (or c (not (setf b (cdr b))))
	    )
    (setf b a)
    (when (not c)
      (loop do 
	    (when (and (eq (car b) 'I)(eq (cadr b) 'AM) 
		       (or (eq (caddr b) 'DR)
			   (eq (caddr b) 'DOCTOR)))
	      (setf c (cdddr b))
	      (setf doc T))
	      until (or c (not (setf b(cdr b)))))
      )
    (setf b a)
    (unless c 
      (loop do 
	    (when (and (or (eq (car b) 'I\'m ) (eq (car b) 'IM))
		       (or (eq (cadr b) 'DR)
			   (eq (cadr b) 'DOCTOR)))
	      (setf c (cddr b))
	      (setf doc T))
	    until (or c (not (setf b (cdr b))))))
    (when (or (eq c 'DR) (eq c 'DOCTOR)) (setf doc T) (setf c (cdr c))); somethin wierd
    (unless c (return nil))
    (setf name (list (car c)))
    (when (cdr c) 
      (setf test (cadr c))
      (unless (canona (list test)) (setf name (list (car c) (cadr c)))))
    (when doc (setf name (const 'DOCTOR name)))
    (return name)
    )
  )

(defun  get_date(a n) ;% FORMS THE OUTPUT SENTENCE FOR YR,MONTH,DAY,DATE %
  (prog (b)
    (setf b (date n))
    (unless b (return '(I DON\'T PLAY GAMES)))
    (when (or (eq a 'YEAR)(eq a 'MONTH)) 
      (return (append  '(THE YEAR IS) (list (car b) '\;)  
		       '(THE MONTH IS) (list (cadr b)) ))
      (when (eq a 'DATE)
	(return (append '(TODAY IS) (list (cadr b) (list (caddr b) '\,) '(I THINK) ))))
      (when (eq a 'DAY) (return (append '(IT\'S) (list (cadddr b)))))
      )
    )
  )

(defun  get_date_arb2 (n) ;% DECIDES WHETHER YEAR,MONTH,DAY,DATE, OR TIME REQUESTED %
  (prog (a) 
    (setf a (assoc 'DAY INPUTQUES))
    (unless a (setf a (assoc 'DATE INPUTQUES)))
    (when (and a (memq (cdr a) '(YEAR MONTH DAY DATE)))
      (return (get_date (cdr A) N)))
    (unless a (setf a (assoc 'WHEN INPUTQUES)))
    (when (and a (or (eq (cdr a) 'TIME)(eq (car a) 'WHEN)))
      (return (get_time)))
    )
  )


(defun  get_date_arb() (get_date_arb2 0))  ; % DATE,TIME FOR TODAY %
(defun  get_date_yes() (get_date_arb2 -1)) ; % DATE,TIME FOR YESTERDAY %
(defun  get_date_tom() (get_date_arb2 1))  ; % DATE,TIME FOR TOMORROW %

(defun  get_date_arb()  (get_date_arb2 0))  ; % DATE,TIME FOR TODAY %
(defun  get_date_yes()  (get_date_arb2 -1)) ; % DATE,TIME FOR YESTERDAY %
(defun  get_date_tom()  (get_date_arb2 1))  ; % DATE,TIME FOR TOMORROW %

(defun  ptyjob() (greaterp (lsh (lsh (ttyuu) 6) -35) 0)); % RETURNS T IF A PTYJOB %
(defun  ddjob()  (greaterp (lsh (lsh (ttyuu) 4) -35) 0)); % RETURNS T IF A DD JOB %

(defun  get_time();        % LOOKS UP SYSTEM TIME AND GETS THE APPROX HOUR %
  (prog (a hour min ahour)
    (setf a (timeuuh))
    (setf hour (car A))
    (setf min (cadr A))
    (setf ahour (+ hour (car (divide min 30))))
    (when (>= ahour 13)
      (setf ahour (- ahour 12))
      (when (= ahour 0) (setf ahour 12))
      (TEN)
      (return (append '(IT\'S ABOUT) (list ahour " O" ) '(CLOCK) )) 
      )
    )
  )

(defun  timeuuh()
  (prog (a)
    (setf A (car (divide (timeuu) 3600)))
    (setf A (divide A 60))
    (return (list (car a) (cdr a))); % THIS RETURNS (HOUR MINUTE) %
    )
  )

(defun  specconcept (L)
  ; % USED FOR GENERAL IYOUME INPUT WHICH THE PATTERN MATCHER DIDNT RECOGNIZE %
  (prog (con you neg found adj inp)
      (setf inp INPUTQUES)
      (loop for word in inp do
	  (when (member (get (car word) 'SET) SENSITIVELIST) (setf con (car word)))
	  until con)
    (unless con (return nil))
    (when (assoc 'YOU inp) (setf you T))
    (setf neg NOT_FLAG)
    (if (assoc 'GOOD inp) 
	(setf adj 'GOOD)
	(when (or (assoc 'BAD inp)(assoc 'ODD INP)) (setf adj 'BAD)))
    (cond
      ((and you adj) (setf found (if (or (and (eq adj 'GOOD) (not neg)) (and neg (eq adj 'BAD)))
		    (choose 'POSADJ) 
		    (choose 'NEGADJ)
		    )))
      ((and YOU (or (get CON 'SPECIAL) ADJ)) (setf found (choose 'SPECCONCEP)))
      (t (setf found (choose 'SENSITIVELIST))))
    (return found)
    )
  )

;% LASTWORD CHANGES L INTO THE APPROPRIATE ENGLISH WORD TO ADD TO THE END OF THE OUTPUT %

(defun  lastword (L)
  (let (a w)
    (setf w L)
    (when (eq W 'SENSITIVELIST)
      (setf W T)
      (loop for i in (get 'SENSITIVELIST 'WORDS) do
	    (when (setf a (assoc i INPUTQUES)) 
	      (setf a (get (car a) 'SET))) 
	    until a))
    (when (eq w 'COMPLEMENT) 
      (setf W T)
      (when (setf A (assoc 'GOOD INPUTQUES))
	(setf A (cdr A)))
      )
    (when (eq w 'SPEC_CONCEPT) 
      (setf W T) 
      (if (setf a (assoc 'LOOKS INPUTQUES)) 
	  (setf A (car (GET (car A) 'WORDS)))
	  (setf A 'LOOKS)))
    (setf a (cond
	      (a (list a))
	      ((or (eq w T)(not w)) '(PROBLEMS))
	      ((atom w) (list w))
	      (t w)))
    a
    )
  )

(format t "end of loading pmem2.lisp~%")

