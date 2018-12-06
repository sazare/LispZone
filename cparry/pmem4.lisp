;; this from pmem4
;;;; stub

(defun pminitialize ())
(defun buffer (x) (format t "buffer:~a~%" x))
(defun nouud (x) x)


;;;;

;% THIS IS ALL THE TOP-LEVEL GOODIES PLUS INFERENCE %

(defun  init() ()); (selectinputn '(PAR BLF) INPUTFILE))

(defun  initfb() 
  (progn 
;    (EVAL '(DSKIN (quote RANDOM.LAP)))
    (setf LAMDA 8)
;    (initf)
;    (initb)
    )
  )

(defun  testm () (test_pattern))

;(defun  lambdaname (L) (equal (chrval L) LAMDA))
;;; lambdaname is defined in pmem.lisp

(defun parry ()
  "parry main function. dont direct trans."
  (prog ()
    (terpri t)
    (format t "READY:~%")
    (loop 
      (let ((ind nil))
	(unless (ERRSET (parry2 ind) nil)
          (paerror (format nil "~a TOP-LEVEL LISP EROR ~a " SSENT TRACE_MEM) !ANAPHLIST)
          (terpri)
          (printstr "WHAT DO YOU MEAN BY THAT")
        )
      )
    )
  (format t "Good bye")
  )
)


;; structure
;;; parry2 ->save_obj()
;;;        ->experiment()
;;;        ->testm()
;;;        ->choose('silence) when len(ssent)==1
;;;        -> analyze(T)
;;;        -> not lambdaname(a) = a=nil
;;;        -> when atom(a) a=get(a, 'meqv)
;;;        -> reactinput = a
;;;        -> window(9,t,a)
;;;        -> readlambda(a); window(9, nil, get(a,bondvalue)
;;;        -> react(list(a, q(ssent), ssent))
;;;        -> when ende {
;;;              tracev = not(tracev); 
;;;              modifvar() 
;;;              winxit();
;;;              swap()
;;;              exit()
;;;            }
;;; question
;;; 1. where are defined ssend, ende?
;;; 2. no definition of exit(). perhaps exit from parry()
;;; 3. where are defined prop and meqv
;;; 4. possibly important functions
;;;    - experiment() define a experimental code for parameter changeing
;;;    - lambdaname has a property 'meqv
;;;    - readlambda(), react(), 
;;;    - readlambda(), react(), 
;;;    - when ende is t is end, modifvar() seems not important

(defun parry2 (ind)
  (prog (a b)
    (list ind) ;; for error defined but not used
    (setf bug 0)
    (when SAVE_DUMP (savejob save_dump 'sav))
      ;; Program will start here again if system clashes

    (unless (not_last_input) 
      (terpri)
      (princ "READY:")
      )
    (setf bug 0)
    (experiment)
    (setf bug 1)
    (setf a (errset (testm) nil)) 
    (if (atom a) 
      (format t  "Pattern match error ~a" (list next_char ssent inputques))
      (err nil)
      )
    (setf bug 3)
    (setf a (car a))
    (setf PM2INPUT PMINPUT)
    (setf PMINPUT a)
    (if (= (length SSENT) 1)
      (setf a (choose 'silence)))
    (analyze t)
    (unless (lambdaname a) (setf a nil))
    (when (and a (atom a) (setf b (get a 'meqv)))
      (setf a b))
    (setf REACTINPUT a)
    (window 9 T a)
    (readlambda a) (window 9 nil (get a 'bondvalue))
    (setf bug 4)
    (unless (errset (react (list a (q ssent) ssent)))
      (paerror ssent " error in react")
      (err nil)
      )
    (setf bug 70)
    (when ende 
	(setf tracev (not tracev))
        (modifyvar)
	(winxit)
	(swapp)
	(exit)
	)
    (setf bug 80)
    )
  )

(defun initb ()
  (let ()
    (eight)
    (nouud T)
    (oparinitialize) ; initializes opar3
    (setupstl) ;; setup story
    (setf INPUTFILEAREA '(1 3))
    (setf DIAFILEAREA '(DIA KMC))
    (gcgag nil)
    (changel 'CHANGE)
    (setf DELNO (setf bug 0))
    (setf OLDMISS 0)
    (setf OLDGIBB 0)
    (setf INPUTNO 0)
    (setf REPEATNO 0)
    (setf SPECFNNO 0)
    (setf MISCNO 0)
    (setf NEWTOPICNO 0)
    (setf HLIST nil)
    (setf OLDTOPIC nil)
    (setf OLDTOPICS nil)
    )
  )

(defun reactprint (L) ;% FORMATS AND PRINTS THE OUTPUT TO TTY AND DIA FILE %
  (prog (SENT ISENT N)
	(setf SENT  L)
        ;%       SUPPRESS NON-VERBALS HERE       %
        (setf BUG  43)
	(when (and SUPPRESS SENT (not (ATOM (car SENT)))) (setf SENT (cdr SENT)))
	(setf SENT (IF SENT (STRINGATE SENT) " "))
	(TERPRI NIL)
	(PRINTSTR  SENT)
	(setf ISENT INPUTSSENT)
	(setf BUG 44)
	(setf ISENT (STRINGATE (IF (and ISENT (cdr ISENT))
				   (REVERSE ISENT) 
				   (car ISENT))))
	(setf N (list PMINPUT (GET REACTINPUT 'BONDVALUE) TRACE_MEM NEWPROVEN INTENT))
	(setf BUG 45)

	(when SAVE_FILE 
          (unless (ERRSET (TO_FILE ISENT SENT N) NIL)
	   (paerror DIACHARNO (format nil "ERROR IN TO_FILE ~a ~a" ISENT SENT))))
	(setf BUG 46)
	(setf INPUTSSENT NIL)
	)
  )

(defun pmin()  (pminitialize))


(defun dosf (L) ;% THIS DOES THE SEMANTIC FN (PROTECTED BY ERRSET) AND RETURNS A LAMBDA OUTPUT %
  (prog (a b)
	(IF (ATOM (setf B (ERRSET (IF (and (LAMBDANAME L)  
					   (setf A (GET L 'SF))
					   (setf A (EVAL A))) A) NIL)))
	    (paerror "BAD SF~a" (list L B))
	    (progn
	      (setf B (car B))
	      (unless (LAMBDANAME B) 
                (setf ?!OUTPUT B) 
                (setf B NIL))
	      (return B))
	    )
	)
  )

(defun chooselead() (nth (parandom 4) '(BOOKIESET GAMBLERSET HORSERACINGSET GANGSTERSET)))

(defun printvars ()
  (progn 
    (BUFFER T)(TERPRI NIL)
    (PRINC "      FEAR = ")(PRINTSTR (NUMED FEAR))
    (PRINC "     ANGER = ")(PRINTSTR (NUMED ANGER))
    (PRINC "     SHAME = ")(PRINTSTR (NUMED HURT))
    (BUFFER NIL)
    ))

(defun wprintvars()
  (prog (A) 
	(setf A (WINSUP T)) ;% SUPPRESS AND GET OLD SETTING %
	(WINDOW 41 T (format nil "      FEAR = ~a" (NUMED FEAR)))
        (WINDOW 41 T (format nil "     ANGER = ~a" (NUMED ANGER)))
        (WINDOW 41 T (format nil "     SHAME = ~a" (NUMED HURT)))
        (WINDOW 41 T "  " )
	(unless A (WINDIS)(WINSUP A)) ;%IF OLD SETTING OFF, THEN DISPLAY AND RESET %
	)
  )

;% ANGERFEARMODE  %
(defun not_last_input () (NOT (or (EQ NEXT_CHAR CR) (EQ NEXT_CHAR LF) )))

;% ANGERFEARMODE RESPONDS TO HIGH ANGER AND FEAR LEVELS IF NO SPECIFIC EMOTIONS ARE AFFECTED %
(defun angerfearmode (topic)
  (prog ()
    (when (or (memq topic (get 'FLARELIST 'SETS))
  	      (memq topic '(MAFIA BYE IYOUME STRONGFEELINGS FEELINGS GAMES))
	  )
          nil)
    (setf bug 14)
    (if (>= FEAR 14) (fearmode) (angermode))
  )
)

(defun angermode ()
  (if (>= ANGER 17.5) 
    (choose 'ANGER)
    (choose 'HOSTILEREPLIES)
    )
  )

(defun fearmode ()
  (cond ((>= FEAR 18.4) (setf ENDE t) (choose 'exit))
      ;DISTINGUISH BETWEEN QUESTIONS AND STATEMENTS OF 'OTHER 
        ((and (not (bl 'DDHARM))
	      (bl 'DHELPFUL)
	      (not (bl 'DMAFIA)))
	    (decf FEAR)
	    nil)
      ((eq STYLE 'Q) (choose 'THREATQ) (choose 'AFRAID))
    )
  )


;% TOPICANALYZE  RECORDS THE NUMBER OF OLD TOPICS, CHANGED TOPICS, AND THE PREVIOUS TOPIC %
(defun topicanalyze ()
  (prog ()
	(unless STOPIC (return nil))
        (when (memq STOPIC '(ANAPH FACTS STRONGFEELINGS GREETINGS)) (return nil))
        (when (eq STOPIC OLDTOPIC) (return nil))
        (setf NEWTOPICNO (+ NEWTOPICNO 1))
        (setf OLDTOPICS (cons OLDTOPIC OLDTOPICS))
        (setf OLDTOPIC STOPIC)
	)
  )

(defun prevtopic () (memq STOPIC OLDTOPICS))

;% HISTORY  RECORDS THE SPECIFIC EMOTION THAT WAS AFFECTED BY THIS INPUT %

(defun history(L)
  (prog ()
    (when L (return (memq L HLIST)))
    (setf HLIST nil)
    (when (and AJUMP (>= AJUMP 0.1)) (addh '(AJUMP MJUMP)))
    (when (and FJUMP (>= FJUMP 0.1)) (addh '(FJUMP MJUMP)))
    )
  )

(defun addh(L) (loop for i in L do (setf HLIST (CONS I HLIST))))

; % REACT2  CALLS REPLYR WITH APPROPRIATE ARGUMENTS AND ENTERS THE INPUT ON THE CONVERSATION LIST %

(defun react2 (B)
  (prog () ;(a) 
	(setf ?!EXHAUST nil)
        (unless (lambdaname B) 
	  (paerror "NONLAMBDA INTO REACT2" B)
	  (return nil))
	(when ?!OUTPUT ;    % ALREADY HAVE SENTENCE IN ?!OUTPUT %
          (andthen (list 'IN B))
	  (andthen (list 'OUT nil))
          (return T))
	(unless (diskread B) 
	  (paerror "REACT2 ERROR BAD DISKREAD " B)
	  (return nil))
	(andthen (list 'IN B))
	(return (replyr B))
	)
  )

;% REACT3  HANDLES CASES WHEN THE OUTPUT RESPONSES HAVE BEEN EXHAUSTED %
(defun react3 (P STRUC SENT) ;% IF NO !OUTPUT FROM REACT2 THEN DO THIS %
  (prog (A B)
	(setf B (CARN P))

	(unless ?!EXHAUST (paerror  "~a BAD INPUT IN REACT3" (list P STRUC SENT)))
	; %  REPETITIOUS INPUT AND EXHAUSTED REPLIES      %
        (when (and (eq TRACE_MEM 'OK) 
		   (not (eq STOPIC 'STRONGFEELINGS))
		   (REPETITION B 'IN))
	  (setf REPEATNO (+ REPEATNO 1))
	  (unless (GET B 'REPEAT) (setf A (GET_STORY) ));  %LET ONE REPEAT GO BY %
          (PUTPROP B T 'REPEAT)
	  (unless A (setf A (CHOOSE 'REPEAT) )))
	(when (and A (REACT2 A))(RETURN A))

	(setf A (GET_STORY))
	(when (and A (REACT2 A)) (RETURN A))
	(unless (setf A (EXHAUSTER)) (setf A (CHOOSE 'EXHAUST)))
	(when (and A (REACT2 A)) (RETURN A))
	)
  )

;% ********** REACT ******* TOP LEVEL ROUTINE OF THE MEMORY ************ %
(defun react (INPUT)
  (prog (A B SENT STRUC FOUND FOUND2)
	(setf BUG 10)
	(setf STRUC (CAR INPUT))(setf STYPE (CADR INPUT))(setf SENT (caddr INPUT))
	(setf INPUTSSENT (cons SSENT INPUTSSENT)) ;% SET ALL THE VARIABLES %
	(setf TRACE_MEM NIL) (setf INPUTNO (+ INPUTNO 1))
	(setf ?!ANAPHLISTNEW (setf ?!EXHAUST NIL))(setf ?!OUTPUT (setf WDFLAG NIL))(setf CHOSEN NIL)
	(when DOC_NAME_FLAG (setf DOCNAME (getdocname)))
	(setf BUG 11)
	(when (and (eq INPUTNO 2) (not ERRNAME))
	  (setf ERRNAME T) ;   % SAVE A POINTER TO THE DIA FILE %
	  (paerror (format nil (IF (PTYJOB) " PTYJOB~a" " ~a") 
			 (IF (not SAVE_FILE) " NSAVED" " ")) NIL)
	  )
	(setf BUG 12)
	(when (and STRUC (not (readlambda STRUC)))
	    (setf REACTTO (setf STRUC NIL))
	    (setf TRACE_MEM 'NOT_IN_MEMORY))
	(WINDOWSET 2)
	(WINDOW 51 T (GET STRUC 'TOPIC))
	(when (setf A (GET STRUC 'UNIT)) (WINDOW 52 T A))
	(setf BUG 13)

	(WINDOW 31 T 'PREPROCESS)
	(setf REACTTO (checkinput STRUC)) ;% PREPROCESS THE INPUT %
	(when (and REACTTO (not (readlambda REACTTO))) (setf REACTTO (setf STRUC NIL)))
	(setf BUG 14)

	(IF (and (not REACTTO) STRUC) 
              (or
                (when (setf REACTTO (SPECFN STRUC)) (setf TRACE_MEM 'SPECIALANAPH) ;% LOOK FOR ANAPHORA %
                        (setf SPECFNNO (+ SPECFNNO 1)))
		(when (setf REACTTO (MEMFIND STRUC)) (setf TRACE_MEM 'OK)) ;% LOOK UP NORMAL INPUT %
		))
	(when (eq REACTTO 'QUIT)(setf REACTTO NIL) (setf TRACE_MEM 'NOSPECIALANAPH))
	(unless (readlambda REACTTO) (setf REACTTO NIL))
	(setf BUG 15)
	(when (and (not REACTTO)(not DELFLAG)(setf REACTTO (skeywd STYPE INPUTQUES)))
	    (setf TRACE_MEM 'KEYWORD))

	(unless (readlambda REACTTO) (setf REACTTO NIL))

	(setf BUG 16)
	(ANALYZE T)

	(setf STOPIC (CARN (GET STRUC 'TOPIC)))
	(TOPICANALYZE) ; % ANALYZE THE CURRENT TOPIC %
	(WINDOW 31 T 'INFERENCES)
        (unless (ERRSET (INFERENCE) NIL) (paerror "INFERENCE ERROR" PROVEL))
	(WINDOW 31 T 'AFFECTS)
	(unless (ERRSET (AFFECT) NIL) (paerror "AFFECT ERROR" ACTION))
	(WINDOW 31 T 'INTENTIONS)
	(unless (ERRSET (setf FOUND (dointent)) NIL) (paerror "DOINTENT" INTENT))
	(IF FOUND (setf TRACE_MEM 'INTENT))
	(WINDOW 31 T 'ACTIONS)
	(setf BUG 17)
	(when (and (not FOUND) REACTTO) (setf FOUND REACTTO))

	; % IF THERE IS NOTHING IN FOUND, THEN WE HAVE TO PUNT AND TAKE A MISCELLANEOUS RESPONSE %

	(unless FOUND 
	  (IF (eq STYPE 'Q) (setf FOUND (miscq SENT)) (setf FOUND (miscs SENT)))
	  (setf MISCNO (+ MISCNO 1))
	  (when (and (not TRACE_MEM) FOUND) (setf TRACE_MEM 'NO_PATTERN)))
        (setf BUG 18)

        (unless (readlambda FOUND) (setf FOUND NIL))
	(setf BUG 20)

	(setf REACTTO FOUND)(setf B NIL)
	; % DO SEMANTIC FUNCTION, RESULT WILL BE EITHER @@NAME OR ACTUAL SENTENCE %
        (setf FOUND (IF (setf B (DOSF FOUND)) (IF (setf A (dosf B)) A B ) FOUND))
	(setf BUG 22)

        (when (not_last_input) (RETURN NIL))
	; % QUIT HERE IF THERE IS ANOTHER INPUT SENTENCE ON THE INPUT LINE %
	(ANALYZE T)

        (when (eq (CARN (GET FOUND 'TOPIC)) 'MAFIA) (setf DELNO (+ DELNO 1))); % RECORD NUMBER OF DELUSION STMTS %
	(REACT2 FOUND);  % GET THE ENGLISH SENTENCE INTO ?!OUTPUT %
	(setf BUG 30)

	(unless ?!OUTPUT (setf FOUND2 (REACT3 FOUND STRUC SENT))) ;% TRY AGAIN TO GET ENGLISH OUTPUT %

	(setf BUG 35)
	(setf ?!ANAPHLISTOLD ?!ANAPHLIST)(setf ?!ANAPHLIST ?!ANAPHLISTNEW) ;% UPDATE ANAPHLIST %

	(when (and ?!OUTPUT (car ?!OUTPUT)) ;% RESCAN OUTPUT FOR FLARE AND DELUSIONAL WORDS %
           (when (ATOM (ERRSET (ascan (canona ?!OUTPUT) NIL) NIL)) 
                   (paerror (format nil "ASCAN~a" ?!OUTPUT) FOUND)))
	(setf BUG 40)

	(setf ?!LAST_OUTPUT FOUND)
	(ANALYZE T);
	(setf BUG 42)
	(WINDOW 31 T 'OUTPUT)(setf PREV_OUTPUT ?!OUTPUT)
	(WINDOW 49 T (IF (ATOM (car ?!OUTPUT)) ?!OUTPUT (cdr ?!OUTPUT )))
	(when (ATOM (ERRSET (REACTPRINT ?!OUTPUT) NIL)) ;% PRINT OUTPUT TO TTY AND DIA FILE %
	  (paerror (format nil "REACTPRINT~a~a" INPUTSSENT ?!OUTPUT) FOUND))
	(setf INPUTSSENT NIL)
	(setf BUG 48)
	(IF (ATOM (ERRSET (progn
			    (HISTORY NIL)          ;% REMEMBER CURRENT THINGS FOR HISTORY %
			    (modifvar)(TERPRI NIL) ;% UPDATE EMOTION VARIABLES %
			    (IF WINDOWS (wprintvars))

			    ; %  UPDATE STORY LISTS %
			    (when (and (LAMBDANAME ?!LAST_OUTPUT) (setf A (GET ?!LAST_OUTPUT 'STORYNAME))) 
			      (deletep A ?!LAST_OUTPUT 'STORY))
			    (when (and (lambdaname FOUND2) (setf A (GET FOUND2 'STORYNAME)))
			      (deletep A FOUND2 'STORY))) NIL))
	    (paerror "ERROR FROM BOTTOM OF REACT" FOUND))

	(ANALYZE T)
	(setf BUG 50)
	(RETURN FOUND)
	)
  )

;% BINIT, READBEL, READINF %
(defun binit()
  (progn
;    (BAKGAG T) (NOUUO T)(TEN)
    (setf INTLIST (setf INTENT (setf PROVEN (setf PROVEL NIL))))
    (setf PRINTALL NIL)
    (readbel) ;;(READBEL 'BEL)
    (readinf) ;;(READINF 'INF)
    )
  )


;% FORMAT OF INPUT: ( <BELNAME> <NUMBER> <CLASS> <OPTIONAL OPPOS NAME> <NUMBER> ) %

(defvar *INTLIST* NIL)

(defvar *belfile* "data/bel0")
(defvar *inffile* "data/inf0")
;(defvar *inffile* "inf1")

(defun make-bel (bel)
  (let (B)
      (setf (get (car bel) 'NTRUTH) (cadr bel))
      (when (eq (caddr bel) 'INN) 
        (setf (get (car bel) 'CLASS) (caddr bel)))
      (when (setf B (cdddr bel))
          (setf (get (car B) 'NTRUTH) (cadr B))
	  (setf (get (car B) 'OPPOS) (car bel))
	  (setf (get (car bel) 'OPPOS) (car B))
	  )
     (car bel)
  )
)

(defun make-bels (bels)
  (let (pre)
    (loop for b in bels 
      collect 
        (progn 
          (setf pre (make-bel b))
          (push pre *INTLIST*)
	)
    )
    (setf *INTLIST* (reverse *INTLIST*))
    (terpri t)
    (format t "~%BELIEF file read, last belief: ~a" pre)(force-output t)
  )
 )

(defun read-bel ()
  (let (bel)
    (setf bel (read-file *belfile*))
    bel
    )
  )

(defun readbel ()
  (make-bels (read-bel))
  )


;% FORMAT OF INPUT: ( <INF NAME> <CONSEQ> <LIST OF ANTECEDENTS> ) %

(defun read-inf ()
  (let (inf)
    (setf inf (read-file *inffile*))
    inf
    )
  )

(defun make-infs (infs)
  (prog (b )
    (loop 
      for a in infs
      do
      (if (memq (car a) '(TH2 EMOTE))
	  (loop for i in (cddr a) do 
		(when (atom i) (setf (get i (car a)) (cons (cadr a) (get i (car a))))))
	  (prog ()
	    (setf b a)
	    (when (get (car a) 'theorem) 
		(format t "~%duplicate inf: ~a" (car a))(force-output t))
;	    (format t "(car a) = ~a~%" (car a))
	    (setf (get (car a) 'theorem) (cons (cadr a) (caddr a)))
;	    (format t "carn(cadr a) = ~a~%" (carn (cadr a)))
            (when (not (get (carn (cadr a)) 'ntruth)) 
		(format t "~%NO BEL: ~a" (cadr a))(force-output t))
            ;; backpointer from antecedent to th name
;	    (format t "caddr a=~a~%" (caddr a))
	    (loop for i in (caddr a) do
	      (when (atom i) 
		  (setf (get i 'TH) (cons (car a) (get i 'th)))
		  (when (and (not (lambdaname i)) 
				 (not (get i 'ntruth)))
			(format t "~%no BEL: ~a" i)(force-output t)))
	      )
	    )
	  )
      )
    (format t "~%INF file read, last inf: ~a~%" b) (force-output t)
    )
  )

(defun readinf ()
  (make-infs (read-inf))
  )



;;;% ASSERT, ASSERT2, ADDTO, PROVE, PROVE2, BL, EVALUATE, STATED, INFERENCE %

(defun posit (B) (IF (ATOM B) (ASSERT2 B) (ADDTO (car B)(cadr B))))

;% B IS A NAME OF A BELIEF %
(defun paassert (B) ;% ASSERT B, AND TRY TO PROVE ANY CONSEQUENCES OF IT %
        (prog2 T (prog2 (ASSERT2 B) (PROVE)))
	)

(defun assert2 (B) ;% ASSERT A NEW BELIEF, AND FIND ALL THEOREMS IN WHICH IT IS THE ANTECEDENT %
  (prog (A)
	(when (setf A (GET B 'EMOTE)) (INFEMOTE B A T))
	(setf A (GET B 'OPPOS))
	(when (GET B 'TRUTH) (RETURN T))
	(when (and A (GET A 'TRUTH)) ;% OPPOSITE BELIEF ALREADY TRUE %
	  (when PRINTALL (PRINTSTR (format nil "CONTRADICTION : TRYING TO ASSERT ~a" B) ))
	  (return T)
        )
        (WINDOW 37 NIL B)
	(PUTPROP B T 'TRUTH) ;% ASSERT %
	(when A (PUTPROP A NIL 'TRUTH)) ;% UNASSERT THE OPPOSITE BELIEF %
	(when (setf A (GET B 'TH)) (setf PROVEL (append PROVEL A))) ;% FIND THEOREMS %
	(setf PROVEN (cons B PROVEN)) ;% RECORD THIS BELIEF %
	(setf NEWPROVEN (cons B NEWPROVEN))
	)
  )


(defun addto (B N) ;% ADDTO A BELIEF, IF THRESHHOLD CROSSED, THEN ASSERT %
  (prog (A VAL)
	(when (setf A (GET B 'EMOTE)) (INFEMOTE B A N))
	(when (setf A (GET B 'TH2))
	  (loop for I in A do (ADDTO (IF (ATOM I) I (car I)) (/ N 2))))
	(setf A (GET B 'OPPOS))
	(when (or (GET B 'TRUTH) (and A (GET A 'TRUTH) )) (RETURN T)) ;% QUIT IF TRUE, OR OPPOSITE TRUE %
	(WINDOW 37 NIL (list B N))
	(setf VAL (+ N (GET0 B 'NTRUTH)))
	(when (eq 'INN (GET B 'CLASS))    ; % UPDATE VALUE %
          (IF (>= VAL 10) (setf VAL 9) (IF (<= VAL 0) (setf VAL 0) )) 
	  )
	(PUTPROP B VAL 'NTRUTH)
	(when (>= VAL 10) (ASSERT2 B)) ; % ASSERT IF THRESHHOLD CROSSED %
	)
  )

(defun get0 (I V) ;% RETURNS A NUMBER FOR GET %
  (prog (A)
	(setf A (GET I V))
	(return (IF (NUMBERP A) A 0))
	)
  )

(defun prove () ;% DO PROVE2 REPEATEDLY ON PROVEL, THE LIST OF THEOREMS TO BE PROVED %
  (loop while PROVEL do (prog2 (car PROVEL) (setf PROVEL (cdr PROVEL))))
  )

;% TH IS A THEOREM NAME FROM THE TOP OF PROVEL %
(defun prove2 (TH) ;% TRY TO PROVE A THEOREM TH %
  (prog (A B C)
	(when (= PRINTALL 2) (PRINTSTR PROVEL))
	(unless (setf A (GET TH 'THEOREM)) (RETURN NIL))
;          % IF BELIEF ALREADY PROVEN THEN DON'T DO IT  %
        (setf B (CARN (car A))) (when (BL B) (RETURN NIL))
	; % C IS THE CONSEQUENT, A IS A LIST OF ANTECEDENTS %
        (WINDOW 35 T A)
	(setf C (car A)) (setf B T)
	(loop for I In (cdr A) do
	      (setf B (and B  (EVALUATE I)))) ;% TRY EACH ANTECEDENT AND "AND" THEM TOGETHER %
	(when B 
	  (IF PRINTALL (PRINTSTR (format nil "~a PROVEN" C) ))
	  (POSIT C)
	  (WINDOW 36 T A))
	)
  )


(defun  evaluate (I) ;% EVALUATE AN ANTECEDENT "I" %
  (cond ((ATOM I) ;% EITHER AN INPUT WHICH IS STATED, OR A BELIEF WHICH IS TRUE %
          (if (LAMBDANAME I) (STATED I) (BL I)))
	((eq (car I) 'NOT) (NOT (BL (cadr I))))
	(T (EVAL I)) ) ;% ELSE A FUNCTION FOR LISP TO EVALUATE %
  )

(defun BL (b) ; % RETURNS T IF BELIEF B IS TRUE, OR INTENTION B IS OVER ITS THRESHHOLD %
  (if (not (atom b)) 
    (progn (paerror "BL not ATOM " b) NIL)
    (if (eq (get b 'class) 'INN) 
      (>= (get b 'ntruth) 5)
      (get b 'truth)
      )
    )
  )

(defun  STATED (I) (EQ REACTTO I))

;% THINK  TAKES ALL THE NEW FACTS FROM THIS INPUT AND PROVES ALL IT CAN ABOUT THE WORLD %
(defun inference()
  (prog (A)
	; SPECIAL PARA, SPECFNRA;
	(setf NEWPROVEN NIL)
	(setf PARA (GREATERP MISTRUST 7)) ;% PARANOID PARAMETER -- MAKES SOME BELIEFS POSSIBLE %
	(IF (eq STOPIC 'GREETINGS) (setf SPECFNNO (+ SPECFNNO 1)))
	(setf SPECFNRA (/ (* 100 SPECFNNO) INPUTNO))
        (setf PROVEL '(IF730 IF740 IF750 IF760 IF770 IF350 IF380 IF566 IF884  ;% TRY THESE EVERY TIME %
                ;%       IF205 IF210 %     
		IF225))
	(when DOC_NAME_FLAG (setf PROVEL (cons 'IF331 PROVEL)))
	(when (eq STOPIC 'MAFIA) (setf PROVEL (cons 'IF888 PROVEL)))
	(setf A (GET REACTTO 'TH) )  ;% THE INPUT MAY HAVE ASSOCIATED INFERENCES %
	(when A (setf PROVEL (append PROVEL A)))
	(when (setf A (GET REACTTO 'TH2)) (loop for I in A do (POSIT I)))
        (PROVE) ;% PROVE ALL THATS POSSIBLE %
	(when PRINTALL (PRINT PROVEN))
	)
  )

(format t "end of loading pmem4.lisp~%")

