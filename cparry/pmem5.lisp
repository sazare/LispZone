;; pmem5.lisp


;;;;

(defun experiment ()
  ;; EXPERIMENTS OF RAISING AND LOWERING SHAME
  (prog(a)
   (list a) ;; dont used
   (when (or (not EXPERIMENT) (not (eq EXPERIMENT 'SEVEN))) (return nil))
   (when (= 7 INPUTNO) (setf HURT (+ HURT 5)))
   (when (= 17 INPUTNO) (setf HURT (- HURT 5)))
  )
)

;(defun paerror (mess L) ;; I dont want to use this. error_file go out of bound
;  (setf ?!ERROR (cons (error_file (list mess L PM2INPUT PMINPUT FILE1 BUG)) ?!ERROR))
;  )

(defun allowrun ()
  (prog (a)
	(setf a (if SUMEX (sumexallow) (allow)))
	(unless a (swapp))     ;  % NOT FROM SCHEDULER %
	(unless SUMEX (namep)) ; NAME THIS PROGRAM PARRY %
	(if (and (not SUMEX) (= A 9)) (setf ONEDIA nil) (setf ONEDIA T))
	(if (<= a 2) (setf LOWMAN T) (setf LOWMAN nil))
	(if (or (= a 1) (= a 3)) (setf TRACEVFLAG nil) (setf TRACEVFLAG T))
	)
  )

(defun sumexallow ()
  (prog (filchan flag status)
    (setf filchan (eval (list 'INPUT DIAFILEAREA 'QPARRY)))
    (inc filchan nil)
    (setf status (read))
    (setf flag (read))
    (when (eq status 'OK) (inc nil T) (return flag))
    (inc nil nil)
    (eval (list 'OUTPUT DIAFILEAREA 'QPARRY))
    (outc T nil) (print 0) (print flag) (outc nil T) ; %RESET FILE %
    (when (= status 0) (sleep 10)(exit) (car nil)) ;;; ?? (car nil) make nothing.
    (return status)
    )
  )

(defun measuere (L M) ; % USED IN INFERENCES TO MEASURE QUANTITIES %
  (if (and (numberp L) (numberp M)) (greaterp L M) (EQ L M)))

(defun inf (a) ;% INITIALIZATION ROUTINE %
  (prog (b va)
	(allowrun)
	(setf INPUTFILE a) (setf LAMDA b) (setf NOTSAVED 'NDIA)
	(initfn 'EXIT)
	(analyze NIL)
	(gcgag nil)
	(runtim nil)
	(setf va nil) ;% VA IS FOR STARTING A PARRY WITHOUT INITIAL QUESTIONS %
	(unless va (initparams) (init))
	(when va (initparams1) (initparams2) (init))
	(when SAVE_FILE 
	  (INIT_FILE (format nil 
			       "~a , TRACEV = ~a, WINDOWS = ~a, PARANOIA = ~a"
			       (cond (va 'VA)(T "")) TRACEV WINDOWS 
                               (cond (WEAK 'WEAK) ((= HURT 0) 'MILD) (T 'STRONG) )
		        )))
        (when WINDOWS (WININIT))
	(parry)
	)
  )

(defun gn () (progn (gcgag nil) (initfn 'GN2))) 
(defun gn2 () (inf 'PDATZ)) ; % THE ROUTINE THAT INITFN STARTS UP %

(defun initparams1() ; % INITIALIZE PARAMS FOR VA INTERVIEWS %
  (let ()
    (setf SUPPRESS T)
    (setf WEAK  nil)
    (setf ANGER 0)
    (setf ANGER0 0)
    (setf FEAR 0)
    (setf FEAR0 0)
    (setf MISTRUST 0)
    (setf MISTRUST0 0)
    (setf HURT 0)
    (setf HURT0 0)
    (setf TRACEV nil)
    (setf SAVE_FILE T)
    )
  )

(defun timestat () ;% THIS KEEPS THE TIME FOR NET AND NONNET JOBS %
  (list (if LOWMAN 'NET 'NONNET) (runtim T)))

(defun anddo (L M) M L)

;(defun window (N F L)
;  (prog ()
;	(when (eq TRACEV 'ALL)
;	  (twindow N F L) 
;	  (return L))
;	)
;  )
;(defun windowsset (n) n)
;(defun window_print (A B C D) (list a b c d))

;(defun twindow (N flag L) ;  % PRINTS OUT WINDOW STUFF FOR A TELETYPE %
;  (prog (a)
;	(setf a (assoc  N 
;			'(( 2 . "Input:   " )
;			  ( 3 . "Respelled:  " )
;			  ( 4 . "Canonical form:  " )
;			  ( 5 . "Segmented:  " )
;			  ( 7 . "Simple patterns:  " )
;			  ( 9 . "Result:  " )
;			  ( 33 . "Preprocess:  " )
;			  ( 36 . "Inferences succeeded:  " )
;			  ( 37 . "New beliefs:  " )
;			  ( 40 . "Emotions:  " )
;			  ( 42 . "Intentions:  " )
;			  ( 44 . "Action:  " )) ))
;	(when (and a (or flag (= n 9) (= n 36) (= n 42)))
;	  (princ (cdr a))
;	  (printstr L))
;	)
;  )

(defun sumex1 ()
  ;  % THIS ROUTINE SETS UP A WORKING VERSION OF PARRY WHICH WILL ALMOST RUN ON SUMEX %
  (prog ()
	(initfn nil)
	(synnym 'a)   ; % READS IN SYNNYM.PAR %
	(spat '((A))) ; % READS IN SPATS.PAR %
	(cpat '((A))) ; % READS IN CPATS.PAR %
	(dskloc 'A)   ; % READS IN PDATX.PAR %
        (setf ONEDIA  T) (setf SUMEX T)(setf TRACEVFLAG nil)
        (putprop 'SWAPP '(LAMBDA NIL T) 'EXPR)
        (putprop 'WINXIT '(LAMBDA NIL T) 'EXPR)
        (putprop 'PPNUU '(LAMBDA NIL 0) 'EXPR)
        (putprop 'TTYUU '(LAMBDA NIL 0) 'EXPR)
        (initfn 'GN2)
	)
  )

(defun sumex2 ()
  (setf DIAFILEAREA (setf INPUTFILEAREA (list (READLIST '(F A U)) (READLIST '(G H T) ))))
  )

;%  CHECKINPUT  LOOKS THRU INPUT FOR SWEARING, INSULTS, NEGATIVE WORDS %

(defun checkinput (L) ;% RETURNS ONLY IMPORTANT CHANGED INPUT %
  (prog (a b c d)
	(setf BADINPUT nil)
        (when (or (and LOWMAN (>= INPUTNO 20) (>= (runtim T) 60000) )
		  (and (>= INPUTNO 30) (not (MEMSIZEOK)) (and (PROG2 (GC) T)(not (MEMSIZEOK) ))))
	  (setf A (choose 'TIRED))
	  (setf B '(INTERVIEW HAS BEEN LONG ENOUGH))
	  (addto 'PEXIT2 10)
	  (setf ENDE T)
	  (paerror (format nil "INPUTNO= ~a  SHORT OF SPACE ~a FS, FW ~a"
			   INPUTNO (length (NUMVAL 13)) (length (NUMVAL 14))) "")
	  )

        (when  (and (not A) (setf B (GET L 'FX)) (ERRSET (setf B (EVAL B)) NIL)) (setf A B)) 
	; % FX HAS A FEW KLUDGES TO NOT ALLOW SOME INPUTS %
        (when (and L (not (get L 'UNIT)) (not A )) (setf A 'DONE))
        (when (and (not A) (ASSOC 'SHIT INPUTQUES))
	  (setf A (choose 'SWEARING)) 
	  (setf BADINPUT T)
	  (setf B 'EXPLETIVES))
        (when (and (not A) (or (ASSOC 'CRAZY INPUTQUES)
			       (ASSOC 'BAD INPUTQUES) 
			       (ASSOC 'ODD INPUTQUES)))
                (setf BADINPUT T) 
                (setf B '(BAD ASSOCIATIONS WITH INPUT WORDS)))
        (setf C (- GIBBERISH OLDGIBB))(setf D (length SSENT))
	;; some >= may be wrong
	(when (and (not A) 
                   (or (and (>= C 5) (<= D 15))   ;;om  ^\ is <= 
                       (and (>= C 3) (<= D 7))
                       (and (>= C 2) (<= D 3))))
	  (setf A (choose 'GIBBERISH))            ;; gibberish -- nonsence
	  (setf B '(TOO MANY UNRECOGNIZED WORDS)))

        (when (>= GIBBERISH 20) (setf DO_SPELL nil))
	(when (and (not A) (>= MISSPELLED 6) (>= (- MISSPELLED OLDMISS) 3));; misspell many, just recently
	  (setf A (CHOOSE 'MISSPELLED))
          (setf B '(TOO MANY MISSPELLED WORDS)))
	(cond ((equal SSENT (append PREV_OUTPUT '(PD))) (ADDTO 'PGAMES 5)) ;; PD is period. ??
	      ((and PREV_SSENT  (eq (car PREV_SSENT) 'TWICE) (equal (cadr PREV_SSENT) SSENT)) (addto 'PGAMES 5))
	      ((equal PREV_SSENT SSENT) (setf PREV_SSENT (list 'TWICE SSENT)))
	      (t (setf PREV_SSENT SSENT)))
	(setf OLDGIBB GIBBERISH)
	(setf OLDMISS MISSPELLED)
	(unless b (setf b 'NORMAL))
	(window 33 T B)
        (when (eq A 'DONE)(setf A nil))
        (return A)   ;  % WILL RETURN NIL IF ANAPH OR NORMAL %
	)
  )

;% THE FOLLOWING ARE SEMANTIC FUNCTIONS WHICH ARE CALLED FROM THE MEMORY BY
;  SPECIFIC INPUT REFERENCES %

(defun apology ()
  (prog ()
    (if (>= MISTRUST 9) (setf AJUMP 0.2)  (setf ANGER (- ANGER 1)))
    (if (and (not (BL 'DHOSTILE)) (BL 'DDKNOW)) 
	    (return (CHOOSE 'SORRY))
            (return (CHOOSE 'ACCUSE)))
  )
)

(defun helper ()
;;  (when (or (bl 'DHOSTILE) (bl '?*DHELPFUL) (bl 'DDHARM))
  (when (or (bl 'DHOSTILE) (bl '*DHELPFUL) (bl 'DDHARM))
    (choose 'CAUTION))
  )

(defun knower ()
  (cond ((or (BL 'DDHARM)(BL 'DHOSTILE)(BL '?*DDHELP)) (choose 'HOSTILEREPLIES))
        ((or (BL '?*DTRUSTWORTHY) (BL '?*DHONEST)) (CHOOSE '?*DHONEST))
        ((BL 'DDHELP)(CHOOSE 'DDHELP))
        ((BL 'DEXCITED)(CHOOSE 'DEXCITED))
        ((BL '?*DINITIATING)(CHOOSE 'DBAD))
	(T nil))
)

(defun leadin ()
  (progn 
    (setf ?!ANAPHLIST (setf ?!ANAPHLISTOLD nil))
    (cond (DELFLAG (delstmt))
	  ((nequal FLARE 'INIT) (flstmt (get FLARE 'SET)))
	  ((eq INTENT 'PINTERACT) (choose 'UPSET))
	  )
    )
  )

(defun aloof()
  (when (or (BL 'DHOSTILE) (BL '?*DHELPFUL)(BL 'DDHARM)) (CHOOSE 'ALOOF))
  )

(defun aloof2()
  (when (or (BL 'DHOSTILE)(BL '?*DHELPFUL)(BL 'DDHARM)) (CHOOSE 'ALOOF2))
  )

(defun namecheck()
  (prog (a)
	(when (and (not (setf a (assoc 'NAME INPUTQUES))) 
                   (setf A (assoc 'YOU INPUTQUES))
		   (eq (cdr A) 'YOUR))
	  (return (CHOOSE 'DONTREMEMBER))
	  )
	)
  )

(defun memsizeok()
    T  ;;om  anyway T in this translation. memory should be enough.
;  (and (>= (length (NUMVAL 13)) 1500) (>= (LENGTH (NUMVAL 14) 300)))
  )

(defun opinion ()
  (prog (a)
	(cond ((or (bl 'DDHARM)(BL 'DHOSTILE)(BL '?*DDHELP)) (setf a (CHOOSE 'HOSTILEREPLIES)))
	      ((or (BL '?*DTRUSTWORTHY)(BL '?*DHONEST)) (setf a (CHOOSE '?*DHONEST)))
	      (T (loop for i in '(DABNORMAL DEXCITED DRATIONAL DHELPFUL DSOCIABLE) 
                do (when (BL i) (setf A (CHOOSE i)))
		until a )))
        (return a)
	)
  )

(defun selffeeling ()
  (cond ((>= ANGER 10) (choose 'ANGRY))
	((>= FEAR 10) (choose 'FEARFUL))
	((bl 'INTHELPFUL) (choose 'GOOD)))
  )

(defun interview ()
  (cond ((bl 'INTBAD) (choose 'INTBAD))
	((bl 'INTHELPFUL) (choose 'PRAISE)))
  )

; % AFFECT, INTENTION, AND INTENTION ROUTINES %
; % AFFECT EXPRESSES EMOTIONS BASED ON THE INPUT RECOMMENDATION, THE CURRENT VALUES,
;          THE TOPIC, AND CURRENT BELIEFS %

(defun affect ()
  (prog (a) ; b)
        (setf ACTION NIL)
	(setf INTENT NIL)
        (when (member (get STOPIC 'SET) SENSITIVELIST) (setf AJUMP  0.2))
        (raise)
        (when (or (>= FEAR 18) (>= ANGER 18.8)) (addto 'PEXIT2 10))

;  % ** THE TEST FOR ACTIVATING THE PARANOID MODE ******* %
        (cond ((or (and (eq VERSION 'STRONG) (or (>= HURT 7) (and HJUMP (>= HJUMP 0.1)) ))
		   (and (eq VERSION 'MILD)  (>= HURT 8)))
	       (anddo (addto 'PPARANOIA 5) (anddo (PARANOIA) (setf INTENT 'PPARANOIA))))
	      ((or (and FJUMP (>= FJUMP 0.01))  
		   (and AJUMP (>= AJUMP 0.01))  
		   (>= FEAR 14) 
                   (>= ANGER 14) 
		   (eq STOPIC 'STRONGFEELINGS) 
		   ACTION) 
	       (anddo (addto 'PSTRONGFEEL 5)
		      (setf INTENT 'PSTRONGFEEL)))
	      ) 
	(loop for I in '((FJUMP . FEAR)(AJUMP . ANGER)(HJUMP . SHAME))
	      do
	      (when (eval (car I)) (WINDOW 40 T 
					   (append (list (cdr I) 'RAISED) 
						   (when (setf a (GET (car I) 'INF))
						     (anddo  (list 'FROM a) (putprop (car I) NIL 'INF)) ) ))
		))
        (when (and (not FJUMP) (not AJUMP) (not HJUMP)) (window 40 T '(NO CHANGE)))
        (when WINDOWS (WPRINTVARS))
        (return ACTION)
	)
  )

(defun infemote (BEL L VAL) ;% L LOOKS LIKE ((HJUMP 0.5) ... )  %
  (loop for a in L do
	(prog (b c d)
          (setf B (car A))
	  (setf C (cadr A))
          (when (eq B 'HJUMP) (setf PARBEL (cons BEL PARBEL)))
	  (when (and (eq B 'HJUMP) WEAK ) (setf C (/ C 2))) ;% IF WEAK PARANOIA THEN DONT LET HJUMP BE STRONG %
          (when (NUMBERP VAL) (setf C (/ C 2))) ;% THIS CAME FROM ADDTO, NOT ASSERT -- WEAKEN THE EFFECT %
          (setf D (ZERONIL (EVAL B)))
          (when (>= C D) (PUTPROP B BEL 'INF))
	  (set b (max d c))
	)
     )
  )

(defun zeronil (L) (if (not L) 0 L))

(defun intention () ;% CALCULATES THE CURRENT INTENTION %
  (prog (a)
	(loop for i in INTLIST do 
	      (when (>= (GET0 I 'NTRUTH) 5) (setf A (WINDOW 42  NIL I)))
	      (when (or (not INTENT)(eq A 'PEXIT)(eq A 'PEXIT2))
			(setf INTENT A))
	      (WINDOW 42 NIL (format nil " : ~a" INTENT)))
	)
  )

;% DOINTENT  PERFORMS THE CURRENT INTENTION, CALCULATES AN ACTION, AND RETURNS A ^H %
;%       CHECKS INTENTS, RETURNS NEW VALUE FOR FOUND IN REACT, ELSE NIL %

(defun dointent ()
  (prog (I a)
	(intention)
	(when PRINTALL (PRINT INTENT) (TERPRI NIL))
	(setf I INTENT)
	(setf a (GET INTENT 'TH))
	(setf PROVEL (append PROVEL A))
	(PROVE)
	(setf A (ERRSET (if I (eval (list I)) NIL)))
        (if (atom A)
	  (progn (paerror  "IN DOINTENT BAD FN" I)  
		 (setf A NIL))
	  (setf A (car A)))

        (setf OLDINTENT INTENT)
        (WINDOW 44 T (if CHOSEN CHOSEN 'ANSWER)) ;% **** ACTION WINDOW **** %
	(return a)
  )
  )

;% THE FOLLOWING ARE INTENTION ROUTINES, ONE PER INTENTION %

(defun pinteract () (when (nequal FLARE 'INIT) (addto 'PHELP 5)))
(defun pgames () (anddo (choose 'GAMES) (addto 'PGAMES -2)))
(defun pfacts () (anddo (choose 'MOVEON) (addto 'PFACTS -2)))
(defun pmafia ()
  (anddo 
    (if (>= FEAR 10) 
      (choose 'PANIC)
      (if (BL 'DGAMES) 
        (anddo nil (setf ANGER (- ANGER 3)))
        (choose 'PROBE)))
    (addto 'PMAFIA -2))
  )

(defun phelp ()
  (if DELFLAG
    (addto 'PTELL 5)
    (when (and (eq FLARE 'INIT) 
	     (or (get STRUC 'UNIT) (not REACTTO) (eq 'LEADIN (get REACTTO 'CLASS))))
      (addto 'PHELP -5)
      (flarelead (chooselead ))
      )
    )
  )

(defun pstop() nil)
(defun ptell() nil)
(defun pstrongfeel() (strongfeel))
(defun pconfirm ()
  (if (and (bl 'NDELUSIONS)
	   (not BADINPUT))
    (anddo (choose 'PRAISE)(addto 'PSTOP 3))
    (if (or (and (not BADINPUT) (get REACTTO 'UNIT))
            (not REACTTO))
        (anddo (choose 'FEELER) (anddo (addto 'PSTOP 2) (addto 'PCONFIRM -5))))
    )
  )

(defun pself () (anddo (choose 'IYOUME) (addto 'PSELF -3)))
(defun pexit () (anddo (choose 'OPINION) (addto 'PEXIT2 10)))
(defun pexit2 () 
  (anddo (choose (cond 
	          ((>= ANGER 9) 'MADEXIT)
	          ((>= FEAR 9) 'FEAREXIT)
	          ((not (memsizeok)) 'TIRED)
	          (T 'EXIT)))
	 (setf ENDE T))
  )

(defun lull ()
  ;% RETURNS T IF THERE IS A LULL IN THE CONVERSATION %
  ;% IF MISC IS USED AND LULL(), THEN WILL JUMP BACK TO FLARES OR DELNS %
  (cond ((or (eq OLDTOPIC 'ANAPH) (eq OLDTOPIC 'IYOUME)) T)
	((>= (length SSENT) 10) nil)
	(T (eq 1 (parandom 2)))
	)
  )

(defun pparanoia ()
  (prog (a)
	(addto 'PARANOIA -5)
	(when (>= HURT 10) (setf HURT (+ 10 (/ (* (- HURT 10)  3) 5))))
	(setf a (assoc (get REACTTO 'CLASS)
		       '((INSULT . PANGER)(CRAZY . AVOIDANCE)(THREAT . PANIC)(ATTACK . LIE)
			 (FEELINGS . LIE)(WEAKINSULT . PPERS)(COMPLEMENT . PDISTANCE)
			 (DISBELIEF . PBELIEVEREPLIES)(APOLOGY . PACCUSE))))
	(unless a (setf a (assoc (carn (get REACTTO 'SF))
				 '((HELPER . PCAUTION)(ALOOF . PALOOF)(ALOOF2 . OPINION)))))
	(when a (setf a (cdr a)))

	(when (and (not a) (eq STOPIC 'MAFIA))
	  (setf a (if DELFLAG 'AVOIDANCE 'NOMAFIA)))
	(when (and (not a) (>= FEAR 14))
	  (setf a (if (eq paTYPE 'Q) 'PTHREATQ 'PAFRAID))
	  (addto 'PEXIT 1))
	(unless a
	  (setf a (if (>= HURT 10) 'ALIEN 'PHOSTILEREPLIES))
	  (addto 'PEXIT 1))
	(return (choose a))
	)
  )

(defun strongfeel ()
; % THIS IS INVOKED ON EITHER FJUMP, AJUMP, OR HIGH EMOTIONS %
; % THIS RETURNS AN ACTION APPROPRIATE TO TAKE CARE OF THE EMOTIONS %
  (prog (a b)
	(addto 'PSTRONGFEEL -5)
	(addto 'PMAFIA -5)
	(cond ((not (setf a (get REACTTO 'CLASS))) nil)
	      ((setf b (assoc a '((INSULT . ANGER)(WEAKINSULT . PERS)(COMPLEMENT . DISTANCE)
                (SENSATTITUDE . SENSREPLIES)(CRAZY . HOSTILEREPLIES)(THREAT . PANIC)
                (DISBELIEF . BELIEVEREPLIES)(APOLOGY . ACCUSE)(LYING . BELIEVEREPLIES) ))) 
	       (setf b (cdr b)))
	      ((eq a 'DISTRUST) (setf b (if (>= (+ FEAR ANGER) 14) 'TURNOFF 'ALOOF))))
	(when (and (<= ANGER 14) (<= FEAR 14)) (return (choose b)))
	(when (and (not FJUMP)
		   (not AJUMP)
		   (not (memq STOPIC '(BYE MAFIA GAMES IYOUME FEELINGS STRONGFEELINGS))))
	  (return (if (>= FEAR 14) (FEARMODE) (ANGERMODE))))
	(return (choose b))
	)
  )

(defun paranoia ()
  (prog (a)
	(paassert '?*DTRUSTWORTHY)
	(loop for i in PARBEL do
	      (when (memq I '(LYING LOSER CRAZY DUMB))
		(addto i -1)
		(setf a i)))
	(when a
	  (setf a (assoc a  '((LYING . ?*DHONEST)
			      (LOSER . ?*DSOCIABLE)
			      (CRAZY . DABNORMAL) 
			      (DUMB . ?*DCHELP) )))
	  (paassert (cdr A))
	  )
	(setf PARBEL nil)
	)
  )


(format t "end of loading pmem5.lisp~%")

