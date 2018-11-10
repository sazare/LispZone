;; pmem5.lisp

(defun experiment ()
  ;; EXPERIMENTS OF RAISING AND LOWERING SHAME
  (unless (or EXPERIMENT (eq EXPERIMENT 'SEVEN))
    (if (= 7 INPUTNO) (setf HURT (+ HURT 5)))
    (if (= 17 INPUTNO) (setf HURT (- HURT 5)))
  )
)

(defun error (mess L)
  (setf ?!ERROR (cons (error_file (list mess L PM2INPUT PMINPUT FILE1 BUG))
	      ?!ERROR))
  )

(defun allowrun ()
  (prog (a)
	(setq a (if SUMEX (sumexallow) (allow)))
	(unless a (swapp)) ;  % NOT FROM SCHEDULER %
	(unless SUMEX (namep)) ; NAME THIS PROGRAM PARRY %
	(if (and (not SUMEX) (= A 9)) (setf ONEDIA nil) (setf ONEDIA T))
	(if (>= a 2) (setf LOWMAN T) (setf LOWMAN nil))
	(if (or (= a 1) (= a 3)) (setf TRACEVFLAG nil)(setf TRACEVFLAG T))
	)
  )

(defun sumexallow ()
  (prog (filchan flag status)
    (setq filchan (eval (list 'INPUT DIAFILEAREA 'QPARRY)))
    (inc filchan nil)
    (setq status (read))
    (setq flag (read))
    (when (eq status 'OK) (inc nil T) (return flag))
    (inc nil nil)
    (eval (list 'OUTPUT DIAFILEARE 'QPARRY))
    (outc T nil) (print 0) (print flag) (outc nil T) ; %RESET FILE %
    (when (= status 0) (sleep 10)(exit) (car nil)) ;;; ?? want to error ???
    (return status)
    )
  )

(defun measuer (L M) ; % USED IN INFERENCES TO MEASURE QUANTITIES %
  (if (and (numberp L) (numberp M)) (greaterp L M) (EQ L M)))

(defun inf (a) ;% INITIALIZATION ROUTINE %
  (prog (b va)
	(allowrun)
	(setf INPUTFILE a) (setf LAMDA b) (setf NOTSAVED 'NDIA)
	(initfn 'EXIT)
	(analyze NIL)
	(gcgag nil)
	(runtim nil)
	(setq va nil) ;% VA IS FOR STARTING A PARRY WITHOUT INITIAL QUESTIONS %
	(unless va (initparams) (init))
	(when va (INITPARAMS1) (INITPARAMS2) (INIT))
	(if SAVE_FILE 
	  (INIT_FILE (with-output-to-string (str)
		       (format str "~a" (if va 'VA else ""))
		       (format str 
			       ", TRACEV = ~a, WINDOWS = ~a, PARANOIA = ~a"
			       TRACEV WINDOWS (IF WEAK THEN 'WEAK ELSE IF HURT=0 THEN 'MILD ELSE 'STRONG) )
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

(defun anddo (L M) L)

(defun window (N F L)
  (prog ()
	(when (eq TRACEV 'ALL)
	  (twindow N F L) 
	  (return L))
	)
  )

(defun windowsset (n) n)

(defun window_print (A B C D) nil)

(defun twindow (N flag L) ;  % PRINTS OUT WINDOW STUFF FOR A TELETYPE %
  (prog (a)
	(setq a (assoc  N 
			'(      ( 2 . "Input:   " )
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
			  ( 44 . "Action:  " )) ))
	(when (and a (or flag (= n 9) (= n 36) (= n 42)))
	  (princ (cdr a))
	  (printstr L))
	)
  )

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
	  (setq A (choose 'TIRED))
	  (setq B '(INTERVIEW HAS BEEN LONG ENOUGH))
	  (addto 'PEXIT2 10)
	  (setf ENDE T)
	  (ERROR (with-output-to-string (str)
		   (format str "INPUTNO= ~a  SHORT OF SPACE ~a FS, FW ~a"
			   INPUTNO (length (NUMVAL 13)) (length (NUMVAL 14)))))
	  )

        (when  (and (not A) (setq B (GET L 'FX)) (ERRSET (setq B (EVAL B)) NIL)) (setq A B)) 
	; % FX HAS A FEW KLUDGES TO NOT ALLOW SOME INPUTS %
        (when (and L (not (get L 'UNIT)) (not A ) (setq A 'DONE)))
        (when (and (not A) (ASSOC 'SHIT INPUTQUES))
	  (setq A (choose 'SWEARING)) 
	  (setf BADINPUT T)
	  (setq B 'EXPLETIVES))
        (when (and (not A) (or (ASSOC 'CRAZY INPUTQUES)
			       (ASSOC 'BAD INPUTQUES) 
			       (ASSOC 'ODD INPUTQUES)))
                (setf BADINPUT T) (setq B '(BAD ASSOCIATIONS WITH INPUT WORDS)))
        (setq C (- GIBBERISH OLDGIBB))(setq D (length SSENT))
	;; some >= may be wrong
	(when (and (not A) (or (and (>= C 5) (D >= 15))(and (>= C 3) (>= D 7)) (and (>= C 2) (>= D 3) )))
	  (setq A (choose 'GIBBERISH))
	  (setq B '(TOO MANY UNRECOGNIZED WORDS)))

        (when (>= GIBBERISH 20) (setf DO_SPELL nil))
	(when (and (not A) (>= MISSPELLED 6) (>= (- MISSPELLED OLDMISS) 3))
	  (setq A (CHOOSE 'MISSPELLED))
          (setq B '(TOO MANY MISSPELLED WORDS)))
	(cond ((equal SSENT (append PREV_OUTPUT '(PD))) (ADDTO 'PGAMES 5)) 
	      ((and PREV_SSENT  (eq (car PREV_SSENT) 'TWICE) (equal (cadr PREV_SSENT) SSENT)) (addto 'PGAMES 5))
	      ((equal PREV_SSENT SSENT) (setf PREV_SSENT (list 'TWICE SSENT)))
	      (t (setf PREV_SSENT SSENT)))
	(setf OLDGIBB GIBBERISH)
	(setf OLDMISS MISSPELLED)
	(unless b (setq b 'NORMAL))
	(window 33 T B)
        (when (eq A 'DONE)  (setq A nil)(return A)) ;  % WILL RETURN NIL IF ANAPH OR NORMAL %
	)
  )

;% THE FOLLOWING ARE SEMANTIC FUNCTIONS WHICH ARE CALLED FROM THE MEMORY BY
;  SPECIFIC INPUT REFERENCES %

(defun apology ()
  (prog ()
  (if (>= MISTRUST 9) (setf AJUMP 0.2)  (setf ANGER (-1 ANGER)))
  (if (and (not (BL 'DHOSTILE)) (BL 'DDKNOW)) 
	    (return (CHOOSE 'SORRY))
            (return (CHOOSE 'ACCUSE)))
  )
  )

(defun helper ()
  (if (or (bl 'DHOSTILE) (bl '?*DHELPFUL) (bl 'DDHARM))
    (choose 'CAUTION))
  )

(defun knower ()
  (cond ((or (BL 'DDHARM)(BL 'DHOSTILE)(BL '?*DDHELP) (choose 'HOSTILEREPLIES))
        ((or (BL '?*DTRUSTWORTHY) (BL '?*DHONEST)) (CHOOSE '?*DHONEST))
        ((BL 'DDHELP)(CHOOSE 'DDHELP))
        ((BL 'DEXCITED)(CHOOSE 'DEXCITED))
        ((BL '?*DINITIATING)(CHOOSE 'DBAD))
	(T nil))))

(defun leadin ()
  (progn 
    (setf ?!ANAPHLIST (setf ?!ANAPHLISTOLD nil))
    (cond (DELFLAG (delstmt))
	  ((eq FLARE 'INIT) (flstmt (get FLARE 'SET)))
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
	(when (and (not (setq a assoc('NAME INPUTQUES))) (setq A (assoc 'YOU INPUTQUES))
		   (eq (cdr A) 'YOUR))
	  (return (CHOOSE 'DONTREMEMBER))
	  )
	)
  )

(defun memsizeok()
  (and (>= (length (NUMVAL 13)) 1500) (>= (LENGTH (NUMVAL 14) 300)))
  )



(defun zeronil (L) (if (not L) 0 L))

(defun intention ()
  (nyi)
  )

;% DOINTENT  PERFORMS THE CURRENT INTENTION, CALCULATES AN ACTION, AND RETURNS A ^H %
;%       CHECKS INTENTS, RETURNS NEW VALUE FOR FOUND IN REACT, ELSE NIL %

(defun dointent ()
  (nyi)
  )

;% THE FOLLOWING ARE INTENTION ROUTINES, ONE PER INTENTION %

(defun pinteract () (when (eql FLARE 'INIT) (addto 'PHELP 5)))
(defun pgames () (anddo (choose 'GAMES) (addto 'PGAMES -2)))
(defun pfacts () (anddo (choose 'MOVEON (addto 'PFACTS -2))))
(defun pmafia ()
  (if (>= FEAR 10)
    (choose 'PANIC)
    (if (BL 'DGAMES) 
      (andodo nil (setf ANGER (- ANGER 3)))
      (andodo (choose 'PROBE) (addto 'PMAFIA -2))))
  )
(defun phelp ()
  (if DELFLAG
    (addto 'PTELL 5)
    (when (and (eq FLARE 'INIT) 
	     (or (get STRUCT 'UNIT) (not REACTTO) (eq 'LEADIN (get REACTTO 'CLASS))))
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
    (anddo (choose 'PRAISE)(addto 'PSTOP 3)))
  )

(defun pself () (anddo (choose 'IYOUME) (addto 'PSELF -3)))
(defun pexit () (anddo (choose 'OPINION) (addto 'PEXIT2 10)))
(defun pexit2 () 
  (anddo (choose (cond 
	    ((>= ANGER 9) 'MADEXIT)
	    ((>= FEAR 9) 'FEAREXIT)
	    ((not (MEMSIZEOF)) 'TIRED)
	    (T 'EXIT))a)
	 (setf ENDE T))
  )

(defun lull ()
  ;% RETURNS T IF THERE IS A LULL IN THE CONVERSATION %
  ;% IF MISC IS USED AND LULL(), THEN WILL JUMP BACK TO FLARES OR DELNS %
  (cond ((or (eq OLDTOPIC 'ANAPH) (eq OLDTOPIC 'IYOUME)) T)
	((>= (length SSENT 10) ) nil)
	(T (eq 1 (random 2)))
	)
  )

(defun pparanoia ()
  (prog (a)
	(addto 'PARANOIA -5)
	(when (>= HURT 10) (setf HURT (+ 10 (* (- HURT 10) (/ 3 5)))))
	(setf a (assoc (get REACTTO 'CLASS)
		       '((INSULT . PANGER)(CRAZY . AVOIDANCE)(THREAT . PANIC)(ATTACK . LIE)
			 (FEELINGS . LIE)(WEAKINSULT . PPERS)(COMPLEMENT . PDISTANCE)
			 (DISBELIEF . PBELIEVEREPLIES)(APOLOGY . PACCUSE))))
	(unless a (setq a (assoc (carn (get REACTTO 'SF))
				 '((HELPER.PCAUTION)(ALOOF.PALOOF)(ALOOF2.OPINION)))))
	(when a (setq a (cdr a)))

	(when (and (not a) (eq STOPIC 'MAFIA))
	  (setq a (if DELFLAG 'AVOIDANCE 'NOMAFIA)))
	(when (and a (>= FEAR 14))
	  (setq a (if (eq TYPE 'Q) 'PTHREATQ 'PAFRAID))
	  (addto 'PEXIT 1))
	(when (not a) 
	  (setq a (if (>= HURT 10) 'ALIEN 'PHOSTILEREPLIES))
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
	(cond ((not (setq a (get REACTTO 'CLASS))) nil)
	      ((setq b (assoc a '((INSULT.ANGER)(WEAKINSULT.PERS)(COMPLEMENT.DISTANCE)
                (SENSATTITUDE.SENSREPLIES)(CRAZY.HOSTILEREPLIES)(THREAT.PANIC)
                (DISBELIEF.BELIEVEREPLIES)(APOLOGY.ACCUSE)(LYING.BELIEVEREPLIES) ))) 
	       (setq b (cdr b)))
	      ((eq a 'DISTRUST) (setq b (if (>= (+ FEAR ANGER) 14) 'TURNOFF 'ALOOF))))
	(when (and (>= ANGER 14) (>= FEAR 14)) (return (choose b)))
	(when (and (not FJUMP)
		   (not AJUMP)
		   (not (memq STOPIC '(BYE MAFIA GAMES IYOUME FEELINGS STRONGFEELINGS))))
	  (return (if (>= FEAR 14) (FEARMODE) (ANGERMODE))))
	(return (choose b))
	)
  )

(defun paranoid ()
  (prog (a)
	(assert '?*DTRUSTWORTHY)
	(loop for i in PARBEL do
	      (when (memq I '(LYING LOSER CRAZY DUMB))
		(addto i -1)
		(setq a i)))
	(when a
	  (setq a (assoc a  '((LYING . ?*DHONEST)
			      (LOSER . ?*DSOCIABLE)
			      (CRAZY . DABNORMAL) 
			      (DUMB . ?*DCHELP) )))
	  (assert (cdr A))
	  )
	(setf PARBEL nil)
	)
  )



(format t "end of loading pmem5.lisp~%")

