;; this file is a translation of OPAR3

;%       THIS FILE CONTAINS THE ROUTINES FROM OLD PARRY WHICH DEALT WITH
;                FLARE AND DELUSION TOPICS AND SETTING VARIABLES FROM THEM  %
;
;
;%               #####   PARANOID MODEL   #####          %

(defun ten () (setf BASE (setf IBASE TEN)))
(defun eight () (setf BASE (setf IBASE EIGHT)))

;%THE FOLLOWING FUNCTION IS TO FIX OCTAL-DECIMAL PROBLEM %
(defun  lapin (L)
  (prog (ibase)
	(setf ibase 8)
	(return (eval (cons 'DSKIN L)))
	)
  )

;%
;OPARINITIALIZE  READS IN RDATA, SETS ALL VARIABLES IN THIS FILE
;                MUST BE DONE WHEN MAKING A NEW CORE IMAGE OF PARRY  %
(defun oparinitialize ()
  (progn 
    (eval '(INC (INPUT (PAR BLF) RDATA) NIL)) ;;; this may be infix notation...
    (loop while (NOT (ATOM (setf X (ERRSET (READ) T) )))do
	  (eval (car x))
	  (inc nil T))
    (setf FLARE 'INIT)                         ;%   FLARE=CURRENT FLARE TOPIC; 'INIT = NONE   %
    (setf LIVEFLARES (GET 'FLARELIST 'SETS))   ;%   FLARES NOT YET DISCUSSED   %
    (setf DEADFLARES  NIL)
    (setf SENSITIVELIST (GET 'SENSITIVELIST 'SETS)) ;%   SENSITIVE TOPICS   %
    (setf DELNLIST (GET 'DELWDS 'NOUNS))      ;% DELUSION TOPICS   %
    (setf DELVLIST (GET 'DELWDS 'VERBS))
    (setf DELALIST (GET 'DELWDS 'AMBIG))      ;% DELUSION TOPICS ABOVE A CERTAIN THRESHOLD OF MISTRUST   %
    (setf ANGER    0)
    (setf ANGER0   0)
    (setf FEAR     0)
    (setf FEAR0    0)
    (setf MISTRUST 0) 
    (setf MISTRUST0 0)
    (setf HURT     0) 
    (setf HURT0    0)
    (setf FJUMP nil)
    (setf AJUMP nil)
    (setf HJUMP NIL)
    )
  )

;%
;CHECKFLARE     SCANS THE INPUT SENTENCE FOR THE FLARE WORD WHICH HAS THE
;                HIGHEST WEIGHT   %
;% CALLED BY FLAREREF, ASCAN  %

(defun checkflare (INP FLARELIST FLAG)
  (prog (NFLARE WORD FSET WT RESULT W)

                ;%   DISTINGUISH FLARES FOUND WITHIN THE STATEMENT (NFLARE)
                ; FROM MOST RECENT FLARE (FLARE)   %
        (setf NFLARE 'INIT)  ;        %   GET ('INIT, 'WT) = 0   %
                ;%   SCAN INPUT FOR FLARES AND CHECK WHETHER WEIGHT IS
                ;    GREATER THAN ANY PRECEDING FLARES IN INPUT   %

;        % ***** SOMEDAY, USE DIFFERENT INP AND DIFFERENT "GET(WORD,'SET)" %
;        %   FOR EXAMPLE, USE THE PATTERN OR THE WORDS THAT MATCHED THE PATTERN  %

        (loop for word in INP do
	      (if (and (member (setf FSET (GET (CAR WORD) 'SET)) (FLARELIST))
		       (not (get (cdr WORD) 'USED)))
		(when (greaterp (get FSET 'WT)(get (get NFRAME 'SET) 'WT))
		  (prog2 (setf NFLARE (CAR WORD))
			 (setf RESULT T)
			 (setf W (CDR WORD))))))
	(when RESULT 
;                   %   IF FLARE ALREADY BEING DISCUSSED, DISREGARD ANY
;                        VERY WEAK NEW FLARE   %
             (if (and (NOT (eq FLARE 'INIT) (NOT (setf WT (greaterp (GET (GET NFLARE 'SET) 'WT) 1)))))
	       (setf RESULT NIL)
	       (progn
		 (setf FLARE NFLARE)
		 (setf WEIGHT WT)   ;%   USED IN COMPUTING RISE IN FEAR   %
		 (when (and W FLAG) (setf (get W 'USED) T))
		 )
	       )
	     )
	(return RESULT)
	)
  )

;%
;DELREF      SCANS THE INPUT SENTENCE FOR THE FIRST DIRECT REFERENCE TO 'SELF'S
;            DELUSIONAL COMPLEX AND RETURNS A FEARFUL REACTION.  IF NO SUCH REFERENCE
;            IS FOUND, NIL IS RETURNED.   %
;%  TOP LEVEL  %

(defun delref (FOUND)
  (prog (result)
	(if (not (eq FOUND 'MAFIAEND))
	  (progn 
             (iF DELFLAG 
;                         %   IF DELUSIONS ALREADY BEING DISCUSSED, THEN
;                          DISTINGUISH BETWEEN "STRONG" AND "AMBIGUOUS" DELUSIONAL TOPICS
;                          IN COMPUTING RISE IN FEAR   %
                (IF (and FOUND (GET (CAR FOUND) 'STRONG)) 
		    (setf FJUMP 0.4)
		    (setf FJUMP 0.1))
		(progn 
		  (setf FJUMP  0.5)
;                         %   'MAFIA' TOPIC NO LONGEV INDUCES FEARFUL REACTION,
;                          SINCE DELUSION DISCUSSION HAS ALREADY BEEN EVOKED   %
                  (PUTPROP 'MAFIA T 'USED)
;                         %   MODIFY FLARE STRUCTURES TO NOTE THAT 'MAFIA' TOPIC
;                          HAS ALREADY BEEN BROUGHT UP   %
                FLMOD ('MAFIASET);
		)
;                   %   SET (OR KEEP) DELFLAG = T UNLESS 'SELF HAS
;                          FINISHED DISCUSSION DELUSIONS   %

             (unless DELEND (setf DELFLAG T))

;                %   RESET SO THAT FLARES OF LOWER PRIORITY THAN THOSE WHICH
;                 MAY HAVE BEEN PREVIOUSLY MENTIONED ARE RECOGNIZED   %
;
             (setf FLARE 'INIT)

;                   %   FORGET ABOUT RECENTLY DISCUSSED SELF-TOPICS   %

             (setf TOPIC 'DELUSIONS)
             (setf RESULT NIL)
	     )
	     )
	  (if (and (eq FOUND 'MAFIAEND)  ;  % I.E. AS ALREADY USED DEL WD AND IN INPUT  %
		   DELEND)
	    (setf RESULT (CHOOSE 'MAFIASET)))
	  )
	  (return RESULT)
	  )
  )

;%
;DELSTMT CAUSES THE "NEXT" DELUSION TO BE EXPRESSED   %
;% CALLED BY DELREF, MISCQ, MISCS, FLSTMT  %
;
(defun delstmt ()
  (prog ()
;%   IN WEAK VEVSION, TALK ABOUT RACKETS RATHER THAN MAFIA   %
        (when WEAK (return (flstmt 'RACKETSET)))

;          %   IF 'SELF HAS ALREADY EXPRESSED ALL HIS DELUSIONS, HE REFERS TO
;                PREVIOUSLY MENTIONED ONES UP TO 2 TIMES TOTAL   %

        (unless (get 'DELNSET 'STORY) 
	  (setf DELFLAG NIL)
	  (CHOOSE 'MAFIASET))

	(setf DELFLAG T)
	(setf FLARE 'INIT)
	(setf TOPIC 'DELUSIONS)

;%   SELECT DELUSION   %

        (setf STMT (CHOOSEDEL NIL))

;%   IF STMT CONTAINS DELUSIONAL FLARE, DELETE AS SUCH   %

;% ***** MAKE SURE DELCHECK IS DONE ON THE OUTPUT SENTENCE  %

;%   REMEMBER THE DELUSIONAL STATEMENT TO WHICH 'OTHER IS ABOUT TO RESPOND   %

     (RETURN STMT)
     )
  )


;%
;FLAREREF    HANDLES FLARE REFERENCES   %
;% TOP LEVEL  %

(defun flareref (INP)
  (prog ()
;                %   CHECK FOR NEW FLARE AND RECORD AS "OLD"   %
         (when (checkflare INP LIVEFLARES NIL) (FLRECORD (GET FLARE 'SET)))

;                %   CHECK FOR OLD FLARE   %

         (when (CHECKFLARE INP DEADFLARES T) ;%   RESPOND TO FLARE   %
	   (return (GET FLARE 'SET))
	   )
	 )
  )

;%
;ASCAN     SCANS 'SELF'S ANSWER FOR MENTION OF FLARE OR MAFIA   %
;% CALLED BY PROMPT, ANSWER--NOW DONE BY NEW PARRY AT END OF OUTPUT%

(defun ascan (ANS Q)
  (progn 
    (when (checkflare ANS LIVEFLARES T) (putprop (GET FLARE 'SET) T 'USED))
    (when (member 'MAFIA ANS) 
      (setf DELFLAG T)
      (setf FLARE 'INIT)
      (setf TOPIC 'DELUSIONS))
    (when DELFLAG (delcheck ANS))
    )
  )

;%
;CHOOSE   SELECTS THE NEXT REPLY FROM THE RELEVANT GROUP
;          INPUT SHOULD BE THE NAME OF A GROUP OF REPLIES
;          OUTPUT WILL BE A LAMBDA NUMBER  %
(defun choose (replies) 
  (prog (response)
	(unless replies (return NIL))
	(setf CHOSEN replies)
	(when (null (setf RESPONSE (get replies 'IND)))
	  (return 
	    (if  (eq replies 'EXHAUST)
	      (progn (setf ENDE T) (choose 'BYEFEDUP))
	      (choose 'EXHAUST))))
	(return response)
	)
  )

;%
;CHOOSEDEL   CHOOSES A DELUSIONAL RESPONSE ACCORDING TO "TYPE", WHICH INDICATES
;          WHETHER THE NEXT GENERAL DELUSION IS TO BE SELECTED (TYPE=NUMBER)
;          OR A CERTAIN TYPE OF QUESTION IS TO BE ANSWERED   %
;          % CALLED BY DELSTMT, SPECQUES  %
(defun choosedel (type)
  (prog (SEMANT)
	(unless (setf SEMANT (GET 'DELNSET 'STORY))
	  (return NIL)
	  (return (car SEMANT))
	  )
	)
  )

;%
;DELCHECK    RETURNS ANY NEW DELUSION-EXPRESSIONS FOUND IN INPUT AND DELETES AS SUCH   %
;            % CALLED BY DELREF, SPECQUES, DELSTMT  %
;% INP IS INPUTQUES   %

(defun delcheck (INP)
  (prog (words)
;%   CHECK FOR STRONG DELUSION-NOUNS AND -VERBS
;    (AT PRESENT THE NOUN-VERB DISTINCTION IS NOT UTILIZED   %
	(if (or (setf words (member3 DELNLIST INP) )
		(setf words (member3 DELVLIST INP) ))
	  nil
;          %   CHECK FOR AMBIGUOUS DELUSION WORDS AT HIGH MISTRUST LEVEL  %
	  (if (and (greaterp MISTRUST 10)
		   (setf WORDS (MEMBER3 DELALIST INP)))
	    nil
            (when (and WORDS  (CDR WORDS))
	      (putprop (CDR WORDS) T 'USED)))
	  (if (and WORDS (ATOM WORDS))
	    (setf WORDS (cons WORDS nil))
            (when (and (member3 'MAFIA INP) ;% AS ALREADY-USED DELN WD% 
		     (not WORDS))
		(setf WORDS 'MAFIAEND)))
	  (return WORDS)
	  )
	)
  )

;%
;DELETE   DELETES WORD WD FROM LIST L   %
(defun padelete (WD L)
  (cond ((null L) nil)
	((equal WD (car L)) (cdr L))
        (T (cons (CAR L)) (padelete WD (CDR L))))
  )

;%
;DELETEP     DELETES WD FROM THE PROP PROPERTY LIST OF L   %
(defun deletep (L WD PROP)
  (PUTPROP L (padelete WD (GET L PROP)) PROP)
  )

;%
;FIXPTRS     TRANSFERS HIERARCHICAL POINTERS TO NEW FLARE
;            TO NEXT HIGHER FLARE IN PATH   %
;% CALLED BY FLMOD  %
(defun fixptrs (FLSET)
  (prog (CONCEPT)
	(loop for CONCEPT in (append LIVEFLARES DEADFLARES) do
	      (if (eq (GET CONCEPT 'NEXT) FLSET)
		(putprop CONCEPT (get FLSET 'NEXT) 'NEXT))
	      )
	)
  )

;%
;FLRECORD    NOTES MENTION OF FLARE AND RAISES FEAR   %
;% CALLED BY FLAREREF  %
(defun flrecord (FLSET)
  (progn 
    (flmod FLSET)
    (setf FJUMP (/ WEIGHT 40.0))
    ; %   REINITIALIZE SELF-TOPIC INDICATORS   %
    (setf TOPIC FLSET)
    )
  )

;%
;FLMOD       MOVES NEW FLARE FROM "LIVELIST" TO "DEADLIST" AND
;          ADJUSTS FLARE POINTER HIERARCHY   %
;% CALLED BY DELREF, FLRECORD, ASCAN, LEADON  %
(defun flmod (FLSET)
  (setf LIVEFLARES (padelete FLSET LIVEFLARES))
  (setf DEADFLARES (cons FLSET DEADFLARES))
  (fixptrs FLSET)
  )

;%
;FLARELEAD DECIDES WHAT TYPE OF "SUSPICIOUSNESS" REPLY IS SUITED
;           TO INTRODUCE THE FLARE CONCEPT   %
;% CALLED BY LEADON  %
(defun flarelead (FLSET)
  (prog ()
    (putprop FLSET T 'USED)
    (if (eq (get FLSET 'TYPE) 'INSTITUTION)
      (setf WDFLAG (append '(THE) (list (CAR (GET FLSET 'WORDS)))  ))
      ;%   DO NOT TREAT SINGULARS AS A GENERIC TOPIC   %
      (setf WDFLAG (if (eq (car (last (explode FLARE))) 'S)
		     (list FLARE)
		     (list (car (get FLSET 'WORDS))))   )
      )
    (ADDANAPH (list (cons 'THEY (if (cdr WDFLAG) (cadr WDFLAG) (car WDFLAG) ) )))
    (ADDANAPH (list (cons 'GO_ON (carn (get FLSET 'STORY)) )) )
    (return (CHOOSE 'NEXTFL))
    )
  )

;%
;FLSTMT   PROVIDES NEXT STATEMENT ABOUT FLARE   %
;% CALLED BY MISCQ, MISCS, ANSVAR, DELSTMT  %
(defun flstmt (FSET)
  (prog (STMT)
;                %   IF REACH 'MAFIASET THRU FLARE HIERARCHY, ENTER DELUSIONAL MODE   %
	(if (and (eq FSET 'MAFIASET) (not DELEND))
	  (setf DELFLAG T)
	  (return (DELSTMT)))
	(if (setf STMT (GET FSET 'STORY))
	  (return (car STMT)))
;       %   GO TO NEXT FLARE TOPIC   %
        (return (LEADON FSET))
	)
  )

;%
;LEADON   %
;% CALLED BY FLSTMT  %
(defun leadon (OLDSET)
  (prog (NEWSET)
	(setf NEWSET (GET OLDSET 'NEXT))
	(cond 
	  ((neq NEWSET 'MAFIASET)

;                %   RECORD NEW FLARE   %
	    (flmod OLDSET) ; % MARK OLD ONE AS BEING USED UP %
	    (setf FLARE (CAR (GET NEWSET 'WORDS)))
	    )
          (DELEND 
;                   %   ARRIVE AT 'MAFIASET BUT THROUGH WITH DELUSIONS   %
             (return (prog2 (setf FLARE 'INIT)(CHOOSE 'FEELER))))
	  ((or WEAK (greaterp FEAR 17) (greaterp ANGER 17)
		     (greaterp (+ FEAR ANGER MISTRUST) 40))
;                   %   ARRIVED AT 'MAFIASET BUT DOES NOT HAVE DELUSIONS ABOUT
;                          MAFIA OR IS UNWILLING TO DISCUSS THEM   %
	   (return (CHOOSE 'CHANGESUBJ)))
	  (t 
             (padelete 'MAFIA DELNLIST)
	     (setf DELFLAG T)
	     (setf FLARE 'INIT)
	     (setf TOPIC 'DELUSIONS))
	  )
;                %   RESPOND WITH NEW FLARE, IF USED THEN NO LEADING STMT   %
;                 %  MARK AS USED SO WE DON'T DO FLARELEAD TWICE ON IT  %
	(if (GET NEWSET 'USED) 
	  (return (FLSTMT NEWSET))
	  (return (FLARELEAD (NEWSET)))
	  )
	)
  )


;%
;MEMBER3         CHECKS IF ATOMS ARE IN INPUT -- INPUT IS A LIST OF DOTTED PAIRS  %
(defun member3 (WLIST L)
  (prog (WORD PAIR)
	(when (atom WLIST) (setf WLIST (list WLIST)))
	(loop for in WLIST do
	      (setf PAIR (assoc WORD L))
	      (when (and PAIR (get (cdr PAIR) 'USED)) (setf PAIR NIL))
          until PAIR) 
	(return PAIR)
	)
  )

;%
;MISCQ       TRIES TO DETECT AND ANSWER CERTAIN RECOGNIZABLE QUESTIONS.
;          IF IT FAILS, IT TRIES TO DISCERN WHETHER THE QUESTION CONTAINS
;          INTERROGATIVE WORDS REQUIRING A SPECIFIC ANSWER, OR WHETHER IT
;          REQUIRES A GENERAL YES- OR NO-TYPE ANSWER,
;          AND CALLS FOR AN APPROPRIATE REPLY   %
;% CALLED BY ANSWER  %
(defun miscq (Q)
  (prog (QWORD ANS CONCEPT)
; %   CHECK FOR QUESTION ABOUT EXTERNAL WORLD   %
	(cond 
	  ((member 'HOW Q)
; %   UNIDENTIFIABLE "HOW-TYPE" QUESTION   %
	   (loop for CONCEPT in '(MANY MUCH LONG OFTEN) do
		 (when (member CONCEPT Q) (setf ANS (CHOOSE CONCEPT)))
		 until ANS)
	   (when ANS (return ANS)))
	  ((setf ANS (SPECCONCEPT Q))
;                %   IF QUESTION NOT RECOGNIZED, TRY TO ANSWER ACCORDING TO CONTEXT   %
	   (RETURN ANS))
	  ((and (neq FLARE 'INIT)(LULL)(setf ANS (FLSTMT (GET FLARE 'SET))))
	   (return ANS))
	  ((and DELFLAG (LULL) (setf ANS (DELSTMT)))
	   (return ANS))
;	               %   WH- QUESTIONS   %
          ((member 'WHY Q) (setf ANS (CHOOSE 'WHY)))
	  ((loop FOR QWORD in (GET 'QLIST 'IND) do 
		 (setf ANS (if (member QWORD Q) (CHOOSE 'UNKNOWN)))
             until ANS)
	   (if ANS (return ANS)))
	  ((member 'TELL Q)
;                %   MISCELLANEOUS "TELL-" QUESTION   %
	   (return (CHOOSE 'KNOWNOTHING)))
;                %   NO CLUES - ANSWER NONCOMMITTALLY   %
	  (T (return (CHOOSE 'QREPLIES)))
	  )
	)
  )


;%
;MISCS   TRIES TO DETECT AND ANSWER CERTAIN RECOGNIZABLE STATEMENTS,
;           MAINLY IMPERATIVES AND EXPECTED EXPRESSIONS   %
;% CALLED BY ANSWER  %
(defun miscs (S)
  (prog (ANS FOUND)
	(setf FOUND 
	      (cond 
		((member 'JUMP S) (PROG2 (setf ENDE T) (CHOOSE 'EXIT)))
		((or (eq (CAR S) 'HI) 
		     (eq (CAR S) 'HELLO) 
		     (member (CADR S) '(MORNING AFTERNOON EVENING)))
		 (CHOOSE 'HELLO))
		((and (or (member 'ALREADY S) (member 'BEFORE S)) 
		      (or (member 'SAID S) (member 'MENTIONED S)))
		 (CHOOSE 'ALREADYSAID))
;                %   LOOK AT CONTEXT OF CONVERSATION   %
		((setf  ANS (SPECCONCEPT S)) ANS)
		((and (neq FLARE 'INIT)
		      (LULL)  
		      (setf ANS (FLSTMT (GET FLARE 'SET))))
		 ANS)
		((and DELFLAG  (LULL) (setf ANS (DELSTMT)))
		 ANS)
;                %   NONCOMMITTAL REPLY  %
		(T (CHOOSE 'SREPLIES))))
        (return FOUND)
	)
  )

;%
;MODIFVAR    MODIFIES AFFECT VARIABLES AFTER EACH I-O PAIR   %
;%  TOP LEVEL  %
(defun modifvar ()
  (prog ()
;                %   ACCOUNT FOR NORMAL DROP IN EACH VARIABLE   %
	(setf ANGER (MAX (-1 ANGER)ANGER0))
	(setf HURT (MAX (- HURT 0.5) HURT0))
	(if DELFLAG (setf FEAR (max (- FEAR 0.1) (+ FEAR0 5)))
;                   %   ADD 5 TO BASE VALUE OF FEAR IF DELUSIONS UNDER DISCUSSION   %
          (if (neq FLARE 'INIT) (setf FEAR (MAX (- FEAR 0.2) (+ FEAR0 3)))
;                   %   ADD 3 TO BASE VALUE OF FEAR IF FLARES UNDER DISCUSSION   %
          (setf FEAR (MAX (- FEAR 0.3)  FEAR0)))
          (setf MISTRUST (MAX (- MISTRUST 0.05) MISTRUST0)))
	(when TRACEV (PRINTVARS)
	  (setf FJUMP NIL)
	  (setf AJUMP NIL)
	  (setf HJUMP NIL))
	)
  )

;%
;RAISE   RAISES LEVEL OF RELEVANT AFFECT VARIABLES;
;           REDUCE JUMP IF IN WEAK VERSION   %
;                % CALLED BY MODIFVAR  %

;;; no definition of RAISE

;; commonlisp has max
;;(defun MAX (L M) (IF (>= L M) L M))





(format t "end of loading opar3.lisp")

