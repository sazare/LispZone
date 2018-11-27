;;; from PMEM

;;;
;; different from original lambdaname. because cwchanged ^H to @@.
(defun lambdaname (s) (and (>= (length (string s)) 3) (equal (subseq (string s) 0 2) "@@")))

;; now replaced ^B to car, alphaname is incorrect
;; but noone call alphaname
(defun alphaneme (s) (equal (subseq (string s) 0 2) ""))

;% READLAMBDA ATTEMPTS TO READ IN A SEMANTIC FUNCTION (FRAME) FROM THE MEMORY
;        GIVEN A LAMBDA NUMBER AS INPUT %
;;; omura dont use disk
(defun readlambda (a)
  (prog ()
    (unless (lambdaname a) (return nil))
    (when (diskread a) (return T))
    (paerror "BAD DISKREAD" "")
    )
  )

;%        DOES DISKREAD2 PROTECTED BY AN ERRSET;
;         ERRORS MAY OCCUR IF THE MEMORY FILE HAS MISMATCHED PARENS  %
;;; omura dont use disk
(defun diskread (name)
  (prog (a)
    (if (not (setf a (diskread2 name)) )
        (progn
	  (paerror t "error in diskread " name)
          (return nil))
        (return (car a)))
  )
)

;%       GIVEN A ^H OR car  NUMBER AS INPUT, READS IN ONE DISK SEXPR
;        IF THE SEXPR IS ALREADY IN CORE, IT JUST RETURNS T
;        OTHERWISE IT CALLS BEL OR ENG TO PROPERLY LINK ALL THE INFO INTO THE MEMORY %

(defun diskread2 (name)
;;; omura dont use disk
  (prog (a b charno)
    (when (get name 'INCORE) (return T))
    (when (get 'DSKLOC 'SUBR) (setf a (dskloc name))) ; % GET THE CHAR NO OF THE SEXPR %
    (unless a (return nil)) ; % NOT THERE %
    (setf CHARNO A)
    ; % CHARNO IS THE CHARACTER NUMBER IN THE FILE THAT THE SEXPR NAME BEGINS ON %
    ; % IF A RECNO IS NEEDED, IT IS CHARNO/OCTAL 1200 + 1 (FIRST RECORD IS REC 1 %

    (setf CHANSAVE (INC NIL NIL))
    (init);% INITIALIZE THE READ CHANNEL FOR THE MEMORY FILE %
    (inc INCHAN NIL)
    (CHSETI INCHAN CHARNO) ;  % SET THE INPUT POINTER TO THE CORRECT SEXPR %
    (setf B (READ))
    (INC CHANSAVE NIL)
    (when b (PUTPROP NAME T 'INCORE))
    (if (eq (car B) '\#B) 
      (BEL (cdr b))
      (if (eq (car b) '\#E)
	(ENG (cdr b))  ;  % LINK UP IN MEMORY %
        (RETURN b)
	)
      )
    )
  )

;carn was defined in diaapp.lisp. diaapp was gone.
(defun carn (s) (if (atom s) s (car s)))

;%       BEL AND ENG TAKE AN SEXPR FROM THE MEMORY FILE (PDAT) AND LINK
;        THE INFORMATION IN THE MEMORY IN THE RIGHT WAY %
;        % READ IN SEMANTS AND SURFACE AND STORE UNDER LAMBDAS AND ALPHAS %
;        % X IS THE LIST WITHOUT THE B OR E %
;        %   X LOOKS LIKE:  ( @@17 100 (LOC I HOSP) LIT (...)  ) %
(defun bel(x)
  (prog (name truth unit)
    (setf name  (car x))
    (setf truth (cadr x))
    (setf unit  (caddr x))
    (when (or (null x)(null (cdr x))(null (cddr x))(not (numberp truth)))
      (paerror "B BAD INPUT " x)
      (return nil)
    )
    (when (get name 'BONDVALUE) (paerror "BAD INPUT-DOUBLE ENTRY " name))
    (putprop name unit 'BONDVALUE)
    (setf x (cdr x))
    (loop while (setf x (cddr x)) do
       ;% PUT THEM ON THE PROPERTY LIST OF THE ^H NAME %
      (if (or (not (atom (car x))) (null (cdr x)))
         (progn
           (paerror "BAD INPUT " name)
           (return nil)
          )
          (putprop name (cadr x) (car x))))
    (return name)
  )
)

(defun eng (x)
  (prog (unit error)
	(when (or (null x) (null (cdr x))(null (cddr x)))
	  (paerror paerror "E BAD INPUT~a" X)
	  (return nil))
	(setf UNIT (CAR X))
	(when (or (GET UNIT 'NORMAL)  (GET UNIT 'EMBQ))
	  (paerror("BAD INPUT-DOUBLE ENTRY ~a" UNIT)))
	(setf X (CDR X))
	(when (eq (CAR X) 'ANAPH)
	  (setf X (CDR X)) ;% PUT ANAPH ON PROPERTY LIST %
	  (PUTPROP UNIT (CAR X) 'ANAPH)
	  (setf X (CDR X)))
	(when (eq (CAR X) 'EXH)
	  (PUTPROP UNIT (CADR X) (CAR X))
	  (setf X (CDDR X)))
	(loop DO 
             (progn 
	      ; % PUT SENTENCES ON THE PROPERTY LIST %
	      (when (or (NULL X) (NULL (cdr X)) (ATOM (cdr X)) (not (ATOM (car X))))
		(paerror "E BAD INPUT " UNIT)  
		(setf ERROR T)
		(RETURN NIL))
	      (PUTPROP UNIT (CADR X) (CAR X))
             )
	until (or ERROR (not (setf X (CDDR X))))
	)
        (unless (GET UNIT 'NORMAL) (paerror "NO NORMAL SENTS " UNIT))
        (return unit)
        ;%-- REPLYR, ANTHEN, EXPRESS, SELSENTENCE, SAY ------%
	)
  )

;% REPLYR IS THE FUNCTION WHICH SELECTS AND EXPRESSES AN OUTPUT SENTENCE
;        AFTER THE PROPER ^H NUMBER HAS BEEN DETERMINED BY THE MEMORY.
;        A SENTENCE GENERATOR WOULD REPLACE THIS FUNCTION %
;% REPLYR  ADDS TO CONVERSATION LIST, AND EXPRESSES %
;        % SEMANT IS ^H NAME, TYPE=D OR Q, CLASS IS RESP   INTO EXPRESS %
;        % CALLS  ANDTHEN, EXPRESS %

(defun replyr (SEMANT)
  (prog (a)
	(unless SEMANT 
	  (paerror "~aNOSEMANT IN REPLYR" "")
	  (return nil))
        (andthen (list 'OUT SEMANT))
	(setf A (express SEMANT 'RESP))
	(setf ?!OUTPUT (if (and a WDFLAG)
			 (append a (LASTWORD WDFLAG))
	 		 a))
        (setf WDFLAG nil) 
	(return ?!OUTPUT)
	)
  )

;% ANDTHEN PUTS THING ON THE CONVERSATION LIST %
;% INPUT THING IS A LIST OF INFORMATION ABOUT THE CURRENT SEMANTS %

(defun andthen (THING);
  (prog (a)
	;; % IF THE LAST THING ADDED TO THE CONVERSATION LIST, THEN DONT DO THIS ONE %
	(when (equal ?!LAST_ANDTHEN (car THING)) (return nil))
        (setf ?!CLIST (cons THING ?!CLIST))
	(if (eql (car THING) 'IN)
	  (setf ?!LASTIN A)
	  (when (eql (car THING) 'OUT) (setf ?!LASTOUT A)))
	(setf ?!LAST_ANDTHEN (car THING))
	(return (car ?!CLIST))
	)
  )

;%-------------------------------------------------------------------%
;%--EXPRESS -------------%
;% SAYS SEMANT USING CLASS, HAVING THE SUBCL AS THE NECESSARY SLOT, IN MODE %
;%  SEMANT = ^H17, CLASS = SQR OR LIT,
;SUBCL = HOSPITAL OR NIL; APPLIES TO SQR, MODE = NORMAL OR EMBD OR EMBQ %
;%    SUBCL IS THE NAME OF THE SLOT WHICH HAD THE QMARK %

(defun express (SEMANT CLASS)
  (prog (a bond c k)
	(DISKREAD SEMANT)
	(setf a (get SEMANT CLASS))
	(setf bond (get SEMANT 'BONDVALUE))

	; % USE PREDICATE FOR FINDING CLASS %
        (when (and (null a) BOND (setf C (get (car bond) 'UNIT))(DISKREAD C))
           (setf A (get c CLASS)))

	; %  ONLY QUIT IF THERE IS NO RESP   %
        (when (and (null a) (null (setf a (get SEMANT 'RESP))))
           (error (format nil "NO CLASS~a " CLASS) SEMANT)
	   (return nil))
	
	; %SET UP ANAPHS FOR NEXT INPUT  %
        (when (setf K (GET SEMANT 'ANAPH)) (addanaph K))

        (setf A (selsentence A))
        (return (when A (say A (cdr bond))))
	)
  )

;%-------------------------------------------------------------------%
;%-  SELSENTENCE  ---------------%
;% SELECTS AND RETURNS A  SENTENCE (AN car ), ADDING TO THE ANAPHORA LIST%
;% INPUT UNIT IS AN car  NUMBER -- LIKE car 17, CLASS IS LIKE NORMAL %
;% DELETES THE SENTENCE FROM MEMORY  %
;
;% CALLS ADDANAPH %

(defun selsentence (unit)
  (prog (SENTS S A ANAPH CLASS)
	(setf CLASS 'NORMAL)
        (unless (DISKREAD UNIT) (return nil)) ;  % READ FROM DISK INTO MEMORY %

	(setf A (GET UNIT CLASS)) ;  % USE NORMAL REPONSES AS DEFAULT %
	(setf ANAPH (GET UNIT 'ANAPH))
        (setf SENTS A)
	(when (NULL SENTS) 
	  (setf ?!EXHAUST T)
	  (RETURN NIL) ;  % IF NO SENTS THEN SET THE EXHAUST FLAG %
	  )
        ;%  IF EXH IS T, THEN TAKE THE SENTENCES IN ORDER, OTHERWISE CHOOSE RANDOMLY %
        (if (GET UNIT 'EXH) 
	  (setf A 1)
	  (setf A (parandom (LENGTH SENTS)))
	  )
	(setf S (nth A SENTS))
	(PUTPROP UNIT (DELETEN SENTS A) CLASS) ;% DELETE THE SENTENCE FROM MEMORY %

	(when (and (ATOM ANAPH) ANAPH) (setf ANAPH (EVAL ANAPH)))
	(ADDANAPH ANAPH) ;% ADD ANAPH FOR THIS SENTENCE %
	(return S)
	)
  )

;%       RETURNS THE LIST L MINUS THE NTH ELEMENT %

(defun  deleten (L N) 
  (if (= N 1) (cdr L) (cons (car L) (DELETEN (cdr L) (- N 1))))
  )

;% SAY  TAKES A LIST OF ENGLISH OR ARGUMENTS TO BE FILLED AND PRODUCES AN ENGLISH SENTENCE %
;
;        % L MAY BE A LIST OF ENGLISH WORDS REPRESENTING THE OUTPUT SENTENCE %
;        % L MAY ALSO BE A LIST OF ONE ELEMENT, WHICH IS A LIST OF ARGUMENTS TO EXPRESS %
;        % THE FIRST ARG SELECTS FROM ARGS WHICH ^H NUMBER TO TAKE %
;        % CALLS EXPRESS  %

(defun say (L ARGS)
  (if (and (not (ATOM (car L)))  
	   (NUMBERP (CAAR L)))
    (express (nth (caar L)  ARGS) (CADAR L))
    L)
  )

;% ANAPHORA ROUTINES, ADDANAPH  %
;
;% ADDANAPH ADDS STUFF FROM ANAPHS FROM car  ON TO GLOBAL ANAPHORA LIST %

(defun addanaph (L)
  (prog (a)
	(loop for i in L do
	      (if (setf a (ASSOC (CAR I) ?!ANAPHLISTNEW))
       ;         % IF ALREADY ON ANAPH LIST THEN REPLACE %
           (RPLACD A (CDR I))
	   (setf ?!ANAPHLISTNEW (cons I ?!ANAPHLISTNEW ))
	   )
	      )
	(return ?!ANAPHLISTNEW)
	)
  )


(defun parandom(N) 1) ;  % GETS REPLACE BY RANDOM.LAP, LOADED IN AFTER THIS FUNCTION %

;% SPECFN CALLS THE SPECIAL FN IF THERE IS ONE %

(defun specfn(STRUC)
  (prog (a name)
	(setf name (get struc  'UNIT)) ;IF not NAME THEN RETURN NIL;  % IE, NOT AN ANAPH %
	(when (member name '(GO_ON ELAB WHO WHAT)); % CALL THE SPEC FN PROTECTED BY ERRSET %
	  (if (ATOM (setf A (ERRSET (EVAL (list NAME NIL T )) NIL)))
	     (progn (paERROR "SPECFN" NAME) (setf A NIL))
	     (setf A (car A )))
	  (RETURN (IF A A 'QUIT))
	  )
        ;% QUIT MEANS THERE WAS AN ANAPHORA, BUT WE DIDNT HAVE THE POINTER FOR IT IN MEMORY %
        (when (ASSOC NAME ?!ALLANAPHS )
	  (setf A (GENL STRUC T NAME ))
	  (RETURN (IF A A 'QUIT )))
	)
  )

;% GO_ON, ELAB, WHO, WHAT
;        TRY TO GET THE ANAPHORA, ELSE USE A STORY %

(defun go_on(L F)
  (prog (a)
	(unless a (setf a (GET_ANAPH 'GO_ON)))
	(unless a (setf a (GET_STORY)))
	(when (and A F) (ANDTHEN (list 'IN (GET 'GO_ON 'UNIT))))
	(return a)
	)
  )

(defun  elab (L F)
  (prog (a)
	(unless A (setf A (GET_ANAPH 'ELAB)))
	(unless A (setf A (GET_STORY)))
	(when (and (not a) f) (setf A (GO_ON L NIL)))
	(when (and a f) (ANDTHEN (list 'IN (GET 'ELAB 'UNIT))))
	(return A)
	)
  )

(defun genl (L F ANAPH) ;% TRY THE ANAPH, ELSE TRY GO_ON  %
  (prog (a)
	(setf a nil)
        (unless A (setf A (GET_ANAPH ANAPH)))
	(unless A (setf A (GO_ON L NIL)))
	(return a)
	)
  )

;% GET_STORY GETS THE NEXT LINE IN WHATEVER STORY IS BEING TALKED ABOUT %
;%  IT GETS THE TOPIC EITHER FROM THE CURRENT INPUT, OR FROM THE PREVIOUS INPUT %
;%  IT THEN LOCATES THE STORY NAME, AND SELECTS THE NEXT ONE WHICH HASNT BEEN USED %

(defun get_story()
  (prog (b c) ; % TOPIC %
	(when (and (setf b (carn (get REACTTO 'TOPIC))) ; % TRY CURRENT INPUT TOPIC %
              ; %       ALREADY A SETNAME               GET CANONICAL WD AND SET  %
	         (if (GET B 'WORDS) 
		   T 
		   (and (setf B (CARN (SYNNYM B)))
			(setf B (GET B 'SET))))
            )
	    (setf c b))
	(when (and (not C) (setf B (CARN (GET ?!LAST_OUTPUT 'TOPIC) ))  ;% TRY PREVIOUS INPUT TOPIC %
              ;%       ALREADY A SETNAME               GET CANONICAL WD AND SET  %
		 (IF (GET B 'WORDS) 
		     T 
		     (and 
		       (setf B (CARN (SYNNYM B)))
		       (setf B (GET B 'SET)) )
		     ))
	  (setf C B))
	(unless c (return nil)) ; % NO STORY FROM EITHER %
	(when (setf b (get c 'STORY)) 
	  (DELETEP C (CARN B) 'STORY)
	  (RETURN (CARN B)))
	(when (memq c (get 'FLARELIST 'SETS)) (RETURN (FLSTMT C)))
	)
  )


;% GET_ANAPH TAKES AN ANAPH NAME AS INPUT (EG THEY, GO_ON, WHO) AND LOOKS
;  ON THE CURRENT ANAPH LIST IF ITS NOT NULL, ELSE LOOKS ON THE OLD ANAPHLIST.
;  IT ALSO USES THE TRANSLATIONS OF ANAPHORA FOUND ON ?!ALLANAPHS %

(defun get_anaph (L)
  (prog (ana b alist)
	(setf ALIST (IF ?!ANAPHLIST ?!ANAPHLIST ?!ANAPHLISTOLD))
	; % GET ALL ANAPHS WITH APPROX THE SAME MEANING %
        (setf ANA (ASSOC L ?!ALLANAPHS))
	(loop for J in ANA do 
	      (setf B (ASSOC J ALIST)) 
	      until B)
	(when (and B  (CDR B) (ATOM (CDR B))) (RETURN (CDR B)))
	(when (and (eq L 'THEY) (setf B (ASSOC L ?!ANAPHLISTOLD))) (RETURN (CDR B)))
	)
  )

(defun  who (L F)
  (prog (a)
	(setf a nil)
        (setf A (get_anaph 'WHO))
	(if (LAMBDANAME A) (RETURN A) (setf A NIL))
	(unless a (stf A (GO_ON L NIL)))
	(RETURN A)
	)
  )

(defun  what (L F)
  (prog (a)
        (setf A (GET_ANAPH 'WHAT))
	(if (and L (equal (cadr (GET A 'BONDVALUE)) (car L))) T (setf A NIL))
	(unless A (setf A (GO_ON L T)))
	(RETURN A)
	)
  )

;% REPETITION RETURNS T IF SEM ( A ^H NUMBER ) HAS BEEN USED BEFORE AS TYPE (IN OR OUT) %
(defun repetition (SEM TYPE)
  (prog (PTR FOUND)
	(setf PTR  ?!CLIST)
	(loop while (and PTR (not FOUND)) dO
	      (IF (and (eql (car  PTR) TYPE) (eql (cadr PTR) SEM) (nequal PTR ?!LASTIN))
		  (setf FOUND T)
		  (setf PTR  (CDR PTR))))
	(RETURN FOUND)
	)
  )

;%  READING INDEXES,   SETUPSTL    %
;%   THESE ROUTINES READ IN DATA FILES AND INTERCONNECT THEM %
;
;% SETUPSTL  SETS UP STORY AND STORYLIST POINTERS %
(defun setupstl () ;% MUST BE DONE AFTER INITIALIZE IN OPAR3  %
  (prog (a b c)
	(setf c T)
        (setf a (loop for I in (GET 'SENSITIVELIST 'SETS) collect (get I 'WORDS)))
	(PUTPROP 'SENSITIVELIST  A  'WORDS);; now sensitivelist.words directly points all words
	(setf A (cons 'DELNSET (append (GET 'FLARELIST 'SETS) (GET 'SETLIST 'SETS) )))
	(loop for I in A do ;% I IS THE NAME OF A FLARESET %
	      for J in (GET I 'STORY) do ;because flareset's i only have story 
	      (progn ;% J IS A @@-NAME % ;;; I dont know omura
                (setf B J)
		(if B (PUTPROP B I 'STORYNAME) 
		  (progn 
		    (setf C NIL) 
		    (print J)
		    )
		  )
		)
	      )
        (RETURN (IF C '(SET UP OK) '(SET UP BAD) ))
	)
  )

(defun readbonds (FILENAME) ;% THIS READS IN A FILE(PDATB) CONTAINING USEFUL INFORMATION ABOUT PDAT %
  (prog (a)
        (SELECTINPUT NIL FILENAME)
	(loop while (and (not (atom (setf A (ERRSET(READDATA))))) 
			 (setf A (car A))) do
                (EVAL A))
        (PRINTSTR (format nil "~a read in." FILENAME))
	(SELECTINPUT NIL NIL)
	)
  )

(defun  changel (FILENAME)
        ;% THIS READS IN A FILE OF TEMPORARY @@ NUMBER CHANGES TO MAKE THE PATTERN MATCHER
        ;  AND MEMORY COMPATIBLE %
  (prog (a)
	(SELECTINPUT NIL FILENAME)
	(loop while (and (not (ATOM (setf A (ERRSET(READDATA)))))
			 (setf A  (car A))) do
                (PUTPROP (car A) (cdr A) 'MEQV))
	(PRINTSTR (format nil "~a read in." FILENAME))
	)
  )

(format t "end of loading pmem.lisp~%")

