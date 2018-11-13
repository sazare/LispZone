;;; from PMEM

;;;

(defun lambdaname (s) (equal (subseq (string s) 0 2) "@@"))
;; now replaced ^B to car, alphaname is incorrect
;; but noone call alphaname
(defun alphaneme (s) (equal (subseq (string s) 0 2) ""))

;% READLAMBDA ATTEMPTS TO READ IN A SEMANTIC FUNCTION (FRAME) FROM THE MEMORY
;        GIVEN A LAMBDA NUMBER AS INPUT %
(defun readlambda (a)
  (prog ()
    (unless (lambdaname a) (return nil))
    (if (diskread a) 
      t
      (error "BAD DISKREAD" )
      )
    )
  )

;%        DOES DISKREAD2 PROTECTED BY AN ERRSET;
;         ERRORS MAY OCCUR IF THE MEMORY FILE HAS MISMATCHED PARENS  %

(defun diskread (name)
  (prog (a)
    (if (setf a (diskread2 name)) 
      (progn 
	(format t "error in diskread ~a" name)
        (return nil))
      (car a))
  )
)

;%       GIVEN A ^H OR car  NUMBER AS INPUT, READS IN ONE DISK SEXPR
;        IF THE SEXPR IS ALREADY IN CORE, IT JUST RETURNS T
;        OTHERWISE IT CALLS BEL OR ENG TO PROPERLY LINK ALL THE INFO INTO THE MEMORY %

(defun diskread2 (name)
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

;carn was defined in diaapp.lisp
(defun carn (s)
  (if (atom s) 
    s
    (car s)
    )
  )

(defun bel(x)
  (prog (name truth unit)
    (setf name (car x))
    (setf truth (cadr x))
    (setf unit    (caddr x))
    (when (or (null x)(null (cdr x))(null (cddr x))(not (numberp truth)))
      (error "B BAD INPUT ~a" x)
      (return nil)
    )
    (when (get name 'bondvalue) (error "BAD INPUT-DOUBLE ENTRy ~%" name))
    (putpropt name unit 'BONDVALUE)
    (setf x (cdr x))
    (loop while (setf x (cddr x)) do
       ;% PUT THEM ON THE PROPERTY LIST OF THE ^H NAME %
      (when (or (not (atom x)) (null (cdr)))
         (error "BAD INPUT ~%~%" name)
         (return nil)
       )
       (putprop name (cadr x) (car x)))
    (return name)
  )
  )

(defun eng (x)
  (prog (unit error)
	(when (or (null x) (null (cdr x))(null (cddr x)))
	  (paerror paerror "E BAD INPUT" X)
	  (return nil))
	(setf UNIT (CAR X))
	(when (or (GET UNIT 'NORMAL)  (GET UNIT 'EMBQ))
	  (paerror("BAD INPUT-DOUBLE ENTRY" UNIT)))
	(setf X (CDR X))
	(when (eq (CAR X) 'ANAPH)
	  (setf X (CDR X)) ;% PUT ANAPH ON PROPERTY LIST %
	  (PUTPROP UNIT (CAR X) 'ANAPH)
	  (setf X (CDR X)))
	(when (eq (CAR X) 'EXH)
	  (PUTPROP UNIT (CADR X) (CAR X))
	  (setf X (CDDR X)))
	(loop DO 
	      ; % PUT SENTENCES ON THE PROPERTY LIST %
	      (when (or (NULL X) (NULL (cdr X)) (ATOM (cdr X)) (not (ATOM (car X))))
		(paerror "E BAD INPUT" UNIT)  
		(setf ERROR T)
		(RETURN NIL))
	      (PUTPROP UNIT (CADR X) (CAR X))
	      until (or ERROR (not (setf X (CDDR X))))
	      )
        (unless (not (GET UNIT 'NORMAL))
	  (paerror "NO NORMAL SENTS" UNIT))
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
	  (error NIL "NOSEMANT IN REPLYR")
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
	(while (equal ?!LAST_ANDTHEN (car THING)) (return nil))
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
        (when (and (null a) BOND (setf C (get (car bond) UNIT))(DISKREAD C))
           (setf A (get c CLASS)))

	; %  ONLY QUIT IF THERE IS NO RESP   %
        (when (and (null a) (null (setf a (get SEMANT 'RESP))))
           (error "NO CLASS~a ~a " CLASS SEMANT)
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
        (unless (not (DISKREAD UNIT)) (return nil)) ;  % READ FROM DISK INTO MEMORY %
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
	  (setf A (RANDOM (LENGTH SENTS)))
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








(format t "end of loading pmem.lisp~%")

