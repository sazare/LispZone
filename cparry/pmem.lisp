;;; from PMEM

;;; MASTER LIST OF GLOBAL VARIABLES
;;;; ***GLOBAL VARIABLES***

;!ANAPHLIST      is the current list of anaphora dotted-pairs.
;!ANAPHLISTOLD   is the previous list of anaphora dotted-pairs.
;!ANAPHLISTNEW   is the next list of anaphora dotted-pairs to go into anaphlist
;!ALLANAPHS      is a list of lists - ((who they he)(he who)(they who)....)
;!CLIST          is a pointer to the first element of the conversation list.
;!CLAST          is a pointer to the last element of the conversation list.
;!LAST_ANDTHEN   is either IN or OUT as the last ANDTHEN processed
;!LASTIN         points to atom under which is stored the last semantic unit
;                inputed by the doctor.
;!LASTOUT        points to the atom under which is stored the last semantic
;                unit outputed by Parry.
;
;!ERROR          contains a list of errors made
;!EXHAUST        is true if a set of responses is exhausted and the exhaust responses are to be used
;                  it is set by SELECT_SENTENCE  and used at the end of REACT2
;!OUTPUT         is the output of parry, to be sent whereever
;!LAST_OUTPUT    is the ^H-name of the last output
;
;INPUTQUES       is a list of dotted pairs from the pattern matcher to the memory
;                        each pair is (canonical 5-letter atom . input word)




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
    (if (setq a (diskread2 name)) 
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
    (when (get 'DSKLOC 'SUBR) (setq a (dskloc name))) ; % GET THE CHAR NO OF THE SEXPR %
    (unless a (return nil)) ; % NOT THERE %
    (setf CHARNO A)
    ; % CHARNO IS THE CHARACTER NUMBER IN THE FILE THAT THE SEXPR NAME BEGINS ON %
    ; % IF A RECNO IS NEEDED, IT IS CHARNO/OCTAL 1200 + 1 (FIRST RECORD IS REC 1 %

    (setf CHANSAVE (INC NIL NIL))
    (init);% INITIALIZE THE READ CHANNEL FOR THE MEMORY FILE %
    (inc INCHAN NIL)
    (CHSETI INCHAN CHARNO) ;  % SET THE INPUT POINTER TO THE CORRECT SEXPR %
    (setq B (READ))
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
(nyi)
  )

(defun eng (x)
(nyi)
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
	(setq A (express SEMANT 'RESP))
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
	(setq a (get SEMANT CLASS))
	(setq bond (get SEMANT 'BONDVALUE))

	; % USE PREDICATE FOR FINDING CLASS %
        (when (and (null a) BOND (setq C (get (car bond) UNIT))(DISKREAD C))
           (setq A (get c CLASS)))

	; %  ONLY QUIT IF THERE IS NO RESP   %
        (when (and (null a) (null (setq a (get SEMANT 'RESP))))
           (error "NO CLASS~a ~a " CLASS SEMANT)
	   (return nil))
	
	; %SET UP ANAPHS FOR NEXT INPUT  %
        (when (setq K (GET SEMANT 'ANAPH)) (addanaph K))

        (setq A (selsentence A))
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
  (nyi)
  )
        




(format t "end of loading pmem.lisp~%")

