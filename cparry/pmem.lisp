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


(format t "end of loading pmem.lisp~%")

