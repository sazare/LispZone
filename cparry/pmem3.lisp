;%  SELECTINPUT, READINC, READDATA  %
;% ************ THESE ARE ALL INPUT-OUTPUT ROUTINES *************%
;
;% takes a file name (or NIL for TTY) and assigns it to the input channel%
;;;; stub
(defun chseto (x y) (list x y))
(defun outc (x y) (list x y))
(defun at (x) x)
(defun tyo (x) x)
(defun tyi () T)
(defun octal (x) x)
(defun readch () T)

;;;;

(defun selectinput (AREA FILE)
  (IF FILE 
      (progn 
	(if AREA 
	  (setf INCHAN (EVAL (list 'INPUT AREA FILE)))
	  (setf INCHAN (EVAL (list 'INPUT 'DSK\: FILE)))
	  )
	(INC NIL NIL)
	(PRINTSTR (format nil "~a SELECTED FOR INPUT." file)))
      (progn 
	(PRINTSTR "TTY SELECTED FOR INPUT.")
	(INC NIL T))
      )
  )

(defun selectinputn (AREA FILE)
  (when FILE 
    (list AREA)
    (setf INCHAN (EVAL (list 'INPUT INPUTFILEAREA FILE)))
    (INC NIL NIL))
  )

(defun  readinc ()
  (prog (I)
	(INC INCHAN NIL)
	(setf I (READ))
	(INC NIL NIL)
	(RETURN I)
	)
  )

(defun readdata ()
  (prog (A)
	(setf A (READINC))
	(when (eq A 'COMMENT)
	    (loop do (setf A (READINC)) 
		  until (or (eq A 'C?^V?) (eq A '?^V?))) ;;om what is this??
            (setf A (READINC) )
	)
	(RETURN A)
	)
  )

;% INIT_FILE, INIT_TO_FILE, TO_FILE, ERROR_FILE %
;% ************* THESE ARE ROUTINES TO SAVE DIALOGS AND ERRORS ************ %

(defun INITFILE (L)  (INIT_FILE L))

(defun INIT_FILE(L)
  (prog (FILCHAN I) 
	;SPECIAL DIAFILEAREA,ONEDIA,FILE12;
        (setf EOF PERCENT)
	(TEN)
	(setf SAVE_FILE T)
	(setf FILCHAN  (EVAL (list 'INPUT DIAFILEAREA '(PAR2.FIL) )))
	(INC FILCHAN NIL)
	(setf I (READ))
	(setf FILE1 (cons (AT (format nil "P~a" I)) 'DIA))
	(setf FILE2 (cons (AT (format nil "P~aA" I)) 'DIA))
        (INC NIL NIL)
	(eval (list 'OUTPUT DIAFILEAREA '(PAR2.FIL)))
	(OUTC T NIL)
	(PRINT (+ I 1))
        (OUTC NIL T)
        (INIT_TO_FILE L)
  )
)


(defun init_to_file (L)
  (prog (FILCHAN)
	(setf FILCHAN (EVAL (list 'OUTPUT DIAFILEAREA FILE1)))
	(OUTC FILCHAN NIL)
	(PRINTSTR (format nil "~a      " L))
	(TERPRI)(princ EOF)  ;; ??? EOL
	(OUTC NIL T)
	(when (and (not SUMEX) (not ONEDIA))
	  (setf FILCHAN (EVAL (list 'OUTPUT DIAFILEAREA FILE2)))
	  (OUTC FILCHAN NIL)
	  (PRINTSTR (format nil "~a        " (str L)))
	  (OUTC NIL T)
	)
        (setf DIACHARNO (+ (length L) 4))
	(setf FILE12 FILE1)
	(TO_FILE LF LF LF)
	(RETURN DIACHARNO)
	)
  )

(defun to_file (L M N)
  (IF (not SUMEX) 
      (TO_FILE1 L M N) 
      (TO_FILE2 L M N))
  )

(defun to_file1 (L M N)
  (prog (FILCHAN C D FLAG)
	(setf FLAG NIL)
        (setf CHANSAVE (INC NIL NIL))
	(setf FILCHAN (EVAL (list 'INOUT DIAFILEAREA FILE1))) 
	(OUTC FILCHAN NIL)
	(CHSETO FILCHAN  DIACHARNO);

	(BUFFER T)(PRINTSTR L)(PRINC M)(PRINC TAB)(PRINC N)(PRINTSTR(TIMESTAT))(TERPRI NIL)(BUFFER NIL)
	; % THIS IS TO CORRECT A BUG IN CHSETO WHICH SETS IT TO A MINUS NUMBER
        ;   IF IT HITS A RECORD BOUNDARY EXACTLY  %
        (setf C (CHSETO FILCHAN NIL))
        (when (MINUSP C) 
	 (paERROR "TO_FILE MINUS" L) 
	 (progn
	   (OUTC NIL T) ;% TO NOT GROW CORE LIKE IT WAS BEFORE %
	   (setf FILCHAN (EVAL (list 'INOUT DIAFILEAREA FILE1 )))
	   (OUTC FILCHAN NIL)
	   (CHSETO FILCHAN DIACHARNO)
           (BUFFER T)(PRINTSTR L)(PRINC M)(PRINC TAB)(PRINC N)(PRINTSTR (TIMESTAT))(TERPRI NIL)(BUFFER NIL)
	   )
         (PRINTSTR "        ")
	 (setf D (CHSETO FILCHAN NIL) )
	 (setf FLAG T)
	 (when (MINUSP D) (paERROR "TO_FILE MIN AGAIN" L))
	 (unless ONEDIA 
	    (progn 
	      (OUTC NIL T)
	      (setf FILCHAN (EVAL (list 'INOUT DIAFILEAREA FILE2)))
	      (OUTC FILCHAN NIL)
	      (CHSETO FILCHAN DIACHARNO)
	      (BUFFER T)(PRINTSTR L)(PRINC M)(PRINC TAB)(PRINC N)(PRINTSTR(TIMESTAT))(TERPRI NIL)(BUFFER NIL)
	      (when FLAG (PRINTSTR "        "))))
	  (setf DIACHARNO (CHSETO FILCHAN NIL))
	  (OUTC NIL T)
	  (INC CHANSAVE NIL)
	  (RETURN DIACHARNO)
	  )
	)
  )

(defun to_file2 (L M N)
  (prog (F CH) ;% FILE12 HAS THE CURRENT GOOD FILE %
	(setf F FILE12)
	(setf FILE12 (IF (equal FILE12 FILE1) FILE2 FILE1)) ;% SWITCH NAMES %
	(setf CHANSAVE (INC NIL NIL))
	(setf FILCHAN (EVAL (list 'OUTPUT DIAFILEAREA FILE12)))
	(OUTC FILCHAN NIL)
	(setf INCHAN (EVAL (list 'INPUT DIAFILEAREA F)))
        (INC INCHAN NIL)
	(BUFFER T)
	(PRINC INPUTNO) (READ) ;% READ THE NUMBER AND THROW IT AWAY %
        (loop do  (list t)
           until (and (eq (TYO (TYI)) (OCTAL 45) )
                      (or (ATOM (setf CH (ERRSET(readch) NIL)))
		          (and (PRINC (CAR CH))  NIL) )
	         )
           )
	(BUFFER T)(PRINTSTR L)(PRINC M)(PRINC TAB)(PRINTSTR N)(TERPRI NIL)(BUFFER NIL)
	(terpri nil) (PRINC EOF)
	(OUTC NIL T)(INC NIL T)(INC CHANSAVE NIL)
  )
)



(defun error_file (A)
  a
;  (prog (FILCHAN FILENAME I)
;	(TEN)
;	(setf CHANSAVE (INC NIL NIL))
;	(setf FILCHAN (eval (list 'INPUT DIAFILEAREA '(ERR.FIL))))
;	(INC FILCHAN NIL)
;	(setf FILENAME (AT (format nil "P~a" (setf I (READ())))))
;	(INC CHANSAVE NIL)
;	(EVAL (list 'OUTPUT DIAFILEAREA '(ERR.FIL)))
;	(OUTC T NIL)
;	(PRINT (+ I 1))
;	(OUTC NIL T)
;
;	(OUTC (EVAL (list 'OUTPUT DIAFILEAREA (cons FILENAME 'ERR ) )) NIL)
;        (PRINT A);
;	(OUTC NIL T)
;	(return a)
;	)
  )



(format t "end of loading pmem3.lisp~%")

