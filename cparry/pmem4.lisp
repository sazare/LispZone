; read s-exp from a file

(defun  init() (selectinputn '(PAR BLF) INPUTFILE))
(defun  initfb() 
  (progn 
    (EVAL '(DSKIN(RANDOM.LAP)))
    (setf LAMDA 8)
    (initf)
    (initb)
    )
  )

(defun  testm () (test_pattern))
(defun  lambdaname (L) (equal (chrval L) LAMDA))

(defun bl (b)
  (if (not (atom b)) 
    (progn (format t "BL not ATOM ~a" b) NIL)
    (if (eq (get b 'class) 'INN) 
      (>= (get b 'ntruth) 5)
      (get b 'truth)
      )
    )
  )


(defun angermode ()
  (if (>= ANGER 17.5) 
    (choose 'ANGER)
    (choose 'HOSTILEREPLIES)
    )
  )

(defun fearmode ()
  (if (>= FEAR 18.4)
    (progn
      (setf ENDE t)
      (choose 'exit)
      )
      ;DISTINGUISH BETWEEN QUESTIONS AND STATEMENTS OF 'OTHER 
    (if (and (not (bl 'DDHARM))
	     (bl 'DHELPFUL)
	     (not (bl 'DMAFIA)))
      (PROGN
	(decf FEAR)
	nil
	)
      (if (equal STYLE 'Q)
	(choose 'THREATQ)
	(choose 'AFRAID))
      )
    )
  )

(defun angerfearmode (topic)
  (if (or (memq topic (get 'FLARELIST 'SETS))
	  (memq topic '(MAFIA BYE IYOUME STRONGFEELINGS FEELINGS GAMES))
	  )
    nil
    (if (>= FEAR 14) (fearmode) (angermode))
    )
  )

;% TOPICANALYZE  RECORDS THE NUMBER OF OLD TOPICS, CHANGED TOPICS, AND THE PREVIOUS TOPIC %
(defun topicanalyze ()
  (prog ()
	(unless STPIC (return nil))
        (when (memq STOPIC '(ANAPH FACTS STRONGFEELINGS GREETINGS)) (return nil))
        (when (eql STOPIC OLDTOPIC) (return nil))
        (setf NEWTOPICNO (+1 NEWTOPICNO))
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

(defun addh(L) (for i in L do (setf HLIST (CONS I HLIST))))

; % REACT2  CALLS REPLYR WITH APPROPRIATE ARGUMENTS AND ENTERS THE INPUT ON THE CONVERSATION LIST %

(defun react2 (B)
  (prog (a) 
	(setf ?!EXHAUST nil)
        (unless (LAMBDANAME B) 
	  (paerror "NONLAMBDA INTO REACT2" B)
	  (return nil))
	(when ?!OUTPUT ;    % ALREADY HAVE SENTENCE IN ?!OUTPUT %
          (andthen (list 'IN B))
	  (andthen (list 'OUT nil))
          (return T))
	(unless (diskread B) 
	  (error "REACT2 ERROR BAD DISKREAD" B)
	  (return nil))
	(andthen (list 'IN B))
	(return (replyr B))
	)
  )

(defun react (x ) x)
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
  (let (a b)
    (when save_dump (savejob save_dump 'sav))
      ;; Program will start here again if system clashes
    (unless (not_last_input) 
      (terpri)
      (format t "READY:")
      (force-output t)
      )
    (experiment)
    (setf a (testm)) 
    (if (atom a) 
      (format t "Pattern match error")
      )
    (setf a (car a))
    (setf PM2INPUT PMINPUT)
    (setf PMINPUT a)
    (if (= (length SSENT) 1)
      (setf a (choose 'silence)))
    (analyze t)
    (if (not (lambdaname a)) (setf a nil))
    (if (and a (atom a) (setf b (get a 'meqv)))
      (setf a b))
    (setf REACTINPUT a)
    (window 9 T a)
    (readlambda a) (window 9 nil (get a 'bondvalue))
    (if (not (react (list a (q ssent) ssent)))
      (format t "error in react ~a" ssent)
      )
    (if ende 
      (progn 
	(setf tracev (not tracev))
        (modifyvar)
	(winxit)
	(swapp)
	(exit)
	))
    )
  )

(defun parry ()
  "parry main function. dont direct trans."
  (prog ()
    (terpri t)
    (format t "READY:~%")
    (loop 
      (let ((ind nil))
	(format t "> ~0%") (force-output t)
	(setf ind (read-line))
	(if (equal ind "bye") (return))
	(ERRSET (parry2 ind) nil)
      )
    )
  (format t "Good bye")
  )
)

(defun initb ()
  (let ()
    (eight)
    (nouud T)
    (dparinitialize)
    (setupstl)
    (setf INPUTFILEAREA '(1 3))
    (setf DIAFILEAREA '(DIA KMC))
    (gcgag nil)
    (changel 'CHANGE)
    (setf DELNO 0)
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

(defun read-file(fname)
    (with-open-file (in fname :direction :input)
      (loop for line = (read in nil nil)
	    while line 
	    collect line
      )
    )
)
;; dont use this
(defun readsexp (fname)
  (let (alls)
    (nyi)
  (with-open-file (stream fname :direction :input)
    (do ((sexp (read stream nil)(read stream nil)))
        ((null sexp))
	(progn (print sexp)
	       (unless (eq sexp '~) (setf alls (cons sexp alls))))
	)
  (reverse alls)
  )
  )
)

(defvar *INTLIST* NIL)

(defvar *belfile* "bel0")
(defvar *inffile* "inf0")
;(defvar *inffile* "inf1")

(defun make-bel (bel)
  (let (B)
    (progn 
      (setf (get (car bel) 'NTRUTH) (cadr bel))
      (if (eq (caddr bel) 'INN) 
        (setf (get (car bel) 'CLASS) (caddr bel)))
      (if (setf B (cdddr bel))
        (progn 
          (setf (get (car B) 'NTRUTH) (cadr B))
	        (setf (get (car B) 'OPPOS) (car bel))
	        (setf (get (car bel) 'OPPOS) (car B))
	      ))    
     (car bel)
    )
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

(defun read-inf ()
  (let (inf)
    (setf inf (read-file *inffile*))
    inf
    )
  )


(defun make-infs (infs)
  (prog (b )
    (loop for a in infs
;;; I may missunderstand READDATA in
;;;  WHILE not ATOM(A ?  ERRSET(READDATA()) ) AND A ?  car A DO
      do
        (prog ()
	  (if (atom a) (return))
          (if (memq (car a) '(TH2 EMOTE))
	      (loop for i in (cddr a) do 
		    (when (atom i) ;; this when is needed for 
;;;  error at (setf (list) 'TH2) corr. (putprop (list) 'TH)
;;; at (TH2 (DDHARM 3) ((MEASURE FEAR 14) DDOMINATING) )
;;; if ignore the measure form, when evaluate where it comes from???
		      (setf (get i (car a)) (cons (cadr a) (get i (car a)))))
		    )
	      (prog ()
	        (setf b a)
	        (if (get (car a) 'theorem) 
		  (progn (format t "~%duplicate inf: ~a" (car a))(force-output t)))
	        (setf (get (car a) 'theorem) (cons (cadr a) (caddr a)))
	        (if (not (get (carn (cadr a)) 'ntruth)) 
		  (progn (format t "~%NO BEL: ~a" (cadr a))(force-output t)))
                ;; backpointer from antecedent to th name
	        (loop for i in (caddr a) do
	          (if (atom i) 
		    (progn
		      (setf (get i 'TH) (cons (car a) (get i 'th)))
		      (if (and (not (lambdaname i)) 
			     (null (get i 'ntruth)))
		      (format t "~%no BEL: ~a" i)(force-output t)))
		    )
		  )
		)
	      )
	  )
	)
        (format t "~%INF file read, last inf: ~a"  (car b))
	(force-output t)
    )
)

(defun readinf ()
  (make-infs (read-inf))
  )


(format t "end of loading pmem4.lisp~%")

