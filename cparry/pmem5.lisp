(defun experiment ()
  ;; EXPERIMENTS OF RAISING AND LOWERING SHAME
  (unless (or EXPERIMENT (eq EXPERIMENT 'SEVEN))
    (if (= 7 INPUTNO) (setf HURT (+ HURT 5)))
    (if (= 17 INPUTNO) (setf HURT (- HURT 5)))
  )
)



(defun anddo (L M) L)

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

