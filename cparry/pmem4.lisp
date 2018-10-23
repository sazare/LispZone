; read s-exp from a file
(load "diaapp.lisp")
(load "pmem.lisp")

(defun testm ()
  (test_pattern)
  )

(defun angermode ()
  (if (>= *ANGER* 17.5) 
    (choose 'ANGER)
    (choose 'HOSTILEREPLIES)
    )
  )

(defun fearmode ()
  (if (>= *FEAR* 18.4)
    (progn
      (setq *ENDE* t)
      (choose 'exit)
      )
      ;DISTINGUISH BETWEEN QUESTIONS AND STATEMENTS OF 'OTHER 
    (if (and (not (bl 'DDHARM))
	     (bl 'DHELPFUL)
	     (not (bl 'DMAFIA)))
      (PROGN
	(decf *FEAR*)
	nil
	)
      (if (equal *STYLE* 'Q)
	(choose 'THREATQ)
	(choose 'AFRAID))
      )
    )
  )

(defun angerfearmode (topic)
  (if (or (member topic (get 'FLARELIST 'SETS))
	  (member topic '(MAFIA BYE IYOUME STRONGFEELINGS FEELINGS GAMES))
	  )
    nil
    (if (>= *FEAR* 14) (fearmode) (angermove))
    )
  )



(defun react ())
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
;;;        -> when *ende* {
;;;              tracev = not(tracev); 
;;;              modifvar() 
;;;              winxit();
;;;              swap()
;;;              exit()
;;;            }
;;; question
;;; 1. *ssend*, *ende*はどこで設定されるのか
;;; 2. exit()の定義がないが、parry()を終わるのだろう
;;; 3. prop meqvはどこで最初に設定されるか
;;; 4. 重要そうな関数は
;;;    - experiment()はパラメタ変化の実験用コード。
;;;    - lambdanameには'meqvというプロバティがある
;;;    - readlambda(), react(), 
;;;    - readlambda(), react(), 
;;;    - *ende*のときは終了だから、modifvar()はあまり重要ではないだろう
(defun parry2 (ind)
  (let (a b)
    (if *save_dump* (savejob *save_dump* 'sav))
      ;; Program will start here again if system clashes

    (format t "~a~%" ind) (force-output t)
    (format t "> ~0%") (force-output t)
    (experiment)
    (setf a (testm)) 
    (if (atom a) 
      (format t "Pattern match error")
      )
    (setf a (car a))
    (setf *pm2input* *pminput*)
    (setf *pminput* a)
    (if (= (length *ssent*) 1)
      (setf a (choose 'silence)))
    ;; (analyze t)
    (if (not (lambdaname a)) (setf a nil))
    (if (and a (atom a) (setf b (get a 'meqv)))
      (setf a b))
    (setf *reactinput* a)
    (window 9 t a)
    (readlambda a) (window 9 nil (get a 'bondvalue))
    (if (not (react (list a (q *ssent*) *ssent*)))
      (format t "error in react ~a" *ssent*)
      )
    (if *ende* 
      (progn 
	(setf *tracev* (not *tracev*))
        (modifyvar)
	(winxit)
	(swapp)
	(exit)
	))
    )
)

(defun parry ()
  "parry main function. dont direct trans."
  (progn
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
	       (unless (eq sexp '~) (setq alls (cons sexp alls))))
	)
  (reverse alls)
  )
  )
)

(defvar *INTLIST* NIL)

(defvar *belfile* "bel0")
;(defvar *inffile* "inf0")
(defvar *inffile* "inf1")

(defun make-bel (bel)
  (let (B)
    (progn 
      (setf (get (car bel) 'NTRUTH) (cadr bel))
      (if (eq (caddr bel) 'INN) 
        (setf (get (car bel) 'CLASS) (caddr bel)))
      (if (setq B (cdddr bel))
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
          (setq pre (make-bel b))
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
  (let (b )
    (loop for a in infs
;;; I may missunderstand READDATA in
;;;  WHILE not ATOM(A ⇦  ERRSET(READDATA()) ) AND A ⇦  car A DO
      do
        (progn
	  (if (atom a) (return))
          (if (member (car a) '(TH2 EMOTE))
	      (loop for i in (cddr a) do 
		    (when (atom i) ;; this when is needed for 
;;;  error at (setf (list) 'TH2) corr. (putprop (list) 'TH)
;;; at (TH2 (DDHARM 3) ((MEASURE FEAR 14) DDOMINATING) )
;;; if ignore the measure form, when evaluate where it comes from???
		      (setf (get i (car a)) (cons (cadr a) (get i (car a)))))
		    )
	      (progn 
	        (setq b a)
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


(defun bl (b)
  (if (not (atom b)) 
    (progn (format t "BL not ATOM ~a" b) NIL)
    (if (eq (get b 'class) 'INN) 
      (>= (get b 'ntruth) 5)
      (get b 'truth)
      )
    )
  )

			    
