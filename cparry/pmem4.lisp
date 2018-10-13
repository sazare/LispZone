;; read s-exp from a file
(load "diaapp.lisp")
(load "pmem.lisp")

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
(defvar *inffile* "inf0")

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

			    
