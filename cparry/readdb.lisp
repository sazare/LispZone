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
    (format t "BELIEF file read, last belief: ~a" pre)
  )
 )

(defun read-bel ()
  (let (bel)
    (setf bel (read-file *belfile*))
    bel
    )
  )

;;(defun read-inf ()
;;  (let (inf)
;;    (setf inf (read-file *inffile*))
;;    inf
;;    )
;;  )
;;
;;(defun make-infs (infs)
;;  (let (b)
;;    (loop for a in infs
;;      collect 
;;	(if (atom a) (return))
;;        (if (member (car a) '(TH2 EMOTE))
;;	  (loop for i in (cddr a) do (setf (get i (car a)) (cons (cadr a) (get i (car a)))))
;;	  (setq b a)
;;	  (if (get (car a 'THEOREM) (format t "duplicate inf: ~a" (car a))))
;;	  (setf (get (car a) 'THEOREM) (cons (cadr a) (caddr a)))
;;	  (if (not (get (carn (cadr a)) 'ntruth) (format t "NO BEL: ~a" (cadr a))))
;;	  (loop for i in (caddr a) do
;;		(if (atom i) 
;;		  (progn
;;		    (setf (get i 'TH) (cons (car a) (get i 'th))))
;;		    (if (and (not (lambdaname i) (not (get i 'NTRUTH))))
;;		      (format t "no BEL: ~a" i))))
;;	  )
;;	)
;;    (format t "INF file read, last inf: ~a"  (car b))
;;    )
;;)
;;
;;
