;; read s-exp from a file

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
(print bel)
      (setf (get (car bel) 'NTRUTH) (cadr bel))
      (if (eq (caddr bel) 'INN) 
        (setf (get (car bel) 'CLASS) (caddr bel)))
      (if (setq B (cdddr bel))
        (progn 
          (setf (get (car B) 'NTRUTH) (cadr B))
	        (setf (get (car B) 'OPPOS) (car bel))
	        (setf (get (car bel) 'OPPOS) (car B))
	      ))    
    )
   (car bel)
  )
)

(defun make-bels (bels)
  (loop for b in bels 
    collect (push (make-bel b) *INTLIST*)
  )
  (setf *INTLIST* (reverse *INTLIST*))
)
(defun read-bel ()
  (let (bel)
    (setf bel (read-file *belfile*))
    bel
    )
  )

(defun read-inf ()
  (let (inf)
    (setf inf (read-file *inffile*))
    inf
    )
  )


