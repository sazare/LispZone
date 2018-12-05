(load "setting.lisp")

(defun nyi() (format t "NOT YET IMPLEMENT"))

(defvar *aodefplist* '())
(defvar *aoputplist* '())

(defmacro defprop (sym val prop)
  `(progn
    (setf (get ',sym ',prop) ',val)
    (when *h3i2-on* (push ',sym *aodefplist*))
    )
  )

(defun putprop (sym val prop)
  (progn
    (setf (get sym prop) val)
    (when *h3i2-on* (push sym *aoputplist*))
    )
  )

(defun printstr (x)
  (format t x)
  )

(defun chrval (x)
  "return the charcode of x maybe"
  x ;; now it seems be enough
  )

(defun nequal (x y) (not (equal x y)))
(defun neq (x y) (not (eq x y)))

(defun gcgag (x) (if x (gc) (gc))) ; just ignore x
(defun bakgag (x) x)
(defun nouuo (x) x)

(defun memq (x y) (member x y))
(defun greaterp (x y) (> x y))
(defun lsh (x y) (* x (expt 2 y))) ;; uncertain
(defun ttyuu () );; uncertain
(defun divide (x y) (floor x y))

;(defun paerror (x y) (format t "~a ~a" x y))

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

;; different from original lambdaname. because cwchanged ^H to @@.
(defun lambdaname (s) (and (>= (length (string s)) 3) (equal (subseq (string s) 0 2) "@@")))

(defun numval (x) x)
(defun runtim (v) v)
(defun readlist (x) x)

(defun synnym (x) (progn x (setf synonyms (read-file "data/synonm.alf"))))
(defun spat (x) (progn x (setf spats (read-file "data/spats.sel"))))
(defun cpat (x) (progn x (setf cpats (read-file "data/cpats.sel"))))
(defun initfn (x) x)
(defun DSKLOC (x) x)

(defun allow () )
(defun namep () )

(defun timeuu () )
(defun dateuu () )
(defun speak () )

(defun explodec (x) x)
(defun explode (x) x)
(defun charval (x) x)
(defun billp () )

(defun canonize (x) x)

(defun paerror (mess L) ;; I dont want to use this. error_file go out of bound
  (setf ?!ERROR (cons (error_file (list mess L PM2INPUT PMINPUT FILE1 BUG)) ?!ERROR))
  )

(defun chseti (x y) (list x y))
(defun swapp () (format t "swapp is stub~%"))
(defun modifyvar () (format t "modifyvar is stub~%"))

(format t "end of loading primitives.lisp~%")

