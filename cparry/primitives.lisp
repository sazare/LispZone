(load "setting.lisp")

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

(defun memq (x y) (member x y))
(defun greaterp (x y) (> x y))
(defun lsh (x y) (* x (expt 2 y))) ;; uncertain
(defun ttyuu (x) x);; uncertain
(defun divide (x y) (floor x y))

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

(format t "end of loading primitives.lisp~%")

