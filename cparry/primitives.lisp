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

(defun memq (x y) (member x y))
(defun greaterp (x y) (> x y))
(defun lsh (x y) (* x (expt 2 y))) ;; uncertain
(defun ttyuu (x) x);; uncertain
(defun divide (x y) (floor x y))
(format t "end of loading primitives.lisp~%")

