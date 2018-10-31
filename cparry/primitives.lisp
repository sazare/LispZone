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


(format t "end of loading primitives.lisp~%")

