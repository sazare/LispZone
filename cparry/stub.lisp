(defun nyi() (format t "NOT YET IMPLEMENT"))

(defun modifyvar ())
(defun react (a) a)
(defun window (x y z) x y z)
(defun winxit ())
;(defun q (a b) a b)
(defun q (a) a)
(defun savejob (a b) a b)
(defun swapp ())
(defun testm ())

;; about error 
(defun ERRSET (a &rest rest) (list a rest))
(defun ERR (x) x)

;; others
(defmacro defprop (sym val prop)
    `(setf (get ',sym ',prop) ',val)
  )

