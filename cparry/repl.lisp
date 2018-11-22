
(defun answer (ind)
  (prog (a)
	(setf a ind)
	(return (format nil "answer: ~a" a))
	)
  )
	
(defun parry2 (ind)
  (format t "parry> ~a~%" (answer ind))
  )

(defun parry ()
  (prog ()
    (loop 
      (let (ind )
	(format t "you> ~0%")
        (force-output t)
	(setf ind (read-line))
	(when (equal ind "end") (return))
	(terpri t)
	(parry2 ind)
	(terpri)
    )
  )
  (format t "Good bye")
  )
)

;(parry)


;;; new repl
(defvar symlist '((be is are were was) (do does did) (see find look)))
(defvar inputq nil)

(defun ask ()
  (read-line)
)  

(defun proc (word)
  (format t ":~a~%" word)
)

(defun setupsym (symlist)
  (loop for syms in symlist do
    (loop for word in syms do
      (setf (get word 'synonym) (car syms))
    )
  )
)

(defun symfy (word)
  (let ((sym (get word 'synonym)))
    (if sym (cons sym word)(cons word word))
  )
)
 
(defun getline ()
  (unless inq (loop for wd in (ask) collect (symfy wd)))
)

(defun getword ()
  (unless inq (setf inq (getline)))
  (pop inq)
)

(defun repl ()
  (prog (word)
    (loop 
      (setf word (getword))
      (proc word)
    )
  )
)

