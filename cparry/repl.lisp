
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

(parry)

