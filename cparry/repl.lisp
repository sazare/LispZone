(defun parry2 ()
  )

(defun parry ()
  (progn
    (format t "READY: ~0%")
    (force-output t)
    (loop 
      (let ((ind nil))
	(format t "> ~0%")
	(setf ind (read-line))
	(parry2 ind)
	(if (equal ind "end") (return))
	(format t "~a~%" ind);(terpri)
    )
  )
  (format t "Good bye")
)
(parry)

