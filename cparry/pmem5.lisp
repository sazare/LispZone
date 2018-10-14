(defun experiment ()
  (if (or *experiment* 
	  (eq *experiment* 'SEVEN))
    (return nil))
  (if (= 7 *inputno*) (setf *hurt (+ *hurt* 5)))
  (if (= 17 *inputno*) (setf *hurt (- *hurt* 5)))
  )
