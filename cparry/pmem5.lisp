(defun experiment ()
  ;; EXPERIMENTS OF RAISING AND LOWERING SHAME
  (unless (or EXPERIMENT (eq EXPERIMENT 'SEVEN))
    (if (= 7 INPUTNO) (setf HURT (+ HURT 5)))
    (if (= 17 INPUTNO) (setf HURT (- HURT 5)))
  )
)

(format t "end of loading pmem5.lisp~%")

