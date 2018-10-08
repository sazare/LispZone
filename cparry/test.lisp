;; test framework
;; based on the code of ch. 9 of Practical Commonlisp by Peter Seibel.
;;  modified it as a novice commonlisper

(defvar *test-name* nil)

(defmacro deftest (name param desc &body body)
  `(defun ,name ,param
     (let ((*test-name* (append *test-name* (list ',name))))
       (format t "test: ~a~%" ,desc)
       ,@body)))

(defmacro test (desc expv form)
    `(let (var)
       (setf var ,form)  ;;; eval form just once
       (report-result ,desc (equal ,expv var) ,expv ',form var)
     )
  )

(defmacro test-case (desc &body forms)
  (let  ((result (gensym)))
  	`(let ((,result t))
	   ,(format t "testcase: ~a ~%" desc)
       	   ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
	   ,result)
	)
  )

(defun report-result (desc result expv form value)
  (if  result 
    (format t ".")
    (progn
      (format t "~% ~a: Test Failed at ~a" desc *test-name*)
      (format t "~%   Expression: ~a" form)
      (format t "~%    Expected ~a: Evaluated: ~a~%" expv value)
    )
  )
  result
)

