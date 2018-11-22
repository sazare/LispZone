;(load "test.lisp")
;(load "loadall.lisp")

(test-set "choose"
  (setf chosen nil)

  (setf ende nil)
  (choose nil)
  (test "resp nil" T (and (null ende) (null chosen)))
  
  (setf ende nil)
  (setf (get 'EXHAUST 'IND) nil)
  (choose 'exhaust)
  (test "resp EXHAUST and ind nil" T (and ENDE (eq CHOSEN 'BYEFEDUP)))

  (setf ende nil)
  (setf (get 'EXHAUST 'IND) nil)
  (setf (get 'something 'IND) nil)
  (choose 'something)
  (test "resp some and ind nil" T (and ENDE (eq CHOSEN 'BYEFEDUP)))

  (setf ende nil)
  (setf (get 'EXHAUST 'IND) nil)
  (setf (get 'something 'IND) 'other)
  (choose 'something)
  (test "resp not exhause and ind not null" T (and (null ENDE) (eq CHOSEN 'something)))
)

