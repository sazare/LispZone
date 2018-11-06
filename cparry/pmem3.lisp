(defun readinc()
  ; original code is uncertain
  (read)
;  (prog (i)
;    (inc inchan nil)
;    (setf i (read))
;    (inc nil nil)
;    (return i)
;    )
  )

(defun readdata ()
  ;; original code is uncertain
  (readinc)
;  (prog (a)
;    (setf a (readinc))
;    (when (eq a 'COMMENT)
;      (until (and (char= a 'C') (char= a '?')) ;; uncertain
;	     (setf a (readinc))
;      (setf a (readinc))
;      )
;    )
;    (return a)
  )

(format t "end of loading pmem3.lisp~%")

