
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
(defvar symlist '((be am is are were was) (do does did) (see find look)))
(defvar inputq nil)

(defun setupsym (symlist)
  (loop for syms in symlist do
    (loop for word in syms do
      (setf (get word 'synonym) (car syms))
    )
  )
)

(setupsym symlist)

(defun ask ()
  (input-line)
 ; (read-line)
)  

(defun proc (word)
  (format t ":~a~%" word)
)


(defun symfy (word)
  (let ((sym (get word 'synonym)))
    (if sym (cons sym word)(cons word word))
  )
)
 
(defun getline ()
  (unless inputq (loop for wd in (ask) collect (symfy wd)))
)

(defun getword ()
  (unless inputq (setf inputq (getline)))
  (pop inputq)
)

(defun repl ()
  (prog (word)
    (loop 
      (setf word (getword))
      (proc word)
    )
  )
)

;
;(loop until (null (setf ii (read))) do(print ii))
;
(defvar dd "I am a doctor .")
(defvar dq "Am I a doctor ?")

(defun separ (dat)
 (prog (pb pa ate que)
  (setf pb 0)(setf pa 0)(setf ate (length dat))(setf que nil)
  (loop 
   (setf pa (search " " dat  :start2 pb :end2 ate))
   (push (subseq dat pb pa) que)
   (unless pa (return))
   (setf pb (+ pa 1))
  )
  (return (reverse que))
 )
) 

(defun input-line ()  
  (with-input-from-string (ss (read-line))
    (prog (buf se)
      (loop while (setq se (read ss nil)) 
	do (push se buf)) 
      (return (reverse buf))
    )
  )
)





