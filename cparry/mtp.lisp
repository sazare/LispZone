;;; UI layer of MTP

;;; here is the starting
(defvar synonym (read-file "data/synonm.alf"))
(defvar inputq nil)
(defun setupsym2 (symlist)
  (loop for symp in symlist do
    (setf (get (car symp) 'synonym) (cadr symp))
  )
)

(setupsym2 synonym)
;; test (eq (get 'whenever 'synonym) 'when)

;;;; 
(defun changemark (str)
  (prog (p)
    (setf str (remove cc str))
    (setq p (position dc str))
    (when p 
      (setf (subseq str p) " ")
      (setf str (format nil "~a~a" str dm))
      (return str)
    )
    (setq p (position qc str))
    (when p 
      (setf (subseq str p) " ")
      (setf str (format nil "~a~a" str qm))
      (return str)
    )
    str
  )
)

(defun input-line (aline)  
"read a line into a list"
  (with-input-from-string (ss aline)
    (prog (buf se )
      (loop while (setq se (read ss nil)) 
	do 
;(format t "~a~%" se)
           (push se buf)
      )
      (return (reverse buf))
    )
  )
)

(defun ask ()
"I would like to prompt and read your input. But not prompt yet."
  (input-line (read-line))
)  

(defun proc (word)
"test stub for proc. this should be replaced parry mental model"
  (format t ":~a|~%" word)
)


(defun symfy (word)
"convert word list to (canonical . original) list as INPUTQUES"
  (let ((sym (get word 'synonym)))
    (if sym (cons sym word)(cons word word))
  )
)
 
(defun getline ()
  (let ()
    (unless inputq (loop for wd in (ask) collect (symfy wd)))
  )
)

(defun getword ()
  (unless inputq (setf inputq (getline)))
  (pop inputq)
)

(defun mtp ()
"the top level function for MTP"
  (prog (word)
    (loop 
      (setf word (getword))
      (when (eq (car word) 'bye) (format t "bye~%")(return))
      (proc word)
    )
  )
)

;;; test statement
(defvar dd "I am a doctor .")
(defvar dq "Am I a doctor ?")



;(changemark xxx dc)
;(changemark yyy qc)
;(remove xxx cc)


(format t "end of loading replpar.lisp~%")

