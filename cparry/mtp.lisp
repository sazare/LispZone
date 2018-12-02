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

(defun input-line ()  
"read a line into a list"
  (with-input-from-string (ss (read-line))
    (prog (buf se )
      (loop while (setq se (read ss nil)) 
	do 
           (push se buf)
      )
      (return (reverse buf))
    )
  )
)

(defun ask ()
"I would like to prompt and read your input. But not prompt yet."
  (input-line)
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


(format t "end of loading replpar.lisp~%")

