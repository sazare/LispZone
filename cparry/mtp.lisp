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
(defun changem (cc ca str)
  (prog (p)
    (setq p (position cc str))
    (when p 
      (setf (subseq str p) " ")
      (setf str (format nil "~a~a" str ca))
      (return str)
    )
    nil
  )
)
(defun changemark (str)
  (prog (ms)
    (setf str (remove cc str))
    (when (setf ms (changem dc da str)) (return ms))
    (when (setf ms (changem qc qm str)) (return ms))
    (return str)
  )
)

(defun input-line (aline)  
"read a line into a list"
  (with-input-from-string (ss aline)
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
  (input-line (changemark (read-line)))
)  

(defun proc (word)
"test stub for proc. this should be replaced parry mental model"
  (format t ":~a|~%" word)
)


(defun symfy (word)
"convert word list to (canonical . original) list as INPUTQUES"
  (let (sym)
    (cond ((numberp word) (cons word word))
          (T (setf sym (get word 'synonym))
             (if sym (cons sym word)(cons word word)))
             )
  )
)
 
(defun getline ()
  (let (symq asked)
    (unless inputq 
       (setf SSENT nil)
       (setf asked (ask))
(format t "asked = ~a~%" asked)
       (loop for wd in asked do 
         (format t "wd=~a~%" wd)
         (push wd SSENT) 
         (push (symfy wd) symq))
       )
    (setf SSENT (reverse SSENT))
    (setf symq (reverse symq))
(format t "SSENT=~a~%" SSENT)
(format t "symq=~a~%" symq)
  )
)

(defun getword ()
  (unless inputq (setf inputq (getline)))
  (pop inputq)
)

(defun initall ()
  (prog ()
    (binit)
    (initfb)
    (initb)
  )
)

(defun mtp-parry2 (ind)
"from parry2"
  (prog (a b)
    (format t "mtp-parry2.ind = ~a~%" ind) ;; undefined ind
;    (experiment)
(format t "ind1=~a~%" ind)
    (setf a (errset (testm) nil)) 
(format t "ind2=~a~%" a)
    (if (atom a) 
      (format t  "Pattern match error ~a" (list next_char ssent inputques))
      (err nil)
      )
(format t "after atom a")
    (setf a (car a))
    (setf PM2INPUT PMINPUT)
    (setf PMINPUT a)
    (if (= (length SSENT) 1) (setf a (choose 'silence)))
    (analyze t)
(format t "analflag=~a~%" analflag)
(format t "a3=~a~%" a)
    (unless (lambdaname a) (setf a nil))
    (when (and a (atom a) (setf b (get a 'meqv))) (setf a b))
    (setf REACTINPUT a)
    ;(readlambda a) (window 9 nil (get a 'bondvalue))
(format t "ssent4=~a~%" ssent)
    (unless (errset (react (list a (q ssent) ssent)))
      (paerror ssent " error in react")
      (err nil)
      )
    (when ende 
	(setf tracev (not tracev))
        (modifyvar)
	(swapp)
	(exit)
	)
    )
  )

(defun initmtp ()
    (initall)
)

(defun mtp ()
"the top level function for MTP"
  (prog (word)
(setf bug 1)
    (loop 
      (setf word (getword))
(setf bug 2)
      (when (eq (car word) 'bye) (format t "bye~%")(return))
(setf bug 3)
      (mtp-parry2 word)
(setf bug 4)
    )
  )
)


(format t "end of loading replpar.lisp~%")

