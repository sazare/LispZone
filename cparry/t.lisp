;; t.lisp has temporal codes

(defun print-pair (n v)
  (format t " ~10a ~a~%" n v)
  )

(defun print-plist (atm)
  (let ((plist (symbol-plist atm)))
    (format t "[~a]~%" atm)
     (loop for nv on plist by #'cddr do
           (print-pair (car nv) (cadr nv)))
     )
  )

(defun print-pplist (list)
  (loop for atm in list do
	(print-plist atm)
	)
  )

(defun print-stl (stl)
  (print-pplist 
    (loop for al in stl collect (car al))
    )
  )

(defun showwords (wds)
  (mapcar (lambda (wd) (print-plist wd))
	  wds)
  )

(defun showset (set)
  (mapcar (lambda (con) (print-pplist (get con 'words)))
	  set)
  )

(defun showset0 (set)
  (mapcar (lambda (con) (showwords (get con 'words)))
	  set)
  )

(defun showlist (list)
  (showset (get list 'sets))
  )

(defun showlist (lst keys prev)
  "show all properties of the elems in lst"
  (loop for elem in lst do 
	(showtree elem keys prev)
	)
  )

;; require lambdaname in pmem.lisp

(defun showtree (atm keys prev)
  "show all properties of list. in the property keys are recursively"
    (unless (member atm prev) 
      (let ((tprev prev) vlist)
      (unless (lambdaname atm) (print-plist atm) )
      (setq tprev (cons atm tprev))
      (loop for kd in keys do
	    (setq vlist (get atm kd))
	    (when vlist 
	      (if (atom vlist) 
		(showtree vlist keys (cons vlist tprev)) 
		(showlist vlist keys tprev))
	      )
	    )
      )
    )
    )

;; (showtree 'racketset '(words sets set) ())
;; (showlist (get 'flarelist 'sets) '(words sets set) ())

;; (defvar myfl '(FLARELIST SENSITIVELIST SETLIST))

;; show all list
; (showtree 'flarelist '(words sets set) ())
; (showtree 'sensitivelist '(words sets set) ())
; (showtree 'setlist '(words sets set) ())

;; show mafia delusion transition
; (showlist (get 'flarelist 'sets) '(next) Nil)

;; show story
; (showlist (mapcar (lambda (x) (car x)) stl) '(story) () )

