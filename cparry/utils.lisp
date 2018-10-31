(load "primitives.lisp")

;; functions for analyzing the property list of known atoms
;;; BEL, INF
(defun dumpprop (LL)
  (dolist (x LL) (format t "~a~%" (cons x (symbol-plist x)))))

(defun allprop (LL)
  (loop for x in LL collect (cons x (symbol-plist x))))

(defun make-arc (a1 p1 targets)
  (cond 
    ((atom targets) (list(list a1 p1 targets)))
    ((atom (car targets)) (loop for tg1 in targets collect (list a1 p1 tg1)))
    )
  )

(defun make-arcs (a1 vps)
  (let ((edges nil) arc)
     (loop for ad on vps by #'cddr do 
	   (setf arc (make-arc a1 (car ad) (cadr ad)))
	   (when arc (setf edges (append edges arc )))
     )
     edges
  )
)

;;; plist = (symbol p1 v1 p2 v2 ...)
(defun make-edge (plist)
  (make-arcs (car plist) (cdr plist))
)

;;; plists = (plist ...)
(defun make-edges (plists)
  (let (edges)
    (loop for plist in plists do
      (setf edges (append edges (make-edge plist))) 
    )
    edges
  )
)

(defun write-edges (edges fname)
  (with-open-file (ss fname :direction :output :if-exists :supersede)
    (dolist (e edges)
      (format ss "~a ~a ~a~%" (nth 0 e) (nth 2 e)(nth 1 e))
      )
    )
  )


;(quote (*INTLIST* *aoputplist* *aodefplist*))
(defun save-edges ()
;  (let (daop dedges)
  (let (paop pedges daop dedges)
    ;; comment out because rdata.lisp is not ok
    (setq paop (union *aoputplist* *aoputplist*))
    (setq pedges (make-edges (allprop paop)))
    (write-edges pedges "pedges.csv")
    (setq daop (union *aodefplist* *aodefplist*))
    (setq dedges (make-edges (allprop daop)))
    (write-edges dedges "dedges.csv")
    )
  )

;; intension
;(defvar vvv (allprop *aoputplist*))
;(defvar defs (make-edges vvv))


;;; RDATA

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

