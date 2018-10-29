
;; functions for analyzing the property list of known atoms

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
    (setq paop (union  :q *aoputplist*))
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
