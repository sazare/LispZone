
;; functions for analyzing the property list of known atoms

(defun dumpprop (LL)
  (dolist (x LL) (format t "~a~%" (cons x (symbol-plist x)))))

(defun allprop (LL)
  (loop for x in LL collect (cons x (symbol-plist x))))


;(quote (*INTLIST* *aoputplist* *aodefplist*))
;(defvar setaoput (union  *aoputplist*  *aoputplist*))
;(defvar setaodef (union *aodefplist* *aodefplist*))

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

;;; nyw
;;; plists = (plist ...)
(defun make-edges (plists)
  (let (edges)
    (loop for plist in plists do
      (setf edges (append edges (make-edge plist))) 
    )
    edges
  )
)


;; intension
;(defvar vvv (allprop *aoputplist*))
;(defvar defs (make-edges vvv))
