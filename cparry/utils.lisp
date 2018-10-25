(defun dumpprop (LL)
  (dolist (x LL) (format t "~a~%" (cons x (symbol-plist x)))))

(defun allprop (LL)
  (loop for x in LL collect (cons x (symbol-plist x))))


(quote (*INTLIST* *aoputplist* *aodefplist*))
(defvar setaoput (union  *aoputplist*  *aoputplist*))
(defvar setaodef (union *aodefplist* *aodefplist*))
