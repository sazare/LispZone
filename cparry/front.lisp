;; front.lisp from front.lap without mlisp code

;;;;; stub
(defun find_words (ssent) ssent)
(defun get_question () (getword))
(defun synonm (x) (symfy x))

;;;; 
;;;(defun test_pattern ()
;;;  ;; too long, original lap ... is an enigma
;;;  ;; anyway, the return value seems NIL
;;;  ;; but JRST may be a return...
;;;
;;;  (prog (ss)
;;;    (tagbody tag1
;;;        (get_question)
;;;        (windowset 1)
;;;        (window 1 t 'INPUT)
;;;        (window 2 t SSENT)
;;;        ;(push p nil)
;;;        ;(push p nil)
;;;        ;(push p nil)
;;;        (unless (equal nil LEARNING) ;; ??
;;;          (format t "in unless test_pattern")
;;;          (setf RIGHT (car SSENT))
;;;          (setf SSENT (cdr SSENT))
;;;          (window 1 t 'RESPELLED)
;;;          (window 3 t (find_words SSENT))
;;;          (setf ss (synonm LEARNING))
;;;          (unless ss (setf right ss)) ;; dobius
;;;          (window 4 t (canonize (window 1 t 'CANONIZE))) ;; ?
;;;          (segment (window 1 t 'segment)) ;; ?
;;;          (window 5 t pattern) ;;???
;;;          (unless (spats learning) (write_sp) (go tag1))
;;;          (when (eq 0 cpats) (setf right (write_cp))(go tag1))
;;;          (setf cp_match nil)
;;;          (setf cp_match 1) ;; ??? (movem 1 sp_match))
;;;
;;;        (window 1 t 'match)
;;;        (window 7 t (reverse (setf sp_match (translate pattern))))
;;;
;;;        ;;(unless (window 10 t cp_match)(go tag14))
;;;;;; giveup 20190128
;;;        (unless (equal (length ssent) 4) ;; when go tag14
;;;          (if ssent (setf @@2600 (length ssent))
;;;                    (setf @@0010 (length ssent)))
;;;          (window_print 0 0 0);; move 1 learning
;;;        )
;;;      )
;;;    (return ss)
;;;    )
;;;  )
;;;    
    
(format t "end of loading front.lisp~%")

