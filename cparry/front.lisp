;; front.lisp from front.lap without mlisp code

;;;;; stub
(defun find_words (ssent) ssent)
(defun get_question () )
(defun synonm (x) (symfy x))

;;;; 
(defun test_pattern ()
  ;; too long
  (prog ()
    (get_question)
    (windowset 1)
    (window 1 t 'INPUT)
    (window 2 t SSENT)
    ;(push p nil)
    ;(push p nil)
    ;(push p nil)
    (unless (equal nil LEARNING)
      (setf RIGHT (car SSENT))
      (setf SSENT (cdr SSENT))
      (window 1 t 'RESPELLED)
      (window 3 t (find_words SSENT))
      (when (synonm LEARNING) (return))
      )
    )
  )
    
    
(format t "end of loading front.lisp~%")

