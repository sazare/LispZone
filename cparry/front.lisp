;; front.lisp from front.lap without mlisp code

(defun test_pattern ()
  ;; too long
  (get_question)
  (windowset 1 *SSENT*)
  (window 1 t 'INPUT)
  (window 2 t *SSENT*)
  ;(push p nil)
  ;(push p nil)
  ;(push p nil)
  (unless (equal nil *LEARNING*)
    (car *SSENT*)
    *RIGHT*
    (cdr *SSENT*)
    *SSENT*
    (window 1 t 'RESPELLED)
    (window 3 t (find_words *SSENT*))
    (when (synonm *LEARNING*) (return))
    
    
(format t "end of loading front.lisp")

