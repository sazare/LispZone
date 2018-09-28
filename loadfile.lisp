(defun readexp (fname)
  (with-open-file (inf fname) 
    (do ((res nil (cons next res))
         (next (read inf nil 'eof) (read inf nil 'eof)))
        ((equal next 'eof) res)))
)

(defun loadexp (fname)
  (eval (readexp fname))
)
  
