;; dump file tool

(defun dump-char (infile)
  (with-open-file (in infile :element-type '(signed-byte 8))
    (loop for byte = (read-byte in nil)
      while byte
      collect (format nil "~a" (code-char byte)))
  )
)

(defun print-byte (s b)
  (format s "~03o " b)
;  (format t "~x " b)
;  (format t "~3o " b)
;  (format t "~b " (code-char b))
)
  

(defun dump-byte (infile &key (width 20))
  (with-open-file (in infile :element-type '(signed-byte 8))
    (loop with cn = 0 for byte = (read-byte in nil)
      while byte
      do 
        (print-byte t byte)
        (when (and 
               (not (zerop width))
                 (zerop (setq cn (mod (incf cn) width))) (format t "~%"))
        )
    )
  )
)

(defun get-bytes (infile &key (width 0))
  (with-open-file (in infile :element-type '(signed-byte 8))
    (loop with bss = () 
          with bs = ()
          with cn = 0
      for byte = (read-byte in nil)
      while byte
      do
        (push byte bs)
        (cond
          ((zerop width) (push byte bss)) 
          ((zerop (setq cn (mod (incf cn) width)))
            (push (reverse bs) bss) 
            (setq bs nil) )
        )
    finally 
       (cond
         ((zerop width) (return (list (reverse bss))))
         (bs (push (reverse bs) bss) (return (reverse bss)))
         (t (format t "ignore: ~a~%" bs))
       )
    )
  )
)

(defun show-chars (s blist)
  (loop for b in blist
    do 
     (cond
      ((equal b #o040) (format s "  SP"))
      ((equal b #o012) (format s "  NL"))
      (t (format s "   ~a" (code-char b)))
     )
   finally (format s "~%")
  )
)

(defun show-octals (s blist)
  (loop for b in blist
    do (format s "~04o" b)
   finally (format s "~%")
  )
)

(defun show-bytes (s bss)
  (loop for bs in bss do
    (show-chars s bs)
    (show-octals s bs)
    (format t "~%")
  )
)

;; width = 0 means ignore line, namely no limit of line(no width)
;; or, a line is devided by max width     
(defun dump-file (infile &key (width 20))
  (show-bytes t (get-bytes infile :width width))
)

(defun show-charsx (s blist)
  (loop for b in blist
    do 
     (cond
      ((equal b #o040) (format s " SP"))
      ((equal b #o012) (format s " NL"))
      (t (format s "  ~a" (code-char b)))
     )
   finally (format s "~%")
  )
)

(defun show-hexs(s blist)
  (loop for b in blist
    do (format s "~03x" b)
   finally (format s "~%")
  )
)


(defun show-bytesx (s bss)
  (loop for bs in bss do
    (show-charsx s bs)
    (show-hexs s bs)
    (format t "~%")
  )
)

(defun dump-filex (infile &key (width 20))
  (show-bytesx t (get-bytes infile :width width))
)
