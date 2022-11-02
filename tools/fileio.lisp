;; lisp code for file io

;; full file read/write
(defun readafile0 (fname)
 (with-open-file (in fname)
  (read in)
 )
)

(defun readafile (fname)
 (with-open-file (ins fname)
   (let (data)
     (loop until (eq :eof (setf data (read ins nil :eof)))
       collect 
          data
       )
     )
 )
)

;;; writeafile
(defun writeafile (fname objects)
  (with-open-file (out fname 
      :direction :output
      :if-exists :supersede)

    (print objects out)
  )
)


;; binary io
(defun readonbyte (infile)
  (with-open-file (in infile :element-type '(signed-byte 8))
    (loop for byte = (read-byte in nil)
      while byte
      collect byte)
  )
)

(defun writeonbyte (outfile data)
  (with-open-file (out outfile 
		    :direction :output 
                    :element-type '(signed-byte 8))
    (loop for b in data 
      do (write-byte b out)
    )
  )
)

