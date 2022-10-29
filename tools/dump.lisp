; dump file tool

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

;;; multi type
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

;; dump-file for options

(defconstant fillo "  ")
(defconstant fillx " ")
(defconstant charfmto  "   ~a")
(defconstant charfmtx  "  ~a")
(defconstant datafmto  "~04o")
(defconstant datafmtx  "~03x")

(defclass dump-file-format ()
  (
    (fill 
      :initarg :fillfmt
      :accessor fillfmt
      :initform "")
    (charfmt 
      :initarg :charfmt 
      :accessor charfmt
      :initform "")
    (datafmt 
      :initarg :datafmt
      :accessor datafmt
      :initform "")
  )
)

(defconstant octalfmt (make-instance 'dump-file-format :fillfmt fillo :charfmt charfmto :datafmt datafmto))
(defconstant hexfmt   (make-instance 'dump-file-format :fillfmt fillx :charfmt charfmtx :datafmt datafmtx))

;; dumoer with octal/hex options

(defun show-chars (s mf blist)
  (loop for b in blist
    do 
     (cond
      ((equal b #o040) (format s "~a~a" (fillfmt mf) "SP"))
      ((equal b #o012) (format s "~a~a" (fillfmt mf) "NL"))
      (t (format s (charfmt mf) (code-char b)))
     )
   finally (format s "~%")
  )
)

(defun show-data(s mf blist)
  (loop for b in blist
    do (format s (datafmt mf) b)
   finally (format s "~%")
  )
)


(defun show-bytes (s m bss)
  (loop for bs in bss 
    with mf = (cond ((eq m :octal) octalfmt)(t hexfmt)) 
    do
      (show-chars s mf bs)
      (show-data s mf bs)
      (format t "~%")
  )
)

(defun dump-file (infile &key (pmode :hex) (width 20))
  (show-bytes t pmode (get-bytes infile :width width))
)
