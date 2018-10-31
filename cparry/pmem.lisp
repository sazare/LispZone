;;; from PMEM

;;; MASTER LIST OF GLOBAL VARIABLES
;;;; ***GLOBAL VARIABLES***

;!ANAPHLIST      is the current list of anaphora dotted-pairs.
;!ANAPHLISTOLD   is the previous list of anaphora dotted-pairs.
;!ANAPHLISTNEW   is the next list of anaphora dotted-pairs to go into anaphlist
;!ALLANAPHS      is a list of lists - ((who they he)(he who)(they who)....)
;!CLIST          is a pointer to the first element of the conversation list.
;!CLAST          is a pointer to the last element of the conversation list.
;!LAST_ANDTHEN   is either IN or OUT as the last ANDTHEN processed
;!LASTIN         points to atom under which is stored the last semantic unit
;                inputed by the doctor.
;!LASTOUT        points to the atom under which is stored the last semantic
;                unit outputed by Parry.
;
;!ERROR          contains a list of errors made
;!EXHAUST        is true if a set of responses is exhausted and the exhaust responses are to be used
;                  it is set by SELECT_SENTENCE  and used at the end of REACT2
;!OUTPUT         is the output of parry, to be sent whereever
;!LAST_OUTPUT    is the ^H-name of the last output
;
;INPUTQUES       is a list of dotted pairs from the pattern matcher to the memory
;                        each pair is (canonical 5-letter atom . input word)




;;;

(defun lambdaname (s) (equal (subseq (string s) 0 2) "@@"))
;; now replaced ^B to car, alphaname is incorrect
;; but noone call alphaname
(defun alphaneme (s) (equal (subseq (string s) 0 2) ""))

(defun diskread (a)
  (progn
    (nyi)
    (format t "nop diskread ~a" a)
    a
  )
)

(defun readlambda (a)
  (if (not (lambdaname a))
    nil
    (if (diskread a) 
      t
      (error "BAD DISKREAD" )
      )
    )
  )

;carn is defined in diaapp.lisp
;(defun carn (s)
;  (if (atom s) 
;    s
;    (car s)
;    )
;  )
;

(format t "end of loading pmem.lisp")

