;;; from the top comments ofmlisp code
;; pmem,pmem2,pmem3 have.
;  
;!ANAPHLIST      is the current list of anaphora dotted-pairs.
;!ANAPHLISTOLD   is the previous list of anaphora dotted-pairs.
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


(defvar ?!ANAPHLIST)
(defvar ?!ANAPHLISTOLD)
(defvar ?!ANAPHLISTNEW)
(defvar ?!CLIST)
(defvar ?!CLAST)
(defvar ?!ALLANAPHS)
(defvar ?!LASTIN)
(defvar ?!LASTOUT)
(defvar ?!LAST_ANDTHEN)
(defvar ?!OUTPUT)
(defvar ?!LAST_OUTPUT)
(defvar ?!EXHAUST)
(defvar ?!ERROR)
(defvar WDFLAG)
(defvar REACTTO)
(defvar ERRNAME)
(defvar STYPE)
(defvar STOPIC)
(defvar TRACE_MEM)
(defvar INPUTQUES)
(defvar SSENT)
(defvar DO_SPELL)
(defvar NEXT_CHAR)
(defvar MISSPELL)
(defvar INPUTSSENT)
(defvar DOCNAME)
(defvar DOC_NAME_FLAG)
(defvar EXHAUSTNO)
(defvar SILENCENO)
(defvar SWEARNO)
(defvar CHANSAVE)
(defvar INCHAN)
(defvar SAVE_FILE)
(defvar SAVE_DUMP)
(defvar EOF)
(defvar FILE1)
(defvar FILE2)
(defvar DIACHARNO)
(defvar INPUTFILE)
(defvar PMINPUT)
(defvar PM2INPUT)
(defvar BUG)
(defvar REACTINPUT)
(defvar INPUTNO)
(defvar REPEATNO)
(defvar SPECFNNO)
(defvar MISCNO)
(defvar NEWTOPICNO)
(defvar OLDTOPIC)
(defvar OLDTOPICS)
(defvar HLIST)
(defvar NEWPROVEN)
(defvar INTENT)
(defvar OLDINTENT)
(defvar BADINPUT)
(defvar DELNO)
(defvar PREV_OUTPUT)
(defvar PROVEL)
(defvar PROVEN)
(defvar NEWPROVEN)
(defvar INTLIST)
(defvar INTENT)
(defvar PRINTALL)
(defvar OLDGIBB)
(defvar OLDMISS)
(defvar LAMDA)
(defvar INPUTFILEAREA)

(defvar EXPERIMENT)

(defvar TRACEV )

(defvar HURT nil)
(defvar ANGER nil)
(defvar FEAR nil)
(defvar MISTRUST nil)

(defvar HURT0 nil)
(defvar ANGER0 nil)
(defvar FEAR0 nil)
(defvar MISTRUST0 nil)


(defvar FJUMP nil)
(defvar AJUMP nil)
(defvar HJUMP nil)

(defvar ENDE nil)

(defvar STYLE nil)

(defvar FLARELIST nil)
(defvar SETLIST nil)
(defvar SENSITIVELIST nil)

(defvar WTS nil)
(defvar FL nil)
(defvar POINTERS nil)
(defvar STL Nil)
(defvar DN nil)
(defvar DV nil)
(defvar AN nil)
(defvar AV nil)
(defvar ANV nil)

(defvar NOT_FLAG nil)

(defvar DELFLAG nil)
(defvar FLARE nil)

(defvar WEAK nil)

(defvar WINDOWS nil)
(defvar INITFN nil)
(defvar PERCENT nil)

(defvar VERSION nil)

(defvar NOTSAVED nil)
(defvar PERCENT '\%)

(defvar PTYJOB nil) ;;?? in 2
(defvar SUMEX nil)
(defvar SUPPRESS nil)
(defvar TRACEVFLAG nil)

(defvar ANALFLAG nil)
(defvar OLDSPEAK nil)
(defvar OLDTIME nil)

(defvar PARBEL nil) ;; pmem5.lisp
(format t "end of specials.lisp~%")

