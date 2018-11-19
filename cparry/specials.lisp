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


(defvar ?!ANAPHLIST nil)
(defvar ?!ANAPHLISTOLD nil)
(defvar ?!ANAPHLISTNEW nil)
(defvar ?!CLIST nil)
(defvar ?!CLAST nil)
(defvar ?!ALLANAPHS nil)
(defvar ?!LASTIN nil)
(defvar ?!LASTOUT nil)
(defvar ?!LAST_ANDTHEN nil)
(defvar ?!OUTPUT nil)
(defvar ?!LAST_OUTPUT nil)
(defvar ?!EXHAUST nil)
(defvar ?!ERROR nil)

(defvar CHOSEN nil)
(defvar WDFLAG nil)
(defvar REACTTO nil)
(defvar ERRNAME nil)
(defvar STYPE nil)
(defvar STOPIC nil)
(defvar TRACE_MEM nil)
(defvar INPUTQUES nil)
(defvar SSENT nil)
(defvar DO_SPELL nil)
(defvar NEXT_CHAR nil)
(defvar MISSPELL nil)
(defvar INPUTSSENT nil)
(defvar DOCNAME nil)
(defvar DOC_NAME_FLAG nil)
(defvar EXHAUSTNO nil)
(defvar SILENCENO nil)
(defvar SWEARNO nil)
(defvar CHANSAVE nil)
(defvar INCHAN nil)
(defvar SAVE_FILE nil)
(defvar SAVE_DUMP nil)
(defvar EOF nil)
(defvar FILE1 nil)
(defvar FILE2 nil)
(defvar DIACHARNO nil)
(defvar INPUTFILE nil)
(defvar PMINPUT nil)
(defvar PM2INPUT nil)
(defvar BUG nil)
(defvar REACTINPUT nil)
(defvar INPUTNO nil)
(defvar REPEATNO nil)
(defvar SPECFNNO nil)
(defvar MISCNO nil)
(defvar NEWTOPICNO nil)
(defvar OLDTOPIC nil)
(defvar OLDTOPICS nil)
(defvar HLIST nil)
(defvar NEWPROVEN nil)
(defvar INTENT nil)
(defvar OLDINTENT nil)
(defvar BADINPUT nil)
(defvar DELNO nil)
(defvar PREV_OUTPUT nil)
(defvar PROVEL nil)
(defvar PROVEN nil)
(defvar NEWPROVEN nil)
(defvar INTLIST nil)
(defvar INTENT nil)
(defvar PRINTALL nil)
(defvar OLDGIBB nil)
(defvar OLDMISS nil)
(defvar LAMDA nil)
(defvar INPUTFILEAREA nil)

(defvar EXPERIMENT nil)

(defvar TRACEV  nil)

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

(defvar WEIGHT 0)
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

(defvar DIAFILEAREA nil)
(defvar ONEDIA nil)
(defvar FILE12 nil)


(format t "end of specials.lisp~%")

