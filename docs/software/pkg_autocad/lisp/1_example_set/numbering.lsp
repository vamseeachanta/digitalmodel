;;; inserts increasing numbers with or without prefix or suffix text
;;; author paul >> paulmcz@yahoo.com
;;; tit  = text+integer+text
;;; trt  = text+real+text
;;; nmc  = integer inside circle
;;; nmp  = integer inside hexagon
;;; nm   = ascending integers
;;; aazz = ascending alphabet letters
;;; aad  = text+ascending alphabet letters

;;; TX+INTEGERS+TX
(defun c:tit (/ p n ni pref suff nns ntx ntxx oecho osn ds th txt)

  (setq	oecho (getvar "cmdecho")
	osn   (getvar "osmode")
  )
  (if (= 0 (getvar "dimscale"))(setq ds 1.0)(setq ds (getvar "dimscale")))
  (setq th (getvar "dimtxt"))
  (setq txt (* th ds))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (if nn
    (setq nn (fix nn))
    (setq nn 1)
  )
  (if (= nn 0)(setq nn 1))
  (princ "\n Increment numbers by < ")
  (princ nn)
  (princ " >? : ")
  (setq ni (getint))
  (if (= ni nil)
    (setq ni nn)
    (setq nn ni)
  )

  (if np
    (setq np (fix np))
    (setq np nn)
  )
  (princ "\n Start or continue with number < ")
  (princ np)
  (princ " >? : ")
  (setq n (getint))
  (if (= n nil)
    (setq n np)
    (setq np n)
  )
  (setq nns (itoa n))

  (princ "\n Prefix text < ")
  (princ pre)
  (princ " >? or <.> for none: ")
  (setq pref (getstring t))
  (if (= pref ".")
    (progn
      (setq pre nil)
      (setq pref nil)
    )
    (progn
      (if (= pref "")
	(setq pref pre)
	(setq pre pref)
      )
      (if pref
	  (setq ntx (strcat pref nns))
      )
    )
  )

  (princ "\n Suffix text < ")
  (princ suf)
  (princ " >? or <.> for none: ")
  (setq suff (getstring t))
  (if (= suff ".")
    (progn
      (setq suf nil)
      (setq suff nil)
    )
    (progn
      (if (= suff "")
	(setq suff suf)
	(setq suf suff)
      )
      (if suff
	(if pref
	  (setq ntxx (strcat pref nns suff))
	  (setq ntxx (strcat nns suff))
	)
      )
    )
  )
  (setq p (getpoint "\n Insert: "))
  (setq oecho (getvar "cmdecho"))

  (while p
    (if	suff
      ;(command "text" "j" "mc" p "" "" ntxx)
      (entmake (list (cons 0 "TEXT")
		     (cons 10 p)
		     (cons 11 p)
		     (cons 1 ntxx)	; actual text
		     (cons 7 (getvar "TEXTSTYLE"))
		     (cons 40 txt)
		     (cons 72 4)
	       )
      )
      (if pref
	;(command "text" "j" "mc" p "" "" ntx)
	(entmake (list (cons 0 "TEXT")	
		    (cons 10 p)	
		    (cons 11 p)	
		    (cons 1 ntx); actual text
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 40 txt)
		    (cons 72 4)
	      )
     )
	;(command "text" "j" "mc" p "" "" n)
	(entmake (list (cons 0 "TEXT")	
		    (cons 10 p)	
		    (cons 11 p)	
		    (cons 1 n); actual text
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 40 txt)
		    (cons 72 4)
	      )
     )
      )
    )
    (setq p   (getpoint "\n Next number location: ")
	  n   (+ ni n)
	  nns (itoa n)
	  np n
    )
    
    (if	suff
      (if pref
	(setq ntxx (strcat pref nns suff))
	(setq ntxx (strcat nns suff))
      )
    )
    (if	pref
      (if suff
	(setq ntxx (strcat pref nns suff))
	(setq ntx (strcat pref nns))
      )
    )
  )
  (setvar "cmdecho" oecho)
  (setvar "osmode" osn)
  (princ)
)

(princ "\n Type > TIT <  to insert text with ascending integers.")


;;;*********************************************************
;;;*********************************************************

;;;TX+REALS+TX
(defun c:trt (/ p n ni pref suff nns ntx ntxx oecho osn ds th txt)

  (setq	oecho (getvar "cmdecho")
	osn   (getvar "osmode")
  )
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (if (= 0 (getvar "dimscale"))(setq ds 1.0)(setq ds (getvar "dimscale")))
  (setq th (getvar "dimtxt"))
  (setq txt (* th ds))
  (if nn
    ()
    (setq nn 1)
  )
  (princ "\n Increment numbers by < ")
  (princ nn)
  (princ " >? : ")
  (setq ni (getreal))
  (if (= ni nil)
    (setq ni nn)
    (setq nn ni)
  )

  (if np
    ()
    (setq np nn)
  )
  (princ "\n Start or continue with number < ")
  (princ np)
  (princ " >? : ")
  (setq n (getreal))
  (if (= n nil)
    (setq n np)
    (setq np n)
  )
  (setq nns (rtos n 2 3))

  (princ "\n Prefix text < ")
  (princ pre)
  (princ " >? or <.> for none: ")
  (setq pref (getstring t))
  (if (= pref ".")
    (progn
      (setq pre nil)
      (setq pref nil)
    )
    (progn
      (if (= pref "")
	(setq pref pre)
	(setq pre pref)
      )
      (if pref
	  (setq ntx (strcat pref nns))
      )
    )
  )

  (princ "\n Suffix text < ")
  (princ suf)
  (princ " >? or <.> for none: ")
  (setq suff (getstring t))
  (if (= suff ".")
    (progn
      (setq suf nil)
      (setq suff nil)
    )
    (progn
      (if (= suff "")
	(setq suff suf)
	(setq suf suff)
      )
      (if suff
	(if pref
	  (setq ntxx (strcat pref nns suff))
	  (setq ntxx (strcat nns suff))
	)
      )
    )
  )
  (setq p (getpoint "\n Insert: "))
  (setq oecho (getvar "cmdecho"))

  (while p
    (if	suff
      (entmake (list (cons 0 "TEXT")	
		    (cons 10 p)	
		    (cons 11 p)	
		    (cons 1 ntxx); actual text
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 40 txt)
		    (cons 72 4)
	      )
     )
      (if pref
	(entmake (list (cons 0 "TEXT")	
		    (cons 10 p)	
		    (cons 11 p)	
		    (cons 1 ntx); actual text
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 40 txt)
		    (cons 72 4)
	      )
     )
	(entmake (list (cons 0 "TEXT")	
		    (cons 10 p)	
		    (cons 11 p)	
		    (cons 1 nns); actual text
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 40 txt)
		    (cons 72 4)
	      )
     )
      )
    )
    (setq p   (getpoint "\n Next number location: ")
	  n   (+ ni n)
	  nns (rtos n 2 3)
	  np n
    )
    
    (if	suff
      (if pref
	(setq ntxx (strcat pref nns suff))
	(setq ntxx (strcat nns suff))
      )
    )
    (if	pref
      (if suff
	(setq ntxx (strcat pref nns suff))
	(setq ntx (strcat pref nns))
      )
    )
  )
  (setvar "cmdecho" oecho)
  (setvar "osmode" osn)
  (princ)
)

(princ "\n Type > TRT <  to insert text with ascending real numbers.")


;;;***********************************************************
;;;***********************************************************


;;; inserts increasing numbers circled up
(defun c:nmc (/ p n ni ts oecho ds th txt na)
  
  (if (= 0 (getvar "dimscale"))(setq ds 1.0)(setq ds (getvar "dimscale")))
  (setq th (getvar "dimtxt"))
  (setq txt (* th ds))
  (setq ts txt)
  (if nn (setq nn (fix nn))(setq nn 1))
  (if (= nn 0)(setq nn 1))
  (princ "\n Increment by < ")
  (princ nn)
  (princ " >? : ")
  (setq ni (getint))
  (if (= ni nil)(setq ni nn)(setq nn ni))
  
  (if np
    (setq np (fix np))
    (setq np 1)
  )
  (princ "\n Start or continue with number < ")
  (princ np)
  (princ " >? : ")
  (setq n (getint))
  (if (= n nil)
    (setq n np)
    (setq np n)
  )
  (setq p (getpoint "\n Number location: "))
  (setq oecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  
  (while p
    (setq na (itoa n))
    (entmake (list (cons 0 "TEXT")	
		    (cons 10 p)	
		    (cons 11 p)	
		    (cons 1 na); actual text
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 40 txt)
		    (cons 72 4)
	      )
     )
    (if (> n 99)
      (command "circle" p (* ts 1.9))
      (command "circle" p (* ts 1.6)))
    (setq p (getpoint "\n Next number location: ")
	  n (+ ni n)
	  np n
    )
  )
	 
  (setvar "cmdecho" oecho)
  (princ)
)

(princ "\n Type > NMC <  to insert numbers inside circle")


;;;***********************************************************
;;;***********************************************************


; inserts increasing numbers inside hexagon
(defun c:nmp (/ p n ni ts oecho ds th txt na)
  
  
  (if (= 0 (getvar "dimscale"))(setq ds 1.0)(setq ds (getvar "dimscale")))
  (setq th (getvar "dimtxt"))
  (setq txt (* th ds))
  (setq ts txt)
  (if nn (setq nn (fix nn))(setq nn 1))
  (if (= nn 0)(setq nn 1))
  (princ "\n Increment by < ")
  (princ nn)
  (princ " >? : ")
  (setq ni (getint))
  (if (= ni nil)(setq ni nn)(setq nn ni))
  
  (if np
    ()
    (setq np 1)
  )
  (princ "\n Start or continue with number < ")
  (princ np)
  (princ " >? : ")
  (setq n (getint))
  (if (= n nil)
    (setq n np)
    (setq np n)
  )
  (setq p (getpoint "\n Number location: "))
  (setq oecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  
  (while p
    (setq na (itoa n))
    (entmake (list (cons 0 "TEXT")	
		    (cons 10 p)	
		    (cons 11 p)	
		    (cons 1 na); actual text
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 40 txt)
		    (cons 72 4)
	      )
     )
    (if (> n 99)
      (command "polygon" "6" p "c" (* ts 1.9))
      (command "polygon" "6" p "c" (* ts 1.6)))
    (setq p (getpoint "\n Next number location: ")
	  n (+ ni n)
	  np n
    )
  )
	 
  (setvar "cmdecho" oecho)
  (princ)
)

(princ "\n Type > NMP <  to insert numbers inside hexagon")


;;;***********************************************************
;;;***********************************************************

; inserts increasing numbers
(defun c:nm (/ p n ni ts oecho ds th txt na)
  
  (setq ts (getvar "textsize"))
  (if (= 0 (getvar "dimscale"))(setq ds 1.0)(setq ds (getvar "dimscale")))
  (setq th (getvar "dimtxt"))
  (setq txt (* th ds))
  (if nn (setq nn (fix nn))(setq nn 1))
  (if (= nn 0)(setq nn 1))
  (princ "\n Increment by < ")
  (princ nn)
  (princ " >? : ")
  (setq ni (getint))
  (if (= ni nil)(setq ni nn)(setq nn ni))
  
  (if np
    ()
    (setq np 1)
  )
  (princ "\n Start or continue with number < ")
  (princ np)
  (princ " >? : ")
  (setq n (getint))
  (if (= n nil)
    (setq n np)
    (setq np n)
  )
  (setq p (getpoint "\n Number location: "))
  (setq oecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  
  (while p
    (setq na (itoa n))
    (entmake (list (cons 0 "TEXT")	
		    (cons 10 p)	
		    (cons 11 p)	
		    (cons 1 na); actual text
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 40 txt)
		    (cons 72 4)
	      )
     )
    (setq p (getpoint "\n Next number location: ")
	  n (+ ni n)
	  np n
    )
  )
	 
  (setvar "cmdecho" oecho)
  (princ)
)

(princ "\n Type > NM <   to insert numbers.")



;;;***********************************************************
;;;***********************************************************


;;; inserts ascending alphabet letters (Capitals only)
(defun c:aazz (/ alf n p nu pnu osn ts oecho ds th txt)
  (setq	oecho (getvar "cmdecho")
	osn   (getvar "osmode")
	ts    (getvar "textsize")
  )

  (if (= 0 (getvar "dimscale"))(setq ds 1.0)(setq ds (getvar "dimscale")))
  (setq th (getvar "dimtxt"))
  (setq txt (* th ds)
	ts txt)

  (setq oerr *error*)
  (defun *error* (msg)
    (setvar "osmode" osn)
    (setvar "textsize" ts)
    (princ "\n Wrong input! >>  ")
    (setvar "cmdecho" oecho)
    (setq *error* oerr)
    (command)
    (princ)
  )

  (setq	alf '("A"   "B"	  "C"	"D"   "E"   "F"	  "G"	"H"   "I"
	      "J"   "K"	  "L"	"M"   "N"   "O"	  "P"	"Q"   "R"
	      "S"   "T"	  "U"	"V"   "W"   "X"	  "Y"	"Z"
	     )
  )
  (if nd
    ()
    (setq nd "A")
  )
  (princ "\n Input - case insensitive, output = upper case only\n Start with letter < ")
  (princ nd)
  (princ " >? : ")
  (setq n (getstring))
  (if (= n "")
    (setq n nd)
  )
  (setq n (strcase n))

  (cond
    ((or (= n "a") (= n "A")) (setq nu 0))
    ((or (= n "b") (= n "B")) (setq nu 1))
    ((or (= n "c") (= n "C")) (setq nu 2))
    ((or (= n "d") (= n "D")) (setq nu 3))
    ((or (= n "e") (= n "E")) (setq nu 4))
    ((or (= n "f") (= n "F")) (setq nu 5))
    ((or (= n "g") (= n "G")) (setq nu 6))
    ((or (= n "h") (= n "H")) (setq nu 7))
    ((or (= n "i") (= n "I")) (setq nu 8))
    ((or (= n "j") (= n "J")) (setq nu 9))
    ((or (= n "k") (= n "K")) (setq nu 10))
    ((or (= n "l") (= n "L")) (setq nu 11))
    ((or (= n "m") (= n "M")) (setq nu 12))
    ((or (= n "n") (= n "N")) (setq nu 13))
    ((or (= n "o") (= n "O")) (setq nu 14))
    ((or (= n "p") (= n "P")) (setq nu 15))
    ((or (= n "q") (= n "Q")) (setq nu 16))
    ((or (= n "r") (= n "R")) (setq nu 17))
    ((or (= n "s") (= n "S")) (setq nu 18))
    ((or (= n "t") (= n "T")) (setq nu 19))
    ((or (= n "u") (= n "U")) (setq nu 20))
    ((or (= n "v") (= n "V")) (setq nu 21))
    ((or (= n "w") (= n "W")) (setq nu 22))
    ((or (= n "x") (= n "X")) (setq nu 23))
    ((or (= n "y") (= n "Y")) (setq nu 24))
    ((or (= n "z") (= n "Z")) (setq nu 25))
    (t
     (progn
       (alert
	 "ONLY SINGLE ALPHABETIC CHARACTER IS ALLOWED!\n    >>>> ABORTING <<<<< "
       )
       (^c^c)
     )
    )
  )

  (princ "\n Letter < ")
  (princ n)
  (princ " > location: ")
  (setq p (getpoint))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)

(while (and p (< nu 26))
  (setq n (nth nu alf))
  (setq pnu (nth (1+ nu) alf))
  (entmake (list (cons 0 "TEXT")	
		    (cons 10 p)	
		    (cons 11 p)	
		    (cons 1 n); actual text
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 40 txt)
		    (cons 72 4)
	      )
     )
  (command "circle" p (* ts 1.4))
  ;(command "polygon" "6" p "c" (* ts 1.3))
  (if pnu
    (progn
      (princ "\n Letter < ")
      (princ pnu)
      (princ " > location? : < or press Enter to quit >")
    )
    (princ "\n End of Alphabet - start over - next default = A")
  )
  (setq	p  (getpoint)
	nu (1+ nu)
  )
  (if p
    ()
    (setq nd (nth nu alf))
  )
  (if (> nu 25)
    (setq nd "A")
  )
)


  (setvar "osmode" osn)
  (setvar "cmdecho" oecho)
  (princ)
)

(princ "\n Type > aazz < to insert ascending letters.")


;;;****************************************************************
;;;****************************************************************


(defun c:aad (/ alf n p nu pnu osn ts oecho pref ds th txt)
  (setq	oecho (getvar "cmdecho")
	osn   (getvar "osmode")
	ts    (getvar "textsize")
  )

  (if (= 0 (getvar "dimscale"))(setq ds 1.0)(setq ds (getvar "dimscale")))
  (setq th (getvar "dimtxt"))
  (setq txt (* th ds))

  (setq oerr *error*)
  (defun *error* (msg)
    (setvar "osmode" osn)
    (setvar "textsize" ts)
    (princ "\n Wrong input! >>  ")
    (setvar "cmdecho" oecho)
    (setq *error* oerr)
    (command)
    (princ)
  )

  (setq	alf '("A"   "B"	  "C"	"D"   "E"   "F"	  "G"	"H"   "I"
	      "J"   "K"	  "L"	"M"   "N"   "O"	  "P"	"Q"   "R"
	      "S"   "T"	  "U"	"V"   "W"   "X"	  "Y"	"Z"
	     )
  )
  (if nd
    ()
    (setq nd "A")
  )
  (princ "\n Input - case insensitive, output = upper case only\n Start with letter < ")
  (princ nd)
  (princ " >? : ")
  (setq n (getstring))
  (if (= n "")
    (setq n nd)
  )
  (setq n (strcase n))

  (cond
    ((or (= n "a") (= n "A")) (setq nu 0))
    ((or (= n "b") (= n "B")) (setq nu 1))
    ((or (= n "c") (= n "C")) (setq nu 2))
    ((or (= n "d") (= n "D")) (setq nu 3))
    ((or (= n "e") (= n "E")) (setq nu 4))
    ((or (= n "f") (= n "F")) (setq nu 5))
    ((or (= n "g") (= n "G")) (setq nu 6))
    ((or (= n "h") (= n "H")) (setq nu 7))
    ((or (= n "i") (= n "I")) (setq nu 8))
    ((or (= n "j") (= n "J")) (setq nu 9))
    ((or (= n "k") (= n "K")) (setq nu 10))
    ((or (= n "l") (= n "L")) (setq nu 11))
    ((or (= n "m") (= n "M")) (setq nu 12))
    ((or (= n "n") (= n "N")) (setq nu 13))
    ((or (= n "o") (= n "O")) (setq nu 14))
    ((or (= n "p") (= n "P")) (setq nu 15))
    ((or (= n "q") (= n "Q")) (setq nu 16))
    ((or (= n "r") (= n "R")) (setq nu 17))
    ((or (= n "s") (= n "S")) (setq nu 18))
    ((or (= n "t") (= n "T")) (setq nu 19))
    ((or (= n "u") (= n "U")) (setq nu 20))
    ((or (= n "v") (= n "V")) (setq nu 21))
    ((or (= n "w") (= n "W")) (setq nu 22))
    ((or (= n "x") (= n "X")) (setq nu 23))
    ((or (= n "y") (= n "Y")) (setq nu 24))
    ((or (= n "z") (= n "Z")) (setq nu 25))
    (t
     (progn
       (alert
	 "ONLY SINGLE ALPHABETIC CHARACTER IS ALLOWED!\n    >>>> ABORTING <<<<< "
       )
       (^c^c)
     )
    )
  )

  (princ "\n Prefix text < ")
  (princ pre)
  (princ " >? or <.> for none: ")
  (setq pref (getstring t))
  (if (= pref ".")
    (progn
      (setq pre nil)
      (setq pref nil)
    )
    (progn
      (if (= pref "")
	(setq pref pre)
	(setq pre pref)
      )
      (if pref
	  (setq n (strcat pref n))
      )
    )
  )

  (princ "\n Text < ")
  (princ n)
  (princ " > location: ")
  (setq p (getpoint))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)

  

(while (and p (< nu 26))
  (if pref
    (setq n (strcat pref (nth nu alf)))
    (setq n (nth nu alf))
  )
  (entmake (list (cons 0 "TEXT")	
		    (cons 10 p)	
		    (cons 11 p)	
		    (cons 1 n); actual text
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 40 txt)
		    (cons 72 4)
	      )
     )
  
  (if (< nu 26)
    (if	pref
      (if (= nu 25)
	(setq pnu nil)
	(setq pnu (strcat pref (nth (1+ nu) alf)))
      )
      (setq pnu (nth (1+ nu) alf))
    )
  )
  (if pnu
    (progn
      (princ "\n Text < ")
      (princ pnu)
      (princ " > location? : (to quit, press Enter)")
    )
    (princ "\n End of Alphabet - start over - next default = A")
  )
  (setq	p  (getpoint)
	nu (1+ nu)
  )
  (if p
    ()
    (setq nd (nth nu alf))
  )
  (if (> nu 25)
    (setq nd "A")
  )
)




  (setvar "osmode" osn)
  (setvar "cmdecho" oecho)
  (princ)
)

(princ "\n Type > aad <  to insert ascending letters with or without prefix text.")