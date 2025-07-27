(DEFUN C:MHT ()
  (WHILE
    (SETQ MHTEXT (ENTSEL "\N MH NAME"))
     (SETQ ENTY (ENTGET (CAR MHTEXT)))
     (SETQ MH (CDR (ASSOC 1 (ENTGET (CAR MHTEXT)))))
     (SETQ
       cl (ATOF
	    (CDR (ASSOC 1 (ENTGET (CAR (ENTSEL "\N COVER LEVEL")))))
	  )
     )
     (SETQ
       oil (ATOF (CDR (ASSOC 1 (ENTGET (CAR (ENTSEL "\N OIL LEVEL")))))
	   )
     )
     (setq DIA (GETREAL "\N DIAMETRE"))

     (setq DEPTH (- (- CL OIL) (/ DIA 1000)))
     (IF (< DEPTH 2.600)
       (IF (<= DIA 200)
	 (SETQ MH1 (STRCAT MH " " "TYPE=" "A"))
	 (SETQ MH1 (STRCAT MH " " "TYPE=" "B"))
       )
       (IF (< DEPTH 3.5)
	 (SETQ MH1 (STRCAT MH " " "TYPE=" "B"))
	 (IF (< DEPTH 5.4)
	   (SETQ MH1 (STRCAT MH " " "TYPE=" "C"))
	   (SETQ MH1 (STRCAT MH " " "TYPE=" "XXXX"))
	 )
       )
     )







     (setq enty (subst (cons 1 MH1) (assoc 1 enty) enty))
     (entmod enty)
  )
)