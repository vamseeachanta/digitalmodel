

(defun DXF (code elist)

; finds the association pair, strips 1st element
	(cdr (assoc code elist))
)
(princ)

(defun c:CEL (/ me ce hl rm bm bmoff dprec i j en ed ety lay ss len etxt wtxt elen pretxt txt posttxt ex ey ez ntxt nz old new nxyz mod)

(prompt "\n\nChange Bench Mark for Annotation Text  v2.2      2/10/93")
;
   (setq me (getvar "menuecho"))
   (setvar "menuecho" 0)
   (setq ce (getvar "cmdecho"))
   (setvar "cmdecho" 0)
   (setq rm (getvar "regenmode"))
   (setvar "regenmode" 0)
   (setq bm (getvar "blipmode"))
   (setvar "blipmode" 0)

;  Prompt for bench mark offset

	(print)
	(initget 1)
	(setq bmoff (getreal "\nEnter value for bench mark offset, <0> to exit? "))
	(if (/= bmoff 0)
		(progn
			(initget 1)
	 		(setq dprec (getint "Enter decimal precision for annotation? "))
 			(prompt "\n\nBench Mark Offset value:            ")(princ bmoff)
 			(prompt   "\nDecimal precision on annotation:    ")(princ dprec)

;  Locate text to change and select all text on that layer

; prompt user to pick text entity
		
;  Create selection set of all annotation text entities


		   	(setq ss (ssget (list (cons 0 "TEXT"))))


;  Process text and modify based on bench mark offset
			(setq i 0
			      mod 0)
			(setq len (sslength ss))
			(while (< i len)
				(setq en (ssname ss i)
      	    		  ed (entget en)
      	    		  txt ""
	      		      pretxt ""
    	  	    	  posttxt ""
    	  	    	  noproc 0)
 			   	(setq etxt (dxf 1 ed))
 			   	(setq wtxt etxt)
	    	  	(setq ex (car (dxf 10 ed)))
    	  		(setq ey (cadr (dxf 10 ed)))
      			(setq ez (caddr (dxf 10 ed)))

				(setq elen (strlen etxt))
				(setq j 1)
; Locate PRE-text
				(if (not (member (substr etxt j 1) '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ".")))
					(progn
						(while (< j  (1+ elen))
							(if (not (member (setq cc (substr etxt j 1)) '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ".")))
								(setq j (1+ j))
								(progn
									(setq pretxt (substr etxt 1 (- j 1)))
									(setq etxt (substr etxt j elen))
									(setq j (+ elen 2))
									(setq noproc 1)
								);progn
							);if
						);while
						(if (= j (1+ elen))
							(setq noproc 1)
						);if
					);progn
					(setq pretxt "")
				);if
;
; Locate text to modify
;
				(setq j 1)
				(setq elen (strlen etxt))
				(if (> elen 0)
					(progn
						(while (< j  (1+ elen))
							(if (member (setq cc (substr etxt j 1)) '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "."))
								(setq j (1+ j))
								(progn
									(if (member cc '("\"" "\'"))
										(progn
											(setq noproc 1)
											(setq j (1+ elen))
										);progn
										(progn
											(setq txt (substr etxt 1 (- j 1)))
											(setq posttxt (substr etxt j elen))
											(setq j (1+ elen))
										);progn
									);if
								);progn
							);if
							(if (= j elen)
								(progn
									(setq txt etxt)
									(setq posttxt "")
								);progn
							);if
						);while
					);progn
				);if
;
; Add BM Change
;

	   		  	(if (= noproc 0)
	   		  		(progn
	   		  			(setq ztxt (rtos (+ (atof txt) bmoff) 2 dprec))
		  				(setq ntxt (strcat pretxt ztxt posttxt))
	    				(setq nz (+ ez bmoff))
;
;  Modifying entity data
;
						(setq old (assoc 1 ed))
						(setq new (cons 1 ntxt))		; Text value
						(setq ed (subst new old ed))
						(setq ed (subst new old ed))
						(entmod ed)
						(setq mod (1+ mod))
					);progn
					(progn
					);progn
				);if
				(setq i (+ i 1))
  		 	);while
		) ;progn

;
; result to BM OFFSET = 0
;
		(prompt "\n\nProgram terminated.  ")

	);endif
	(princ "\n\nThere were ")(princ len)(princ " entities processed and ")
		(princ mod)(princ " entities modified.")


; reset system variables
	(setvar "regenmode" rm)
	(setvar "blipmode" bm)
	(setvar "cmdecho" ce)
	(setvar "menuecho" me)


	(princ)

) ;End of CEL

