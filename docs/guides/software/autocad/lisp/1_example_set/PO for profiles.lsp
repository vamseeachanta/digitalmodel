
      ;;;(IN THE NAME OF ALLAH THE BENIFICIENT THE MERCIFULL);;;
      ;;;      ( ALLAH'S PEACE BE UPON PROPHET MUHAMMAD)     ;;;




      ;;; This program is created by AAMER HAMEED AWAN       ;;;
      ;;;       (BIN ATEK EST. MEDINA MUNAVRA    K.S.A)             ;;;




      ;;; Purpose of this program is to draw the PROFILE OF ROAD                ;;;
      ;;; Entering only the Station and Distance eg.[25,601.253]                        ;;;
      ;;; The by default vertical scale is assumed 10 time the Horizontal scale ;;;
      ;;; But any how the user can change it from a line bellow.                           ;;;








;MODIFIED ON 04-09-2K6
************************************************************************
************************************************************************

;;; PO STARTS ;;;
;;; PO STARTS ;;;
;;; PO STARTS ;;;
;;; PO STARTS ;;;

(defun c:po()
(setvar "insunits" 4)
(setvar "aunits" 0)
(setvar "angbase" 0)
(setvar "angdir" 0)
(setvar "lunits"  2)
(setvar "dimzin" 1)
(command "lweight" "BYLAYER")
(command "color" "bylayer")
(command "layer" "N" "N.G" "c" "7" "N.G" "")
(command "layer" "N" "green82" "c" "82" "green82" "")
(command "Layer" "N" "cyan2k6" "c" "4" "cyan2k6" "")
(command "layer" "N" "red2k6" "c" "1" "red2k6" "")
(setq oldsnap (getvar "osmode"))
(setq newsnap 0)
(setvar "osmode" newsnap)
(setq verscale 10)   ;;;;;;*******;;;;;;user can change the vertical scale from this line;;;********;;;;;

(setq txtht 6.4)
 
(if (or (< verscale 10)
	(= verscale 10)
	)
(setq stxtht (* 0.60 verscale))
(setq stxtht 6)
)
(setq ra 90)  
(terpri)


;************************************************************
(setq datreal (getpoint "\nPlease Enter The Datum:"))
;************************************************************



(setq datscaled  (list (car datreal)(* verscale (cadr datreal)) ))
 (setq opt nil)
  (while  (and(/= opt "Yes")(/= opt "No"))
   
   (SETQ dopt "Yes")
   (initget "Yes No Options")
        (setq option (getkword (acet-str-format "\nAttach Start-up Template [Yes/No/Options] <%1>: " Dopt))
              option (if option option Dopt) )
(IF (= option "Options")
      (pro)
)
      
(setq opt option)

    )







  (if
     (= option "Yes")
              
    
  (PROGN
    (TOP-MIDDLE-LOWER-SQUARE-BOXES2K6)
    (TOP-MIDDLE-LOWER-CURVED-BOXES2K6)
    (LEFT-HEADINGS-2K6)
);; END PROGN
    );;end iff;;
(COMMAND "REGEN")




;*************************************************************
(setq ptleft1 datscaled)
(setq ptleft2 (list (car datscaled)(- (cadr datscaled) 60) ))
(setq ptleft3 (list (car datscaled) (- (cadr datscaled) 62 )  ))
(setq ptleft4 (list (car datscaled) (- (cadr datscaled) 122)  ))
(setq ptleft5 (list (car datscaled) (- (cadr datscaled) 124)   ))
(setq ptleft6 (list (car datscaled) (- (cadr datscaled) 184)  ))
(setq suspender (list (car pt2)(cadr ptleft6) ))
;*************************************************************



;WRITING DOWN THE TEXT ON THE VERTICAL SCALE OF TEMPLATE

(setq dattextp1 (list(- (car datscaled) 93.2857) (+ (cadr datscaled) 2.5629) ))
(setq text1 (rtos (cadr datreal) 2 2))


(setq dattextp2 (list(- (car datscaled) 44.8037) (+ (cadr datscaled) (* verscale 5))) )
(setq dattextp3 (list (car dattextp2)(+ (cadr dattextp2) (* verscale 5)) ))
(setq dattextp4 (list (car dattextp3)(+ (cadr dattextp3) (* verscale 5)) ))
(setq dattextp5 (list (car dattextp4) (+ (cadr dattextp4) (* verscale 5)) ))
(setq dattextp6 (list (car dattextp5) (+ (cadr dattextp5) (* verscale 5)) ))
(setq dattextp7 (list (car dattextp6) (+ (cadr dattextp6) (* verscale 5)) ))
(setq dattext1 (strcat "Datum =" text1) )
(setq dattext2 (rtos (+(cadr datreal) 5) 2 2) )
(setq dattext3 (rtos (+(cadr datreal) 10) 2 2) )
(setq dattext4 (rtos (+ (cadr datreal) 15) 2 2))
(setq dattext5 (rtos (+ (cadr datreal) 20) 2 2 ))
(setq dattext6 (rtos (+ (cadr datreal) 25) 2 2))
(setq dattext7 (rtos (+ (cadr datreal) 30) 2 2 ))

(IF    
   (= option "Yes")
         
  (progn
    (SCALE-VER)
(COMMAND "ZOOM" "E")
(COMMAND "REGEN")
(command "layer" "S" "red2k6" "")
(command "style" "rcp2k6" "romanc.shx" "0" "1" "0" "n" "n" "n")
(command "text" dattextp1 stxtht "0.00" dattext1)
(command "text" dattextp2 stxtht "0.00" dattext2)
(command "text" dattextp3 stxtht "0.00" dattext3)
(command "text" dattextp4 stxtht "0.00" dattext4)
(command "text" dattextp5 stxtht "0.00" dattext5)
(command "text" dattextp6 stxtht "0.00" dattext6)
(command "text" dattextp7 stxtht "0.00" dattext7)
 );;end progn;
  );; end if;;
(terpri)



;*****************************************************************
(setq pt1 (getpoint "N.G.L. Line Starts From/Paste the N.G.L.s:"))
;*****************************************************************


;*****************************************************************
(setq pt1scaled (list (car pt1 )(* (cadr pt1) verscale)))
;*****************************************************************
;WRITING THE DISTANCE AT FIRST POINT ONLY IN 0+000.00 FORMATE


(setq ngldistextp1 (list (+ (car pt1scaled) 8.4) (+ (cadr ptleft6 ) 4.8)))
(command "layer" "s" "N.G" "")
(command "style" "rsp2k6" "romans.shx" "0" "1" "0" "n" "n" "n")
(command "text" ngldistextp1 txtht ra (km+mmm.mm (car pt1)))


************************************************************
;WRITING THE N.G.L. AT FIRST POINT ONLY
(setq ngltextp (list (+ (car pt1scaled) 8.4) (+ (cadr ptleft4 ) 4.8) ))
(setq ngltext (rtos (cadr pt1) 2 3 ))
(command "layer" "s" "N.G" "")
(command "style" "rsp2k6" "romans.shx" "0" "1" "0" "n" "n" "n")
(command "text" ngltextp txtht ra ngltext)


;************************************************************
(PRINC (CAR PT1))(PRINC ",")(PRINC (CADR PT1))
;************************************************************


;;; -WHILE-   STARTES;;;
;;;- WHILE-   STARTES;;;
;;; -WHILE-   STARTES;;;
;;; -WHILE-   STARTES;;;
  
(terpri)


(while
(setq pt2 (getpoint "\nN.G.L. Line Goes To <close>:"))
  
(setq pt2scaled (list (car pt2)(* (cadr pt2 ) verscale) ) )




;*************************************************************
;WRITING THE DISTANCE TEXT IN 0+00.00 FORMATE

(setq ngldistextp (list (+ (car pt2scaled) 8.4) (+ (cadr ptleft6 ) 4.8) ))
(command "layer" "s" "N.G" "")
(command "lweight" "bylayer")
(command "style" "rsp2k6" "romanc.shx" "0" "1" "0" "n" "n" "n")
(command "text" ngldistextp txtht ra (km+mmm.mm (car pt2)))

;*************************************************************
;DRAWING THE LINE OF NGL
(setq ptleft1 datscaled)
(setq ptleft2 (list (car datscaled)(- (cadr datscaled) 60) ))
(setq ptleft3 (list (car datscaled) (- (cadr datscaled) 62 )  ))
(setq ptleft4 (list (car datscaled) (- (cadr datscaled) 122)  ))
(setq ptleft5 (list (car datscaled) (- (cadr datscaled) 124)   ))
(setq ptleft6 (list (car datscaled) (- (cadr datscaled) 184)  ))
(setq suspender (list (car pt2)(cadr ptleft6) ))

(command "layer" "s" "N.G" "")
(command "lweight" 0.40)

(command "line" pt1scaled pt2scaled "")

;************************************************************
;DRAWING THE VERTICAL LINES
(setq ngltextp (list (+ (car pt2scaled) 8.4) (+ (cadr ptleft4 ) 4.8) ))
(setq ngltext (rtos (cadr pt2) 2 3 ))
(command "layer" "s" "N.G" "")
(command "style" "rsp2k6" "romans.shx" "0" "1" "0" "n" "n" "n")
(command "lweight" "bylayer")
(command "text" ngltextp  txtht ra ngltext)


(command "layer" "s" "cyan2k6" "")
(command "lweight" "bylayer")
(command  "linetype" "s" "continuous" "")
(command "line" pt2scaled suspender "")


;****************************************************************
;REPEATING THE COMMAND LINE
(setq pt1scaled pt2scaled)
(command "regen")
(PRINC (CAR PT2))(PRINC ",")(PRINC (CADR PT2))

)
  ;;; -WHILE-  CLOSED;;;
  ;;; -WHILE - CLOSED;;;
  ;;; -WHILE-  CLOSED;;;
  ;;; -WHILE-  CLOSED;;;
  
;**************************************************************


;**************************************************************
;SECOND PHASE FOR CLOSING THE BOX OF PROFILE

(command "osnap" "off")
(command "zoom" "e")
(setq attachment suspender)
(command "osnap" "off")

(setq ptright1 (list (car attachment) (+ (cadr attachment) 184) ) )
(setq ptright2 (list (car attachment) (+ (cadr attachment) 124 )) )
(setq ptright3 (list (car attachment) (+ (cadr attachment) 122)) )
(setq ptright4 (list (car attachment) (+ (cadr attachment) 62)) )
(setq ptright5 (list (car attachment) (+ (cadr attachment) 60 )) )
(setq ptright6 attachment) 


;;;; drawing the closing vertical  lines and arcs;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(COMMAND "LAYER" "S" "green82" "")
(command "lweight" 0.35)


(setq attachment3 (list (+
				   (car attachment)
				   20
				   )
				 (cadr attachment)
				 )
      )



(command "line" attachment attachment3 "")
(command "line" attachment3 (list (car attachment3)
				  (+
				    (cadr attachment3)
				    40
				    )
				  )
	 "")
(command "arc" (list (car attachment3)
		     (+
		       (cadr attachment3)
		       40
		       )
		     )
	 "c"
	 (list (-
		 (car attachment3)
		 20
		 )
	       (+
		 (cadr attachment3)
		 40
		 )
	       )
	 ptright5
	 )

(setq attachment2 (list (+
			  (car ptright4)
			  20
			  )
			(cadr ptright4)
			)
      )

(command "line" ptright4 attachment2 "")
				 
			
		


(command "line" attachment2 (list (car attachment2)
				  (+
				    (cadr attachment2)
				    40
				    )
				  )
	 "")

(command "arc" (list (car attachment2)
		     (+
		       (cadr attachment2)
		       40
		       )
		     )
	 "c"
	 (list (-
		 (car attachment2)
		 20
		 )
	       (+
		 (cadr attachment2)
		 40
		 )
	       )
	 ptright3
	 )

(setq attachment1 (list (+
			  (car ptright2)
			  20
			  )
			(cadr ptright2)
			)
      )
(command "line" ptright2 attachment1 "")

(command "line" attachment1 (list (car attachment1)
				  (+
				    (cadr attachment1)
				   40
				    )
				  )
	 "" )

(command "arc" (list (car attachment1)
		     (+
		       (cadr attachment1)
		      40
		       )
		     )
	 "c"
	 (list
	   (car ptright2)
	   (+
	   (cadr attachment1)
	   40
	   )
	   )
	   ptright1
	   )	     



;;drawing the closing horizontal lines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq datscaled  (list (car datreal)(* verscale (cadr datreal)) ))
(setq ptleft1 datscaled)
(setq ptleft2 (list (car datscaled)(- (cadr datscaled) 60) ))
(setq ptleft3 (list (car datscaled) (- (cadr datscaled) 62 )  ))
(setq ptleft4 (list (car datscaled) (- (cadr datscaled) 122)  ))
(setq ptleft5 (list (car datscaled) (- (cadr datscaled) 124)   ))
(setq ptleft6 (list (car datscaled) (- (cadr datscaled) 184)  ))
(setq suspender (list (car pt2)(cadr ptleft6) ))


(command "osnap" "off")
(command "layer" "s" "green82" "")
(command "lweight" 0.35)
(command "linetype" "s" "continuous" "")


(command "line" ptright1 ptleft1 "")
(command "line" ptright2 ptleft2 "")
(command "line" ptright3 ptleft3 "")
(command "line" ptright4 ptleft4 "")
(command "line" ptright5 ptleft5 "")
(command "line" ptright6 ptleft6 "")

;;**********************************************************************
;;**********************************************************************

;;;removing the used settings and setting the defaults;;;;;;;;;;;;;;;;;



(command "lweight" "bylayer")
(command "layer" "s" "0" "")
(command "color" "bylayer")
(command  "linetype" "s" "continuous" "")

(setvar "osmode" oldsnap)
(command "zoom" "e")
(command "regen")
(princ "\n*** !Profile is Complete ***    ***Written By Aamer H. Awan***")
(princ)
  



);;;;;;;;;;;;
;;;END PO;;;;
;;;END PO;;;;
;;;END PO;;;;
;;;END PO;;;;




************************************************************************************
(defun SCALE-VER()
(setq LeadLength (* verscale 1.8))

(setq startwidth 0)
(setq endwidth (* verscale 1))
(setq TOPpt (list (car datscaled)(+ (cadr datscaled)(* verscale 35.6))))
(setq LeadDownPt (list (car TOPpt)(-(cadr TOPpt)LeadLength)))
(setq smarkpt1(list (- (car datscaled)(* verscale 0.250))(+ (cadr datscaled)(* verscale 1))))
  (setq smarkpt1b(list (+(car smarkpt1)(* verscale 0.500))(cadr smarkpt1)))
(setq smarkpt2(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt1)(* verscale 1))))
  (setq smarkpt2b(list (+(car smarkpt2)(* verscale 0.500))(cadr smarkpt2)))
(setq smarkpt3(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt2)(* verscale 1))))
  (setq smarkpt3b(list (+(car smarkpt3)(* verscale 0.500))(cadr smarkpt3)))
(setq smarkpt4(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt3)(* verscale 1))))
  (setq smarkpt4b(list (+(car smarkpt4)(* verscale 0.500))(cadr smarkpt4)))
(setq smarkpt5(list (- (car datscaled)(* verscale 0.500))(+ (cadr smarkpt4)(* verscale 1))))
  (setq smarkpt5b(list (+(car smarkpt5)(* verscale 1.000))(cadr smarkpt5)))
(setq smarkpt6(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt5)(* verscale 1))))
  (setq smarkpt6b(list (+(car smarkpt6)(* verscale 0.500))(cadr smarkpt6)))
(setq smarkpt7(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt6)(* verscale 1))))
  (setq smarkpt7b(list (+(car smarkpt7)(* verscale 0.500))(cadr smarkpt7)))
(setq smarkpt8(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt7)(* verscale 1))))
  (setq smarkpt8b(list (+(car smarkpt8)(* verscale 0.500))(cadr smarkpt8)))
(setq smarkpt9(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt8)(* verscale 1))))
  (setq smarkpt9b(list (+(car smarkpt9)(* verscale 0.500))(cadr smarkpt9)))
(setq smarkpt10(list (- (car datscaled)(* verscale 0.500))(+ (cadr smarkpt9)(* verscale 1))))
  (setq smarkpt10b(list (+(car smarkpt10)(* verscale 1.000))(cadr smarkpt10)))
(setq smarkpt11(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt10)(* verscale 1))))
  (setq smarkpt11b(list (+(car smarkpt11)(* verscale 0.500))(cadr smarkpt11)))
(setq smarkpt12(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt11)(* verscale 1))))
  (setq smarkpt12b(list (+(car smarkpt12)(* verscale 0.500))(cadr smarkpt12)))
(setq smarkpt13(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt12)(* verscale 1))))
  (setq smarkpt13b(list (+(car smarkpt13)(* verscale 0.500))(cadr smarkpt13)))
(setq smarkpt14(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt13)(* verscale 1))))
  (setq smarkpt14b(list (+(car smarkpt14)(* verscale 0.500))(cadr smarkpt14)))
(setq smarkpt15(list (- (car datscaled)(* verscale 0.500))(+ (cadr smarkpt14)(* verscale 1))))
  (setq smarkpt15b(list (+(car smarkpt15)(* verscale 1.000))(cadr smarkpt15)))
(setq smarkpt16(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt15)(* verscale 1))))
  (setq smarkpt16b(list (+(car smarkpt16)(* verscale 0.500))(cadr smarkpt16)))
(setq smarkpt17(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt16)(* verscale 1))))
  (setq smarkpt17b(list (+(car smarkpt17)(* verscale 0.500))(cadr smarkpt17)))
(setq smarkpt18(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt17)(* verscale 1))))
  (setq smarkpt18b(list (+(car smarkpt18)(* verscale 0.500))(cadr smarkpt18)))
(setq smarkpt19(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt18)(* verscale 1))))
  (setq smarkpt19b(list (+(car smarkpt19)(* verscale 0.500))(cadr smarkpt19)))
(setq smarkpt20(list (- (car datscaled)(* verscale 0.500))(+ (cadr smarkpt19)(* verscale 1))))
  (setq smarkpt20b(list (+(car smarkpt20)(* verscale 1.000))(cadr smarkpt20)))
(setq smarkpt21(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt20)(* verscale 1))))
  (setq smarkpt21b(list (+(car smarkpt21)(* verscale 0.500))(cadr smarkpt21)))
(setq smarkpt22(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt21)(* verscale 1))))
  (setq smarkpt22b(list (+(car smarkpt22)(* verscale 0.500))(cadr smarkpt22)))
(setq smarkpt23(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt22)(* verscale 1))))
  (setq smarkpt23b(list (+(car smarkpt23)(* verscale 0.500))(cadr smarkpt23)))
(setq smarkpt24(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt23)(* verscale 1))))
  (setq smarkpt24b(list (+(car smarkpt24)(* verscale 0.500))(cadr smarkpt24)))
(setq smarkpt25(list (- (car datscaled)(* verscale 0.500))(+ (cadr smarkpt24)(* verscale 1))))
  (setq smarkpt25b(list (+(car smarkpt25)(* verscale 1.000))(cadr smarkpt25)))
(setq smarkpt26(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt25)(* verscale 1))))
  (setq smarkpt26b(list (+(car smarkpt26)(* verscale 0.500))(cadr smarkpt26)))
(setq smarkpt27(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt26)(* verscale 1))))
  (setq smarkpt27b(list (+(car smarkpt27)(* verscale 0.500))(cadr smarkpt27)))
(setq smarkpt28(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt27)(* verscale 1))))
  (setq smarkpt28b(list (+(car smarkpt28)(* verscale 0.500))(cadr smarkpt28)))
(setq smarkpt29(list (- (car datscaled)(* verscale 0.250))(+ (cadr smarkpt28)(* verscale 1))))
  (setq smarkpt29b(list (+(car smarkpt29)(* verscale 0.500))(cadr smarkpt29)))
(setq smarkpt30(list (- (car datscaled)(* verscale 0.500))(+ (cadr smarkpt29)(* verscale 1))))
  (setq smarkpt23b(list (+(car smarkpt23)(* verscale 0.500))(cadr smarkpt23)))

  	(command "layer" "s" "N.G" "")
  	(COMMAND "PLINE" TOPpt "W" startwidth endwidth LeadDownPt "")
  	(COMMAND "LINE" datscaled TOPpt "")
        (command "layer" "s" "cyan2k6" "")
  	(COMMAND "LINE" smarkpt1 smarkpt1b "")
  	(COMMAND "LINE" smarkpt2 smarkpt2b "")
  	(COMMAND "LINE" smarkpt3 smarkpt3b "")
	(COMMAND "LINE" smarkpt4 smarkpt4b "")
	(COMMAND "LINE" smarkpt5 smarkpt5b "")
	(COMMAND "LINE" smarkpt6 smarkpt6b "")
	(COMMAND "LINE" smarkpt7 smarkpt7b "")
	(COMMAND "LINE" smarkpt8 smarkpt8b "")
	(COMMAND "LINE" smarkpt9 smarkpt9b "")
	(COMMAND "LINE" smarkpt10 smarkpt10b "")
	(COMMAND "LINE" smarkpt11 smarkpt11b "")
	(COMMAND "LINE" smarkpt12 smarkpt12b "")
	(COMMAND "LINE" smarkpt13 smarkpt13b "")
	(COMMAND "LINE" smarkpt14 smarkpt14b "")
	(COMMAND "LINE" smarkpt15 smarkpt15b "")
	(COMMAND "LINE" smarkpt16 smarkpt16b "")
	(COMMAND "LINE" smarkpt17 smarkpt17b "")
	(COMMAND "LINE" smarkpt18 smarkpt18b "")
	(COMMAND "LINE" smarkpt19 smarkpt19b "")
	(COMMAND "LINE" smarkpt20 smarkpt20b "")
	(COMMAND "LINE" smarkpt21 smarkpt21b "")
	(COMMAND "LINE" smarkpt22 smarkpt22b "")
	(COMMAND "LINE" smarkpt23 smarkpt23b "")
	(COMMAND "LINE" smarkpt24 smarkpt24b "")
	(COMMAND "LINE" smarkpt25 smarkpt25b "")
	(COMMAND "LINE" smarkpt26 smarkpt26b "")
	(COMMAND "LINE" smarkpt27 smarkpt27b "")
	(COMMAND "LINE" smarkpt28 smarkpt28b "")
	(COMMAND "LINE" smarkpt29 smarkpt29b "")
	(COMMAND "LINE" smarkpt30 smarkpt30b "")
	  

);; END OF DEFUN SCALE
********************************************************************
********************************************************************
(DEFUN LEFT-HEADINGS-2K6()
  (setq deshdpt (list (- (car datscaled)93)(- (cadr datscaled) 25)))
  (setq elevhdpt1 (list (- (car datscaled)72.50)(- (cadr datscaled) 42)))
   (setq grdhdpt (list (- (car datscaled)93)(- (cadr datscaled) 87)))
  (setq elevhdpt2 (list (- (car datscaled)72.50)(- (cadr datscaled) 104)))
   (setq stahdpt (list (- (car datscaled)87.5)(- (cadr datscaled) 160)))
  (command "layer" "s" "N.G" "")
  (command "style" "rcp2k6" "romanc.shx" "0" "1" "0" "n" "n" "n")
  (command "text" deshdpt "8" "0" "DESIGN")
  (command "text" elevhdpt1 "8" "0" "ELEVATION")
  (command "text" grdhdpt "8" "0" "GROUND")
  (command "text" elevhdpt2 "8" "0" "ELEVATION")
  (command "text" stahdpt "12" "0" "STATION")
  
  )

(DEFUN TOP-MIDDLE-LOWER-SQUARE-BOXES2K6()
(setq ptleft1 datscaled)
(setq ptleft1L (list (- (car datscaled) 100)(cadr datscaled)))
(setq ptleft2 (list (car datscaled)(- (cadr datscaled) 60) ))
(setq ptleft2L (list (- (car datscaled) 100)(cadr ptleft2)))
(setq ptleft3 (list (car datscaled) (- (cadr datscaled) 62 )  ))
(setq ptleft3L (list (- (car datscaled) 100)(cadr ptleft3)))
(setq ptleft4 (list (car datscaled) (- (cadr datscaled) 122)  ))
  (setq ptleft4L (list (- (car datscaled) 100)(cadr ptleft4)))
(setq ptleft5 (list (car datscaled) (- (cadr datscaled) 124)   ))
  (setq ptleft5L (list (- (car datscaled) 100)(cadr ptleft5)))
(setq ptleft6 (list (car datscaled) (- (cadr datscaled) 184)  ))
  (setq ptleft6L (list (- (car datscaled) 100)(cadr ptleft6)))



  (COMMAND "LAYER" "S" "green82" "")
  (command "lweight" 0.35)
  (command "line" ptleft1 ptleft1L ptleft2L ptleft2 ptleft1 "")
  (command "line" ptleft3 ptleft3L ptleft4L ptleft4 ptleft3 "")
  (command "line" ptleft5 ptleft5L ptleft5L ptleft6L ptleft6 ptleft5 "")
   (command "lweight" "BYLAYER")
  
  
);; end of defun TEMPLATE-LEFT
************************************************************************
************************************************************************
(DEFUN TOP-MIDDLE-LOWER-CURVED-BOXES2K6()
  (setq ULD1 (list (- (car datscaled) 98)(- (cadr datscaled) 7)))
  (setq ULU1 (list (- (car datscaled) 93)(- (cadr datscaled) 2)))
  (setq URU1 (list (- (car datscaled) 7 )(- (cadr datscaled) 2)))
  (setq URD1 (list (- (car datscaled) 2 )(- (cadr datscaled) 7)))
  (setq LRU1 (list (- (car ptleft2 ) 2)(+ (cadr ptleft2) 7)))
  (setq LRD1 (list (- (car ptleft2 ) 7)(+ (cadr ptleft2) 2)))
  (setq LLD1 (list (- (car ptleft2 ) 93)(+(cadr ptleft2) 2)))
  (setq LLU1 (list (- (car ptleft2 ) 98)(+ (cadr ptleft2) 7)))

  	(command "lweight" 0.35)
  	(COMMAND "LAYER" "S" "green82" "")
 	(command "lweight" 0.35)
	(command "arc" ULU1 "E" ULD1 "R" "5")
  	(command "line" ULU1 URU1 "")
 	 (command "arc" URD1 "E" URU1 "R" "5")
  	(command "line" URD1 LRU1 "")
   	(command "arc" LRD1 "E" LRU1 "R" "5")
 	 (command "line" LRD1 LLD1 "")
  	(command "arc" LLU1 "E" LLD1 "R" "5")
        (command "line" LLU1 ULD1 "")
  
  

  	
  (setq ULD2 (list (- (car ptleft3) 98)(- (cadr ptleft3) 7)))
  (setq ULU2 (list (- (car ptleft3) 93)(- (cadr ptleft3) 2)))
  (setq URU2 (list (- (car ptleft3) 7 )(- (cadr ptleft3) 2)))
  (setq URD2 (list (- (car ptleft3) 2 )(- (cadr ptleft3) 7)))
  (setq LRU2 (list (- (car ptleft4 ) 2)(+ (cadr ptleft4) 7)))
  (setq LRD2 (list (- (car ptleft4 ) 7)(+ (cadr ptleft4) 2)))
  (setq LLD2 (list (- (car ptleft4 ) 93)(+(cadr ptleft4) 2)))
  (setq LLU2 (list (- (car ptleft4 ) 98)(+ (cadr ptleft4) 7)))

  	(COMMAND "LAYER" "S" "green82" "")
 	(command "lweight" 0.35)
	(command "arc" ULU2 "E" ULD2 "R" "5")
  	(command "line" ULU2 URU2 "")
 	 (command "arc" URD2 "E" URU2 "R" "5")
  	(command "line" URD2 LRU2 "")
   	(command "arc" LRD2 "E" LRU2 "R" "5")
 	 (command "line" LRD2 LLD2 "")
  	(command "arc" LLU2 "E" LLD2 "R" "5")
        (command "line" LLU2 ULD2 "")
  

  

  (setq ULD3 (list (- (car ptleft5) 98)(- (cadr ptleft5) 7)))
  (setq ULU3 (list (- (car ptleft5) 93)(- (cadr ptleft5) 2)))
  (setq URU3 (list (- (car ptleft5) 7 )(- (cadr ptleft5) 2)))
  (setq URD3 (list (- (car ptleft5) 2 )(- (cadr ptleft5) 7)))
  (setq LRU3 (list (- (car ptleft6 ) 2)(+ (cadr ptleft6) 7)))
  (setq LRD3 (list (- (car ptleft6 ) 7)(+ (cadr ptleft6) 2)))
  (setq LLD3 (list (- (car ptleft6 ) 93)(+(cadr ptleft6) 2)))
  (setq LLU3 (list (- (car ptleft6 ) 98)(+ (cadr ptleft6) 7)))


  	(COMMAND "LAYER" "S" "green82" "")
 	(command "lweight" 0.35)
	(command "arc" ULU3 "E" ULD3 "R" "5")
  	(command "line" ULU3 URU3 "")
 	 (command "arc" URD3 "E" URU3 "R" "5")
  	(command "line" URD3 LRU3 "")
   	(command "arc" LRD3 "E" LRU3 "R" "5")
 	 (command "line" LRD3 LLD3 "")
  	(command "arc" LLU3 "E" LLD3 "R" "5")
        (command "line" LLU3 ULD3 "")
   (command "lweight" "BYLAYER")
	
  );;; END OF DEFUN TOP-MIDDLE-LOWER-CURVED-BOXES2K6
************************************************************************
************************************************************************
;;DEFINING THE KM+MMM.MM FOR WRITING DISTANCE IN 0+000.00 FORMATE


(defun km+mmm.mm ( abc / km1 mr mr1 kmr km m )


  (setq km1(/ abc 1000.0000))
  (setq mr(rem abc 1000.0000))
  (setq mr1(/ mr 1000.0000))
  (setq kmr(- km1 mr1))
  (setq km (rtos kmr 2 0))
  (setq m (rtos mr 2 2))
  
    
  (if
  
     (and
    (and
      (> mr 0)
      (< mr 10)
    )
    (or
      (> kmr 0)
      (= kmr 0)
      )
    )
  

    (setq km+m (strcat km "+00" m))

(progn

  (if
    (and
    (= mr 0)
    (or
      (= kmr 0)
      (> kmr 0)
      )
    )
    (setq km+m (strcat km "+00" m))

 

  (if
    (and
    (or
    (= mr 10)
    (and
    (> mr 10)
    (< mr 100)
    )
    )
    (or
    (> kmr 0)
    (= kmr 0)
    )
    )
    (setq km+m (strcat km "+0" m))



  (if
    (and
    (or
    (= mr 100)
    (> mr 100)
    )
    (or
      (> kmr 0)
      (= kmr 0)
      )
    )

    (setq km+m(strcat km "+" m))

  (if
    (and
    (< mr 0)
    (or
      (< kmr 0)
      (= kmr 0)
      )
    )
    (setq km+m (strcat km m))

    (if
    (and
    (= mr 0)
    (or
      (< kmr 0)
      (= kmr 0)
      )
    )
    (setq km+m (strcat km "-00" (rtos (abs mr) 2 2)))



    );; end if
    );; end if
    );; end if
    );; end if
    );; end if
  

  );; end progn
);; end if
  
  );;; end km+mmm.mm






































































































































































































































































































































































































































































































































































































































































































































































































































































;****************************************************************************

(terpri)   
(PRINC "PO for profiles! ***Created by Aamer Hameed Awan (Bin Atek Est. MEDINA)")


;****************************************************************************


















