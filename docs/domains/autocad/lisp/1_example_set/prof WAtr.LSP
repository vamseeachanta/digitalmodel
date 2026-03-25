					;· ‰“Ì· «·„ÌÊ· ⁄·Ï «·ﬁÿ«⁄ «·ÿÊ·Ï «”›·
					;                                    
					;                                    
(defun C:CV (/ pt1 PT1Y X1 X2 LPT1 LPT2 LPT3 LPT4 T1 T2 T12 RAD DEG)
  (SETQ ROP (GETPOINT "\NGET POINT"))
  (SETQ PT1Y (* 1000 (ATOI (RTOS (/ (CADR ROP) 1000) 2 0))))
  ;(setq pt1y (* 1000 pt1))
					;CURVE
  (command "osnap" "endpoint" "")
  (WHILE (setq X1 (getPOINT "\nfirest point"))
    (setq X2 (GETPOINT "\nsecond point"))


					;ROAD LINE
    (command "osnap" "none" "")
    (setq LPT1 (LIST (CAR X1) (- PT1Y 82.9)))
    (SETQ LPT3 (LIST (CAR X1) (- PT1Y 94.18)))

    (COMMAND "-LAYER" "S" "VCL" "")
    (COMMAND "LINE" LPT1 LPT3 "")
    (SETQ LPT2 (LIST (CAR X2) (- PT1Y 82.9)))
    (SETQ LPT4 (LIST (CAR X2) (- PT1Y 94.18)))
    (COMMAND "LINE" LPT2 LPT4 "")
					;ROAD NAM
    (SETQ T1 (- (/ (+ (CAR X2) (CAR X1)) 2) 5))
    (SETQ T2 (- PT1Y 90.54))
    (SETQ T12 (LIST T1 T2))
    (COMMAND "TEXT" "S" "PRO1 " T12 "0" "V.C")
    (command "osnap" "endpoint" "")
  )

  (command "osnap" "none" "")
					;LINE
  (WHILE


    (SETQ L (ENTGET (car (ENTSEL "SELECT LIN"))))
					;IF
     (IF (<= (car (cdr (ASSOC '10 L))) (car (cdr (ASSOC '11 L))))
;;;;;;;;;;;;;
       (SETQ LP1x (car (cdr (ASSOC '10 L)))
	     LP1y (cadr (cdr (ASSOC '10 L)))
	     LP2x (car (cdr (ASSOC '11 L)))
	     LP2y (cadr (cdr (ASSOC '11 L)))
       )
;;;;;;;;;;;;;;;;;

       (SETQ LP2x (car (cdr (ASSOC '10 L)))
	     LP2y (cadr (cdr (ASSOC '10 L)))
	     LP1x (car (cdr (ASSOC '11 L)))
	     LP1y (cadr (cdr (ASSOC '11 L)))
       )
     )
					;END IF
					;SLOP
     (SETQ DLX	(- LP2X LP1X)
	   DLY	(- LP2Y LP1Y)
	   slop	(atof (rtos (* 100 (/ (/ dly 4) dlx)) 2 2))
     )
					;END SLOP


					;ROAD LINE

     (if (<= slop 0)
       (setq LPT1 (LIST lp1X (- PT1Y 82.9))
	     LPT3 (LIST lp2x (- PT1Y 94.18))
       )
       (setq LPT1 (LIST lp1X (- PT1Y 94.18))
	     LPT3 (LIST lp2x (- PT1Y 82.9))
       )
     )

     (COMMAND "-LAYER" "S" "VCL" "")
     (command "-osnap" "none")
     (COMMAND "LINE" LPT1 LPT3 "")
					; text

     (SETQ RAD (ANGLE LPT1 LPT3))
     (setq deg (atof (angtos rad 0 5)))

     (SETQ T12 (LIST (+ (/ dlx 2) lp1x)
		     (+ (/ (+ (cadr lpt1) (cadr lpt3)) 2) 1.00)
	       )
     )
     (COMMAND "TEXT"
	      "S"
	      "PRO1"
	      "j"
	      "c"
	      T12
	      DEG
	      (strcat (rtos slop 2 2) "%")
     )
  )
)


					;                                
					;                                
					;                                
					;· ‰“Ì· «·„‰”Ê» ⁄·Ï «·ﬁÿ«⁄ «·ÿÊ·Ï
(defun C:d (/ pt1 pt2 datum PT1Y PT2Y PT2L PT2X)
  (COMMAND "-OSNAP" "ENDPOINT")
  (setq pt1 (getpoint "insrtfirestpoint"))
 (setq datum (GETREAL "insetr datum"))
  ;(setq datum 375.00)
  (COMMAND "-OSNAP" "INTersection")
  (WHILE (setq pt2 (getpoint "insrtsecondpoint"))
    (setq pt1y (cadr pt1))
    (setq pt2y (cadr pt2))
    (setq pt2x (+ (car pt2) 0))
    (SETQ PT2L (RTOS (+ (/ (- PT2Y PT1Y) 10) DATUM) 2 3))
    (SETQ TX1PT (LIST (+ PT2X 4.0) (- PT1Y 43.09)))
    (COMMAND "LAYER" "SET" "DRLEVEL" "")
    (command "OSNAP" "NONE")
    (COMMAND "TEXT" "S" "PRO1 " TX1PT "90" PT2L)
    (COMMAND "-OSNAP" "INTersection")
  )
)


					;                         
					;                         
					;                         
					;                         
					;· Õ—Ìﬂ «·„‰«”Ì» «· ’„Ì„Ì…
(defun c:ml (/ p1)
  (setq p1 (list 0 -73.34))
  (command "move" (ssget) "" p1 "")
)
					;                                  
					;                                  
					;· ‰“Ì· «· ﬁ«ÿ⁄«  ⁄·Ï «·ﬁÿ«⁄ «·ÿÊ·Ï
(defun C:TS (/ pt1 XTX1PT PT1Y TXVAL TX1PT TX2PT)
  (setq pt1 (getREAL "ROAD NO"))
  (WHILE (setq XTX1PT (getREAL "STATION"))
    (setq pt1y (* 1000 pt1))
    (SETQ TX1PT (LIST (+ XTX1PT 4.0) (- PT1Y 78.0)))

;;;    (IF	(>= XTX1PT 1000)
;;;      (SETQ TXVAL (STRCAT "1+" (substr (RTOS XTX1PT 2 1) 2)))
;;;
;;;      (IF (>= XTX1PT 100)
;;;	(SETQ TXVAL (STRCAT "0+" (RTOS XTX1PT 2 1)))
;;;
;;;	(IF (>= XTX1PT 10)
;;;	  (SETQ TXVAL (STRCAT "0+0" (RTOS XTX1PT 2 1)))
;;;
;;;	  (SETQ TXVAL (STRCAT "0+00" (RTOS XTX1PT 2 1)))
;;;	)
;;;      )
;;;    )
    (SETQ TXVAL (RTOS XTX1PT 2 3))
    (COMMAND "-OSNAP" "NONE")
    (COMMAND "-LAYER" "S" "STA" "")
    (COMMAND "TEXT" "S" "PRO1 " TX1PT "90" TXVAL)
					;ROAD LINE
    (setq LPT1 (LIST XTX1PT PT1Y))
    (SETQ LPT2 (LIST XTX1PT (+ 180 PT1Y)))
    (COMMAND "-LAYER" "S" "ROIN" "")
    (COMMAND "LINE" LPT1 LPT2 "")
					;ROAD NAM
    (SETQ TX2PT (LIST (CAR TX1PT) (+ 78.0 (CADR TX1PT))))
    (COMMAND "-LAYER" "S" "RON" "")    (COMMAND "TEXT"    "S"	     "PRO1 "	     TX2PT
	     "90"
	     (STRCAT "RO " (STRCASE (GETSTRING "RON")))
    )
  )
)



					;                                           
					;                                           
					;                                           
					; ·«Œ– «·„Ì· «·ÿÊ·Ï
(defun C:CU ()
  (command "osnap" "endpoint" "")
  (if (setq pt1 (getPOINT "\N SELECT POINT"))

    (progn
      (command "zoom"
	       "c"
	       (list 300.0 (* (getreal "\nro nomber") 1000))
	       "500"
      )
					;·«Œ– ‰ﬁÿ Ì‰ ⁄·Ï «·« Ã«Â «·ÿÊ· ··ÿ—Ìﬁ
      (command "osnap" "nea" "")
      (SETQ LP1	 (GETPOINT "SELECT FIREST POINT")
	    LP2	 (GETPOINT "SELECT SECOND POINT")
	    LP1X (CAR LP1)
	    LP1Y (CADR LP1)
	    LP2X (CAR LP2)
	    LP2Y (CADR LP2)
      )





      (SETQ DLX	(- LP2X LP1X)
	    DLY	(- LP2Y LP1Y)
      )
					;·Õ”«» «·„Ì· «·⁄—÷Ï »„ﬁÌ«” 1/4

      (setq slopv1  (atof (rtos (* 100 (/ (/ dly 4.00) dlx)) 2 3))
	    slopv2  (* slopv1 -1)
	    txslop1 (rtos slopv1 2 3)
	    txslop2 (rtos slopv2 2 3)
      ) ;_ end of setq
      (setq side (getstring "\n side"))
					;  ÕœÌœ  ‰ﬁÿ… «·ﬂ «»… text point
					;„·ÕÊŸ… „Â„… · ÕœÌœ «·Ã«‰» «·«Ì„‰ «Ê «
					;·«Ì”— ÌÃ» «‰ ÌﬂÊ‰ «·Õ—› «·«‰Ã·Ì“Ì… ’€Ì— Ê·Ì”» ﬂ» «·
      (if (= side "r")
	(SETQ txpnt1 (list (+ 2.77 (car pt1)) (+ 4.19 (cadr pt1)))
	      txpnt2 (list (car txpnt1) (+ (cadr txpnt1) 8.53))
	)
	(if (= side "l")
	  (SETQ	txpnt2 (list (+ 2.77 (car pt1)) (+ 4.19 (cadr pt1)))
		txpnt1 (list (car txpnt2) (+ (cadr txpnt2) 8.53))
	  )
	  "x"
	)
      )

					;text «·ﬂ «»…

      (command "osnap" "none" "")
      (command "layer" "set" "super" "")
      (COMMAND "TEXT" "S" "super" "j" "c" txpnt2 "90" txslop1)
      (COMMAND "TEXT" "j" "c" txpnt1 "90" txslop2)
      (command "zoom" "c" pt1 "53.0")
					;(sup)
    )
					;(sup)





					;                            ·⁄„· «· œ—Ã ›Ï ·„Ì· «·⁄—÷Ï                                       
					;                                                                                             
					;                                                                                             
					;                                                                                             

    (progn
      (while (setq tx1 (entget (car (entsel "\nselect text1:"))))
	(setq tx2 (entget (car (entsel "\nselect text2:")))
	) ;_ end of setq
	(setq txv1  (cdr (assoc '1 tx1)) ;text value
	      txpn1 (cdr (assoc '10 tx1))
	)				;text coor

	(setq txv2  (cdr (assoc '1 tx2))
	      txpn2 (cdr (assoc '10 tx2))
	)				;text coor

	(setq v1 (atof txv1)
	      v2 (atof txv2)
	) ;_ end of setq
	(setq v0 (- v2 v1))

	(setq xtx1 (car txpn1)
	      ytx  (cadr txpn1)
	      xtx2 (car txpn2)
	) ;_ end of setq

	(setq txdis (abs (- xtx2 xtx1))
	      txper (/ v0 txdis)
	)


 ;_ end of setq
	(command "osnap" "end,INS")
	(while (setq pnt (getpoint "\nSelect station point:"))
	  (setq	xpnt   (car pnt)
		pntdis (- xpnt (- xtx1 2.768))
		pntv   (rtos (+ v1 (* pntdis txper)) 2 3)
		txtpnt (list (+ xtx1 pntdis) ytx)
	  ) ;_ end of setq
	  (setvar "osmode" 0)

					;··„”Õ ﬁ»· «·ﬂ «»…
					;                 


	  (setq	pe1 (list (- (car txtpnt) 2.5) (- (cadr txtpnt) 0.5))
		pe2 (list (+ (car txtpnt) 0.5) (+ (cadr txtpnt) 10.00))
	  )

	  (command "osnap" "none")
	  (command "erase" "w" pe1 pe2 "")
	  (COMMAND "TEXT" "S" "super" txtpnt "90" pntv)
	  (setvar "osmode" 33)
	) ;_ end of while

					;)
      )
    )
  )
)
 ;_ end of defun

					;‰Â«Ì… «· œ—Ã ›Ï «·„Ì· «·⁄—÷Ï                                                              
					;                                                                                          					;· ‰“Ì· «·„Ì· «·⁄—÷Ï ›Ï »œ«Ì… Ê ‰Â«Ì… «·ÿ—Ìﬁ

					;·· ‰ﬁ· »Ì‰ «·ﬁÿ«⁄«  «·ÿÊ·ÌÂ œ«Œ· «·—”„
					;                                      

(defun c:ze ()
  (command "zoom"
	   "c"
	   (list 300.0 (* (getreal "\nro nomber") 1000))
	   "500"
  )
)

					;·ﬂ «»… «·›—ﬁ »Ì‰ «·„‰”Ê» «· ’„Ì„Ï Ê „‰”Ê» «·«»Ê«» ⁄·Ï «·ÿ—Ìﬁ
					;                                                           
					;                                                           
					;                                                           




(defun c:x1 (/ txt1 txt2 txpn txv1 txv2 v1 v2 v0 txt y0 txtpnt)
  (while
    (setq tx1 (entget (car (entsel "\nselect text1:"))))
    (progn
      (setq tx2 (entget (car (entsel "\nselect text2:"))))
      (setq txv1 (cdr (assoc '1 tx1)))
      (setq txpn (cdr (assoc '10 tx2)))
      (setq txv2 (cdr (assoc '1 tx2)))
      (setq v1 (atof txv1)
	    v2 (atof txv2)
      ) ;_ end of setq
      (setq v0 (- v2  v1)))
      (setq txt (rtos v0 2 3))
      (setq y0 (car txpn)
	    y1 (cadr txpn)
	    y2 (- y1 30.0)
      )
      (setq txtpnt (list y0 y2))
      (command "osnap" "non")
      (if (<= v0 0.00)
	(command "layer" "set" "defc" "")
	(command "layer" "set" "deff" "")
      )
      (COMMAND "TEXT"  txtpnt "" txt "" "")
    ) ;_ end of progn
  ) ;_ end of while
