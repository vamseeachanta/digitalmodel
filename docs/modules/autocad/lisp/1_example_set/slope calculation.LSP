(defun C:sl (/ pt1 PT1Y X1 X2 LPT1 LPT2 LPT3 LPT4 T1 T2 T12 RAD DEG)
  (SETQ ROP (GETPOINT "\NGET POINT"))
  (SETQ PT1Y (* 1000 (ATOI (RTOS (/ (CADR ROP) 1000) 2 0))))
  ;(setq pt1y (* 1000 pt1))
					;CURVE
  (command "osnap" "endpoint" "")
;;;  (WHILE (setq X1 (getPOINT "\nfirest point"))
;;;    (setq X2 (GETPOINT "\nsecond point"))
;;;
;;;
;;;					;ROAD LINE
;;;    (command "osnap" "none" "")
;;;    (setq LPT1 (LIST (CAR X1) (- PT1Y 82.9)))
;;;    (SETQ LPT3 (LIST (CAR X1) (- PT1Y 94.18)))
;;;
;;;    (COMMAND "-LAYER" "S" "VCL" "")
;;;    (COMMAND "LINE" LPT1 LPT3 "")
;;;    (SETQ LPT2 (LIST (CAR X2) (- PT1Y 82.9)))
;;;    (SETQ LPT4 (LIST (CAR X2) (- PT1Y 94.18)))
;;;    (COMMAND "LINE" LPT2 LPT4 "")
;;;					;ROAD NAM
;;;    (SETQ T1 (- (/ (+ (CAR X2) (CAR X1)) 2) 5))
;;;    (SETQ T2 (- PT1Y 90.54))
;;;    (SETQ T12 (LIST T1 T2))
;;;    (COMMAND "TEXT" "S" "PRO1 " T12 "0" "V.C")
;;;    (command "osnap" "endpoint" "")
;;;  )

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
	   slop	(atof (rtos (* 100 (/ (/ dly 10) dlx)) 2 3))
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
	      (strcat (rtos slop 2 3) "%")
     )
  )
)