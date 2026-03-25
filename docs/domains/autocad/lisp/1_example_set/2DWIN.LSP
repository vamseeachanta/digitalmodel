;THIS IS A PROGRAM TO DRAW A WINDOW.
;WRITTEN BY TOM ROTHSTEIN
;
(Defun C:opg ()
(SETQ Hor (Getdist "\nHorizontal Distance <56>: "))
	(IF (= Hor nil) (setq hor 56))
(SETQ Ver (Getdist "\nVertical Distance <48>: "))
	(IF (= Ver nil) (setq ver 48))
(SETQ Tri (Getdist "\nTrim Width <2>: "))
	(IF (= Tri nil) (setq tri 2))
(SETQ SP (Getpoint "\nStart Point: "))
(SETQ CL (GETVAR "CLAYER"))
(SETQ HOR (+ HOR (* 2.0 TRI)))
(SETQ VER (+ VER (* 2.0 TRI)))
(SETQ P2 (Polar SP (dtr 0) hor))
(SETQ P3 (Polar P2 (dtr 90) ver))
(SETQ P4 (Polar SP (dtr 90) ver))
  (COMMAND "LAYER" "M" "WINDOW" "C" "YELLOW" "" "")
  (COMMAND "Line" SP P2 P3 P4 "c")
(SETQ J (Polar SP (dtr 90) Tri))
(SETQ P5 (Polar J (dtr 0) Tri))
(setq iver (- ver (* 2 tri)))
(SETQ P6 (Polar P5 (dtr 90) iver))
(setq ihor (- hor (* 2 Tri)))
(SETQ P7 (Polar P6 (dtr 0) ihor))
(SETQ P8 (Polar P7 (dtr 270) iver))
  (COMMAND "LINE" P5 P6 P7 P8 "c")
(SETQ MID (* (- HOR (* TRI 2)) 0.5))
(SETQ P9 (POLAR P5 (DTR 0) MID))
(SETQ P10 (POLAR P9 (DTR 180) 0.5))
(SETQ MID2 (* (- VER (* TRI 2)) 0.5))
(SETQ P11 (POLAR P5 (DTR 90) MID2))
(SETQ P12 (POLAR P11 (DTR 270) 0.5))
(SETQ HOTP (- MID 0.5))
(SETQ P13 (POLAR P12 (DTR 0) HOTP))
  (COMMAND "LINE" P10 P13 P12 "")
(SETQ P14 (POLAR P11 (DTR 90) 0.5))
(SETQ P15 (POLAR P14 (DTR 0) HOTP))
(SETQ VOTP (- (/ IVER 2.0) 0.5))
(SETQ P16 (POLAR P15 (DTR 90) VOTP))
  (COMMAND "LINE" P14 P15 P16 "")
(SETQ P17 (POLAR P9 (DTR 0) 0.5))
(SETQ P18 (POLAR P17 (DTR 90) VOTP))
(SETQ P19 (POLAR P18 (DTR 0) HOTP))
  (COMMAND "LINE" P17 P18 P19 "")
(SETQ P20 (POLAR P19 (DTR 90) 1.0))
(SETQ P21 (POLAR P20 (DTR 180) HOTP))
(SETQ P22 (POLAR P21 (DTR 90) VOTP))
  (COMMAND "LINE" P20 P21 P22 "")
(SETVAR "CLAYER" CL) 
  )
(Defun DTR (a) (* Pi (/ a 180.0)))
(Princ "\nType OPG For An Opening.")
(Princ)









