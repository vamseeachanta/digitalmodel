(defun c:poicor (/
			PT	HT	A	N	LAY
			HL	VL	
			STEP1	STEP2	step3
			x0	X1	X2	X3
			x3	x4	x5
			x4	x5	x6
			x7	x8	x9	x10
			Y	Y1	YR	Yend
			P1	P2	P3
			P4	P5	P6
			P7	P8	P9	P10
			PR1	PR2	PR3
			I	F	ST
			XI 	YI	PI
			TXT	TXT1	TXT2	TXT3	POINTTXT 
		)

(command "CMDecho" "0")
(command "osnap" "off")
(setq LAY (getstring T "\nEnter Layer Name:"))	; Layer name for Cordinates
(SETQ HT (getreal "\nEnter Text Height:"))	; TEXT HEIGHT
(SETQ ST (GETINT "\nEnter the first No.:"))
(setq PT (getpoint "\nEnter Top Left Corner:")) ; Table Start point
(setq A (ssget)) ; select POINTS
(setq N (sslength A)) ; No of POINTS
(setq HL (* 33 HT)) ; Horzontal Line of Table
(setq VL (* (+ 1 N) (* 2.50 HT))) ; Vertical Line of Table
(setq step1 (* 5 HT))
(setq step2 (* 14 HT))
(setq step3 (* 2.50 HT))
(command "layer" "m" LAY "")
; To Draw The Table
	; To Draw The Horzontal Lines of Table
	
	(command "line" PT (polar PT 0 HL)  "")
	(command "array" PT "" "R" (+ 2 N) "1" (- 0 step3 ))

; To Write The Header Of Table AND Draw The Vertical Lines of Table
;------------------------------------------------------------------
(setq x0 (car PT))			
(setq x1 (+ (car PT) (* 2.5 HT)))		; X Cordinate of 1st Text Point
(setq x2 (+ (car PT) (* 12 HT)))		; X Cordinate of 2nd Text Point
(setq x3 (+ (car PT) (* 26 HT)))		; X Cordinate of 3rd Text Point
(setq x4 (+ (car PT) (* 5 HT)))			; X Cordinate of 1st Line Point
(setq x5 (+ (car PT) (* 19 HT)))		; X Cordinate of 2nd Line Point
(setq x6 (+ (car PT) (* 33 HT)))		; X Cordinate of 3rd Line Point
(setq y  (cadr PT))				; y Cordinate of Table Start
(setq y1 (- y (* 1.25 HT)))			; y Cordinate of 1st Row Text
(setq Yend (- y (* (+ 1 N) (* 2.50 HT))))	; y Cordinate of last Hor. Line
(setq P1 (list x1 y1))
(setq P2 (list x2 y1))
(setq P3 (list x3 y1))
(setq P4 (list x4 y))
(setq P5 (list x5 y))
(setq P6 (list x6 y))
(setq P7 (list x4 Yend))
(setq P8 (list x5 Yend))
(setq P9 (list x6 Yend))
(setq P10 (list x0 Yend))
	(command "text" "m" P1 Ht "" "No." "" )
	(command "text" "m" P2 Ht "" "X Cord." "" )
	(command "text" "m" P3 Ht "" "Y Cord." "" )
	(command "line" PT P10 "")
	(command "line" P4 P7 "")
	(command "line" P5 P8 "")
	(command "line" P6 P9 "")

; Loop to get Points
(setq I 0)
(setq f (open "c:/points.txt" "a")) ; Open Text File for points Data
;(write-line "CORDINATES OF Points at sec " f)
(while (< I N )
(setq y (- y (* 2.50 Ht)))		; Decrease y Cordinate of Table Start by Step
(setq yR (- y (* 1.25 Ht)))		; y Cordinate of Each Row Text
(setq PR1 (list x1 yR))
(setq PR2 (list x2 yR))
(setq PR3 (list x3 yR))
(setq XI (car (cdr (assoc 10 (entget (ssname A I ))))))	; Get X cordinate for Each Circle
(setq YI (cadr (cdr (assoc 10 (entget (ssname A I )))))); Get Y cordinate for Each Circle
(setq PI (list XI YI))					
(setq txt (fix (+ I ST)))			; Convert No. of circle from Real to Int.
(setq txt1 (itoa txt))				; Convert No. of circle from Int. to Text
(setq txt2 (rtos XI))				; Convert X Cor. of circle from Real to Text
(setq txt3 (rtos YI))				; Convert Y Cor. of circle from Real to Text
	(command "text" "m" PI Ht "" txt1 "" )	; Write The No. of Circle inside the Circle
	(command "text" "m" PR1 Ht "" txt1 "" )
	(command "text" "m" PR2 Ht "" txt2 "" )
	(command "text" "m" PR3 Ht "" txt3 "" )
(setq Pointtxt (strcat txt1 "\t" txt2 "\t" txt3))	; Format of Text in the Ascii file
(write-line Pointtxt f)
(setq I (+ I 1))
)
(close f)
)