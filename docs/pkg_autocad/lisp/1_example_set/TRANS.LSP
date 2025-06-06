(defun C:TRA()
(defun *ERROR* (MSG)
	(princ "error: ")
	(princ msg)
	(terpri)
)
	(command "LAYER" "THAW" "0" "")
	(command "LAYER" "SET" "0" "")
	(setq sca (getint "MAP SCALE PLEASE:"))
	(command "shell" "cls")
	(prompt "           *** TRANSFORMATION OF MAP COORDINATES ***")
	(TERPRI)
	(setvar "cmdecho" 0)
	(setq fnam (getvar "dwgname"))
	(prompt "\n   TRANSFORMATION OF MAP COORDINATES")
	(princ "\n=====================================================================")
	(prompt "\nTRANSFORMING COORDINATES FOR SHEET:")
	(prompt fnam)
	(textscr)
	(setq rext ".rtc"
		mext ".mtc"
		mexc ".mcr"
		rexc ".rcr"
		rmsf ".rms"
		rfnam (strcat fnam rext)
		mfnam (strcat fnam mext)
		mcfnam (strcat fnam mexc)
		rcfnam (strcat fnam rexc)
		rmsfil (strcat fnam rmsf)
		i (open rfnam "r")
		j (open mfnam "r")
	)
	(setq g 0.0 h 0.0 p 0.0 q 0.0 r 0.0 s 0.0 t 0.0 u 0.0)
	(setq ss 0.0 pp 0.0 uu 0.0 rmso 0.0 cnt 0 cont 0)
(if i
	(progn
		(while (setq z (read-line i))
			(setq z (read z)
				x0 (car z)
				y0 (cadr z)
				m (read-line j)
				m (read m)
				mx0 (car m)
				my0 (cadr m)
				cont (1+ cont)
				g (+ g mx0)
				h (+ h my0)
				p (+ p (* mx0 my0))
				q (+ q (* mx0 mx0))
				r (+ r (* mx0 X0))
				s (+ s (* my0 my0))
				t (+ t (* my0 x0))
				u (+ u x0)
				ss (+ ss (* y0 mx0))
				pp (+ pp (* y0 my0))
				uu (+ uu y0)
			)
		)
	)
 )
		(setq nt1 (- h (/ (* p g) q))
			nt2 (- s (/ (* p p) q))
			nt3 (- cont (/ (* g g) q))
			nt4 (- (/ (* p r) q) t)
			nt5 (- (/ (* g r) q) u)
			r1 (/ nt5 nt1)
			r2 (/ nt4 nt2)
			r3 (/ nt1 nt2)
			r4 (/ nt3 nt1)
			c (/ (- r1 r2) (- r3 r4))
			r5 (* c nt1)
			b (/ (+ nt4 r5) (* nt2 -1))
			r6 (+ (* p b) (* g c))
			a (/ (- r r6) q)
			snt4 (- (/ (* p ss) q) pp)
			snt5 (- (/ (* g ss) q) uu)
			r7 (/ snt5 nt1)
			r8 (/ snt4 nt2)
			f (/ (- r7 r8) (- r3 r4))
			r9 (* f nt1)
			e (/ (+ snt4 r9) (* nt2 -1))
			r10 (+ (* p e) (* g f))
			d (/ (- ss r10) q)
		)
(close i)
(close j)
		(setq i (open rfnam "r")
			fil (open rmsfil "w")
		)
		(princ "\nTIC ID       Input  X       Input  Y   " fil)
		(princ "\n" fil)
		(PRINC "             Output X       Output Y         X error      Y error  " fil)
		(princ "\n" fil)
		(princ "====================================================================" fil)
		(princ "\n" fil)
;===============================
;RMS ERROR REPORT ON SCREEN
;==============================
		(princ "\nTIC ID       Input  X         Input  Y  ")
		(PRINC "\n             Output X         Output Y          X error    Y error  ")
		(princ "\n=====================================================================")
		(princ "\n")
		(setq j (open mfnam "r"))
(if i
	(progn
		(while (setq z (read-line i))
			(setq z (read z)
				cnt (1+ cnt)
				x0 (car z)
				y0 (cadr z)
				m (read-line j)
				m (read m)
				mx0 (car m)
				my0 (cadr m)
				r11 (- c x0)
				dx (+ (* a mx0) (* b my0) r11)
				r12 (- f y0)
				dy (+ (* d mx0) (* e my0) r12)
				rmso (+ rmso (+ (* dx dx) (* dy dy)))
			)
			(princ "    " fil)
			(princ cnt fil)
			(princ "         " fil)
			(setq fx0 (rtos x0 2 2)
				fy0 (rtos y0 2 2)
				fmx0 (rtos mx0 2 2)
				fmy0 (rtos my0 2 2)
				fdx (rtos dx 2 2)
				fdy (rtos dy 2 2)
			)
			(princ fmx0 fil) 
			(princ "         " fil)
			(princ fmy0 fil)
			(princ "\n" fil)
			(princ "              " fil)
			(princ fx0 fil) 
			(princ "         " fil)
			(princ fy0 fil)
			(princ "             " fil)
			(princ fdx fil)
			(princ "        " fil)
			(princ fdy fil)
			(princ "\n" fil)
			(princ "    ")
			(princ cnt)
			(princ "        ")
			(princ fmx0) 
			(princ "        ")
			(princ fmy0)
			(princ "\n")
			(princ "             ")
			(princ fx0) 
			(princ "        ")
			(princ fy0)
			(princ "          ")
			(princ fdx)
			(princ "        ")
			(princ fdy)
			(princ "\n")
		)
	)
 )
		(setq rmso (/ rmso cnt)
			rmso (sqrt rmso)
			rmsi (/ (+ a e) 2)
			rmsi (/ rmso rmsi)
			rmsi (rtos rmsi 2 3)
		)
		(princ "   RMS Error(input,output) = (" fil)
		(princ rmsi fil)
		(princ "," fil)
		(princ rmso fil)
		(princ ")" fil)
		(princ "\n" fil)
		(princ "   Scale (X,Y) = (" FIL)
		(princ a fil)
		(princ "," fil)
		(princ e fil)
		(princ ")" fil)
		(princ "  Translation= (" fil)
		(princ c fil)
		(princ "," fil)
		(princ f fil)
		(princ ")" fil)
		(princ "\n   RMS Error (input,output)= (")
		(princ rmsi)
		(princ ",")
		(princ rmso)
		(princ ")")
		(princ "\n")
		(princ "   Scale (X,Y) = (")
		(princ a)
		(princ ",")
		(princ e)
		(princ ")")
		(princ "  Translation= (")
		(princ c)
		(princ ",")
		(princ f)
		(princ ")")
		(princ "\n====================================================================")
		(princ "\n")
(close i)
(close j)
(close fil)
		(setq tole (/ (* sca 2.0) 10000))
(if (> rmso tole)
	(progn
		(prompt "\nTRANSFORMATION HAS BEEN ABORTED BY PROGRAM...")
		(prompt "\nPLEASE CHECK MAP TIC FILE...!")
	)
)
(if (< rmso tole)
	(progn
		(initget 1 "YES NO")
		(setq ans (getkword "DO YOU WISH TO CONTINUE (Yes/No):"))
(IF (= ans "YES")
	(progn
	(graphscr)
(princ "                              Working ... Please Wait!")
		(setq mf (open mcfnam "r")
			rf (open rcfnam "w")
			bmixr 1000000.0
			bmiyr 1000000.0
		)
(if mf
	(progn
		(while (setq cor (read-line mf))
			(setq cor (read cor)
				xm (car cor)
				ym (cadr cor)
				r20 (* a xm)
				r21 (* b ym)
				r22 (* d xm)
				r23 (* e ym)
				xr (+ r20 r21 c)
				yr (+ r22 r23 f)
				mixr (min bmixr xr)
				miyr (min bmiyr yr)
				bmixr mixr
				bmiyr miyr
				xr (rtos xr 2 3)	
				yr (rtos yr 2 3)
			)
			(princ xr rf)
			(princ "," rf)
			(princ yr rf)
			(princ "\n" rf)		
		)
	)
)
(close mf)
(close rf)
;===================================
		(setq pt1 '(0.0 0.0)
			pt2 '(1000000.0 1000000.0)
		)
		(command "save" fnam "y"
			"erase" "w" pt1 pt2 ""
		)
		(setvar "pdmode" 32)
		(setvar "pdsize" 5)
		(command "color" 4)
		(command "ucsicon" "off")
		(setq xsz (/ (* 10.0 sca) 100.0)
			fmixr (- mixr xsz)
			fmiyr (- miyr xsz)
			pt3 (mapcar '1+ (list fmixr fmiyr))
			xsiz (/ (* 60.0 sca) 100.0)
			xpt1 (+ xsiz mixr)
			ypt1 (+ xsiz miyr)
			pt4 (mapcar '1+ (list xpt1 ypt1))
		)
		(command "limits" pt3 pt4
			"zoom" "a"
		)
		(setq i (open rcfnam "r")
			pntp (read-line i)
		)
(close i)
		(setq i (open rcfnam "r"))
(if i
	(progn
		(while (setq pnt (read-line i))
			(command "point" pnt
				"line" pntp pnt
				""
			)
			(setq pntp pnt)
		)
	)
)
(close i)
	)
)
(if (= ANS "NO")
	(progn
		(prompt "\nTRANSFORMATION HAS BEEN ABORTED BY USER...?")
	)
)
	)
)
)
