(defun tan(x)
(/ (sin x) (cos x))
)
(defun rtd(x)
(* x (/ 180.0 pi))
)
(defun dtr(x)
(* pi (/ x 180.0))
)

	(defun c:rdg()
	(PRINT "\n GIVE THE STANDARD VALUES WHICH YOU WANT AS DEFAULTS.")
	(COMMAND "UNITS" "2" "2" "" "" "" "")
	(setq h_scal  (getreal "\n HORIZONTAL SCALE ?"))
	(setq v_scal  (getreal "\n VERTICAL SCALE ?"))
	(COMMAND "STYLE" "TEXT" "SIMPLEX.SHX" (* 3 H_SCAL) "" "" "" "" ""  )
	(COMMAND "STYLE" "LAB" "COMPLEX.SHX" (* 5 H_SCAL) "" "" "" "" ""  )
	(COMMAND "STYLE" "DIM" "SIMPLEX.SHX" (* 2 H_SCAL) "" "" "" "" ""  )
	(setq gr_num (getint "\n HOW MANY GRAPHS DO YOU WANT IN THIS SESSION ?"))
	(setq BT_WID_L (* 1000 (getreal "\n BLACK TOP HALF WIDTH LEFT (IN METRES)?")))
	(setq BT_WID_R (* 1000 (getreal "\n BLACK TOP HALF WIDTH RIGHT (IN METRES)?")))
	(setq B_WID (* 1000 (getreal "\n BERM WIDTH FROM BLACK TOP EDGE (IN METRES)?")))
	(setq dirn ( getstring "\n PIPELINE TO LEFT OR RIGHT (L/R)"))
	(if (= dirn "L") (setq nn 1) (setq nn 2))
(SETQ PR_DIST (* 1000 ( GETREAL "\n DISTANCE OF PIPE CEN. FROM BLACK TOP EDGE (IN METRES)?")))

(SETQ P0 (GETPOINT "\n WHERE DO YOU WANT THE GRAPH?"))
(SETQ NUM 1)
	(REPEAT GR_NUM 
		(setq chain_l (GETSTRING "\n CROSS SECTION AT CHAIN LENGTH (IN METRES)?"))
		(setq d_val (* 1000 (getreal "\n DATUM (IN METRES)?")))
		(SETQ HT_RC (- (* 1000 (getreal "\n HT. AT ROAD CENTRE (IN METRES)?")) d_val))
		(SETQ HT_P_CL (- (* 1000 (GETREAL "\n HT. AT PIPELINE CENTRE (IN METRES)?")) d_val))
		(SETQ HT_BE_L (- (* 1000 (GETREAL "\n HT. AT BLACK TOP EDGE LEFT (IN METRES)?")) d_val ))
		(SETQ HT_B_L (- (* 1000 (GETREAL "\n HT. AT BERM EDGE LEFT (IN METRES)?")) d_val))
		(SETQ L_NUM (GETINT "\n HOW MANY ORDINATES TO THE LEFT"))
		(SETQ PL_BE (LIST (- (CAR P0) BT_WID_L) (+ (CADR P0) (*  HT_BE_L (/ H_SCAL V_SCAL  )) ) ) )
		
		(SETQ L_N 1)
		(REPEAT L_NUM
		  (SET (READ  (STRCAT "LH_ORD" (ITOA L_N))) (- (car PL_BE) (* 1000 (GETREAL "\n HORIZONTAL DIST."))))
		  (SET (READ  (STRCAT "LV_ORD" (ITOA L_N))) (- (* 1000 (GETREAL "\n HEIGHT.")) D_VAL))
		  (SET 
			(READ (STRCAT "LP_" (ITOA L_N)) )
				(LIST 
				(EVAL (READ  (STRCAT "LH_ORD" (ITOA L_N)))) 
				(+ (* (EVAL (READ  (STRCAT "LV_ORD" (ITOA L_N)))) (/ H_SCAL V_SCAL ) ) (cadr p0))
				) 
		  )
		   (SETQ L_N (+ L_N 1))
		)               ; REPEAT LEFT ORDINATES
		
		(SETQ P_RC (POLAR P0 (DTR 90)  (* HT_RC (/ H_SCAL V_SCAL )) ))

		(SETQ HT_BE_R (- (* 1000 (GETREAL "\n HT. AT BLACK TOP EDGE RIGHT (IN METRES)?")) d_val))
		(SETQ HT_B_R (- (* 1000 (GETREAL "\n HT. AT BERM EDGE RIGHT (IN METRES)?")) d_val))
		(SETQ PR_BE (LIST (+ (CAR P0) BT_WID_R) (+ (CADR P0) (*  HT_BE_R (/ H_SCAL V_SCAL ) ) ) ))
		(SETQ R_NUM (GETINT "\n HOW MANY ORDINATES TO THE RIGHT"))
		(SETQ R_N 1)
		(REPEAT R_NUM
		  (SET (READ  (STRCAT "RH_ORD" (ITOA R_N))) (+ (car PR_BE) (* 1000 (GETREAL "\n HORIZONTAL DIST."))))
		  (SET (READ  (STRCAT "RV_ORD" (ITOA R_N))) (- (* 1000 (GETREAL "\n HEIGHT.")) D_VAL))
		  (SET 
			(READ (STRCAT "RP_" (ITOA R_N))) 
				(LIST 
				(EVAL (READ  (STRCAT "RH_ORD" (ITOA R_N)))) 
				(+ 
				(* 
				(EVAL (READ  (STRCAT "RV_ORD" (ITOA R_N)))) 
				(/ H_SCAL V_SCAL ) 
				) 		;*
				(cadr p0)
				)		;+
		  	)			;READ
		)				;SET
		   (SETQ R_N (+ R_N 1))
		)               ; REPEAT RIGHT ORDINATES
	

	
	
	(SETQ PL_B (LIST (- (CAR P0) (+ BT_WID_L B_WID)) (+ (CADR P0) (*  HT_B_L  (/ H_SCAL V_SCAL )) ) ) )
	(SETQ PR_B (LIST (+ (CAR P0) (+ BT_WID_R B_WID)) (+ (CADR P0) (*  HT_B_R  (/ H_SCAL V_SCAL )) ) ) )
	(if (= dirn "L") (SETQ P_PC (LIST (- (CAR PL_BE) PR_DIST) (+  (CADR P0) (*  HT_P_CL   (/ H_SCAL V_SCAL ))))) 
			(SETQ P_PC (LIST (+ (CAR PL_BE) PR_DIST) (+  (CADR P0) (*  HT_P_CL   (/ H_SCAL V_SCAL )))))
	);if 
	(if (= dirn "L")
	(IF (= L_NUM 0) 
	(command "pline" p_rc "W" (* 0.8 (/ H_SCAL 100)) "" pl_be pl_b p_pc  "" )
		(IF (= L_NUM 1) 
		(command "pline" p_rc "W" (* 0.8 (/ H_SCAL 100)) "" pl_be pl_b p_pc lp_1  "" )
			(IF (= L_NUM 2) 
			(command "pline" p_rc "W" (* 0.8 (/ H_SCAL 100)) "" pl_be pl_b p_pc lp_1 lp_2  "")
				(IF (= L_NUM 3) 
				(command "pline" p_rc "W" (* 0.8 (/ H_SCAL 100)) "" pl_be pl_b p_pc lp_1 lp_2 lp_3  "")
					(IF (= L_NUM 4) 
					(command "pline" p_rc "W" (* 0.8 (/ H_SCAL 100)) "" pl_be pl_b p_pc lp_1 lp_2 lp_3 lp_4  "")
					);
				);if
			);if
		);if
	);if
	
	(IF (= L_NUM 0) 
	(command "pline" p_rc "W" (* 0.8 (/ H_SCAL 100)) "" pl_be pl_b   "" )
		(IF (= L_NUM 1) 
		(command "pline" p_rc "W" (* 0.8 (/ H_SCAL 100)) "" pl_be pl_b  lp_1  "" )
			(IF (= L_NUM 2) 
			(command "pline" p_rc "W" (* 0.8 (/ H_SCAL 100)) "" pl_be pl_b  lp_1 lp_2  "")
				(IF (= L_NUM 3) 
				(command "pline" p_rc "W" (* 0.8 (/ H_SCAL 100)) "" pl_be pl_b  lp_1 lp_2 lp_3  "")
					(IF (= L_NUM 4) 
					(command "pline" p_rc "W" (* 0.8 (/ H_SCAL 100)) "" pl_be pl_b  lp_1 lp_2 lp_3 lp_4  "")
					);
				);if
			);if
		);if
	);if
	);if
	
	( if (= dirn "L")
		(IF (= R_NUM 0) (command "pline" p_rc pr_be pr_b "") 
			(IF (= R_NUM 1) (command "pline" p_rc pr_be pr_b rp_1 "") 
				(IF (= R_NUM 2) (command "pline" p_rc pr_be pr_b  rp_1  rp_2 "")
					(IF (= R_NUM 3) (command "pline" p_rc pr_be pr_b  rp_1  rp_2 rp_3 "") 
						(IF (= R_NUM 4) (command "pline" p_rc pr_be pr_b  rp_1  rp_2 rp_3 rp_4 "")
	);if
		);if 
			);if
				);if
					);if
						
	(IF (= R_NUM 0) (command "pline" p_rc pr_be pr_b p_pc"") 
	(IF (= R_NUM 1) (command "pline" p_rc pr_be pr_b p_pc rp_1 "") 
	(IF (= R_NUM 2) (command "pline" p_rc pr_be pr_b  p_pc rp_1  rp_2 "")
	(IF (= R_NUM 3) (command "pline" p_rc pr_be pr_b  p_pc rp_1  rp_2 rp_3 "") 
	(IF (= R_NUM 4) (command "pline" p_rc pr_be pr_b  p_pc rp_1  rp_2 rp_3 rp_4 "")
	);if
	);if 
	);if
	);if
	);if
	);if   
(COMMAND "COLOR" "2") 
(command "line" 
(polar p0 (dtr 180) 
(+ 
(* 50 h_scal) 
 (- 	(ABS (CAR P0))
	(ABS  (CAR (EVAL (READ  (STRCAT "LP_" (ITOA L_NUM))))))
 
 )	;+
)	;+
)	;POLAR 
(polar p0 (dtr 0) (- (ABS (CAR (EVAL (READ  (STRCAT "RP_" (ITOA R_NUM)))))) (ABS (CAR P0)))
	)               ;polar
"")             ;command
 
(COMMAND "COPY" "L" "" P0 (POLAR P0 (DTR 270) ( * 30 H_SCAL)))
(COMMAND "COPY" "p" "" P0 (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
(COMMAND "COLOR" "3")
(command "line" 
(polar p0 (dtr 180) 
(+ 
(* 50 h_scal) 
 (- (ABS (CAR P0))
	(ABS  (CAR (EVAL (READ  (STRCAT "LP_" (ITOA L_NUM))))))
 
 )	;+
)	;+
)	;POLAR
(POLAR (polar p0 (dtr 180) 
(+ 
(* 50 h_scal) 
 (- (ABS (CAR P0))
	(ABS  (CAR (EVAL (READ  (STRCAT "LP_" (ITOA L_NUM))))))
 
 )	;-
)	;+
)	;POLAR 
(DTR 270) (* 60 H_SCAL))
"")
(COMMAND "LINE" P_RC (LIST 
			(CAR P_RC) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" )
(COMMAND "TEXT" (LIST (- (CAR P_RC) (* H_SCAL 2)) (+ (CADR P0) (* 50 H_SCAL)) ) "90" "RC" "")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR P_RC) (* H_SCAL 2)) (- (CADR P0) (* 5 H_SCAL)) )  "90" (RTOS (/ (+ HT_RC D_VAL) 1000) 2 2) "")
(COMMAND "LINE" PL_BE (LIST 
			(CAR PL_BE) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))

		      ) "" )
(COMMAND "COLOR" "14")
(COMMAND "TEXT" (LIST (- (CAR PL_BE) (* H_SCAL 2)) (+ (CADR P0) (* 50 H_SCAL)) ) "90" "BE" "")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR PL_BE) (* H_SCAL 2)) (- (CADR P0) (* 5 H_SCAL)) )  "90" ( RTOS (/ (+ HT_BE_L D_VAL) 1000) 2 2) "")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR PL_BE) (* H_SCAL 2)) (- (CADR P0) (* 35 H_SCAL)) )  "90" "0.0" "")
(COMMAND "COLOR" "3")
(COMMAND "LINE" PR_BE (LIST 
			(CAR PR_BE) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" )
(COMMAND "COLOR" "14")
(COMMAND "TEXT" (LIST (- (CAR PR_BE) (* H_SCAL 2)) (+ (CADR P0) (* 50 H_SCAL)) ) "90" "BE" "")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR PR_BE) (* H_SCAL 2)) (- (CADR P0) (* 5 H_SCAL)) )  "90" (RTOS (/ (+ HT_BE_R D_VAL) 1000) 2 2) "")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR PR_BE) (* H_SCAL 2)) (- (CADR P0) (* 35 H_SCAL)) )  "90" "0.0" "")
(COMMAND "COLOR" "3")
(COMMAND "LINE" PL_B (LIST 
			(CAR PL_B) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" )
(COMMAND "COLOR" "14")
(COMMAND "TEXT" (LIST (- (CAR PL_B) (* H_SCAL 2)) (+ (CADR P0) (* 50 H_SCAL)) ) "90" "B" "")	
(COMMAND "TEXT" "J" "R" (LIST (- (CAR PL_B) (* H_SCAL 2)) (- (CADR P0) (* 5 H_SCAL)) )  "90" (RTOS (/ (+ HT_B_L D_VAL) 1000) 2 2) "")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR PL_B) (* H_SCAL 2)) (- (CADR P0) (* 35 H_SCAL)) )  "90"  (RTOS (abs (/ (- (car pl_b) (car pl_be)) 1000)) 2 2) "")
(COMMAND "COLOR" "3")
(COMMAND "LINE" PR_B (LIST 
			(CAR PR_B) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" )
(COMMAND "COLOR" "14")
(COMMAND "TEXT" (LIST (- (CAR PR_B) (* H_SCAL 2)) (+ (CADR P0) (* 50 H_SCAL)) ) "90" "B" "")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR PR_B) (* H_SCAL 2)) (- (CADR P0) (* 5 H_SCAL)) )  "90" (RTOS (/ (+ HT_B_R D_VAL) 1000) 2 2) "")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR PR_B) (* H_SCAL 2)) (- (CADR P0) (* 35 H_SCAL)) )  "90"  (RTOS (abs (/ (- (car pr_b) (car pr_be)) 1000)) 2 2) "")
(COMMAND "COLOR" "3")
(COMMAND "LINE" P_PC (LIST 
			(CAR P_PC) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" )
(COMMAND "COLOR" "14")
(COMMAND "TEXT" (LIST (- (CAR P_PC) (* H_SCAL 2)) (+ (CADR P0) (* 50 H_SCAL)) ) "90" "C.L  PR" "")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR P_PC) (* H_SCAL 2)) (- (CADR P0) (* 5 H_SCAL)) )  "90" (RTOS (/ (+ HT_P_CL D_VAL) 1000) 2 2) "")
(if (= dirn "L")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR P_PC) (* H_SCAL 2)) (- (CADR P0) (* 35 H_SCAL)) )  "90"  (RTOS (abs (/ (- (car P_PC) (car pl_be)) 1000)) 2 2) "")
(COMMAND "TEXT" "J" "R" (LIST (- (CAR P_PC) (* H_SCAL 2)) (- (CADR P0) (* 35 H_SCAL)) )  "90"  (RTOS (abs (/ (+ (car P_PC) (car pr_be)) 1000)) 2 2) "")
)
(COMMAND "COLOR" "4")
(if (= l_num 1)
(COMMAND "LINE" lp_1 (LIST 
			(CAR lp_1) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" ))
(if (= l_num 2)

(COMMAND "LINE" lp_2 (LIST 
			(CAR lp_2) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" lp_1 (LIST 
			(CAR lp_1) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) ""))
(if (= l_num 3)

(COMMAND "LINE" lp_3 (LIST 
			(CAR lp_3) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" lp_1 (LIST 
			(CAR lp_1) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" lp_2 (LIST 
			(CAR lp_2) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) ""))
(if (= l_num 4)

(COMMAND "LINE" lp_4 (LIST 
			(CAR lp_4) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" lp_1 (LIST 
			(CAR lp_1) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" lp_2 (LIST 
			(CAR lp_2) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" lp_3 (LIST 
			(CAR lp_3) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) ""))
;,,,,,,,,,,,,,,,,,,,,,,
(COMMAND "COLOR" "4")
(if (= r_num 1)
(COMMAND "LINE" rp_1 (LIST 
			(CAR rp_1) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" ))
(if (= r_num 2)

(COMMAND "LINE" rp_2 (LIST 
			(CAR rp_2) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" rp_1 (LIST 
			(CAR rp_1) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) ""))
(if (= r_num 3)

(COMMAND "LINE" rp_3 (LIST 
			(CAR rp_3) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" rp_1 (LIST 
			(CAR rp_1) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" rp_2 (LIST 
			(CAR rp_2) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) ""))
(if (= r_num 4)

(COMMAND "LINE" rp_4 (LIST 
			(CAR rp_4) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" rp_1 (LIST 
			(CAR rp_1) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" lp_2 (LIST 
			(CAR rp_2) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) "" "LINE" rp_3 (LIST 
			(CAR rp_3) 
			(CADR (POLAR P0 (DTR 270) ( * 60 H_SCAL)))
		      ) ""))
	(COMMAND)
	(COMMAND "UNITS" "2" "2" "" "" "" "")
(COMMAND "COLOR" "15")
(command "dim" "dimlfac" "0.001" "dimasz" (* h_scal 2) "hor" (list (car pr_be) (cadr p0)) (list (car p_rc) (cadr p0)) (polar (list (car p_rc) (cadr p0)) (dtr 90) (* h_scal 5)) "")
(command "dim"  "hor" (list (car pl_be) (cadr p0)) (list (car p_rc) (cadr p0)) (polar (list (car p_rc) (cadr p0)) (dtr 90) (* h_scal 5)) "" "dimlfac" "1" "exit" )	
(COMMAND "COLOR" "17")
(command "text" "style" "lab" 	"C" (list (car p_rc) (- (cadr p0) (* h_scal 85))) 0 (STRCAT "%%uCROSS SECTION AT CHAIN LENGTH " chain_l " m") "" )
(COMMAND "COLOR" "35")
(command "text" "style" "text" "J" "R"
 	(polar (polar p0 (dtr 180) 
		(+ 
			(* 50 h_scal) 
 				(- (ABS (CAR P0))
				(ABS  (CAR (EVAL (READ  (STRCAT "LP_" (ITOA L_NUM))))))
 				
 				)	;-
		)	;+
		)	;POLAR 
	(dtr 90) (* 1.5 h_scal)) 
0 (strcat "DATUM LEVEL: " (RTOS (abs (/ D_val 1000)) 2 2) " m")"" ) 
(COMMAND "COLOR" "14")
(SETQ L_N 1)
(REPEAT L_NUM

(COMMAND "TEXT" "STYLE" "DIM"  	
	(LIST 
	(- (CAR (EVAL (READ  (STRCAT "LP_" (ITOA L_N))))) (* 2 H_SCAL))
	(- (CADR P0) (* 12 H_SCAL) ))
	 90
	(RTOS (/ (+ (EVAL (READ  (STRCAT "LV_ORD" (ITOA L_N)))) D_VAL) 1000) 2 2)
		"")	; COMMAND
(SETQ L_N (+ L_N 1))
)	; REPEAT
(SETQ L_N 1)
(REPEAT L_NUM
(COMMAND "TEXT"   "j" "r"	
	(LIST 
	(- (CAR (EVAL (READ  (STRCAT "LP_" (ITOA L_N))))) (* 2 H_SCAL))
	(- (CADR P0) (* 35 H_SCAL) )
	)
	 90
	(RTOS (abs (- (abs (/ (CAR PL_BE) 1000)) (abs (/ (EVAL (READ  (STRCAT "LH_ORD" (ITOA L_N)))) 1000)))) 2 2)
	
		"")	; COMMAND
(SETQ L_N (+ L_N 1))
)	; REPEAT



(setq r_n 1)
(REPEAT r_NUM
(COMMAND "TEXT" "STYLE" "DIM"  	
	(LIST 
	(- (CAR (EVAL (READ  (STRCAT "rP_" (ITOA r_N))))) (* 2 H_SCAL))
	(- (CADR P0) (* 12 H_SCAL) ))
	 90
	(RTOS (/ (+ (EVAL (READ  (STRCAT "RV_ORD" (ITOA R_N)))) D_VAL) 1000) 2 2)
		"")	; COMMAND
(SETQ r_N (+ r_N 1))
)	; REPEAT
(SETQ R_N 1)
(REPEAT R_NUM
(COMMAND "TEXT"   "j" "r"	
	(LIST 
	(- (CAR (EVAL (READ  (STRCAT "RP_" (ITOA R_N))))) (* 2 H_SCAL))
	(- (CADR P0) (* 35 H_SCAL) )
	)
	 90
	(RTOS (abs (- (abs (/ (CAR PR_BE) 1000)) (abs (/ (EVAL (READ  (STRCAT "RH_ORD" (ITOA R_N)))) 1000)))) 2 2)
	
		"")	; COMMAND
(SETQ R_N (+ R_N 1))
)	; REPEAT
(COMMAND "COLOR" "7")
)                       ; REPEAT GRAPH
)                               ; defun rdg
