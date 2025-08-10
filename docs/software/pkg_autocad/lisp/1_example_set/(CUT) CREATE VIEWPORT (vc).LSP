;Tip1537A:  vc.LSP   Create Viewports   (c)1999, Amy Ang

;(princ "\nLoading c:CUT...")
(defun C:vc (/	CMD ENAM MP P1 P2 P3 P4	PS_X3 PS_Y3 SF VP_SCALE	X1 XP1
	      Y1)
;=====================================================
  (defun *ERROR* (MSG)
    (command "UNDO" "End")
    (command "UNDO" "A" "ON")
    (setvar "cmdecho" CMD)
    (if	MSG
      (princ MSG)
    )
    (setq *ERROR* OLDERR)
    (princ)
  )
;=====================================================
  (setq CMD (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "UNDO" "A" "OFF")
  (command "UNDO" "G")
  (U_SCL)
  (if (= (getvar "tilemode") 0)
    (command "tilemode" "1")
  )
  (command "zoom" "e")
  (command "zoom" "0.85x")
  (setq P1 (getpoint "\nFirst Point: "))
  (initget 7)
  (setq P3 (getcorner P1 "\nOther corner Point: "))
  (setq	MP (progn (if (not CAL)
		    (arxload "geomcal")
		  )
		  (CAL "(p1+p3)/2.0")
	   )
  )
  (setq P2 (list (car P3) (cadr P1)))
  (setq P4 (list (car P1) (cadr P3)))
  (command "tilemode" "0")
  (setq X1 (distance P1 P2))
  (setq Y1 (distance P2 P3))
  (setq PS_X3 (/ X1 SF))
  (setq PS_Y3 (/ Y1 SF))
  (if (/= (getvar "cvport") 1)
    (command "_.PSPACE")
  )
  (setq XP1 (getpoint "\nInsertion point for Viewport: "))
  (command "mview"
	   XP1
	   (strcat "@" (rtos PS_X3) "," (rtos PS_Y3))
  )
  (setq ENAM (entlast))
  (command ".mspace")
  (setvar "cvport" (cdr (assoc 69 (entget ENAM))))
  (setq VP_SCALE (abs (- (cadr P3) (cadr P1))))
  (command "zoom" "c" MP VP_SCALE)
  (command "_.PSPACE")
  (setvar "cmdecho" CMD)
  (*ERROR* MSG)
  (princ)
)
(setq ARCH (list " 1/8\"=1'"	  " 1/32\"=1'"	   " 1/16\"=1'"
		 " 3/32\"=1'"	  " 3/16\"=1'"	   " 1/4\"=1'"
		 " 3/8\"=1'"	  " 1/2\"=1'"	   " 3/4\"=1'"
		 " 1\"=1'"	  " 1 1/2\"=1'"	   " 3\"=1'"
		 " 1\"=1\""
		)
)
;(princ ".")
(setq ARCH_SCALE (list 96 384 192 128 64 48 32 24 16 12 8 4 1))
(setq ENG (list	"1\"=500'"  "1\"=200'"	"1\"=100'"  "1\"=60'"
		"1\"=50'"   "1\"=40'"	"1\"=30'"   "1\"=20'"
		"1\"=10'"   "1\"=5'"	"1\"=1'"    "1\"=1\""
		"1=500"	    "1=200"	"1=100"	    "1=60"
		"1=50"	    "1=40"	"1=30"	    "1=20"
		"1=10"	    "1=5"	"1=2"	    "1=1"
		"2=1"
	       )
)

(setq ENG_SCALE	(list 6000 2400	1200 720  600  480  360	 240  120  60
		      12   1	500  200  100  60   50	 40   30   20
		      10   5	2    1	  0.5
		     )
)
;(princ ".")
(defun U_SCL (/ DAT DISP PICK)
  (setq DAT (load_dialog "cut"))
  (if (not (new_dialog "cut" DAT))
    (exit)
  )
  (mode_tile "scale" 1)
  (action_tile "arch" "(toggle $key)")
  (action_tile "eng" "(toggle $key)")
  (action_tile
    "scale"
    "(setq pick (atoi $value))(show (atoi 
             $value))"
  )
  (action_tile "accept" "(done_dialog)")
  (action_tile "cancel" "(done_dialog)(exit)")
  (start_dialog)
  (unload_dialog DAT)
  (if (null PICK)
    (setq PICK 0)
  )
  (setq SF (nth PICK (eval (read (strcat DISP "_scale")))))
  SF
)
(defun SHOW (ARG)
  (if (null PICK)
    (setq PICK 0)
  )
  (setq SF (nth PICK (eval (read (strcat DISP "_scale")))))
  (set_tile "factor" (strcat "Factor of:  " (rtos SF 2 1)))
  SF
)

(defun TOGGLE (ARG)
  (mode_tile "scale" 0)
  (setq DISP ARG)
  (start_list "scale")
  (mapcar 'add_list (eval (read ARG)))
  (end_list)
  (set_tile "factor" "")
  (if (= DISP "arch")
    (set_tile "factor" (strcat "Factor of: " "96"))
    (set_tile "factor" (strcat "Factor of:  " "6000"))
  )
)
;(princ ".")
(princ)

