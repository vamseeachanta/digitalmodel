(defun c:mucs (/ o bp dp)
  (command "undo" "begin")
  (setq o (ssget))
  (setq *os (getvar "osmode")
	*om (getvar "ORTHOMODE")
	)
  (setvar "osmode" 2)
  (setvar "ORTHOMODE" 0)
  (setq bp (getpoint "Base point or displacement:"))
  (setvar "osmode" 32)
  (setq dp (getpoint bp "Second point of displacement:"))
  (command "move" o "" bp dp)
  (setvar "ORTHOMODE" *om)
  (setvar "osmode" *os)
  (c:uc)
  (command "undo" "end")
  (princ)
  )
(defun c:uc (/ pt el com str1 cnt str)
   (SETVAR "CMDECHO" 0)
  (princ "\nUcs Control")
  (setq pt (getpoint "\nPick Ref. Point")
	en1 (car (entsel "\nPick Elevation"))
	com "0,-"
	)
(if (/= en1 nil)
  (if (OR (equal (cdr (assoc 0 (entget en1))) "TEXT") (equal (cdr (assoc 0 (entget en1))) "MTEXT"))
     (setq el (cdr (assoc 1 (entget en1))))
    (progn
  (setq el1 (cdr (assoc 1 (entget (entnext en1))))
	cnt 0
	str1 "")
  (REPEAT (STRLEN EL1)
  (setq cnt (1+ cnt))
  (setq str (ascii (substr el1 cnt 1)))
  (if (and (< str 58)(> str 47))
    (progn
    (setq str1 (strcat str1 (chr str)))
    (if (= (strlen str1) 3)
    (setq str1 (strcat str1 "."))
    )
    )
    );if
    (IF (> (strlen str1) 4)
      (setq EL str1)
      );if
    );repeat
    );progn
    );if
(setq el (rtos (getreal "Enter Elevation From Keyboard")));otherwise
  );if
  (command "ucs" "o" pt)
  (command "ucs" "o" (strcat com el))
  (princ)
  (princ "\nElevation captured: ")
  (princ el)
  (princ)
  )
(defun c:AA (/ x y of el lp pt pt2 pt3 osmd)
  (setvar "cmdecho" 0)
  (command "undo" "begin")
  (setq *os (getvar "osmode")
	*om (getvar "ORTHOMODE")
	)
  (setvar "osmode" 1)
  (setvar "orthomode" 0)
  (setq pt (getpoint "\nPick id Point")
	x (car pt)
	y (cadr pt)
	)
  (setvar "osmode" 0)
(setq pt2 (getpoint pt "\nTo point:"))
  (command "line" pt  pt2 "")
    (setq LST (ENTGET (ENTLAST))
	OLDLA (ASSOC 8 LST)
	LA (CONS 8 "QUOTE"))
  (ENTMOD (SUBST LA OLDLA LST))
  (setvar "osmode" 128)
  (setq pt3 (getpoint pt2 "\nTo point:"))
(if (/= pt3 nil)
  (progn
   (setvar "osmode" 0)
   (command "line" pt2 pt3 "")
     (setq LST (ENTGET (ENTLAST))
	OLDLA (ASSOC 8 LST)
	LA (CONS 8 "QUOTE"))
  (ENTMOD (SUBST LA OLDLA LST))
   (setq of (polar pt3 (* (/ pi 180) 94) 1.3)
         EL (polar pt3 (* (/ pi 180) 91) 6.01)
      )
   )
(progn
  (setvar "osmode" 0)
(setq of (polar pt2 (* (/ pi 180) 94) 1.3)
      EL (polar pt2 (* (/ pi 180) 91) 6.01)
      )
  )
  )
  (command "text" of 0.5 90 (rtos (abs X) 2 3))
(command "text" el 0.5 90 (rtos (abs Y) 2 3))
  (setvar "osmode" *os)
  (setvar "osmode" *om)
  (command "undo" "end")
(princ)
    )
(defun c:A (/ x y of el lp pt pt2 pt3 osmd LST OLDLA LA)
  (setvar "cmdecho" 0)
  (command "undo" "begin")
  (setq *os (getvar "osmode"))
  (setvar "osmode" 1)
  (setq pt (getpoint "\nPick id Point")
	x (car pt)
	y (cadr pt)
	)
  (setvar "osmode" 128)
(setq pt2 (getpoint pt "\nTo point:"))
 (setvar "osmode" 0) 
  (command "line" pt pt2 "")
  (setq LST (ENTGET (ENTLAST))
	OLDLA (ASSOC 8 LST)
	LA (CONS 8 "QUOTE"))
  (ENTMOD (SUBST LA OLDLA LST))
  (setq of (polar pt2 (* (/ pi 180) 94) 1.3)
      EL (polar pt2 (* (/ pi 180) 91) 6.01)
      )
  (command "text" of 0.5 90 (rtos (abs X) 2 3))
(command "text" el 0.5 90 (rtos (abs Y) 2 3))
(setvar "osmode" *os)
(command "undo" "end")
(princ)
 )
(defun c:XV (/ x y of el lp pt pt2 pt3 osmd)
  (setvar "cmdecho" 0)
  (setq *os (getvar "osmode"))
  (setvar "osmode" 1)
  (setq pt (getpoint "\nPick id Point")
	x (car pt)
	)
 (setvar "osmode" 0) 
 (setq of (polar pt (* (/ pi 180) 97) 7.85)
        )
  (command "text" of 2.5 90 (rtos (abs X) 2 3))
(setvar "osmode" *os)
(princ "\n")
  (princ X)
  (PRINC)
 )
  (defun c:LO (/ pt1 pt2 odist entlst lyr)	;for offsetting at drawing scale
(setq *os (setvar "osmode" *os))
    (SETVAR "CMDECHO" 0)
  (princ "\nOffset at 0.10")
    (setvar "osmode" 0)
(setq *oD (getvar "OFFSETDIST"))
  (setq pt1 (ENTSEL "\nSelect object to offset "))
  ;(setvar "osmode" 0)
  (setq pt2 (getpoint "\nSpecify point on Side to offset "))
  (command "offset" 0.1 (CADR pt1) pt2 "")
    (setq entlst (entget (entlast))
	lyr (assoc 8 entlst)
	)
  (entmod (subst (cons 8 "l01") lyr entlst))
  ;(setvar "osmode" *os)
  (setvar "OFFSETDIST" *OD)
    (setvar "osmode" *os)
  (princ)
    )
(defun c:CHOe (/ ob tr id pta ptb mid of x y xpta ypta yptb half)
(command "undo" "begin")
(princ "\nChainage Offset and Elevations ")  
  (setvar "cmdecho" 0)
  (setq *os (getvar "osmode"))
  (while (= ob nil)
  (setq ob (car (entsel "\nselect object to offset")))
    (if (not ob)(princ "Nothing selected"))
    )
;;;  (princ  "\nSelect cutting edges:")
;;;  (setq tr (ssget))
  (setq id 0)
  (while id
    (setvar "osmode" 1)
    (setq id (getpoint "\nPick id Point"))
    (if (/= id nil)
      (progn
      (setq x (car id)
	  y (cadr id)
	  )
        (command "offset" "t" ob id "")
    ;(command "ucs" "w")
    (setvar "osmode" 0)
;;;    (setq pta (cdr (assoc 10 (entget (entlast))))
;;;	  ptb (cdr (assoc 11 (entget (entlast))))
;;;	  )
;;;    ;(command "zoom" 9)
;;;    (command "trim" tr "" pta ptb "")
    ;(command "zoom" "p")
    (setq pta (cdr (assoc 10 (entget (entlast))))
	  ptb (cdr (assoc 11 (entget (entlast))))
    	  PTA (TRANS PTA 0 1)
	  PTB (TRANS PTB 0 1)
	  xpta (car pta)
	  ypta (cadr pta)
	  yptb (cadr ptb)
	  half (/ (+ ypta yptb) 2)
	  mid (list xpta half)
	  )
 (setq of (polar mid (* (/ pi 180) 180) 0.364))
  (command "text" "j" "m" of 0.5 90 (STRCAT "CH. " (rtos (abs X) 2 3) " " (rtos (abs y) 2 3)))
  ;(command "ucs" "p")
);progn
      );if
    );while
  (setvar "osmode" *os)
  (command "undo" "end")
(princ)
 )
(defun c:CHO (/ ob tr id pta ptb mid of x y xpta ypta yptb half)
(command "undo" "begin")
(princ "\nChainage Offset and Elevations ")  
  (setvar "cmdecho" 0)
  (setq *os (getvar "osmode"))
  (while (= ob nil)
  (setq ob (car (entsel "\nselect object to offset")))
    (if (not ob)(princ "Nothing selected"))
    )
;;;  (princ  "\nSelect cutting edges:")
;;;  (setq tr (ssget))
  (setq id 0)
  (while id
    (setvar "osmode" 1)
    (setq id (getpoint "\nPick id Point"))
    (if (/= id nil)
      (progn
      (setq x (car id)
	  )
        (command "offset" "t" ob id "")
    ;(command "ucs" "w")
    (setvar "osmode" 0)
;;;    (setq pta (cdr (assoc 10 (entget (entlast))))
;;;	  ptb (cdr (assoc 11 (entget (entlast))))
;;;	  )
;;;    ;(command "zoom" 9)
;;;    (command "trim" tr "" pta ptb "")
    ;(command "zoom" "p")
    (setq pta (cdr (assoc 10 (entget (entlast))))
	  ptb (cdr (assoc 11 (entget (entlast))))
    	  PTA (TRANS PTA 0 1)
	  PTB (TRANS PTB 0 1)
	  xpta (car pta)
	  ypta (cadr pta)
	  yptb (cadr ptb)
	  half (/ (+ ypta yptb) 2)
	  mid (list xpta half)
	  )
 (setq of (polar mid (* (/ pi 180) 180) 0.364))
  (command "text" "j" "m" of 0.5 90 (rtos (abs X) 2 3))
  ;(command "ucs" "p")
);progn
      );if
    );while
  (setvar "osmode" *os)
  (command "undo" "end")
(princ)
 )
(DEFUN C:MC (/ OB1 OB2 STR1 STR2 CNT CNT2 CNT3 STRA STRB UNITXT NEWRD CNTPOS1 CNTPOS2 OLDRD STR2A STR2B MOD_STR STR2_DATA)
  (setvar "cmdecho" 0)
  (princ "\nMatch chainage contents")
  (command "redraw")
  (WHILE (= ob1 nil)
  (SETQ t1 (CAR (ENTSEL "\nSelect Source Text:")))
    (if (= T1 NIL)
      (progn
      (PRINC "\nNothing found")
      (setq ob1 nil)
      )
      (IF (= (cdr (assoc 0 (entget t1))) "TEXT")
      (setq ob1 t1)
	(progn
	  (setq ob1 nil)
	  (princ "\nSelected object is not a Text")
	);progn
	);if
    );if
    );WHILE
  (WHILE (= ob2 nil)
  (SETQ t2 (CAR (ENTSEL "\nSelect Target Text:")))
    (if (= T2 NIL)
      (progn
      (PRINC "\nNothing found")
      (setq ob2 nil)
      )
      (IF (= (cdr (assoc 0 (entget t2))) "TEXT")
      (setq ob2 t2)
	(progn
	  (setq ob2 nil)
	  (princ "\nSelected object is not a Text")
	);progn
	);if
    );if
    );WHILE
  (SETQ STR1 (cdr (ASSOC 1 (entget ob1)))
	STR2 (cdr (ASSOC 1 (entget ob2)))
	cnt 0
	cnt2 0
	stra ""
	strb ""
	)
  (while (<= cnt (str                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   