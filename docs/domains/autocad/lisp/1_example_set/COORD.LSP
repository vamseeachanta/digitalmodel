;************************************************************************
;*                                                                      *
;*      Program Developer:       IBRAHIM K. A.         23.07.97         *
;*                                                                      *
;*      Application Objective:  To read the coordinates of a block,     *
;*      by prompting the user for block name, to read the attribute     *
;*      value and to arrange the details in tabular form.               *
;*                                                                      *
;*      (c)  Copyright  1997   Microtech  India  International.         *
;*                                                                      *
;************************************************************************
;
(defun C:COORD (/ dz ss ss0 ss1 pt1 pt2 pt3 ang counter atvalue ip ip1 xvalue xvalue1 yvalue yvalue1 dt)
;
;                                       Version Number
	(setq coord_ver "07.97")
	(princ (strcat "\n              Coordinate Readout: Version " coord_ver))
	(princ "\n  Copyright (c) 1996 by Microtech India International.")
  (setq nam (strcat (chr 73) (chr 66) (chr 82) (chr 65) (chr 72) (chr 73) (chr 77)))
  (setq dz (getvar "DIMZIN"))
  (setq cmde (getvar "CMDECHO"))
  (setq osme (getvar "OSMODE"))
  (setq ortm (getvar "ORTHOMODE"))
  (setq clay (getvar "CLAYER"))
  (setvar "DIMZIN" 0)
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setq dis "This programme belongs to ")
  (setq nam1 (strcat dis nam))
  (if (= blknm nil) (setq blknm "COORDTP"))
  (if (= dist1 nil) (setq dist1 18.5))
  (if (= dist2 nil) (setq dist2 45))
  (if (= txtht nil) (setq txtht 2.5))
  (if (= gap nil) (setq gap 7.5))
  (setq blknm1 (strcase (getstring (strcat "\nBlock Name to Select <" blknm ">: "))))
  (if (/= blknm1 "") (setq blknm blknm1))
  (initget 1 "All Select")
  (setq sel (strcase (getkword "\  Do you want Selected entities/All entities ( S or A ) ?: ")))
  (if (= sel "SELECT") (setq ss (ssget (list (cons 2 blknm)))) (setq ss (ssget "X" (list (cons 2 blknm)))))
  (if (/= ss nil)
    (progn
      (setq styl (getvar "textstyle"))
      (if (/= (tblsearch "style" "coord") nil) (setvar "textstyle" "coord")
      (command "style" "coord" "romans" "0" "0.9" "0" "n" "n" "n"))
      (setq pt1 (getpoint "\nTable Start Point: "))
      (setvar "ORTHOMODE" 1)
      (setq dist11 (getdist pt1 (strcat "\nDistance to the Second Column <" (rtos dist1) ">: ")))
      (if (/= dist11 nil) (setq dist1 dist11))
      (setq dist21 (getdist pt1 (strcat "\nDistance to the Third Column <" (rtos dist2) ">: ")))
      (if (/= dist21 nil) (setq dist2 dist21))
      (setq txtht1 (getdist pt1 (strcat "\nHeight of Text <" (rtos txtht) ">: ")))
      (if (/= txtht1 nil) (setq txtht txtht1))
      (setq gap1 (getdist pt1 (strcat "\nDistance between Rows <" (rtos gap) ">: ")))
      (if (/= gap1 nil) (setq gap gap1))
      (setq ang (/ pi 2)) 
      (setq pt2 (polar pt1 0 dist1))
      (setq pt3 (polar pt1 0 dist2))
      (setq counter 0)
      (while (< counter (sslength ss))
	(if (equal (cdr (assoc 0 (entget
	(ssname ss counter)))) "INSERT")
	  (progn
	    (setq ss0 (cdr (assoc -1 (entget
	    (ssname ss counter)))))
	    (setq ss1 (entnext ss0))
	    (setq atvalue (cdr (assoc 1 (entget ss1))))
	    (setq ip (cdr (assoc 10 (entget
	    (ssname ss counter)))))
	    (setq ip1 (trans ip 0 1))
	    (setq xvalue1 (car ip1))
	    (setq xvalue (rtos xvalue1 2 3))
	    (setq yvalue1 (cadr ip1))
	    (setq yvalue (rtos yvalue1 2 3))
	    (setq dt (Tblsearch "layer" "coord_text"))
	    (if (= dt nil) (command "layer" "n" "coord_text" ""))
	    (Command  "layer" "u" "coord_text" "t" "coord_text"
	    "on" "coord_text" "s" "coord_text" "c" "2" "coord_text" "")
	    (Command "TEXT" "J" "M" pt1 txtht "0" atvalue)
	    (Command "TEXT" "j" "M" pt2 txtht "0" xvalue)
	    (Command "TEXT" "J" "M" pt3 txtht "0" yvalue)
	    (grtext 25 nam)
	    (grtext -1 nam1)
	    (setq pt4 (polar pt1 ang gap))
	    (setq pt1 pt4)
	    (setq pt2 (polar pt1 0 dist1))
	    (setq pt3 (polar pt1 0 dist2))
	    (setq counter (1+ counter))
	  )
	)
      ) 
    )
  )
  (princ)
  (setvar "DIMZIN" dz)
  (setvar "CMDECHO" cmde)
  (setvar "OSMODE" osme)
  (setvar "ORTHOMODE" ortm)
  (setvar "TEXTSTYLE" styl)
  (setvar "CLAYER" clay)
  (command "LAYER" "f" "coordtp,coordtp_at" "")
  (princ)
)
