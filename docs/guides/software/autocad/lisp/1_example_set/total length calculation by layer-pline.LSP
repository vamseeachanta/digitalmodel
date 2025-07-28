;---------This lisp is dedicated for THEOVEL SURVEYS By M.velu---------------------
;**This command will give the total length for all polyline for the selected layer **
;------------------------------------------------------------------------------------
(defun c:PR()
  (PROMPT "\n Select only the polyline for the required layer: ")
  (setq Ent(car(entsel))
	lay_name(cdr(Assoc 8 (entget ent)))
	sset(Ssget "x" (list (cons 0 "lwpolyline")(Cons 8 lay_name)))
	index 0
	runlen 0
	) 
	
  ;(if  sset 
(progn
    (Setq ent (ssname sset index))
	 (while (/= ent nil)
	  (command "area" "object" ent)
	  (setq area(getvar "perimeter")
                runlen (+ runlen area) 
		index (+ 1  index)
		ent(Ssname sset index))))
(alert (strcat "TOTAL LENGTH OF THE SELECTED LAYER=" "\n** "  (rtos runlen 2 3) " Meters **")))
