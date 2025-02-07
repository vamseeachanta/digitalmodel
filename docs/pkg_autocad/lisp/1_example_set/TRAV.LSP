;   func dms final out put variable is strdeg  
; 	convert angles in degs to rads
(defun dtr (a)
  (* pi (/ a 180.00))
)

(defun rtd (a)
  (/ (* a 180.00) pi)
)

(defun dms (/ degs mins secs strmin strsec )
		(setq deg (getreal "\nEnter degs: ")
		      mins (getreal "\nEnter mins: ")
		      sec (getreal "\nEnter secs: ")
		      ang_degs (+ deg (/ mins 60.0) (/ sec 3600.0))
		); **		end setq
);**  end func degs
(defun c:trav ()
	(graphscr)
	(setq Stpt (Getpoint "\nPick starting point: "))
	(if (null stpt)(setq stpt (getvar "LASTPOINT")))
 (while (setq d (getdist "\nDist: "))
	(print "4=NW | 1=NE")
	(print "-----+-----    5=Last bearing")
	(print "3=SW | 2=SE    Enter quadrant: ")
	(setq quad (getint))
	(if (= quad 5)(setq bear bear)
	 (progn 
	  (dms)
	  (cond ((= quad 1)(setq bear (- 90.0 ang_degs)))
		((= quad 2)(setq bear (+ 270.0 ang_degs)))   
		((= quad 3)(setq bear (- 270.0 ang_degs)))   
		((= quad 4)(setq bear (+ 90.0 ang_degs)))
	  ); end cond
	 ); end progn
	); end if
        (setq newpt (polar stpt (dtr bear) d))
	(command "LINE" stpt newpt "")
	(setq stpt newpt)
	(setvar "LASTPOINT" newpt)
  ); end while
)
       
