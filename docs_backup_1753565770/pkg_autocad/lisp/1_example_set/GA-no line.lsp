;;;     GL.LSP
;;;     Permission to use, copy, modify, and distribute this software
;;;     for any purpose and without fee is hereby granted, provided
;;;     that the copyright notice appears in all copies and 
;;;     that both that copyright notice and the limited warranty and 
;;;     restricted rights notice below appear in all supporting 
;;;     documentation.
;;;
;;;     I PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS. 
;;;     JAMIL TAYYAB 
;;;     DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE 
;;;     UNINTERRUPTED OR ERROR FREE.
;;;
;;;     Credits: JAMIL TAYYAB
(ALERT "Life Is The Way To Knowledge And Sharing Knowledge Is The Way To Wisdom.Share Your Knowkedge And Be Wise")

(DEFUN C:GA()
(command "cmdecho" "0")
(WHILE 
(SETQ PT1 (GETpoint"\nLine Starting Level:"))
(SETQ PT2 (GETpoint"\nLine Ending Level:"))

(setq a (angle pt1 pt2))
(setq ang (* a (/ 180.0 pi)))

(setq x (car pt1))	;x coordinates of line start
(setq y (cadr pt1))	;y coordinates of line start

(setq x2 (car pt2))	;x coordinates of line end
(setq y2 (cadr pt2))	;y coordinates of line end

(setq xi (/ (+ x x2) 2 ))	;mid x coordinates of line
(setq yi (/ (+ y y2) 2 ))	;mid y coordinates of line

(setq Pti (list xi yi))	;mid point of the line

(setq xa (if (> x x2) (- x x2) (- x2 x)))
(setq ya (if (> y y2) (- y y2) (- y2 y)))

(setq div (/ ya xa))

(setq d (rtos div))

(setq a1 (rtos ang))

(setq ga (if (> y y2) (strcat "-" d "%") (strcat d "%")))

(command "line" pt1 pt2 "" )

(command "text" "_mc" pti a1 ga )

)

)
