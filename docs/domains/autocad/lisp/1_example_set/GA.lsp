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

(write-line "Search ->> JT CAD <<-- on Youtube for AutoCAD Tips & Tricks")
(write-line "Visit >> http://www.freecadtipsandtricks.com/ <<")
(DEFUN C:GA()
(command "cmdecho" "0")

(setq hs (getreal "\n Horizontal Scale:  "))
(setq vs (getreal "\n Vertical Scale:  "))

(WHILE 
(SETQ PT1 (GETpoint"\n Line Starting Level:"))
(SETQ PT2 (GETpoint"\n Line Ending Level:"))


(setq a (angle pt1 pt2))
(setq ang (* a (/ 180.0 pi)))

(setq x (* (car pt1) hs))	;x coordinates of line start
(setq y (* (cadr pt1) VS))	;y coordinates of line start

(setq x2 (* (car pt2) hs))	;x coordinates of line end
(setq y2 (* (cadr pt2) VS))	;y coordinates of line end

(setq xi (/ (+ x x2) 2 ))	;mid x coordinates of line
(setq yi (/ (+ y y2) 2 ))	;mid y coordinates of line

(setq Pti (list xi yi))		;mid point of the line

(setq x3 (/ XI hs))		;x coordinates of line end
(setq y3 (/ YI VS))		;y coordinates of line end

(setq Ptt (list x3 y3))		;mid point for text

(setq xa (if (> x x2) (- x x2) (- x2 x)))
(setq ya (if (> y y2) (- y y2) (- y2 y)))

(setq div (/ ya xa))

(setq d (rtos (* div 100)))

(setq a1 (rtos ang))

(setq ga (if (> y y2) (strcat "-" d "%") (strcat d "%")))

(command "_.LAYER" "_M" "-1 Gradient Line" "_C" "253" "" "")

(command "line" pt1 pt2 "" )

(command "_.Style" "GRADE" "romant" "2.5" "0.75" "" "" "" "")
(command "_.Text" "_S" "GRADE") (command)

(command "_.LAYER" "_M" "-1 Gradient" "_C" "100" "" "")

(command "text" "_mc" ptt a1 ga )

)

)
