;;;     PTL.LSP
;;;     Permission to use, copy, modify, and distribute this software
;;;     for any purpose and without fee is hereby granted, provided
;;;     that the above copyright notice appears in all copies and 
;;;     that both that copyright notice and the limited warranty and 
;;;     restricted rights notice below appear in all supporting 
;;;     documentation.
;;;
;;;     JAMIL TAYYAB PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS. 
;;;     JAMIL TAYYAB 
;;;     DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE 
;;;     UNINTERRUPTED OR ERROR FREE.
;;;
;;;     Credits: JAMIL TAYYAB
(write-line "Search ->> JT CAD <<-- on Youtube for AutoCAD Tips & Tricks")
(write-line "Visit >> http://www.freecadtipsandtricks.com/ <<")
(ALERT "Life is The Way To Knowledge and Sharing Knowledge is The Way To Wisdom. Share Your Knowkedge and Be Wise.")
(DEFUN C:PTL()
(WHILE
(SETQ PT (GETPOINT"\nClick At Point Position:"))
(SETQ ele (GETreal"\nPoint Elevation?"))
(setq x (car pt))
(setq y (cadr pt))
(setq ptn (list x y ele))
(COMMAND "COLOR" "bylayer")
(command "layer" "make" "-1 POINT" "")
(command "LAYER" "THAW" "-1 POINT" "")
(command "LAYER" "SET" "-1 POINT" "c" 3"" "")
(command "POINT" ptn "")
(command "layer" "make" "-1 POINT ELEVATION" "")
(command "LAYER" "THAW" "-1 POINT ELEVATION" "")
(command "LAYER" "SET" "-1 POINT ELEVATION" "c" 2"" "")
(command "text" ptn ".25" "0" (rtos ele) )
))