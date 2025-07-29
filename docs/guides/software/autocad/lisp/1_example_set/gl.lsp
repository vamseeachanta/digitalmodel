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
(ALERT "Life Is The Way To Knowledge And Sharing Knowledge Is The Way To Wisdom.Share Your Knowkedge And Be Wise.")

(write-line "Search ->> JT CAD <<-- on Youtube for AutoCAD Tips & Tricks")
(write-line "Visit >> http://www.freecadtipsandtricks.com/ <<")
(defun c:gl ()
(setq lvl (getreal "\n Enter Spot Level (Elevation):  "))
(setq grd (getreal "\n Enter Gradient ( % ):  "))

(while

(setq ofs (getreal "\n Enter Offset From the Spot:  "))

(setq grdp (/ grd 100))	;

( setq chg (* ofs grdp))

(setq lvl2 (+ lvl chg))

(setq sofs (rtos ofs))		;convert real to string

(setq slvl2 (rtos lvl2))		;convert real to string

(setq sgrd (rtos grd))		;convert real to string

(alert
        (strcat
           "\n\n" "Offset\t->\t" sofs 
           "\n\n" "Level\t->\t" slvl2 
           "\n\n" "Gradient\t->\t" sgrd "%"
        )
)
     (prompt
        (strcat
           "\n\n" "\t\t\t\t\t\t\t\t--------------->\tOffset\t\t\t->\t" sofs 
           "\n\n" "\t\t\t\t\t\t\t\t--------------->\tLevel\t\t->\t" slvl2
           "\n\n" "\t\t\t\t\t\t\t\t--------------->\tGradient\t->\t" sgrd "%"
        );prompt
     );strcat
);while

(prompt "\n--------Thank You-----------")

(princ)
)


(prompt "\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t--------Thank You-----------")
(prompt "\nType GL to run \t\tVersion 1.11.5.11")

(princ)