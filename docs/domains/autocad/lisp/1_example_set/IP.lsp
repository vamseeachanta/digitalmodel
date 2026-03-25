;;;     IP.LSP
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
(defun c:IP ()
(setq cs (getreal "\n Start Chainage (Length):  "))
(setq ls (getreal "\n Start Chainage Level:  "))

(setq ce (getreal "\n End Chainage (Length): "))
(setq le (getreal "\n End Chainage Level:  "))

(while

(setq ic (getreal "\n Enter Your Desired Chainage:  "))

(setq gc (max cs ce))
(setq gl (max ls le))

(setq sc (min cs ce))
(setq sl (min ls le))

(setq dc (- gc sc))
(setq dl (- gl sl))

(setq gr (/ dl dc))

(setq int (* gr ic))

(setq intl (+ sl int))

(setq inte (* (- ic cs) gr))

(setq intlvl (if (= ls sl) (+ ls inte) (- ls inte)))

(setq grd (* gr 100))

(setq ics (rtos ic))
(setq ils (rtos intlvl))

(setq grds (rtos (if (= ls sl) grd (* grd -1))))

(alert
        (strcat
           "\n\n" "Chainage\t->\t" ics 
           "\n\n" "Level\t->\t" ils 
           "\n\n" "Gradient\t->\t" grds "%"
        )
)
     (prompt
        (strcat
           "\n\n" "\t\t\t\t\t\t\t\t--------------->\tChainage\t->\t" ics 
           "\n\n" "\t\t\t\t\t\t\t\t--------------->\tLevel\t\t->\t" ils 
           "\n\n" "\t\t\t\t\t\t\t\t--------------->\tGradient\t->\t" grds "%"
        )
     )

)
(prompt "\n--------Thank You-----------")

(princ)
)

(prompt "\n--------Thank You-----------")
(prompt "\nType IP to run \t\tVersion 1.9.5.11")

(princ)