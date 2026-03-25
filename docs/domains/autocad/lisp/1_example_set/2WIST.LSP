;  Originally this function was intended for making assembly drawings of
;electronic cables using twisted pairs of wires.
;  However,it can also be used for drawing extension springs when the number
;requested for the degree of twist is at the upper limit of 85.
;  Other uses include drawing ropes, steel cables or decorative borders.
;                                                      D.Mead    3/17/94
;
;
; DRAWS TWISTED PAIR OF WIRES
(defun c:2wist (/ n tw deg pt1 pt2 oj jf ej eo of ef em ac lk ck
ek len x pta ptb ptc ptd pte ptf ptk ptm count)
  (graphscr)
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "orthomode" 1)
  (setvar "blipmode" 1)
;GET INPUT DATA
  (prompt "Draws a twisted pair of wires from left to right") (terpri)
  (setq n (getreal "Enter wire diameter:  ")) (terpri)
  (setq tw (getreal "Select degree of twist: {between 5 and 85} <10> "))
  (setq pt1 (getpoint "Pick starting point:  ")) (terpri)
  (setq pt2 (getpoint "Pick ending point:  ")) (terpri)
  (if (< tw 5) (setq tw 10))
  (if (> tw 85) (setq tw 10))
;FORMULAS AND VARIABLES
  (setq deg tw)
  (setq oj (* 0.5 n))
  (setq eo (/ oj (sin (* deg (/ pi 180)))))
  (setq ej (* eo (sin (* (- 90 deg) (/ pi 180)))))
  (setq of (/ oj (cos (* deg (/ pi 180)))))
  (setq jf (* of (cos (* (- 90 deg) (/ pi 180)))))
  (setq ef (+ ej jf))
  (setq em (/ ef (sin (* deg (/ pi 180)))))
  (setq ac (* 2 eo))
  (setq ck (/ eo (sin (* deg (/ pi 180)))))
  (setq lk (* ck (sin (* (- 90 deg) (/ pi 180)))))
  (setq ek (- lk of))
  (setq len (distance pt1 pt2))
  (setq x (- (/ len ac) 2))
;ESTABLISH POINTS
  (setq pta pt1)
  (setq ptb (polar pta (* (- 360 deg) (/ pi 180)) ef))
  (setq ptc (polar pta (* deg (/ pi 180)) ef))
  (setq ptd (polar pta  0.0 ac))
  (setq ptk (polar pta (* 270 (/ pi 180)) ek))
  (setq ptm (polar ptd (* 90 (/ pi 180)) ek))
  (setq pte (polar ptm (* (+ 270 deg) (/ pi 180)) ck))
  (setq ptf (polar ptk (* (+ 90 deg) (/ pi 180)) ck))
;DRAW LINES & ARC
  (setvar "blipmode" 0)
  (command "line" pta ptb "")
  (command "line" ptc ptd "")
  (command "arc" "c" ptk ptc ptf)
  (command "arc" "c" ptm ptb pte)
;START LOOP - add up sections til =  dist from pt1 to pt2
    (setq count 0)
    (while count (< count x)
       (setq count (+ count 1))
;REDEFINE POINT LOCATIONS
       (setq pta ptd)
       (setq ptb (polar pta (* (- 360 deg) (/ pi 180)) ef))
       (setq ptc (polar pta (* deg (/ pi 180)) ef))
       (setq ptd (polar pta  0.0 ac))
       (setq ptk (polar pta (* 270 (/ pi 180)) ek))
       (setq ptm (polar ptd (* 90 (/ pi 180)) ek))
       (setq pte (polar ptm (* (+ 270 deg) (/ pi 180)) ck))
       (setq ptf (polar ptk (* (+ 90 deg) (/ pi 180)) ck))
;REPEAT LINES & ARCS
       (command "line" pta ptb "")
       (command "line" ptc ptd "")
       (command "arc" "c" ptk ptc ptf)
       (command "arc" "c" ptm ptb pte)
       (if (>= count x) (setq count nil))
    )
  (setvar "blipmode" 1)
)





 