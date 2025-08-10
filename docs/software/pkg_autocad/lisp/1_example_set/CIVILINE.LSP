; CIVIL LINE AutoLISP PROGRAM       BILLINGS VO-TECH       1-28-92
;                                   3803 Central
;                                   Billings, MT
;                                   406-656-4445
;
; Draws a boundry line and labels the distance and bearing using
; the current layer and text height.
;    Students - Don S., Don A., Steve B., Tom E., Hal S., Bill P.,
;               Dave H., Kyle W., Callie D.
;    Instructor - Tim Urbaniak
;
; This program was created during a classroom exercise and is hereby
; placed into the public domain. Support education in your area...
;
(defun C:CIVILINE ()
  (graphscr)
    (setq th (getvar "textsize"))
    (setq p1 (getpoint "\nPick the Boundry Start Point: "))
    (setq dist (getdist "\nEnter the Boundry Distance: "))
    (setq bearing (getangle "\nEnter the Boundry Bearing: "))
    (setq p2 (polar p1 bearing dist))
    (setq hdist (/ dist 2))     ;half of the boundry distance
    (setq hth (/ th 2))         ;save half of the text height
    (setq p3 (polar p1 bearing hdist))
    (setq p4 (polar p3 (+ (dtr 90) bearing) hth))
    (setq p5 (polar p3 (+ (dtr 270) bearing) hth))
 (command "line" p1 p2 "")
 (command "text" "j" "bc" p4 "" (rtd bearing) (rtos dist))
 (command "text" "j" "tc" p5 "" (rtd bearing) (angtos bearing))
)
(defun dtr (a)
 (* pi (/ a 180.0)))
(defun rtd (a)
 (/ (* a 180.0) pi))
(princ "\nUses current layer, current text height and units.")
(princ "\nMake sure default conditions are appropriate.")
(princ "\nIf the text is upside down, use rotate, select the text,")
(princ "\ntype !p3 for the point of rotation with an angle of 180.")
(princ "\nWill crash if your text style has a default height other")
(princ "\nthan 0.00 -")
(princ)

