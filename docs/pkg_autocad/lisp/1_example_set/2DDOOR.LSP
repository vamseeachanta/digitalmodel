;REID REAGAN 1-25-95
;PROGRAM FOR DOOR WITH TRIM AND KNOB
(DEFUN C: 2dDOOR ()
 (SETVAR "CMDECHO" 0)
 (SETQ HOR (GETDIST "\nHORIZONTAL DISTANCE:"))
 (SETQ VER (GETDIST "\nVERTICAL DISTANCE:"))
 (SETQ TR  (GETDIST "\nTRIM WIDTH:"))
 (SETQ KNOB (GETDIST "\nENTER DOOR KNOB RADIUS:"))
 (SETQ CL (GETVAR "CLAYER"))
 (SETQ SP (GETPOINT "\nPICK LOWER LEFT CORNER OF OPENING:"))
 (SETQ P2 (POLAR SP (DTR 0) HOR))
 (SETQ P3 (POLAR SP (DTR 180) TR))
 (SETQ P4 (POLAR P2 (DTR 0) TR))
 (SETQ P5 (POLAR P4 (DTR 90) (+ TR VER)))
 (SETQ P6 (POLAR P5 (DTR 270) TR))
 (SETQ P7 (POLAR P6 (DTR 180) TR))
 (SETQ P8 (POLAR P7 (DTR 180) HOR))
 (SETQ P9 (POLAR P8 (DTR 180) TR))
 (SETQ P10 (POLAR P9 (DTR 90) TR))
 (SETQ P11 (POLAR P10 (DTR 270) (+ TR VER)))
 (COMMAND "LAYER" "M" "DOOR" "C" "YELLOW" "" "")
 (COMMAND "LINE" SP P8 P7 P2 P3 P10 P8 P10 P5 P7 P5 P4 P2 C) 
 (SETQ P12 (POLAR SP (DTR 90) 36))
 (SETQ P13 (POLAR P12 (DTR 0) 3))
 (COMMAND "LAYER" "M" "KNOB" "C" "GREEN" "" "")
 (COMMAND "CIRCLE" P13 KNOB "")
 (SETVAR "CLAYER" CL)
)
 (DEFUN DTR (a) (* Pi (/ a 180.0)))
 (PRINC)



 
 