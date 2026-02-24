;Breakline1.LSP                             (C)2007, AROBAT NOSYAJ


(defun
   C:BL1 (/ CE BM OS OE SP XD XA EP P1 P2 P2 P3 P4 P5 P6
         )
  (setq
    CE (getvar "CMDECHO")
    BM (getvar "BLIPMODE")
    OM (getvar "OSMODE")
  ) ;_ end of setq
  (defun
     NE (NE)
    (setvar "CMDECHO" CE)
    (setvar "BLIPMODE" BM)
    (setvar "OSMODE" OM)
    (princ "Function cancelled ")
    (princ)
  ) ;_ end of defun
  (setq
    OE *ERROR*
    *ERROR* NE
  ) ;_ end of setq
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 16383)
  (while (= SP NIL)
    (setq SP (getpoint "\nPick start point of breakline: "))
  ) ;_ end of while
  (while (= EP NIL)
  (setvar "OSMODE" 128)
    (setq
      EP (getcorner SP "\nPick end point of breakline <length>: ")
    ) ;_ end of setq
  ) ;_ end of while
  ;(if (>= (angle SP EP) (/ pi 2))
  ;  (setq EP (polar SP (/ pi 4) (distance SP EP)))
  ;) ;_ end of if
  (setvar "OSMODE" 0)
  (setq
    XD (distance SP EP)
    XA (angle SP EP)
    P1 (polar SP (+ XA pi) (/ XD 10))   
    P2 (polar P1 XA (/ XD 2))
    P3 (polar P2 (+ XA 1.570796327) (/ XD 5))
    P4 (polar P3 (+ XA 5.175992654) (/ XD 2.236065955))
    P5 (polar P4 (+ XA 1.570796327) (/ XD 5))
    P6 (polar P5 XA (/ XD 2))
    
    
  ) ;_ end of setq
  (command
    ".LAYER"
    "N"
    "BRKLINE"
    "C"
    "7"
    ""
    "S"
    "BRKLINE"
    ""
    ".PLINE"
    P1
    P2
    P3
    P4
    P5
    P6
    ""
    
    
  ) ;_ end of command
 (setvar "OSMODE" 16383)
  (princ)
) ;_ end of defun
;Breakline2.LSP                             (C)2007, AROBAT NOSYAJ


(defun
   C:BL2 (/ CE BM OS OE SP XD XA EP P1 P2 P2 P3 P4 P5 P6 P7 P8 P9 P10
         )
  (setq
    CE (getvar "CMDECHO")
    BM (getvar "BLIPMODE")
    OM (getvar "OSMODE")
  ) ;_ end of setq
  (defun
     NE (NE)
    (setvar "CMDECHO" CE)
    (setvar "BLIPMODE" BM)
    (setvar "OSMODE" OM)
    (princ "Function cancelled ")
    (princ)
  ) ;_ end of defun
  (setq
    OE *ERROR*
    *ERROR* NE
  ) ;_ end of setq
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 16383)
  (while (= SP NIL)
    (setq SP (getpoint "\nPick start point of breakline: "))
  ) ;_ end of while
  (while (= EP NIL)
  (setvar "OSMODE" 128)
    (setq
      EP (getcorner SP "\nPick end point of breakline <length>: ")
    ) ;_ end of setq
  ) ;_ end of while
  ;(if (>= (angle SP EP) (/ pi 2))
  ;  (setq EP (polar SP (/ pi 4) (distance SP EP)))
  ;) ;_ end of if
  (setvar "OSMODE" 0)
  (setq
    XD (distance SP EP)
    XA (angle SP EP)
    P1 (polar SP (+ XA pi) (/ XD 10))
    P2 (polar P1 XA (/ XD 2))
    P3 (polar P2 (+ XA 1.570796327) (/ XD 10))
    P4 (polar P3 (+ XA 0.7854) (/ XD 14.14207125))
    P5 (polar P4 (- XA 0.7854) (/ XD 14.14207125))
    P6 (polar P5 (- XA 1.570796327) (/ XD 5))
    P7 (polar P6 (- XA 0.7854) (/ XD 14.14207125))
    P8 (polar P7 (+ XA 0.7854) (/ XD 14.14207125))
    P9 (polar P8 (+ XA 1.570796327) (/ XD 10))
    P10 (polar P9 XA (/ XD 2))
    
    
  ) ;_ end of setq
  (command
    ".BLIPMODE"
    "OFF"
    ".FILL"
    "ON"
    ".ZOOM"
    "W"
    XZ
    YZ
    ".LAYER"
    "N"
    "BRKLINE"
    "C"
    "7"
    ""
    "S"
    "BRKLINE"
    ""
    ".PLINE"
    P1
    P2
    P3
    "ARC"
    P4
    P5
    "LINE"
    P6
    "ARC"
    P7
    P8
    "LINE"
    P9
    P10
    ""
  ) ;_ end of command
  (setvar "OSMODE" 16383)
  (princ)
) ;_ end of defun
;Breakline3.LSP                             (C)2007, AROBAT NOSYAJ


(defun
   C:BL3 (/ CE BM OS OE SP XD XA EP P1 P2 P2 P3 P4 P5 P6
         )
  (setq
    CE (getvar "CMDECHO")
    BM (getvar "BLIPMODE")
    OM (getvar "OSMODE")
  ) ;_ end of setq
  (defun
     NE (NE)
    (setvar "CMDECHO" CE)
    (setvar "BLIPMODE" BM)
    (setvar "OSMODE" OM)
    (princ "Function cancelled ")
    (princ)
  ) ;_ end of defun
  (setq
    OE *ERROR*
    *ERROR* NE
  ) ;_ end of setq
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 16383)
  (while (= SP NIL)
    (setq SP (getpoint "\nPick start point of breakline: "))
  ) ;_ end of while
  (while (= EP NIL)
  (setvar "OSMODE" 128)  
    (setq
      EP (getcorner SP "\nPick end point of breakline <length>: ")
    ) ;_ end of setq
  ) ;_ end of while
  ;(if (>= (angle SP EP) (/ pi 2))
  ;  (setq EP (polar SP (/ pi 4) (distance SP EP)))
  ;) ;_ end of if
  (setvar "OSMODE" 0)
  (setq
    XD (distance SP EP)
    XA (angle SP EP)
    P1 (polar SP (+ XA 0.3805) (/ XD 3.7139))
    P2 (polar SP XA (/ XD 2))
    P3 (polar P2 (- XA 0.3805) (/ XD 3.7139))
    P4 (polar EP (+ XA 2.7611) (/ XD 3.7139))
    P5 (polar SP (+ XA 3.8163) XD)
    P6 (polar P2 (+ XA 3.8163) XD)
    
    
  ) ;_ end of setq
  (command
    ".BLIPMODE"
    "OFF"
    ".FILL"
    "ON"
    ".ZOOM"
    "W"
    XZ
    YZ
    ".LAYER"
    "N"
    "BRKLINE"
    "C"
    "7"
    ""
    "S"
    "BRKLINE"
    ""
    ".SPLINE"
    SP
    P1
    P2
    P3
    EP
    P4
    P2
    ""
    P5
    P6
    
    
  ) ;_ end of command
  (setvar "OSMODE" 16383)
  (princ)
) ;_ end of defun
