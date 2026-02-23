
;  Listing 1:  ASCII table output generator.
; ---------------------------------------------------------
(defun C:SeeASCII ()
   (prompt "ASCII Values 32 - 126")
   (setq II 32) (terpri)
   (while (< II 127)
      (if (= (rem II 8) 0) (terpri)) ; <-- 8 per row.
      (princ II ) 
      (princ " ") ; <--- Space
      (princ 
         (chr II ) ) 
      (princ (chr 9)) ;<--- Tab
      (setq II 
         (1+ II ) ) )
   (prompt "\nEnd of ASCII table."))
; -------1---------2---------3---------4---------5---------6--------7
;
;   Listing 2.
;
;   Bearing Entries         CADENCE 7/87    Bill Kramer
;
;   BRNG   Input survey information: Bearings, Sideshots
;
;   Free Symbols:
;    C:BRNG   Bearing line entries
;    BEARING  User entry of bearing, return angle off X axis.
;
; -------1---------2---------3---------4---------5---------6--------7
(defun bearing (P1)
   (polar P1
            (getangle P1 "\nBearing angle:")
            (getdist P1 "\nDistance to point:")))
;
; -------1---------2---------3---------4---------5---------6--------7
(defun C:BRNG ( / Exit P1 P2 P3 INLST Upd)
  (prompt "\nBearing line entries.  Press return to exit.")
  (setvar "CMDECHO" 0)
  (setvar "AUNITS" 4) ; <--- Set for Survey Units entry.
  (setq INLST "Bearing New Side"
        Exit nil
        P1 (getpoint "\nStarting point:"))  
  (while (null Exit)
     (initget 8 INLST)
     (setq P2 (getpoint P1 "\nTo point <B/N/S>:"))
     (cond
       ((null P2) (setq Exit "Exit"))
       ((= P2 "Bearing") (setq P2 (bearing P1)))
       ((= P2 "New") (setq P1 (getpoint "\nNew point:")))
       ((= P2 "Side")
            (initget 8 INLST)
            (setq P2 (getpoint P1 "\nSide shot <B>:"))
            (cond
              ((= (type P2) 'LIST) (setq Upd 1))
              (t (setq Upd 1) (setq P2 (bearing P1))))))
      (if (= (type P2) 'LIST)
        (progn
          (command "LINE" P1 P2 "")
          (setq P3 (polar P1 (angle P1 P2) (/ (distance P1 P2) 2.0))
                P3 (polar P3 (+ (angle P1 P2) (/ pi 2.0))
                             (/ (getvar "TEXTSIZE") 4.0)))
          (command "TEXT" "C" P3 "" (* (/ (angle P1 P2) pi) 180.0)
           (strcat (angtos (angle P1 P2) 4 4) " " (rtos (distance 
           P1 P2) 2 2)))
          (if (null Upd) (setq P1 P2))
          (setq Upd nil))))
   (setq Exit "Exit"))
; -------1---------2---------3---------4---------5---------6--------7
