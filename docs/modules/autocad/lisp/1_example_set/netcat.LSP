;;; SYMBOL.LSP Version 2.1, Copyright (C) (1997) by DO MANH HIEN, 44X5
;=============================================================================
(defun C:Netcat (
/  ps1  ps2  pf1  pf2  pf3  pf4  pf5  pf6
   pf7  pf8  sb   sh   se   ang  old_osmode old_wid)
   (setq old_osmode (getvar "OSMODE"))
   (setq ps1 (getpoint "\nPick first point: "))
   (setq ps2 (getpoint ps1 "\nPick second point: "))
   (setq sb 2.0 sh 4.0 se 2.0)
   (setq ang (angle ps1 ps2))
   (setq pf1 (polar ps1 ang (- 0.0 se)))
   (setq pf2 (polar ps1 ang (/ (- (distance ps1 ps2) sb) 2.0) ))
   (setq pf7 (polar pf2 ang (/ sb 3.0)))
   (setq pf3 (polar pf7 (+ ang (/ pi 2)) (/ sh 2.0)))
   (setq pf8 (polar pf7 ang (/ sb 3.0)))
   (setq pf4 (polar pf8 (- ang (/ pi 2)) (/ sh 2.0)))
   (setq pf5 (polar pf8 ang (/ sb 3.0)))
   (setq pf6 (polar ps2 ang se))

   (setvar "OSMODE" 0)
   (setvar "BLIPMODE" 0)
   (setq old_wid (getvar "PLINEWID"))
   (setvar "PLINEWID" 0)

   (command "PLINE" pf1 pf2 pf3 pf4 pf5 pf6 "")

   (setvar "OSMODE" old_osmode)
   (setvar "BLIPMODE" 1)
   (setvar "PLINEWID" old_wid)
   (princ)
   )
;;=============================================================================
(defun C:DF (
/  ps1  ps2  pf1  pf2  pf3  pf4  pf5  pf6
   pf7  pf8  sb   sh   se   ang  old_osmode old_wid)
   (setq old_osmode (getvar "OSMODE"))
   (setq ps1 (getpoint "\nPick first point: "))
   (setq ps2 (getpoint ps1 "\nPick second point: "))
   (setq sb 2.5 sh 3.5 se 1.0)
   (setq ang (angle ps1 ps2))
   (setq pf1 (polar ps1 ang (- 0.0 se)))
   (setq pf2 (polar ps1 ang (/ (- (distance ps1 ps2) sb) 2.0) ))
   (setq pf3 (polar pf2 (+ ang (/ pi 2)) (/ sh 2.0)))
   (setq pf5 (polar pf2 ang sb))
   (setq pf4 (polar pf5 (- ang (/ pi 2)) (/ sh 2.0)))
   (setq pf6 (polar ps2 ang se))

   (setvar "OSMODE" 0)
   (setvar "BLIPMODE" 0)
   (setq old_wid (getvar "PLINEWID"))
   (setvar "PLINEWID" 0)

   (command "PLINE" pf1 pf2 pf3 pf4 pf5 pf6 "")

   (setvar "OSMODE" old_osmode)
   (setvar "BLIPMODE" 1)
   (setvar "PLINEWID" old_wid)
   (princ)
   )
;============================================================================
(defun C:G (/ point p1 p2 oldwid oldosmode)
 (setq point (getpoint "\nInsert point : "))
 (setq p1 (polar point (+ (/ pi 4) pi) 1) )
 (setq p2 (polar point    (/ pi 4)     1) )
 (command "LAYER" "M" "CHI" "")
 (setq oldwid (getvar "PLINEWID"))
 (setq oldosmode (getvar "OSMODE"))
 (setvar "PLINEWID" 0.2)
 (setvar "OSMODE" 0)
 (command "PLINE" p1 point p2 "")
 (setvar "PLINEWID" oldwid)
 (setvar "OSMODE" oldosmode)
 (princ)
)