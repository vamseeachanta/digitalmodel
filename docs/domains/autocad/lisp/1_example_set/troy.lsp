;-------------------------------------------------------------------------------
; Program Name: Troy.lsp [Troy R5] - Asteroids AutoLISP game
; Created By:   Terry Miller (Email: terrycadd@yahoo.com)
;               (URL: http://web2.airmail.net/terrycad)
; Globalization by XANADU (www.xanadu.cz) 27.9.2006
; Date Created: 1-20-06
; Notes:        Troy is an Asteroids AutoLISP game driven by the grread function.
;               It can be run inside of an existing drawing. When it's finished,
;               it purges all entities, styles and layers it created. You have
;               three ships to use to shoot down as many Troys as possible. If
;               a Troy runs into your ship, it blows up your ship and you loose
;               10 points. Each Troy you blow up, you gain its value in points.
;               Use the mouse to keep the game moving. Pick the mouse to fire
;               at Troys. Each fire cost you 1 point. Press P to pause the game.
;               Press Q to quit the game before it ends, in order to purge all
;               entities, styles and layers it created. If you press the escape
;               key to abort the game, simply rerun Troy again and select the
;               Clear option. So do not press the escape key to abort the game.
; Disclaimer:   This program is free to download and share and learn from. It
;               contains many useful functions that may be applied else where.
;               Every effort on my part has been to create a grread game that
;               will run in most versions of AutoCAD, and when finished it will
;               return to the environment before it started.  Troy is now yours
;               to tweak, debug, add to, rename, use parts of, or create another
;               grread game from. It is now your responsibility when, and within
;               what drawings you should run it. If you are unsure of how it may
;               affect certain drawing environments, do a saveas before running
;               it. Do not save a drawing without running the Troy Clear option.
;-------------------------------------------------------------------------------
; Revision History
; Rev  By     Date    Description
;-------------------------------------------------------------------------------
; 1    TM   1-20-06   Initial version.
; 2    TM   6-20-06   Revised PurgeGroups function.
; 3    TM   6-24-06   Revised program to switch to the Model tab if there are
;                     viewports on the current Layout tab.
; 4    TM   6-26-06   Added Settings option to adjust number of Troys, speed of
;                     Troys and Color Scheme.
; 5    TM   1-1-07    The *_nth list functions were revised for maximum speed.
;-------------------------------------------------------------------------------
; c:Troy - Asteroids AutoLISP game
;-------------------------------------------------------------------------------
(defun c:Troy (/ Colors$ Loop Option$ Settings$)
  (initget "Intro Clear Settings Play")
  (if (not (setq Option$ (getkword "\nTroy options [Intro/Clear/Settings/<Play>]: ")))
    (setq Option$ "Play")
  );if
  (cond
    ((= Option$ "Clear")(TroyClear)(princ "\nTroy objects Cleared."))
    ((= Option$ "Settings")
      (initget "Troys Speed Colors Defaults")
      (if (not (setq Settings$ (getkword "\nSettings [Troys/Speed/Colors/<Defaults>]: ")))
        (setq Settings$ "Defaults")
      );if
      (cond
        ((= Settings$ "Troys")
          (setq Loop t)
          (while Loop
            (if (not (setq *MinTroys#* (getint "\nMinimum number of Troys <5>: ")))
              (setq *MinTroys#* 5)
            );if
            (if (not (setq *MaxTroys#* (getint "\nMaximum number of Troys <10>: ")))
              (setq *MaxTroys#* 10)
            );if
            (if (or (< *MinTroys#* 1) (<= *MaxTroys#* *MinTroys#*))
              (princ "\nThe maximum number must be greater than the minimum number,\nand the minimum number must be greater than 0.")
              (setq Loop nil)
            );if
          );while
          (if (> *MaxTroys#* 20)
            (princ "\nIncreasing the maximum number of Troys may slow down the game.")
          );if
        );case
        ((= Settings$ "Speed")
          (setq Loop t)
          (while Loop
            (if (not (setq *TroySpeed~* (getreal "\nAdjust speed of Troys\nEnter a number between 0.5 and 5.0 <1.0>: ")))
              (setq *TroySpeed~* 1.0)
            );if
            (if (or (< *TroySpeed~* 0.5)(> *TroySpeed~* 5.0))
              (princ "\nThe number must in the range of 0.5 to 5.0.\nThe larger the number the faster the Troys move.")
              (setq Loop nil)
            );if
          );while
        );case
        ((= Settings$ "Colors")
          (initget "Bright Dim Ghost")
          (if (not (setq Colors$ (getkword "\nColor Scheme [<Bright>/Dim/Ghost]: ")))
            (setq Colors$ "Bright")
          );if
          (setq *ColorScheme#*
            (cond
              ((= Colors$ "Bright") 1)
              ((= Colors$ "Dim")    2)
              ((= Colors$ "Ghost")  3)
            );cond
          );setq
        );case
        ((= Settings$ "Defaults")
          (setq *MinTroys#* 5 *MaxTroys#* 10 *TroySpeed~* 1.0 *ColorScheme#* 1)
        );case
      );cond
      (c:Troy)
    );case
    (t (Troy Option$))
  );if
  (princ)
);defun c:Troy
;-------------------------------------------------------------------------------
; Troy - Troy main function
;-------------------------------------------------------------------------------
(defun Troy (Option$ / AddArray: Ang~ AxisPt BuildShip: CenPt ChangeArray: CirAng~
  CirEnt^ CirLimits~ CirPt1 CirPt2 Color1 Color1_5 Color2 Color3 Color4 Color5
  Color6 Color7 Color8 Cnt# Code# Counter# CreateArray: Dia1~ Dia2~ Direction#
  Dist~ Ent^ Ent1^ Ent2^ Flame$ Flame^ FlameArray@ HalfStep~ Inc# Inc1~ Inc2~
  Increase~ Item Limit# Loop MainEnt^ MainList@ MainNum# NorthEast NorthWest
  Nth# Nths@ Num# NumSteps# Offset~ OldDirection# Option$ Passed Pnts# Points#
  Previous@ Pt Pt1 Pt2 Pt3 Pt4 Pt5 Pt6 Pt7 Pt8 Pt9 Pt10 Pt11 Pt12 Radius~ Read@
  Refresh: Rotate~ ShipName$ SouthEast SouthWest SS& StepDist~ SubList@ TextEnt^
  Total# TroyArray@ Unit~ Value ViewExtents@ ViewSize~ Xmin~ Xmax~ Ymin~ Ymax~)
  ;-----------------------------------------------------------------------------
  ; AddArray: - Add new Troy entity specs to the TroyArray@ list
  ; Arguments: 1
  ;   StartPt = Specify starting point or nil
  ; Returns: A list of a new random Troy specs to be added to TroyArray@ list
  ;-----------------------------------------------------------------------------
  (defun AddArray: (StartPt / Ang~ Num#)
    (if StartPt
      (setq CirPt1 StartPt)
      (setq CirPt1 (polar CenPt (* (GetRnd 6283) 0.001) CirLimits~))
    );if
    (setq Num# (GetRnd 8))
    (setq StepDist~;Determines Troys Speed
      (cond;                                   Points Dia Units
        ((= Num# 0)(* Unit~ 0.100 *TroySpeed~*));50     2.0
        ((= Num# 1)(* Unit~ 0.125 *TroySpeed~*));75     2.5
        ((= Num# 2)(* Unit~ 0.150 *TroySpeed~*));100    3.0
        ((= Num# 3)(* Unit~ 0.175 *TroySpeed~*));125    3.5
        ((= Num# 4)(* Unit~ 0.200 *TroySpeed~*));150    4.0
        ((= Num# 5)(* Unit~ 0.225 *TroySpeed~*));175    4.5
        ((= Num# 6)(* Unit~ 0.250 *TroySpeed~*));200    5.0
        ((= Num# 7)(* Unit~ 0.275 *TroySpeed~*));225    5.5
        ((= Num# 8)(* Unit~ 0.300 *TroySpeed~*));250    6.0
      );cond
    );setq
    (setq HalfStep~ (/ StepDist~ 2.0))
    (setq Points# (+ (* Num# 25) 50));50 to 250
    (setq Radius~ (/ (* Unit~ (* 0.1 (+ (+ (* Num# 5) 10) 10))) 2.0))
    (command "_CIRCLE" CirPt1 Radius~)
    (setq Ent1^ (entlast))
    (command "_CHPROP" Ent1^ "" "_C" Color8 "")
    (command "_HATCH" "AR-CONC" (* (getvar "VIEWSIZE") 0.0045) "" Ent1^ "")
    (setq Ent2^ (entlast))
    (command "_CHPROP" Ent2^ "" "_C" Color8 "")
    (command "_-GROUP" "_C" (UniqueName) "" Ent1^ Ent2^ "")
    (setq CirEnt^ (entlast))
    (setq CirAng~ (+ (- (angle CirPt1 CenPt) (dtr 30)) (* (GetRnd 1047) 0.001)))
    (setq CirPt2 (polar CirPt1 CirAng~ StepDist~))
    (setq Offset~ (+ (* Radius~ 2)(* Radius~ (GetRnd 10))))
    (setq Ang~ (atan (/ HalfStep~ Offset~)))
    (setq Pt (polar CirPt1 CirAng~ HalfStep~))
    (if (< CirAng~ (angle CirPt1 CenPt))
      (setq AxisPt (polar Pt (+ CirAng~ (dtr 90)) Offset~) Direction# 1)
      (setq AxisPt (polar Pt (- CirAng~ (dtr 90)) Offset~) Direction# -1)
    );if
    (setq NumSteps# (+ (GetRnd 10) 2))
    (list CirEnt^ CirPt1 CirPt2 AxisPt Radius~ Direction# NumSteps# Points#)
  );defun AddArray:
  ;-----------------------------------------------------------------------------
  ; ChangeArray: - Change or Move entity in the TroyArray@ list
  ; Arguments: 1
  ;   List@ = A sublist within the TroyArray@ list
  ; Returns: Changes or Moves Troy entities in the TroyArray@ list
  ;-----------------------------------------------------------------------------
  (defun ChangeArray: (List@ / Ang~ Num#)
    (setq CirEnt^ (nth 0 List@)
          CirPt1 (nth 1 List@)
          CirPt2 (nth 2 List@)
          AxisPt (nth 3 List@)
          Radius~ (nth 4 List@)
          Direction# (nth 5 List@)
          NumSteps# (nth 6 List@)
          Points# (nth 7 List@)
          StepDist~ (distance CirPt1 CirPt2)
          HalfStep~ (/ StepDist~ 2.0)
          Ang~ (- (* pi 0.5)(acos (/ HalfStep~ (distance AxisPt CirPt2))))
    );setq
    (command "_MOVE" CirEnt^ "" CirPt1 CirPt2)
    (setq NumSteps# (1- NumSteps#))
    (if (= NumSteps# 0)
      (progn
        (setq NumSteps# (+ (GetRnd 10) 2))
        (setq OldDirection# Direction#)
        (setq Num# (GetRnd 10))
        (if (> Num# 5)
          (setq Direction# 1);ccw
          (setq Direction# -1);cw
        );if
        (setq Offset~ (+ (* Radius~ 2)(* Radius~ (GetRnd 10))))
        (if (= OldDirection# 1);ccw
          (if (= Direction# 1);ccw
            (setq AxisPt (polar CirPt2 (angle CirPt2 AxisPt) Offset~))
            (setq AxisPt (polar CirPt2 (angle AxisPt CirPt2) Offset~))
          );if
          (if (= Direction# -1);cw
            (setq AxisPt (polar CirPt2 (angle CirPt2 AxisPt) Offset~))
            (setq AxisPt (polar CirPt2 (angle AxisPt CirPt2) Offset~))
          );if
        );if
        (setq Ang~ (- (* pi 0.5)(acos (/ HalfStep~ Offset~))))
        (if (= Direction# 1);ccw
          (setq Pt (polar AxisPt (+ (angle AxisPt CirPt2) (* Ang~ 2)) (distance AxisPt CirPt2)))
          (setq Pt (polar AxisPt (- (angle AxisPt CirPt2) (* Ang~ 2)) (distance AxisPt CirPt2)))
        );if
        (setq CirPt1 CirPt2 CirPt2 Pt)
      );progn
      (if (= Direction# 1);ccw
        (progn
          (setq Pt (polar AxisPt (+ (angle AxisPt CirPt2) (* Ang~ 2)) (distance AxisPt CirPt2)))
          (setq CirPt1 CirPt2 CirPt2 Pt)
        );progn
        (progn
          (setq Pt (polar AxisPt (- (angle AxisPt CirPt2) (* Ang~ 2)) (distance AxisPt CirPt2)))
          (setq CirPt1 CirPt2 CirPt2 Pt)
        );progn
      );if
    );if
    ;(command "LINE" AxisPt CirPt1 ""); Uncomment to see Troys paths while debuging
    ;If you're tweaking or debugging this code, you've got to uncommend the above line
    ;at least once to see these patterns. Run Troy in the Intro or Play mode for about
    ;10 seconds then press the escape key to abruptly abort the game. Then turn off
    ;all layers except for the Troy layer, and do a zoom extents and print it.
    (list CirEnt^ CirPt1 CirPt2 AxisPt Radius~ Direction# NumSteps# Points#)
  );defun ChangeArray:
  ;-----------------------------------------------------------------------------
  ; CreateArray: - Creates the initial TroyArray@ list
  ; Arguments: 1
  ;   TowardCenter = 1 for toward center, else away from center
  ; Returns: Creates the initial TroyArray@ list moving in direction specified.
  ;-----------------------------------------------------------------------------
  (defun CreateArray: (TowardCenter)
    (setq TroyArray@ nil)
    (if (= TowardCenter 1)
      (progn
        (setq Rotate~ (* (GetRnd 6283) 0.001))
        (repeat 10
          (setq TroyArray@ (append TroyArray@ (list (AddArray: (polar CenPt Rotate~ CirLimits~)))))
          (setq Rotate~ (+ Rotate~ (/ pi 5.0)))
        );repeat
      );progn
      (progn
        (setq Rotate~ (* (GetRnd 6283) 0.001)
              Dist~ (/ (distance NorthWest NorthEast) 7)
              Increase~ (/ (* Dist~ 3) 20.0)
        );setq
        (repeat 10
          (setq Pt (polar CenPt Rotate~ Dist~))
          (setq List@ (AddArray: Pt))
          (setq List@ (Switch_nth 1 2 List@))
          (setq List@ (Change_nth 5 (* (nth 5 List@) -1) List@))
          (setq TroyArray@ (append TroyArray@ (list List@)))
          (setq Rotate~ (+ Rotate~ 0.897 (* (GetRnd 359) 0.001))
                Dist~ (+ Dist~ Increase~)
          );setq
        );repeat
      );progn
    );if
  );defun CreateArray:
  ;-----------------------------------------------------------------------------
  ; BuildShip: - Draws Ships
  ; Arguments: 2
  ;   Num# = The number of ship created in the function BuildShip:
  ;   InsPt = Insertion base point of the ship
  ; Returns: Draws and makes a block of ship at the insertion point specified.
  ; Also creates the variables MainEnt^ and MainList@ of the ships specs.
  ;-----------------------------------------------------------------------------
  (defun BuildShip: (Num# InsPt / SS&)
    (if (not (member Num# (list 0 1 2 3)))(setq Num# 1))
    (cond
      ((= Num# 0);Red Ship in Intro
        (setq Pt1 (polar InsPt (dtr 90) (* Unit~ 0.5))
              Pt1 (polar Pt1 pi (* Unit~ 0.875))
              Pt2 (polar Pt1 pi (* Unit~ 0.375))
              Pt2 (polar Pt2 (dtr 270) (* Unit~ 0.125))
              Pt3 (polar Pt2 pi (* Unit~ 0.25))
              Pt3 (polar Pt3 (dtr 270) (* Unit~ 0.125))
              Pt4 (polar Pt3 (dtr 270) (* Unit~ 0.75))
              Pt4 (polar Pt4 0 (* Unit~ 0.5))
              Pt5 (polar Pt4 0 (* Unit~ 1.25))
              Pt5 (polar Pt5 (dtr 270) (* Unit~ 0.5))
              Pt6 (polar InsPt 0 (* Unit~ 2.5))
              Pt7 (polar Pt6 (dtr 90) (* Unit~ 0.5))
              Pt7 (polar Pt7 pi Unit~)
              Pt8 (polar Pt7 pi (* Unit~ 0.5))
              Pt8 (polar Pt8 (dtr 90) (* Unit~ 0.125))
              Pt9 (polar Pt3 0 (* Unit~ 0.5))
              Pt10 (polar InsPt (dtr 270) (* Unit~ 0.25))
              Pt11 (polar Pt9 0 (* Unit~ 2.25))
              Pt12 (polar InsPt (dtr 90) Unit~)
        );setq
        (setq SS& (ssadd))
        (command "_COLOR" Color1);Red
        (command "_ARC" Pt1 Pt2 Pt3)(ssadd (entlast) SS&)
        (command "_ARC" Pt3 Pt4 Pt5)(ssadd (entlast) SS&)
        (command "_ARC" "" Pt6)(ssadd (entlast) SS&)
        (command "_ARC" Pt6 Pt7 Pt8)(ssadd (entlast) SS&)
        (command "_COLOR" Color4);Cyan
        (command "_ARC" Pt9 Pt10 Pt11)(ssadd (entlast) SS&)
        (command "_ARC" Pt11 Pt12 Pt9)(ssadd (entlast) SS&)
        (command "_COLOR" "_BYLAYER")
        (setq ShipName$ (UniqueName))
        (command "_BLOCK" ShipName$ InsPt SS& "")
        (command "_INSERT" ShipName$ InsPt 1 1 0)
        (setq MainEnt^ (entlast))
        (setq MainList@ (entget MainEnt^))
      );case
      ((= Num# 1);Green Ship
        (setq Pt (polar InsPt pi Unit~) Pt (polar Pt (dtr 90) (* Unit~ 0.5)))
        (command "_PLINE" Pt (polar Pt (dtr 270) Unit~) (polar InsPt 0 (* Unit~ 2)) "_C")
        (command "_CHPROP" "_L" "" "_C" Color3 "");Green
        (setq ShipName$ (UniqueName))
        (command "_BLOCK" ShipName$ InsPt "_L" "")
        (command "_INSERT" ShipName$ InsPt 1 1 0)
        (setq MainEnt^ (entlast))
        (setq MainList@ (entget MainEnt^))
      );case
      ((= Num# 2);Cyan Ship
        (setq Pt (polar InsPt pi Unit~) Pt1 (polar Pt (dtr 270) Unit~)
              Pt4 (polar Pt1 (dtr 90) (* Unit~ 2)) Pt (polar InsPt 0 Unit~)
              Pt2 (polar Pt (dtr 270) (* Unit~ 0.5)) Pt3 (polar Pt2 (dtr 90) Unit~)
        );setq
        (command "_PLINE" (polar InsPt pi (* Unit~ 0.5)) Pt1 (polar InsPt (dtr 270) (* Unit~ 0.5))
          Pt2 (polar InsPt 0 (* Unit~ 2)) Pt3 (polar InsPt (dtr 90) (* Unit~ 0.5)) Pt4 "_C"
        );command
        (command "_CHPROP" "_L" "" "_C" Color4 "");Cyan
        (setq ShipName$ (UniqueName))
        (command "_BLOCK" ShipName$ InsPt "_L" "")
        (command "_INSERT" ShipName$ InsPt 1 1 0)
        (setq MainEnt^ (entlast))
        (setq MainList@ (entget MainEnt^))
      );case
      ((= Num# 3);Magenta Ship
        (setq Pt (polar InsPt pi Unit~) Pt1 (polar Pt (dtr 270) (* Unit~ 0.5))
              Pt4 (polar Pt1 (dtr 90) Unit~) Pt2 (polar Pt1 0 (* Unit~ 1.5))
              Pt3 (polar Pt4 0 (* Unit~ 1.5))
        );setq
        (command "_PLINE" InsPt Pt1 (polar InsPt (dtr 270) Unit~) Pt2
          (polar InsPt 0 (* Unit~ 2)) Pt3 (polar InsPt (dtr 90) Unit~) Pt4 "_C"
        );command
        (command "_CHPROP" "_L" "" "_C" Color6 "");Magenta
        (setq ShipName$ (UniqueName))
        (command "_BLOCK" ShipName$ InsPt "_L" "")
        (command "_INSERT" ShipName$ InsPt 1 1 0)
        (setq MainEnt^ (entlast))
        (setq MainList@ (entget MainEnt^))
      );case
    );cond
    (princ)
  );defun BuildShip:
  ;-----------------------------------------------------------------------------
  ; Refresh: - Erases Troy entities and creates a new TroyArray@ list
  ;-----------------------------------------------------------------------------
  (defun Refresh: ()
    (setq SS& (ssget "_x" (list '(8 . "Troy"))))
    (command "_ERASE" SS& "")
    (setq FlameArray@ nil TroyArray@ nil Counter# 0 MainNum# (1+ MainNum#))
    (CreateArray: (GetRnd 1))
    (princ)
  );defun Refresh:
  ;=============================================================================
  ; Start of Main Function
  ;=============================================================================
  (if (not *MinTroys#*) (setq *MinTroys#* 5))
  (if (not *MaxTroys#*) (setq *MaxTroys#* 10))
  (if (not *TroySpeed~*) (setq *TroySpeed~* 1.0))
  (if (not *ColorScheme#*) (setq *ColorScheme#* 1))
  (if (not *Speed#) (Speed))
  (if (not *Clayer$*) (setq *Clayer$* (getvar "CLAYER")))
  (if (not *Osmode#*) (setq *Osmode#* (getvar "OSMODE")))
  (if (not *TextStyle$*) (setq *TextStyle$* (getvar "TEXTSTYLE")))
  (if (not *TextSize~*) (setq *TextSize~* (getvar "TEXTSIZE")))
  (setvar "BLIPMODE" 0)(setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)(setvar "GRIDMODE" 0)(graphscr)
  (if (>= (atoi (getvar "ACADVER")) 15)
    (progn
      (if (not *CTab$*) (setq *CTab$* (getvar "CTAB")))
      (if (/= (getvar "CTAB") "Model")
        (progn
          (command "_PSPACE")
          (if (setq SS& (ssget "_x" (list '(-4 . "<AND")'(0 . "VIEWPORT")(cons 410 (getvar "CTAB"))'(-4 . "AND>"))))
            (if (> (sslength SS&) 1)
              (command "_LAYOUT" "_S" "Model")
            );if
          );if
        );progn
      );if
      (setq *TroyTab$* (getvar "CTAB"))
    );progn
  );if
  (if (tblsearch "LAYER" "Troy")
    (command "_LAYER" "_T" "Troy" "_U" "Troy" "_ON" "Troy" "_M" "Troy" "")
    (command "_LAYER" "_M" "Troy" "")
  );if
  (if (setq SS& (ssget "_x" (list '(8 . "Troy"))))
    (command "_ERASE" SS& "")
  );if
  (setq ViewExtents@ (ViewExtents))
  (command "_ZOOM" "_W" (car ViewExtents@)(cadr ViewExtents@))
  (setq Xmin~ (car (nth 0 ViewExtents@))
        Ymax~ (cadr (nth 0 ViewExtents@))
        Xmax~ (car (nth 1 ViewExtents@))
        Ymin~ (cadr (nth 1 ViewExtents@))
        NorthWest (car ViewExtents@)
        SouthEast (cadr ViewExtents@)
        SouthWest (list Xmin~ Ymin~)
        NorthEast (list Xmax~ Ymax~)
        CenPt (getvar "VIEWCTR")
        ViewSize~ (getvar "VIEWSIZE")
        Unit~ (/ (getvar "VIEWSIZE") 50.0)
        Limit# (1+ (fix (/ (distance CenPt (car ViewExtents@)) Unit~)))
        CirLimits~ (* (+ Limit# 3) Unit~)
        North (polar CenPt (dtr 90) (+ (* Unit~ 3) (/ (getvar "VIEWSIZE") 2.0)))
        South (polar CenPt (dtr 270) (+ (* Unit~ 3) (/ (getvar "VIEWSIZE") 2.0)))
        East (polar CenPt 0 (+ (* Unit~ 3) (/ (distance NorthWest NorthEast) 2.0)))
        West (polar CenPt pi (+ (* Unit~ 3) (/ (distance NorthWest NorthEast) 2.0)))
  );setq
  ; Customize Color Schemes as desired and add to top menu in c:Troy
  (cond
    ((= *ColorScheme#* 1);  Bright colors
      (setq Color1     1 ;Red      Red ship
            Color1_5  30 ;Orange   Exploding Troys
            Color2     2 ;Yellow   Bonus points
            Color3     3 ;Green    1st ship
            Color4     4 ;Cyan     2nd ship
            Color5     5 ;Blue     Letter O in TroyIntro
            Color6     6 ;Magenta  3rd ship
            Color7     7 ;White    Not used
            Color8    33 ;Grey     Troys
      );setq
    );case
    ((= *ColorScheme#* 2);  Dim colors
      (setq Color1    12 ;Red      Red ship
            Color1_5  32 ;Orange   Exploding Troys
            Color2    52 ;Yellow   Bonus points
            Color3    86 ;Green    1st ship
            Color4   152 ;Cyan     2nd ship
            Color5   162 ;Blue     Letter O in TroyIntro
            Color6   192 ;Magenta  3rd ship
            Color7     7 ;White    Not used
            Color8   250 ;Grey     Troys
      );setq
    );case
    ((= *ColorScheme#* 3);  Ghost colors
      (setq Color1   250 ;Red      Red ship
            Color1_5 250 ;Orange   Exploding Troys
            Color2   250 ;Yellow   Bonus points
            Color3   250 ;Green    1st ship
            Color4   250 ;Cyan     2nd ship
            Color5   250 ;Blue     Letter O in TroyIntro
            Color6   250 ;Magenta  3rd ship
            Color7   250 ;White    Not used
            Color8   250 ;Grey     Troys
      );setq
    );case
  );cond
  ; Create Flame$ block
  (setq SS& (ssadd))(setq Pt SouthEast)
  (command "_COLOR" Color2);Yellow
  (command "_LINE" Pt (setq Pt (polar Pt 0 Unit~)) "")(ssadd (entlast) SS&)
  (command "_COLOR" Color1);Red
  (command "_LINE" Pt (setq Pt (polar Pt 0 Unit~)) "")(ssadd (entlast) SS&)
  (command "_COLOR" "_BYLAYER")(setq Flame$ (UniqueName))
  (command "_BLOCK" Flame$ SouthEast SS& "")
  (if (= Option$ "Intro")(TroyIntro))
  ;(command "RECTANG" (car ViewExtents@)(cadr ViewExtents@)); Uncomment while debuging
  ;(command "CIRCLE" CenPt CirLimits~); Uncomment while debuging
  ; Build Ship 1
  (BuildShip: 1 CenPt)
  ; Create first Troys
  (CreateArray: (GetRnd 1))
  (command "_STYLE" "Troy" "ROMANS" "0.0" "0.75" "" "" "" "")
  ;-----------------------------------------------------------------------------
  ; Start of grread Loop
  ;-----------------------------------------------------------------------------
  (setq Loop t Counter# 0 Total# 100 MainNum# 1)
  (setq Previous@ (list 5 (polar CenPt 0 Unit~)));Start the Loop moving
  (princ (strcat "\nCommand:\nTotal: " (itoa Total#) "\n"))
  (while Loop
    ; Read the mouse movements and picks
    (if (not (setq Read@ (grread t 8)))
      (setq Read@ Previous@)
    );if
    (setq Code# (nth 0 Read@))
    (setq Value (nth 1 Read@))
    (cond
      ((= Code# 3); Fire if picked
        (setq Ang~ (angle CenPt Value)
              Pt1 (polar CenPt Ang~ (* Unit~ 2))
              Pt2 (polar Pt1 Ang~ Unit~)
        );setq
        (command "_INSERT" Flame$ Pt1 1 1 (rtd Ang~))
        (setq FlameArray@ (append FlameArray@ (list (list (entlast) Pt1 Pt2 Ang~))))
        (setq Total# (1- Total#))
        (princ (strcat "\nCommand:\nTotal: " (itoa Total#) "\n"))
      );case
      ((= Code# 5); Rotate if moved
        (setq Previous@ Read@)
        (setq Ang~ (angle CenPt Value))
        (setq MainList@ (entmod (subst (cons 50 Ang~) (assoc 50 MainList@) MainList@)))
      );case
      ((= Code# 2); Key was pressed
        (cond
          ((or (= Value 80)(= Value 112));P or p then pause
            (getpoint "\nTroy paused.  Pick mouse to continue. ")
            (princ (strcat "\nCommand:\nTotal: " (itoa Total#) "\n"))
          );case
          ((or (= Value 81)(= Value 113));Q or q then quit
            (setq Loop nil)
          );case
          (t (princ "\nMove mouse to rotate ship, pick mouse to fire, press P to Pause, or Q to quit.")
             (princ (strcat "\nTotal: " (itoa Total#) "\n"))
          );case
        );case
      );case
    );cond
    ; Move flame objects
    (if FlameArray@
      (progn
        (setq Cnt# 0 Nths@ nil)
        (foreach List@ FlameArray@
          (setq Flame^ (nth 0 List@)
                Pt1 (nth 1 List@)
                Pt2 (nth 2 List@)
                Ang~ (nth 3 List@)
          );setq
          (if (or (and (> (car Pt2)(car East))(> (car Pt2)(car Pt1)))
                  (and (< (car Pt2)(car West))(< (car Pt2)(car Pt1)))
                  (and (> (cadr Pt2)(cadr North))(> (cadr Pt2)(cadr Pt1)))
                  (and (< (cadr Pt2)(cadr South))(< (cadr Pt2)(cadr Pt1)))
              );or
            (progn
              (command "_ERASE" Flame^ "")
              (setq Nths@ (append Nths@ (list Cnt#)))
            );progn
            (progn
              (command "_MOVE" Flame^ "" Pt1 Pt2)
              (setq Pt1 Pt2 Pt2 (polar Pt2 Ang~ Unit~))
              (setq List@ (list Flame^ Pt1 Pt2 Ang~))
              (setq FlameArray@ (Change_nth Cnt# List@ FlameArray@))
            );progn
          );if
          (setq Cnt# (1+ Cnt#))
        );foreach
        (if Nths@
          (setq FlameArray@ (Remove_nths Nths@ FlameArray@))
        );if
      );progn
    );if
    ; Check if Troys are hit
    (setq Cnt# 0 Nths@ nil)
    (foreach List@ TroyArray@ ; Troy list
      (if FlameArray@ ; Flame list
        (progn
          (setq CirEnt^ (nth 0 List@)
                CirPt1 (nth 1 List@)
                Radius~ (nth 4 List@)
                Points# (nth 7 List@)
          );setq
          (setq Num# 0 Num@ nil)
          (foreach SubList@ FlameArray@
            (setq Flame^ (nth 0 SubList@)
                  Pt2 (nth 2 SubList@)
            );setq
            (if (and (> (car Pt2) (+ Xmin~ Radius~))(< (car Pt2) (- Xmax~ Radius~))
                     (> (cadr Pt2) (+ Ymin~ Radius~))(< (cadr Pt2) (- Ymax~ Radius~)))
              (if (<= (distance Pt2 CirPt1) Radius~)
                (progn
                  (command "_ERASE" CirEnt^ Flame^ "")
                  (setq Num@ (append Num@ (list Num#)))
                  (setq Nths@ (append Nths@ (list Cnt#)))
                  (command "_TEXT" "_M" CirPt1 Unit~ 0 (itoa Points#))
                  (command "_CHPROP" "_L" "" "_C" Color2 "")
                  (setq TextEnt^ (entlast))
                  (setq Total# (+ Total# Points#))
                  (princ (strcat "\nCommand:\nTotal: " (itoa Total#) "\n"))
                  (command "_COLOR" Color1_5)
                  (setq Dia1~ (* Radius~ 2) Dia2~ (* Radius~ 3) Ang~ (dtr 270) Pnts# 7)
                  (repeat 3
                    (StarBurst CirPt1 Dia1~ Dia2~ Pnts# Ang~)(delay 0.125)
                    (setq Dia2~ (* Radius~ 3.5) Ang~ (+ Ang~ (/ (* pi 2) (* Pnts# 3))))
                    (command "_ERASE" (entlast) "")
                    (StarBurst CirPt1 Dia1~ Dia2~ Pnts# Ang~)(delay 0.125)
                    (setq Dia2~ (* Radius~ 3) Ang~ (dtr 90))
                    (command "_ERASE" (entlast) "")
                    (StarBurst CirPt1 Dia1~ Dia2~ Pnts# Ang~)(delay 0.125)
                    (setq Dia2~ (* Radius~ 3.5) Ang~ (- Ang~ (/ (* pi 2) (* Pnts# 3))))
                    (command "_ERASE" (entlast) "")
                    (StarBurst CirPt1 Dia1~ Dia2~ Pnts# Ang~)(delay 0.125)
                    (setq Dia2~ (* Radius~ 3) Ang~ (dtr 270))
                    (command "_ERASE" (entlast) "")
                    (StarBurst CirPt1 Dia1~ Dia2~ Pnts# Ang~)(delay 0.125)
                    (setq Dia2~ (* Radius~ 3.5) Ang~ (- Ang~ (/ (* pi 2) (* Pnts# 3))))
                    (command "_ERASE" (entlast) "")
                    (StarBurst CirPt1 Dia1~ Dia2~ Pnts# Ang~)(delay 0.125)
                    (setq Dia2~ (* Radius~ 3) Ang~ (dtr 90))
                    (command "_ERASE" (entlast) "")
                    (StarBurst CirPt1 Dia1~ Dia2~ Pnts# Ang~)(delay 0.125)
                    (setq Dia2~ (* Radius~ 3.5) Ang~ (+ Ang~ (/ (* pi 2) (* Pnts# 3))))
                    (command "_ERASE" (entlast) "")
                    (StarBurst CirPt1 Dia1~ Dia2~ Pnts# Ang~)(delay 0.125)
                    (setq Dia2~ (* Radius~ 3) Ang~ (dtr 270))
                    (command "_ERASE" (entlast) "")
                  );repeat
                  (command "_COLOR" "_BYLAYER")
                  (command "_ERASE" TextEnt^"")
                );progn
              );if
            );if
            (setq Num# (1+ Num#))
          );foreach
          (if Num@
            (setq FlameArray@ (Remove_nths Num@ FlameArray@))
          );if
        );progn
      );if
      (if TroyArray@
        (setq TroyArray@ (Change_nth Cnt# (ChangeArray: List@) TroyArray@))
        (CreateArray: 1)
      );if
      (setq Cnt# (1+ Cnt#))
    );foreach
    (if Nths@
      (setq TroyArray@ (Remove_nths Nths@ TroyArray@))
    );if
    (if (not TroyArray@)
      (CreateArray: 1)
    );if
    ; Erase Troys that are out of limits
    (setq Cnt# 0)
    (foreach List@ TroyArray@
      (setq CirEnt^ (nth 0 List@)
            CirPt1 (nth 1 List@)
            CirPt2 (nth 2 List@)
      );setq
      (if (or (and (> (car CirPt1)(car East))(> (car CirPt2)(car CirPt1)))
              (and (< (car CirPt1)(car West))(< (car CirPt2)(car CirPt1)))
              (and (> (cadr CirPt1)(cadr North))(> (cadr CirPt2)(cadr CirPt1)))
              (and (< (cadr CirPt1)(cadr South))(< (cadr CirPt2)(cadr CirPt1)))
          );or
        (progn
          (command "_ERASE" CirEnt^ "")
          (setq TroyArray@ (Change_nth Cnt# (AddArray: nil) TroyArray@))
          (setq Counter# (1+ Counter#))
          (if (= Counter# 3);Add Troys per Counter#
            (progn
              (setq Counter# 0)
              (if (< (length TroyArray@) *MaxTroys#*)
                (setq TroyArray@ (append TroyArray@ (list (AddArray: nil))))
              );if
            );progn
          );if
        );progn
      );if
      (setq Cnt# (1+ Cnt#))
    );foreach
    ; Check if Troys ran into Ship or total points is <= 0
    (setq Cnt# 0 Passed t)
    (while Passed
      (setq List@ (nth Cnt# TroyArray@)
            CirEnt^ (nth 0 List@)
            CirPt1 (nth 1 List@)
            Radius~ (nth 4 List@)
      );setq
      (if (or (< (distance CenPt CirPt1) (+ Radius~ (* Unit~ 2.5))) (<= Total# 0))
        (progn
          (command "_ERASE" MainEnt^ "")
          (cond
            ((= MainNum# 1)(setq Color# Color3));Green
            ((= MainNum# 2)(setq Color# Color4));Cyan
            ((= MainNum# 3)(setq Color# Color6));Magenta
          );cond
          (command "_COLOR" Color#)
          (setq Dia1~ 1 Dia2~ 4 Ang~ (dtr 270) Inc# 0 Inc1~ 0.125 Inc2~ 0.375)
          (repeat 20
            (if (= Inc# 11)(setq Inc1~ -0.125 Inc2~ -0.375))
            (StarBurst CenPt (* Unit~ Dia1~) (* Unit~ Dia2~) 5 Ang~)(delay 0.5)
            (setq Dia1~ (+ Dia1~ Inc1~) Dia2~ (+ Dia2~ Inc2~))
            (setq Ang~ (+ Ang~ (/ (* pi 2) 3)))
            (command "_ERASE" (entlast) "")
            (setq Inc# (1+ Inc#))
          );repeat
          (command "_COLOR" "_BYLAYER")
          (setq Total# (- Total# 10))
          (if (<= Total# 0)
            (progn
              (setq MainNum# 3)
              (princ "\nCommand:\nTotal: 0")
            );progn
            (princ (strcat "\nCommand:\nTotal: " (itoa Total#) "\n"))
          );if
          (cond
            ((= MainNum# 1); Build Ship 2
              (Refresh:)
              (BuildShip: 2 CenPt)
            );case
            ((= MainNum# 2); Build Ship 3
              (Refresh:)
              (BuildShip: 3 CenPt)
            );case
            ((= MainNum# 3); Finished!
              (setq Passed nil Loop nil)
            );case
          );cond
          (setq Passed nil)
        );progn
      );if
      (setq Cnt# (1+ Cnt#))
      (if (> Cnt# (1- (length TroyArray@)))
        (setq Passed nil)
      );if
    );while
    (if (< (length TroyArray@) *MinTroys#*)
      (setq TroyArray@ (append TroyArray@ (list (AddArray: nil))))
    );if
    (if (or (/= (getvar "VIEWCTR") CenPt)(/= (getvar "VIEWSIZE") ViewSize~))
      (command "_ZOOM" "_W" (car ViewExtents@)(cadr ViewExtents@))
    );if
  );while
  (TroyClear)
  (princ (strcat "\nCommand:\nTotal: " (itoa Total#) " Finished!"))
  (princ)
);defun Troy
;-------------------------------------------------------------------------------
; TroyIntro - Introduction
;-------------------------------------------------------------------------------
(defun TroyIntro (/ Color# Divisions# Fire# Fourth# Inc~ Increase~ Ltr# Move#
  O-Ang~ O-Cnt# O-Ent^ O-Ins O-List@ O-Pt O-Pts@ O-Size~ Path# Path@ Path1@
  Path2@ Path3@ Path4@ R-Ang~ R-Cen R-Cnt# R-Ent^ R-Ins R-List@ R-Pt R-Pts@
  R-Size~ Rotate~ Rnd# RndLtr@ Sevenths Step~ T-Ang~ T-Cen T-Cnt# T-Ent^ T-Ins
  T-List@ T-Pt T-Pts@ T-Size~ Tl-Ang~ TxSize~ TxSizeInc~ TxSizeMax~ TxSizeMin~
  Y-Ang~ Y-Cnt# Y-Ent^ Y-Ins Y-List@ Y-Pt Y-Pts@ Y-Size~)
  (princ "\nTroy Intro.\n")
  (command "_STYLE" "Troy" "ROMAND" "0.0" "1" "" "" "" "")
  (setq T-Pt (polar CenPt pi (* Unit~ 4.5))
        R-Pt (polar CenPt pi (* Unit~ 1.5))
        O-Pt (polar CenPt 0 (* Unit~ 1.5))
        Y-Pt (polar CenPt 0 (* Unit~ 4.5))
        TxSizeMax~ (* Unit~ 3)
        TxSizeMin~ (* Unit~ 0.5)
        Inc~ (* Unit~ 2);Speed of letters
        Pt0 (polar R-Pt (- (angle R-Pt SouthWest) 0.009) (distance R-Pt SouthWest))
        Pt (polar R-Pt (angle R-Pt Pt0) (/ (distance R-Pt Pt0) 2.0))
        Pt (polar Pt (+ (angle R-Pt Pt0) (* pi 0.5)) (/ (distance R-Pt Pt0) 7.0))
        R-Cen (Center3Pt R-Pt Pt Pt0)
        Radius~ (distance R-Pt R-Cen)
        Ang~ (* (- (* pi 0.5) (acos (/ (/ Inc~ 2.0) Radius~))) 2)
        Inc# (fix (/ (- (angle R-Cen R-Pt) (angle R-Cen SouthWest)) Ang~))
        Pt0 (polar T-Pt (- (angle T-Pt NorthWest) 0.043) (distance R-Pt SouthWest))
        Pt (polar T-Pt (angle T-Pt Pt0) (/ (distance R-Pt Pt0) 2.0))
        Pt (polar Pt (+ (angle T-Pt Pt0) (* pi 0.5)) (/ (distance R-Pt Pt0) 7.0))
        T-Cen (Center3Pt T-Pt Pt Pt0)
        TxSizeInc~ (/ (- TxSizeMax~ TxSizeMin~) (float Inc#))
        TxSize~ TxSizeMax~
        T-Pts@ (list T-Pt)
        R-Pts@ (list R-Pt)
        O-Pts@ (list O-Pt)
        Y-Pts@ (list Y-Pt)
        T-Ang~ 0
  );setq
  (repeat Inc#
    (setq T-Pt (polar T-Cen (- (angle T-Cen T-Pt) Ang~) Radius~)
          T-Pts@ (append T-Pts@ (list T-Pt))
          R-Pt (polar R-Cen (- (angle R-Cen R-Pt) Ang~) Radius~)
          R-Pts@ (append R-Pts@ (list R-Pt))
          O-Pt (polar CenPt (angle R-Pt CenPt) (distance R-Pt CenPt))
          O-Pts@ (append O-Pts@ (list O-Pt))
          Y-Pt (polar CenPt (angle T-Pt CenPt) (distance T-Pt CenPt))
          Y-Pts@ (append Y-Pts@ (list Y-Pt))
          T-Ang~ (- T-Ang~ (dtr 30))
          TxSize~ (- TxSize~ TxSizeInc~)
    );setq
  );repeat
  (setq T-Pts@ (reverse T-Pts@)
        R-Pts@ (reverse R-Pts@)
        O-Pts@ (reverse O-Pts@)
        Y-Pts@ (reverse Y-Pts@)
        R-Ang~ T-Ang~ O-Ang~ T-Ang~ Y-Ang~ T-Ang~
        T-Size~ TxSize~ R-Size~ TxSize~ O-Size~ TxSize~ Y-Size~ TxSize~
        T-Cnt# 0 R-Cnt# 0 O-Cnt# 0 Y-Cnt# 0 Fourth# (/ Inc# 4)
  );setq
  (setq T-Pt (last T-Pts@) R-Pt (last R-Pts@) O-Pt (last O-Pts@) Y-Pt (last Y-Pts@) RndLtr@ (list 0))
  (while (/= (length RndLtr@) 5)
    (setq Rnd# (1+ (GetRnd 3)))
    (cond
      ((= Rnd# 1)(setq Pt T-Pt))
      ((= Rnd# 2)(setq Pt R-Pt))
      ((= Rnd# 3)(setq Pt O-Pt))
      ((= Rnd# 4)(setq Pt Y-Pt))
    );cond
    (if (not (member Pt RndLtr@))
      (setq RndLtr@ (append RndLtr@ (list Pt)))
    );if
  );while
  (setq Rotate~ (* (GetRnd 6283) 0.001)
        Dist~ (/ (distance NorthWest NorthEast) 10)
        Increase~ (/ (* Dist~ 3) 20.0)
  );setq
  (repeat 20
    (setq Pt (polar CenPt Rotate~ Dist~))
    (setq List@ (AddArray: Pt))
    (setq List@ (Switch_nth 1 2 List@))
    (setq List@ (Change_nth 5 (* (nth 5 List@) -1) List@))
    (setq TroyArray@ (append TroyArray@ (list List@)))
    (setq Rotate~ (+ Rotate~ 0.897 (* (GetRnd 359) 0.001))
          Dist~ (+ Dist~ Increase~)
    );setq
  );repeat
  (setq Step~ (* Unit~ 1.5);Speed of red ship
        Pt1 (polar SouthWest (dtr 90) (/ (distance SouthWest NorthWest) 6.0))
        Pt2 (polar Pt1 0 (/ (distance SouthWest SouthEast) 3.0))
        Pt (polar Pt1 0 (/ (distance Pt1 Pt2) 2.0))
        Pt (polar Pt (dtr 90) (* Unit~ 2))
        Pt (Center3Pt Pt1 Pt Pt2)
        Radius~ (distance Pt Pt1)
        Tl-Ang~ (- (angle Pt Pt1) (angle Pt Pt2))
        Ang~ (* 2 (- (* pi 0.5) (acos (/ (* Step~ 0.5) Radius~))))
        Divisions# (fix (1+ (/ Tl-Ang~ Ang~)))
        Pt2 (polar Pt (- (angle Pt Pt1) (* Ang~ Divisions#)) Radius~)
  );setq
  (setq Path1@ (list Pt1))
  (repeat Divisions#
    (setq Pt1 (polar Pt (- (angle Pt Pt1) Ang~) Radius~))
    (setq Path1@ (append Path1@ (list Pt1)))
  );repeat
  (setq Pt (polar Pt (angle Pt Pt2) (* Radius~ 2)))
  (repeat (fix (1+ (/ Divisions# 2.0)))
    (setq Pt1 (polar Pt (+ (angle Pt Pt1) Ang~) Radius~))
    (if (< (angle Pt Pt1) (dtr 270))
      (setq Path1@ (append Path1@ (list Pt1)))
    );if
  );repeat
  (setq Pt1 (last Path1@)
        Pt2 (inters Pt1 (polar Pt1 0 Unit~) NorthEast SouthEast nil)
        Ang~ (atan (/ 1 2.0))
        Radius~ (* (distance Pt1 Pt2) (tan Ang~))
        Pt (polar Pt1 (dtr 90) Radius~)
        Tl-Ang~ (atan (/ (distance Pt1 Pt2) Radius~))
        Ang~ (* 2 (- (* pi 0.5) (acos (/ (* Step~ 0.5) Radius~))))
        Divisions# (fix (1+ (/ Tl-Ang~ Ang~)))
  );setq
  (repeat Divisions#
    (setq Pt1 (polar Pt (+ (angle Pt Pt1) Ang~) Radius~))
    (setq Path1@ (append Path1@ (list Pt1)))
  );repeat
  (setq Pt Pt2
        Radius~ (distance Pt Pt1)
        Ang~ (* 2 (- (* pi 0.5) (acos (/ (* Step~ 0.5) Radius~))))
        Tl-Ang~ (- (angle Pt Pt1) (* pi 0.5))
        Divisions# (fix (1+ (/ Tl-Ang~ Ang~)))
  );setq
  (repeat Divisions#
    (setq Pt2 Pt1)
    (setq Pt1 (polar Pt (- (angle Pt Pt1) Ang~) Radius~))
    (if (> (angle Pt Pt1) (* pi 0.5))
      (setq Path1@ (append Path1@ (list Pt1)))
    );if
  );repeat
  (setq Ang~ (angle Pt2 Pt1))
  (repeat 5
    (setq Pt1 (polar Pt1 Ang~ Step~))
    (setq Path1@ (append Path1@ (list Pt1)))
  );repeat
  (setq Ang~ (angle (nth 1 Path1@) (nth 0 Path1@)))
  (repeat 5
    (setq Pt (polar (nth 0 Path1@) Ang~ Step~))
    (setq Path1@ (Insert_nth 0 Pt Path1@))
  );repeat
  (foreach Item Path1@
    (setq Pt2 (MirrorPt Item CenPt 0))
    (setq Path2@ (append Path2@ (list Pt2)))
    (setq Pt3 (MirrorPt Item CenPt (dtr 90)))
    (setq Path3@ (append Path3@ (list Pt3)))
    (setq Pt4 (MirrorPt Pt3 CenPt 0))
    (setq Path4@ (append Path4@ (list Pt4)))
  );foreach
  (setq Path# (1+ (GetRnd 3)))
  (cond
    ((= Path# 1)(setq Path@ Path1@))
    ((= Path# 2)(setq Path@ Path2@))
    ((= Path# 3)(setq Path@ Path3@))
    ((= Path# 4)(setq Path@ Path4@))
  );cond
  ;-----------------------------------------------------------------------------
  ; First Loop
  ;-----------------------------------------------------------------------------
  (setq Loop t)
  (while Loop
    (if (<= T-Cnt# Inc#)
      (if (= T-Cnt# 0)
        (progn
          (command "_TEXT" "_M" (nth T-Cnt# T-Pts@) T-Size~ (rtd T-Ang~) "T")
          (setq T-Ent^ (entlast))
          (command "_CHPROP" T-Ent^ "" "_C" Color3 "");Green
          (setq T-List@ (entget T-Ent^)
                T-Size~ (+ T-Size~ TxSizeInc~)
                T-Ang~ (+ T-Ang~ (dtr 30))
                T-Cnt# (1+ T-Cnt#)
                T-Ins (nth T-Cnt# T-Pts@)
          );setq
        );progn
        (progn
          (setq T-List@ (entmod (subst (cons 50 T-Ang~) (assoc 50 T-List@) T-List@)))
          (setq T-List@ (entmod (subst (cons 11 T-Ins) (assoc 11 T-List@) T-List@)))
          (setq T-List@ (entmod (subst (cons 40 T-Size~) (assoc 40 T-List@) T-List@)))
          (setq T-Size~ (+ T-Size~ TxSizeInc~)
                T-Ang~ (+ T-Ang~ (dtr 30))
                T-Cnt# (1+ T-Cnt#)
          );setq
          (if (<= T-Cnt# Inc#) (setq T-Ins (nth T-Cnt# T-Pts@)))
        );progn
      );if
    );if
    (if (>= T-Cnt# Fourth#)
      (if (<= R-Cnt# Inc#)
        (if (= R-Cnt# 0)
          (progn
            (command "_TEXT" "_M" (nth R-Cnt# R-Pts@) R-Size~ (rtd R-Ang~) "R")
            (setq R-Ent^ (entlast))
            (command "_CHPROP" R-Ent^ "" "_C" Color4 "");Cyan
            (setq R-List@ (entget R-Ent^)
                  R-Size~ (+ R-Size~ TxSizeInc~)
                  R-Ang~ (+ R-Ang~ (dtr 30))
                  R-Cnt# (1+ R-Cnt#)
                  R-Ins (nth R-Cnt# R-Pts@)
            );setq
          );progn
          (progn
            (setq R-List@ (entmod (subst (cons 50 R-Ang~) (assoc 50 R-List@) R-List@)))
            (setq R-List@ (entmod (subst (cons 11 R-Ins) (assoc 11 R-List@) R-List@)))
            (setq R-List@ (entmod (subst (cons 40 R-Size~) (assoc 40 R-List@) R-List@)))
            (setq R-Size~ (+ R-Size~ TxSizeInc~)
                  R-Ang~ (+ R-Ang~ (dtr 30))
                  R-Cnt# (1+ R-Cnt#)
            );setq
            (if (<= R-Cnt# Inc#) (setq R-Ins (nth R-Cnt# R-Pts@)))
          );progn
        );if
      );if
    );if
    (if (>= R-Cnt# Fourth#)
      (if (<= O-Cnt# Inc#)
        (if (= O-Cnt# 0)
          (progn
            (command "_TEXT" "_M" (nth O-Cnt# O-Pts@) O-Size~ (rtd O-Ang~) "O")
            (setq O-Ent^ (entlast))
            (command "_CHPROP" O-Ent^ "" "_C" Color5 "");Blue
            (setq O-List@ (entget O-Ent^)
                  O-Size~ (+ O-Size~ TxSizeInc~)
                  O-Ang~ (+ O-Ang~ (dtr 30))
                  O-Cnt# (1+ O-Cnt#)
                  O-Ins (nth O-Cnt# O-Pts@)
            );setq
          );progn
          (progn
            (setq O-List@ (entmod (subst (cons 50 O-Ang~) (assoc 50 O-List@) O-List@)))
            (setq O-List@ (entmod (subst (cons 11 O-Ins) (assoc 11 O-List@) O-List@)))
            (setq O-List@ (entmod (subst (cons 40 O-Size~) (assoc 40 O-List@) O-List@)))
            (setq O-Size~ (+ O-Size~ TxSizeInc~)
                  O-Ang~ (+ O-Ang~ (dtr 30))
                  O-Cnt# (1+ O-Cnt#)
            );setq
            (if (<= O-Cnt# Inc#) (setq O-Ins (nth O-Cnt# O-Pts@)))
          );progn
        );if
      );if
    );if
    (if (>= O-Cnt# Fourth#)
      (if (<= Y-Cnt# Inc#)
        (if (= Y-Cnt# 0)
          (progn
            (command "_TEXT" "_M" (nth Y-Cnt# Y-Pts@) Y-Size~ (rtd Y-Ang~) "Y")
            (setq Y-Ent^ (entlast))
            (command "_CHPROP" Y-Ent^ "" "_C" Color6 "");Magenta
            (setq Y-List@ (entget Y-Ent^)
                  Y-Size~ (+ Y-Size~ TxSizeInc~)
                  Y-Ang~ (+ Y-Ang~ (dtr 30))
                  Y-Cnt# (1+ Y-Cnt#)
                  Y-Ins (nth Y-Cnt# Y-Pts@)
            );setq
          );progn
          (progn
            (setq Y-List@ (entmod (subst (cons 50 Y-Ang~) (assoc 50 Y-List@) Y-List@)))
            (setq Y-List@ (entmod (subst (cons 11 Y-Ins) (assoc 11 Y-List@) Y-List@)))
            (setq Y-List@ (entmod (subst (cons 40 Y-Size~) (assoc 40 Y-List@) Y-List@)))
            (setq Y-Size~ (+ Y-Size~ TxSizeInc~)
                  Y-Ang~ (+ Y-Ang~ (dtr 30))
                  Y-Cnt# (1+ Y-Cnt#)
            );setq
            (if (<= Y-Cnt# Inc#) (setq Y-Ins (nth Y-Cnt# Y-Pts@)))
          );progn
        );if
      );if
    );if
    ; Erase Troys that are out of limits
    (setq Cnt# 0)
    (foreach List@ TroyArray@
      (setq CirEnt^ (nth 0 List@)
            CirPt1 (nth 1 List@)
            Radius~ (nth 4 List@)
      );setq
      (if (> (distance CenPt CirPt1) CirLimits~)
        (progn
          (command "_ERASE" CirEnt^ "")
          (setq TroyArray@ (Change_nth Cnt# (AddArray: nil) TroyArray@))
        );progn
        (setq TroyArray@ (Change_nth Cnt# (ChangeArray: List@) TroyArray@))
      );if
      (setq Cnt# (1+ Cnt#))
    );foreach
    (delay 0.15);Speed of Loop
    (if (> Y-Cnt# Inc#)(setq Loop nil))
    (if (or (/= (getvar "VIEWCTR") CenPt)(/= (getvar "VIEWSIZE") ViewSize~))
      (command "_ZOOM" "_W" (car ViewExtents@)(cadr ViewExtents@))
    );if
  );while
  ;-----------------------------------------------------------------------------
  ; Second Loop
  ;-----------------------------------------------------------------------------
  (setq Loop t Move# 0 Ltr# 0 Sevenths# (/ (length Path@) 7) Fire# (1+ Sevenths#))
  (BuildShip: 0 (nth 0 Path@))
  (if (> Path# 2)
    (setq MainList@ (entmod (subst (cons 42 -1.0) (assoc 42 MainList@) MainList@)))
  );if
  (while Loop
    ; Move Ship
    (setq Pt1 (nth Move# Path@)
          Pt2 (nth (1+ Move#) Path@)
          Ang~ (angle Pt1 Pt2)
    );setq
    ;(command "LINE" Pt1 Pt2 "");Uncomment while debuging
    (setq MainList@ (entmod (subst (cons 50 Ang~) (assoc 50 MainList@) MainList@)))
    (setq MainList@ (entmod (subst (cons 10 Pt1) (assoc 10 MainList@) MainList@)))
    ; Fire at Troy Letters
    (setq Fire# (1+ Fire#))
    (if (= Fire# (fix (* Sevenths# 2.5)))(setq Fire# Sevenths#));First time
    (if (= Fire# Sevenths#);Fire in these intervals
      (progn
        (setq Fire# 0 Ltr# (1+ Ltr#))
        (if (member Ltr# (list 1 2 3 4))
          (progn
            (setq Pt (nth Ltr# RndLtr@)
                  Ang~ (angle Pt1 Pt)
                  Pt1 (polar Pt1 Ang~ (* Unit~ 2))
                  Pt2 (polar Pt1 Ang~ Unit~)
            );setq
            (command "_INSERT" Flame$ Pt1 1 1 (rtd Ang~))
            (setq FlameArray@ (append FlameArray@ (list (list (entlast) Pt1 Pt2 Ang~))))
          );progn
        );if
      );progn
    );if
    ; Move flame objects
    (if FlameArray@
      (progn
        (setq Cnt# 0 Nth# nil)
        (foreach List@ FlameArray@
          (setq Flame^ (nth 0 List@)
                Pt1 (nth 1 List@)
                Pt2 (nth 2 List@)
                Ang~ (nth 3 List@)
          );setq
          (if (or (and (> (car Pt2)(car East))(> (car Pt2)(car Pt1)))
                  (and (< (car Pt2)(car West))(< (car Pt2)(car Pt1)))
                  (and (> (cadr Pt2)(cadr North))(> (cadr Pt2)(cadr Pt1)))
                  (and (< (cadr Pt2)(cadr South))(< (cadr Pt2)(cadr Pt1)))
              );or
            (progn
              (command "_ERASE" Flame^ "")
              (setq Nth# Cnt#)
            );progn
            (progn
              (command "_MOVE" Flame^ "" Pt1 Pt2)
              (setq Pt1 Pt2 Pt2 (polar Pt2 Ang~ Unit~))
              (setq List@ (list Flame^ Pt1 Pt2 Ang~))
              (setq FlameArray@ (Change_nth Cnt# List@ FlameArray@))
            );progn
          );if
          (setq Cnt# (1+ Cnt#))
        );foreach
        (if Nth#
          (setq FlameArray@ (Delete_nth Nth# FlameArray@))
        );if
      );progn
    );if
    ; Check to see if Troy Letters are hit
    (if FlameArray@
      (progn
        (setq Num# 0)
        (foreach List@ FlameArray@
          (setq Ent^ (nth 0 List@)
                Pt2 (nth 2 List@)
                Pt nil
          );setq
          (cond
            ((<= (distance Pt2 T-Pt) Unit~)
              (command "_ERASE" T-Ent^ Ent^ "")
              (setq FlameArray@ (Delete_nth Num# FlameArray@))
              (setq Pt T-Pt T-Pt SouthWest Color# Color3);Green
            );case
            ((<= (distance Pt2 R-Pt) Unit~)
              (command "_ERASE" R-Ent^ Ent^ "")
              (setq FlameArray@ (Delete_nth Num# FlameArray@))
              (setq Pt R-Pt R-Pt SouthWest Color# Color4);Cyan
            );case
            ((<= (distance Pt2 O-Pt) Unit~)
              (command "_ERASE" O-Ent^ Ent^ "")
              (setq FlameArray@ (Delete_nth Num# FlameArray@))
              (setq Pt O-Pt O-Pt SouthWest Color# Color5);Blue
            );case
            ((<= (distance Pt2 Y-Pt) Unit~)
              (command "_ERASE" Y-Ent^ Ent^ "")
              (setq FlameArray@ (Delete_nth Num# FlameArray@))
              (setq Pt Y-Pt Y-Pt SouthWest Color# Color6);Magenta
            );case
          );cond
          ; Explode Letter
          (if Pt
            (progn
              (command "_COLOR" Color#)
              (setq Dia1~ 0.5 Dia2~ 3 Ang~ (* (GetRnd 6283) 0.001) Inc# 0 Inc1~ 0.125 Inc2~ 0.375)
              (repeat 10
                (if (= Inc# 6)(setq Inc1~ -0.125 Inc2~ -0.375))
                (StarBurst Pt (* Unit~ Dia1~) (* Unit~ Dia2~) (+ (GetRnd 5) 5) Ang~)(delay 0.125)
                (setq Dia1~ (+ Dia1~ Inc1~) Dia2~ (+ Dia2~ Inc2~))
                (setq Ang~ (* (GetRnd 6283) 0.001))
                (command "_ERASE" (entlast) "")
                (setq Inc# (1+ Inc#))
              );repeat
              (command "_COLOR" "_BYLAYER")
            );progn
          );if
          (setq Num# (1+ Num#))
        );foreach
      );progn
    );if
    ; Erase Troys that are out of limits
    (setq Cnt# 0)
    (foreach List@ TroyArray@
      (setq CirEnt^ (nth 0 List@)
            CirPt1 (nth 1 List@)
            Radius~ (nth 4 List@)
      );setq
      (if (> (distance CenPt CirPt1) CirLimits~)
        (progn
          (command "_ERASE" CirEnt^ "")
          (setq TroyArray@ (Change_nth Cnt# (AddArray: nil) TroyArray@))
        );progn
        (setq TroyArray@ (Change_nth Cnt# (ChangeArray: List@) TroyArray@))
      );if
      (setq Cnt# (1+ Cnt#))
    );foreach
    (delay 0.15);Speed of Loop
    (setq Move# (1+ Move#))
    (if (= Move# (1- (length Path@)))(setq Loop nil))
    (if (or (/= (getvar "VIEWCTR") CenPt)(/= (getvar "VIEWSIZE") ViewSize~))
      (command "_ZOOM" "_W" (car ViewExtents@)(cadr ViewExtents@))
    );if
  );while
  (setq SS& (ssget "x" (list '(8 . "Troy"))))
  (command "_ERASE" SS& "")
  (princ)
);defun TroyIntro
;-------------------------------------------------------------------------------
; TroyClear - Troy clear function
;-------------------------------------------------------------------------------
(defun TroyClear (/ Block$ Passed SS&)
  (if *TroyTab$* (command "_LAYOUT" "_S" *TroyTab$*))
  (if *Clayer$* (setvar "CLAYER" *Clayer$*))
  (if *Osmode#* (setvar "OSMODE" *Osmode#*))
  (if *TextStyle$* (setvar "TEXTSTYLE" *TextStyle$*))
  (if *TextSize~* (setvar "TEXTSIZE" *TextSize~*))
  (command "_COLOR" "_BYLAYER")
  (if (setq SS& (ssget "_x" (list '(8 . "Troy"))))
    (command "_ERASE" SS& "")
  );if
  (setq Block$ (strcat (substr (UniqueName) 1 5) "*"))
  (foreach Item (GetBlockList)
    (if (wcmatch Item Block$) (setq Passed t))
  );foreach
  (if Passed (command "_PURGE" "_BL" Block$ "_N"))
  (if (tblsearch "LAYER" "Troy") (command "_PURGE" "_LA" "Troy" "_N"))
  (if (tblsearch "STYLE" "Troy") (command "_PURGE" "_ST" "Troy" "_N"))
  (setq *Clayer$* nil *Osmode#* nil *TextStyle$* nil *TextSize~* nil)
  (PurgeGroups)
  (if *CTab$*
    (progn (command "_LAYOUT" "_S" *CTab$*)(setq *CTab$* nil *TroyTab$* nil))
  );if
  (repeat 45 (princ (strcat "\n" (chr 160))))
  (princ)
);defun TroyClear
;-------------------------------------------------------------------------------
; Start of Troy Support Utility Functions
;-------------------------------------------------------------------------------
; acos
; Arguments: 1
;   x = real number between 0 and 1. May be passed as the sum of dividing two
;       sides of a right triangle.
; Returns: acos of x, the radian degrees between sides of a right triangle
;-------------------------------------------------------------------------------
(defun acos (x)
  (atan (/ (sqrt (- 1 (* x x))) x))
);defun acos
;-------------------------------------------------------------------------------
; asin
; Arguments: 1
;   sine = real number between -1 to 1
; Returns: arcsin of sine
;-------------------------------------------------------------------------------
(defun asin (sine / cosine)
  (setq cosine (sqrt (- 1.0 (expt sine 2))))
  (if (zerop cosine)
    (setq cosine 0.000000000000000000000000000001)
  );if
  (atan (/ sine cosine))
);defun asin
;-------------------------------------------------------------------------------
; Center3Pt - Center point of 3 points on a circle
; Arguments: 3
;   Pt1 = First point
;   Pt2 = Second point
;   Pt3 = Third point
; Returns: Center point of 3 points on a circle
;-------------------------------------------------------------------------------
(defun Center3Pt (Pt1 Pt2 Pt3 / Pt Pt4 Pt5 Pt6 Pt7)
  (setq Pt4 (polar Pt1 (angle Pt1 Pt2) (/ (distance Pt1 Pt2) 2.0))
        Pt5 (polar Pt4 (+ (angle Pt1 Pt2) (* pi 0.5)) 1)
        Pt6 (polar Pt2 (angle Pt2 Pt3) (/ (distance Pt2 Pt3) 2.0))
        Pt7 (polar Pt6 (+ (angle Pt2 Pt3) (* pi 0.5)) 1)
        Pt (inters Pt4 Pt5 Pt6 Pt7 nil)
  );setq
);defun Center3Pt
;-------------------------------------------------------------------------------
; Change_nth - Changes the nth item in a list with a new item value.
; Arguments: 3
;   Num# = Nth number in list to change
;   Value = New item value to change to
;   OldList@ = List to change item value
; Returns: A list with the nth item value changed.
;-------------------------------------------------------------------------------
(defun Change_nth (Num# Value OldList@)
  (if (<= 0 Num# (1- (length OldList@)))
    (if (> Num# 0)
      (cons (car OldList@) (Change_nth (1- Num#) Value (cdr OldList@)))
      (cons Value (cdr OldList@))
    );if
    OldList@
  );if
);defun Change_nth
;-------------------------------------------------------------------------------
; delay - time delay function
; Arguments: 1
;   Percent~ - Percentage of *Speed# variable
; Returns: time delay
;-------------------------------------------------------------------------------
(defun delay (Percent~ / Number~)
  (if (not *Speed#) (Speed))
  (repeat (fix (* *Speed# Percent~)) (setq Number~ pi))
  (princ)
);defun delay
;-------------------------------------------------------------------------------
; Delete_nth - Deletes the nth item from a list.
; Arguments: 2
;   Num# = Nth number in list to delete
;   OldList@ = List to delete the nth item
; Returns: A list with the nth item deleted.
;-------------------------------------------------------------------------------
(defun Delete_nth (Num# OldList@)
  (setq Num# (1+ Num#))
  (vl-remove-if '(lambda (x) (zerop (setq Num# (1- Num#)))) OldList@)
);defun Delete_nth
;-------------------------------------------------------------------------------
; dtr - Degrees to Radians.
; Arguments: 1
;   Deg~ = Degrees
; Syntax: (dtr Deg~)
; Returns: Value in radians.
;-------------------------------------------------------------------------------
(defun dtr (Deg~)
  (* pi (/ Deg~ 180.0))
);defun dtr
;-------------------------------------------------------------------------------
; GetBlockList
;-------------------------------------------------------------------------------
(defun GetBlockList (/ BlockList@ Block$ List@)
  (if (setq List@ (tblnext "BLOCK" 't))
    (while List@
      (setq Block$ (cdr (assoc 2 List@)))
      (if (/= (substr Block$ 1 1) "*")
        (setq BlockList@ (append BlockList@ (list Block$)))
      );if
      (setq List@ (tblnext "BLOCK"))
    );while
  );if
  (if BlockList@
    (setq BlockList@ (Acad_StrlSort BlockList@))
  );if
  BlockList@
);defun GetBlockList
;-------------------------------------------------------------------------------
; GetRnd - Generates a random number
; Arguments: 1
;   Num# = Maximum random integer number range greater than or less than 0.
; Returns: Random integer number between 0 and Num#.
;-------------------------------------------------------------------------------
(defun GetRnd (Num# / MaxNum# PiDate$ RndNum# Minus Loop)
  (if (or (/= (type Num#) 'INT)(= Num# 0))
    (progn
      (princ "\nSyntax: (GetRnd Num#) Num# = Maximum random integer number range\ngreater than or less than 0.")
      (exit)
    );progn
  );if
  (if (< Num# 0)
    (setq MaxNum# (abs (1- Num#)) Minus t)
    (setq MaxNum# (1+ Num#))
  );if
  (if (not *RndNum*) (setq *RndNum* 10000))
  (setq Loop t)
  (while Loop
    (if (or (null *int*)(> *int* 100))
      (setq *int* 1)
      (setq *int* (1+ *int*))
    );if
    (setq PiDate$ (rtos (* (getvar "cdate") (* pi *int*)) 2 8 ))
    (cond
      ((>= MaxNum# 10000)
        (setq RndNum# (fix (* (atof (substr PiDate$ 13 5)) (* MaxNum# 0.00001))))
      )
      ((>= MaxNum# 1000)
        (setq RndNum# (fix (* (atof (substr PiDate$ 14 4)) (* MaxNum# 0.0001))))
      )
      ((>= MaxNum# 100)
        (setq RndNum# (fix (* (atof (substr PiDate$ 15 3)) (* MaxNum# 0.001))))
      )
      ((>= MaxNum# 10)
        (setq RndNum# (fix (* (atof (substr PiDate$ 16 2)) (* MaxNum# 0.01))))
      )
      ((>= MaxNum# 1)
        (setq RndNum# (fix (* (atof (substr PiDate$ 17 1)) (* MaxNum# 0.1))))
      )
      (t (setq RndNum# 0))
    );cond
    (if (/= RndNum# *RndNum*)
      (setq Loop nil)
    );if
  );while
  (setq *RndNum* RndNum#)
  (if Minus
    (setq RndNum# (* RndNum# -1))
  );if
  RndNum#
);defun GetRnd
;-------------------------------------------------------------------------------
; Insert_nth - Inserts a new item value into the nth number in list.
; Arguments: 3
;   Num# = Nth number in list to insert item value
;   Value = Item value to insert
;   OldList@ = List to insert item value
; Returns: A list with the new item value inserted.
;-------------------------------------------------------------------------------
(defun Insert_nth (Num# Value OldList@ / Temp@)
  (if (< -1 Num# (1+ (length OldList@)))
    (progn
      (repeat Num#
        (setq Temp@ (cons (car OldList@) Temp@)
              OldList@ (cdr OldList@)
        );setq
      );repeat
      (append (reverse Temp@) (list Value) OldList@)
    );progn
    OldList@
  );if
);defun Insert_nth
;-------------------------------------------------------------------------------
; MirrorPt - Mirror point
; Arguments: 3
;   Pt = Point to mirror
;   BasePt = Base point
;   Angle~ = Mirror angle in radians
; Returns: Returns location of mirrored point
;-------------------------------------------------------------------------------
(defun MirrorPt (Pt BasePt Angle~ / Pt1)
  (if (> Angle~ pi)
    (setq Angle~ (- Angle~ pi))
  );if
  (setq Pt1 (inters Pt (polar Pt (+ Angle~ (* pi 0.5)) 1)
                    BasePt (polar BasePt Angle~ 1) nil)
        Pt1 (polar Pt1 (angle Pt Pt1) (distance Pt Pt1))
  );setq
);defun MirrorPt
;-------------------------------------------------------------------------------
; Move_nth - Moves the nth Num1# item value to the nth Num2# location in a list.
; Arguments: 3
;   Num1# = Nth number in list to move item value
;   Num2# = Nth number in list to move item value of nth Num1# into
;   OldList@ = List to move item values
; Returns: A list with nth item value moved.
;-------------------------------------------------------------------------------
(defun Move_nth (Num1# Num2# OldList@ / Move_nth:)
  (defun Move_nth: (Num1# Num2# OldList@ Nth# Item)
    (cond
      ((and (> Nth# Num1#) (> Nth# Num2#))
        OldList@
      );case
      ((= Nth# Num1#)
        (Move_nth: Num1# (1+ Num2#) (cdr OldList@) (1+ Nth#) Item)
      );case
      ((= Nth# Num2#)
        (cons Item (Move_nth: (1+ Num1#) Num2# OldList@ (1+ Nth#) Item))
      );case
      ((cons (car OldList@)
        (Move_nth: Num1# Num2# (cdr OldList@) (1+ Nth#) Item))
      );case
    );cond
  );defun Move_nth:
  (if (and (/= Num1# Num2#) (<= 0 Num1# (1- (length OldList@))) (<= 0 Num2# (1- (length OldList@))))
    (Move_nth: Num1# Num2# OldList@ 0 (nth Num1# OldList@))
    OldList@
  );if
);defun Move_nth
;-------------------------------------------------------------------------------
; PurgeGroups - Purge Unused Groups
;-------------------------------------------------------------------------------
(defun PurgeGroups (/ AllGroups@ Cnt# Dictionary^ EntFirst^ EntList@ FirstGroup$
  Group^ GroupName$ Item Previous$ Pt SS& UsedGroups@)
  (setq Pt (polar (getvar "VIEWCTR") (* pi 1.5)(/ (getvar "VIEWSIZE") 2.0)))
  (command "_LINE" Pt (polar Pt (* pi 1.5) 0.00000001) "")
  (setq EntFirst^ (entlast))
  (setq FirstGroup$ (UniqueName))
  (command "_-GROUP" "_C" FirstGroup$ "" EntFirst^ "")
  (setq EntList@ (entget EntFirst^))
  (setq Group^ (cdr (assoc 330 EntList@)))
  (setq EntList@ (entget Group^))
  (setq Dictionary^ (cdr (assoc 330 EntList@)))
  (setq EntList@ (entget Dictionary^))
  (foreach Item EntList@
    (if (= (car Item) 3)
      (if (not (member (cdr Item) AllGroups@))
        (setq AllGroups@ (append AllGroups@ (list (cdr Item))))
      );if
    );if
  );foreach
  (setq SS& (ssget "_X"))
  (setq Cnt# 0)
  (repeat (sslength SS&)
    (setq EntList@ (entget (ssname SS& Cnt#)))
    (if (= (cdr (assoc 102 EntList@)) "{ACAD_REACTORS")
      (progn
        (setq Group^ (cdr (assoc 330 EntList@)))
        (setq EntList@ (entget Group^))
        (if (setq Dictionary^ (cdr (assoc 330 EntList@)))
          (progn
            (setq EntList@ (entget Dictionary^))
            (setq Previous$ "")
            (foreach Item EntList@
              (setq Item (cdr Item))
              (if (equal Item Group^)
                (setq GroupName$ Previous$)
              );if
              (setq Previous$ Item)
            );foreach
            (if (not (member GroupName$ UsedGroups@))
              (setq UsedGroups@ (append UsedGroups@ (list GroupName$)))
            );if
          );progn
        );if
      );progn
    );if
    (setq Cnt# (1+ Cnt#))
  );repeat
  (foreach GroupName$ AllGroups@
    (if (not (member GroupName$ UsedGroups@))
      (command "_-GROUP" "_E" GroupName$)
    );if
  );foreach
  (command "_-GROUP" "_E" FirstGroup$)
  (command "_ERASE" EntFirst^ "")
  (princ)
);defun PurgeGroups
;-------------------------------------------------------------------------------
; Remove_nths - Removes the RemoveList@ of nths from a list.
; Arguments: 2
;   RemoveList@ = List of nths to remove
;   OldList@ = List to remove the list of nths from
; Returns: A list with the list of nths removed.
;-------------------------------------------------------------------------------
(defun Remove_nths (RemoveList@ OldList@)
  (if (and RemoveList@ OldList@)
    (if (zerop (car RemoveList@))
      (Remove_nths (mapcar '1- (cdr RemoveList@)) (cdr OldList@))
      (cons (car OldList@) (Remove_nths (mapcar '1- RemoveList@) (cdr OldList@)))
    );if
    OldList@
  );if
);defun Remove_nths
;-------------------------------------------------------------------------------
; rtd - Radians to degrees
; Arguments: 1
;   Rad~ = radians
; Syntax: (rtd R)
; Returns: value in degrees.
;-------------------------------------------------------------------------------
(defun rtd (Rad~)
  (* 180.0 (/ Rad~ pi))
);defun rtd
;-------------------------------------------------------------------------------
; Speed - Determines the computer processing speed and sets the global variable
; *speed# which may be used in delay loops.
;-------------------------------------------------------------------------------
(defun Speed (/ Cdate~ Cnt# NewSecond# OldSecond#)
  (setq Cdate~ (getvar "CDATE"))
  (setq NewSecond# (fix (* (- (* (- Cdate~ (fix Cdate~)) 100000)(fix (* (- Cdate~ (fix Cdate~)) 100000))) 10)))
  (repeat 2
    (setq Cnt# 0)
    (setq OldSecond# NewSecond#)
    (while (= NewSecond# OldSecond#)
      (setq Cdate~ (getvar "CDATE"))
      (setq NewSecond# (fix (* (- (* (- Cdate~ (fix Cdate~)) 100000)(fix (* (- Cdate~ (fix Cdate~)) 100000))) 10)))
      (setq Cnt# (1+ Cnt#))
    );while
  );repeat
  (setq *Speed# Cnt#)
  (princ)
);defun Speed
;-------------------------------------------------------------------------------
; StarBurst - Draws a starburst shape
; Arguments: 5
;   CenPt = Center of starburst
;   Dia1~ = Inside diameter
;   Dia2~ = Outside diameter
;   Sides# = Number of points
;   StartAng~ = Radian angle of first point
; Returns: Draws a starburst shape
;-------------------------------------------------------------------------------
(defun StarBurst (CenPt Dia1~ Dia2~ Sides# StartAng~ / Ang~ Ang1~ List@ List1@
  List2@ List3@ Cnt1# Cnt2# Pt)
  (setq Ang~ (/ pi Sides#))
  (setq Ang1~ (+ StartAng~ (/ Ang~ 2.0)))
  (repeat (* Sides# 2)
    (setq Pt (polar CenPt Ang1~ (/ Dia1~ 2.0)))
    (setq List1@ (append List1@ (list Pt)))
    (setq Ang1~ (+ Ang1~ Ang~))
  );repeat
  (setq Ang1~ (+ StartAng~ Ang~))
  (repeat Sides#
    (setq Pt (polar CenPt Ang1~ (/ (+ Dia1~ Dia2~) 4.0)))
    (setq List2@ (append List2@ (list Pt)))
    (setq Ang1~ (+ Ang1~ (* Ang~ 2)))
  );repeat
  (setq Ang1~ StartAng~)
  (repeat Sides#
    (setq Pt (polar CenPt Ang1~ (/ Dia2~ 2.0)))
    (setq List3@ (append List3@ (list Pt)))
    (setq Ang1~ (+ Ang1~ (* Ang~ 2)))
  );repeat
  (setq Cnt1# 0 Cnt2# 0)
  (repeat Sides#
    (setq List@ (append List@ (list (nth Cnt1# List3@))))
    (setq List@ (append List@ (list (nth Cnt2# List1@))))
    (setq Cnt2# (1+ Cnt2#))
    (setq List@ (append List@ (list (nth Cnt1# List2@))))
    (setq List@ (append List@ (list (nth Cnt2# List1@))))
    (setq Cnt2# (1+ Cnt2#))
    (setq Cnt1# (1+ Cnt1#))
  );repeat
  (setq List@ (append List@ (list (nth 0 List3@))))
  (command "_PLINE" (foreach Pt List@ (command Pt)))
  (princ)
);defun StarBurst
;-------------------------------------------------------------------------------
; Switch_nth - Switches the nth Num1# and Num2# item values in a list.
; Arguments: 3
;   Num1# = nth number in list to switch with nth Num2#
;   Num2# = nth number in list to switch with nth Num1#
;   OldList@ = List to switch item values
; Returns: A list with two item values switched.
;-------------------------------------------------------------------------------
(defun Switch_nth (Num1# Num2# OldList@ / Index#)
  (setq Index# -1)
  (if (and (< -1 Num1# (length OldList@)) (< -1 Num2# (length OldList@)))
    (mapcar '(lambda (x) (setq Index# (1+ Index#))
      (cond
        ((= Index# Num2#) (nth Num1# OldList@))
        ((= Index# Num1#) (nth Num2# OldList@))
        (x)
      )) OldList@
    );mapcar
    OldList@
  );if
);defun Switch_nth
;-------------------------------------------------------------------------------
; tan - Tangent of radian degrees
; Arguments: 1
;   radians = Radian degrees
; Returns: Tangent of radian degrees
;-------------------------------------------------------------------------------
(defun tan (radians)
  (/ (sin radians) (cos radians))
);defun tan
;-------------------------------------------------------------------------------
; UniqueName - Creates a unique name for temp blocks and groups
;-------------------------------------------------------------------------------
(defun UniqueName (/ Loop Name$)
  (setq Loop t)
  (while Loop
    (setq Name$ (rtos (getvar "CDATE") 2 8))
    (setq Name$ (strcat (substr Name$ 4 5)(substr Name$ 10 8)))
    (if (/= Name$ *UniqueName$)
      (setq *UniqueName$ Name$ Loop nil)
    );if
  );while
  *UniqueName$
);defun UniqueName
;-------------------------------------------------------------------------------
; ViewExtents
; Returns: List of upper left and lower right points of current view
;-------------------------------------------------------------------------------
(defun ViewExtents (/ A B C D X)
  (setq B (getvar "VIEWSIZE")
        A (* B (/ (car (getvar "SCREENSIZE")) (cadr (getvar "SCREENSIZE"))))
        X (trans (getvar "VIEWCTR") 1 2)
        C (trans (list (- (car X) (/ A 2.0)) (+ (cadr X) (/ B 2.0))) 2 1)
        D (trans (list (+ (car X) (/ A 2.0)) (- (cadr X) (/ B 2.0))) 2 1)
  );setq
  (list C D)
);defun ViewExtents
;-------------------------------------------------------------------------------
(princ)
