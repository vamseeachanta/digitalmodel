;;**************************************************************************
;                    LC.LSP (Layer Control)
;                          by Zoltan Toth
;    ZOTO Technologies,
;    23 Greenhills Dve,
;    Melton 3337.
;    E-MAIL: zoltan.toth@ains.net.au
;       WWW: http://www.ains.net.au/zoto/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Version 1.1: Modified to use the block layer when the sub-entity layer
;                is "0" instead of when the block is anonymous.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This program prompts the user to pick an object and after determining
; what layer the object resides on, will prompt the user to manipulate
; the layer (Freeze, Lock, Unlock, turn Off). It will recognize layers
; within BLOCKs and XREFs including anonymous BLOCKs (DIMS & HATCHes)
; which generally return layer 0. The program will prefix the XREF name
; to the base layer name according to AutoCAD's normal practice.
;*****************************************************************************
(defun C:LC(/ CMD2 EXP2 LAY2 LS2 KWD2)    ;define program & localize variables
 (setq CMD2 (getvar "CMDECHO")                       ;save command echo status
       EXP2 (getvar "EXPERT")                            ;save "expert" status
 )
 (setvar "CMDECHO" 0)                                ;turn command echoing off
 (if (= 0(getvar "EXPERT"));if "expert" is 0
  (setvar "EXPERT" 1)                                         ;set expert to 1
 )
 (command "._UNDO" "_GROUP")                                 ;start UNDO group
 (setq ENT2 (nentsel "\nPick object on layer to control: "))    ;select object
 (if ENT2                                              ;if object was selected
  (progn
   (if (= 4 (length ENT2))                     ;if object was a complex entity
;if subentity is on layer "0"
    (if (= "0" (substr (cdr (assoc 8 (entget (car ENT2))))1 1))
;get layer of complex entity
     (setq LAY2 (cdr (assoc 8 (entget (car (last ENT2))))))
;else get layer of subentity
     (setq LAY2 (cdr (assoc 8 (entget (car ENT2)))))
    )
;else if object was not a complex entity, get layer
    (setq LAY2 (cdr (assoc 8 (entget (car ENT2)))))
   )
   (setq LS2 (cdr (assoc 70 (tblsearch "LAYER" LAY2))))   ;get status of layer
   (if (or (= 68 LS2)(= 116 LS2))                ;if layer is currently locked
    (progn
     (initget 1 "Freeze Unlock Off")              ;initialize keyword response
     (setq KWD2 (getkword (strcat"\nFreeze/Unlock/turn Off layer "LAY2"? ")))
    )
    (progn                                        ;else if layer is not locked
     (initget 1 "Freeze Lock Off")
     (setq KWD2 (getkword (strcat"\nFreeze/Lock/turn Off layer "LAY2"? ")))
    )
   )
;if current layer is about to be frozen
   (if (and (= (getvar "CLAYER")LAY2)(= KWD2 "Freeze"))
;warn user
    (alert (strcat"\nCannot freeze layer "LAY2"! It is the current layer."))
    (progn                                                ;else carry out work
     (command "._LAYER" KWD2 LAY2 "")
     (prompt (strcat "\nLayer "LAY2))
;and inform user of work completion
     (cond((= "Freeze" KWD2)(prompt " frozen."))
          ((= "Off" KWD2)(prompt " turned off."))
          ((= "Lock" KWD2)(prompt " locked."))
          ((= "Unlock" KWD2)(prompt " unlocked."))
     )
    )
   )
  )
  (prompt"\nNo object selected.")       ;else if nothing selected, inform user
 )
 (command "._UNDO" "_END")                                     ;end UNDO group
 (setvar "CMDECHO" CMD2)                          ;restore command echo status
 (setvar "EXPERT" EXP2)                               ;restore "expert" status
 (princ)                                                         ;exit quietly
)
