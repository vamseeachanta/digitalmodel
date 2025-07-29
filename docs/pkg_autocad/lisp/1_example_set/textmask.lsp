;;;     TEXTMASK.LSP
;;;     Copyright (C) 1997 by Autodesk, Inc.
;;;
;;;     Created 3/12/97 by Dominic Panholzer
;;;
;;;     Permission to use, copy, modify, and distribute this software
;;;     for any purpose and without fee is hereby granted, provided
;;;     that the above copyright notice appears in all copies and 
;;;     that both that copyright notice and the limited warranty and 
;;;     restricted rights notice below appear in all supporting 
;;;     documentation.
;;;
;;;     AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.  
;;;     AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF 
;;;     MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC. 
;;;     DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE 
;;;     UNINTERRUPTED OR ERROR FREE.
;;;
;;;     Use, duplication, or disclosure by the U.S. Government is subject to 
;;;     restrictions set forth in FAR 52.227-19 (Commercial Computer 
;;;     Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;     (Rights in Technical Data and Computer Software), as applicable. 
;;;
;;;  ----------------------------------------------------------------
;;;
;;;     TEXTMASK works in conjunction with WIPEOUT.ARX to hide all
;;;     entities behind the selected text or mtext. The text object
;;;     is then grouped together with the wipeout object such that
;;;     they move, copy, erase, etc. together. To update after editing
;;;     text, run TEXTMASK again and select the text item to be updated.
;;;     The the previous wipeout object will be erased and a new one
;;;     will be created.
;;;     
;;;
;;;
;;;  External Functions:
;;;
;;;     INIT_BONUS_ERROR  --> AC_BONUS.LSP   Intializes bonus error routine
;;;     RESTORE_OLD_ERROR --> AC_BONUS.LSP   Restores old error routine
;;;     B_LAYER_LOCKED    --> AC_BONUS.LSP   Checks to see if layer is locked
;;;     UCS_2_ENT         --> AC_BONUS.LSP   Sets current ucs to extrusion vector 
;;;     BNS_MAKGRP        --> AC_BONUS.LSP   Creates anonymous group
;;;     BNS_MTEXTBOX      --> AC_BONUS.LSP   Textbox function for mtext
;;;     BNS_UCS_2_MTEXT   --> AC_BONUS.LSP   Sets ucs to mtext object
;;;
;;;
;;;  Updates:
;;;
;;;     Date      By     Description
;;;    -----------------------------------------------
;;;     07/02/97  DBP  - Added dimzin to variable list in error handler. If dimzin was
;;;                      not set to 0 value written to ACAD.CFG would not have the
;;;                      leading 0 (eg ".35" instead of "0.35") causing the read function
;;;                      to fail.
;;;
;;;     07/02/97  DBP  - Corrected error checking for offset value read from ACAD.CFG
;;;
;;;     07/16/97  DBP  - Removed local functions UCS_2_MTEXT, MAKGROUP, and MTEXTBOX
;;;
;;;     08/20/97  DBP  - Added cecolor setting to error handler
;;;
;;;     09/04/97  DBP  - Added filtering for empty text strings (i.e. "")
;;;



(defun c:textmask ( / grplst getgname getgmem drawbox
                      WIPOUT CNT GLST OSET TMP SS ENT PNTLST ZM LOCKED GDICT 
                      GNAM GRP MLST TXT TOS TXTLAY TXLCK TXTSTR
                  )

; --------------------- Error initialization ---------------------

  (init_bonus_error 
    (list
      (list "cmdecho"   0
            "plinewid"  0
            "highlight" 1
            "osmode"    0
            "clayer"    (getvar "clayer")
            "dimzin"    0
            "cecolor"   "bylayer"
      )
      
      T     ;flag. True means use undo for error clean up.  
      
    );list  
  );init_bonus_error

; --------------------- GROUP LIST FUNCTION ----------------------
;   This function will return a list of all the group names in the
;   drawing and their entity names in the form:
;   (<ename of "ACAD_GROUP"> (<ename1> . <name1>) (<ename2> . <name2>))
; ----------------------------------------------------------------

  (defun grplst (/ GRP MSTR ITM NAM ENT GLST)

    (setq GRP  (dictsearch (namedobjdict) "ACAD_GROUP"))
    (while (setq ITM (car GRP))       ; While edata item is available
      (if (= (car ITM) 3)             ; if the item is a group name
        (setq NAM (cdr ITM)           ; get the name
              GRP (cdr GRP)           ; shorten the edata
              ITM (car GRP)           ; get the next item
              ENT (cdr ITM)           ; which is the ename
              GRP (cdr GRP)           ; shorten the edata
              GLST                    ; store the ename and name
                  (if GLST
                    (append GLST (list (cons ENT NAM)))
                    (list (cons ENT NAM))
                  )
        )
        (setq GRP (cdr GRP))          ; else shorten the edata
      )
    )
    GLST                              ; return the list
  )

; ------------------- GET GROUP NAME FUNCTION --------------------
;   This function returns a list of all the group names in GLST
;   where ENT is a member. The list has the same form as GLST
; ----------------------------------------------------------------

  (defun getgname (ENT GLST / MSTR GRP GDATA ITM NAM NLST)
    (if (and GLST (listp GLST))
      (progn
        (foreach GRP GLST
          (setq GDATA (entget (car GRP)))
          (foreach ITM GDATA                   ; step through the edata
            (if (and
                  (= (car ITM) 340)            ; if the item is a entity name
                  (eq (setq NAM (cdr ITM)) ENT) ; and the ename being looked for
                )
              (setq NLST                       ; store the ename and name
                      (if NLST
                        (append NLST (list (cons (car GRP) (cdr GRP))))
                        (list (cons (car GRP) (cdr GRP)))
                      )
              )
            )
          )
        )
      )
    )
    NLST
  )

; --------------------- GROUP MEMBER FUNCTION ----------------------
;   This function returns a list of all the entity names of the
;   members of group GNAM. GNAM is a list (<ename1> . <name1>).
; ----------------------------------------------------------------

  (defun getgmem (GNAM / GRP GDATA ITM NLST)

    (if GNAM
      (progn
        (setq GDATA (entget (car GNAM)))
        (foreach ITM GDATA                    ; step through the edata
          (if (= (car ITM) 340)               ; if the item is a entity name
            (setq NLST  (cons (cdr ITM) NLST) ; store the ename
            )
          )
        )
      )
    )
    NLST
  )

; ---------------------- DRAWBOX FUNCTION ------------------------
;   Function to draw the pline bounding box with the specified
;   offset (DIST) around text or mtext (TXT).    
;
;   External Functions:
;
;     BNS_MTEXTBOX  --> AC_BONUS.LSP   Textbox function for mtext
; ----------------------------------------------------------------


  (defun drawbox (TXT DIST / TBX PT ORGBND)

    (if (= TXTYP "TEXT")
      (progn
        (setq TBX (textbox TXT))    ; normal text
        (command "_.pline" (car TBX) (list (caadr TBX)(cadar TBX))
                 (cadr TBX) (list (caar TBX)(cadadr TBX)) "_close"
        )
      )
      (progn
        (setq TBX (bns_mtextbox TXT))   ; Mtext
        (command "_.pline")
        (foreach PT TBX (command PT))
        (command "_c")
      )
    )

    (setq ORGBND (entlast))

    (command "_.offset" DIST (entlast))
    (if (= TXTYP "TEXT")
      (command "-1,-1" "")
      (command (polar 
                 (cdr (assoc 10 TXT))
                 (cdr (assoc 50 TXT))
                 (* 2 (cdr (assoc 42 TXT)))
               )
               ""
      ) 
    )

    (entdel ORGBND)

  );end defun

; ----------------------------------------------------------------
;                          MAIN PROGRAM
; ----------------------------------------------------------------


  (if (member "wipeout.arx" (arx))
    (setq WIPOUT T)
    (progn
      (princ "\nLoading WIPEOUT for use with TEXTMASK...")
      (if (arxload "wipeout.arx" nil)
        (setq WIPOUT T)
        (progn
          (prompt "\nWIPEOUT.ARX, an AutoCAD Bonus Tool needed for this application")
          (prompt "\ncould not be found. Operation aborted.")
        )
      )
    )
  )
  
  (if WIPOUT                                           ; if wipeout.arx is loaded
    (progn

      (setq CNT   0                                    ; Initilize the counter.
            GLST  (grplst)                             ; Get all the groups in drawing
            GDICT (if GLST
                      (dictsearch (namedobjdict) "ACAD_GROUP")
                    )
            FLTR   '( (-4 . "<OR")                     ; Filter for ssget.
                          (0 . "MTEXT")
                          (0 . "TEXT")
                      (-4 . "OR>")
                    )
      )

; ------------------ Set the offset value to use ----------------- 
    
      (setq OSET (getcfg "AppData/AC_Bonus/Txtmsk_Offset"))

      (if (= (setq OSET (atof OSET)) 0)                ; If no prior valid setting
        (setq OSET 0.35 )                              ; use 0.35 as default.
      )

      (initget 4)                                      ; No negative values allowed
      (setq TMP
        (getdist (strcat "\nEnter offset factor relative to text height <" (rtos OSET 2 2) ">: "))
      )

      (if TMP (setq OSET TMP))
      (setcfg "AppData/AC_Bonus/Txtmsk_Offset" (rtos OSET 2 2))

; ---------------------- get text to mask ------------------------

      (Princ "\nSelect Text to MASK...")
      
      (if (setq SS (ssget FLTR))                       ; Select text and mtext
        (progn

          (command "_.wipeout" "_frame" "_off")        ; Turn off wipeout frames

          (if (b_layer_locked (getvar "clayer"))       ; if current layer is locked
            (progn
              (command "_.layer" "_unl" (getvar "clayer") "")  ; unlock it
              (setq LOCKED T)
            )
          )

; ----------------- Step through each and mask -------------------

          (While (setq ENT (ssname SS CNT))            ; step through each object in set

            (and
              GLST                                     ; if groups are present in the drawing
              (setq GNAM (getgname ENT GLST))          ; and the text item is in one or more
              (foreach GRP GNAM                        ; step through those groups
                (and
                  (setq MLST (getgmem GRP))            ; Get the members of the group.
                  (= (length MLST) 2)                  ; If the group has two members
                  (if (eq (car MLST) ENT)              ; get the member which is not
                    (setq MLST (cadr MLST))            ; the text.
                    (setq MLST (car MLST))
                  )
                  (= "WIPEOUT" (cdr (assoc 0 (entget MLST))))   ; If it is a wipeout entity
                  (dictremove (cdr (assoc -1 GDICT)) (cdr GRP)) ; explode the group
                  (entdel MLST)                                 ; and delete the wipeout
                )
              )
            )
                  
            (setq TXT   (entget ENT (list "*"))
                  TXTYP (cdr (assoc 0 TXT))            ; Text or Mtext
                  TXTSTR (cdr (assoc 1 TXT))
            )

            (if (/= TXTSTR "")
              (progn
                (if (= TXTYP "TEXT")
                  (command "_.ucs" "_object" ENT)          ; set UCS to object
                   (bns_ucs_2_mtext ENT)
                )

                (setq TXTLAY (cdr (assoc 8 TXT))           ; Get the layer of the text
                      TOS    (* (cdr (assoc 40 TXT)) OSET) ; Set the offset for the text
                )

                (drawbox TXT TOS)                          ; Draw pline around text

                (command "_.ucs" "_previous")              ; reset the ucs

                (command "_.wipeout" "_new" (entlast) "_yes")  ; create wipeout entity

                (setq WIPOUT (entlast))

                (command "_.change" WIPOUT "" "_Prop" "_Layer" TXTLAY "") ; and set its layer

                (if (setq TXLCK (b_layer_locked TXTLAY))   ; If text layer is locked
                  (command "_.layer" "_unl" TXTLAY "")     ; unlock it
                )

                (entmake TXT)                              ; recreate text
                (setq TXT (entlast))                       ; such that it's on top

                (bns_makgrp (list WIPOUT TXT) "In use by TEXTMASK") ; make the text and wipeout a group
        
                (entdel ENT)                               ; delete original text

                (if TXLCK (command "_.layer" "_lock" TXTLAY "")) : relock if needed
              )
            )

            (setq CNT (1+ CNT))                        ; get the next text item
          ); while

          (if LOCKED (command "_.layer" "_lock" (getvar "clayer") "")) : relock if needed

        );progn
        (prompt "\nNothing selected.")
      );if SS
    );progn
  );if wipeout

  (restore_old_error)                                  ; Retsore values

)

