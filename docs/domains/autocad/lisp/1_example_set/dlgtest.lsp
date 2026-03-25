;;;------------------------------------------------------------------------
;;;   DLGTEST.LSP
;;;   (C) Copyright 1990-1994 by Autodesk, Inc.
;;;
;;;   Permission to use, copy, modify, and distribute this software and its
;;;   documentation for any purpose and without fee is hereby granted.
;;;
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED
;;;   WARRANTY. ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.
;;;
;;;------------------------------------------------------------------------
;
;
; Programmable Dialog Box Test Program
;
; This program is the AutoLISP counterpart to the ADS test
; program, dlgtest.c.  It provides a simple dimensioning
; dialog invoked with the command "dimen" and a simple color
; dialog invoked with the command "setcolor".
;
; The purposes of providing this program:
; 1) Demonstrate Programmable Dialog Box use with minimum of code
;       to sort through
; 2) Demonstrate differences between LISP and ADS dialog programming
; 3) Use as a starting point for testing new dialog functions
;
; Dlgtest uses the file dlgtest.dcl as the DCL (Dialog Control Language) file.
; LISP functions are associated with dialog tiles (buttons, edit boxes,
;   etc) with the "action_tile" statements.  These actions are evaluated 
;   when the user presses buttons during the start_dialog function.
;
; Special tile names (keys): 
;   "accept" - Ok button
;   "cancel" - Cancel button

; Initialization--set the dialog position to default (centered).  
;   Only required if you want to reposition it where the user last left it.
(setq dim_pos '(-1 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; DIMEN -- AutoCAD dimensioning variables.  Set AutoCAD variables 
;   only if OK pressed, by defining the action for the "accept"
;   tile.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:dimen ( / chklist realist)
  ;load DCL file
  (setq di_dcl_id (load_dialog "dlgtest.dcl"))
  (if (< di_dcl_id 0) (exit))

  ; display dialog
  (if (not (new_dialog "dimensions" di_dcl_id "" dim_pos)) (exit))

  ; Create list of button names to match AutoCAD variables
  (setq chklist '("dimse1"  "dimse2" "dimtih" "dimtoh" "dimtad"  "dimtol"
            "dimlim"  "dimalt" "dimaso" "dimsho")
  )
  (setq realist '("dimasz" "dimtsz" "dimtxt" "dimcen" "dimexo" "dimexe"
            "dimdle")
  )
  ; Send the current value of AutoCAD variables to the dialog
  (mapcar 'set_tile_int chklist)
  (mapcar 'set_tile_real realist)


  ; Define the action to take when the user presses OK, which
  ;   is to call the LISP function "dimen_ok".  If the user
  ;   terminates the dialog with CANCEL, no action will be taken.
  ;   "accept" is the key name of the OK button (tile).

  (action_tile "accept" "(dimen_ok)")


  (start_dialog)                ;returns after OK or CANCEL selected
  (unload_dialog di_dcl_id)     ;free DCL from memory
)

;If the user selects OK, this function will be called to update
;  data, etc.

(defun dimen_ok ()
  ; Get values from dialog, update AutoCAD
  (mapcar 'get_tile_int chklist)
  (mapcar 'get_tile_real realist)

  ;return 1 to start_dialog (Ok).  "dim_pos" contains the position
  ;  of the dialog.  Next call will use that position.
  (setq dim_pos (done_dialog 1)) 
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; SETCOLOR -- Test Various PDB Functions
;
;            This is a COLOR dialog that sets AutoCAD's current 
;            color using (command "_.color" color_num).  The color
;            names are displayed in a list box, color codes in an 
;            edit box, and actual color in an image tile.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:setcolor ( / ok coloridx clist colorstr)
  ;load DCL file
  (setq sc_dcl_id (load_dialog "dlgtest.dcl"))
  (if (< sc_dcl_id 0) (exit))

  ; get current color
  (setq colorstr (getvar "cecolor"))
  (setq coloridx (atoi colorstr))

  ; load a dialog from dialog file
  (if (not (new_dialog "setcolor" sc_dcl_id)) (exit))
                                        ; load a dialog from dialog file

  ; Set up dialog list box

  (setq clist '("255"))
  (setq idx 254)
  (while (> idx 7) 
    (setq clist (cons (itoa idx) clist))
    (setq idx (1- idx))
  )
  (setq clist (cons "White" clist))
  (setq clist (cons "Magenta" clist))
  (setq clist (cons "Blue" clist))
  (setq clist (cons "Cyan" clist))
  (setq clist (cons "Green" clist))
  (setq clist (cons "Yellow" clist))
  (setq clist (cons "Red" clist))
  (setq clist (cons "By layer" clist))
 
  (start_list "list_col")
  (mapcar 'add_list clist)
  (end_list)

  ; show initial color in image tile, list box, and edit box
  (clist_act colorstr)
  (cedit_act colorstr)

  ; Define the action to take when the user presses various buttons.
  ;   $value will be substituted with the current value from the
  ;   dialog widget, such as "4" from the 5th list box item
  ;   (zero based).
  ;
  (action_tile "list_col" "(clist_act $value)")
  (action_tile "edit_col" "(cedit_act $value)")
  (if (= 1 (start_dialog))
    ; User pressed OK
    (if (/= coloridx 0)(command "_.color" coloridx)(command "_.color" "_bylayer")))
  (unload_dialog sc_dcl_id)     ;free DCL from memory
)

; List selections end up here
(defun clist_act (value)
  ; update the edit box
  (set_tile "edit_col" value)
  (setq coloridx (atoi value))
  (color_tile "show_image" coloridx)
)

; Text entry selections end up here
(defun cedit_act (value)
  ; update the list box
  (set_tile "list_col" value)
  (setq coloridx (atoi value))
  (color_tile "show_image" coloridx)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;            General Purpose LISP PDB Functions
;
;
;   The get_ and set_ functions below assume that the tile key 
;   (button or edit box name) is the same as the AutoCAD 
;   variable name.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Get integer variable from AutoCAD and display in dialog

(defun set_tile_int (varname)
  (setq vint (getvar varname))
  (set_tile varname (itoa vint))
)


; Get floating point variable from AutoCAD and display in dialog

(defun set_tile_real (varname)
  (setq vreal (getvar varname))
  (set_tile varname (rtos vreal))
)


; Get integer variable from dialog and set in AutoCAD

(defun get_tile_int (varname)
   (setvar varname (atoi (get_tile varname)))
)

; Get floating point variable from dialog and set in AutoCAD

(defun get_tile_real (varname)
   (setvar varname (distof (get_tile varname)))
)



; Color a tile and show a border around it

(defun color_tile (tile color)
  (setq x (dimx_tile tile))
  (setq y (dimy_tile tile))
  (start_image tile)
  (fill_image 0 0 x y color)
  (tile_rect 0 0 x y 7)
  (end_image)
)

; Draw a rectangle in a tile (assumes start_image has been called)

(defun tile_rect (x1 y1 x2 y2 color)
  (setq x2 (- x2 1))
  (setq y2 (- y2 1))
  (vector_image x1 y1 x2 y1 color)
  (vector_image x2 y1 x2 y2 color)
  (vector_image x2 y2 x1 y2 color)
  (vector_image x1 y2 x1 y1 color)
)
