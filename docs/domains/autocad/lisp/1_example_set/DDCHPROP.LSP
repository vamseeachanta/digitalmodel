;;;----------------------------------------------------------------------------
;;;
;;;   DDCHPROP.LSP   Version 0.5
;;;
;;;   Copyright (C) 1991-1992 by Autodesk, Inc.
;;;
;;;   Permission to use, copy, modify, and distribute this software
;;;   for any purpose and without fee is hereby granted, provided
;;;   that the above copyright notice appears in all copies and that
;;;   both that copyright notice and this permission notice appear in
;;;   all supporting documentation.
;;;
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED
;;;   WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.
;;;
;;;   2 February 1992
;;;   
;;;----------------------------------------------------------------------------
;;;   DESCRIPTION
;;;----------------------------------------------------------------------------
;;;   C:DDCHPROP is a dialogue interface for the CHPROP command.
;;;
;;;   The command looks similar to DDEMODES.  The main dialogue has an image 
;;;   tile, 3 buttons (layer, color, linetype), and an editbox (thickness).   
;;;   The 3 buttons each launch a sub-dialogue containing a list and edit box.  
;;;   The dialogues are all defined in the DDCHPROP.DCL file.
;;;
;;;
;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;;   Prefixes in command and keyword strings: 
;;;      "."  specifies the built-in AutoCAD command in case it has been        
;;;           redefined.
;;;      "_"  denotes an AutoCAD command or keyword in the native language
;;;           version, English.
;;;----------------------------------------------------------------------------
;;;
;;;
;;; ===========================================================================
;;; ===================== load-time error checking ============================
;;;

  (defun ai_abort (app msg)
     (defun *error* (s)
        (if old_error (setq *error* old_error))
        (princ)
     )
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       )
     )
     (exit)
  )

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

  (cond
     (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.

     (  (not (findfile "ai_utils.lsp"))                     ; find it
        (ai_abort "DDCHPROP"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "DDCHPROP" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "DDCHPROP" nil)        ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================

;;; Initialize program subroutines and variables.

(defun ddchprop_init ()

  ;;
  ;; Define buttons and set values in CHPROP dialogue box 
  ;;
  (defun call_chp ()
    (if (not (new_dialog "ch_prop" dcl_id)) (exit))
    (set_tile "error" "")
    ;; Set initial dialogue tile values
    (set_col_tile)
    (if (= lay-idx nil)
      (set_tile "t_layer" "Varies")
      (set_tile "t_layer" (nth lay-idx laynmlst))
    )
    (cond 
      ((= lt-idx nil)
        (set_tile "t_ltype" "Varies")
      )
      ((= lt-idx 0) ; set tile "By layer & layer linetype"
        (set_tile "t_ltype" (bylayer_lt))
      )
      (T 
        (set_tile "t_ltype" (nth lt-idx ltnmlst))
      )
    )
    (if (or (= ethickness nil) (= ethickness "Varies"))
      (set_tile "eb_thickness" "Varies")
      (set_tile "eb_thickness" (rtos ethickness))
    )
    ;; Define action for tiles
    (action_tile "b_color" "(setq ecolor (getcolor))")
    (action_tile "show_image" "(setq ecolor (getcolor))")
    (action_tile "b_name" "(setq elayer (getlayer))")
    (action_tile "b_line" "(setq eltype (getltype))")
    (action_tile "eb_thickness"  "(getthickness $value)")
    (action_tile "help" "(acad_helpdlg \"acad.hlp\"  \"ddchprop\")")
    (action_tile "accept" "(test-ok)")
    (if (= (start_dialog) 1)
      (progn
        (command "_.chprop" ss "")
        (if ecolor
          (progn
            (if (= 0 ecolor )   (setq ecolor "BYBLOCK"))
            (if (= 256 ecolor ) (setq ecolor "BYLAYER"))
            (command "_c" ecolor)
          )
        )
        (if (and (/= eltype "Varies") lt-idx)
          (command "_lt" eltype)
        )
        (if (and (/= elayer "Varies") lay-idx)
          (command "_la" elayer)
        )
        (if (and (/= ethickness "Varies") ethickness)
          (command "_t" ethickness)
        )
        (command "")
      )
      (princ "\nProperties unchanged")
    )
    (princ)
  )
  ;;
  ;; Function to set the Color text tile and swab to the current color value.
  ;;
  (defun set_col_tile()
    (cond 
      ((= ecolor nil)
        (set_tile "t_color" "Varies")
        (col_tile "show_image" 0 nil)
      )
      ((= ecolor 0)
        (set_tile "t_color" "BYBLOCK")
        (col_tile "show_image" 0 nil)
      )
      ((= ecolor 1)
        (set_tile "t_color" "1 red")
        (col_tile "show_image" 1 nil)
      )
      ((= ecolor 2)
        (set_tile "t_color" "2 yellow")
        (col_tile "show_image" 2 nil)
      )
      ((= ecolor 3)
        (set_tile "t_color" "3 green")
        (col_tile "show_image" 3 nil)
      )
      ((= ecolor 4)
        (set_tile "t_color" "4 cyan")
        (col_tile "show_image" 4 nil)
      )
      ((= ecolor 5)
        (set_tile "t_color" "5 blue")
        (col_tile "show_image" 5 nil)
      )
      ((= ecolor 6)
        (set_tile "t_color" "6 magenta")
        (col_tile "show_image" 6 nil)
      )
      ((= ecolor 7)
        (set_tile "t_color" "7 white")
        (col_tile "show_image" 7 nil)
      )
      ;; If the color is "BYLAYER", then set the tile to
      ;; show it's set By layer, but also indicate the
      ;; color of the layer - i.e. By layer (red)
      ((= ecolor 256)
        (set_tile "t_color" (bylayer_col))
        (col_tile "show_image" cn nil)
      )
      (T 
        (set_tile "t_color" (itoa ecolor))
        (col_tile "show_image" ecolor nil)
      )
    )
  )
  ;;
  ;;  Function to put up the standard color dialogue.
  ;;
  (defun getcolor(/ col_def temp_color)
    ;; col_def is the default color used when rq_color is called.  If ecolor 
    ;; is nil (varies) then set it to 1, else use the value of ecolor.
    (if ecolor
      (setq col_def ecolor)
      (setq col_def 1)
    )
    (if (numberp (setq temp_color (acad_colordlg col_def t)))
      (progn
        (setq ecolor temp_color)
        (set_col_tile)
        ecolor
      )
      ecolor
    )  
  )
  ;;
  ;; This function pops a dialogue box consisting of a list box, image tile, 
  ;; and edit box to allow the user to select or type a linetype.  It returns 
  ;; the linetype selected.
  ;;
  (defun getltype (/ old-idx ltname)
    ;; Initialize a dialogue from dialogue file
    (if (not (new_dialog "setltype" dcl_id)) (exit))
    (start_list "list_lt")
    (mapcar 'add_list ltnmlst)         ; initialize list box
    (end_list)
    (setq old-idx lt-idx)
    ;; Show initial ltype in image tile, list box, and edit box
    (if (/= lt-idx nil)
      (ltlist_act (itoa lt-idx))
      (progn
        (set_tile "edit_lt" "Varies")
        (col_tile "show_image" 0 nil)
      )
    )
    (action_tile "list_lt" "(ltlist_act $value)")
    (action_tile "edit_lt" "(ltedit_act $value)")
    (action_tile "accept" "(test-ok)")
    (action_tile "cancel" "(reset-lt)")
    (if (= (start_dialog) 1)           ; User pressed OK
      (cond 
        ((= lt-idx nil)
          (set_tile "t_ltype" "Varies")
          "Varies"
        )
        ((= lt-idx 0)
          (set_tile "t_ltype" (bylayer_lt))
          "BYLAYER"
        )
        ((= lt-idx 1)
          (set_tile "t_ltype" "BYBLOCK")
          "BYBLOCK"
        )
        (T  
          (set_tile "t_ltype" ltname) 
          ltname
        )
      )
      eltype
    )
  )
  ;;
  ;; Edit box entries end up here
  ;;
  (defun ltedit_act (ltvalue)
    ;; If linetype name,is valid, then clear error string, 
    ;; call ltlist_act function, and change focus to list box.
    ;; Else print error message.
    (setq ltvalue (strcase ltvalue))
    (if (or (= ltvalue "BYLAYER") (= ltvalue "BY LAYER"))
      (setq ltvalue "BYLAYER")
    )
    (if (or (= ltvalue "BYBLOCK") (= ltvalue "BY BLOCK"))
      (setq ltvalue "BYBLOCK")
    )
    (if (setq lt-idx (getindex ltvalue ltnmlst))
      (progn
        (set_tile "error" "")
        (ltlist_act (itoa lt-idx))
        (mode_tile "list_lt" 2)
      )
      (progn
        (if (/= ltvalue "VARIES")
          (set_tile "error" "Invalid linetype.")
        )
        (setq lt-idx old-idx)
      ) 
    )
  )
  ;;
  ;; List selections end up here
  ;;
  (defun ltlist_act (index / dashdata)
    ;; Update the list box, edit box, and color tile
    (set_tile "error" "")
    (setq lt-idx (atoi index))
    (setq ltname (nth lt-idx ltnmlst))
    (setq dashdata (nth lt-idx mdashlist))
    (col_tile "show_image" 0 dashdata)
    (set_tile "list_lt" (itoa lt-idx))
    (set_tile "edit_lt" ltname)
  )
  ;;
  ;; Reset to original linetype when cancel it selected
  ;;
  (defun reset-lt ()
    (setq lt-idx old-idx)
    (done_dialog 0)
  )
  ;;
  ;; This function pops a dialogue box consisting of a list box and edit box to 
  ;; allow the user to select or type a layer name.  It returns the layer name 
  ;; selected.  It also the status (On, Off, Frozen, etc.) of all layer in the 
  ;; drawing.
  ;;
  (defun getlayer (/ old-idx layname on off frozth linetype colname)
    ;; Load a dialogue from dialogue file
    (if (not (new_dialog "setlayer" dcl_id)) (exit))
    (start_list "list_lay")
    (mapcar 'add_list longlist)        ; initialize list box
    (end_list)
    ;; Display current layer, show initial layer name in edit 
    ;; box, and highlight list box.
    (setq old-idx lay-idx)
    (if (/= lay-idx nil) (laylist_act (itoa lay-idx)))
    (set_tile "cur_layer" (getvar "clayer"))
    (action_tile "list_lay" "(laylist_act $value)")
    (action_tile "edit_lay" "(layedit_act $value)")
    (action_tile "accept" "(test-ok)")
    (action_tile "cancel" "(reset-lay)")
    (if (= (start_dialog) 1)           ; User pressed OK
      (progn
        (if (= lay-idx nil) (setq layname "Varies"))
        (set_tile "t_layer" layname)
        ; If layer or ltype equals bylayer reset their tiles
        (if (= lt-idx 0)
          (set_tile "t_ltype" (bylayer_lt))
        )
        (if (= ecolor 256)
          (progn
            (set_tile "t_color" (bylayer_col))
            (col_tile "show_image" cn nil)
          )
        )
        layname
      )
      elayer
    )
  )
  ;;
  ;; Edit box selections end up here
  ;;
  (defun layedit_act (layvalue)
    ;; Convert layer entry to upper case.  If layer name is
    ;; valid, clear error string, call (laylist_act) function,
    ;; and change focus to list box.  Else print error message.
    (setq layvalue (strcase layvalue))
    (if (setq lay-idx (getindex layvalue laynmlst))
      (progn
        (set_tile "error" "")
        (laylist_act (itoa lay-idx))
      )
      (progn
        (set_tile "error" "Invalid layer name.")
        (setq lay-idx old-idx)
      )
    )
  )
  ;;
  ;; List entry selections end up here
  ;;
  (defun laylist_act (index / layinfo color dashdata)
    ;; Update the list box, edit box, and color tile
    (set_tile "error" "")
    (setq lay-idx (atoi index))
    (setq layname (nth lay-idx laynmlst))
    (setq layinfo (tblsearch "layer" layname))
    (if (= (logand (cdr (assoc 70 layinfo)) 4) 4)
      (set_tile "error" "Cannot change entity to locked layer.")
      (progn
        (setq color (cdr (assoc 62 layinfo)))
        (setq color (abs color))
        (setq colname (colorname color))
        (set_tile "list_lay" (itoa lay-idx))
        (set_tile "edit_lay" layname)
        (mode_tile "list_lay" 2)
      )
    )
  )
  ;;
  ;; Reset to original layer when cancel is selected
  ;;
  (defun reset-lay ()
    (setq lay-idx old-idx)
    (done_dialog 0)
  )
  ;;
  ;; Checks validity of thickness from edit box. Since (atof) returns 0 when a 
  ;; string can't be converted to a real, this routine checks if the first 
  ;; character is "0".  It also checks to see if the value equals "Varies".
  ;;
  (defun getthickness (value)
    (setq value (strcase value))
    (if (or (= value "VARIES")
            (distof value)
        )
      (progn
        (set_tile "error" "")
        (if (= value "VARIES")
          (progn
            (set_tile "eb_thickness" "Varies")
            (setq ethickness nil)
          )
          (progn
            (setq ethickness (distof value))
            (set_tile "eb_thickness" (rtos ethickness))
            ethickness
          )
        )
      )
      (progn
        (set_tile "error" "Invalid thickness.")
        nil
      )
    )
  )
  ;;
  ;; This function make a list called laynmlst which consists of all the layer 
  ;; names in the drawing.  It also creates a list called longlist which 
  ;; consists of strings which contain the layer name, color, linetype, etc.  
  ;; Longlist is later mapped into the layer listbox.  Both are ordered the 
  ;; same.
  ;;
  (defun makelaylists (/ layname onoff frozth color linetype vpf vpn ss cvpname 
                         xdlist vpldata sortlist name templist bit-70
                      )
    (if (= (setq tilemode (getvar "tilemode")) 0)
      (progn
        (setq ss (ssget "x" (list (cons 0 "VIEWPORT")
                                  (cons 69 (getvar "CVPORT"))
                            )
                 )
        )     
        (setq cvpname (ssname ss 0))
        (setq xdlist (assoc -3 (entget cvpname '("acad"))))
        (setq vpldata (cdadr xdlist))
      )
    )
    (setq sortlist nil)
    (setq templist (tblnext "LAYER" T))
    (while templist
      (setq name (cdr (assoc 2 templist)))
      (setq sortlist (cons name sortlist))
      (setq templist (tblnext "LAYER"))
    ) 
    (if (>= (getvar "maxsort") (length sortlist))
      (setq sortlist (acad_strlsort sortlist))
      (setq sortlist (reverse sortlist))
    )
    (setq laynmlst sortlist)
    (setq longlist nil)
    (setq layname (car sortlist))
    (while layname
      (setq laylist (tblsearch "LAYER" layname))
      (setq color (cdr (assoc 62 laylist)))
      (if (minusp color)
        (setq onoff ".")
        (setq onoff "On")
      )
      (setq color (abs color))
      (setq colname (colorname color))
      (setq bit-70 (cdr (assoc 70 laylist)))
      (if (= (logand bit-70 1) 1)
        (setq frozth "F" fchk laylist)
        (setq frozth ".")
      )
      (if (= (logand bit-70 2) 2)
        (setq vpn "N")
        (setq vpn ".")
      )
      (if (= (logand bit-70 4) 4)
        (setq lock "L")
        (setq lock ".")
      )
      (setq linetype (cdr (assoc 6 laylist)))
      (setq layname (substr layname 1 31))
      (if (= tilemode 0)
        (progn
          (if (member (cons 1003 layname) vpldata)
            (setq vpf "C")
            (setq vpf ".")
          )
        )
        (setq vpf ".")
      )
      (setq ltabstr (strcat layname "\t"
                              onoff "\t"
                             frozth "\t"
                               lock "\t"
                                vpf "\t"
                                vpn "\t"
                            colname "\t"
                           linetype
                    )
      )
      (setq longlist (append longlist (list ltabstr)))
      (setq sortlist (cdr sortlist))
      (setq layname (car sortlist))
    )
  )
  ;;
  ;; This function makes 2 lists - ltnmlst & mdashlist.  Ltnmlst is a list of 
  ;; linetype names read from the symbol table.  Mdashlist is list consisting 
  ;; of lists which define the linetype pattern - numbers that indicate dots, 
  ;; dashes, and spaces taken from group code 49.  The list corresponds to the 
  ;; order of names in ltnmlst.
  ;;
  (defun makeltlists (/ ltlist ltname)
    (setq mdashlist nil)
    (setq ltlist (tblnext "LTYPE" T))
    (setq ltname (cdr (assoc 2 ltlist)))
    (setq ltnmlst (list ltname))

    (if (= ltname "CONTINUOUS")
      (setq mdashlist (list "CONT"))
      (setq mdashlist 
            (append mdashlist (list (add-mdash ltlist)))
      )
    )
    (while (setq ltlist (tblnext "LTYPE"))
           (setq ltname (cdr (assoc 2 ltlist)))
           (setq ltnmlst (append ltnmlst (list ltname)))
           (setq mdashlist 
                (append mdashlist (list (add-mdash ltlist)))
           )
    )
    (setq ltnmlst (cons "BYBLOCK" ltnmlst))
    (setq mdashlist  (cons nil mdashlist))
    (setq ltnmlst (cons "BYLAYER" ltnmlst))
    (setq mdashlist  (cons nil mdashlist))
  )
  ;;
  ;; Get all the group code 49 values for a linetype and put them in a list 
  ;; (pen-up, pen-down info)
  ;;
  (defun add-mdash (ltlist1 / dashlist assoclist dashsize)
    (setq dashlist nil)
    (while (setq assoclist (car ltlist1))
      (if (= (car assoclist) 49)
        (progn
          (setq dashsize (cdr assoclist))
          (setq dashlist (cons dashsize dashlist))
        )
      )
      (setq ltlist1 (cdr ltlist1))
    )
    (setq dashlist (reverse dashlist))
  )
  ;;
  ;; Color a tile, draw linetype, and draw a border around it
  ;;
  (defun col_tile (tile color patlist / x y)
    (setq x (dimx_tile tile))
    (setq y (dimy_tile tile))
    (start_image tile)
    (fill_image 0 0 x y color)
    (if (= color 7)
      (progn
        (if patlist (drawpattern x (/ y 2) patlist 0))
        (tile_rect 0 0 x y 0)
      )
      (progn
        (if patlist (drawpattern x (/ y 2) patlist 7))
        (tile_rect 0 0 x y 7)
      )
    )
    (end_image)
  )
  ;;
  ;; Draw a border around a tile
  ;;
  (defun tile_rect (x1 y1 x2 y2 color)
    (setq x2 (- x2 1))
    (setq y2 (- y2 1))
    (vector_image x1 y1 x2 y1 color)
    (vector_image x2 y1 x2 y2 color)
    (vector_image x2 y2 x1 y2 color)
    (vector_image x1 y2 x1 y1 color)
  )
  ;;
  ;; Draw the linetype pattern in a tile.  Boxlength is the length of the image 
  ;; tile, y2 is the midpoint of the height of the image tile, pattern is a 
  ;; list of numbers that define the linetype, and color is the color of the 
  ;; tile.
  ;;
  (defun drawpattern (boxlength y2 pattern color / x1 x2
                      patlist dash)
    (setq x1 0 x2 0)
    (setq patlist pattern)
    (if (= patlist "CONT")
      (progn 
        (setq dash boxlength)
        (vi)
        (setq x1 boxlength)
      )
    )
    (while (< x1 boxlength)
      (if (setq dash (car patlist))
        (progn
          (setq dash (fix (* 30 dash)))
          (cond 
            ((= dash 0) 
              (setq dash 1) 
              (vi)
            )
            ((> dash 0) 
              (vi)
            )
            (T 
              (if (< (abs dash) 2) (setq dash 2))
              (setq x2 (+ x2 (abs dash)))
            )
          )
          (setq patlist (cdr patlist))
          (setq x1 x2)
        )
        (setq patlist pattern)
      )
    )
  )
  ;;
  ;; Draw a dash or dot in image tile
  ;;
  (defun vi ()
    (setq x2 (+ x2 dash))
    (vector_image x1 y2 x2 y2 color)
  )
  ;;
  ;; This function takes a selection and returns a list of the color, linetype, 
  ;; layer, and thickness properties that are common to every entities in the
  ;; selection set - (color linetype layer thickness).  If all entities do not 
  ;; share the same property value it returns "Varies" in place of the 
  ;; property value.  i.e. ("BYLAYER" "DASHED" "Varies" 0)
  ;;
  (defun getprops (selset / sslen elist color ltype layer
                            thickness go chk-col chk-lt chk-lay chk-th ctr)
    (setq sslen (sslength selset))
    (setq elist (entget (ssname selset 0)))
    (setq color (cdr (assoc 62 elist)))
    (if (not color) (setq color 256))
    (setq ltype (cdr (assoc 6 elist)))
    (if (not ltype) (setq ltype "BYLAYER"))
    (setq layer (cdr (assoc 8 elist)))
    (setq thickness (cdr (assoc 39 elist)))
    (if (not thickness) (setq thickness 0))
    (setq go T chk-col T chk-lt T chk-lay T chk-th T ctr 1)

    ;; Page through the selection set.  When a property
    ;; does not match, stop checking for that property.
    ;; If all properties vary, stop paging.

    (while (and (> sslen ctr) go)
      (setq elist (entget (setq en (ssname selset ctr))))
      (if chk-col (match-col))
      (if chk-lt (match-lt))
      (if chk-lay (match-lay))
      (if chk-th (match-th))
      (setq ctr (1+ ctr))
      (if (and (not chk-col) (not chk-lt) (not chk-lay) (not chk-th))
        (setq go nil)
      )
    )
    (list color ltype layer thickness)
  )

  (defun match-col (/ ncolor)
    (setq ncolor (cdr (assoc 62 elist)))
    (if (not ncolor) (setq ncolor 256))
    (if (/= color ncolor)
      (progn
        (setq chk-col nil)
        (setq color nil)
      )
    )
  )

  (defun match-lt (/ nltype)
    (setq nltype (cdr (assoc 6 elist)))
    (if (not nltype) (setq nltype "BYLAYER"))
    (if (/= ltype nltype)
      (progn
        (setq chk-lt nil)
        (setq ltype "Varies")
      )
    )
  )

  (defun match-lay (/ nlayer)
    (setq nlayer (cdr (assoc 8 elist)))
    (if (/= layer nlayer)
      (progn
        (setq chk-lay nil)
        (setq layer "Varies")
      )
    )
  )

  (defun match-th (/ nthickness)
    (setq nthickness (cdr (assoc 39 elist)))
    (if (not nthickness) (setq nthickness 0))
    (if (/= thickness nthickness)
      (progn
        (setq chk-th nil)
        (setq thickness "Varies")
      )
    )
  )

  ;;
  ;; If an item is a member of the list, then return its index number, else 
  ;; return nil.
  ;;
  (defun getindex (item itemlist / m n)
    (setq n (length itemlist))
    (if (> (setq m (length (member item itemlist))) 0)
        (- n m)
        nil
    )
  )
  ;;
  ;; This function is called if the linetype is set "BYLAYER". It finds the 
  ;; ltype of the layer so it can be displayed beside the linetype button.
  ;;
  (defun bylayer_lt (/ layname layinfo ltype)
    (if lay-idx
      (progn
        (setq layname (nth lay-idx laynmlst))
        (setq layinfo (tblsearch "layer" layname))
        (setq ltype (cdr (assoc 6 layinfo)))
        (strcat "BYLAYER (" ltype ")")
      )
      "BYLAYER"
    )
  )
  ;;
  ;; This function is called if the color is set "BYLAYER".  It finds the 
  ;; color of the layer so it can be displayed  beside the color button.
  ;;
  (defun bylayer_col (/ layname layinfo color)
    (if lay-idx
      (progn
        (setq layname (nth lay-idx laynmlst))
        (setq layinfo (tblsearch "layer" layname))
        (setq color (abs (cdr (assoc 62 layinfo))))
        (setq cn color)
        (strcat "BYLAYER (" (colorname color) ")")
      )
      (progn
        (setq cn 0)
        "BYLAYER"
      )
    )
  )
  ;;
  ;; If there is no error message, then close the dialogue
  ;;
  ;; If there is an error message, then set focus to the tile
  ;; that's associated with the error message.
  ;;
  (defun test-ok ( / errtile)
    (setq errtile (get_tile "error"))
    (cond
      (  (= errtile "")
         (done_dialog 1))
      (  (= errtile "Invalid thickness.")
         (mode_tile "eb_thickness" 2))
    )
  )
  ;;
  ;; A color function used by getlayer.
  ;;
  (defun colorname (colnum)
    (setq cn (abs colnum))
    (cond ((= cn 1) "red")
          ((= cn 2) "yellow")
          ((= cn 3) "green")
          ((= cn 4) "cyan")
          ((= cn 5) "blue")
          ((= cn 6) "magenta")
          ((= cn 7) "white")
          (T (itoa cn))
    )
  )

;;; Construct layer and ltype lists and initialize all
;;; program variables:

  (makelaylists)                     ; layer list - laynmlst
  (makeltlists)                      ; linetype lists - ltnmlst, mdashlist
  ;; Find the property values of the selection set.
  ;; (getprops ss) returns a list of properties from
  ;; a selection set - (color ltype layer thickness).
  (setq proplist (getprops ss))
  (setq ecolor (car proplist))
  (setq eltype (cadr proplist))
  (setq elayer (caddr proplist))
  (setq ethickness (cadddr proplist))
  ;; Find index of linetype, and layer lists
  (cond
    ((= eltype "Varies") (setq lt-idx nil))
;   ((= eltype "BYLAYER") (setq lt-idx (getindex "BYLAYER" ltnmlst)))
;   ((= eltype "BYBLOCK") (setq lt-idx (getindex "BYBLOCK" ltnmlst)))
    (T (setq lt-idx (getindex eltype ltnmlst)))
  )
  (if (= elayer "Varies")
      (setq lay-idx nil)
      (setq lay-idx (getindex elayer laynmlst))
  )
  (if (= ethickness "Varies")
      (setq ethickness nil)
  )

)   ; end (ddchprop_init)

;;; (ddchprop_select)
;;;
;;; Aquires selection set for DDCHPROP, in one of three ways:
;;;
;;;   1 - Autoselected.
;;;   2 - Prompted for.
;;;   3 - Passed as an argument in a call to (ddchprop <ss> )
;;;
;;; The (ddchprop_select) function also sets the value of the
;;; global symbol AI_SELTYPE to one of the above three values to
;;; indicate the method thru which the entity was aquired.


(defun ddchprop_select ()
   (cond
      (  (and ss (eq (type ss) 'pickset))        ; selection set passed to
         (cond                                   ; (ddchprop) as argument
            (  (not (zerop (sslength ss)))       ;   If not empty, then
               (setq ai_seltype 3)               ;   then return pickset.
               (ai_return ss))))

      (  (ai_aselect))                          ; Use current selection
                                                ; set or prompt for objects

      (t (princ "\nNothing selected.")
         (ai_return nil))
   )
)

;;; Define command function.

(defun C:DDCHPROP ()
   (ddchprop nil)
   (princ)
)


;;; Main program function - callable as a subroutine.
;;;
;;; (ddchprop <pickset> )
;;;
;;; <pickset> is the selection set of objects to be changed.
;;;
;;; If <pickset> is nil, then the current selection set is
;;; aquired, if one exists.  Otherwise, the user is prompted
;;; to select the objects to be changed.
;;;
;;; Before (ddchprop) can be called as a subroutine, it must
;;; be loaded first.  It is up to the calling application to
;;; first determine this, and load it if necessary.

(defun ddchprop (ss  /

                  add-mdash      ecolor          ltedit_act      s
                  assoclist      elayer          ltidx           selset
                  bit-70         elist           ltlist          set_col_tile
                  boxlength      eltype          ltlist1
                  bylayer-lt     en              ltlist_act      sortlist
                  bylayer_col    ethickness      ltname
                  bylayer_lt     fchk            ltnmlst         sslen
                  call_chp       frozth          ltvalue         templist
                  chk-col        getcolor        ltype           temp_color
                  chk-lay        getindex        m               test-ok
                  chk-lt         getlayer        makelaylists    testidx
                  chk-th         getltype        makeltlists     testlay
                  cmd            getprops        match-col       th-value
                  cmdecho        getthickness    match-in        thickness
                  cn             globals         match-lay       tile
                  cnum           go              match-lt        tilemode
                  col-idx        index           match-th        tile_rect
                  colname        item            match_col       vi
                  colnum         item1           mdashlist       vpf
                  color          item2           n               vpldata
                  colorname      itemlist        name            vpn
                  col_def        lay-idx         ncolor          x
                  col_tile       layedit_act     nlayer          x1
                  ctr            layer           nltype          x2
                  cvpname        layinfo         nthickness      xdlist
                  dash           laylist         off             y
                  dashdata       laylist_act     old-idx         y1
                  dashlist       layname         olderr          y2
                  dashsize       laynmlst        on              undo_init
                  dcl_id         layvalue        onoff
                                 linetype        patlist
                  ddchprop-err   list1           pattern
                  longlist       proplist
                  lt-idx         reset-lay
                  drawpattern    ltabstr         reset-lt
                )

  (setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
  )

  (setvar "cmdecho" 0)

  (cond
     (  (not (ai_notrans)))                      ; Not transparent?
     (  (not (ai_acadapp)))                      ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "ddchprop")))) ; is .DCL file loaded?
     (  (not (setq ss (ddchprop_select))))       ; objects to modify?

     (t (ai_undo_push)
        (ddchprop_init)                          ; Everything's cool,
        (call_chp)                               ; so proceed!
        (ai_undo_pop)
     )
  )
  
  (setq *error* old_error) 
  (setvar "cmdecho" old_cmd)
  (princ)
)

;;;----------------------------------------------------------------------------

(princ "   DDCHPROP loaded.")
(princ)

