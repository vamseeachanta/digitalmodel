;;;----------------------------------------------------------------------------
;;;
;;;    EXCHPROP.LSP    
;;;    Polyline and text modification capabilities added by 
;;;    Randy Kintzley  
;;; 
;;;    Copyright (C) 1997 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;.
;;;    28 February 1997
;;;
;;;
;;;----------------------------------------------------------------------------
;;;   DESCRIPTION
;;;----------------------------------------------------------------------------
;;;   C:EXCHPROP is an extended or enhanced version of ddchprop. This  
;;;   command gives the user the abilitie to change several characteristics 
;;;   of selected polyline and text objects. 
;;;       The style and height of selectected text objects can be modified 
;;;   (including text, mtext and attribute definitions) as well as, width and 
;;;   elevation characteristics of selected polylines (includes lightweight and 
;;;   traditional polylines.)
;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;;   Prefixes in command and keyword strings:
;;;      "."  specifies the built-in AutoCAD command in case it has been
;;;           redefined.
;;;      "_"  denotes an AutoCAD command or keyword in the native language
;;;           version, English.
;;;----------------------------------------------------------------------------
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions created as result of loading file: exchprop.lsp
; DDCHPROP2
; DDCHPROP2_INIT
; DDCHPROP2_SELECT
;
;Variables created as result of loading file: exchprop.lsp
; OLD_ALLOC
;
;Functions created as a result of executing the commands in: exchprop.lsp
;
;Variables created as a result of executing the commands in: exchprop.lsp
; AI_SELTYPE
; BONUS_ALIVE
; BONUS_OLD_ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Avoid (gc)s on load to improve load time.
;;;
(defun do_alloc (/ old_allod new_alloc)
  (setq old_alloc (alloc 2000) new_alloc (alloc 2000))
  (expand (1+ (/ 4750 new_alloc)))
  (alloc old_alloc)
);defun

;runs at load time - rk
(do_alloc)
(setq do_alloc nil)
;;;
;;; ===========================================================================
;;; ===================== load-time error checking ============================
;;;

  (defun ai_abort (app msg) 
     (defun *error* (s)
      (if old_error (setq *error* old_error))
      (princ)
     );defun
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       );alert
     );if
     ;(*error* msg)
     (exit)
  );defun ai_abort

;runs at load time - rk
;(if (and *error*      ;added the if wrapper around this - rk.
;         (not old_error)       
;    );and 
;    (setq old_error *error*);setq
;);if


;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

;runs at load time - rk.
(cond
 (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.
 (  (not (findfile "ai_utils.lsp"))                     ; find it
    (ai_abort "EXCHPROP"
              (strcat "Can't locate file AI_UTILS.LSP."
                      "\n Check support directory.")
    );ai_abort
 )
 (  (eq "failed" (load "ai_utils" "failed"))            ; load it
    (ai_abort "EXCHPROP" "Can't load file AI_UTILS.LSP")
 )
);cond close

(if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
    (ai_abort "EXCHPROP" nil)       ; a Nil <msg> supresses
);if                                 ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================

;;; Initialize program subroutines and variables.

(defun ddchprop2_init()



  ;;
  ;; Define buttons and set values in CHPROP dialogue box
  ;;
  (defun call_chp2 (/ cmdact p1 p2)

    (if (not (new_dialog "ch_prop" dcl_id)) 
        (exit)
    )

    (set_tile "error" "")
    ;; Set initial dialogue tile values
    (set_col_tile)
    (set_tile "t_layer" elayer)

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

    (if (or (= ethickness nil)
            (= ethickness "")
            (= ethickness "Varies") 
        );or
        (set_tile "eb_thickness" "")
        (set_tile "eb_thickness" (ai_rtos ethickness))
    );if
    (if (or (= eltscale nil)
            (= eltscale "")
            (= eltscale "Varies")
 
        )
        (set_tile "eb_ltscale" "")
        (set_tile "eb_ltscale" (ai_rtos eltscale))
    )
    (if
      (numberp ewidth)
      (set_tile "poly_wid" (ai_rtos ewidth))
      (set_tile "poly_wid" ewidth)
    )
    (if
      (numberp eelevation)
      (set_tile "poly_elev" (ai_rtos eelevation))
      (set_tile "poly_elev" eelevation)
    )
    (if
      (numberp eheight)
      (set_tile "text_hgt" (ai_rtos eheight))
      (set_tile "text_hgt" eheight)
    )
    (if (not estyle)
        (setq estyle "")
    );if 
    (setq hair_style_list (tnlist '("style" 16)));setq
    (if (not (member estyle hair_style_list))
        (setq hair_style_list (append hair_style_list (list estyle)));setq
    );if 

    (setq hair_style_list (acad_strlsort hair_style_list));setq
    (mpoplst "text_style" hair_style_list)
    (set_tile "text_style" 
              (itoa (position estyle hair_style_list))
    );set_tile 

    ;; Disable tiles if need be...  
    (setq a 0)
    (while ( < a  (sslength ss))
      (setq which_tiles
            (ai_common_state (cdr (assoc '0 (entget (ssname ss a))))))
    
      ;; If all fields are enabled, don't bother checking anymore.
      (if (/= which_tiles (logior 1 2 4 8 16))
        (setq a (1+ a))
        (setq a (sslength ss))
      )
    )
    ;; Layer Button and Text Field

    (if (/= 1 (logand 1 which_tiles))
      (progn
        (mode_tile "t_layer" 1)
        (mode_tile "b_name" 1)
      )
    )

    ;; Color Button and Text Field
    (if (/= 2 (logand 2 which_tiles))
      (progn
        (mode_tile "t_color" 1)
        (mode_tile "b_color" 1)
        (mode_tile "show_image" 1)
      )
    )
    ;; Linetype Button and Text Field
    (if (/= 4 (logand 4 which_tiles))
      (progn
        (mode_tile "t_ltype" 1)
        (mode_tile "b_line" 1)
      )
    )
    ;; Linetype Scale Edit Field
    (if (/= 8 (logand 8 which_tiles))
      (progn
        (mode_tile "eb_ltscale" 1)
      )
    )
    ;; Thickness Edit Field.
    (if (/= 16 (logand 16 which_tiles))
      (progn
        (mode_tile "eb_thickness" 1)
      )
    )

    ;; Polyline box and tiles          
    (if (not (= 2 (logand 2 eflag)))
        (progn
         (mode_tile "text_hgt" 1)
         (mode_tile "text_style" 1)
        );progn
    );if
    (if (not (= 1 (logand 1 eflag)))
        (progn 
         (mode_tile "poly_wid" 1)
         (mode_tile "poly_elev" 1)
        );progn then disable polyline fields
    );if

    ;; Define action for tiles
    (action_tile "b_color" "(setq ecolor (getcolor))")
    (action_tile "show_image" "(setq ecolor (getcolor))")
    (action_tile "b_name" "(setq elayer (getlayer))")
    (action_tile "b_line" "(setq eltype (getltype))")
    (action_tile "eb_ltscale"  "(getscale $value)")
    (action_tile "eb_thickness"  "(getthickness $value)")
    (action_tile "poly_wid" "(getwidth $value)")
    (action_tile "poly_elev" "(getelevation $value)")
    (action_tile "text_hgt" "(getheight $value)")
    (action_tile "text_style" "(getstyle $value hair_style_list)") 
    (action_tile "help" "(help \"AC_BONUS.HLP\" \"EXCHPROP\")")
    (action_tile "accept" "(test-main-ok)")

    (if (= (start_dialog) 1)
      (progn
        ; Update special properties for polyline and text selection-sets.
        
        (if (and (= 1 (logand 1 eflag))   ; polylines rk chprop
                 (or ewidth
                     eelevation
                 );or
            );and
            (progn
             (setq  ss-index 0 
                   ss-length (sslength poly_ss)
             );setq
             (while (< ss-index ss-length)
               (setq ename (ssname poly_ss ss-index)
                     elist (entget ename)
               );setq
               (ucs_2_ent (cdr (assoc 210 elist)));this function lives in ac_bonus.lsp
               (if ewidth 
                   (command "_.pedit" ename "_W" ewidth "_x")
               );if 
               (if eelevation 
                   (progn
                    (setq p2 (list 0.0 0.0 eelevation));setq
                    (if (equal (cdr (assoc 0 elist)) "POLYLINE")
                        (setq p1 (list 0.0 0.0
                                       (caddr (cdr (assoc 10 elist)))
                                 );list
                        );setq
                        (progn
                         (if (assoc 38 elist)
                             (setq p1 (list 0.0 0.0
                                            (cdr (assoc 38 elist))
                                      );list
                             );setq
                             (setq p1 '(0.0 0.0 0.0))
                         );if
                        );progn 
                    );if
                    (command "_.move" ename "" p1 p2)
                   );progn then change the elevation of the polyline
               );if
               (command "_.ucs" "_p")
               (setq ss-index (1+ ss-index))
             );while
           );progn then polylines are in the selset
         );if
         (if (and (= 2 (logand 2 eflag))     ; text
                  (or eheight 
                      estyle
                  );or
             );and
             (progn
              (setq ss-index 0 ss-length (sslength txt_ss))
              (while
                (< ss-index ss-length)
                (setq elist (entget (setq ename (ssname txt_ss ss-index))))
                (if (numberp eheight)
                    (setq elist (subst (cons 40 eheight)
                                       (assoc 40 elist)
                                       elist
                                );subst
                    );setq
                );if
                (if (and estyle 
                         (not (equal estyle ""))
                         (not (equal estyle (cdr (assoc 7 elist))))
                    );and
                    (progn  
                     (setq elist (subst (cons 7 estyle)
                                        (assoc 7 elist)
                                        elist
                                 );subst
                     );setq else
                    );progn
                );if
                (entmod elist)
                (setq ss-index (1+ ss-index))
              );while
             );progn then
         );if

        (setq cmdact (getvar "cmdactive"))
        (command "_.chprop" ss "")
        (if (/= cmdact (getvar "cmdactive"))  ; Is CHPROP actually running?
          (progn
            (if ecolor
                (progn
                 (if (= 0 ecolor)   (setq ecolor ;|MSG0|;"BYBLOCK"))
                 (if (= 256 ecolor) (setq ecolor ;|MSG0|;"BYLAYER"))
                 (command "_c" ecolor)
                );progn then
            )
            (if (and lt-idx
                     (/= eltype ;|MSG0|;"Varies")
                )
                (command "_lt" eltype)
            )
            (if (and lay-idx 
                     (/= elayer ;|MSG0|;"Varies")
                )
                (command "_la" elayer)
            )
            (if (and ethickness 
                     (/= ethickness "")
                     (/= ethickness "Varies") 
                )
                (command "_t" ethickness)
            )
            (if (and  eltscale (/= eltscale ""))
                (command "_lts" eltscale)
            );if
            (command "")
          )
          (princ "\nProperties unchanged")  ; CHPROP didn't like our SS set
        )

      );progn then OK was picked in the dialog.
      
      ;; Fred GERBER - 25-AUG-94
      ;; Don't print the "Properties unchanged" message when the user cancels
      ;; the dialog because he knows that already (otherwise he would have
      ;; hit the "OK" button). Display the message only if CHPROP fails for
      ;; some reason, because it is not the expected behavior of the command.
      ;;
      ;; (princ ;|DDCHPROP2_LSP_8|;"\nProperties unchanged")
    );if
    (princ)
  );defun call_chp2
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
        (col_tile "show_image" 7 nil)    ; show BYBLOCK as white
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
  (defun getcolor(/ col_def lay_clr temp_color)
    ;; col_def is the default color used when rq_color is called.  If ecolor
    ;; is nil (varies) then set it to 1, else use the value of ecolor.
    (if ecolor
      (setq col_def ecolor)
      (setq col_def 1)
    )

    ;; If we're working with a single layer, get its color
    ;; for use in the color swatch if the user selects color BYLAYER.
    (if (/= elayer ;|MSG0|;"Varies")
      (setq lay_clr (cdr (assoc 62 (tblsearch "layer" elayer))))
      (setq lay_clr 0)
    )
    (if (numberp (setq temp_color (acad_colordlg col_def T lay_clr)))
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
         (set_tile "edit_lt" "")
         (col_tile "show_image" 0 nil)
        );progn else
    );if
    (action_tile "list_lt" "(ltlist_act $value)")
    (action_tile "edit_lt" "(ltedit_act)")
    (action_tile "accept" "(test-ok)")
    (action_tile "cancel" "(reset-lt)")
    (if (= (start_dialog) 1)           ; User pressed OK
        (cond
         ((or (= lt-idx nil)
              (= lt-idx (1- (length ltnmlst)))
          );or
          (set_tile "t_ltype" "Varies")
          ;|MSG0|;"Varies"
         )
         ((= lt-idx 0)
          (set_tile "t_ltype" (bylayer_lt))
           ;|MSG0|;"BYLAYER"
         )
         ((= lt-idx 1)
           (set_tile "t_ltype" "BYBLOCK")
           ;|MSG0|;"BYBLOCK"
         )
         (T
           (set_tile "t_ltype" ltname)
           ltname
         )
        );cond then
        eltype
    );if
  );defun
  ;;
  ;; Edit box entries end up here
  ;;
  (defun ltedit_act ( / flag)
    ;; If linetype name,is valid, then clear error string,
    ;; call ltlist_act function, and change focus to list box.
    ;; Else print error message.
    
    (setq ltvalue (xstrcase (get_tile "edit_lt")))
    (if (or (= ltvalue ;|MSG0|;"BYLAYER")
            (= ltvalue "BY LAYER")
        )
        (setq ltvalue "BYLAYER")
    )
    (if (or (= ltvalue ;|MSG0|;"BYBLOCK")
            (= ltvalue "BY BLOCK")
        )
        (setq ltvalue "BYBLOCK")
    )
    (if (setq lt-idx (getindex ltvalue ltnmlst))
        (progn
         (set_tile "error" "")
         (ltlist_act (itoa lt-idx))
         ;(mode_tile "list_lt" 2)
        );progn then
        (progn
         (if (/= ltvalue "")
             (progn
               (set_tile "error" "Invalid linetype.")
               (setq flag T)
             );progn
         )
         (setq lt-idx old-idx)
        );progn else
    );if
    (if (and (not flag) ;added so a return will take you out of the dialog.
             (= $reason 1)
        );and
        (done_dialog 1) 
    );if 
  );defun ltedit_act
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
    ;; Create layer list the first time the layer
    ;; dialogue is called.
    (if (not lay-idx)
        (progn
         (makelaylists)                     ; layer list - laynmlst
         ;rk
         (setq lay-idx (getindex elayer laynmlst))
        );progn
    );if

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
    (action_tile "edit_lay" "(layedit_act)")
    (action_tile "accept" "(test-ok)")
    (action_tile "cancel" "(reset-lay)")
    (if (= (start_dialog) 1)           ; User pressed OK
       (progn
         (if (or (= lay-idx nil)
                 (= lay-idx (1- (length laynmlst)))
             );or
             (progn
              (setq lay-idx nil)  
              (setq layname ;|MSG0|;"VARIES")
              (set_tile "t_layer" "Varies")
              (setq layname "")
             );progn
             (set_tile "t_layer" layname)
         );if
         ; If layer or ltype equals bylayer reset their tiles
         (if (= lt-idx 0)
             (set_tile "t_ltype" (bylayer_lt))
         );if
         (if (= ecolor 256)
             (progn
              (set_tile "t_color" (bylayer_col))
              (col_tile "show_image" cn nil)
             )
         );if
         layname
       );progn
       elayer
    );if
  )
  ;;
  ;; Edit box selections end up here
  ;;
  (defun layedit_act()
    ;; Convert layer entry to upper case.  If layer name is
    ;; valid, clear error string, call (laylist_act) function,
    ;; and change focus to list box.  Else print error message.
    (setq layvalue (xstrcase (get_tile "edit_lay")))
    (if (setq lay-idx (getindex layvalue laynmlst))
        (progn
         (set_tile "error" "")
         (laylist_act (itoa lay-idx))
        )
        (progn
         (set_tile "error" "Invalid layer name.")
         (mode_tile "edit_lay" 2)
         (setq lay-idx old-idx)
        )
    );if
  );defun
  ;;
  ;; List entry selections end up here
  ;;
  (defun laylist_act (index / layinfo color dashdata)
    ;; Update the list box, edit box, and color tile
    (set_tile "error" "")
    (setq lay-idx (atoi index))
    (if (not (equal lay-idx (1- (length laynmlst))))
        (progn 
         (setq layname (nth lay-idx laynmlst))
         (setq layinfo (tblsearch "layer" layname))
         (setq color (cdr (assoc 62 layinfo)))
         (setq color (abs color))
         (setq colname (colorname color))
         (set_tile "list_lay" (itoa lay-idx))
         (set_tile "edit_lay" layname)
         ;(mode_tile "list_lay" 2)
        );progn then 
        (set_tile "edit_lay" "") 
    );if 
  );defun laylist_act
  ;;
  ;; Reset to original layer when cancel is selected
  ;;
  (defun reset-lay ()
    (setq lay-idx old-idx)
    (done_dialog 0)
  )

  ;; Checks validity of linetype scale from edit box.  It checks to
  ;; see if the value equals "Varies".

  (defun getscale (value / rval)
    (setq value (strcase value)
           rval (distof value)
    );setq
    (if (or (= value "")
            (> rval 0.0)
        )
        (progn
          (set_tile "error" "")
          (if (= value "")
              (progn
               (set_tile "eb_ltscale" "")
               (setq eltscale nil)
              );progn then
              (progn
               (setq eltscale (distof value))
               (set_tile "eb_ltscale" (ai_rtos eltscale))
               eltscale
              );progn else
          );if
        );progn
        (progn
         (set_tile "error" "Invalid ltscale.")
         nil
        );progn else
    );if
  );defun
  ;;
  ;; Checks validity of thickness from edit box. Since (atof) returns 0 when a
  ;; string can't be converted to a real, this routine checks if the first
  ;; character is "0".  It also checks to see if the value equals "Varies".
  ;;
  (defun getthickness (value)
    (setq value (strcase value))
    (if (or (= value "")
            (distof value)
        );or
        (progn
         (set_tile "error" "")
         (if (= value "")
             (progn
              (set_tile "eb_thickness" "")
              (setq ethickness nil)
             );progn
             (progn
              (setq ethickness (distof value))
              (set_tile "eb_thickness" (ai_rtos ethickness))
              ethickness
             );progn
         );if
        );progn
        (progn
          (set_tile "error" "Invalid thickness.")
          nil
        );progn
    );if
  );defun 
  ;;
  ;; Checks validity of polyline elevation from edit box.
  ;;
  (defun getelevation (value)
    (setq value (strcase value))
    (if (or (= value "")
            (distof value)
        )
        (progn
         (set_tile "error" "")
         (if (= value "")
             (progn
              (set_tile "poly_elev" "")
              (setq eelevation nil)
             );progn
             (progn
              (setq eelevation (distof value))
              (set_tile "poly_elev" (ai_rtos eelevation))
              eelevation
             );progn
         );if
        );progn
        (progn
         (set_tile "error" "Invalid elevation.")
         nil
        );progn
    );if
  );defun
  ;;
  ;; Checks validity of polyline width from edit box.
  ;;
  (defun getwidth (value / rval)
    (setq value (strcase value)
           rval (distof value)
    )
    (if (or (= value "")
            (>= rval 0.0)
        )
        (progn
         (set_tile "error" "")
         (if (= value "")
             (progn
              (set_tile "poly_wid" "")
              (setq ewidth nil)
             );progn
             (progn
              (setq ewidth (distof value))
              (set_tile "poly_wid" (ai_rtos ewidth))
              ;width
             );progn
         );if
        );progn
        (progn
         (set_tile "error" "Invalid width.")
         ;(setq ewidth nil)
         nil 
        );progn
    );if
  );defun
  ;;
  ;; Checks validity of text height from edit box.
  ;;
  (defun getheight (value / rval)
    (setq value (strcase value)
           rval (distof value)
    )
    (if (or (= value "")
            (> rval 0.0)
        )
        (progn
         (set_tile "error" "")
         (if (= value "")
             (progn
              (set_tile "text_hgt" "")
              (setq eheight nil)
             );progn
             (progn
              (setq eheight (distof value))
              (set_tile "text_hgt" (ai_rtos eheight))
              eheight
             );progn
         );if
        );progn
        (progn
         (set_tile "error" "Invalid height.")
         nil
        );progn
    );if
  );defun

  (defun getstyle (value lst / rval)
    ;(setq value (strcase value))
    (set_tile "error" "")
    (setq estyle (nth (atoi value) lst))
    (if (equal estyle "")
        (setq estyle nil)
    );if
 
  );defun getstyle

  ;;
  ;; This function make a list called laynmlst which consists of all the layer
  ;; names in the drawing.  It also creates a list called longlist which
  ;; consists of strings which contain the layer name, color, linetype, etc.
  ;; Longlist is later mapped into the layer listbox.  Both are ordered the
  ;; same.
  ;;
  (defun makelaylists (/ layname onoff frozth color linetype lock vpf vpn ss cvpname
                         xdlist vpldata sortlist name templist bit-70
                         layer_number
                      )
    (if (= (setq tilemode (getvar "tilemode")) 0)
      (progn
        (setq ss (ssget "_x" (list (cons 0 "VIEWPORT")
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
    (setq layer_number 1)
    (while templist
      (if (not (equal 16 (logand 16 (cdr (assoc 70 templist)))))
          (progn  
           (setq name (cdr (assoc 2 templist)))
           (setq sortlist (cons name sortlist))
           ;; Not dead message...
           (setq layer_number (1+ layer_number))
          );progn
      );if
      (setq templist (tblnext "LAYER"))
      (if (= (/ layer_number 50.0) (fix (/ layer_number 50.0)))
          (set_tile "error" (strcat "Collecting..." (itoa layer_number)))
      );if
    )
    (set_tile "error" "")
    (if (>= (getvar "maxsort") (length sortlist))
      (progn
        (if (> layer_number 50)
          (set_tile "error" "Sorting...")
        )
        (setq sortlist (acad_strlsort sortlist))
      )
      (setq sortlist (reverse sortlist))
    )
    (set_tile "error" "")
    (setq laynmlst sortlist)
    ;rk 
    (setq laynmlst (append laynmlst (list "")))     

    (setq longlist nil)
    (setq layname (car sortlist))
    (setq layer_number 1)
    (while layname
      (if (= (/ layer_number 50.0) 
             (fix (/ layer_number 50.0))
          )
          (set_tile "error" (strcat "Analyzing..." (itoa layer_number)))
      );if
      (setq layer_number (1+ layer_number))
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
    );while
    (setq longlist (append longlist (list "")))
    (set_tile "error" "")
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
    (while (setq ltlist (tblnext "LTYPE"))
     (if (not (equal 16 (logand 16 (cdr (assoc 70 ltlist)))))
         (progn 
          (setq ltname (cdr (assoc 2 ltlist)))
          (setq ltnmlst (append ltnmlst (list ltname)))
         );progn
     );if 
    );while
    (setq ltnmlst (acad_strlsort ltnmlst))
    (setq ltnmlst (append ltnmlst (list "")));add by rk 
    (foreach ltname ltnmlst
      (setq ltlist (tblsearch "LTYPE" ltname))
      (if (= ltname "CONTINUOUS")
        (setq mdashlist (append mdashlist (list "CONT")))
        (setq mdashlist
            (append mdashlist (list (add-mdash ltlist)))
        )
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
    (setq fx 30)
    (if (= patlist "CONT")
      (progn
        (setq dash boxlength)
        (vi)
        (setq x1 boxlength)
      )
      (foreach dash patlist
        (if (> (abs dash) 2.5)
          (setq fx 2)
        )
      )
    )
    (while (< x1 boxlength)
      (if (setq dash (car patlist))
        (progn
          (setq dash (fix (* fx dash)))
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

  ;; This function takes a selection and returns a list of the color,
  ;; linetype, layer, linetype scale, and thickness properties that
  ;; are common to every entities in the selection set - (color
  ;; linetype layer thickness).  If all entities do not share the same
  ;; property value it returns "Varies" in place of the property
  ;; value.  i.e.  ("BYLAYER" "DASHED" "Varies" 0)
  ;; The last item in the return list is an integer flag for the
  ;; homegenity of the selection-set object types.
  ;;   1 = All polylines
  ;;   2 = All text or mtext or attdef, or a combination of the three
  ;;  -1 = Any other mix of objects

  (defun getprops (selset / sslen elist color ltype layer ltscale thickness
                          width elevation height go ctr 
                          eflag 
                          etype temp 
                          txt_ss     ;;;;rk 11:24 AM 1/30/97
                          tmp 
                          poly_ss   
                          style
                  )


    (setq     sslen (sslength selset)
              elist (entget (ssname selset 0))
              etype (strcase (cdr (assoc 0 elist)))
              color (cdr (assoc 62 elist))
              ltype (cdr (assoc 6 elist))
              layer (cdr (assoc 8 elist))
          thickness (cdr (assoc 39 elist))
            ltscale (cdr (assoc 48 elist))
    );setq

    (if (not color)     (setq color 256))
    (if (not ltype)     (setq ltype "BYLAYER"))
    (if (not thickness) (setq thickness 0))
    (if (not ltscale)   (setq ltscale 1))

    (if (not width)     (setq width ""))
    (if (not elevation) (setq elevation ""))
    (if (not height)    (setq height ""))

    (setq      go T 
          chk-col T 
           chk-lt T 
          chk-lay T 
          chk-lts T 
           chk-th T 
              ctr 0
    );setq

    ;; Page through the selection set.  When a property
    ;; does not match, stop checking for that property.
    ;; When the selection set is not homogenous, stop checking.
    ;; If all properties vary and the set is not a type 1
    ;; (polyline) or type 2 (attdef/text/mtext) set, stop paging.

    ;Lets set the eflag so we know if the selection set includes any 
    ;combination of polylines, lwpolylines, text, mtext or attdefs.

    (setq eflag 0)
    (if (setq poly_ss 
              (ssget "_P" (list '(0 . "*POLYLINE")
                               '(-4 . "<AND") 
                                  '(-4 . "<NOT") '(-4 . "&=") '(70 . 8)  '(-4 . "NOT>")
                                  '(-4 . "<NOT") '(-4 . "&=") '(70 . 16) '(-4 . "NOT>")
                                  '(-4 . "<NOT") '(-4 . "&=") '(70 . 64) '(-4 . "NOT>")
                               '(-4 . "AND>") 
                         );list
              );ssget get 2d polylines (legacy and lw)
        );setq
        (progn
         (setq eflag (+ eflag 1))
         
         (setq tmp (entget (ssname poly_ss 0)));setq

         (if (equal "POLYLINE" (cdr (assoc 0 tmp)))
             (setq elevation (last (cdr (assoc 10 tmp))))
             (setq elevation (cdr (assoc 38 tmp)))
         );if
         (if (not elevation)
             (setq elevation 0.0);setq
         );if

         (setq tmp (ssget "_P" 
                          (list 
                             '(-4 . "<OR")
                               '(-4 . "<AND") 
                                 '(0 . "LWPOLYLINE")
                                  (cons 38 elevation)
                               '(-4 . "AND>")
                               '(-4 . "<AND")
                                 '(0 . "POLYLINE")
                                 '(-4 . "*,*,=") 
                                  (cons 10 (list 1.0 1.0 elevation))
                               '(-4 . "AND>") 
                              '(-4 . "OR>")
                         );list
                   );ssget
         );setq

         (if (and tmp 
                  (equal (sslength tmp) (sslength poly_ss))
             );and
             (setq elevation (ai_rtos elevation));setq
             (setq elevation "")
         );if
          
         (setq width (pl_width_getter poly_ss));setq
         
        );progn then 2d polylines and/or lwpolylines are in the HHHHHOUSE!
    );if
    (command "_.select" selset "")
     
    (if (setq txt_ss 
              (ssget "_P" '((-4 . "<OR") (0 . "TEXT") (0 . "MTEXT") 
                           (0 . "ATTDEF") (-4 . "OR>"))
              )  
        );setq
        (progn
         (setq eflag (+ eflag 2))
         (setq height (cdr (assoc 40 (entget (ssname txt_ss 0))))
                  tmp (ssget "_P" (list (cons 40 height))) 
         );setq 
         (if (and tmp
                  (equal (sslength txt_ss) (sslength tmp))                    
             );and
             (setq height (ai_rtos height));setq  
             (setq height "");setq else the height varies 
         );if
         (command "_.select" txt_ss "") 
         (setq style (cdr (assoc 7 (entget (ssname txt_ss 0))))
                 tmp (ssget "_P" (list (cons 7 style))) 
         );setq 
         (if (not (and tmp
                       (equal (sslength txt_ss) (sslength tmp))                    
                  );and
             );not
             (setq style "");setq then the style varies 
         );if
        );progn the text type objects are in the selection set
    );if
    (command "_.select" selset "")
    
    (while (and (> sslen ctr) 
                go
           );and
      (setq elist (entget (setq en (ssname selset ctr))))

      (if chk-col (match-col))
      (if chk-lt (match-lt))
      (if chk-lay (match-lay))
      (if chk-lts (match-lts))
      (if chk-th (match-th))

      ;(if chk-etype (match-etype))

      (setq ctr (1+ ctr))
      (if (and (not chk-col)
               (not chk-lt)
               (not chk-lay)
               (not chk-lts)
               (not chk-th)
               ;(not chk-etype)
          );and
          (setq go nil)
      );if
    );while
    
    (list color ltype layer thickness ltscale
          width elevation height eflag 
          style poly_ss txt_ss
    )
  );defun getprops

  ;  This is a speedy little routine to tell whether the polylines in 
  ;the selection set argument are of varying width or a constant value. 
  ;Looping through the vertex's has to be done for old polylines when 
  ;the polyline header has width values of 0.0. Basically, in this case, 
  ;information in the polyline entity header is abmiguous. Width values 
  ;of 0.0 in the header entity could mean the polyline has a constant 
  ;width of 0.0 or it could mean that the polyline has vertex's of varying 
  ;width.
  ;
  ;  It's all in wrist. Err a.., I mean it's all in the 'if'
  ;;
  (defun pl_width_getter ( ss / ss2 ss3 n na e1 width width_a width_b flag flag2)

   (if ss 
       (command "_.select" ss "")
   );if
   (setq width ""
          flag nil
         flag2 nil
   );setq
   (if (not 
         (and ss
              (setq ss2 (ssget "_p" '((0 . "LWPOLYLINE"))));setq
              (setq    na (ssname ss2 0)
                    width (cdr (assoc 43 (entget na)))
              );setq
              (setq ss3 (ssget "_p" (list '(0 . "LWPOLYLINE")
                                          (cons 43 width)
                                   );list
                        );ssget
              );setq
              (setq flag T) 
              (equal (sslength ss2) (sslength ss3))
         );and
       );not
       (progn
        (if flag
            (setq width nil) 
        );if
       );progn
   );if 
       
   (if (not 
        (and 
             ss
             (progn (command "_.select" ss "") 
                    (setq ss2 (ssget "_p" '((0 . "POLYLINE"))));setq
             ) 
             (setq      na (ssname ss2 0)
                        e1 (entget na)
                   width_a (cdr (assoc 40 e1))
                   width_b (cdr (assoc 41 e1))
             );setq
             (equal width_a width_b)
             (setq ss3 (ssget "_p" (list '(0 . "POLYLINE")
                                         (cons 40 width_a)
                                         (cons 41 width_b)
                                  );list
                       );ssget
             );setq
             (setq flag2 T)
             (equal (sslength ss2) (sslength ss3))
        );and 
       );not 
       (progn
        (if flag2
            (setq width nil);setq
        );if
       );progn then
       (progn
        (if (or (equal width "")
                (not flag)
            );or
            (setq width width_a)
            (progn
             (if (not (equal width width_a))
                 (setq width "")
             );if  
            );progn
        );if
       );progn
   );if
      

   ;now for the special handling for old polylines
   (if (and width 
            (equal width 0.0)
            flag2
       );and 
       (progn
        
        (setq n 0);setq
        (while (and (equal width 0.0)
                    (< n (sslength ss3))
               );and
         (setq flag nil
                 na (ssname ss3 n)
                 na (entnext na)
                 e1 (entget na)
         );setq
         (while (not flag)
          (if (or (equal (cdr (assoc 0 e1)) "SEQEND")
                  (not (equal (cdr (assoc 40 e1)) 0.0))
                  (not (equal (cdr (assoc 41 e1)) 0.0))
              );or
              (progn
               (setq flag T);
               (if (not (equal (cdr (assoc 0 e1)) "SEQEND"))
                   (setq width nil) 
               );if
              );progn then jump out of the loop
              (setq na (entnext na)
                    e1 (entget na)
              );setq 
          );if 
         );while
        (setq n (+ n 1));setq  
        );while 
           
       );progn then it's a legacy 
   );if    ;legacy polylines that may have varying widths  

   (if (not width)
       (setq width "");setq
   );if
   (if (not (equal 'STR (type width)))
       (setq width (ai_rtos width));setq 
   );if 
    
   width   
  );defun pl_width_getter

    
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
        (setq ltype ;|MSG0|;"Varies")
      )
    )
  )

  (defun match-lay (/ nlayer)
    (setq nlayer (cdr (assoc 8 elist)))
    (if (/= layer nlayer)
      (progn
        (setq chk-lay nil)
        (setq layer ;|MSG0|;"Varies")
      )
    )
  )

  (defun match-th (/ nthickness)
    (setq nthickness (cdr (assoc 39 elist)))
    (if (not nthickness) (setq nthickness 0))
    (if (/= thickness nthickness)
      (progn
        (setq chk-th nil)
        (setq thickness ;|MSG0|;"Varies")
      )
    )
  )

  (defun match-lts (/ nltscale)
    (setq nltscale (cdr (assoc 48 elist)))
    (if (not nltscale) (setq nltscale 1))
    (if (/= ltscale nltscale)
      (progn
        (setq chk-lts nil)
        (setq ltscale ;|MSG0|;"Varies")
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
        (strcat "BYLAYER" " (" ltype ")")
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
        (strcat "BYLAYER" " (" (colorname color) ")")
      )
      (progn
        (setq layname elayer)
        (if (and (/= elayer "")
                 (/= elayer "Varies")
            );and
            (progn
             (setq layinfo (tblsearch "layer" elayer))
             (setq color (abs (cdr (assoc 62 layinfo))))
             (setq cn color)
             (strcat "BYLAYER" " (" (colorname color) ")")
            )
            (progn
             (setq cn 0)
             "BYLAYER"
            )
        );if
      );progn
    );if
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
  ;; OK in main dialogue.
  ;;
  (defun test-main-ok ( / flag)
   (setq flag T) 
   (if (not (or (distof (get_tile "eb_thickness"))
                (= "" (get_tile "eb_thickness"))
            );or
       );not
       (progn
        (set_tile "error" "Invalid thickness.")
        (mode_tile "eb_thickness" 2)
        (setq flag nil);setq
       );progn
   );if
   (if (and flag
            (not (or (< 0 (distof (get_tile "eb_ltscale")))
                     (= "" (get_tile "eb_ltscale"))
                 );or      
            );not
       );and 
       (progn
        (set_tile "error" "Invalid ltscale.")
        (mode_tile "eb_ltscale" 2)
        (setq flag nil); 
       );progn then
   );if
   (if (and flag
          ; Don't test the tile's value unless it's enabled.
          ; We're not set up for the display-only value
          ; of "" here in the error handler.
          (= 2 (logand 2 eflag))
          (not (or (< 0 (distof (get_tile "text_hgt")))
                   (= "" (get_tile "text_hgt"))
               );or
          );not
        );and
        (progn
         (set_tile "error" "Invalid height.")
         (mode_tile "text_hgt" 2)
         (setq flag nil);
        );progn then
   );if
   (if (and flag
            (= 1 (logand 1 eflag))
            (not (or (<= 0 (distof (get_tile "poly_wid")))
                     (= "" (get_tile "poly_wid"))
                 );or
            );not
       );and
       (progn
        (set_tile "error" "Invalid width.")
        (mode_tile "poly_wid" 2)
        (setq flag nil)
       );progn then
   );if 
   (if (and flag
            (= 1 (logand 1 eflag))
            (not (or (distof (get_tile "poly_elev"))
                     (= "" (get_tile "poly_elev"))
                 );or
            );not
        );and
        (progn
         (set_tile "error" "Invalid elevation.")
         (mode_tile "poly_elev" 2)
         (setq flag nil)
        );progn then
   );if  
   (if flag 
       (done_dialog 1)
   );if
  );defun test-main-ok

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
  );defun

;;; Construct layer and ltype lists and initialize all
;;; program variables:

;  (makelaylists)                     ; layer list - laynmlst


  (makeltlists)                      ; linetype lists - ltnmlst, mdashlist

  ;; Find the property values of the selection set.
  ;; (getprops ss) returns a list of properties from
  ;; a selection set - (color ltype layer thickness HEGHT STYLE WIDTH).

  (setq proplist (getprops ss))

  (setq
            ecolor (car proplist)
            eltype (nth 1 proplist)
            elayer (nth 2 proplist)
        ethickness (nth 3 proplist)
          eltscale (nth 4 proplist)
            ewidth (nth 5 proplist)
        eelevation (nth 6 proplist)
           eheight (nth 7 proplist)
            ;etype (nth 8 proplist);commented out and replaced with the line below. RK.
             eflag (nth 8 proplist)
            estyle (nth 9 proplist)
           poly_ss (nth 10 proplist)
           txt_ss  (nth 11 proplist)
  );setq

  ;; Find index of linetype, and layer lists
  (cond
    ((= eltype "Varies") (setq lt-idx nil))
    ((= eltype "BYLAYER")
     (setq lt-idx (getindex "BYLAYER" ltnmlst)))
    ((= eltype "BYBLOCK")
     (setq lt-idx (getindex "BYBLOCK" ltnmlst)))
    (T (setq lt-idx (getindex eltype ltnmlst)))
  )
  (if (= elayer "Varies")
      (setq lay-idx nil)
      (setq lay-idx (getindex elayer laynmlst))
  );if
  (if (= ethickness "")
      (setq ethickness nil)
  );if
  (if (= eltscale "")
      (setq eltscale nil)
  );if

);defun ddchprop2_init   ; end (ddchprop2_init)

;;; (ddchprop2_select)
;;;
;;; Aquires selection set for DDCHPROP2, in one of three ways:
;;;
;;;   1 - Autoselected.
;;;   2 - Prompted for.
;;;   3 - Passed as an argument in a call to (ddchprop2 <ss> )
;;;
;;; The (ddchprop2_select) function also sets the value of the
;;; global symbol AI_SELTYPE to one of the above three values to
;;; indicate the method thru which the entity was aquired.


(defun ddchprop2_select ( / )

;;;begin the work of ddchprop2_select
 
 ;; temporarily restore original highlight setting.
 (b_set_sysvars (assoc "HIGHLIGHT" (car bonus_varlist)))
 (cond
   ((and ss 
         (eq (type ss) 'pickset)
    )        ; selection set passed to
    (cond                                   ; (ddchprop2) as argument
     ((not (zerop (sslength ss)))       ;   If not empty, then
      (setq ai_seltype 3)               ;   then return pickset.
      (ai_return ss)
     )
    );cond close
   );cond #1
   ((setq ss (ai_aselect)))                          ; Use current selection
                                                     ; set or prompt for objects
   (T (princ "\nNothing selected.")
      (ai_return nil)
   )
 );cond close
 (b_restore_sysvars)

 ;(if ss
 ;    (setq ss (ss_remove_locked ss)) 
 ;);if
 (if ss
     (setq ss (ss_in_current_space ss)) 
 );if

 ss
);defun ddchprop2_select


;;; Define command function.
(defun C:EXCHPROP ()
  (ddchprop2 nil)
  (princ)
);defun


;;; Main program function - callable as a subroutine.
;;;
;;; (ddchprop2 <pickset> )
;;;
;;; <pickset> is the selection set of objects to be changed.
;;;
;;; If <pickset> is nil, then the current selection set is
;;; aquired, if one exists.  Otherwise, the user is prompted
;;; to select the objects to be changed.
;;;
;;; Before (ddchprop2) can be called as a subroutine, it must
;;; be loaded first.  It is up to the calling application to
;;; first determine this, and load it if necessary.

(defun ddchprop2 (ss  /

                  a
                  add-mdash
                  assoclist
                  bit-70
                  boxlength
                  bylayer-lt
                  bylayer_col
                  bylayer_lt
                  call_chp2
                  chk-col
                  ;chk-etype        ;var removed by rk 
                  chk-lay    
                  chk-lt
                  chk-lts           ;var added by rk 
                  chk-th
                  cmd
                  cmdecho
                  cn
                  cnum
                  col-idx
                  col_def
                  col_tile
                  colname
                  colnum
                  color
                  colorname
                  cvpname
                  dash
                  dashdata
                  dashlist
                  dashsize
                  dcl_id
                  ddchprop-err
                  drawpattern
                  ecolor
                  eelevation
                  eflag
                  eheight
                  elayer
                  elevation
                  elist
                  eltscale
                  eltype
                  en
                  ename
                  ESTYLE             ;var added by rk
                  ethickness
                  ;etype             ;var removed by rk
                  ewidth
                  fchk
                  frozth
                  fx
                  getcolor
                  GETELEVATION      ;function added by rk
                  GETHEIGHT         ;function added by rk
                  getindex
                  getlayer
                  ;get_locked_layers ;function added and then removed by rk 
                  getltype
                  getprops
                  getscale          ;function added by rk
                  GETSTYLE          ;function added by rk
                  getthickness
                  GETWIDTH          ;function added by rk
                  globals
                  HAIR_STYLE_LIST   ;var added by rk
                  height
                  index
                  item
                  item1
                  item2
                  itemlist
                  lay-idx
                  layedit_act
                  layer
                  layinfo
                  laylist
                  laylist_act
                  layname
                  laynmlst
                  layvalue
                  linetype
                  list1
                  longlist
                  lt-idx
                  ltabstr
                  ltedit_act
                  ltidx
                  ltlist
                  ltlist1
                  ltlist_act
                  ltname
                  ltnmlst
                  ltvalue
                  ltype
                  m
                  makelaylists
                  makeltlists
                  match-col
                  ;match-etype          ;function removed by rk
                  match-in
                  match-lay
                  match-lt
                  match-lts
                  match-th
                  match_col
                  mdashlist
                  ;MPOPLST               ;function added and then move to ac_bonus.lsp by rk
                  n
                  name
                  ncolor
                  nlayer
                  nltype
                  nthickness
                  off
                  old-idx
                  olderr
                  on
                  onoff
                  patlist
                  pattern
                  PL_WIDTH_GETTER       ;function added by rk
                  POLY_SS               ;var added by rk
                  ;POSITION              ;function added by rk and then moved to ac_bonus.lsp
                  proplist
                  reset-lay
                  reset-lt
                  s
                  selset
                  set_col_tile
                  sortlist
                  ss
                  ss-index
                  ss-length
                  ;ss_remove_locked     ;function added and then removed by rk.
                  sslen
                  temp_color
                  templist
                  test-main-ok
                  test-ok
                  testidx
                  testlay
                  th-value
                  thickness
                  tile
                  tile_rect
                  tilemode
                  ;TNLIST           ;function removed and moved to ac_bonus.lsp by rk
                  TXT_SS            ;var added by rk
                  ;undo_init        ;removed by rk.
                  vi
                  vpf
                  vpldata
                  vpn
                  which_tiles
                  width
                  x
                  x1
                  x2
                  xdlist
                  y
                  y1
                  y2
                )

  (if (and (not init_bonus_error) 
           (equal -1 (load "ac_bonus.lsp"  -1)) 
      );and
      (progn (alert "Error:\n     Cannot find AC_BONUS.LSP.")(exit))
  );if
  (init_bonus_error (list
                     (list   "cmdecho" 0 
                           "highlight" 0
                           "regenmode" 1
                             "ucsicon" 0
                     ) 
                     T     ;flag. True means use undo for error clean up.                    
                    );list  
  );init_bonus_error

  (cond
     (  (not (ai_notrans)))                       ; Not transparent?
     (  (not (ai_acadapp)))                       ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "exchprop")))) ; is .DCL file loaded?
     (  (not (setq ss (ddchprop2_select))))       ; objects to modify?
     (t 
        ;(ai_undo_push)
        (ddchprop2_init)                          ; Everything's cool,
        (call_chp2)                               ; so proceed!
        ;(ai_undo_pop)
     )
  );cond close

  (restore_old_error)

 (princ)
);defun ddchprop2

;;;----------------------------------------------------------------------------

(princ "   EXCHPROP loaded.")
(princ)

