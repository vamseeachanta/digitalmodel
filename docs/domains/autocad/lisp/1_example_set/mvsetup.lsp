;;;   MVSETUP.LSP
;;;   Copyright (C) 1990-1992 by Autodesk, Inc.
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
;;; DESCRIPTION
;;;
;;;   This is a setup routine for Mview.
;;;   
;;;   It is based around a set of functionality that was determined to be a
;;;   necessary part of preparing a drawing for plotting.  This routine allows
;;;   the user to insert several pre-defined title blocks (ANSI A - E) and in
;;;   addition it allows the user to create a set of viewports within the 
;;;   title block just inserted.  A global scale may be specified as a ratio
;;;   between the scale of the title block in paperspace and the model geometry
;;;   in modelspace.  For example, if you were doing an engineering drawing at
;;;   a scale of 1:4 or quarter scale, then you would specify that you wanted
;;;   a ratio of 1 paperspace unit to 4 modelspace units.  The routine will 
;;;   convert this to 0.25xp for the Zoom command.  You may also elect to 
;;;   align the viewports.
;;;   
;;;   (The first time you invoke MVSETUP, you may notice a slight delay.
;;;   This occurs because the routine is creating a default file of various
;;;   title blocks and viewport layouts.  If you should accidentally erase
;;;   your default file, another will be created the next time you invoke
;;;   MVSETUP.  The file will be created in the directory specified by the
;;;   AutoCAD system variable "ACADPREFIX".  If you run AutoCAD from a
;;;   directory other than that one, and the system variables ACAD or ACADCFG
;;;   do not point to that directory, then MVSETUP will not be able to find 
;;;   it, and will create a new one in the directory pointed to by the first
;;;   path found in the current setting of the AutoCAD system variable 
;;;   "ACADPREFIX".)
;;;   
;;;   When you invoke MVSETUP from the command line or one of the menus, you
;;;   are given four options;  three dealing with the creation and manipulation
;;;   of viewports, and one which allows you to insert various "title blocks".
;;;   The initial prompts are shown below.
;;;   
;;;   
;;;         MVSetup, Version 1.15, (c) 1990-1992 by Autodesk, Inc.  
;;;         Align/Create/Scale viewports/Options/Title block/Undo:  
;;;         
;;;   The Align viewports option presents you with several more options; you
;;;   are asked to determine the type of alignment you wish to perform.
;;;   
;;;         Angled/Horizontal/Vertical alignment/Rotate view/Undo? 
;;;           
;;;   The Horizontal and Vertical options ask you to pick a basepoint in one
;;;   viewport, and the point to move in another viewport.  The view in the
;;;   second viewport is panned by the offset distance in X or Y between
;;;   the two points relative to the zoom scale factor of the second viewport.
;;;   
;;;   The Angled option asks you for these two points and for a distance and
;;;   angle from the basepoint.  The point in the first viewport at the 
;;;   specified distance and angle from the basepoint is where the "other"
;;;   point will be panned.
;;;   
;;;   The Rotate view option asks you for a basepoint and a rotation angle
;;;   and uses the DVIEW command to change the view twist.  This generally 
;;;   will be useful only when the UCS of the view you are rotating is 
;;;   parallel to the screen and would be used to align a view with an
;;;   angled edge with the Align Angled option.
;;;   
;;;   Selecting Create viewports prompts you with the following:
;;;   
;;;         Delete objects/<Create viewports>:
;;;   
;;;   Selecting Delete objects provides you with a general selection prompt
;;;   at which time you may delete any paperspace objects that you wish.
;;;   This is intended to allow you to clear an area for your new viewports.
;;;   Modelspace entities will NOT be removed.
;;;   
;;;   
;;;   Selecting Create viewports prompts you to select one of the viewport 
;;;   options listed.
;;;   
;;;         Available Mview viewport layout options: 
;;; 
;;;         0:       None
;;;         1:       Single
;;;         2:       Std. Engineering
;;;         3:       Array of Viewports
;;; 
;;;         Redisplay/<Number of entry to load>: 
;;;         
;;;   Pressing RETURN or selecting "None" returns you to the main prompt
;;;   without creating any viewports.  
;;;   
;;;   "Single" is a single viewport which can fill the default area open in 
;;;   the sheet or it can be located by picking two points on the screen.  
;;;   
;;;   Std. Engineering is a set of four viewports with the upper left viewport 
;;;   in plan view, the lower left in a front view, the lower right in a right 
;;;   side view, and the upper right in an oblique view at -45 degrees from 0 
;;;   and up 30 degrees.
;;;   
;;;   The "Array of Viewports" allows you to specify any array of viewports
;;;   that you want to place on your sheet, from a 1 by 2 or 2 by 1 to any
;;;   level allowed by AutoCAD.
;;;   
;;;   
;;;   After selecting option 1, 2 or 3, you are prompted to specify the bounding
;;;   area for the viewports that are to be created.  Each of the title blocks
;;;   provided has a bounding region defined for it in the default file.  You
;;;   can elect to create all of the viewports within this region by selecting 
;;;   "Default" at the following prompt:
;;;   
;;;         Bounding area for viewports.  Default/<First point >: 
;;;   
;;;   You can also select two points on the screen and the number of viewports 
;;;   you subsequently define will be mapped into this area.
;;;   
;;;   Picking options 2 or 3 prompts you to specify the distance between the
;;;   viewports; the interstitial distance.  This value must be a positive 
;;;   number but may be zero.  The value you enter for X is automatically
;;;   assigned to Y, though you may specify Y to have a different value.
;;;   If you selected option 2 above, then the four viewports are created and
;;;   the four views are mapped into them as defined in the default file.
;;;   The other options create the viewports but do not change the views in 
;;;   any of them; the view will be a plan view in the current UCS.
;;;   
;;;   
;;;   Selecting Scale viewports at the main menu prompts you to select the 
;;;   viewports you wish to scale.  If you select one or more viewports
;;;   you asked whether you wnat to set the zoom scales all at once or for
;;;   each viewport separately:
;;;   
;;;         Set zoom scale factors for viewports.  Interactively/<Uniform>:
;;;   
;;;   After selecting one of these you are asked the following;
;;;   
;;;         Enter the ratio of paper space units to model space units... 
;;;         Number of paper space units.  <1.0>: 
;;;         Number of model space units.  <1.0>: 
;;;   
;;;   The number of viewports specified will have their zoom scales changed
;;;   by the ratio of the paper space units divided by the model space units.
;;;   This is cumulative over time, so performing this operation twice with
;;;   paper space units set to 1.0 and model space units set to 2.0 will give
;;;   the same results as doing it once with 1.0 an 4.0 as the values.
;;;   
;;;   
;;;   Selecting Options at the main menu allows you to specify several
;;;   preferences for operation of Mvsetup.  They are:
;;;
;;;         Set Layer/LImits/Units/Xref: 
;;;   
;;;   The Layer option allows you to specify a layer, existing or new, on
;;;   which to insert the title block, the LImits option allows you to
;;;   specify whether or not to reset the limits to the drawing extents after
;;;   a title block has been inserted, Units specifies whether the sizes and 
;;;   point locations are to be translated to inch or millimeter paper units,
;;;   and the Xref option let's you determine whether the title block is to 
;;;   be inserted or Xref'ed.
;;;
;;;
;;;   Selecting Title block from the main menu gives you another sub-menu.
;;;   
;;;         Delete objects/Origin/<Insert title block>:
;;;   
;;;   Delete objects works as described above under Create viewports.
;;;   Origin allows you to specify a new UCS origin for the subsequent
;;;   insertion of a title block.  Pressing RETURN will cause you to be 
;;;   presented with a list of available title blocks or sheets.
;;;   
;;;         Available title block options:  
;;; 
;;;         0:       NONE
;;;         1:       ISO A4 Size(mm)
;;;         2:       ISO A3 Size(mm)
;;;         3:       ISO A2 Size(mm)
;;;         4:       ISO A1 Size(mm)
;;;         5:       ISO A0 Size(mm)
;;;         6:       ANSI-V Size(in)
;;;         7:       ANSI-A Size(in)
;;;         8:       ANSI-B Size(in)
;;;         9:       ANSI-C Size(in)
;;;         10:      ANSI-D Size(in)
;;;         
;;;         11:      ANSI-E Size(in)
;;;         12:      Arch/Engineering (24 x 36 in)
;;; 
;;;         Add/Redisplay/<Number of entry to load>:      
;;;         
;;;   This list includes ISO standard sheets used outside the US, specified
;;;   in millimeters, the ANSI standard sheet layouts from A to E and size 
;;;   A Vertical specified in inches.  Selecting the number preceding one of the
;;;   selections causes one of two things to occur.  One, if the AutoCAD
;;;   drawing associated with the selections cannot be found, then the 
;;;   default file is read, a definition is extracted, and the drawing is
;;;   created in your current drawing.  You are then asked whether you want
;;;   to save this drawing to disk.  If you want to use this drawing more
;;;   than once, you should answer Yes to this prompt.  Two, if the AutoCAD
;;;   drawing can be found then it is INSERTed into your drawing at 0,0.
;;;   
;;;   The other options are Add, Delete and Redisplay.
;;;   
;;;   Pressing RETURN or selecting 0 for "None" at this prompt returns you 
;;;   to the main menu.  Selecting the number of a viable entry causes one
;;;   of two things to occur:  if you selected one of the built-in entries,
;;;   and you have not created a drawing by the name associated with this 
;;;   entry, the default file is scanned for a definition, and if found, 
;;;   the title block is created in your current drawing.  You are asked
;;;   whether you want to create a drawing of the entities just created.
;;;   For example, picking 1 (ANSI-V Size) gives you the following prompt:
;;;   
;;;         Create a drawing named ansi-v.dwg? <Y>:
;;;   
;;;   Answering Yes causes a drawing to be created and reinserted into your
;;;   drawing where it was created.
;;;   
;;;   If the drawing already exists it is inserted at the current UCS origin.
;;;   This is the mechanism for using your own title blocks and parts of
;;;   title blocks.  
;;;   
;;;   Selecting Add after the available title blocks are listed gives you
;;;   the following prompts; example text follows the colon:
;;;   
;;;         Title block description: A/E (24 x 18in)
;;;   
;;;         Drawing to insert (without extension): arch-b
;;;         Specify default usable area? <Y>:  Y
;;;         
;;;         Lower left corner: (1.12 0.99 0.00)
;;;         Upper right corner: (18.63 17.02 0.00)
;;;   
;;;   A line looking like this
;;;   
;;;         A/E (24 x 18in),arch-b.dwg,(1.12 0.99 0.00),(18.63 17.02 0.00),in
;;;   
;;;   is added after the last entry in the default file.
;;;   
;;;   The last field of the line specifies whether the title block has been 
;;;   created in inch or millimeter units.  This filed is used to allow title 
;;;   blocks created in either unit system to be interchanged simply by setting 
;;;   the unit type in the Options menu.
;;;   
;;;   Selecting Delete at the same prompt allows you to delete any of the 
;;;   lines listed except line 0.  Redisplay causes the list to be displayed 
;;;   again.
;;;   
;;;   If neither a valid drawing or a definition can be found you are so
;;;   informed and returned to the main menu.
;;;   
;;;   
;;;   GLOBALS:
;;;     mv_nln -- New layer name for title block insertions.
;;;     mv_slr -- Set Limits Request from drawing extents.
;;;     mv_utr -- Unit Translation Request.
;;;     mv_uxr -- Use Xref Request for title block insertions.
;;;   
;;;----------------------------------------------------------------------------;
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
        (ai_abort "MVSETUP"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "MVSETUP" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "MVSETUP" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================
;;;   
;;;   Start routine from menu pick or by typing MVS at the
;;;   command prompt.
;;;   
(defun mv_sup ( / mv_err s mv_oer mv_oce mv_olu ll_crn need_z mv_orm mv_oln
                  deffi deffo mv_utr mv_oas undo_setting)

  ;;
  ;; Internal error handler defined locally
  ;;

  (defun mv_err (s)                   ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ)
        (princ (strcat "\nError: " s))
      )
    )
    (if deffi (close deffi))
    (if deffo (close deffo))
    (command "_.UNDO" "_EN")
    (ai_undo_off)
    (if mv_oer                        ; If an old error routine exists
      (setq *error* mv_oer)           ; then, reset it 
    )
    (if mv_olu (setvar "lunits" mv_olu)) ; Restore prev. linear units value
    (setvar "regenmode" mv_orm)       ; Restore prev. Regenmode value
    (setvar "osmode" mv_osm)          ; Restore prev. Object snap mode value
    (setvar "pickfirst" mv_oas)       ; Restore prev. Pickfirst value
    (setvar "cmdecho" mv_oce)         ; Reset command echoing on error
    (princ)
  )
  
  (if (not *DEBUG*)
    (if *error*                       ; If there is an error routine defined
      (setq mv_oer   *error*          ; Store AutoLisp error routine
            *error*  mv_err)          ; Temporarily replace it
    )
  )
  (setq mv_oce (getvar "cmdecho"))
  (setvar "cmdecho" 0)                ; Turn off command echoing

  (ai_undo_on)                        ; Turn UNDO on

  (setq mv_orm (getvar "regenmode"))
  (setq mv_osm (getvar "osmode"))
  (setq mv_oas (getvar "pickfirst"))
  (setvar "pickfirst" 0)              ; Turn off pickfirst 
  (setvar "osmode" 0)                 ; Turn off object snap mode
  (setvar "regenmode" 1)              
  (if (mv_ctm)                        ; Is Tile-mode on? T or nil
    (mv_dos)                          ; Do old setup
    ;; else    
    (mv_dns)                          ; Do new setup
  )
  (if deffi (setq deffi (close deffi)))
  (if deffo (setq deffo (close deffo)))
  (setvar "regenmode" mv_orm)         ; Restore prev. Regenmode value       
  (setvar "osmode" mv_osm)            ; Restore prev. Object snap mode value
  (setvar "pickfirst" mv_oas)         ; Restore prev. Pickfirst value

  (ai_undo_off)                       ; Return UNDO to initial state

  (setvar "cmdecho" mv_oce)           ; Reset command echoing
  (if mv_oer                          ; If an old error routine exists
    (setq *error* mv_oer)             ; then, reset it 
  )
  (princ)
)
;;;
;;; Check Tile-mode.  Returns T if ON and nil if not on.
;;;
;;; mv_ctm == MView_Check_TileMode
;;;
(defun mv_ctm ()
  (if (= (getvar "TILEMODE") 1) 
    (progn
      (initget "Yes No")
      (setq ans (getkword (strcat
        "\nPaperspace/Modelspace is disabled.  The pre-R11 setup will be "
        "\ninvoked unless it is enabled.  Enable Paper/Modelspace?  <Y>: "))
      )
      (if (= ans "No")
        T
        (progn
          (setvar "TILEMODE" 0)
          nil
        )
      )
    )
    nil
  )
)
;;;
;;; Do a new setup relative to a point to be given by the user.
;;; The default is the current 0,0,0.
;;;
;;; mv_dns == MView_Do_New_Setup
;;;
(defun mv_dns (/ mv_ver mv_xdf mv_xlf o_cvpt ans sset ITEM_LIST fd)

  (setq mv_ver "1.15")                ; Reset this local if you make a change.
  (setq mv_xpf (mv_cpf (getvar "acadprefix" )))
  
  (setq mv_xdf (strcat mv_xpf "mvsetup.dfs"))         
  (setq mv_xlf "mvsetup.lsp")         ; Reset these locals if you make changes.
  
  (setq uctr 0)
  (setq o_cvpt (getvar "cvport"))
  (if (/= o_cvpt 1)
    (command "_.PSPACE")               ; Change to paperspace
  )
                                        
  ;; Look for external definitions --  set fd to file descriptor
  (setq fd (mv_lfx mv_xdf "r"))         
                                        
  ;; Close the file, but do not set the handle to nil.
  (if fd (close fd))
                                        
  (if (null fd)
    ;; then
    (progn
      (princ (strcat "\n\tCreating the default file mvsetup.dfs" 
                     "\n\tin the directory "
                     mv_xpf ". "))
      (mv_cdf)
      (setq fd (mv_lfx mv_xdf "r"))

      ;; Close the file; we were just checking to see if it was there.
      (close fd)
    )
    ;; else
  )
  (princ (strcat 
    "\n\tMVSetup, Version " mv_ver ", (c) 1990-1992 by Autodesk, Inc. "))
  (setq temp T
        mv_utr "in"
  )
  (while temp
    (initget "Align Create Scale Title Undo Options")
    (setq ans (getkword (strcat
      "\n\tAlign/Create/Scale viewports/"
      "Options/Title block/Undo: ")))
      
    (cond
      ((= ans "Align")
        (mv_vpa)                      ; Viewport alignment
      )
      ((= ans "Create")
        (setq temp1 T)
        (while temp1
          (initget "Create Delete Undo")
          (setq ans (getkword 
            "\n\tDelete objects/Undo/<Create viewports>: "))
          (cond
            ((= ans "Delete")
              (command "_.UNDO" "_GROUP")
              (setq uctr (1+ uctr))
          
              (princ "\n\tSelect the objects to delete: ")
              (setq sset (ssget))
              (if sset
                (command "_.ERASE" sset "")
              )
              (command "_.UNDO" "_EN")
            )
            ((= ans "Undo")
              (cond
                ((= uctr 0) (princ "\n\tNothing to undo. \n"))
                ((> uctr 0) 
                  (command "_.U")
                  (setq uctr   (- uctr 1)
                        ll_crn nil
                  )
                )
              ) 
            )
            (T
              (command "_.UNDO" "_GROUP")
              (setq uctr (1+ uctr))
              (setq temp1 nil)
              (if (setq deffi (mv_lfx mv_xdf "r"))
                (progn
                  (textpage)
                  (setq str1 "\n\tAvailable Mview viewport layout options: \n")
            
                  (setq vp_item_list (mv_pao "MVIEWS" str1))
            
                  (if (and (= (type vp_item_list) 'LIST) (null skip))
                    (mv_mvi)
                  )
                  (setq deffi (close deffi))
                )
                (princ (strcat "
                  \n\tCouldn't open the file " mv_xdf " for reading. "))
              )
              (command "_.UNDO" "_EN")
            )
          )
        )
        (command "_.UNDO" "_EN")
      )
      ((= ans "Options")
        (mv_sop)                      ; Set options
      )
      ((= ans "Scale")
        (mv_szs)                      ; Set zoom scale factors
      )
      ((= ans "Title")
        (setq temp1 T)
        (while temp1
          (if (/= (getvar "cvport") 1)
            (command "_.PSPACE")
          )
          (initget "Delete Origin Insert Undo")
          (setq ans (getkword 
            "\n\tDelete objects/Origin/Undo/<Insert title block>: "))
          (cond
            ((= ans "Delete")
              (command "_.UNDO" "_GROUP")
              (setq uctr (1+ uctr))

              (princ "\n\tSelect the objects to delete: ")
              (setq sset (ssget))
              (if sset
                (command "_.ERASE" sset "")
              )
              (command "_.UNDO" "_EN")
            )
            ((= ans "Origin")
              (command "_.UNDO" "_GROUP")
              (setq uctr (1+ uctr))

              ;; Get an origin point for the new title block.
              (initget 1)
              (setq ans (getpoint '(0 0 0) 
                "\n\tNew origin point for this sheet: "))
              (command "_.UCS" "_O" ans)
              (command "_.UNDO" "_EN")
            )
            ((= ans "Undo")
              (cond
                ((= uctr 0) (princ "\n\tNothing to undo. \n"))
                ((> uctr 0) 
                  (command "_.U")
                  (setq uctr   (- uctr 1)
                        ll_crn nil
                  )
                )
              ) 
            )
            (T
              (command "_.UNDO" "_GROUP")
              (setq uctr (1+ uctr))
              (setq temp1 nil)
              (if fd
                (mv_gop)
                (princ (strcat 
                  "\nCouldn't open the file " mv_xdf " for reading. "))
              )
              (command "_.UNDO" "_EN")
            )
          )
        )
        (command "_.UNDO" "_EN")
      )
      ((= ans "Undo")
        (cond
          ((= uctr 0) (princ "\n\tNothing to undo. \n"))
          ((> uctr 0) 
            (command "_.U")
            (setq uctr   (- uctr 1)
                  ll_crn nil
            )
          )
        ) 
      )
      (T
        (setq temp nil)
      )
    )
  )
  (if (/= o_cvpt 1)
    (progn
      (setq sset (ssget "x" '((0 . "VIEWPORT"))))
      (if sset 
        (setq o_cvpt (mv_vvp o_cvpt sset))
        (setq o_cvpt nil)
      )
      (command "_.MSPACE")              ; Change to modelspace
      ;; If the previously current viewport has been deleted,
      ;; this will do nothing.
      (if o_cvpt (setvar "cvport" o_cvpt)) ; Restore previous active viewport
    )
  )
)
;;;
;;; Set preference options.
;;;
;;; mv_sop == MView_Set_OPtions
;;;
(defun mv_sop (/ temp lay cont)
  (setq cont T)
  (while cont
    (initget "Layer LImits Units Xref")
    (setq temp (getkword (strcat "\n\tSet Layer/LImits/Units/Xref: ")))
    (cond
      ((= temp "Layer")
        (setq mv_oln (getvar "CLAYER"))
        (setq temp (if mv_nln (strcat " <" mv_nln ">: ") ": "))
        (setq temp (getstring (strcat 
          "\n\tLayer name for title block or . for current layer" temp))
        )
        (cond
          ((= temp "")
            (setq mv_nln nil)
          )
          ((= temp ".")
            (setq mv_nln nil)
          )
          (T
            (if (setq lay (tblsearch "LAYER" temp))
              (if (= (logand (cdr(assoc 70 lay)) 2) 2)
                (command "_.LAYER" "_THAW" temp "")
              )
              (command "_.LAYER" "_NEW" temp "")
            )
            (setq mv_nln temp)
          )
        )
      )
      ((= temp "LImits")
        (setq temp (if mv_slr "Y" "N"))
        (initget "Yes No")
        (setq temp (getkword (strcat "\n\tSet drawing limits? <"
                              temp ">: ")))
        (if mv_slr
          (if (= temp "No")
            (setq mv_slr nil)
            (setq mv_slr T)
          )
          (if (= temp "Yes")
            (setq mv_slr T)
            (setq mv_slr nil)
          )
        )
      )
      ((= temp "Units")
        (setq temp (if mv_utr mv_utr "in"))
        (initget "Mm Millimeters Inches MEters Feet")
        (setq temp (getkword (strcat 
          "\n\tPaperspace units are in Feet/Inches/MEters/Millimeters? <"
          temp ">: "))
        )
        (if temp
          (cond
            ((= temp "Millimeters") (setq mv_utr "mm"))
            ((= temp "Mm")          (setq mv_utr "mm"))
            ((= temp "MEters")      (setq mv_utr "M"))
            ((= temp "Feet")        (setq mv_utr "ft"))
            ((= temp "Inches")      (setq mv_utr "in"))
            (T (princ))
          )
        )
      )
      ((= temp "Xref")
        (setq temp (if mv_uxr "Xref" "Insert"))
        (initget "Xref Insert")
        (setq temp (getkword (strcat 
          "\n\tXref Attach or Insert title block? <"
          temp ">: "))
        )
        (if mv_uxr
          (if (= temp "Insert")
            (setq mv_uxr nil)
            (setq mv_uxr T)
          )
          (if (= temp "Xref")
            (setq mv_uxr T)
            (setq mv_uxr nil)
          )
        )
      )
      (T
        (setq cont nil)
      )
    )
  )
)
;;;
;;; Return the first path in ACADPREFIX delimited by ";".
;;;
;;; mv_cpf == MView_Check_acadPreFix
;;;
(defun mv_cpf (pf / temp)
  (setq j 1
        l (strlen pf)
  )
  (while (<= j l)
    (if (= (substr pf j 1) ";")
      (progn
        (setq temp (substr pf 1 (1- j)))
        (setq j (1+ l))
      )
      (setq j (1+ j))
    )
  )
  (if temp
    temp
    pf
  )
)
;;;
;;; Verify the Mview viewport whose number we have in vp_n.
;;;
;;; mv_vvp == MView_Verify_ViewPort
;;;
(defun mv_vvp (num sset / j vp ss_len cont)
  (setq ss_len (sslength sset)
        j      0
        cont   T
  )
  (while (and cont (< j ss_len))
    (setq temp (entget (ssname sset j)))
    (setq j (1+ j))
    (if (= (cdr(assoc 68 temp)) 2)
      (setq vp temp)
    )
    (if (= (cdr(assoc 69 temp)) num)
      (setq cont nil
            vp   temp
      )
    )
  )
  (cdr(assoc 69 vp))
)
;;;
;;; Align viewport geometry
;;;
;;; mv_vpa == MView_ViewPort_Align
;;;
(defun mv_vpa ( / temp temp1 ans p1 pt1 p2 a1 d1)
  (setq temp T)
  (while temp
    (initget "Angled Horizontal Rotate Vertical Undo")
    (setq ans (getkword 
      "\n\tAngled/Horizontal/Vertical alignment/Rotate view/Undo? "))
    (if (or (= ans "") (= ans "Rotate") (= ans "Undo") (null ans))
      (if (= ans "Rotate")
        (progn
          (princ "\n\tSpecify in which viewport the view is to be rotated. ")
          (command "_.MSPACE")
          (command "_.UCS" "_W")
          (setq p1 (getpoint "\n\tBasepoint: "))
          (setq temp (getvar "cvport"))
          (command "_.UCS" "_V")
          (setq a1 (getangle (trans p1 0 1) "\n\tAngle from basepoint: "))
          (command "_.DVIEW" ""  "_TW" (* a1 (/ 180 pi)) "")
          (command "_.UCS" "_P")
          (command "_.UCS" "_P")
          (command "_.PSPACE")
        )
        (if (= ans "Undo")
          (cond
            ((= uctr 0) (princ "\n\tNothing to undo. \n") )
            ((> uctr 0) 
              (command "_.U")
              (setq uctr   (- uctr 1)
                    ll_crn nil
            )
            )
          ) 
          (setq temp nil)
        )
      )
      (progn
        (command "_.UNDO" "_GROUP")
        (setq uctr (1+ uctr))
        (command "_.MSPACE")
        (command "_.UCS" "_W")
        (setq p1 (getpoint "\n\tBasepoint: "))
        (setq pt1 (trans (trans p1 1 2) 2 3))
        (setq temp (getvar "cvport"))
        
        (setq p2 (getpoint "\n\tOther point: "))
        (setq p2 (trans (trans p2 1 2) 2 3))
        (cond 
          ((= ans "Angled")
            (setq temp1 (getvar "cvport"))
            (if (= temp1 temp)
              (princ "\n\tPoints must be in different viewports. ")
              (progn
                (setvar "cvport" temp) 
                (setvar "orthomode" 0) 
                (princ (strcat
                  "\n\tSpecify the distance and angle "
                  "to the new alignment point "))  
                (princ (strcat
                  "\n\tin the current viewport where "
                  "you specified the basepoint. "))
                (setq d1 (getdist "\n\tDistance from basepoint: "))
                (setq a1 (getangle "\n\tAngle from basepoint: "))
                (setq p1 (polar p1 a1 d1))
                (setq p1 (trans (trans p1 1 2) 2 3))
                (setvar "cvport" temp1) 
                (command "_.UCS" "_V")
                (command "_.PAN" (trans p2 3 2) (trans p1 3 2))
              )
            )
          )
          ((= ans "Horizontal")
            (setq temp1 (getvar "cvport"))
            (command "_.UCS" "_V")
            (setq p1 (list (car p2) (cadr pt1) (caddr p2)))
            (if (= temp1 temp)
              (princ "\n\tPoints must be in different viewports. ")
              (command "_.PAN" (trans p2 3 2) (trans p1 3 2))
            )
          )
          ((= ans "Vertical")
            (setq temp1 (getvar "cvport"))
            (command "_.UCS" "_V")
            (setq p1 (list (car pt1) (cadr p2) (caddr p2)))
            (if (= temp1 temp)
              (princ "\n\tPoints must be in different viewports. ")
              (command "_.PAN" (trans p2 3 2) (trans p1 3 2))
            )
          )
          (T
            (setq temp nil)
          )
        )
        (command "_.UCS" "_P")
        (command "_.UNDO" "_EN")
      )
    )
  )
)
;;;
;;; Read lines from a file until the argument matches the given sub-string
;;;
;;; mv_rux == MView_Read_Until_Xx_found
;;;
(defun mv_rux (str j k / l cont line)
  (setq cont T l 1)
  (while cont
    (setq line (read-line deffi))
    (setq l (1+ l))
    ;; Seek to the first instance of str at position j - k.
    (if line
      (if (= (substr line j k) str)
        (setq cont nil)
      )
      (progn
        (setq cont nil l nil)
      )
    )
  )
  l                                   ; Return nil or line number where 
                                      ; matching string is found
)
;;;
;;; Tokenize the line, removing any blanks not within the string.
;;; Return the tokenized list of strings found.
;;;
;;; mv_tok == MView_TOKenize
;;;
(defun mv_tok (str / sl j str_list)
  (setq s_list (mv_tkw str))
  (setq list_l (length s_list)
        j      0
  )
  (while (< j list_l)
    (setq s_list (subst (mv_tki (nth j s_list)) (nth j s_list) s_list))
    (setq j (1+ j))
  )
  s_list
)
;;;
;;; Tokenize the item, removing any leading and trailing blanks.
;;; Return the string.
;;;
;;; mv_tki == MView_ToKenize_Item
;;;
(defun mv_tki (str / sl j k str_list)
  (setq sl (strlen str)
        j  1
        k  0
  )
  (while (and (< j sl) (= (substr str j 1) " "))
    (setq j (1+ j))
  )
  (while (and (< k sl) (= (substr str (- sl k) 1) " "))
    (setq k (1+ k))
  )
  (substr str j (- sl k))
)
;;;
;;; Tokenize a string into a list of strings.
;;; Return the tokenized string list.
;;;
;;; mv_tkw == MView_ToKenize_into_Words
;;;
(defun mv_tkw (str / sl k)
  (setq sl (strlen str)
        k  0
  )
  (while (and (< k sl) (/= (substr str (1+ k) 1) ","))
    (setq k (1+ k))
  )
  (if str_list 
    (setq str_list (append str_list (list (substr str 1 k))))
    (setq str_list (list (substr str 1 k)))
  )
  (setq k (+ k 2))
  (if (< k sl)
    (mv_tkw (substr str k))
  )
  str_list
)
;;;
;;; List names on the screen until an end of list marker is found.
;;; Store the items found into a list, ITEM_LIST, a global
;;; Ignore blank lines and commented lines. Return number of lines.
;;;
;;; mv_lns == MView_List_Names_on_Screen
;;;
(defun mv_lns (str j k / l cont line)
  (setq cont T l -1)
  (while cont
    (if (setq line (read-line deffi))
      ;; Seek to the end of the section delimited by "str"
      ;; Else print the line to the screen preceded by an integer
      (if (= (substr line j k) str)
        (setq cont nil)
        (progn
          (setq l         (1+ l)
                new_line  (mv_tok line)
                item      (car new_line)
                ITEM_LIST (if ITEM_LIST
                            (append ITEM_LIST (list new_line))
                            (list new_line)
                          )
          )
          (if (and (= (rem l 10) 1) (> l 1))
            (if (= (rem l 20) 1)
              (progn
                (princ "\n\t<more> ")
                (grread)
              )
              (terpri)
            )
          )
          (princ (strcat "\n\t" (itoa l) ":\t " item))
        )
      )
      (setq cont nil)
    )
  )
  l
)
;;;
;;; Add an entry to the default file.  Get all inputs.
;;;
;;; mv_aef == MView_Add_an_Entry_to_default_File
;;;
(defun mv_aef ( / str ans deffo p1 p2)
  (setq ans (getstring T "\n\tTitle block description: "))
  (if (not (or (= ans "") (null ans))) 
    (progn
      (setq str ans)
      (setq ans (getstring "\n\tDrawing to insert (without extension): "))
      (if (not (or (= ans "") (null ans))) 
        (progn
          (initget "Yes No")
          (setq p1 (getkword "\n\tSpecify default usable area? <Y>: "))
          (if (= p1 "No")
            (setq str (strcat str "," ans ".dwg" "," mv_utr))
            (progn
              (initget 1)
              (setq p1 (getpoint "\n\tLower left corner: "))
              (initget 1)
              (setq p2 (getcorner p1 "\n\tUpper right corner: "))
              (mv_s2p 'p1 'p2)
              (setq str (strcat str "," ans ".dwg" 
                                ",(" (rtos (car p1))
                                " " (rtos (cadr p1))
                                " " (rtos (caddr p1))
                                ")"
                                ",(" (rtos (car p2))
                                " " (rtos (cadr p2))
                                " " (rtos (caddr p2))
                                ")"
                                "," mv_utr))
            )
          )
          (setq deffi (close deffi))
          (if (setq deffi (mv_lfx mv_xdf "r"))
            (if (setq deffo (mv_lfx "temp.tdf" "w"))
              (progn
                (setq cur_ln (+ cur_ln max_l))
                (repeat cur_ln (progn
                  (write-line (read-line deffi) deffo)
                ))
                (write-line str deffo)
                (while (setq line (read-line deffi))
                  (write-line line deffo)
                )
              )
            )
          )
          (setq deffo (close deffo))
          (setq deffi (close deffi))
          (if (setq deffi (mv_lfx "temp.tdf" "r"))
            (if (setq deffo (mv_lfx mv_xdf "w"))
              (while (setq line (read-line deffi))
                (write-line line deffo)
              )
            )
          )
          (setq deffo (close deffo))
          (setq deffi (close deffi))
          (command "_.FILES" "3" "temp.tdf" "" "")
          (textpage)
        )
      )
    )
  )
)
;;;
;;; Subtract an entry from the default file.  Get all inputs.
;;;
;;; mv_sef == MView_Subtract_an_Entry_from_default_File
;;;
(defun mv_sef ( / str ans deffo)
  (setq str (nth 0 d_item_list))
  (setq deffi (close deffi))
  (if (setq deffi (mv_lfx mv_xdf "r"))
    (if (setq deffo (mv_lfx "temp.tdf" "w"))
      (progn
        (setq cur_ln (mv_rux str 1 (strlen str)))
        (setq cur_ln (- cur_ln 2))
        (close deffi)
        (setq deffi (mv_lfx mv_xdf "r"))

        (repeat cur_ln (progn
          (write-line (read-line deffi) deffo)
        ))
        (read-line deffi)
        (while (setq line (read-line deffi))
          (write-line line deffo)
        )
      )
    )
  )
  (setq deffo (close deffo))
  (setq deffi (close deffi))
  (if (setq deffi (mv_lfx "temp.tdf" "r"))
    (if (setq deffo (mv_lfx mv_xdf "w"))
      (while (setq line (read-line deffi))
        (write-line line deffo)
      )
    )
  )
  (setq deffo (close deffo))
  (setq deffi (close deffi))
  (command "_.FILES" "3" "temp.tdf" "" "")
  (textpage)
  (setq deffi (mv_lfx mv_xdf "r"))

  (princ)
)
;;;
;;; Pick from the list by typing an integer, returns the item or zero.
;;; cont is global to this routine, local to the calling routine.
;;;
;;; mv_pfl == MView_Pick_From_List
;;;
(defun mv_pfl (max_l ig_b ig_str prmpt / OK ans return str)
  (while (null OK)
    (initget ig_b ig_str)
    (setq ans (getint prmpt))
    (cond 
      ((or (= ans "") (null ans))
        (setq OK T cont nil return 0)
      )
      ((= ans "Add")
        (mv_aef)
        (setq OK T)
      )
      ((= ans "Delete")
        (setq str "\n\tNumber of entry to delete from list: ")
        (setq d_item_list (mv_pfl max_l 6 "" str))
        (if d_item_list
          (progn
            (princ (strcat "\n\tDeleting " (car d_item_list) " from list. "))
            (mv_sef)
          )
        )
        (setq OK T cont T return 0)
      )
      ((= ans "Redisplay")
        (setq OK T)
      )
      (T
        (if (and (>= ans 0) (<= ans max_l))
          (setq return (nth ans ITEM_LIST)
                OK     T
                cont   nil
          )
          (progn
            (princ (strcat 
              "\n\tNumber must be between 0 and " (itoa max_l) "\n"))
            (setq OK nil)
          )
        )
      )
    )
  )
  return
)
;;;
;;; Get the user's options
;;;
;;; mv_gop == MView_Get_OPtions
;;;
(defun mv_gop (/ deffi d_item_name max_lines ans cont isc fsc mmsc msc li ll)
  (if (setq deffi (mv_lfx mv_xdf "r"))
    (progn
      
      (textpage)
      (setq str1 "\n\tAvailable title block options: \n")
      
      (setq d_item_list (mv_pao "NAMES" str1))
      
      (if (and (= (type d_item_list) 'LIST) (null skip))
        (progn
          (mv_tbi)
          (setq ll (length d_item_list)
                li (nth 2 d_item_list)
          )
          (if li                      ; ll is > 2
            (progn
              (if (= (type (read li)) 'LIST)
                (progn
                  (setq ll_crn li)
                  (if (> ll 3)
                    (setq ur_crn (nth 3 d_item_list)
                          li     (nth 4 d_item_list)
                    )
                    (setq ll_crn nil ur_crn nil)
                  )
                )
              )
              (if (= (type ur_crn) 'STR)
                (setq ll_crn (read ll_crn)
                      ur_crn (read ur_crn)
                )
              )
              (cond
                ((= (strcase mv_utr T) "mm")
                  (setq isc   25.4
                        mmsc  1.0
                        msc   1000.0
                        fsc   (* 12 25.4)
                  )
                )
                ((= (strcase mv_utr T) "m")
                  (setq isc   (/ 25.4 1000)
                        mmsc  (/ 1.0 1000)
                        msc   1.0
                        fsc   (/ (* 12 25.4) 1000)
                  )
                )
                ((= (strcase mv_utr T) "ft")
                  (setq isc   (/ 1.0 12)
                        mmsc  (/ 1.0 (* 12 25.4))
                        msc   (/ 1000.0 (* 12 25.4))
                        fsc   1.0
                  )
                )
                ((= (strcase mv_utr T) "in")
                  (setq isc   1.0
                        mmsc  (/ 1.0 25.4)
                        msc   (/ 1000.0 25.4)
                        fsc   12.0
                  )
                )
                (T
                  (princ "\nError: unit type not specified in default file.")
                  (exit)
                )
              )
            )
          )
          (cond             
            ((= li "in")                 ; And are defined in inches
              (command "_.SCALE" (entlast) "" "0,0" isc)
              (setq ll_crn (mapcar '(lambda (x) (* x isc)) ll_crn))
              (setq ur_crn (mapcar '(lambda (x) (* x isc)) ur_crn))
            )
            ((= li "ft")                 ; And are defined in feet
              (command "_.SCALE" (entlast) "" "0,0" fsc)
              (setq ll_crn (mapcar '(lambda (x) (* x fsc)) ll_crn))
              (setq ur_crn (mapcar '(lambda (x) (* x fsc)) ur_crn))
            )
            ((= li "mm")                 ; And are defined in millimeters
              (command "_.SCALE" (entlast) "" "0,0" mmsc)
              (setq ll_crn (mapcar '(lambda (x) (* x mmsc)) ll_crn))
              (setq ur_crn (mapcar '(lambda (x) (* x mmsc)) ur_crn))
            )
            ((= li "M")                 ; And are defined in meters
              (command "_.SCALE" (entlast) "" "0,0" msc)
              (setq ll_crn (mapcar '(lambda (x) (* x msc)) ll_crn))
              (setq ur_crn (mapcar '(lambda (x) (* x msc)) ur_crn))
            )
          )
          (command "_.ZOOM" "_E")
          (if mv_slr                      ; Set Limits requested
            (progn
              (setq temp (getvar "EXTMIN"))
              (setvar "LIMMIN" (list (car temp) (cadr temp)))
              (setq temp (getvar "EXTMAX"))
              (setvar "LIMMAX" (list (car temp) (cadr temp)))
            )
          )
        )
      )
      (setq ITEM_LIST nil)

      (setq deffi (close deffi))
      
    )
    (princ (strcat "\n\tCouldn't open the file " mv_xdf " for reading. "))
  )
)
;;;
;;; Pick options
;;;
;;; mv_pao == MView_Pick_An_Option
;;;
(defun mv_pao (str str1 / cont max_lines d_item_list)
  (setq cont T)
  (while cont
    (princ str1)
    (setq cur_ln (mv_rux str 1 (strlen str)))
    (setq ITEM_LIST nil)
    (setq max_lines (mv_lns (strcat "END_" str) 
                                          1 (+ 4 (strlen str))))
    (if (= str "MVIEWS")
      (setq d_item_list (mv_pfl max_lines 4 "Redisplay"  
        "\n\n\tRedisplay/<Number of entry to load>: "))
      (setq d_item_list (mv_pfl max_lines 4 "Add Redisplay Delete"  
        "\n\n\tAdd/Delete/Redisplay/<Number of entry to load>: "))
    )
    ;; 
    ;; Skip the title block insertion if "None" is selected.
    ;;
    (if (and (= (type d_item_list) 'LIST) (= (car d_item_list) "None"))
      (setq skip T)
      (setq skip nil)
    )
    ;;
    ;; If the Redisplay option is chosen, rewind the file by
    ;; Closing it, opening it again, and seeking to the start
    ;; of the NAMES section again.
    ;;
    (if cont
      (progn
        (if deffi (setq deffi (close deffi)))
        (if (null (setq deffi (mv_lfx mv_xdf "r")))
          (progn
            (princ (strcat "\n\tCouldn't open  " mv_xdf " for reading."))
            (setq cont nil)
          )
        )
      )
    )
  )
  d_item_list
)
;;;
;;; Title block insertion
;;; Check that the drawing exists and if not, try to create it
;;; from the definition, if it can be found.  If not,
;;; reject the selection.  If there is a definition, then
;;; execute it and perform the WBLOCK command, then
;;; insert the resulting block.
;;;
;;; mv_tbi == MView_Title_Block_Insertion
;;;
(defun mv_tbi ()
  ;; If an alternate layer has been specified, then set that layer for the
  ;; subsequent title block insertion or Xref.
  (if mv_nln
    (command "_.LAYER" "_SET" mv_nln "")
  )
  ;; 
  ;; a definition in the default file.  If that fails, then  
  
  (if (> (length d_item_list) 1) 
    (if (setq block_name (findfile (nth 1 d_item_list)))
      (if mv_uxr
        (mv_xtb block_name)
        (mv_itb block_name)
      )
      ;; Block named not found;  try to create it...
      (progn
        (setq block_name (nth 1 d_item_list))
        (setq e_last (cdr(assoc -1 (entget(entlast)))))
        (setq item_name (strcat "NAME - " (nth 0 d_item_list)))
        (if (mv_rux item_name 1 (strlen item_name))
          (mv_cfd)
        )
        (if (not (eq (cdr(assoc -1 (entget(entlast)))) e_last))
          (progn
            (setq sset (ssadd))   ; Create a null selection set.
            (while (setq e_last (if e_last (entnext e_last) (entnext)))
              (ssadd e_last sset) ; Add new entities to end of database.
            )
            (initget "Yes No")
            (setq ans (getkword (strcat
              "\n\tCreate a drawing named " block_name "? <Y>: ")))
            (if (/= ans "No")
              (progn
                (command "_.WBLOCK" block_name "" "0,0" sset "")
                (if mv_uxr
                  (mv_xtb block_name)
                  (mv_itb block_name)
                )
              )
              ;; Else do nothing...
            )
          )
          (progn
            (princ (strcat
              "\n\tCouldn't find a definition for block " block_name ". "))
            ;;(exit)
          )
        )
      )
    )
    (progn
      (setq d_item_list (strcat "NAME - " (nth 0 d_item_list)))
      (if (mv_rux d_item_list 1 (strlen d_item_list))
        (mv_cfd)
      )
    )
  )
  (if mv_nln
    (command "_.LAYER" "_SET" mv_oln "") ; Reset old layer
  )
)
;;;
;;; Insert title blocks which may have variable attributes.
;;;
;;; mv_itb == MView_Insert_Title_Block
;;;
(defun mv_itb (block_name / b_def b_nam hasatt temp cont count)
  ;; Cancel the block command after inserting the block definition.
  (command "_.INSERT" block_name \03)
  ;; Strip name of path and extension.
  (setq b_nam (substr block_name 1 (setq sl (- (strlen block_name) 4))))
  (setq cont T temp "")
  (while (and cont (> sl 0) (setq ch (substr b_nam sl 1)))
    (if (or (= ch "\\") (= ch "/") (= ch ":"))
      (setq cont nil)
      (setq temp (strcat ch temp)
            sl   (1- sl)
      )
    )
  )
  (setq oarq (getvar "attreq"))
  (setq otev (getvar "texteval"))
  (setvar "attreq" 1)
  (setq b_nam temp)
  ;; Check for trailing attributes.
  (setq b_def (tblsearch "block" b_nam))
  (setq hasatt (if (= (logand (cdr(assoc 70 b_def)) 2) 2) T nil))
  (if hasatt
    (progn
      (setq temp (entget(cdr(assoc -2 b_def)))
          cont   T
          count  0
      )
      (while (or cont (and temp (/= (cdr(assoc 0 temp)) "SEQEND")))
        (if (and (= (cdr(assoc 0 temp)) "ATTDEF") 
                 (/= (logand (cdr(assoc 70 temp)) 2) 2)
                 (/= (logand (cdr(assoc 70 temp)) 8) 8)
            )
          (setq count (1+ count)
                cont nil
          )
        )
        (setq temp (entnext (cdr(assoc -1 temp))))
        (if temp (setq temp (entget temp)))
      )
      (setq temp "(command")
      (repeat (1- count)
        (setq temp (strcat temp " pause"))
      )
      (setq temp (strcat temp ")"))
    )
  )
  ;; Insert the block
  (command "_.INSERT" block_name "0,0" "" "")
  ;; If there are attributes, insert 'em...
  (if hasatt
    (progn
      (setvar "texteval" 1)
      (setvar "cmdecho" 1)            ; Turn on command echoing
    )
  )
  ;; Note the peculiar order of "cmdecho" and (command) 
  ;; Finish
  (command "")
  (if hasatt
    (eval(read temp))
  )
  (setvar "cmdecho" 0)                ; Turn off command echoing
  (if hasatt
    (command pause)
  )
  (setvar "attreq" oarq)
  (setvar "texteval" otev)
)
;;;
;;; Xref title blocks; variable attributes are ignored.
;;;
;;; mv_xtb == MView_Xref_Title_Block
;;;
(defun mv_xtb (block_name / b_def b_nam hasatt temp cont count)
  ;; Insert the block
  (command "_.XREF" "_ATTACH" block_name "0,0" "" "" "0")
)
;;;
;;; Create the mview viewports.
;;;
;;; mv_mvi == MView_MView_Insertion
;;;
(defun mv_mvi (/ n_vp_x n_vp_y i_l_len view_n p1 p2 ok_size)
  (if (> (length vp_item_list) 0) 
    (progn
      (if (> (length vp_item_list) 2) 
        (setq n_vp_x (nth 1 vp_item_list)
              n_vp_y (nth 2 vp_item_list)
        )
      )
      (if (> (setq i_l_len (length vp_item_list)) 3) 
        (setq view_n (- i_l_len 3))  ; Number of views defined.
        (setq view_n 0)
      )
      (setq d_item_name (strcat "VPSETNAME  - " (nth 0 vp_item_list)))
      (if (mv_rux d_item_name 1 (strlen d_item_name))
        (progn
          (mv_gba)                    ; Get bounding area
          (mv_s2p 'mvs_p1 'mvs_p2)
          (mv_gnv)                    ; Get number of viewport
          (mv_gid)                    ; Get interstitial distances
          (mv_cfp)                    ; Calculate first points
          (mv_cba)                    ; Check area is on-screen
          (setvar "osmode" 0)
          (command "_.MVIEW" p1 p2)
          (if (> (cdr (assoc 68 (entget (entlast)))) 0)
            (progn
              (setq ok_size T)
              (mv_cna)                ; Check number of active viewports
            )
          )
          (setq f_vp_n (mv_gvn))      ; Get viewport number
          (mv_avp)                    ; Array other Mview viewports
          (setq l_vp_n (mv_gvn))      ; Get viewport number
          (if (and (not ok_size) (> i_l_len 3))
              (princ "\n\t Viewport size is too small to change views.")
              (mv_cav)                ; Change the view of all viewports
          )
        )
      )
    )
  )
)
;;;
;;; Set the zoom scale factors for a set of viewports.
;;;
;;; mv_szs == MView_Set_Zoom_Scales
;;;
(defun mv_szs ( / temp)
  (command "_.UNDO" "_GROUP")
  (setq uctr (1+ uctr))
  (if (/= (getvar "cvport") 1)
    (command "_.PSPACE")
  )
  (princ "\n\tSelect the viewports to scale: ")
  (setq sset (ssget))
  (if sset
    (progn
      (if (> (sslength sset) 1)
        (progn
          (initget "Interactively Uniform")
          (setq ans (getkword (strcat
            "\n\tSet zoom scale factors for viewports.  "
            "Interactively/<Uniform>: "))
          )
          (if (= ans "Interactively")
            (setq vp_s_i T)
            (setq vp_s_i nil)
          )
        )
        (setq vp_s_i nil)
      )
      (setq temp (sslength sset)
            j    0
      )
      (if (= (getvar "cvport") 1)
        (command "_.MSPACE")
      )
      (while (< j temp)
        (progn
          (setq vp_n (cdr(assoc 69 (entget(ssname sset j)))))
          (setvar "cvport" vp_n)
          (setq j (1+ j))
          (if (or vp_s_i (< j 2))
            (mv_ssi)
          )
          (command "_.ZOOM" (strcat (mv_sts vp_scale) "xp"))
        )
      )
      (command "_.PSPACE")
    )
    (princ "\n\tNo viewports selected. ")
  )
  (command "_.UNDO" "_EN")
)
;;;
;;; Interactively set the scale of each viewport.
;;;
;;; mv_ssi == MView_Setup_Scale_Interactively
;;;
(defun mv_ssi (/ ans)
  (princ "\n\tEnter the ratio of paper space units to model space units... ")
  (initget 6)
  (setq ans (getreal 
    "\n\tNumber of paper space units.  <1.0>: ")
  )
  (if (= (type ans) 'REAL)
    (setq vp_scale ans)
    (setq vp_scale 1.0)
  )
  (initget 6)
  (setq ans (getreal 
    "\n\tNumber of model space units.  <1.0>: ")
  )
  (if (= (type ans) 'REAL)
    (setq vp_scale (/ vp_scale ans))
    (setq vp_scale (/ vp_scale 1.0))
  )
  vp_scale
)
;;;
;;; Set up the scale either interactively or uniformly.
;;;
;;; mv_sus == MView_Set_Up_Scale
;;;
(defun mv_sus ()
  (if vp_s_i
    (mv_ssi)
  )
    (setq vp (mv_gvp (+ n vp_n) sset))
  (command "_.ZOOM" (strcat (mv_sts vp_scale) "xp"))
)
;;;
;;; Convert a real number to its shortest value; no trailing zeros.
;;;
;;; mv_sts == MView_Scale_To_String
;;;
(defun mv_sts (num / scale j return)
  (setq scale (rtos num 2 15)
        j     0
  )
  (while (< j (strlen scale))
    (if (= (setq return (substr scale (- (strlen scale) j) 1)) "0")
      (setq j (1+ j))
      (if (= return ".")
        (progn
          (setq return (substr scale 1 (- (strlen scale) (1- j)))) 
          (setq j (strlen scale))
        )
        (progn
          (setq return (substr scale 1 (- (strlen scale) j))) 
          (setq j (strlen scale))
        )
      )
    )
  )
  return
)
;;;
;;; Change to a new plan view and restore.
;;;
;;; mv_npv == MView_set_New_Plan_View
;;;
(defun mv_npv (ord_1 amnt_1 ord_2 amnt_2)
  (command "_.UCS" ord_1 amnt_1)
  (command "_.UCS" ord_2 amnt_2)
  (command "_.PLAN" "" )
  (command "_.UCS" "_P")
  (command "_.UCS" "_P")
  (princ)
)
;;;
;;;Get the Mview viewport whose number we have in vp_n.
;;;
;;; mv_gvp == MView_Get_ViewPort
;;;
(defun mv_gvp (num sset / j vp ss_len cont)
  (setq ss_len (sslength sset)
        j      0
        cont   T
  )
  (while (and cont (< j ss_len))
    (if (= (cdr(assoc 69 (setq vp (entget (ssname sset j))))) num)
      (setq cont nil)
    )
    (setq j (1+ j))
  )
  vp
)
;;;
;;; Change the view into all Mview viewports.
;;;
;;; mv_cav == MView_Change_All_Views
;;;
(defun mv_cav (/ n sset vp_n vp m_4_iso)
  (if need_z
    (command "_.ZOOM" "_E")
  )
  (setq n    0
        sset (ssget "x" '((0 . "VIEWPORT")))
        vp_n f_vp_n
  )
  (command "_.MSPACE")
  ;;(setvar "cvport" vp_n)
  ;; While we still have both viewports and view definitions for them...
  (while (and (< n view_n) (<= (+ n vp_n) l_vp_n))
    (setq vp (mv_gvp (+ n vp_n) sset))
    (if m_4_iso
      (command "_.MVIEW" "_on" (cdr (assoc -1 vp)) "")
    )
    (setvar "cvport" (+ n vp_n))
    (cond 
      ((= (nth (+ 3 n) vp_item_list) "PLAN")
        (command "_.PLAN" "")
        ;;(mv_sus)
      )
      ((= (nth (+ 3 n) vp_item_list) "FRONT")
        (mv_npv "x"  "90" "x"  "0") 
        ;;(mv_sus)
      )
      ((= (nth (+ 3 n) vp_item_list) "RIGHT")
        (mv_npv "z"  "90" "x" "90")
        ;;Test for case when all 4 viewports can't be on.  We
        ;;turn this one off so we can continue to the iso view.
        (if (= maxact 4)
          (progn
            (command "_.MVIEW" "_OFF" (cdr (assoc -1 vp)) "")
            (setq m_4_iso T)
          )
        )
        ;;(mv_sus)
      )
      ((= (nth (+ 3 n) vp_item_list) "LEFT")
        (mv_npv "z" "-90" "x" "90")
        ;;(mv_sus)
      )
      ((= (nth (+ 3 n) vp_item_list) "ISO")
        (command "_.VPOINT" "_R" "-45" "30")
        ;;(mv_sus)
      )
      (T
        (princ "\n\tUndefined view definition in default file. ")
        (exit)
      )
    )
    (setq n (1+ n))
  )
  (command "_.PSPACE")
)
;;;
;;; Array other Mview viewports.
;;;
;;; mv_avp == MView_Array_ViewPorts
;;;
(defun mv_avp ( / dx dy)
  (if (and (> (car mvs_p4) 1) (> (cadr mvs_p4) 1))
    (progn
      (setq dx (+ (car mvs_p5)  (car mvs_p3)))
      (setq dy (+ (cadr mvs_p5)  (cadr mvs_p3)))
      (command "_.ARRAY" 
               (entlast) "" 
               "_r" 
               (cadr mvs_p4)
               (car mvs_p4) 
               (+ (cadr mvs_p5) (cadr mvs_p3))
               (+ (car mvs_p5)  (car mvs_p3))
      )
    )
    (if (> (car mvs_p4) 1)
      (progn
        (setq dx (+ (car mvs_p5)  (car mvs_p3)))
        (command "_.ARRAY" 
                 (entlast) "" 
                 "_r" 
                 (cadr mvs_p4)
                 (car mvs_p4) 
                 (+ (car mvs_p5)  (car mvs_p3))
        )
      )
      (if (> (cadr mvs_p4) 1)
        (progn
          (setq dy (+ (cadr mvs_p5)  (cadr mvs_p3)))
          (command "_.ARRAY" 
                   (entlast) "" 
                   "_R" 
                   (cadr mvs_p4)
                   (car mvs_p4) 
                   (+ (cadr mvs_p5) (cadr mvs_p3))
          )
        )
      )
    )
  )
)
;;;
;;; Check the number of active viewports plus the number of new
;;; viewports to be created to determine whether the allowable
;;; number of active viewports will be exceeded.  If so, noff
;;; holds the number of viewports to turn off before creating
;;; the rest of the new ones.
;;;
;;; mv_cna == MView_Check_Number_of_Active_viewports
;;;
(defun mv_cna (/ sset ssl nsset nssl n temp total noff)
  (setq sset (ssget "x" '((0 . "viewport")))
        ssl (sslength sset)
        nsset (ssadd)
        n 0
  )
  (while (< n ssl)
    (setq temp (ssname sset n))
    (if (> (cdr (assoc 68 (entget temp))) 0)
        (ssadd temp nsset)
    )
    (setq n (1+ n))
  )
  (if (> (setq total (+ (setq nssl (sslength nsset))
                        (1- (* (car mvs_p4) (cadr mvs_p4)))
                     )
         )
         (setq maxact (getvar "maxactvp"))
      )
      (progn
        (setq noff (- total maxact)
              n 1
        )
        (if (> maxact 4)
            (while (and (<= n noff) (< n (1- nssl)))
              (setq temp (ssname nsset n))
              (command "_.MVIEW" "_OFF" temp "")
              (setq n (1+ n))
            )
            (while (< n (1- ssl))
              (setq temp (ssname sset n))
              (command "_.MVIEW" "_OFF" temp "")
              (setq n (1+ n))
            )
        )
      )
  )
)
;;;
;;; Get the name of a Mview viewport.
;;;
;;; mv_gvn == MView_Get_Viewport_Name
;;;
(defun mv_gvn ()
  (cdr(assoc 69 (entget(entlast))))
)
;;;
;;; Calculate the size of the individual viewports from the two
;;; corner points, mvs_p1 and mvs_p2, the interstitial distances
;;; held in mvs_p3, and the number of viewports held in mvs_p4.
;;;
;;; mv_cfp == MView_Calculate_First_Points
;;;
(defun mv_cfp (/ x y dx dy )
  (mv_s2p 'mvs_p1 'mvs_p2)
  ;; Points are now sorted so that mvs_p1 IS lower left....
  (setq x  (- (car  mvs_p2) (car  mvs_p1))
        y  (- (cadr mvs_p2) (cadr mvs_p1))
        dx (/ (- x (* (1- (car  mvs_p4)) (car  mvs_p3))) (car  mvs_p4))
        dy (/ (- y (* (1- (cadr mvs_p4)) (cadr mvs_p3))) (cadr mvs_p4))
        p1 mvs_p1
        p2 (mapcar '+ p1 (setq mvs_p5  (list dx dy 0)))
  )
)
;;;
;;; Sort two points into lower-left to upper-right order.
;;;
;;; mv_s2p == MView_Sort_2_Points
;;;
(defun mv_s2p (_p1 _p2 / x y pt1 pt2 )
  (setq pt1 (eval _p1)
        pt2 (eval _p2)
  )
  (if (> (car pt1) (car pt2))
    (setq x (car pt1)
          pt1 (list (car pt2) (cadr pt1) 0.0)
          pt2 (list x (cadr pt2) 0.0)
    )
  )
  (if (> (cadr pt1) (cadr pt2))
    (setq x (cadr pt1)
          pt1 (list (car pt1) (cadr pt2) 0.0)
          pt2 (list (car pt2) x 0.0)
    )
  )
  (set _p1 pt1)
  (set _p2 pt2)
)
;;;
;;; Get the number of viewports in X and Y.
;;; Sets the global mvs_p4 which is a point containing the X and Y
;;; amounts as the (car) and (cadr) of mvs_p4.
;;;
;;; mv_gnv == MView_Get_Number_of_Viewports_in_x_and_y
;;;
(defun mv_gnv (/ )
  (if n_vp_x
    (progn
      (setq mvs_p4 (list (read n_vp_x) (read n_vp_y) 0))
    )
    (progn
      (setq ans (getint "\n\tNumber of viewports in X. <1>: "))
      (if (= (type ans) 'INT)
        (setq mvs_p4 (list ans))
        (setq mvs_p4 (list 1))
      )
      (setq ans (getint "\n\tNumber of viewports in Y. <1>: "))
      (if (= (type ans) 'INT)
        (setq mvs_p4 (append mvs_p4 (list ans 0)))
        (setq mvs_p4 (append mvs_p4 (list 1   0)))
      )
    )
  )
)
;;;
;;; Get the horizontal and vertical distances between the viewports.
;;; Sets the global mvs_p3 which is a point containing the X and Y
;;; interstitial distances as the (car) and (cadr) of mvs_p3.
;;;
;;; mv_gid == MView_Get_Interstitial_Distances
;;;
(defun mv_gid (/ )
  (setq mvs_p3 (list 0.0 0.0 0.0))
  (if (> (car mvs_p4) 1)
    (progn
      (setq ans (getdist (strcat 
        "\n\tDistance between viewports in X. <"
        (mv_sts (car mvs_p3))
        ">: ")))
      (if (= (type ans) 'REAL)
        (setq mvs_p3 (list ans ans ans))
      )
    )
  )
  (if (> (cadr mvs_p4) 1)
    (progn
      (setq ans (getdist (strcat 
        "\n\tDistance between viewports in Y. <"
        (mv_sts (cadr mvs_p3))
        ">: ")))
      (if (= (type ans) 'REAL)
        (setq mvs_p3 (list (car mvs_p3) ans (caddr mvs_p3)))
      )
    )
  )
  mvs_p3
)
;;;
;;; Get set up for creating viewports.
;;; Sets the globals mvs_p1 and mvs_p2 which are the corners of the
;;; allowable area for viewports in paperspace coordinates.
;;;
;;; mv_gba == MView_Get_Bounding_Area
;;;
(defun mv_gba (/ )
  (if ll_crn
    (initget 1 "Default")
    (initget 1)
  )
  (graphscr)
  (setq ans (getpoint 
    (if ll_crn
      "\n\tBounding area for viewports.  Default/<First point >: "
      "\n\tBounding area for viewports.  First point: "
    )
  ))
  (if (= ans "Default")
    (progn
      (if ll_crn
        (setq mvs_p1 ll_crn
              mvs_p2 ur_crn
        )
        (progn
          (princ "\n\tNo default points defined. ")
          (exit)
        )
      )
    )
    (progn
      (initget 1)
      (setq mvs_p1 ans
            mvs_p2 (getcorner mvs_p1 "\n\tOther point: ")
      )
    )
  )
)
;;;
;;; Check that the corners given are on-screen.  If not, zoom so they are.
;;;
;;; mv_cba == MView_Check_Bounding_Area
;;;
(defun mv_cba (/ vs vc ss dx dy)
  (setq vs (getvar "viewsize")
        vc (getvar "viewctr")
        ss (getvar "screensize")
        dx (* vs (/ (car ss) (cadr ss)))
  ) 
  (if (or (< (car  mvs_p1) (- (car vc) (/ dx 2.0)))
          (< (cadr mvs_p1) (- (cadr vc) (/ vs 2.0)))
          (> (car  mvs_p2) (+ (car vc) (/ dx 2.0)))
          (> (cadr mvs_p2) (+ (cadr vc) (/ vs 2.0)))
      )
    (setq need_z T)
  )          
)
;;;
;;; Create a title block or Mview viewport layout from the code in the 
;;; default file.  This may be anything from inserting a block to actually 
;;; drawing every  component of the title block from scratch.  Any 
;;; single-line valid Lisp expression may be written in the default file 
;;; after the DATA name.
;;;
;;; mv_cfd == MView_Create_From_Defaults
;;;
(defun mv_cfd (/ cont theCmd)
  (setq cont T)
  (while cont
    (setq theCmd (mv_rpl))
    (if (= (substr theCmd 1 8) "END_ITEM")
      (progn
        (setq cont nil)
      )
      (mv_etl theCmd)
    )
  )
)
;;;
;;; Evaluate the line or string passed in.
;;;
;;; mv_etl == MView_Evaluate_The_Line
;;;
(defun mv_etl (str / )
  ;(princ str)(terpri)
  (if (eval(read str)) T nil)
)
;;;
;;; Read and parse out a line of the Lisp code found in the defaults
;;; file.  This must be done so that literal strings may be written
;;; directly in the defaults file, without having to escape all of them.
;;; We will do the escaping here, if necessary.
;;; 
;;; Return the escaped string
;;;
;;; mv_rpl == MView_Read_and_Parse_a_Line
;;;
(defun mv_rpl ( / line j k sl str)
  (if (setq line (read-line deffi))
    (progn
      (setq j 1 k 1 sl (strlen line) str "")
      (while (<= j sl)
        (if (= (setq sb_str (substr line j k)) "\"")
          (setq str (strcat str "\""))
          (setq str (strcat str sb_str))
        )
        (setq j (1+ j))
      )
    )
    (progn
      (princ "\n\tNo lines left to read. ")
      (exit)
    )
  )
  str
)
;;;
;;; Create a default definitions file in the current directory
;;; Read it out of this file (mvsetup.lsp) at the end of the file.
;;;
;;; mv_cdf == MView_Create_Defaults_File
;;;
(defun mv_cdf (/ deffi deffo cont line)
  ;; The default file name for mvsetup is "mvsetup.dfs".
  ;; Look for it in AutoCAD's search paths
  ;;
  (if (setq deffi (mv_lfx "mvsetup.lsp" "r"))
    (if (setq deffo (mv_lfx mv_xdf "w"))
      (progn
        (setq cont T)
        (while cont
          (setq line (read-line deffi))
          ;; Seek to the start of the default file definition
          (if (= (substr line 1 13) ";;; Start DDF")
            (setq cont nil)
          )
        )
        ;; Start writing the file
        ;; Throw away the first four characters of each line to the EOF.
        
        (while (setq line (read-line deffi))
          ;; Seek to the start of the default file definition
          (write-line (substr line 5) deffo)
        )
        (setq deffi (close deffi))
        (setq deffo (close deffo))
      )
      (princ (strcat "\nError opening " mv_xdf " for writing. "))
    )
    (princ (strcat "\nError opening " mv_xlf " for reading. "))
  )
)
;;;
;;; Look for an external definitions file in AutoCAD's search path
;;;
;;; mv_lfx == MView_Look_For_eXternal_file
;;;
(defun mv_lfx (f_name r_or_w / lfile)
  ;; Look for f_name in AutoCAD's search paths.
  (if (= r_or_w "w")
    (if (setq temp (open f_name r_or_w))
      temp                            ; Return file descriptor
      (progn
        (princ (strcat "\n\tCouldn't open " f_name " for writing. "))
        (exit)
      )
    )
    (if (setq lfile (findfile f_name))
      (if (setq temp (open lfile r_or_w))
        temp                          ; Return file descriptor
        (progn
          (princ (strcat "\n\tCouldn't open " f_name " for reading. "))
          (exit)
        )
      )
      nil                             ; or nil
    )
  )
)
;;;
;;; Change an existing setup, including the titleblock and/or viewports
;;;
;;; mv_coc == MView_Change_Or_Create?
;;;
(defun mv_coc ()
  (initget "Create Update")
  (setq ans (getkword "\n\tUpdate current setup/<Create>: "))
  (if (= ans "Update")
    -1                                ; Return -1
     1                                ; Return  1
  )
)
;;;
;;; Tilemode setup.
;;;
;;; mv_ == MView_
;;;
(defun mv_dos (/ temp xl yl)

  (princ "\n\tTILEMODE is set to 1;  cannot set up paperspace/modelspace")
  (princ "\n\tviewports unless TILEMODE is set to 0.  Release 10 setup: \n")
  
  (menucmd "S=UNITS0")
       
  (initget 7)                         ; No null input, negative or zero values
 
  (setq temp (getint "\n\tSelect the Units from the screen menu: "))
  (menucmd (strcat "S=U" (itoa temp)))

  (setq mv_olu (getvar "lunits"))     ; Store current linear units setting
  (if (= temp 5)                      ; Set linear units to new value
    (setvar "lunits" 2)               ; If metric
    (setvar "lunits" temp)            ; otherwise
  )                                   
  
  (initget 5)                         ; 0 ok, but no null or negative values
 
  (setq mv_sc (getreal "\n\tSelect the Scale from the screen menu: "))
  (cond
    ((= mv_sc 0)
      (progn
       (initget 7)              
       (setq mv_sc (getreal "\n\tEnter the scale: "))
       (setq mv_sc (float mv_sc))
      )
    )
  )
  (cond
    ((= temp 5)
      (menucmd "S=METRIC")
    )
    (T
      (menucmd "S=ENGLISH")
    )
  )
  (initget 5)                         ; 0 ok, but no null or negative values
  (setq xl (getdist "\n\tSelect the Paper size from the screen menu: "))
  (initget 5)                         ; 0 ok, but no null or negative values
  (setq yl (getdist))
  (cond
    ((= xl 0)
      (progn
       (initget 7)                    ; No null, negative or zero values
       (setq xl (getdist "\n\tEnter the Horizontal Dimension of the paper: "))
       (initget 7)                    ; No null, negative or zero values
       (setq yl (getdist "\n\tEnter the Vertical Dimension of the paper: "))
      )
    )
  )
  (setq xl (* mv_sc xl) 
        yl (* mv_sc yl)
  )
  (command  
    "_.LIMITS" "0,0" (list xl yl)
    "_.PLINE" "0,0" (list 0 yl) (list xl yl) (list xl 0) "0,0" "_C"
    "_.ZOOM" "a"
  )
  (menucmd "S=")
  (menucmd "S=")
  (menucmd "S=")
)
;;;
;;; C: function definition.
;;;
(defun c:mvs () (mv_sup))
(defun c:mvsetup () (mv_sup))

;;;
;;; The rest of this file in source form is the default file.  When creating
;;; a default file, each line following the line which contains "Start DDF"
;;; is read and written to the default file minus its first four characters.
;;; 
;;; This file contains definitions for ANSI A through E size title block 
;;; sheets and several viewport setup options.  If this file is ever 
;;; Kelvinated or protected, then this section should be stripped out and 
;;; shipped separately.  Some code changes would also be required.
;;;

;;; Start DDF
;;; ;;; Do NOT erase or change the first three lines
;;; ;;; Version 1.15 -- Copyright (c) Autodesk, Inc.  1990 - 1992
;;; ;;; MView Setup Defaults
;;; ;;; This file contains sets of defaults for setting up multiple viewport
;;; ;;; configurations in Paperspace in Release 12 of AutoCAD.  It is intended
;;; ;;; that you modify the entries given here to create your own customized
;;; ;;; versions.  The format of the file is as follows:
;;; ;;;
;;; ;;; The first part of the file is a list of the NAME's of the entries
;;; ;;; followed by an END_NAMES delimiter.
;;; ;;;
;;; ;;; Each of the names may have optional, comma delimited fields in the
;;; ;;; following order:  the first optional field is the file/path name of
;;; ;;; an AutoCAD drawing which is to be inserted at the local UCS origin
;;; ;;; when the name is selected, followed by a window in paperspace units
;;; ;;; which represents the extents (in paperspace) of the usable paper
;;; ;;; area.  This is the area which may be automatically filled with
;;; ;;; viewports.  Last is either mm or in which specifies whether the units
;;; ;;; used to construct the title block are specified in millimeters or
;;; ;;; inches.  If the area points are not specified this filed may follow
;;; ;;; the drawing name immediately.
;;; ;;;
;;; ;;; The data portion of the file for any NAME entry is of arbitrary
;;; ;;; length, and contains lines of AutoLISP code with all coordinates
;;; ;;; in Paperspace units.  It is terminated with END_DATA.
;;; ;;;
;;; ;;; Lines of AutoLisp code cannot be split across multiple lines.
;;; ;;;
;;; ;;; Lines may be commented out with ";".
;;;
;;; NAMES
;;; None
;;; ISO A4 Size(mm),iso_a4.dwg,(22.0 48.0 0.0),(198.0 278.0 0.0),mm
;;; ISO A3 Size(mm),iso_a3.dwg,(27.0 48.0 0.0),(408.0 285.0 0.0),mm
;;; ISO A2 Size(mm),iso_a2.dwg,(27.0 48.0 0.0),(582.0 408.0 0.0),mm
;;; ISO A1 Size(mm),iso_a1.dwg,(27.0 58.0 0.0),(819.0 572.0 0.0),mm
;;; ISO A0 Size(mm),iso_a0.dwg,(27.0 58.0 0.0),(1167.0 819.0 0.0),mm
;;; ANSI-V Size(in),ansi-v.dwg,(0.505 2.125 0.0),(7.995 9.5777 0.0),in
;;; ANSI-A Size(in),ansi-a.dwg,(0.375 2.255 0.0),(10.625 6.9477 0.0),in
;;; ANSI-B Size(in),ansi-b.dwg,(0.745 0.505 0.0),(10.005 10.495 0.0),in
;;; ANSI-C Size(in),ansi-c.dwg,(0.625 0.875 0.0),(15.125 16.125 0.0),in
;;; ANSI-D Size(in),ansi-d.dwg,(1.125 0.625 0.0),(25.255 21.375 0.0),in
;;; ANSI-E Size(in),ansi-e.dwg,(0.625 1.125 0.0),(35.755 32.875 0.0),in
;;; Arch/Engineering (24 x 36in) ,arch-a.dwg,(1.0 1.0 0.0),(30.5 23.0 0.0),in
;;; Generic D size Sheet (24 x 36in),gs24x36.dwg,(1.625 1.375 0.0),(33.625 22.625 0.0),in
;;; END_NAMES
;;;
;;; DATA
;;;
;;; NAME - None
;;; (princ)
;;; END_ITEM
;;;
;;; An ISO - A4 size sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 mm.
;;;
;;; NAME - ISO A4 Size(mm)
;;; (command "_.ZOOM" "_W" "-80,-5" "330,300")
;;; ;;;
;;; ;;; Trimming marks
;;; (command "_.LINE" "0,5" "0,0" "5,0" "")
;;; (command "_.LINE" "205,0" "210,0" "210,5" "")
;;; (command "_.LINE" "210,292" "210,297" "205,297" "")
;;; (command "_.LINE" "5,297" "0,297" "0,292" "")
;;; ;;;
;;; ;;; Drawing sheet frame
;;; (command "_.LINE" "20,10" "200,10" "200,287" "20,287" "_C")
;;; ;;;
;;; ;;; Title block
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "95,20" "200,20" "")
;;; (command "_.LINE" "20,30" "200,30" "")
;;; (command "_.LINE" "20,40" "200,40" "")
;;; (command "_.LINE" "20,46" "200,46" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "33,40" "33,46" "")
;;; (command "_.LINE" "50,30" "50,46" "")
;;; (command "_.LINE" "75,30" "75,40" "")
;;; (command "_.LINE" "95,10" "95,30" "")
;;; (command "_.LINE" "125,30" "125,40" "")
;;; (command "_.LINE" "150,30" "150,46" "")
;;; (command "_.LINE" "165,10" "165,20" "")
;;; (command "_.LINE" "180,10" "180,20" "")
;;; (command "_.LINE" "180,30" "180,40" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.STYLE" "ADESK2" "iso" "0" "1.0" "0" "" "")
;;; (command "_.TEXT" "21,42" "2.5" "0" "Itemref")
;;; (command "_.TEXT" "34,42" "2.5" "0" "Quantity")
;;; (command "_.TEXT" "51,42" "2.5" "0" "Title/Name, designation, material, dimension etc")
;;; (command "_.TEXT" "151,42" "2.5" "0" "Article No./Reference")
;;; (command "_.TEXT" "21,36" "2.5" "0" "Designed by")
;;; (command "_.TEXT" "51,36" "2.5" "0" "Checked by")
;;; (command "_.TEXT" "76,36" "2.5" "0" "Approved by - date")
;;; (command "_.TEXT" "126,36" "2.5" "0" "Filename")
;;; (command "_.TEXT" "152,36" "2.5" "0" "Date")
;;; (command "_.TEXT" "181,36" "2.5" "0" "Scale")
;;; (command "_.TEXT" "21,26" "2.5" "0" "Owner")
;;; (command "_.TEXT" "96,26" "2.5" "0" "Title/Name")
;;; (command "_.TEXT" "96,16" "2.5" "0" "Drawing number")
;;; (command "_.TEXT" "166,16" "2.5" "0" "Edition")
;;; (command "_.TEXT" "181,16" "2.5" "0" "Sheet")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontal
;;; (command "_.LINE" "20,280" "200,280" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "30,280" "30,287" "")
;;; (command "_.LINE" "151,280" "151,287" "")
;;; (command "_.LINE" "171,280" "171,287" "")
;;; (command "_.LINE" "186,280" "186,287" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.TEXT" "21,282.5" "2.5" "0" "RevNo")
;;; (command "_.TEXT" "31,282.5" "2.5" "0" "Revision note")
;;; (command "_.TEXT" "152,282.5" "2.5" "0" "Date")
;;; (command "_.TEXT" "172,282.5" "2.5" "0" "Signature")
;;; (command "_.TEXT" "187,282.5" "2.5" "0" "Checked")
;;; END_ITEM
;;;
;;; An ISO - A3 size sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 mm.
;;;
;;; NAME - ISO A3 Size(mm)
;;; (command "_.ZOOM" "_W" "-5,-5" "425,302")
;;; ;;;
;;; ;;; Trimming marks
;;; (command "_.LINE" "0,5" "0,0" "5,0" "")
;;; (command "_.LINE" "415,0" "420,0" "420,5" "")
;;; (command "_.LINE" "420,292" "420,297" "415,297" "")
;;; (command "_.LINE" "5,297" "0,297" "0,292" "")
;;; ;;;
;;; ;;; Drawing sheet frame
;;; (command "_.LINE" "25,10" "410,10" "410,287" "25,287" "_C")
;;; ;;;
;;; ;;; Title block
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "310,20" "410,20" "")
;;; (command "_.LINE" "240,30" "410,30" "")
;;; (command "_.LINE" "240,40" "410,40" "")
;;; (command "_.LINE" "240,46" "410,46" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "240,10" "240,46" "")
;;; (command "_.LINE" "250,40" "250,46" "")
;;; (command "_.LINE" "265,30" "265,46" "")
;;; (command "_.LINE" "290,30" "290,40" "")
;;; (command "_.LINE" "310,10" "310,30" "")
;;; (command "_.LINE" "340,30" "340,40" "")
;;; (command "_.LINE" "365,30" "365,46" "")
;;; (command "_.LINE" "380,10" "380,20" "")
;;; (command "_.LINE" "395,10" "395,20" "")
;;; (command "_.LINE" "395,30" "395,40" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.STYLE" "ADESK2" "iso" "0" "1.0" "0" "" "")
;;; (command "_.TEXT" "240.7,42" "2.5" "0" "Itemref")
;;; (command "_.TEXT" "251,42" "2.5" "0" "Quantity")
;;; (command "_.TEXT" "266,42" "2.5" "0" "Title/Name, designation, material, dimension etc")
;;; (command "_.TEXT" "366,42" "2.5" "0" "Article No./Reference")
;;; (command "_.TEXT" "241,36" "2.5" "0" "Designed by")
;;; (command "_.TEXT" "266,36" "2.5" "0" "Checked by")
;;; (command "_.TEXT" "291,36" "2.5" "0" "Approved by - date")
;;; (command "_.TEXT" "341,36" "2.5" "0" "Filename")
;;; (command "_.TEXT" "366,36" "2.5" "0" "Date")
;;; (command "_.TEXT" "396,36" "2.5" "0" "Scale")
;;; (command "_.TEXT" "241,26" "2.5" "0" "Owner")
;;; (command "_.TEXT" "311,26" "2.5" "0" "Title/Name")
;;; (command "_.TEXT" "311,16" "2.5" "0" "Drawing number")
;;; (command "_.TEXT" "381,16" "2.5" "0" "Edition")
;;; (command "_.TEXT" "396,16" "2.5" "0" "Sheet")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontal
;;; (command "_.LINE" "25,17" "205,17" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "35,10" "35,17" "")
;;; (command "_.LINE" "156,10" "156,17" "")
;;; (command "_.LINE" "176,10" "176,17" "")
;;; (command "_.LINE" "191,10" "191,17" "")
;;; (command "_.LINE" "205,10" "205,17" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.TEXT" "26,12.5" "2.5" "0" "RevNo")
;;; (command "_.TEXT" "36,12.5" "2.5" "0" "Revision note")
;;; (command "_.TEXT" "157,12.5" "2.5" "0" "Date")
;;; (command "_.TEXT" "177,12.5" "2.5" "0" "Signature")
;;; (command "_.TEXT" "192,12.5" "2.5" "0" "Checked")
;;; END_ITEM
;;;
;;; An ISO - A2 size sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 mm.
;;;
;;; NAME - ISO A2 Size(mm)
;;; (command "_.ZOOM" "_W" "-5,-5" "600,425")
;;; ;;;
;;; ;;; Trimming marks
;;; (command "_.LINE" "0,5" "0,0" "5,0" "")
;;; (command "_.LINE" "589,0" "594,0" "594,5" "")
;;; (command "_.LINE" "594,415" "594,420" "589,420" "")
;;; (command "_.LINE" "5,420" "0,420" "0,415" "")
;;; ;;;
;;; ;;; Drawing sheet frame
;;; (command "_.LINE" "25,10" "584,10" "584,410" "25,410" "_C")
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "484,20" "584,20" "")
;;; (command "_.LINE" "414,30" "584,30" "")
;;; (command "_.LINE" "414,40" "584,40" "")
;;; (command "_.LINE" "414,46" "584,46" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "414,10" "414,46" "")
;;; (command "_.LINE" "424,40" "424,46" "")
;;; (command "_.LINE" "439,30" "439,46" "")
;;; (command "_.LINE" "464,30" "464,40" "")
;;; (command "_.LINE" "484,10" "484,30" "")
;;; (command "_.LINE" "514,30" "514,40" "")
;;; (command "_.LINE" "539,30" "539,46" "")
;;; (command "_.LINE" "554,10" "554,20" "")
;;; (command "_.LINE" "569,10" "569,20" "")
;;; (command "_.LINE" "569,30" "569,40" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.STYLE" "ADESK2" "iso" "0" "1.0" "0" "" "")
;;; (command "_.TEXT" "414.7,42" "2.5" "0" "Itemref")
;;; (command "_.TEXT" "425,42" "2.5" "0" "Quantity")
;;; (command "_.TEXT" "440,42" "2.5" "0" "Title/Name, designation, material, dimension etc")
;;; (command "_.TEXT" "540,42" "2.5" "0" "Article No./Reference")
;;; (command "_.TEXT" "415,36" "2.5" "0" "Designed by")
;;; (command "_.TEXT" "440,36" "2.5" "0" "Checked by")
;;; (command "_.TEXT" "465,36" "2.5" "0" "Approved by - date")
;;; (command "_.TEXT" "515,36" "2.5" "0" "Filename")
;;; (command "_.TEXT" "540,36" "2.5" "0" "Date")
;;; (command "_.TEXT" "570,36" "2.5" "0" "Scale")
;;; (command "_.TEXT" "415,26" "2.5" "0" "Owner")
;;; (command "_.TEXT" "485,26" "2.5" "0" "Title/Name")
;;; (command "_.TEXT" "485,16" "2.5" "0" "Drawing number")
;;; (command "_.TEXT" "555,16" "2.5" "0" "Edition")
;;; (command "_.TEXT" "570,16" "2.5" "0" "Sheet")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontal
;;; (command "_.LINE" "25,17" "205,17" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "35,10" "35,17" "")
;;; (command "_.LINE" "156,10" "156,17" "")
;;; (command "_.LINE" "176,10" "176,17" "")
;;; (command "_.LINE" "191,10" "191,17" "")
;;; (command "_.LINE" "205,10" "205,17" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.TEXT" "26,12.5" "2.5" "0" "RevNo")
;;; (command "_.TEXT" "36,12.5" "2.5" "0" "Revision note")
;;; (command "_.TEXT" "157,12.5" "2.5" "0" "Date")
;;; (command "_.TEXT" "177,12.5" "2.5" "0" "Signature")
;;; (command "_.TEXT" "192,12.5" "2.5" "0" "Checked")
;;; END_ITEM
;;;
;;; An ISO - A1 size sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 mm.
;;;
;;; NAME - ISO A1 Size(mm)
;;; (command "_.ZOOM" "_W" "-5,-5" "846,599")
;;; ;;;
;;; ;;; Trimming marks
;;; (command "_.LINE" "0,5" "0,0" "5,0" "")
;;; (command "_.LINE" "836,0" "841,0" "841,5" "")
;;; (command "_.LINE" "841,589" "841,594" "836,594" "")
;;; (command "_.LINE" "5,594" "0,594" "0,589" "")
;;; ;;;
;;; ;;; Drawing sheet frame
;;; (command "_.LINE" "25,20" "821,20" "821,574" "25,574" "_C")
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "721,30" "821,30" "")
;;; (command "_.LINE" "651,40" "821,40" "")
;;; (command "_.LINE" "651,50" "821,50" "")
;;; (command "_.LINE" "651,56" "821,56" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "651,20" "651,56" "")
;;; (command "_.LINE" "661,50" "661,56" "")
;;; (command "_.LINE" "676,40" "676,56" "")
;;; (command "_.LINE" "701,40" "701,50" "")
;;; (command "_.LINE" "721,20" "721,40" "")
;;; (command "_.LINE" "751,40" "751,50" "")
;;; (command "_.LINE" "776,40" "776,56" "")
;;; (command "_.LINE" "791,20" "791,30" "")
;;; (command "_.LINE" "806,20" "806,30" "")
;;; (command "_.LINE" "806,40" "806,50" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.STYLE" "ADESK2" "iso" "0" "1.0" "0" "" "")
;;; (command "_.TEXT" "651.7,52" "2.5" "0" "Itemref")
;;; (command "_.TEXT" "662,52" "2.5" "0" "Quantity")
;;; (command "_.TEXT" "677,52" "2.5" "0" "Title/Name, designation, material, dimension etc")
;;; (command "_.TEXT" "777,52" "2.5" "0" "Article No./Reference")
;;; (command "_.TEXT" "652,46" "2.5" "0" "Designed by")
;;; (command "_.TEXT" "677,46" "2.5" "0" "Checked by")
;;; (command "_.TEXT" "702,46" "2.5" "0" "Approved by - date")
;;; (command "_.TEXT" "752,46" "2.5" "0" "Filename")
;;; (command "_.TEXT" "777,46" "2.5" "0" "Date")
;;; (command "_.TEXT" "807,46" "2.5" "0" "Scale")
;;; (command "_.TEXT" "652,36" "2.5" "0" "Owner")
;;; (command "_.TEXT" "722,36" "2.5" "0" "Title/Name")
;;; (command "_.TEXT" "722,26" "2.5" "0" "Drawing number")
;;; (command "_.TEXT" "792,26" "2.5" "0" "Edition")
;;; (command "_.TEXT" "807,26" "2.5" "0" "Sheet")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontal
;;; (command "_.LINE" "25,27" "205,27" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "35,20" "35,27" "")
;;; (command "_.LINE" "156,20" "156,27" "")
;;; (command "_.LINE" "176,20" "176,27" "")
;;; (command "_.LINE" "191,20" "191,27" "")
;;; (command "_.LINE" "205,20" "205,27" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.TEXT" "26,22.5" "2.5" "0" "RevNo")
;;; (command "_.TEXT" "36,22.5" "2.5" "0" "Revision note")
;;; (command "_.TEXT" "157,22.5" "2.5" "0" "Date")
;;; (command "_.TEXT" "177,22.5" "2.5" "0" "Signature")
;;; (command "_.TEXT" "192,22.5" "2.5" "0" "Checked")
;;; END_ITEM
;;;
;;; An ISO - A0 size sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 mm.
;;;
;;; NAME - ISO A0 Size(mm)
;;; (command "_.ZOOM" "_W" "-5,-5" "1194,846")
;;; ;;;
;;; ;;; Trimming marks
;;; (command "_.LINE" "0,5" "0,0" "5,0" "")
;;; (command "_.LINE" "1184,0" "1189,0" "1189,5" "")
;;; (command "_.LINE" "1189,836" "1189,841" "1184,841" "")
;;; (command "_.LINE" "5,841" "0,841" "0,836" "")
;;; ;;;
;;; ;;; Drawing sheet frame
;;; (command "_.LINE" "25,20" "1169,20" "1169,821" "25,821" "_C")
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "1069,30" "1169,30" "")
;;; (command "_.LINE" "999,40" "1169,40" "")
;;; (command "_.LINE" "999,50" "1169,50" "")
;;; (command "_.LINE" "999,56" "1169,56" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "999,20" "999,56" "")
;;; (command "_.LINE" "1009,50" "1009,56" "")
;;; (command "_.LINE" "1024,40" "1024,56" "")
;;; (command "_.LINE" "1049,40" "1049,50" "")
;;; (command "_.LINE" "1069,20" "1069,40" "")
;;; (command "_.LINE" "1099,40" "1099,50" "")
;;; (command "_.LINE" "1124,40" "1124,56" "")
;;; (command "_.LINE" "1139,20" "1139,30" "")
;;; (command "_.LINE" "1154,20" "1154,30" "")
;;; (command "_.LINE" "1154,40" "1154,50" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.STYLE" "ADESK2" "iso" "0" "1.0" "0" "" "")
;;; (command "_.TEXT" "999.7,52" "2.5" "0" "Itemref")
;;; (command "_.TEXT" "1010,52" "2.5" "0" "Quantity")
;;; (command "_.TEXT" "1025,52" "2.5" "0" "Title/Name, designation, material, dimension etc")
;;; (command "_.TEXT" "1125,52" "2.5" "0" "Article No./Reference")
;;; (command "_.TEXT" "1000,46" "2.5" "0" "Designed by")
;;; (command "_.TEXT" "1025,46" "2.5" "0" "Checked by")
;;; (command "_.TEXT" "1050,46" "2.5" "0" "Approved by - date")
;;; (command "_.TEXT" "1100,46" "2.5" "0" "Filename")
;;; (command "_.TEXT" "1125,46" "2.5" "0" "Date")
;;; (command "_.TEXT" "1155,46" "2.5" "0" "Scale")
;;; (command "_.TEXT" "1000,36" "2.5" "0" "Owner")
;;; (command "_.TEXT" "1070,36" "2.5" "0" "Title/Name")
;;; (command "_.TEXT" "1070,26" "2.5" "0" "Drawing number")
;;; (command "_.TEXT" "1140,26" "2.5" "0" "Edition")
;;; (command "_.TEXT" "1155,26" "2.5" "0" "Sheet")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontal
;;; (command "_.LINE" "25,27" "205,27" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "35,20" "35,27" "")
;;; (command "_.LINE" "156,20" "156,27" "")
;;; (command "_.LINE" "176,20" "176,27" "")
;;; (command "_.LINE" "191,20" "191,27" "")
;;; (command "_.LINE" "205,20" "205,27" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.TEXT" "26,22.5" "2.5" "0" "RevNo")
;;; (command "_.TEXT" "36,22.5" "2.5" "0" "Revision note")
;;; (command "_.TEXT" "157,22.5" "2.5" "0" "Date")
;;; (command "_.TEXT" "177,22.5" "2.5" "0" "Signature")
;;; (command "_.TEXT" "192,22.5" "2.5" "0" "Checked")
;;; END_ITEM
;;;
;;;
;;; An ANSI - A size Vertical sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 inch.
;;; 
;;; NAME - ANSI-V Size(in)
;;; (command "_.ZOOM" "_W" "-0.5,-0.5" "9.0,11.5")
;;; (command "_.LINE" "0,0" "8.5,0" "8.5,11" "0,11" "_C")
;;; (command "_.LINE" ".38,.25" "8.12,.25" "8.12,10.75" ".38,10.75" "_C")
;;; ;;; Bottom microfilm alignment arrow
;;; (command "_.PLINE" "4.25,0" "_W" "0.02" "" "4.25,.1" "")
;;; (command "_.SOLID" "4.1,.1" "4.4,.1" "4.25,.25" "" "")
;;; ;;; Right microfilm alignment arrow
;;; (command "_.PLINE" "8.37,5.5" "_W" "0.02" "" "8.27,5.5" "")
;;; (command "_.SOLID" "8.27,5.35" "8.27,5.65" "8.12,5.5" "" "")
;;; ;;; Top microfilm alignment arrow
;;; (command "_.PLINE" "4.25,11" "_W" "0.02" "" "4.25,10.9" "")
;;; (command "_.SOLID" "4.1,10.9" "4.4,10.9" "4.25,10.75" "" "")
;;; ;;; Left microfilm alignment arrow
;;; (command "_.PLINE" ".13,5.5" "_W" "0.02" "" ".23,5.5" "")
;;; (command "_.SOLID" ".23,5.35" ".23,5.65" ".38,5.5" "" "")
;;; ;;;
;;; ;;; Title block
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "1.87,0.25" "1.87,2"    "8.12,2" "")
;;; (command "_.LINE" "1.87,.565" "3.87,.565" "")
;;; (command "_.LINE" "1.87,.88"  "8.12,.88"  "")
;;; (command "_.LINE" "3.87,.5"   "8.12,.5"   "")
;;; (command "_.LINE" "3.87,1.5"  "8.12,1.5"  "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "3.87,0.25" "3.87,2"  "")
;;; (command "_.LINE" "4.87,.25"  "4.87,.5"  "")
;;; (command "_.LINE" "6.37,.25"  "6.37,.5"  "")
;;; (command "_.LINE" "4.25,.5"   "4.25,.88" "")
;;; (command "_.LINE" "5.37,.5"   "5.37,.88" "")
;;; (command "_.LINE" "7.74,.5"   "7.74,.88" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.STYLE" "ADESK1" "romans" "0" "1.0" "0" "" "" "")
;;; (command "_.TEXT" "3.9223,.3425" "0.065" "0" "SCALE")
;;; (command "_.TEXT" "6.4228,.3425" "0.065" "0" "SHEET")
;;; (command "_.TEXT" "3.9579,.7659" "0.065" "0" "SIZE")
;;; (command "_.TEXT" "4.3189,.7659" "0.065" "0" "FSCM NO.")
;;; (command "_.TEXT" "5.4410,.7659" "0.065" "0" "DWG NO.")
;;; (command "_.TEXT" "7.8205,.7659" "0.065" "0" "REV")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "2.62,10.5" "8.12,10.5" "")
;;; (command "_.LINE" "2.62,10.25" "8.12,10.25" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "2.62,10.75" "2.62,9.7027" "")
;;; (command "_.LINE" "3.12,10.25" "3.12,10.5" "")
;;; (command "_.LINE" "3.50,10.25" "3.50,10.5" "")
;;; (command "_.LINE" "6.24,10.25" "6.24,10.5" "")
;;; (command "_.LINE" "7.12,10.25" "7.12,10.5" "")
;;; ;;;
;;; ;;; Revision bar text
;;; ;;;
;;; (command "_.TEXT" "5.3302,10.5825" "0.065" "0" "REVISIONS")
;;; (command "_.TEXT" "2.7287,10.3403" "0.065" "0" "ZONE")
;;; (command "_.TEXT" "3.2001,10.3403" "0.065" "0" "REV")
;;; (command "_.TEXT" "4.5020,10.3403" "0.065" "0" "DESCRIPTION")
;;; (command "_.TEXT" "6.5677,10.3403" "0.065" "0" "DATE")
;;; (command "_.TEXT" "7.3614,10.3403" "0.065" "0" "APPROVED")
;;; END_ITEM
;;; 
;;; An ANSI - A size sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 inch.
;;; 
;;; NAME - ANSI-A Size(in)
;;; (command "_.ZOOM" "_W" "-0.5,-0.5" "11.5,9.0")
;;; (command "_.LINE" "0,0" "11,0" "11,8.5" "0,8.5" "_C")
;;; (command "_.LINE" ".25,.38" "10.75,.38" "10.75,8.12" ".25,8.12" "_C")
;;; ;;; Bottom microfilm alignment arrow
;;; (command "_.PLINE" "5.5,.13" "_W" "0.02" "" "5.5,.23" "")
;;; (command "_.SOLID" "5.35,.23" "5.65,.23" "5.5,.38" "" "")
;;; ;;; Right microfilm alignment arrow
;;; (command "_.PLINE" "11,4.25" "_W" "0.02" "" "10.9,4.25" "")
;;; (command "_.SOLID" "10.9,4.1" "10.9,4.4" "10.75,4.25" "" "")
;;; ;;; Top microfilm alignment arrow
;;; (command "_.PLINE" "5.5,8.37" "_W" "0.02" "" "5.5,8.27" "")
;;; (command "_.SOLID" "5.35,8.27" "5.65,8.27" "5.5,8.12" "" "")
;;; ;;; Left microfilm alignment arrow
;;; (command "_.PLINE" "0,4.25" "_W" "0.02" "" ".1,4.25" "")
;;; (command "_.SOLID" ".1,4.1" ".1,4.4" ".25,4.25" "" "")
;;; ;;;
;;; ;;; Title block
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "4.5,.38"   "4.5,2.13"   "10.75,2.13" "")
;;; (command "_.LINE" "4.5,.695"  "6.5,.695"   "")
;;; (command "_.LINE" "4.5,1.01"  "10.75,1.01" "")
;;; (command "_.LINE" "6.5,.63"   "10.75,.63"  "")
;;; (command "_.LINE" "6.5,1.63"  "10.75,1.63" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "6.5,0.38" "6.5,2.13"  "")
;;; (command "_.LINE" "7.5,.38"   "7.5,.63"    "")
;;; (command "_.LINE" "9.0,.38"   "9.0,.63"    "")
;;; (command "_.LINE" "6.88,.63"  "6.88,1.01"  "")
;;; (command "_.LINE" "8,.63"     "8,1.01"     "")
;;; (command "_.LINE" "10.37,.63" "10.37,1.01" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.STYLE" "ADESK1" "romans" "0" "1.0" "0" "" "" "")
;;; (command "_.TEXT" "6.5523,0.4725" "0.065" "0" "SCALE")
;;; (command "_.TEXT" "9.0528,0.4725" "0.065" "0" "SHEET")
;;; (command "_.TEXT" "6.5879,0.8959" "0.065" "0" "SIZE")
;;; (command "_.TEXT" "6.9489,0.8959" "0.065" "0" "FSCM NO.")
;;; (command "_.TEXT" "8.0710,0.8959" "0.065" "0" "DWG NO.")
;;; (command "_.TEXT" "10.4505,0.8959" "0.065" "0" "REV")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "5.25,7.87" "10.75,7.87" "")
;;; (command "_.LINE" "5.25,7.62" "10.75,7.62" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "5.25,8.12" "5.25,7.0727" "")
;;; (command "_.LINE" "5.75,7.62" "5.75,7.87" "")
;;; (command "_.LINE" "6.13,7.62" "6.13,7.87" "")
;;; (command "_.LINE" "8.87,7.62" "8.87,7.87" "")
;;; (command "_.LINE" "9.75,7.62" "9.75,7.87" "")
;;; ;;;
;;; ;;; Revision bar text
;;; ;;;
;;; (command "_.TEXT" "7.9602,7.9525" "0.065" "0" "REVISIONS")
;;; (command "_.TEXT" "5.3587,7.7103" "0.065" "0" "ZONE")
;;; (command "_.TEXT" "5.8301,7.7103" "0.065" "0" "REV")
;;; (command "_.TEXT" "7.1320,7.7103" "0.065" "0" "DESCRIPTION")
;;; (command "_.TEXT" "9.1977,7.7103" "0.065" "0" "DATE")
;;; (command "_.TEXT" "9.9914,7.7103" "0.065" "0" "APPROVED")
;;; END_ITEM
;;; 
;;; An ANSI - B size sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 inch.
;;; 
;;; NAME - ANSI-B Size(in)
;;; (command "_.ZOOM" "_W" "-0.5,-0.5" "17.5,11.5")
;;; (command "_.LINE" "0,0" "17,0" "17,11" "0,11" "_C")
;;; (command "_.LINE" ".62,.38" "16.38,.38" "16.38,10.62" ".62,10.62" "_C")
;;; ;;; Bottom microfilm alignment arrow
;;; (command "_.PLINE" "8.5,.13" "_W" "0.02" "" "8.5,.23" "")
;;; (command "_.SOLID" "8.35,.23" "8.65,.23" "8.5,.38" "" "")
;;; ;;; Right microfilm alignment arrow
;;; (command "_.PLINE" "16.62,5.5" "_W" "0.02" "" "16.52,5.5" "")
;;; (command "_.SOLID" "16.52,5.35" "16.52,5.65" "16.38,5.5" "" "")
;;; ;;; Top microfilm alignment arrow
;;; (command "_.PLINE" "8.5,10.87" "_W" "0.02" "" "8.5,10.77" "")
;;; (command "_.SOLID" "8.35,10.77" "8.65,10.77" "8.5,10.62" "" "")
;;; ;;; Left microfilm alignment arrow
;;; (command "_.PLINE" ".38,5.5" "_W" "0.02" "" ".48,5.5" "")
;;; (command "_.SOLID" ".48,5.35" ".48,5.65" ".62,5.5" "" "")
;;; ;;;
;;; ;;; Title block
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "10.13,.38"   "10.13,2.13"   "16.38,2.13" "")
;;; (command "_.LINE" "10.13,.695"  "12.13,.695"   "")
;;; (command "_.LINE" "10.13,1.01"  "16.38,1.01" "")
;;; (command "_.LINE" "12.13,.63"   "16.38,.63"  "")
;;; (command "_.LINE" "12.13,1.63"  "16.38,1.63" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "12.13,0.38" "12.13,2.13"  "")
;;; (command "_.LINE" "13.13,.38" "13.13,.63"   "")
;;; (command "_.LINE" "14.63,.38" "14.63,.63"   "")
;;; (command "_.LINE" "12.51,.63" "12.51,1.01"  "")
;;; (command "_.LINE" "13.63,.63" "13.63,1.01"  "")
;;; (command "_.LINE" "16,.63"    "16,1.01"     "")
;;; ;;;
;;; ;;; Text
;;; (command "_.STYLE" "ADESK1" "romans" "0" "1.0" "0" "" "" "")
;;; (command "_.TEXT" "12.1823,0.4725" "0.065" "0" "SCALE")
;;; (command "_.TEXT" "14.6828,0.4725" "0.065" "0" "SHEET")
;;; (command "_.TEXT" "12.2179,0.8959" "0.065" "0" "SIZE")
;;; (command "_.TEXT" "12.5789,0.8959" "0.065" "0" "FSCM NO.")
;;; (command "_.TEXT" "13.7010,0.8959" "0.065" "0" "DWG NO.")
;;; (command "_.TEXT" "16.0805,0.8959" "0.065" "0" "REV")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "10.88,10.37" "16.38,10.37" "")
;;; (command "_.LINE" "10.88,10.12" "16.38,10.12" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "10.88,10.62" "10.88,9.5727" "")
;;; (command "_.LINE" "11.38,10.12" "11.38,10.37" "")
;;; (command "_.LINE" "11.76,10.12" "11.76,10.37" "")
;;; (command "_.LINE" "14.5,10.12" "14.5,10.37" "")
;;; (command "_.LINE" "15.38,10.12" "15.38,10.37" "")
;;; ;;;
;;; ;;; Revision bar text
;;; ;;;
;;; (command "_.TEXT" "13.5902,10.4525" "0.065" "0" "REVISIONS")
;;; (command "_.TEXT" "10.9887,10.2103" "0.065" "0" "ZONE")
;;; (command "_.TEXT" "11.4601,10.2103" "0.065" "0" "REV")
;;; (command "_.TEXT" "12.7620,10.2103" "0.065" "0" "DESCRIPTION")
;;; (command "_.TEXT" "14.8277,10.2103" "0.065" "0" "DATE")
;;; (command "_.TEXT" "15.6214,10.2103" "0.065" "0" "APPROVED")
;;; END_ITEM
;;; 
;;; An ANSI - C size sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 inch.
;;; 
;;; NAME - ANSI-C Size(in)
;;; (command "_.ZOOM" "_W" "-0.5,-0.5" "22.5,17.5")
;;; (command "_.LINE" "0,0" "22,0" "22,17" "0,17" "_C")
;;; (command "_.LINE" ".5,.75" "21.5,.75" "21.5,16.25" ".5,16.25" "_C")
;;; (command "_.LINE" "5.5,0.375" "5.5,0.75" "")
;;; (command "_.ARRAY" (entlast) "" "_R" "2" "2" "15.875" "11")
;;; (command "_.LINE" "0.125,4.25" "0.5,4.25" "")
;;; (command "_.ARRAY" (entlast) "" "_R" "2" "2" "8.5" "21.375")
;;; ;;;
;;; ;;; Bottom microfilm alignment arrow
;;; (command "_.PLINE" "11,.5" "_W" "0.02" "" "11,.6" "")
;;; (command "_.SOLID" "10.85,.6" "11.15,.6" "11,.75" "" "")
;;; ;;; Right microfilm alignment arrow
;;; (command "_.PLINE" "21.75,8.5" "_W" "0.02" "" "21.65,8.5" "")
;;; (command "_.SOLID" "21.65,8.35" "21.65,8.65" "21.5,8.5" "" "")
;;; ;;; Top microfilm alignment arrow
;;; (command "_.PLINE" "11,16.5" "_W" "0.02" "" "11,16.4" "")
;;; (command "_.SOLID" "10.85,16.4" "11.15,16.4" "11,16.25" "" "")
;;; ;;; Left microfilm alignment arrow
;;; (command "_.PLINE" ".25,8.5" "_W" "0.02" "" ".35,8.5" "")
;;; (command "_.SOLID" ".35,8.35" ".35,8.65" ".5,8.5" "" "")
;;; ;;;
;;; ;;; Title block
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "15.25,0.75"   "15.25,2.5"   "21.50,2.5" "")
;;; (command "_.LINE" "15.25,1.065"  "17.25,1.065"   "")
;;; (command "_.LINE" "15.25,1.38"  "21.5,1.38" "")
;;; (command "_.LINE" "17.25,1"   "21.5,1"  "")
;;; (command "_.LINE" "17.25,2"  "21.5,2" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "17.25,0.75" "17.25,2.5"  "")
;;; (command "_.LINE" "18.25,0.75" "18.25,1"    "")
;;; (command "_.LINE" "19.75,0.75" "19.75,1"    "")
;;; (command "_.LINE" "17.63,1"    "17.63,1.38" "")
;;; (command "_.LINE" "18.75,1"    "18.75,1.38" "")
;;; (command "_.LINE" "21.12,1"    "21.12,1.38" "")
;;; ;;;
;;; ;;; Text
;;; (command "_.STYLE" "ADESK1" "romans" "0" "1.0" "0" "" "" "")
;;; (command "_.TEXT" "17.3023,0.8425" "0.065" "0" "SCALE")
;;; (command "_.TEXT" "19.8028,0.8425" "0.065" "0" "SHEET")
;;; (command "_.TEXT" "17.3379,1.2659" "0.065" "0" "SIZE")
;;; (command "_.TEXT" "17.6989,1.2659" "0.065" "0" "FSCM NO.")
;;; (command "_.TEXT" "18.8210,1.2659" "0.065" "0" "DWG NO.")
;;; (command "_.TEXT" "21.2005,1.2659" "0.065" "0" "REV")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "16,16" "21.5,16" "")
;;; (command "_.LINE" "16,15.75" "21.5,15.75" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "16,16.25" "16,15.2027" "")
;;; (command "_.LINE" "16.5,15.75" "16.5,16" "")
;;; (command "_.LINE" "16.88,15.75" "16.88,16" "")
;;; (command "_.LINE" "19.62,15.75" "19.62,16" "")
;;; (command "_.LINE" "20.5,15.75" "20.5,16" "")
;;; ;;;
;;; ;;; Revision bar text
;;; ;;;
;;; (command "_.TEXT" "18.7102,16.0825" "0.065" "0" "REVISIONS")
;;; (command "_.TEXT" "16.1087,15.8403" "0.065" "0" "ZONE")
;;; (command "_.TEXT" "16.5801,15.8403" "0.065" "0" "REV")
;;; (command "_.TEXT" "17.8820,15.8403" "0.065" "0" "DESCRIPTION")
;;; (command "_.TEXT" "19.9477,15.8403" "0.065" "0" "DATE")
;;; (command "_.TEXT" "20.7414,15.8403" "0.065" "0" "APPROVED")
;;; ;;;
;;; (command "_.TEXT" "_MC"  "0.25,2.125"  "0.25" "0" "A")
;;; (command "_.TEXT" "_MC"  "0.25,6.375"  "0.25" "0" "B")
;;; (command "_.TEXT" "_MC"  "0.25,10.625" "0.25" "0" "C")
;;; (command "_.TEXT" "_MC"  "0.25,14.875" "0.25" "0" "D")
;;; (command "_.TEXT" "_MC" "21.75,2.125"  "0.25" "0" "A")
;;; (command "_.TEXT" "_MC" "21.75,6.375"  "0.25" "0" "B")
;;; (command "_.TEXT" "_MC" "21.75,10.625" "0.25" "0" "C")
;;; (command "_.TEXT" "_MC" "21.75,14.875" "0.25" "0" "D")
;;; ;;;
;;; (command "_.TEXT" "_MC" "19.25,0.5"    "0.25" "0" "1")
;;; (command "_.TEXT" "_MC" "13.75,0.5"    "0.25" "0" "2")
;;; (command "_.TEXT" "_MC"  "8.25,0.5"    "0.25" "0" "3")
;;; (command "_.TEXT" "_MC"  "2.75,0.5"    "0.25" "0" "4")
;;; ;;;
;;; (command "_.TEXT" "_MC" "19.25,16.5"   "0.25" "0" "1")
;;; (command "_.TEXT" "_MC" "13.75,16.5"   "0.25" "0" "2")
;;; (command "_.TEXT" "_MC"  "8.25,16.5"   "0.25" "0" "3")
;;; (command "_.TEXT" "_MC"  "2.75,16.5"   "0.25" "0" "4")
;;; END_ITEM
;;; 
;;; An ANSI - D size sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 inch.
;;; 
;;; NAME - ANSI-D Size(in)
;;; (command "_.ZOOM" "_W" "-0.5,-0.5" "34.5,22.5")
;;; (command "_.LINE" "0,0" "34,0" "34,22" "0,22" "_C")
;;; (command "_.LINE" "1,.5" "33,.5" "33,21.5" "1,21.5" "_C")
;;; (command "_.LINE" "4.25,.125" "4.25,.5" "")
;;; (command "_.ARRAY" (entlast) "" "_R" "2" "8" "21.375" "4.25")
;;; (command "_.LINE" ".5,5.5" "1,5.5" "")
;;; (command "_.ARRAY" (entlast) "" "_R" "3" "2" "5.5" "32.5")
;;; ;;;
;;; ;;; Bottom microfilm alignment arrow
;;; (command "_.PLINE" "17,.1" "_W" "0.02" "" "17,.3" "")
;;; (command "_.SOLID" "16.8,.3" "17.2,.3" "17,.5" "" "")
;;; ;;; Right microfilm alignment arrow
;;; (command "_.PLINE" "33.4,11" "_W" "0.02" "" "33.4,11" "")
;;; (command "_.SOLID" "33.2,10.8" "33.2,11.2" "33,11" "" "")
;;; ;;; Top microfilm alignment arrow
;;; (command "_.PLINE" "17,21.9" "_W" "0.02" "" "17,21.7" "")
;;; (command "_.SOLID" "16.8,21.7" "17.2,21.7" "17,21.5" "" "")
;;; ;;; Left microfilm alignment arrow
;;; (command "_.PLINE" ".6,11" "_W" "0.02" "" ".8,11" "")
;;; (command "_.SOLID" ".8,10.8" ".8,11.2" "1,11" "" "")
;;; ;;;
;;; ;;;
;;; ;;; Title block
;;; ;;;
;;; (command "_.LINE" "25.38,.5" "25.38,3" "33,3" "")
;;; (command "_.LINE" "27.88,.5" "27.88,3" "")
;;; (command "_.LINE" "27.88,.75" "33,.75" "")
;;; (command "_.LINE" "25.38,1.25" "33,1.25" "")
;;; (command "_.LINE" "27.88,2.37" "33,2.37" "")
;;; (command "_.LINE" "25.38,.875" "27.88,.875" "")
;;; ;;;
;;; (command "_.LINE" "28.87,.5" "28.87,.75" "")
;;; (command "_.LINE" "31.25,.5" "31.25,.75" "")
;;; (command "_.LINE" "28.26,.75" "28.26,1.25" "")
;;; (command "_.LINE" "29.51,.75" "29.51,1.25" "")
;;; (command "_.LINE" "32.5,.75" "32.5,1.25" "")
;;; ;;;
;;; (command "_.STYLE" "ADESK1" "romans" "0" "1.0" "0" "" "" "")
;;; (command "_.TEXT" "27.9323,0.5925" "0.065" "0" "SCALE")
;;; (command "_.TEXT" "31.3028,0.5925" "0.065" "0" "SHEET")
;;; (command "_.TEXT" "27.9679,1.1359" "0.065" "0" "SIZE")
;;; (command "_.TEXT" "28.3289,1.1359" "0.065" "0" "FSCM NO.")
;;; (command "_.TEXT" "29.5810,1.1359" "0.065" "0" "DWG NO.")
;;; (command "_.TEXT" "32.6405,1.1359" "0.065" "0" "REV")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "26,21.25" "33,21.25" "")
;;; (command "_.LINE" "26,21" "33,21" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "26,20.4527" "26,21.5" "")
;;; (command "_.LINE" "26.5,21"  "26.5,21.25" "")
;;; (command "_.LINE" "26.88,21" "26.88,21.25" "")
;;; (command "_.LINE" "31.12,21" "31.12,21.25" "")
;;; (command "_.LINE" "32,21"    "32,21.25" "")
;;; ;;;
;;; ;;; Revision bar text
;;; ;;;
;;; (command "_.TEXT" "29.5746,21.3325" "0.065" "0" "REVISIONS")
;;; (command "_.TEXT" "26.1087,21.0903" "0.065" "0" "ZONE")
;;; (command "_.TEXT" "26.5801,21.0903" "0.065" "0" "REV")
;;; (command "_.TEXT" "28.7464,21.0903" "0.065" "0" "DESCRIPTION")
;;; (command "_.TEXT" "31.4477,21.0903" "0.065" "0" "DATE")
;;; (command "_.TEXT" "32.2477,21.0903" "0.065" "0" "APPROVED")
;;; ;;;
;;; (command "_.TEXT" "_MC"  "0.5,2.75" "0.25" "0" "A")
;;; (command "_.TEXT" "_MC"  "0.5,8.25" "0.25" "0" "B")
;;; (command "_.TEXT" "_MC"  "0.5,13.75" "0.25" "0" "C")
;;; (command "_.TEXT" "_MC"  "0.5,19.25" "0.25" "0" "D")
;;; (command "_.TEXT" "_MC" "33.5,2.75" "0.25" "0" "A")
;;; (command "_.TEXT" "_MC" "33.5,8.25" "0.25" "0" "B")
;;; (command "_.TEXT" "_MC" "33.5,13.75" "0.25" "0" "C")
;;; (command "_.TEXT" "_MC" "33.5,19.25" "0.25" "0" "D")
;;; ;;;
;;; (command "_.TEXT" "_MC" "2.125,0.25"  "0.25" "0" "8")
;;; (command "_.TEXT" "_MC" "6.375,0.25"  "0.25" "0" "7")
;;; (command "_.TEXT" "_MC" "10.625,0.25"  "0.25" "0" "6")
;;; (command "_.TEXT" "_MC" "14.875,0.25"  "0.25" "0" "5")
;;; (command "_.TEXT" "_MC" "19.125,0.25"  "0.25" "0" "4")
;;; (command "_.TEXT" "_MC" "23.375,0.25"  "0.25" "0" "3")
;;; (command "_.TEXT" "_MC" "27.625,0.25"  "0.25" "0" "2")
;;; (command "_.TEXT" "_MC" "31.875,0.25"  "0.25" "0" "1")
;;; ;;;
;;; (command "_.TEXT" "_MC" "2.125,21.75"  "0.25" "0" "8")
;;; (command "_.TEXT" "_MC" "6.375,21.75"  "0.25" "0" "7")
;;; (command "_.TEXT" "_MC" "10.625,21.75"  "0.25" "0" "6")
;;; (command "_.TEXT" "_MC" "14.875,21.75"  "0.25" "0" "5")
;;; (command "_.TEXT" "_MC" "19.125,21.75"  "0.25" "0" "4")
;;; (command "_.TEXT" "_MC" "23.375,21.75"  "0.25" "0" "3")
;;; (command "_.TEXT" "_MC" "27.625,21.75"  "0.25" "0" "2")
;;; (command "_.TEXT" "_MC" "31.875,21.75"  "0.25" "0" "1")
;;; END_ITEM
;;; 
;;; An ANSI - E size sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 inch.
;;; 
;;; NAME - ANSI-E Size(in)
;;; (command "_.ZOOM" "_W" "-0.5,-0.5" "44.5,34.5")
;;; (command "_.LINE" "0,0" "44,0" "44,34" "0,34" "_C")
;;; (command "_.LINE" ".5,1" "43.5,1" "43.5,33" ".5,33" "_C")
;;; (command "_.LINE" "5.5,.5" "5.5,1" "")
;;; (command "_.ARRAY" (entlast) "" "_R" "2" "8" "32.5" "5.5")
;;; (command "_.LINE" ".125,4.25" ".5,4.25" "")
;;; (command "_.ARRAY" (entlast) "" "_R" "7" "2" "4.25" "43.375")
;;; ;;;
;;; ;;; Bottom microfilm alignment arrow
;;; (command "_.PLINE" "22,.6" "_W" "0.02" "" "22,.8" "")
;;; (command "_.SOLID" "21.8,.8" "22.2,.8" "22,1" "" "")
;;; ;;; Right microfilm alignment arrow
;;; (command "_.PLINE" "43.9,17" "_W" "0.02" "" "43.7,17" "")
;;; (command "_.SOLID" "43.7,16.8" "43.7,17.2" "43.5,17" "" "")
;;; ;;; Top microfilm alignment arrow
;;; (command "_.PLINE" "22,33.4" "_W" "0.02" "" "22,33.2" "")
;;; (command "_.SOLID" "21.8,33.2" "22.2,33.2" "22,33" "" "")
;;; ;;; Left microfilm alignment arrow
;;; (command "_.PLINE" ".1,17" "_W" "0.02" "" ".3,17" "")
;;; (command "_.SOLID" ".3,16.8" ".3,17.2" ".5,17" "" "")
;;; ;;;
;;; ;;;
;;; ;;; Title block
;;; ;;;
;;; (command "_.LINE" "35.88,1" "35.88,3.5" "43.5,3.5" "")
;;; (command "_.LINE" "35.88,1.375" "38.38,1.375" "")
;;; (command "_.LINE" "35.88,1.75" "43.5,1.75" "")
;;; (command "_.LINE" "38.38,1.25" "43.5,1.25" "")
;;; (command "_.LINE" "38.38,2.87" "43.5,2.87" "")
;;; ;;;
;;; (command "_.LINE" "38.38,1" "38.38,3.5" "")
;;; (command "_.LINE" "39.37,1" "39.37,1.25" "")
;;; (command "_.LINE" "41.75,1" "41.75,1.25" "")
;;; (command "_.LINE" "38.76,1.25" "38.76,1.75" "")
;;; (command "_.LINE" "40.01,1.25" "40.01,1.75" "")
;;; (command "_.LINE" "43,1.25" "43,1.75" "")
;;; ;;;
;;; (command "_.STYLE" "ADESK1" "romans" "0" "1.0" "0" "" "" "")
;;; (command "_.TEXT" "38.4323,1.0925" "0.065" "0" "SCALE")
;;; (command "_.TEXT" "41.8028,1.0925" "0.065" "0" "SHEET")
;;; (command "_.TEXT" "38.4679,1.6359" "0.065" "0" "SIZE")
;;; (command "_.TEXT" "38.8289,1.6359" "0.065" "0" "FSCM NO.")
;;; (command "_.TEXT" "40.0810,1.6359" "0.065" "0" "DWG NO.")
;;; (command "_.TEXT" "43.1405,1.6359" "0.065" "0" "REV")
;;; ;;;
;;; ;;; Revision bar
;;; ;;;
;;; ;;; Horizontals
;;; (command "_.LINE" "36.5,32.75" "43.5,32.75" "")
;;; (command "_.LINE" "36.5,32.5" "43.5,32.5" "")
;;; ;;;
;;; ;;; Verticals
;;; (command "_.LINE" "36.5,31.9527" "36.5,33" "")
;;; (command "_.LINE" "37,32.5"    "37,32.75" "")
;;; (command "_.LINE" "37.38,32.5" "37.38,32.75" "")
;;; (command "_.LINE" "41.62,32.5" "41.62,32.75" "")
;;; (command "_.LINE" "42.5,32.5"  "42.5,32.75" "")
;;; ;;;
;;; ;;; Revision bar text
;;; ;;;
;;; (command "_.TEXT" "40.0746,32.8325" "0.065" "0" "REVISIONS")
;;; (command "_.TEXT" "36.6087,32.5903" "0.065" "0" "ZONE")
;;; (command "_.TEXT" "37.0801,32.5903" "0.065" "0" "REV")
;;; (command "_.TEXT" "39.2464,32.5903" "0.065" "0" "DESCRIPTION")
;;; (command "_.TEXT" "41.9477,32.5903" "0.065" "0" "DATE")
;;; (command "_.TEXT" "42.7477,32.5903" "0.065" "0" "APPROVED")
;;; ;;;
;;; (command "_.TEXT" "_MC" "0.25,2.125"  "0.25" "0" "A")
;;; (command "_.TEXT" "_MC" "0.25,6.375"  "0.25" "0" "B")
;;; (command "_.TEXT" "_MC" "0.25,10.625" "0.25" "0" "C")
;;; (command "_.TEXT" "_MC" "0.25,14.875" "0.25" "0" "D")
;;; (command "_.TEXT" "_MC" "0.25,19.125" "0.25" "0" "E")
;;; (command "_.TEXT" "_MC" "0.25,23.375" "0.25" "0" "F")
;;; (command "_.TEXT" "_MC" "0.25,27.625" "0.25" "0" "G")
;;; (command "_.TEXT" "_MC" "0.25,31.875" "0.25" "0" "H")
;;; ;;;
;;; (command "_.TEXT" "_MC" "43.75,2.125"  "0.25" "0" "A")
;;; (command "_.TEXT" "_MC" "43.75,6.375"  "0.25" "0" "B")
;;; (command "_.TEXT" "_MC" "43.75,10.625" "0.25" "0" "C")
;;; (command "_.TEXT" "_MC" "43.75,14.875" "0.25" "0" "D")
;;; (command "_.TEXT" "_MC" "43.75,19.125" "0.25" "0" "E")
;;; (command "_.TEXT" "_MC" "43.75,23.375" "0.25" "0" "F")
;;; (command "_.TEXT" "_MC" "43.75,27.625" "0.25" "0" "G")
;;; (command "_.TEXT" "_MC" "43.75,31.875" "0.25" "0" "H")
;;; ;;;
;;; (command "_.TEXT" "_MC" "2.75,0.5"  "0.25" "0" "8")
;;; (command "_.TEXT" "_MC" "8.25,0.5"  "0.25" "0" "7")
;;; (command "_.TEXT" "_MC" "13.75,0.5"  "0.25" "0" "6")
;;; (command "_.TEXT" "_MC" "19.25,0.5"  "0.25" "0" "5")
;;; (command "_.TEXT" "_MC" "24.75,0.5"  "0.25" "0" "4")
;;; (command "_.TEXT" "_MC" "30.25,0.5"  "0.25" "0" "3")
;;; (command "_.TEXT" "_MC" "35.75,0.5"  "0.25" "0" "2")
;;; (command "_.TEXT" "_MC" "41.25,0.5"  "0.25" "0" "1")
;;; ;;;
;;; (command "_.TEXT" "_MC" "2.75,33.5"  "0.25" "0" "8")
;;; (command "_.TEXT" "_MC" "8.25,33.5"  "0.25" "0" "7")
;;; (command "_.TEXT" "_MC" "13.75,33.5"  "0.25" "0" "6")
;;; (command "_.TEXT" "_MC" "19.25,33.5"  "0.25" "0" "5")
;;; (command "_.TEXT" "_MC" "24.75,33.5"  "0.25" "0" "4")
;;; (command "_.TEXT" "_MC" "30.25,33.5"  "0.25" "0" "3")
;;; (command "_.TEXT" "_MC" "35.75,33.5"  "0.25" "0" "2")
;;; (command "_.TEXT" "_MC" "41.25,33.5"  "0.25" "0" "1")
;;; END_ITEM
;;; 
;;; A sample Architectural 24 x 36 sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 inch.
;;; 
;;; NAME - Arch/Engineering (24 x 36)
;;; (command "_.ZOOM" "_W" "-0.5,-0.5" "36.5,24.5")
;;; (command "_.LINE" "0,0" "36,0" "36,24" "0,24" "_C")
;;; ;;; Outer border line
;;; (command "_.PLINE" ".5,.5" "_W" "0.1" "" "35.5,.5" "35.5,23.5" ".5,23.5" "_C")
;;; (command "_.FILLET" "_R" "1")
;;; (command "_.FILLET" "_P" "l")
;;; ;;;
;;; ;;; Title block
;;; ;;;
;;; ;;; Outer border line
;;; (command "_.PLINE" "31,1" "_W" "0.05" "" "35,1" "35,23" "31,23" "_C")
;;; (command "_.FILLET" "_R" ".5")
;;; (command "_.FILLET" "_P" "l")
;;; ;;;
;;; ;;; Sheet No. border line
;;; (command "_.PLINE" "31.25,1.25" "34.75,1.25" "34.75,2.75" "31.25,2.75" "_C")
;;; (command "_.FILLET" "_R" ".25")
;;; (command "_.FILLET" "_P" "l")
;;; ;;;
;;; ;;; Project border line
;;; (command "_.PLINE" "31.25,3" "34.75,3" "34.75,5" "31.25,5" "_C")
;;; (command "_.FILLET" "_P" "l")
;;; ;;;
;;; ;;; Firm border line
;;; (command "_.PLINE" "31.25,5.25" "34.75,5.25" "34.75,7.25" "31.25,7.25" "_C")
;;; (command "_.FILLET" "_P" "l")
;;; ;;;
;;; ;;; Notes/Revisions border line
;;; (command "_.PLINE" "31.25,7.5" "34.75,7.5" "34.75,22.75" "31.25,22.75" "_C")
;;; (command "_.FILLET" "_P" "l")
;;; ;;;
;;; ;;; Sheet No. lines
;;; (command "_.PLINE" "33.25,1.25" "_W" "0.025" "" "33.25,2.75" "")
;;; (command "_.PLINE" "31.25,2.25" "33.25,2.25" "")
;;; (command "_.PLINE" "31.25,1.75" "33.25,1.75" "")
;;; ;;;
;;; ;;; Notes/Revisions lines
;;; (command "_.PLINE" "31.75,7.5"   "31.75,8.625" "")
;;; (command "_.PLINE" "34.125,7.5"  "34.125,8.625" "")
;;; (command "_.PLINE" "31.25,7.875" "34.75,7.875" "")
;;; (command "_.PLINE" "31.25,8.25"  "34.75,8.25" "")
;;; (command "_.PLINE" "31.25,8.625" "34.75,8.625" "")
;;; ;;;
;;; (command "_.PLINE" "31.25,22.375" "34.75,22.375" "")
;;; ;;;
;;; ;;; Sheet text
;;; (command "_.STYLE" "ADESK1" "romans" "0" "1.0" "0" "" "" "")
;;; (command "_.TEXT" "31.4054,7.0711" "0.065" "0" "Firm Name and Address")
;;; (command "_.TEXT" "31.4054,4.8211" "0.065" "0" "Project Name and Address")
;;; (command "_.TEXT" "31.4054,2.5846" "0.065" "0" "Project")
;;; (command "_.TEXT" "33.3899,2.5846" "0.065" "0" "Sheet")
;;; (command "_.TEXT" "31.4054,2.0989" "0.065" "0" "Date")
;;; (command "_.TEXT" "31.4054,1.6132" "0.065" "0" "Scale")
;;; ;;;
;;; ;;; Revision bar text
;;; ;;;
;;; (command "_.TEXT" "_MC" "32.9983,22.5578" "0.1" "0" "General Notes")
;;; (command "_.TEXT" "_MC" "31.5136,7.7034" "0.1" "0" "No.")
;;; (command "_.TEXT" "_MC" "32.9983,7.7034" "0.1" "0" "Revision/Issue")
;;; (command "_.TEXT" "_MC" "34.4338,7.7034" "0.1" "0" "Date")
;;; ;;;
;;; END_ITEM
;;; 
;;; A generic 24 x 36 sheet with Title block and revision bar.
;;; All points are in paperspace units at a scale of 1 inch.
;;; 
;;; NAME - Generic D size Sheet (24 x 36in)
;;; (command "_.ZOOM" "_W" "-0.5,-0.5" "36.5,24.5")
;;; (command "_.PLINE" "0,2" "0,0" "2,0" "")
;;; (command "_.PLINE" "0,22" "0,24" "2,24" "")
;;; (command "_.PLINE" "34,24" "36,24" "36,22" "")
;;; (command "_.PLINE" "34,0" "36,0" "36,2" "")
;;; ;;; Outer border line
;;; (command "_.PLINE" "1.5,1.25" "33.75,1.25" "33.75,22.75" "1.5,22.75" "_C")
;;; ;;;
;;; ;;; Title block
;;; ;;;
;;; ;;; Outer border line
;;; (command "_.PLINE" "33.875,1.25" "35.5,1.25" "35.5,22.75" "33.875,22.75" "_C")
;;; ;;;
;;; (command "_.LINE" "33.875,2.5693" "35.5,2.5693" "")
;;; (command "_.LINE" "33.875,2.9443" "35.5,2.9443" "")
;;; (command "_.LINE" "33.875,3.3193" "35.5,3.3193" "")
;;; (command "_.LINE" "33.875,3.6943" "35.5,3.6943" "")
;;; (command "_.LINE" "33.875,4.0693" "35.5,4.0693" "")
;;; (command "_.LINE" "33.875,20.5" "35.5,20.5" "")
;;; (command "_.LINE" "33.875,20.75" "35.5,20.75" "")
;;; (command "_.LINE" "33.875,21" "35.5,21" "")
;;; (command "_.LINE" "33.875,21.25" "35.5,21.25" "")
;;; (command "_.LINE" "33.875,21.5" "35.5,21.5" "")
;;; (command "_.LINE" "33.875,21.75" "35.5,21.75" "")
;;; (command "_.LINE" "33.875,22" "35.5,22" "")
;;; (command "_.LINE" "33.875,22.375" "35.5,22.375" "")
;;; ;;;
;;; (command "_.LINE" "35.125,20.5" "35.125,22.375" "")
;;; ;;;
;;; END_ITEM
;;; END_DATA
;;; 
;;; MVIEWS
;;; None, 0, 0
;;; Single, 1, 1, PLAN
;;; Std. Engineering, 2, 2, FRONT, PLAN, RIGHT, ISO
;;; Array of Viewports
;;; END_MVIEWS
;;; 
;;; VPSETNAME  - Single
;;; (command ".mview" ll_crn ur_crn)
;;; END_ITEM
;;; VPSETNAME  - Std. Engineering
;;; END_ITEM
;;; VPSETNAME  - Array of Viewports
;;; END_ITEM
;;; 
;;; END_DATA
;;; 
(princ "  MVSETUP loaded.")
(princ)
