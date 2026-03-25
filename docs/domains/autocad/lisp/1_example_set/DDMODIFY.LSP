;;;----------------------------------------------------------------------------
;;;   DDMODIFY.LSP
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
;;;   Revision date: February 2, 1992
;;;
;;;----------------------------------------------------------------------------
;;;   DESCRIPTION
;;;
;;;   This function allows the user to get a listing comparable to the LIST 
;;;   command for most entities.  In addition, most entity fields in the 
;;;   dialogue box are editable.  Points can be specified dynamically by 
;;;   temporarily dismissing the dialogue box.  Each entity has a unique 
;;;   dialogue.
;;;
;;;   Naming conventions
;;;   Long function and widget names may use an underscore "_"
;;;   in their names to make them easier to read, long variable
;;;   names use a dash "-".
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
;;;
;;; ===========================================================================
;;; ===================== load-time error checking ============================

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
;;; and then try to load it.  If it can't be found or can't be
;;; loaded, then abort the loading of this file immediately.

  (cond
     (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.

     (  (not (findfile "ai_utils.lsp"))                     ; find it
        (ai_abort "DDMODIFY"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "DDMODIFY" "Can't load file AI_UTILS.LSP"))
  )

;;; If we get this far, then AI_UTILS.LSP is loaded and it can
;;; be assumed that all functions defined therein are available.

;;; Next, check to see if ACADAPP.EXP has been xloaded, and abort
;;; if the file can't be found or xloaded.  Note that AI_ACADAPP
;;; does not abort the running application itself (so that it can
;;; also be called from within the command without also stopping
;;; an AutoCAD command currently in progress).

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "DDMODIFY" nil)        ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================

;;; If we get this far, both ai_utils.lsp and acadapp.exp are
;;; assumed to be available.

;;; Define and encapsulate all subroutines that are declared
;;; locals of the (ddmodify) function.

(defun ddmodify_init ()
  ;;
  ;; These two functions modify the enitity list for common properties.  Since 
  ;; color, ltype, and thickness are absent from the entity list when they are 
  ;; set to their defaults (i.e. color = bylayer), a simple substitution using 
  ;; SUBST is not possible.
  ;;
  (defun modify_properties ()
    (emod ecolor 62)
    (emod eltype 6)
    (emod ethickness 39)
    (tempmod elayer 8 nil)
  )

  (defun emod (value bit)
    (if (= bit 62)
      (progn
        (if (= value "BYLAYER") (setq value 256))
        (if (= value "BYBLOCK") (setq value 0))
      )
    )
    (if (setq oldlist (cdr (assoc bit elist)))
      (tempmod value bit nil)
      (setq elist (append elist (list (cons bit value))))
    )
  )
  ;;
  ;; Resets entity list to original values.  Called when the dialogue or function 
  ;; is cancelled.
  ;;
  (defun reset ()
    (setq elist  old-elist
          ecolor (cdr (assoc 62 old-elist))
          eltype (cdr (assoc 6 old-elist))
          elayer (cdr (assoc 8 old-elist))
          ethickness (cdr (assoc 39 old-elist))
    )
    (if (not ecolor) (setq ecolor "BYLAYER"))
    (if (not eltype) (setq eltype "BYLAYER"))
    (if (not ethickness) (setq ethickness 0))
    (modify_properties)
    (setq reset_flag t)
    (entmod elist)
  )
  ;;
  ;; Modify entity when dialogue is temporarily dismissed to reflect latest 
  ;; settings of dialogue.  It converts the point from current UCS coordinates to 
  ;; the proper entity coordinates (world or entity).
  ;;
  ;; Arguments: value - in current UCS coordinates
  ;;            bit   - entity code (i.e. 10 for start point)
  ;;            ptype - point type  0=world 1=planar
  ;;
  (defun tempmod (value bit ptype / newpoint)
    (cond
      ((= ptype 1) (setq value (trans value 1 ename)))
      ((= ptype 0) (setq value (trans value 1 0)))
    )
    (setq elist (subst (cons bit value)
                       (assoc bit elist)
                       elist
                )
    )
  )
  ;;
  ;; The following functions are called after a dialogue has been temporarily 
  ;; dismissed and the user is selecting a point.  If a point is selected the 
  ;; entity list is modified and new X,Y,Z values set.  If no point is selected 
  ;; (null response), then the point is reset back to its previous values.
  ;;
  (defun ver_pt1 (ptype)
    (if pt1
      (progn
        (tempmod pt1 10 ptype)
        (entmod elist)
      )
      (setq pt1 (list x1 y1 z1))
    )
  )

  ; (move_pt1 <ptype> )
  ;
  ; Called in liew of (ver_pt1) to translate block insertions which
  ; might have variable attributes attached to them.  If the distance
  ; the block is to be moved is < 1e-6, the move is deferred.

  (defun move_pt1 (ptype / basept hi)
    (setq basept (trans (cdr (assoc 10 (entget ename))) ename 1))
    (cond
       (  (not pt1)
          (setq pt1 (list x1 y1 z1)))

       (  (> 1e-6 (distance pt1 basept)))

       (t (tempmod pt1 10 ptype)
          (setq hi (getvar "highlight"))
          (setvar "highlight" 0)
          (command "._move" ename "" basept pt1)
          (setvar "highlight" hi))
    )
  )

  (defun ver_pt2 (ptype)
    (if pt2
      (progn
        (tempmod pt2 11 ptype)
        (entmod elist)
      )
      (setq pt2 (list x2 y2 z2))
    )
  )

  (defun ver_pt3 (ptype)
    (if pt3
      (progn
        (tempmod pt3 12 ptype)
        (entmod elist)
      )
      (setq pt3 (list x3 y3 z3))
    )
  )

  (defun ver_pt4 (ptype)
    (if pt4
      (progn
        (tempmod pt4 13 ptype)
        (entmod elist)
      )
      (setq pt4 (list x4 y4 z4))
    )
  )
  ;;
  ;; Common properties for all entities
  ;;
  (defun set_tile_props ()
    (set_tile "error" "")
    (setcolor)
    (set_tile "t_ltype" (nth lt-idx ltnmlst))
    (set_tile "t_layer" (nth lay-idx laynmlst))
    (set_tile "eb_thickness" (rtos ethickness))
  )
  ;;
  ;; XYZ Point values for all enitites
  ;;
  (defun set_tile_pt1 (ptype)
    (if (= ptype 0)
      (setq pt1 (trans (cdr (assoc 10 elist)) 0 1))
      (setq pt1 (trans (cdr (assoc 10 elist)) ename 1))
    )
    (set_tile "x1_pt" (rtos (setq x1 (car pt1))))
    (set_tile "y1_pt" (rtos (setq y1 (cadr pt1))))
    (set_tile "z1_pt" (rtos (setq z1 (caddr pt1))))
  )
  (defun set_tile_pt2 (ptype)
    (if (= ptype 0)
      (setq pt2 (trans (cdr (assoc 11 elist)) 0 1))
      (setq pt2 (trans (cdr (assoc 11 elist)) ename 1))
    )
    (set_tile "x2_pt" (rtos (setq x2 (car pt2))))
    (set_tile "y2_pt" (rtos (setq y2 (cadr pt2))))
    (set_tile "z2_pt" (rtos (setq z2 (caddr pt2))))
  )
  (defun set_tile_pt3 (ptype)
    (if (= ptype 0)
      (setq pt3 (trans (cdr (assoc 12 elist)) 0 1))
      (setq pt3 (trans (cdr (assoc 12 elist)) ename 1))
    )
    (set_tile "x3_pt" (rtos (setq x3 (car pt3))))
    (set_tile "y3_pt" (rtos (setq y3 (cadr pt3))))
    (set_tile "z3_pt" (rtos (setq z3 (caddr pt3))))
  )
  (defun set_tile_pt4 (ptype)
    (if (= ptype 0)
      (setq pt4 (trans (cdr (assoc 13 elist)) 0 1))
      (setq pt4 (trans (cdr (assoc 13 elist)) ename 1))
    )
    (set_tile "x4_pt" (rtos (setq x4 (car pt4))))
    (set_tile "y4_pt" (rtos (setq y4 (cadr pt4))))
    (set_tile "z4_pt" (rtos (setq z4 (caddr pt4))))
  )
  ;; 
  ;; Handle for all entities
  ;;
  (defun set_tile_handle ()
    (if (setq hand (cdr (assoc 5 elist)))
      (set_tile "Handle" hand)
      (set_tile "Handle" "None")
    )
  )
  ;; 
  ;; Radius for ARC and CIRCLE
  ;;
  (defun set_tile_rad ()
    (setq radius (cdr (assoc 40 elist)))
    (set_tile "radius" (rtos radius))
  )
  ;;
  ;; Start angle for ARC
  ;;
  (defun set_tile_stang ()
    (setq st_ang (cdr (assoc 50 elist)))
    (set_tile "st_ang" (angtos st_ang))
  )
  ;;
  ;; End angle for ARC
  ;;
  (defun set_tile_endang ()
    (setq end_ang (cdr (assoc 51 elist)))
    (set_tile "end_ang" (angtos end_ang))
  ) 
  ;;
  ;; Width Factor - Text, Attributes, Block insertions and Shapes
  ;;
  (defun set_tile_rot ()
    (setq rot (cdr (assoc 50 elist)))
    (set_tile "rot" (angtos rot))
  )
  ;;
  ;; Width Factor - Text, Attributes and Shapes
  ;;
  (defun set_tile_hght ()
    (setq hght (cdr (assoc 40 elist)))
    (set_tile "hght" (rtos hght))
  )
  ;;
  ;; Width Factor - Text, Attributes and Shapes
  ;;
  (defun set_tile_wid ()
    (setq wid (cdr (assoc 41 elist)))
    (set_tile "wid" (rtos wid))
  )
  ;;
  ;; Obliquing Angle - Text, Attributes and Shapes
  ;;
  (defun set_tile_obl ()
    (setq obl (cdr (assoc 51 elist)))
    (set_tile "obl" (angtos obl))
  )
  ;;
  ;; Text string
  ;;
  (defun set_tile_text ()
    (setq text (cdr (assoc 1 elist)))
    (set_tile "t_string" text)
  )
  ;;
  ;; Attribute Tag
  ;;
  (defun set_tile_tag ()
    (if (= etype "ATTDEF")
      (progn
        (setq attag (cdr (assoc 2 elist)))
        (set_tile "tag" attag)
      )
    )
  )
  ;;
  ;; Attribute Definition
  ;;
  (defun set_tile_prompt ()
    (if (= etype "ATTDEF")
      (progn
        (setq atprompt (cdr (assoc 3 elist)))
        (set_tile "prompt" atprompt)
      )
    )
  )
  ;;
  ;; Justification setting for Attributes and Text.  Initializes
  ;; popup list box
  ;;
  (defun set_tile_just ()
    (setq ha (cdr (assoc 72 elist)))  ; horizontal alignment
    (setq va (cdr (assoc 73 elist)))  ; vertical alignment
    (setq jlist
          (list "Left"        "Center"        "Right"
                "Aligned"     "Middle"        "Fit"
                "Top left"    "Top center"    "Top right"
                "Middle left" "Middle center" "Middle right"
                "Bottom left" "Bottom center" "Bottom right"
          )
    )
    (start_list "popup_just")
    (mapcar 'add_list jlist)
    (end_list)
    (set_just_idx)
    (set_tile "popup_just" (jlist_act just-idx))
  )
  ;;
  ;; Style setting for Attributes and Text.  Reads symbol table for popup list 
  ;; box.
  ;;
  (defun set_tile_style (/ sname style-idx)
    (setq slist (list (cdr (assoc 2 (tblnext "STYLE" T)))))
    (while (setq sname (cdr (assoc 2 (tblnext "STYLE"))))
      (if (/= sname "") (setq slist (cons sname slist)))
    )
    (setq slist (acad_strlsort slist))  ; alphabetize style list
    (start_list "style")
    (mapcar 'add_list slist)
    (end_list)
    (setq tstyle (cdr (assoc 7 elist)))
    (setq style-idx (getindex tstyle slist))
    (set_tile "style" (itoa style-idx))
  )
  ;;
  ;; Text and Attribute setting - upside-down, backwards
  ;;
  (defun set_tile_bk-up ()
    (setq bk-up (cdr (assoc 71 elist)))
    (if (= (logand bk-up 2) 2)
      (set_tile "bkwd" (itoa (setq bkwd 1)))
      (set_tile "bkwd" (itoa (setq bkwd 0)))
    )
    (if (= (logand bk-up 4) 4)
      (set_tile "upsd" (itoa (setq upsd 1)))
      (set_tile "upsd" (itoa (setq upsd 0)))
    )
  )
  ;;
  ;; Attribute setting - invisible, constant, verify, preset
  ;; 
  (defun set_tile_icvp ()
    (if (not (setq icvp (cdr (assoc 70 elist))))
      (setq icvp 0)
    )
    (if (= (logand icvp 1) 1)
      (set_tile "inv" (itoa (setq inv 1)))
      (set_tile "inv" (itoa (setq inv 0)))
    )
    (if (= (logand icvp 2) 2)
      (set_tile "con" (itoa (setq con 1)))
      (set_tile "con" (itoa (setq con 0)))
    )
    (if (= (logand icvp 4) 4)
      (set_tile "ver" (itoa (setq ver 1)))
      (set_tile "ver" (itoa (setq ver 0)))
    )
    (if (= (logand icvp 8) 8)
      (set_tile "pre" (itoa (setq pre 1)))
      (set_tile "pre" (itoa (setq pre 0)))
    )
  )
  ;;
  ;; Scale factors for block insertions
  ;;
  (defun set_tile_scale ()
    (setq xscale (cdr (assoc 41 elist)))
    (set_tile "xscale" (rtos xscale))
    (setq yscale (cdr (assoc 42 elist)))
    (set_tile "yscale" (rtos yscale))
    (setq zscale (cdr (assoc 43 elist)))
    (set_tile "zscale" (rtos zscale))
  )
  ;;
  ;; Rows and columns for block insertions
  ;;
  (defun set_tile_rc ()
    (setq columns (cdr (assoc 70 elist)))
    (set_tile "columns" (itoa columns ))
    (setq rows (cdr (assoc 71 elist)))
    (set_tile "rows" (itoa rows))
    (setq col-sp (cdr (assoc 44 elist)))
    (set_tile "col_sp" (rtos col-sp))
    (setq row-sp (cdr (assoc 45 elist)))
    (set_tile "row_sp" (rtos row-sp))
  )
  ;;
  ;; Invisible edges for 3DFACE
  ;;
  (defun set_tile_edges ()
    (setq f-vis (cdr (assoc 70 elist)))
    (if (= (logand f-vis 1) 1)
      (set_tile "edge_1" (setq edge1 "0"))
      (set_tile "edge_1" (setq edge1 "1"))
    )
    (if (= (logand f-vis 2) 2)
      (set_tile "edge_2" (setq edge2 "0"))
      (set_tile "edge_2" (setq edge2 "1"))
    )
    (if (= (logand f-vis 4) 4)
      (set_tile "edge_3" (setq edge3 "0"))
      (set_tile "edge_3" (setq edge3 "1"))
    )
    (if (= (logand f-vis 8) 8)
      (set_tile "edge_4" (setq edge4 "0"))
      (set_tile "edge_4" (setq edge4 "1"))
    )
  )
  ;;
  ;; XYZ Point values for polyline vertex 
  ;;
  (defun set_tile_vpt (ptype)
    (if (= ptype 0)
      (setq vpt (trans (cdr (assoc 10 vlist)) 0 1))
      (setq vpt (trans (cdr (assoc 10 vlist)) ename 1))
    )
    (set_tile "xtext" (rtos (setq x1 (car vpt))))
    (set_tile "ytext" (rtos (setq y1 (cadr vpt))))
    (set_tile "ztext" (rtos (setq z1 (caddr vpt))))
  )
  ;;
  ;; Fit curve, fit spline, or smooth spline surface setting
  ;;
  (defun set_tile_fitsmooth ()
    (cond
      ((= (logand bit70 4) 4)
       (cond 
         ((= bit75 0)
           (set_tile "none" "1")
           (setq spltype 0)
         )
         ((= bit75 5)
           (set_tile "quad" "1")
           (setq spltype 5)
         )
         ((= bit75 6)
           (set_tile "cubic" "1")
           (setq spltype 6)
         )
         ((= bit75 8)
           (set_tile "bezier" "1")
           (setq spltype 8)
         )
       )
      )
      ((= (logand bit70 2) 2)
        (set_tile "fit" "1")
        (setq spltype 1)
      )
      (T (set_tile "none" "1"))
    )
  )
  ;;
  ;; Closed or Open mesh and polyline setting
  ;;
  (defun set_tile_closed ()
      (if (= pltype "3D mesh")
        (progn
          (if (= (logand bit70 32) 32)
            (set_tile "closedn" (setq closedn "1"))
            (set_tile "closedn" (setq closedn "0"))
          )
          (if (= (logand bit70 1) 1)
            (set_tile "closedm" (setq closed "1"))
            (set_tile "closedm" (setq closed "0"))
          )
          (setq old-closedm closedm old-closedn closedn)
        )
      )
      (if (or (= pltype "2D polyline")
              (= pltype "3D polyline")
          )
        (progn
          (if (= (logand bit70 1) 1)
            (set_tile "closed" (setq closed "1"))
            (set_tile "closed" (setq closed "0"))
          )
          (setq old-closed closed)
        )
      )
  )
  ;; Set common action tiles
  ;;
  ;; Defines action to be taken when pressing various widgets.  It is called 
  ;; for every entity dialogue.  Not all widgets exist for each entity dialogue,
  ;; but defining an action for a non-existent widget does no harm.
  (defun set_action_tiles ()
    (action_tile "cancel"       "(dismiss_dialog 0)")
    (action_tile "accept"       "(dismiss_dialog 1)")
    (action_tile "help"         "(acad_helpdlg \"acad.hlp\" help_entry)")
    (action_tile "b_color"      "(getcolor)")
    (action_tile "show_image"   "(getcolor)")
    (action_tile "b_name"       "(setq elayer (getlayer))")
    (action_tile "b_line"       "(setq eltype (getltype))")
    (action_tile "eb_thickness" "(getthickness $value)")

    (action_tile "pick_1"       "(dismiss_dialog 3)")
    (action_tile "pick_2"       "(dismiss_dialog 4)")
    (action_tile "pick_3"       "(dismiss_dialog 5)")
    (action_tile "pick_4"       "(dismiss_dialog 6)")
    (action_tile "x1_pt"        "(ver_x1 $value)")
    (action_tile "y1_pt"        "(ver_y1 $value)")
    (action_tile "z1_pt"        "(ver_z1 $value)")
    (action_tile "x2_pt"        "(ver_x2 $value)")
    (action_tile "y2_pt"        "(ver_y2 $value)")
    (action_tile "z2_pt"        "(ver_z2 $value)")
    (action_tile "x3_pt"        "(ver_x3 $value)")
    (action_tile "y3_pt"        "(ver_y3 $value)")
    (action_tile "z3_pt"        "(ver_z3 $value)")
    (action_tile "x4_pt"        "(ver_x4 $value)")
    (action_tile "y4_pt"        "(ver_y4 $value)")
    (action_tile "z4_pt"        "(ver_4 $value)")

    (action_tile "edge_1"       "(setq edge1 $value)")
    (action_tile "edge_2"       "(setq edge2 $value)")
    (action_tile "edge_3"       "(setq edge3 $value)")
    (action_tile "edge_4"       "(setq edge4 $value)")

    (action_tile "radius"       "(ver_rad $value)")
    (action_tile "st_ang"       "(ver_ang1 $value)")
    (action_tile "end_ang"      "(ver_ang2 $value)")

    (action_tile "xscale"       "(ver_xscl $value)")
    (action_tile "yscale"       "(ver_yscl $value)")
    (action_tile "zscale"       "(ver_zscl $value)")
    (action_tile "rot"          "(ver_rot $value)")
    (action_tile "columns"      "(ver_col $value)")
    (action_tile "rows"         "(ver_row $value)")
    (action_tile "col_sp"       "(ver_colsp $value)")
    (action_tile "row_sp"       "(ver_rowsp $value)")

    (action_tile "hght"         "(ver_hght $value)")
    (action_tile "wid"          "(ver_wid $value)")
    (action_tile "obl"          "(ver_obl $value)")
    (action_tile "style"        "(style_act $value)")

    (action_tile "t_string"     "(setq text $value)")
    (action_tile "tag"          "(ver_tag $value)")
    (action_tile "prompt"       "(setq atprompt $value)")
    (action_tile "bkwd"         "(setq bkwd (atoi $value))")
    (action_tile "upsd"         "(setq upsd (atoi $value))")
    (action_tile "inv"          "(setq inv (atoi $value))")
    (action_tile "con"          "(setq con (atoi $value))")
    (action_tile "ver"          "(setq ver (atoi $value))")
    (action_tile "pre"          "(setq pre (atoi $value))")
    (action_tile "popup_just"   "(jlist_act $value)")

    (action_tile "closed"       "(setq closed $value)")
    (action_tile "ltgen"        "(setq ltgen $value)")
    (action_tile "closedm"      "(setq closedm $value)")
    (action_tile "closedn"      "(setq closedn $value)")
    (action_tile "next_v"       "(next_vertex)")

    (action_tile "none"         "(if (radio_gaga \"none\")(set_uv 0))")
    (action_tile "fit"          "(if (radio_gaga \"fit\")(set_uv 1))")
    (action_tile "quad"         "(if (radio_gaga \"quad\")(set_uv 5))")
    (action_tile "cubic"        "(if (radio_gaga \"cubic\")(set_uv 6))")
    (action_tile "bezier"       "(if (radio_gaga \"bezier\")(set_uv 8))")

    (action_tile "u"            "(ver_u $value)")
    (action_tile "v"            "(ver_v $value)")
  )
  ;; As OW doesn't support disabling of individual radio buttons within 
  ;; clusters, a check must be performed as to the legitimacy of the 
  ;; button pushed and reset if necessary.
  (defun radio_gaga (pushed)
    (cond 
      ((and (= pltype "3D polyline")
            (or (= pushed "fit")
                (= pushed "bezier")
            )
       )
        (set_tile "none" "1")
        nil
      )
      ((and (= pltype "3D mesh")
            (= "fit" pushed)
       )
        (set_tile "none" "1")
        nil
      )           
      ((= pltype "Polyface mesh")
        (set_tile "none" "1")
        nil
      )
      ((and (= pltype "2D polyline")
            (= "bezier" pushed)
       )
        (set_tile "none" "1")
        nil
      )  
      (T)
    )
  )
  ;;
  (defun set_uv (type)
    (setq spltype type)
    (if (= pltype "3D mesh")
      (if (= spltype 0)
        (progn
          (set_tile "u" (itoa (setq u 0)))
          (set_tile "v" (itoa (setq v 0)))
        )
        (progn
          (if (= u 0)
            (set_tile "u" (itoa (setq u (getvar "surfu"))))
          )
          (if (= v 0)
            (set_tile "v" (itoa (setq v (getvar "surfv"))))
          )
        )
      )
    )
  )
  

  ;;
  ;; Verification functions
  ;;
  ;; Verify distance function.  This takes a new X, Y, or Z coordinate or 
  ;; distance value, the tile name, and the previous value as arguments.
  ;; If the distance is valid, it returns the distance and resets the tile.
  ;; Otherwise, it returns the previous value, sets the error tile and keeps
  ;; focus on the tile.  Shifting focus to the tile with invalid value can
  ;; trigger a callback from another tile whose value is valid.  In order
  ;; to keep the error message from being cleared by this secondary callback,
  ;; the variable errchk is set and checked.  The last-tile variable is set
  ;; and checked to ensure the error message is properly cleared when the
  ;; user corrects the value and hits return.
  ;;
  (defun verify_d (tile value old-value / coord valid errmsg)
    (setq valid nil errmsg "Invalid input value.")
    (if (setq coord (distof value))
      (progn
        (cond
          ((or (= tile "radius")
               (= tile "hght")
               (= tile "wid")
           )
           (if (> coord 0)
             (setq valid T)
             (setq errmsg "Value must be positive and nonzero.")
           )
          )
          ((or (= tile "xscale")
               (= tile "yscale")
               (= tile "zscale")
           )
           (if (/= coord 0)
             (setq valid T)
             (setq errmsg "Value must be nonzero.")
           )
          )
          (T (setq valid T))
        )
      )
      (setq valid nil)
    )
    (if valid
      (progn 
        (if (or (= errchk 0) (= tile last-tile))
          (set_tile "error" "")
        )
        (set_tile tile (rtos coord))
        (setq errchk 0)
        (setq last-tile tile)
        coord
      )
      (progn
        (mode_tile tile 2)
        (set_tile "error" errmsg)
        (setq errchk 1)
        (setq last-tile tile)
        old-value
      )
    )
  )
  ;;
  ;; Verify angle function.  This takes an angle and a tile name as arguments.
  ;; If the angle is valid, it returns the angle and resets the tile.  
  ;; Otherwise, it sets the error tile and keeps focus on the tile.
  ;;
  (defun verify_a (tile value old-value / ang)
    (if (setq ang (angtof value))
      (progn 
        (if (or (= errchk 0) (= tile last-tile))
          (set_tile "error" "")
        )
        (set_tile tile (angtos ang))
        (setq errchk 0)
        (setq last-tile tile)
        ang
      )
      (progn
        (mode_tile tile 2)
        (setq last-tile tile)
        (setq errchk 1)
        (set_tile "error" "Illegal input value.")
        old-value
      )
    )
  )
  ;;
  ;; Verify integer function.  This takes an integer and a tile name as 
  ;; arguments.  If the integer is valid, it returns the integer and resets the 
  ;; tile.  Otherwise, it sets the error tile and keeps focus on the tile.
  ;;
  (defun verify_i (tile value old-value / int valid errmsg)
    (setq valid nil)
    (setq errmsg "Value must be an integer.")
    (setq int (atoi value))
    (if (setq intchk (distof value))
      (cond
        ((or (= tile "columns") (= tile "rows"))
         (if (and (= int intchk)
                  (>= int 0)
                  (< int 32767)
             )
           (setq valid T)
           (setq errmsg "Value must be an integer between 1 and 32767.")
         )
        )
        ((and (or (= tile "u") (= tile "v")))
         (if (and (= int intchk)
                  (>= int 0)
                  (< int 201)
             )
           (setq valid T)
           (setq errmsg "Value must be an integer between 0 and 200.")
         )
        )
      )
    )
    (if valid
      (progn 
        (if (or (= errchk 0) (= tile last-tile))
            (set_tile "error" "")
        )
        (set_tile tile (itoa int))
        (setq errchk 0)
        (setq last-tile tile)
        int
      )
      (progn
        (mode_tile tile 2)
        (set_tile "error" errmsg)
        (setq errchk 1)
        (setq last-tile tile)
        old-value
      )
    )
  )
  ;;
  ;; Functions that verify tile values for integers
  ;;
  (defun ver_col (value)
    (setq columns (verify_i "columns" value columns))
  )
  (defun ver_row (value)
    (setq rows (verify_i "rows" value rows))
  )
  (defun ver_u (value)
    (setq u (verify_i "u" value u))
  )
  (defun ver_v (value)
    (setq v (verify_i "v" value v))
  )
  ;;
  ;; Functions that verify tile values for reals
  ;;
  (defun ver_x1 (value)
    (if (setq x1 (verify_d "x1_pt" value x1)) (calc))
  )
  (defun ver_y1 (value)
    (if (setq y1 (verify_d "y1_pt" value y1)) (calc))
  )
  (defun ver_z1 (value)
    (if (setq z1 (verify_d "z1_pt" value z1)) (calc))
  )
  (defun ver_x2 (value)
    (if (setq x2 (verify_d "x2_pt" value x2)) (calc))
  )
  (defun ver_y2 (value)
    (if (setq y2 (verify_d "y2_pt" value y2)) (calc))
  )
  (defun ver_z2 (value)
    (if (setq z2 (verify_d "z2_pt" value z2)) (calc))
  )
  (defun ver_x3 (value)
    (setq x3 (verify_d "x3_pt" value x3))
  )
  (defun ver_y3 (value)
    (setq y3 (verify_d "y3_pt" value y3))
  )
  (defun ver_z3 (value)
    (setq z3 (verify_d "z3_pt" value z3))
  )
  (defun ver_x4 (value)
    (setq x4 (verify_d "x4_pt" value x4))
  )
  (defun ver_y4 (value)
    (setq y4 (verify_d "y4_pt" value y4))
  )
  (defun ver_4 (value)
    (setq z4 (verify_d "z4_pt" value z4))
  )
  (defun ver_xscl (value)
    (setq xscale (verify_d "xscale" value xscale))
  )
  (defun ver_yscl (value)
    (setq yscale (verify_d "yscale" value yscale))
  )
  (defun ver_zscl (value)
    (setq zscale (verify_d "zscale" value zscale))
  )
  (defun ver_colsp (value)
    (setq col-sp (verify_d "col_sp" value col-sp))
  )
  (defun ver_rowsp (value)
    (setq row-sp (verify_d "row_sp" value row-sp))
  )
  (defun ver_rad (value)
    (if (setq radius (verify_d "radius" value radius))
      (calc)
    )
  )
  (defun ver_hght (value)
    (setq hght (verify_d "hght" value hght))
  )
  (defun ver_wid (value)
    (setq wid (verify_d "wid" value wid))
  )
  ;;
  ;; Functions that verify tile values for angles
  ;;
  (defun ver_ang1 (value)
    (if (setq st_ang (verify_a "st_ang" value st_ang))
      (calc)
    )
  )
  (defun ver_ang2 (value)
    (if (setq end_ang (verify_a "end_ang" value end_ang))
      (calc)
    )
  )
  (defun ver_rot (value)
    (setq rot (verify_a "rot" value rot))
  )
  (defun ver_obl (value)
    (setq obl (verify_a "obl" value obl))
  )
  ;;
  ;; Function that verifies attribute tag field for null string,
  ;; or a string that contains one or more spaces.  Tile value
  ;; is also converted to upper-case as well.
  ;;
  ;;
  (defun ver_tag (value / tval)
     (set_tile "error" "")
     (cond
        (  (or (eq "" (setq tval (strcase (ai_strtrim value))))
               (wcmatch tval "* *"))
           (set_tile "error" "Invalid attribute tag.")
           (mode_tile "tag" 2))
        (t (set_tile "error" "")
           (set_tile "tag" tval)
           (setq attag tval)))
  )
  ;;
  ;; Calculation functions
  ;;
  (defun calc ()
    (if (= etype "LINE") (line_calc))
    (if (= etype "ARC") (arc_calc))
    (if (= etype "CIRCLE") (cir_calc))
  )
  ;;
  ;; Calculation functions for lines, arcs, and circles
  ;;
  (defun line_calc ()
    (setq stpt  (list x1 y1 z1))
    (setq endpt (list x2 y2 z2))
    (set_tile "delta_x" (rtos (- x2 x1)))
    (set_tile "delta_y" (rtos (- y2 y1)))
    (set_tile "delta_z" (rtos (- z2 z1)))
    (set_tile "l_length" (rtos (distance stpt endpt)))
    (set_tile "l_angle" (angtos (angle stpt endpt)))
  )

  (defun cir_calc ()
        (setq radtest radius)
        (set_tile "Dia" (rtos (* 2 radius)))
        (set_tile "Circum" (rtos (* 2 pi radius)))
        (set_tile "Area" (rtos (* pi (* radius radius))))
  )

  (defun arc_calc ()
    (setq totang (- end_ang st_ang))
    (while (< totang 0)
      (setq totang (+ totang (* 2 pi)))
    )
    (while (> totang (* 2 pi))
      (setq totang (- totang (* 2 pi)))
    )
    (set_tile "tot_angle" (angtos totang))
    (setq arclen (* (* 2 pi radius) (/ totang (* 2 pi))))
    (set_tile "arclen" (rtos arclen))
  )
  ;;
  ;; Modify POINT
  ;;
  (defun modify_point ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 0)
    (entmod elist)
  )

  (defun ddpoint ()
    (if (not (new_dialog "ddpoint" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 0)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 3)
      (progn
        (modify_point)
        (setq pt1 (getpoint (list x1 y1 z1)  "\nPoint: "))
        (ver_pt1 0)
        (ddpoint)
      )
    )
    (if (= dialog-state 1)
      (modify_point)
    )
  )
  ;;
  ;; Modify LINE 
  ;;
  (defun modify_line ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (setq pt2 (list x2 y2 z2))
    (tempmod pt1 10 0)
    (tempmod pt2 11 0)
    (entmod elist)
  )
  (defun ddline ()
    (if (not (new_dialog "ddline" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 0)
    (set_tile_pt2 0)
    (line_calc)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 3)
      (progn
        (modify_line)
        (setq pt1 (getpoint (list x1 y1 z1)  "\nFrom pt: "))
        (ver_pt1 0)
        (ddline)
      )
    )
    (if (= dialog-state 4)
      (progn
        (modify_line)
        (setq pt2 (getpoint (list x2 y2 z2) "\nTo point: "))
        (ver_pt2 0)
        (ddline)
      )
    )
    (if (= dialog-state 1)
      (modify_line)
    )
  )
  ;; 
  ;; Modify CIRCLE
  ;;
  (defun modify_circle ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 1)
    (tempmod radius 40 nil)
    (entmod elist)
  )

  (defun ddcircle ()
    (if (not (new_dialog "ddcircle" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 1)
    (set_tile_rad)
    (cir_calc)
    ;; Define action for tiles
    (set_action_tiles)
    (set_tile_pt1 1)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_circle)
    )
    (if (= dialog-state 3)
      (progn
        (modify_circle)
        (setq pt1 (getpoint (list x1 y1 z1)  "\nCenter point: "))
        (ver_pt1 1)
        (ddcircle)
      )
    )
  ) 
  ;;
  ;; Modify ARC
  ;;
  (defun modify_arc ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 1)
    (tempmod radius 40 nil)
    (tempmod st_ang 50 nil)
    (tempmod end_ang 51 nil)
    (entmod elist)
  )
  (defun ddarc ()
    (if (not (new_dialog "ddarc" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 1)
    (set_tile_rad)
    (set_tile_stang)
    (set_tile_endang)
    (arc_calc)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_arc)
    )
    (if (= dialog-state 3)
      (progn
        (modify_arc)
        (setq pt1 (getpoint (list x1 y1 z1) "\nCenter point: "))
        (ver_pt1 1)
        (ddarc)
      )
    )
  )
  ;;
  ;; Modify SOLID or TRACE
  ;; Note the Z value of the entity is determined by the Z value of the fourth 
  ;; point - code 13.  Changing the point values of a solid or trace from a UCS 
  ;; that is nonplanar to the UCS the entity was created may confuse the user.
  (defun modify_solid ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (setq pt2 (list x2 y2 z2))
    (setq pt3 (list x3 y3 z3))
    (setq pt4 (list x4 y4 z4))
    (tempmod pt1 10 1)
    (tempmod pt2 11 1)
    (tempmod pt3 12 1)
    (tempmod pt4 13 1)
    (entmod elist)
  )

  (defun ddsolid ()
    (if (= etype "SOLID")
        (if (not (new_dialog "ddsolid" dcl_id)) (exit))
        (if (not (new_dialog "ddtrace" dcl_id)) (exit))
    )
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 1)
    (set_tile_pt2 1)
    (set_tile_pt3 1)
    (set_tile_pt4 1)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_solid)
    )
    (if (= dialog-state 3)
      (progn
        (modify_solid)
        (setq pt1 (getpoint (list x1 y1 z1) "\nFirst point: "))
        (ver_pt1 1)
        (ddsolid)
      )
    )
    (if (= dialog-state 4)
      (progn
        (modify_solid)
        (entmod elist)
        (setq pt2 (getpoint (list x2 y2 z2) "\nSecond point: "))
        (ver_pt2 1)
        (ddsolid)
      )
    )
    (if (= dialog-state 5)
      (progn
        (modify_solid)
        (setq pt3 (getpoint (list x3 y3 z3) "\nThird point: "))
        (ver_pt3 1)
        (ddsolid)
      )
    )
    (if (= dialog-state 6)
      (progn
        (modify_solid)
        (setq pt4 (getpoint (list x4 y4 z4) "\nFourth point: "))
        (ver_pt4 1)
        (ddsolid)
      )
    )
  )
  ;;
  ;; Modify 3DFACE
  ;;
  ;; Check visibility of edges
  ;;
  (defun edgetest (/ bit1 bit2 bit3 bit4)
    (if (= edge1 "1") (setq bit1 0) (setq bit1 1))
    (if (= edge2 "1") (setq bit2 0) (setq bit2 2))
    (if (= edge3 "1") (setq bit3 0) (setq bit3 4))
    (if (= edge4 "1") (setq bit4 0) (setq bit4 8))
    (+ bit1 bit2 bit3 bit4)
  )

  (defun modify_3dface ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (setq pt2 (list x2 y2 z2))
    (setq pt3 (list x3 y3 z3))
    (setq pt4 (list x4 y4 z4))
    (tempmod pt1 10 0)
    (tempmod pt2 11 0)
    (tempmod pt3 12 0)
    (tempmod pt4 13 0)
    (tempmod (edgetest) 70 nil)
    (entmod elist)
  )

  (defun dd3dface ()
    (if (not (new_dialog "dd3dface" dcl_id)) (exit))
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 0)
    (set_tile_pt2 0)
    (set_tile_pt3 0)
    (set_tile_pt4 0)
    (set_tile_edges)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_3dface)
    )
    (if (= dialog-state 3)
      (progn
        (modify_3dface)
        (setq pt1 (getpoint (list x1 y1 z1) "\nFirst pt: "))
        (ver_pt1 0)
        (dd3dface)
      )
    )
    (if (= dialog-state 4)
      (progn
        (modify_3dface)
        (setq pt2 (getpoint (list x2 y2 z2) "\nSecond point: "))
        (ver_pt2 0)
        (dd3dface)
      )
    )
    (if (= dialog-state 5)
      (progn
        (modify_3dface)
        (setq pt3 (getpoint (list x3 y3 z3) "\nThird point: "))
        (ver_pt3 0)
        (dd3dface)
      )
    )
    (if (= dialog-state 6)
      (progn
        (modify_3dface)
        (setq pt4 (getpoint (list x4 y4 z4) "\nFourth point: "))
        (ver_pt4 0)
        (dd3dface)
      )
    )
  )
  ;;
  ;; Modify BLOCK
  ;;
  (defun modify_block ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod xscale 41 nil)
    (tempmod yscale 42 nil)
    (tempmod zscale 43 nil)
    (tempmod col-sp 44 nil)
    (tempmod row-sp 45 nil)
    (tempmod rot 50 nil)
    (tempmod columns 70 nil)
    (tempmod rows 71 nil)
    (entmod elist)
    (move_pt1 1)
    (setq elist (entget ename))
  )

  (defun ddblock ()
    (setq blkname (cdr (assoc 2 elist)))
    (setq blklist (tblsearch "block" blkname))
    (setq blktype (cdr (assoc 70 blklist)))
    (if (= (logand blktype 4) 4)
      (progn
        (setq xrefpath (cdr (assoc 1 blklist)))
        (setq help_entry "ddmodify,Modify_External_Reference")
        (if (not (new_dialog "ddxref" dcl_id)) (exit))
        (set_tile "path" xrefpath)
      )
      (progn
        (if (not (new_dialog "ddblock" dcl_id)) (exit))
        (setq help_entry "ddmodify,Modify_Block_Insertion")
      )
    )
    (set_tile_handle)
    (set_tile_pt1 1)
    (set_tile_rot)
    (set_tile "Bl_name" blkname)
    (set_tile_scale)
    (set_tile_rc)
    (if (= (logand blktype 1) 1)
      (progn
        (set_tile "Bl_name" (strcat blkname " - Anonomous block"))
        (mode_tile "xscale" 1)
        (mode_tile "yscale" 1)
        (mode_tile "zscale" 1)
        (mode_tile "rot" 1)
        (mode_tile "columns" 1)
        (mode_tile "rows" 1)
        (mode_tile "col_sp" 1)
        (mode_tile "row_sp" 1)
      )
    )
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (cond
       (  (eq dialog-state 0)
          (setq pt1 (cdr (assoc 10 old-elist)))
          (move_pt1 1)
          (reset))
       (  (eq dialog-state 1)
          (modify_block))
       (  (eq dialog-state 3)
          (modify_block)
          (setq pt1 (getpoint (list x1 y1 z1)  "\nInsertion point: "))
          (move_pt1 1)
          (ddblock))
    )
  )
  ;;
  ;; Modify SHAPE
  ;;
  (defun modify_shape ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 1)
    (tempmod hght 40 nil)
    (tempmod wid 41 nil)
    (tempmod rot 50 nil)
    (tempmod obl 51 nil)
    (entmod elist)
  )

  (defun ddshape ()
    (if (not (new_dialog "ddshape" dcl_id)) (exit))
    (set_tile_props)                       
    (set_tile_handle)
    (set_tile_pt1 1)
    (set_tile_rot)
    (set_tile_hght)
    (set_tile_wid)
    (set_tile_obl)
    (set_tile "sh_name" (cdr (assoc 2 elist)))
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_shape)
    ) 
    (if (= dialog-state 3)
      (progn
        (modify_shape)
        (setq pt1 (getpoint (list x1 y1 z1)  "\nInsertion point: "))
        (ver_pt1 1)
        (ddshape)
      )
    )
  )
  ;;
  ;; Modify TEXT or ATTDEF
  ;;
  ;; Set bit code for upside-down and backwards setting
  ;;
  (defun code_71 ()
    (cond ((and (= bkwd "0") (= upsd "0")) 0)
          ((and (= bkwd "1") (= upsd "0")) 2)
          ((and (= bkwd "0") (= upsd "1")) 4)
          ((and (= bkwd "1") (= upsd "1")) 6)
    )
  )
  ;;
  ;; Style action.  Reset widget values to style defaults
  ;;
  (defun style_act (index / style-list)
    (setq style-idx (atoi index))
    (setq tstyle (nth style-idx slist))
    (setq style-idx (itoa style-idx))
    (set_tile "style" style-idx)
    (setq style-list (tblsearch "style" tstyle))
    (setq shght (cdr (assoc 40 style-list)))
    (if (/= shght 0)
      (progn
        (setq hght shght)
        (set_tile "hght" (rtos hght))
      )
    )
    (setq wid (cdr (assoc 41 style-list)))
    (set_tile "wid" (rtos wid))
    (setq obl (cdr (assoc 50 style-list)))
    (set_tile "obl" (angtos obl))
    (setq bk-up (cdr (assoc 71 style-list)))
    (if (= (logand bk-up 2) 2)
      (set_tile "bkwd" (itoa (setq bkwd 1)))
      (set_tile "bkwd" (itoa (setq bkwd 0)))
    )
    (if (= (logand bk-up 4) 4)
      (set_tile "upsd" (itoa (setq upsd 1)))
      (set_tile "upsd" (itoa (setq upsd 0)))
    )
  )
  ;;
  ;; Justification action.  Set vertical and horizontal alignment variables, 
  ;; grey out rotation and height if alignment = "aligned", grey out rotation 
  ;; if alignment = "fit".
  ;;
  (defun jlist_act (index / templist)
    (setq just-idx (atoi index))
    (cond 
      ((= just-idx 0) (setq va 0 ha 0))
      ((= just-idx 1) (setq va 0 ha 1))
      ((= just-idx 2) (setq va 0 ha 2))
      ((= just-idx 3) (setq va 0 ha 3))
      ((= just-idx 4) (setq va 0 ha 4))
      ((= just-idx 5) (setq va 0 ha 5))
      ((= just-idx 6) (setq va 3 ha 0))
      ((= just-idx 7) (setq va 3 ha 1))
      ((= just-idx 8) (setq va 3 ha 2))
      ((= just-idx 9) (setq va 2 ha 0))
      ((= just-idx 10) (setq va 2 ha 1))
      ((= just-idx 11) (setq va 2 ha 2))
      ((= just-idx 12) (setq va 1 ha 0))
      ((= just-idx 13) (setq va 1 ha 1))
      ((= just-idx 14) (setq va 1 ha 2))
    )
    (if (or (= ha 3) (= ha 5))  ; If Aligned or Fit text
      (mode_tile "rot" 1)
      (mode_tile "rot" 0)
    )
    (if (= ha 3)                ; If Aligned text
      (mode_tile "hght" 1)
      (mode_tile "hght" 0)
    )
    (setq just-idx (itoa just-idx))
  )
  ;;
  ;; Set intitial alignment setting based on vertical and horizontal alignment 
  ;; bit codes.
  ;;
  (defun set_just_idx ()
    (cond 
      ((= ha 0)             ; Horiz alignment = Left
        (cond 
          ((= va 0) (setq just-idx "0"))
          ((= va 1) (setq just-idx "12"))
          ((= va 2) (setq just-idx "9"))
          ((= va 3) (setq just-idx "6"))
        )
      )
      ((= ha 1)             ; Horiz alignment = Center
        (cond 
          ((= va 0) (setq just-idx "1"))
          ((= va 1) (setq just-idx "13"))
          ((= va 2) (setq just-idx "10"))
          ((= va 3) (setq just-idx "7"))
        )
      )
      ((= ha 2)             ; Horiz alignment = Right
        (cond 
          ((= va 0) (setq just-idx "2"))
          ((= va 1) (setq just-idx "14"))
          ((= va 2) (setq just-idx "11"))
          ((= va 3) (setq just-idx "8"))
        )
      )
      ((= ha 3) (setq just-idx "3"))   ; Aligned
      ((= ha 4) (setq just-idx "4"))   ; Middle
      ((= ha 5) (setq just-idx "5"))   ; Fit
      (T (setq just-idx "0"))
    )
    just-idx
  )

  (defun modify_text ()
    (if (or (and (= ha 0) (= va 0))
            (= ha 3)
            (= ha 5)
        )
      (progn
        (setq bit-10 (trans showpt 1 ename))
        (setq alipt (trans alipt 1 ename))
        (setq bit-11 (list
                       (car alipt)
                       (cadr alipt)
                       (caddr showpt)
                     )
        )      
      )
      (progn
        (setq bit-11 (trans showpt 1 ename))
        (setq bit-10 pt1)
      )
    )
    (modify_properties)
    (tempmod tstyle 7 nil)
    (tempmod bit-10 10 nil)
    (tempmod bit-11 11 nil)
    (tempmod text 1 nil)
    (tempmod hght 40 nil)
    (tempmod wid 41 nil)
    (tempmod rot 50 nil)
    (tempmod obl 51 nil)
    (setq bk-up (+ (* bkwd 2) (* upsd 4)))
    (tempmod bk-up 71 nil)
    (tempmod ha 72 nil)
    (tempmod va 73 nil)
    (if (= etype "ATTDEF")
      (progn
        (tempmod attag 2 nil)
        (tempmod atprompt 3 nil)
        (setq icvp (+ inv (* 2 con) (* 4 ver) (* 8 pre)))
        (tempmod icvp 70 nil)
      )
    )
    (entmod elist)
  )

  (defun ddtext (/ 2ndpt slist)
    (if (= etype "TEXT")
      (if (not (new_dialog "ddtext" dcl_id)) (exit))
      (if (not (new_dialog "ddattdef" dcl_id)) (exit))
    )
    (set_tile_props)
    (set_tile_handle)
    (set_tile_text)
    (set_tile_tag)
    (set_tile_prompt)
    (set_tile_hght)
    (set_tile_wid)
    (set_tile_rot)
    (set_tile_obl)
    (set_tile_bk-up)
    (set_tile_icvp)
    (set_tile_style)
    (set_tile_just)
    (setq pt1 (trans (cdr (assoc 10 elist)) ename 1))
    (setq pt2 (trans (cdr (assoc 11 elist)) ename 1))
    (if (or (and (= ha 0) (= va 0))
                 (= ha 3)
                 (= ha 5)
        )
      (setq showpt pt1 alipt pt2)
      (setq showpt pt2 alipt '(0.0 0.0 0.0))
    )

    (set_tile "x1_pt" (rtos (setq x1 (car showpt))))
    (set_tile "y1_pt" (rtos (setq y1 (cadr showpt))))
    (set_tile "z1_pt" (rtos (setq z1 (caddr showpt))))

    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_text)
    )
    (if (= dialog-state 3)
      (progn
        (modify_text)
        (if (or (= ha 3) (= ha 5))
          (progn
            (setq showpt (getpoint (list x1 y1 z1) "\nFirst point: "))
            (if (not showpt)
              (setq showpt (list x1 y1 z1))
            )
            (setq 2ndpt (getpoint showpt "\nSecond point: "))
            (if 2ndpt
              (progn
                (setq alipt 2ndpt)
                (tempmod showpt 10 1)
                (tempmod alipt 11 1)
                (entmod elist)
              )
            )
            (setq elist (entget ename))
          )
          (progn
            (setq showpt (getpoint (list x1 y1 z1) "\nInsertion point: "))
            (if showpt
              (progn
                (if (and (= ha 0) (= va 0))
                  (tempmod showpt 10 1)
                  (tempmod showpt 11 1)
                )
                (entmod elist)
              )
              (setq showpt (list x1 y1 z1))
            )
          )
        )
        (ddtext)
      )
    )
  )
  ;;  
  ;; Modify VIEWPORT
  ;;

  (defun ddvport ()
    (if (not (new_dialog "ddvport" dcl_id)) (exit))
    (set_tile_props)
    (set_tile_handle)
    (setq vpt (cdr (assoc 10 elist)))
    (set_tile "xtext" (rtos (setq x1 (car vpt))))
    (set_tile "ytext" (rtos (setq y1 (cadr vpt))))
    (set_tile "ztext" (rtos (setq z1 (caddr vpt))))
    (setq wid (cdr (assoc 40 elist)))
    (set_tile "wid" (rtos wid))
    (setq hght (cdr (assoc 41 elist)))
    (set_tile "hght" (rtos hght))
    (setq vpid (cdr (assoc 69 elist)))
    (set_tile "vpid" (itoa vpid))
    (setq on-off (cdr (assoc 68 elist)))
    (cond 
      ((= on-off 0) (set_tile "on-off" "OFF"))
      ((> on-off 0) (set_tile "on-off" "ON and Active"))
      (T (set_tile "on-off" "ON and Inactive"))
    )

    ;; Define action for tiles
    (set_action_tiles)

    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (progn
        (if (= ecolor 0) (setq ecolor "byblock"))
        (if (= ecolor 256) (setq ecolor "bylayer"))
        (command "_.chprop" ename ""
                 "_la" elayer
                 "_c" ecolor ""
        )
      )
    )
  )
  ;;
  ;; Modify POLYLINE
  ;;
  (defun modify_polyline ()
    (modify_properties)
    (if (= ltgen "1")
      (if (/= (logand bit70 128) 128)
        (setq bit70 (+ bit70 128))
      )
    )
    (if (= ltgen "0")
      (if (= (logand bit70 128) 128)
        (setq bit70 (- bit70 128))
      )
    )
    (setq elist (subst (cons 70 bit70) (assoc 70 elist) elist))
    (entmod elist)
  )

  ;; Increment vertex.  Set tile values to next vertex
  ;;
  (defun next_vertex ()
    (setq vname (entnext vname))
    (setq vlist (entget vname))
    (if (= (cdr (assoc 0 vlist)) "VERTEX")
      (progn
        (set_tile "ctr" (itoa (setq ctr (+ 1 ctr))))
        (set_tile_vpt pointype)
      )
      (progn
        (setq vname (entnext ename))
        (setq vlist (entget vname))
        (set_tile_vpt pointype)
        (set_tile "ctr" (itoa (setq ctr 1)))
      )
    )
  )

  (defun ddpline ()
    (if (not (new_dialog "ddpline" dcl_id)) (exit))
    (set_tile_props)
    (set_tile_handle)
    (setq bit70 (cdr (assoc 70 elist)))
    (setq bit75 (cdr (assoc 75 elist)))
    (cond 
      ((= (logand bit70 8) 8)   ; 3DPOLY
        (set_tile "ptype" (setq pltype"3D polyline"))
        (setq pointype 0)       ; WCS or ECS point values
        (mode_tile "fit" 1)
        (mode_tile "mesh" 1)
        (mode_tile "bezier" 1)
        (mode_tile "ltgen" 1)
        (set_tile "none" "1")
        (set_tile_closed)
        (set_tile_fitsmooth)
      )
      ((= (logand bit70 16) 16) ; 3DMESH
        (set_tile "ptype" (setq pltype"3D mesh"))
        (setq pointype 0)
        (mode_tile "pline" 1)
        (mode_tile "fit" 1)
        (mode_tile "ltgen" 1)
        (setq m (1- (cdr (assoc 71 elist))))
        (setq n (1-(cdr (assoc 72 elist))))
        (setq u (1- (cdr (assoc 73 elist))))
        (if (< u 0) (setq u 0))
        (setq v (1- (cdr (assoc 74 elist))))
        (if (< v 0) (setq v 0))
        (set_tile "m" (itoa m))
        (set_tile "n" (itoa n))
        (set_tile "u" (itoa u))
        (set_tile "v" (itoa v))
        (set_tile_closed)
        (set_tile_fitsmooth)
      )
      ((= (logand bit70 64) 64) ; POLYFACE MESH
        (set_tile "ptype" (setq pltype "Polyface mesh"))
        (setq pointype 0)
        (mode_tile "f-s" 1)
        (mode_tile "mesh" 1)
        (mode_tile "pline" 1)
      )
      (T                        ; 2D POLYLINE
        (set_tile "ptype" (setq pltype "2D polyline"))
        (setq pointype 1)
        (mode_tile "bezier" 1)
        (mode_tile "mesh" 1)
        (if (= (logand bit70 128) 128)
          (set_tile "ltgen" (setq ltgen "1"))
        )
        (set_tile_closed)
        (set_tile_fitsmooth)
      )
    )
    (if (not next) (setq vname (entnext ename)))
    (setq next T)
    (set_tile "ctr" (itoa (setq ctr 1)))
    (setq vlist (entget vname))
    (set_tile_vpt pointype)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))

    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (progn
        (modify_polyline)
        (if (or (= pltype "2D polyline")
                (= pltype "3D polyline")
            )
          (progn
            (command "_.pedit" ename)
            (if (= spltype 0) (command "_d"))
            (if (= spltype 1) (command "_f"))
            (if (or (= spltype 5)
                    (= spltype 6)
                    (= spltype 8)
                )
              (progn
                (setvar "splinetype" spltype)
                (command "_s")
              )
            )
            (if (= closed "0")
              (command "_o")
              (command "_c")
            )
            (command "")
          )
        )
        (if (= pltype "3D mesh")
          (progn
            (command "_.pedit" ename)
            (if (= spltype 0) (command "_d"))
            (if (or (= spltype 5)
                    (= spltype 6)
                    (= spltype 8)
                )
              (progn
                (setvar "splinetype" spltype)
                (setvar "surfu" u)
                (setvar "surfv" v)
                (command "_s")
              )
            )
            (if (/= closedm old-closedm)
              (command "_m")
            )
            (if (/= closedn old-closedn)
              (command "_n")
            )
            (command "")
          )
        )
      )
    )
  )
  ;;
  ;; Modify DIMENSION
  ;;
  (defun ddimen (/ dtypebit blkname bename sublist a)
    (if (not (new_dialog "ddimen" dcl_id)) (exit))
    (set_tile_props)
    (set_tile_handle)
    (set_tile "dstyle" (cdr (assoc 3 elist)))
    (setq dtypebit (cdr (assoc 70 elist)))

    ;; (logand) for bits, but these aren't really bits...
    (if (<= 128 dtypebit) (setq dtypebit (- dtypebit 128)))
    (cond
      ((= dtypebit 0) (set_tile "dtype" "Linear"))
      ((= dtypebit 1) (set_tile "dtype" "Linear"))
      ((= dtypebit 2) (set_tile "dtype" "Angular"))
      ((= dtypebit 3) (set_tile "dtype" "Diameter"))
      ((= dtypebit 4) (set_tile "dtype" "Radius"))
      ((= dtypebit 5) (set_tile "dtype" "Angular"))
      ((= dtypebit 6) (set_tile "dtype" "Y Ordinate"))
      ((= dtypebit 70)(set_tile "dtype" "X Ordinate"))
      (T (set_tile "dtype" " "))
    )
    (setq blkname (cdr (assoc 2 elist)))
    (setq blklist (tblsearch "block" blkname))
    (setq dimtext (cdr (assoc 1 elist)))
    (if (= " " dimtext) (setq dimtext "Suppressed"))
    (if (= "" dimtext) (setq dimtext "Default Text"))

    (if (> (strlen dimtext) 14)
      (setq dimtext (strcat (substr dimtext 1 14) "~"))
    )

    (set_tile "dtext" dimtext)

    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (progn
        (modify_properties)
        (entmod elist)
      )
    )
  )

  ;;
  ;; Sub-dialogues for properties.  Common to all entity dialogues
  ;;
  ;; This function pops a dialogue box consisting of a list box,image tile, and
  ;; edit box to allow the user to select or type a color number.  It returns 
  ;; the color number selected.
  (defun getcolor (/ old-idx colorno cname)
    (if (= (get_tile "error") "")
      (if (numberp (setq temp_color (acad_colordlg ecolor t)))
        (progn
          (setq ecolor temp_color)
          (setcolor)
        )
        (setq testcolor temp_color)
      )
    )
    ecolor
  )
  ;;
  ;; Function to set the color tiles. 
  (defun setcolor()
    (cond 
      ((= 0 ecolor)
        (set_tile "t_color" "BYBLOCK")
        (col_tile "show_image" 0 nil)
      )
      ((= 1 ecolor)
        (set_tile "t_color" "1 red")
        (col_tile "show_image" 1 nil)
      )
      ((= 2 ecolor)
        (set_tile "t_color" "2 yellow")
        (col_tile "show_image" 2 nil)
      )
      ((= 3 ecolor)
        (set_tile "t_color" "3 green")
        (col_tile "show_image" 3 nil)
      )
      ((= 4 ecolor)
        (set_tile "t_color" "4 cyan")
        (col_tile "show_image" 4 nil)
      )
      ((= 5 ecolor)
        (set_tile "t_color" "5 blue")
        (col_tile "show_image" 5 nil)
      )
      ((= 6 ecolor)
        (set_tile "t_color" "6 magenta")
        (col_tile "show_image" 6 nil)
      )
      ((= 7 ecolor)
        (set_tile "t_color" "7 white")
        (col_tile "show_image" 7 nil)
      )
      ((= 256 ecolor) 
        (set_tile "t_color" "BYLAYER")
        (col_tile "show_image" (bylayer_col) nil)
      )
      (T
        (set_tile "t_color" (itoa ecolor))
        (col_tile "show_image" ecolor nil)
      )
    )
  )
  ;;
  ;; This function pops a dialogue box consisting of a list box, image tile, and 
  ;; edit box to allow the user to select or  type a linetype.  It returns the 
  ;; linetype selected.
  ;;
  (defun getltype (/ old-idx ltname)
    (if (= (get_tile "error") "")
     (progn
      (if (not (new_dialog "setltype" dcl_id)) (exit))
      (start_list "list_lt")
      (mapcar 'add_list ltnmlst)  ; initialize list box
      (end_list)
      (setq old-idx lt-idx)
      (ltlist_act (itoa lt-idx))

      (action_tile "list_lt" "(ltlist_act $value)")
      (action_tile "edit_lt" "(ltedit_act $value)")
      (action_tile "accept" "(test_ok)")
      (action_tile "cancel" "(reset_lt)")

      (if (= (start_dialog) 1) ; User pressed OK
        (cond 
          ((= lt-idx 0)
            (set_tile "t_ltype" (bylayer_lt))
            "BYLAYER"
          )
          ((= lt-idx 1)
            (set_tile "t_ltype" "BYBLOCK")
            "BYBLOCK"
          )
          (T  (set_tile "t_ltype" ltname) ltname)
        )
        eltype
      )
     )
     eltype
    )
  )
  ;;
  ;; Edit box entries end up here
  (defun ltedit_act (ltvalue)
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
        (set_tile "error" "Invalid linetype.")
        (setq lt-idx old-idx)
        (mode_tile "edit_lt" 2)
      )
    )
  )
  ;;
  ;; List selections end up here.  Update the list box, edit box, and color 
  ;; tile.
  ;;
  (defun ltlist_act (index / dashdata)
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
  (defun reset_lt ()
    (setq lt-idx old-idx)
    (done_dialog 0)
  )
  ;;
  ;; This function pops a dialogue box consisting of a list box,image tile, and 
  ;; edit box to allow the user to select or type a layer name.  It returns the 
  ;; layer name selected.  It also has a button to find the status (On, Off, 
  ;; Frozen, etc.) of any layer selected.
  ;;
  (defun getlayer (/ old-idx layname on off frozth linetype)
    (if (= (get_tile "error") "")
     (progn
      (if (not (new_dialog "setlayer" dcl_id)) (exit))
      (set_tile "cur_layer" (getvar "clayer"))
      (start_list "list_lay")
      (mapcar 'add_list longlist)  ; initialize list box
      (end_list)
      (setq old-idx lay-idx)
      (laylist_act (itoa lay-idx))
      (action_tile "list_lay" "(laylist_act $value)")
      (action_tile "edit_lay" "(layedit_act $value)")
      (action_tile "accept" "(test_ok)")
      (action_tile "cancel" "(reset_lay)")
      (if (= (start_dialog) 1) ; User pressed OK
        (progn
          (set_tile "t_layer" layname)
          ;; If layer equals bylayer reset color tile
          (if (= ecolor 256)
            (col_tile "show_image" (bylayer_col) nil)
          )
          layname
        )
        elayer
      )
     )
     elayer
    )
  )
  ;;
  ;; Edit box selections end up here.  Convert layer entry to upper case.  If 
  ;; layer name is valid, clear error string, call (laylist_act) function,
  ;; and change focus to list box.  Else print error message.
  ;;
  (defun layedit_act (layvalue)
    (setq layvalue (strcase layvalue))
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
    )
  )
  ;;
  ;; List entry selections end up here.
  ;;
  (defun laylist_act (index / layinfo color dashdata)
    ;; Update the list box, edit box, and color tile.
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
  ;; Reset to original layer when cancel is selected.
  ;;
  (defun reset_lay ()
    (setq lay-idx old-idx)
    (done_dialog 0)
  )
  ;;
  ;; Checks validity of thickness from edit box.
  (defun getthickness (value)
    (setq ethickness (verify_d "eb_thickness" value ethickness))
  )
  ;;
  ;; This function makes a list called laynmlst which consists of all the layer
  ;; names in the drawing.  It also creates a list called longlist which 
  ;; consists of strings which contain the layer name, color, linetype, etc.  
  ;; Longlist is later mapped into the layer listbox.  Both are ordered the 
  ;; same.
  ;;
  (defun make_lay_lists (/ layname onoff frozth color linetype vpf vpn ss 
                           cvpname xdlist vpldata sortlist name templist
                           bit-70
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
        (setq frozth "F")
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
  ;; This function makes 2 list - ltnmlst & mdashlist.
  ;; Ltnmlst is a list of linetype names read from the symbol table.  Mdashlist 
  ;; is list consisting of lists which define the linetype pattern - numbers 
  ;; that indicate dots, dashes, and spaces taken from group code 49.  The list 
  ;; corresponds to the order of names in ltnmlst.
  ;;
  (defun make_lt_lists (/ ltlist ltname)
    (setq mdashlist nil)
    (setq ltlist (tblnext "LTYPE" T))
    (setq ltname (cdr (assoc 2 ltlist)))
    (setq ltnmlst (list ltname))
    (if (= ltname "CONTINUOUS")
      (setq mdashlist (list "CONT"))
      (setq mdashlist 
            (append mdashlist (list (add_mdash ltlist)))
      )
    )
    (while (setq ltlist (tblnext "LTYPE"))
      (setq ltname (cdr (assoc 2 ltlist)))
      (setq ltnmlst (append ltnmlst (list ltname)))
      (setq mdashlist 
            (append mdashlist (list (add_mdash ltlist)))
      )
    )
    (setq ltnmlst (cons "BYBLOCK" ltnmlst))
    (setq mdashlist  (cons nil mdashlist))
    (setq ltnmlst (cons "BYLAYER" ltnmlst))
    (setq mdashlist  (cons nil mdashlist))
  )
  ;;
  ;; Get all the group code 49 values for a linetype and put them in a list 
  ;; (pen-up, pen-down info).
  ;;
  (defun add_mdash (ltlist1 / dashlist assoclist dashsize)
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
      (progn (setq dash boxlength)
        (vi)
        (setq x1 boxlength)
      )
    )
    (while (< x1 boxlength)
      (if (setq dash (car patlist))
        (progn
          (setq dash (fix (* 30 dash)))
          (cond 
            ((= dash 0) (setq dash 1) (vi))
            ((> dash 0) (vi))
            (T 
              (if (< (abs dash) 2)
               (setq dash 2)
              )
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
  ;; ltype of the layer so it can be displayed  beside the linetype button.
  ;;
  (defun bylayer_lt (/ layname layinfo ltype)
    (if lay-idx
      (progn
        (setq layname (nth lay-idx laynmlst))
        (setq layinfo (tblsearch "layer" layname))
        (setq ltype (cdr (assoc 6 layinfo)))
        "BYLAYER"
      )
      "BYLAYER"
    )
  )
  ;;
  ;; This function is called if the color is set "BYLAYER".  It finds the color 
  ;; of the layer so it can be displayed beside the color button.
  ;;
  (defun bylayer_col (/ layname layinfo color)
    (if lay-idx
      (progn
        (setq layname (nth lay-idx laynmlst))
        (setq layinfo (tblsearch "layer" layname))
        (setq color (abs (cdr (assoc 62 layinfo))))
      )
      0
    )
  )
  ;;
  ;; Used to set the color name in layer subdialogue.
  ;;
  (defun colorname (colnum / cn)
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
  ;;
  ;; If their is no error message, then close the dialogue.
  ;;
  (defun dismiss_dialog (action)
    (if (= action 0)
      (done_dialog 0)
      (if (= (get_tile "error") "")
        (done_dialog action)
      )
    )
  )

  (defun test_ok ()
    (if (= (get_tile "error") "")
      (done_dialog 1)
    )
  )

  (defun cancel ()
    (done_dialog 0)
  )

;;; =======================================================================
;;; SETUP layer and linetype lists for application, and initialize all
;;; program variables.

  (make_lay_lists)                   ; layer list - laynmlst
  (make_lt_lists)                    ; linetype lists - ltnmlst, mdashlist
  (setq elist       (entget ename)
        old-elist   elist
        modlist     elist
        etype       (cdr (assoc 0 elist))
        extru       (cdr (assoc 210 elist))
        ecolor      (cdr (assoc 62 elist))
        elayer      (cdr (assoc 8 elist))
        ethickness  (cdr (assoc 39 elist))
        eltype      (cdr (assoc 6 elist))
  )
  (if (not ecolor) (setq ecolor 256))
  (if (not eltype) (setq eltype "BYLAYER"))
  (if (not ethickness) (setq ethickness 0))

  ;; Find index of linetype and layer lists

  (cond
     (  (= eltype "BYLAYER")
        (setq lt-idx (getindex "BYLAYER" ltnmlst)))
     (  (= eltype "BYBLOCK")
        (setq lt-idx (getindex "BYBLOCK" ltnmlst)))
     (t (setq lt-idx (getindex eltype ltnmlst)))
  )

  (setq lay-idx (getindex elayer laynmlst))
) ; end ddmodify_init

;;; --------------------------------------------------------------------------
;;; Function: DDMODIFY_SELECT
;;;
;;; Entity aquisition function.
;;;
;;; (ddmodify_select)
;;;
;;; Obtains entity to be modified, in one of three ways:
;;;
;;;   1 - Autoselected.
;;;   2 - Prompted for.
;;;   3 - Passed as an argument in a call to (ddmodify <ename> )
;;;
;;; The (ddmodify_select) function also sets the value of the
;;; global symbol AI_SELTYPE to one of the above three values to
;;; indicate the method thru which the entity was aquired.
;;;
;;; This value can be useful to applications that want to RESTORE
;;; an entity that was autoselected to its previous selected state
;;; when they terminate, although there doesn't appear to be any
;;; way to do this right now.

(defun ddmodify_select ()
   (cond
      (  ename                             ; (ddmodify) was called
         (cond                             ; with an <ename> argument
            (  (entget ename)              ;   If entity is non-deleted
               (setq ai_seltype 3)         ;   then return its ename.
               (ai_return ename))))


      (  (ai_aselect1 "\nSelect object to modify: ")) ; return autoselected
                                                      ; entity (if only one
                                                      ; entity is selected)
                                                      ; or prompt for entity
      (t (princ "\nNothing selected.")
         (ai_return nil))
   )
)

;;; ============= Command line interface function =======================

(defun C:DDMODIFY ()
   (ddmodify nil)
   (princ)
)

;;; ================== (ddmodify) - Main program ========================
;;;
;;; (ddmodify <ename> )
;;;
;;; Main program function, callable as a subroutine.
;;;
;;; <ename> = entity name of the object to modify.
;;;
;;; If <ename> is nil, then user is prompted to select
;;; the object interactively.
;;;
;;; Before (ddmodify) can be called as a subroutine, it must
;;; be loaded first.  It is up to the calling application to
;;; first determine this, and load it if necessary.


(defun ddmodify (ename /

       2ndpt            eltype              old-elist        totang
       add_mdash        emod                old-fit          tstyle
       alipt            endpt               old-idx          u
       ang              end_ang             old-spltype      upsd
       arclen           ethickness          old-u            v
       arc_calc         etype               old-v            va
       assoclist        extru               olderr           value
       atprompt         f-vis               oldlist          verify_a
       attag            fchk                on               verify_d
       attprompt        fit                 on-off           verify_i
       bit              frozth              onoff            ver_4
       bit-10           getcolor            patlist          ver_ang1
       bit-11           getindex            pattern          ver_ang2
       bit-70           getlayer            pltype           ver_col
       bit1             getltype            polytype         ver_colsp
       bit2             getthickness        pre              ver_hght
       bit3             get_color           proplist         ver_obl
       bit4             globals             pt               ver_pt1
       bit70            ha                  pt1              ver_pt2
       bit75            hght                pt2              ver_pt3
       bk-up            icvp                pt3              ver_pt4
       bkwd             index               pt4              ver_rad
       boxlength        inv                 ptype            ver_rot
       bylayer_col      item                radius           ver_row
       bylayer_lt       item1               reset            ver_rowsp
       calc             item2               reset_lay        ver_u
       cancel           itemlist            reset_lt         ver_v
       cir_calc         jlist               rot              ver_wid
       closed           jlist_act           row-sp           ver_x1
       closedm          just-idx            rows             ver_x2
       closedn          lay-idx             s                ver_x3
       cmd              layedit_act         set_action_tiles ver_x4
       cn               layinfo             set_just_idx     ver_xscl
       cname            laylist             set_tile_bk-up   ver_y1
       code_71          laylist_act         set_tile_edges   ver_y2
       col-idx          layname             set_tile_endang  ver_y3
       col-sp           laynmlst            set_tile_hght    ver_y4
       colname          layvalue            set_tile_icvp    ver_yscl
       colnmlst         linetype            set_tile_just    ver_z1
       colnolst         line_calc           set_tile_obl     ver_z2
       colnum           list1               set_tile_prompt  ver_z3
       color            longlist            set_tile_props   ver_zscl
       colorname        lt-idx              set_tile_pt1     vi
       colorno          ltabstr             set_tile_pt2     vlist
       columns          ltedit_act          set_tile_pt3     vname
       col_tile         ltidx               set_tile_pt4     vpf
       con              ltlist              set_tile_rad     vpid
       coord            ltlist1             set_tile_rc      vpldata
       ctr              ltlist_act          set_tile_rot     vpn
       cvpname          ltname              set_tile_scale   vpt
       dash             ltnmlst             set_tile_stang   wid
       dashdata         ltvalue             set_tile_style   x
       dashlist         ltype               set_tile_tag     x1
       dashsize         m                   set_tile_text    x2
       dcl_id           make_lay_lists      set_tile_vpt     x3
       dd3dface         make_lt_lists       set_tile_wid     x4
       ddarc            mdashlist           shght            xdlist
       ddblock          modify_3dface       showpt           xscale
       ddcircle         modify_arc          size             y
       ddline           modify_block        slist            y1
       ddlist           modify_circle       sname            y2
       ddmodify_err     modify_line                          y3
       ddpline          modify_point        sortlist         y4
       ddpoint          modify_polyline     spltype          yscale
       ddshape          modify_properties   ss               z1
       ddsolid          modify_shape        stpt             z2
       ddtext           modify_solid        style-idx        z3
       ddvport          modify_text         style-list       z4
       dialog-state     modify_vport        style_act        zscale
       modlist          st_ang              setcolor         reset_flag
       drawpattern      n                   temp             reset_uv
       echo             name                templist         ver_tag
       ecolor           newpoint            tempmod          move_pt1
       edge1            next                temp_color       undo_init
       edge2            next_vertex         test_ok          help_entry
       edge3            obl                 text
       edge4            off                 th-value
       edgetest         old-closed          tile
       elayer           old-closedm         tilemode
       elist            old-closedn         tile_rect
       ddimen           errchk              dismiss_dialog

  )

  (setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
  )

  (setvar "cmdecho" (cond (  (or (not *debug*) (zerop *debug*)) 0)
                          (t 1)))

  (cond
     (  (not (ai_notrans)))                      ; Not transparent?
     (  (not (ai_acadapp)))                      ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "ddmodify")))) ; is .DLG file loaded?
     (  (not (setq ename (ddmodify_select))))    ; entity to modify?

     (t (ai_undo_push)
        (ddmodify_init)                          ; everything okay, proceed.
        (cond
           ((= etype "ARC")       
             (setq help_entry "ddmodify,Modify_Arc")
             (ddarc)
           )
           ((= etype "ATTDEF")    
             (setq help_entry "ddmodify,Modify_Attribute_Definition")
             (ddtext)
           )
           ((= etype "CIRCLE")    
             (setq help_entry "ddmodify,Modify_Circle")
             (ddcircle)
           )
           ((= etype "INSERT")    ; see ddblock for help_entry
             (ddblock)
           )
           ((= etype "LINE")      
             (setq help_entry "ddmodify,Modify_Line")
             (ddline)
           )
           ((= etype "POINT")     
             (setq help_entry "ddmodify,Modify_Point")
             (ddpoint)
           )
           ((= etype "POLYLINE")  
             (setq help_entry "ddmodify,Modify_Polyline")

             ;; If a 2D pline, check to see if it is planar to the current
             ;; UCS, reject if not.   To see if the pline is parallel,
             ;; the 210 group (WCS) is added to the current UCS origin (WCS)
             ;; and then converted to the current UCS and checked to see if
             ;; it is equal to (0,0,1).
             (if (and (zerop (logand 120 (cdr (assoc 70 (entget ename)))))
                      (not (equal '(0.0 0.0 1.0) 
                                   (trans (mapcar '+ 
                                             (cdr (assoc 210 (entget ename)))
                                             (trans '(0.0 0.0 0.0) 1 0)
                                          ) 
                                     0 1
                                   )
                                   0.0000000001            ; fuzz
                           )
                      )
                 )
               (princ "\nThe 2D Polyline is not parallel to the current UCS.")
               (ddpline)
             )
           )

           ((= etype "SHAPE")     
             (setq help_entry "ddmodify,Modify_Shape")
             (ddshape)
           )
           ((= etype "SOLID")     
             (setq help_entry "ddmodify,Modify_Solid")
             (ddsolid)
           )
           ((= etype "TEXT")
             (setq help_entry "ddmodify,Modify_Text")
             (ddtext)
           )
           ((= etype "TRACE")     
             (setq help_entry "ddmodify,Modify_Trace")
             (ddsolid)
           )
           ((= etype "VIEWPORT")  
             (setq help_entry "ddmodify,Modify_Viewport")
             (ddvport)
           )
           ((= etype "3DFACE")    
             (setq help_entry "ddmodify,Modify_3D_Face")
             (dd3dface)
           )
           ((= etype "DIMENSION") 
             (setq help_entry "ddmodify,Modify_Dimension")
             (ddimen)
           )
           (t (princ (strcat "No dialogue support for entity type: "
                             etype "."
                     )
              )
           )
           (ai_undo_pop)
        )
     )
  )
  
  (setq *error* old_error) 
  (setvar "cmdecho" old_cmd)
  (if (not reset_flag)            ; if entity was modified, then
      (ai_return ename)           ; return it's ename to caller
  )
)


(princ "  DDMODIFY loaded.  ")
(princ)

