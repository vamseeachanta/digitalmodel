;;;----------------------------------------------------------------------------
;;;   DDSELECT.LSP  Version 0.5
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
        (ai_abort "DDSELECT"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "DDSELECT" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "DDSELECT" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================

;;;----------------------------------------------------------------------------
;;; The main routine.
;;;----------------------------------------------------------------------------
(defun c:ddselect( /
                   cmd             draw_size       set_default     x2         
                   dcl_id          ent_sort        set_sort        x_pickbox  
                   globals         sortents        y_pickbox       undo_init
                   sortents_init   ddselect_main
                   do_setvars      pickboxsize     x1              
                   pickfirst_init  pickadd_init    pickdrag_init    
                   pickauto_init   pickboxsize_init
                 )
  ;;
  ;; Pass an integer and draw a square of that size in the pickboxsize 
  ;; image tile.
  ;; 
  (defun draw_size (intsize)
    (setq x1 (- (/ x_pickbox 2) (1+ intsize) ))
    (setq x2 (+ (/ x_pickbox 2) (1+ intsize) ))
    (setq y1 (- (/ y_pickbox 2) (1+ intsize) ))
    (setq y2 (+ (/ y_pickbox 2) (1+ intsize) ))
    (start_image "pickbox_image")
    (fill_image 0 0 x_pickbox y_pickbox -2)  ; -2 is the graphics screen color
    (vector_image x1 y1 x2 y1 -1)            ; -1 is the cursor color
    (vector_image x2 y1 x2 y2 -1)
    (vector_image x2 y2 x1 y2 -1)
    (vector_image x1 y2 x1 y1 -1)
    (end_image)
  )
  ;;
  ;; Set the SORTENTS system variable to selected setting.
  ;;
  (defun set_sort()
    (setq sortents 0)             ; initialise to 0.
    (if (= "1" (get_tile "sort_obj_sel")) 
      (setq sortents (logior sortents 1))
    )
    (if (= "1" (get_tile "sort_obj_snap")) 
      (setq sortents (logior sortents 2))
    )
    (if (= "1" (get_tile "sort_redraws")) 
      (setq sortents (logior sortents 4))
    )
    (if (= "1" (get_tile "sort_slide")) 
      (setq sortents (logior sortents 8))
    )
    (if (= "1" (get_tile "sort_regens")) 
      (setq sortents (logior sortents 16))
    )
    (if (= "1" (get_tile "sort_plot")) 
      (setq sortents (logior sortents 32))
    )
    (if (= "1" (get_tile "sort_post")) 
      (setq sortents (logior sortents 64))
    )
    (setq sortents sortents)
  )
  ;;
  ;;  The entity sort dialogue.
  ;;
  (defun ent_sort()
    (if (not (new_dialog "sortents" dcl_id)) (exit))
    ;; Display current settings.
    (if (= 1 (logand 1 sortents)) (set_tile "sort_obj_sel" "1"))
    (if (= 2 (logand 2 sortents)) (set_tile "sort_obj_snap" "1"))
    (if (= 4 (logand 4 sortents)) (set_tile "sort_redraws" "1"))
    (if (= 8 (logand 8 sortents)) (set_tile "sort_slide" "1"))
    (if (= 16 (logand 16 sortents)) (set_tile "sort_regens" "1"))
    (if (= 32 (logand 32 sortents)) (set_tile "sort_plot" "1"))
    (if (= 64 (logand 64 sortents)) (set_tile "sort_post" "1"))
  
    (action_tile "accept" "(set_sort)(done_dialog)")
    (start_dialog)
  )
  ;;
  ;;  If OK, set all setvars to selected values.
  ;;
  (defun do_setvars()
    (if (/= pickfirst_init (get_tile "pickfirst"))
      (setvar "pickfirst" (atoi (get_tile "pickfirst")))
    )
    (if (/= pickadd_init (get_tile "pickadd"))
      (setvar "pickadd" (abs (- 1 (atoi (get_tile "pickadd")))))
    )
    (if (/= pickdrag_init (get_tile "pickdrag"))
      (setvar "pickdrag" (atoi (get_tile "pickdrag")))
    )
    (if (/= pickauto_init (get_tile "pickauto"))
      (setvar "pickauto" (atoi (get_tile "pickauto")))
    )
    (if (/= pickboxsize_init pickboxsize)
      (setvar "pickbox" (1+ pickboxsize))
    )
    ; Also set the sortents sysvar, if it has been modified.
    (if (/= sortents sortents_init) 
      (setvar "sortents" sortents)
    )
  )
  ;;
  ;;  Set all toggles to default mode.
  ;; 
  (defun set_default()
    (set_tile "pickfirst" "1")
    (set_tile "pickadd" "0")         ; label is titled opposite to the setvar.
    (set_tile "pickdrag" "0")
    (set_tile "pickauto" "1")
  )
  ;;
  ;; Put up the dialogue.
  ;;
  (defun ddselect_main()

    (if (not (new_dialog "ddselect" dcl_id)) (exit))

    ;; Get the value of the SORTENTS system variable.
    (setq sortents_init (getvar "sortents")
          sortents     sortents_init
    )

    ;; The pickbox variable must be within 1 - 20 for display within the image 
    ;; tile.  The pickbox variable will only be updated if the user moves
    ;; the slider bar (0 - 19).
    (setq pickboxsize (1- (getvar "pickbox")))
    (if (< 19 pickboxsize) (setq pickboxsize 19))
    (if (> 0 pickboxsize) (setq pickboxsize 0))
    (setq pickboxsize_init pickboxsize)

    ;; Get current settings of variables.
    (set_tile "pickfirst" (setq pickfirst_init (itoa (getvar "pickfirst"))))
    (set_tile "pickadd" 
              (setq pickadd_init (itoa (abs (- 1 (getvar "pickadd"))))))
    (set_tile "pickdrag" (setq pickdrag_init (itoa (getvar "pickdrag"))))
    (set_tile "pickauto" (setq pickauto_init (itoa (getvar "pickauto"))))

    (set_tile "pickbox_slider" (itoa pickboxsize))

    (setq x_pickbox (dimx_tile "pickbox_image"))
    (setq y_pickbox (dimy_tile "pickbox_image"))

    (draw_size pickboxsize)

    (action_tile "default_mode" "(set_default)")

    (action_tile "pickbox_slider" 
                 "(draw_size (setq pickboxsize (atoi $value)))")
    (action_tile "ent_sort" "(ent_sort)")
    (action_tile "help" "(acad_helpdlg \"acad.hlp\" \"ddselect\")")  
    (action_tile "accept" "(do_setvars)(done_dialog)")

    (start_dialog)
  )

  ;; Set up error function.
  (setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
  )

  (setvar "cmdecho" 0)

  (cond
     (  (not (ai_transd)))                        ; transparent OK
     (  (not (ai_acadapp)))                       ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "ddselect"))))  ; is .DCL file loaded?

     (t (if (and (/= 1 (logand 1 (getvar "cmdactive")))
                 (/= 8 (logand 8 (getvar "cmdactive")))
            )
         (ai_undo_push)
        )

        (ddselect_main)                          ; proceed!

        (if (and (/= 1 (logand 1 (getvar "cmdactive")))
                 (/= 8 (logand 8 (getvar "cmdactive")))
            )
         (ai_undo_pop)
        )        

     )
  )

  (setq *error* old_error) 
  (setvar "cmdecho" old_cmd)
  (princ)
)

;;;----------------------------------------------------------------------------
(princ "  DDSELECT loaded.")
(princ)


