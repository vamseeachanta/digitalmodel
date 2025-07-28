; Next available MSG number is    22 
; MODULE_ID APPLOAD_LSP_
;;;----------------------------------------------------------------------------
;;;
;;;    APPLOAD.LSP   Version 0.5
;;;
;;;    Copyright 1991, 1992, 1993, 1994, 1996 by Autodesk, Inc.
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
;;;----------------------------------------------------------------------------
;;;   DESCRIPTION
;;;
;;;  An AutoLISP routine with a dialogue interface allowing users select
;;;  AutoLISP and ADS routines to load or unload.  Frequently used routines
;;;  can be saved to a file so that subsequent loads or unloads can be 
;;;  performed quickly and easily from a small list of favorites rather than 
;;;  scrolling through complete directory listings.
;;;  
;;;----------------------------------------------------------------------------
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
        (ai_abort "APPLOAD"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "APPLOAD" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "APPLOAD" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================
;;;----------------------------------------------------------------------------
;;; The main fuinction.
;;;----------------------------------------------------------------------------
(defun c:appload (/ 
                    a             fp_list1      pickf         strcase_list
                    add2lists     from          pickf1        the_list    
                    addfile       globals       pickf_list    ub          
                    appload_err   grey          pickf_no      unloadf     
                    cmd           is_one_ads    pos           updbox      
                    dcl_id        item          read_dfs      what        
                    lb            remfile       what_next     appload_main
                    er            loadf         remove        what_pos    
                    f             make_list     rs_err        yep         
                    filetype      no_load       s             filename
                    fname         no_unload     save_list   
                    fp_list       olderr        save_tog    
                  )
  ;;
  ;; Make a list of all highlighted files for loading or unloading.  Similar
  ;; code to remfile below.  Returns the list.
  ;;
  (defun make_list(/ pickf_no pickf_list fp_list1 n) 
    (setq pickf1 pickf)
    (while (setq pickf_no (read pickf1))
      (setq pickf_list (cons pickf_no pickf_list))
      (setq pickf1 (substr pickf1 (+ 2 (strlen (itoa pickf_no)))))
    )
    (setq n 0)
    (while (< n (length fp_list))
      (if (member n pickf_list)
        (progn 
          (setq fp_list1 (cons (nth n fp_list) fp_list1))
        )
      )
      (setq n (1+ n))
    )
    fp_list1
  )
  ;;
  ;; Load the files.
  ;;
  ;; Updated for Arx applications.
  ;;
  (defun loadf( / n)
    (setq no_load 0)
    (foreach n (setq er (make_list))
      (princ (strcat "\nLoading " n " ..."))
      (cond 
        ((= "lsp" (strcase (substr n (- (strlen n) 2)) T))
          (if (= (load n "invalid") "invalid")
			(princ(strcat "File " n " not found."))
		  )
        )
        ((or (member (strcase n) (arx)) (member (strcase n) (ads)))
          (princ (strcat "\nApplication " n " already loaded."))
        )
        (T 
          (if (and (= (arxload n "invalid") "invalid")
                   (= (xload n "invalid") "invalid")
              )
              (princ (strcat "\nFile " n " is invalid."))
              (princ (strcat "\nFile " n " loaded."))
          )
        )
      )
    )
  )
  ;;
  ;; Unload the files.
  ;;
  ;; Updated for Arx applications.
  ;;
  (defun unloadf(/ n pathname)
    (setq no_unload 0)
    (foreach pathname (make_list)
      (setq n (fnextract pathname))
      (princ (strcat "\nUnloading " n " ..."))
      (cond 
        ((= "lsp" (strcase (substr n (- (strlen n) 2)) T))
          (princ (strcat "\nInvalid file " n 
                         " - AutoLISP files cannot be unloaded."))
        )
        ((not (or (member (strcase n) (strcase_list (ads))) (member (strcase n) (strcase_list (arx))) ))
          (princ (strcat "\n Invalid file " n
                         " - Application not loaded."))
        )                           
        ;; Remove Arx apps first as they can appear in (ads).
        ((member (strcase n) (strcase_list (arx)))
          (arxunload n)
        )
        ((member (strcase n) (strcase_list (ads)))
          (xunload n)
        )
      )
    )
  )      

  ;;
  ;; Convert all entries in a list to uppercase
  ;;
  (defun strcase_list(inlist / entry outlist)
    (foreach entry inlist
      (setq outlist (cons (strcase entry) outlist))	      
    )
  )

  ;; extract file name out of complete path 
  (defun fnextract (path)
      (setq pos (strlen path))
	  (setq char " ")
	  (while (and (> pos 1)(/= char "\\"))
	     (setq char (substr path pos 1))
         (setq pos (- pos 1))
	  )
	  (if (> pos 1) (setq pos (+ pos 2)))
      (substr path pos)		
  )


  ;;
  ;; Check the list to find out whether the load and unload buttons should be
  ;; enabled or not.  Returns a list which consist of two numbers, l and u.
  ;; The buttons are enabled if the corresponding value is greater than 0.
  ;;
  ;; Updated for Arx applications.
  ;;
  (defun is_one_ads(/ yep n)
    (setq lb 0)
    (setq ub 0)
    (foreach n (make_list)
(setq globvar n)
      (if (/= ".lsp" (strcase (substr n (- (strlen n) 3)) T))
        (progn 
          (if (or (member (fnextract n) (ads)) (member (fnextract n) (arx)) )
            (setq ub (1+ ub))  ; enable unload button
            (setq lb (1+ lb))  ; enable load button
          )
        )
        (setq lb (1+ lb))
      )
    )
    (list lb ub)
  )
  ;;
  ;; Disable the Remove control if no items are highlighted.
  ;;
  (defun grey()
    (if (read (get_tile "fp_list"))
      (progn
        (mode_tile "remove_item" 0)
        (if (< 0 (car (is_one_ads)))
          (mode_tile "load" 0)
          (mode_tile "load" 1)
        )
        (if (< 0 (cadr (is_one_ads)))
          (mode_tile "unload" 0)
          (mode_tile "unload" 1)
        )
      )
      (progn
        ;; Set focus to the File... control so we don't disable a
        ;; a control that has focus.
        (mode_tile "add_to_list" 2)
        (mode_tile "remove_item" 1)
        (mode_tile "load" 1)
        (mode_tile "unload" 1)
      )
    )
  )
  ;;
  ;; Reset the error tile.
  ;;
  (defun rs_err()
    (set_tile "error" "")
  )
  ;;
  ;; Read appload.dfs for defaults.
  ;;
  (defun read_dfs()
    ;; Look for .dfs file in the standard places. 
    (if (not (setq filename (findfile "appload.dfs")))
      (setq filename "appload.dfs")
    )
    (if (setq f (open filename "r"))
      (progn 
        (while (setq a (read-line f))
          (setq fp_list (cons a fp_list))
        )
        (close f)
        (if (and fp_list (>= (getvar "maxsort") (length fp_list)))
          (setq fp_list (acad_strlsort fp_list))
        )
        (updbox)
      )
      (updbox)
    )
  )
  ;;
  ;; Save the current list to file.  Null lists are allowed.
  ;;
  (defun save_list()
    (if (= "1" save_tog)
      (progn 
        ;; Look for .dfs file in the standard places. 
        (if (not (setq filename (findfile "appload.dfs")))
          (setq filename "appload.dfs")
        )
        (if (setq f (open filename "w"))
          (progn
            (if fp_list
              (progn
                (foreach n fp_list
                  (write-line n f)
                )
              )
            )
            (close f)
          )
          (alert (strcat "Cannot Save List to current directory \n"
                         " - directory must have write permission.")
          )
        )
      )  
    )
  )
  ;;
  ;; Add a file to the list, using the File Dialog box
  ;;
  (defun addfile ()
    ;; make sure the directory variable exists
    (if (= last_appload_dir nil)
        (setq last_appload_dir "")
    )
    (setq fname (getfiled "Select AutoLISP, ADS, or ARX File" last_appload_dir filetype 2))
    (if fname
      (progn
        (setq last_appload_dir (car (fnsplitl fname)))
        (add2lists fname)
      )
    )
  )
  ;;
  ;; Add a file to the internal lists used for loading
  ;;
  (defun add2lists (fname)
    (if (not (member fname fp_list))
      (progn 
        (setq fp_list (append fp_list (list fname)))            
        (if (and fp_list (>= (getvar "maxsort") (length fp_list)))
          (setq fp_list (acad_strlsort fp_list))
        )
        (updbox)
        (set_tile "fp_list" (itoa (what_pos fname fp_list)))
        (setq pickf (get_tile "fp_list"))
        (grey)
      )
	  (progn
        (set_tile "fp_list" (itoa (what_pos fname fp_list)))
        (setq pickf (get_tile "fp_list"))
        (grey)
	  )
    )
  )
  ;;
  ;; Pass an item and a list and recieve a number showing it's position in  
  ;; the list, nil otherwise.  Item must be in the list, and the list must 
  ;; contain unique names. 0 if first item.                               
  ;;
  (defun what_pos (item the_list / pos)
    (setq pos (- (length the_list)
                 (length (member item the_list)))
    )          
  )
  ;;
  ;;  Remove the currently highlighted selections fp_list
  ;;
  (defun remfile (/ pickf_list pickf_no fp_list1)
    (while (setq pickf_no (read pickf))
      (setq pickf_list (cons pickf_no pickf_list))
      (setq pickf (substr pickf (+ 2 (strlen (itoa pickf_no)))))
    )
    (setq n 0)
    (while (< n (length fp_list))
      (if (not (member n pickf_list))
        (progn 
          (setq fp_list1 (cons (nth n fp_list) fp_list1))
        )
      )
      (setq n (1+ n))
    )
    (setq fp_list (reverse fp_list1))
    (updbox)
    (setq pickf "")
    (grey)
  )
  ;;
  ;;  Remove an item from the list. 
  ;;
  (defun remove (what from)
    (append (reverse (cdr (member what (reverse from))))
            (cdr (member what from))
    )
  )
  ;;
  ;; Build and display a list in the list_box
  ;;
  (defun updbox ()
    (start_list "fp_list")
    (mapcar 'add_list fp_list)
    (end_list)
  )

  ;;
  ;; Put up the dialogue.
  ;;
  ;; Updated for Arx apps.
  ;;
  (defun appload_main()

    (setq fp_list nil)
    ;; (acad_getfiled) only accpts two file extensions and so all
    ;; platforms must use "*" with the addition of a third filetype
    ;; (.arx).  If the (acad_getfiled) limit is increased from two,
    ;; we can revert to displaying only those files of interest on
    ;; a per platform basis.  In the meantime, that code is commented
    ;; out.
    (cond 
      ((= (getvar "platform") "386 DOS Extender")
       (setq filetype "lsp;exp;arx")
      )
      ;; Only check the initial letters as these extensions are
      ;; good for NT and Windows.
      ((= (substr (getvar "platform") 1 17) "Microsoft Windows")
       (setq filetype "lsp;exe;arx")
      )
      ;;
      ;; The following weird extension "::1" is an
      ;; illegal extension on the mac and is used to
      ;; signal the mac-specific file dialog code
      ;; to filter for the mac file _type_  'libr'.
      ;; I would pass in the 4 char filetype directly,
      ;; but we're trying to enforce a 3 char limit
      ;; in getfiled for cross-platform compatibility.
      ;;
      ((= (getvar "platform") "Apple Macintosh")
       (setq filetype "lsp;::1;arx")
      )
      (t 
         (setq filetype "*")
      )
    )

    (if (not (new_dialog "appload" dcl_id)) (exit))
    (read_dfs)
    (if fp_list 
      (progn 
        (grey)
      )
      (progn
        (mode_tile "remove_item" 1)
        (mode_tile "load" 1)
        (mode_tile "unload" 1)
      )
    )
    ;; If a default exists for the save list toggle, use it.  Else set the 
    ;; toggle to 1.
    (if (setq save_tog (cadr (assoc "appload" ai_defaults)))
      (set_tile "save_list" save_tog)
      (set_tile "save_list" (setq save_tog "1"))
    )
    (action_tile "fp_list"      "(rs_err)(setq pickf $value)(grey)" ) 
    (action_tile "add_to_list"  "(rs_err)(addfile)" )
    (action_tile "remove_item"  "(rs_err)(remfile)" )
    (action_tile "save_list"    "(rs_err)(setq save_tog $value)")
    (action_tile "load"         "(save_list)(done_dialog 2)")
    (action_tile "unload"       "(save_list)(done_dialog 3)")
    (action_tile "cancel"       "(save_list)(done_dialog 0)")
    (action_tile "help"         "(help \"\" \"appload\")")
    (setq what_next (start_dialog))
    (cond 
      ((= 2 what_next) (loadf))
      ((= 3 what_next) (unloadf))
    )
    (if (assoc "appload" ai_defaults)
      (setq ai_defaults (subst (list "appload" save_tog) 
                               (assoc "appload" ai_defaults)
                               ai_defaults
                        )
      )
      (setq ai_defaults (cons (list "appload" save_tog) ai_defaults))
    )
  )

  ;; Set up error function.
  (setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
  )

  (setvar "cmdecho" 0)

  (cond
     (  (not (ai_transd)))                       ; transparent OK
     (  (not (ai_acadapp)))                      ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "appload"))))  ; is .DCL file loaded?
     (t (appload_main))                          ; proceed!
  )

  (setq *error* old_error) 
  (setvar "cmdecho" old_cmd)

  (princ)
)

;;;----------------------------------------------------------------------------
(princ "  APPLOAD loaded.  ")
(princ)
