;;
;;    aseupd.lsp
;;    
;;    Copyright (C) 1995-1997 by Autodesk, Inc.
;;    
;;    Permission to use, copy, modify, and distribute this software 
;;    for any purpose and without fee is hereby granted, provided
;;    that the above copyright notice appears in all copies and 
;;    that both that copyright notice and the limited warranty and 
;;    restricted rights notice below appear in all supporting 
;;    documentation.
;;    
;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.  
;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF 
;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC. 
;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE 
;;    UNINTERRUPTED OR ERROR FREE.
;;    
;;    Use, duplication, or disclosure by the U.S. Government is subject to 
;;    restrictions set forth in FAR 52.227-19 (Commercial Computer 
;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;    (Rights in Technical Data and Computer Software), as applicable. 
;;
;; CREATED BY: Volodya Sirotinin  Mar 20 1997
;;
;;-------------------------------------------------------------------------------------
;; The Link Path Update program
;; 
;; DESCRIPTION:
;;   
;;   This utility program allows to update the Table Path for 
;;   existing Link Path Name
;;   Open drawing with ASE links, load aseupd.lsp module, then perform
;;   ASEUPD command.
;;   
;;-------------------------------------------------------------------------------------

; GLOBAL VARIABLES

(setq ASEUPD-FILE "aseupd.dcl")              ; Name of dialog file
(setq ASEUPD-ID nil)                         ; Dialogue identifier
(setq aseupd-lpnlist nil)                    ; LPNs list
(setq aseupd-envlist nil)                    ; Environments list
(setq aseupd-catlist nil)                    ; Catalogs list
(setq aseupd-schlist nil)                    ; Schemas list
(setq aseupd-tablist nil)                    ; Tables list
(setq aseupd-dopath  nil)                    ; Selected database path

; DIAGNOSTIC MESSAGES:

(setq ASEUPD_MSG_ASE_NOT_LOADED "\nError during ASE loading"
      ASEUPD_MSG_NO_LPNS        "\nNo LPNs are registered"
      ASEUPD_MSG_LOADING_ASE    "\nASE is being loading..."
      ASEUPD_MSG_LOADING_DONE   " done."
      ASEUPD_MSG_SELECT_DCL     "Select aseupd.dcl file"
      ASEUPD_MSG_LPN_UPDATED    "Link Path is successfully updated"
)

;
; Gets the position of the key word in the list
;
(defun getpos (entkey entlist / index) 
  (progn
    (setq index 0)  
    (while (and (< index (length entlist)) (not (eq entkey (nth index entlist))))
      (setq index (1+ index))
    )
  )
  index  
)

;
; Initialize Tables list
;
(defun init-tab-list (tabname / pathlist schpath tabpath)
  (setq schpath (ase_dopathmake (ase_dopathname aseupd-dopath 1) 
                                 (if aseupd-catlist
                                     (if (ase_dopathname aseupd-dopath 2)
                                         (ase_dopathname aseupd-dopath 2)
                                         "")
                                      "*")
                                 (if aseupd-schlist
                                     (if (ase_dopathname aseupd-dopath 3)
                                         (ase_dopathname aseupd-dopath 3)
                                         "")
                                      "*")
  ))
  (if (and (>= (ase_dopathcode schpath) 3) (setq pathlist (ase_dolist schpath)))
    (progn
      (mode_tile "ID_POPUP_TAB" 0)
      ; Update the aseupd-tablist
      (setq aseupd-tablist nil)
      (foreach tabpath pathlist (setq aseupd-tablist 
                   (append aseupd-tablist (list (ase_dopathname tabpath 4)))))
      (start_list "ID_POPUP_TAB")
      (mapcar 'add_list aseupd-tablist)
      (end_list)
      (if (not tabname)
        (setq tabname (nth 0 aseupd-tablist))
      )
      (set_tile "ID_POPUP_TAB" (itoa (getpos tabname aseupd-tablist)))
    )
    (progn
      (setq aseupd-tablist nil)
      (start_list "ID_POPUP_TAB")
      (end_list)
      (mode_tile "ID_POPUP_TAB" 1)
    )
  )  
  (setq aseupd-dopath (ase_dopathmake (ase_dopathname aseupd-dopath 1) 
                                      (if (ase_dopathname aseupd-dopath 2) 
                                          (ase_dopathname aseupd-dopath 2)
                                          "")
                                      (if (ase_dopathname aseupd-dopath 3) 
                                          (ase_dopathname aseupd-dopath 3)
                                          "")
                                      tabname))    
  aseupd-tablist
)

;
; Initialize Schemas list
;
(defun init-sch-list (schname / pathlist catpath schpath)
  (setq catpath (ase_dopathmake 
                  (ase_dopathname aseupd-dopath 1) 
                    (if aseupd-catlist
                        (if (ase_dopathname aseupd-dopath 2)
                            (ase_dopathname aseupd-dopath 2)
                            "")
                        "*")))
  (if (and (>= (ase_dopathcode catpath) 2) (setq pathlist (ase_dolist catpath)))
    (progn
      (mode_tile "ID_POPUP_SCH" 0)
      ; Update the aseupd-schlist
      (setq aseupd-schlist nil)
      (foreach schpath pathlist (setq aseupd-schlist 
                         (append aseupd-schlist (list (ase_dopathname schpath 3)))))
      (setq aseupd-schlist (append (list "") aseupd-schlist))                                
      (start_list "ID_POPUP_SCH")
      (mapcar 'add_list aseupd-schlist)
      (end_list)
      (if (not schname)
        (setq schname (nth 1 aseupd-schlist))
      )
      (set_tile "ID_POPUP_SCH" (itoa (getpos schname aseupd-schlist)))
    )
    (progn
      (setq aseupd-schlist nil)
      (start_list "ID_POPUP_SCH")
      (end_list)
      (mode_tile "ID_POPUP_SCH" 1)
    )
  )  
  (if (not schname) 
      (setq schname (if aseupd_schlist
                        (nth 1 aseupd-schlist)
                        ""))
  )                      
  ; Correct DO path
  (if (not (ase_docmp schname 
                      (if (ase_dopathname aseupd-dopath 3) 
                          (ase_dopathname aseupd-dopath 3)
                          "")
                          3))
    (setq aseupd-dopath (ase_dopathmake (ase_dopathname aseupd-dopath 1) 
                                        (if (ase_dopathname aseupd-dopath 2) 
                                            (ase_dopathname aseupd-dopath 2)
                                            "")
                                        schname))    
  )
  (init-tab-list (ase_dopathname aseupd-dopath 4) )
  aseupd-schlist
)

;
; Initialize Catalogs list
;
(defun init-cat-list (catname / pathlist catpath)
  (if (setq pathlist (ase_dolist (ase_dopathname aseupd-dopath 1)))
    (progn
      ; Enable catlog popup
      (mode_tile "ID_POPUP_CAT" 0)
      ; Update the aseupd-catlist
      (setq aseupd-catlist nil)
      (foreach catpath pathlist (setq aseupd-catlist 
                        (append aseupd-catlist (list (ase_dopathname catpath 2)))))
      (setq aseupd-catlist (append (list "") aseupd-catlist))                                
      (start_list "ID_POPUP_CAT")
      (mapcar 'add_list aseupd-catlist)
      (end_list)
      (if (not catname)
        (setq catname (nth 1 aseupd-catlist))
      )
      (set_tile "ID_POPUP_CAT" (itoa (getpos catname aseupd-catlist)))
    )
    (progn
      (setq aseupd-catlist nil)
      (start_list "ID_POPUP_CAT")
      (end_list)
      (mode_tile "ID_POPUP_CAT" 1)
    )
  )  
  ; Correct the specified DO path
  (if (not catname) 
    (setq catname (if aseupd_catlist
                      (nth 1 aseupd-catlist)
                      ""))
  )
  ; Correct DO path
  (if (not (ase_docmp catname
                      (if (ase_dopathname aseupd-dopath 2) 
                          (ase_dopathname aseupd-dopath 2)
                          "")
                          2))
    (setq aseupd-dopath (ase_dopathmake (ase_dopathname aseupd-dopath 1) catname))
  )
  (init-sch-list (ase_dopathname aseupd-dopath 3) )
  aseupd-catlist
)

;
; Initialize Environments list
;
(defun init-env-list (envname)
  ; Correct DO path
  (if (not (ase_docmp envname (ase_dopathname aseupd-dopath 1) 1))
    (setq aseupd-dopath envname)
  )
  (if (and envname aseupd-envlist)
    (progn
      (mode_tile "ID_POPUP_ENV" 0)
      (start_list "ID_POPUP_ENV")
      (mapcar 'add_list aseupd-envlist)
      (end_list)
      (set_tile "ID_POPUP_ENV" (itoa (getpos envname aseupd-envlist)))
    )
    (progn
      (start_list "ID_POPUP_ENV")
      (end_list)
      (mode_tile "ID_POPUP_ENV" 1)
    )
  )  
  (init-cat-list (ase_dopathname aseupd-dopath 2))
  aseupd-envlist
)

;
; Initialize LPNs list
;
(defun init-lpn-list ( lpname )
  (if aseupd-lpnlist
    (progn
      (start_list "ID_POPUP_LPN")
      (mapcar 'add_list aseupd-lpnlist)
      (end_list)
      (set_tile "ID_POPUP_LPN" (itoa (getpos lpname aseupd-lpnlist)))
      ; Now set the Environment list
      (if lpname
        (progn                                
          (setq aseupd-dopath (ase_lppath lpname))
          (set_tile "ID_TEXT_LINKPATH" (if aseupd-dopath aseupd-dopath ""))
          (init-env-list (ase_dopathname (ase_lppath lpname) 1))
        )
        (setq aseupd-dopath nil)  
      )
    )
  )  
  aseupd-lpnlist
)

;
; Performs the Link Path update
;
(defun proceed-update ()
  ; Check the currently selected path
  (if (eq (ase_dopathcode aseupd-dopath) 4)
    (if (ase_lpupd aseupd-dopath 
                   (nth (atoi (get_tile "ID_POPUP_LPN")) aseupd-lpnlist))
      (progn             
        (set_tile "ID_TEXT_MSG" ASEUPD_MSG_LPN_UPDATED)
        (set_tile "ID_TEXT_LINKPATH" aseupd-dopath)
      )
      (set_tile "ID_TEXT_MSG" (ase_errmsg 0))
    )    
  )     
)

;
; Loads ASE module if it is neccessary
;
(defun try-load-ase ( / err )
  (if (not (setq err (member "ase.arx" (arx))))
    (progn
      (princ ASEUPD_MSG_LOADING_ASE)
      (setq err (arxload "ase" nil))
      (if (not err) 
        (princ ASEUPD_MSG_ASE_NOT_LOADED)
        (princ ASEUPD_MSG_LOADING_DONE)
      )  
    )
  )
  err
)

;;;-------------------------------------------------------------------------------------
;;
;;	dialogue functon
;;
;;;-------------------------------------------------------------------------------------
(defun aseupd-dialog ()
  (if (try-load-ase)
    (if (ase_lplist) 
      (progn
        (if (not (new_dialog "aseupd_dlg" ASEUPD-ID))	; Initialize the dialog
	      (exit)
	    )
        ; Make aseupd-lpnlist
        (setq aseupd-lpnlist (append (list "") (ase_lplist)))
        ; Make aseupd-envlist
        (setq aseupd-envlist (ase_dolist ""))
        (init-lpn-list (ase_dopathname (ase_docurrent) 5)) 
        
        (action_tile
          "ID_POPUP_LPN"
          "(init-lpn-list (nth (atoi (get_tile \"ID_POPUP_LPN\")) aseupd-lpnlist))"
        )
        
        (action_tile
          "ID_POPUP_ENV"
          "(init-env-list (nth (atoi (get_tile \"ID_POPUP_ENV\")) aseupd-envlist))"
        )
        (action_tile
          "ID_POPUP_CAT"
          "(init-cat-list (nth (atoi (get_tile \"ID_POPUP_CAT\")) aseupd-catlist))"
        )
        (action_tile
          "ID_POPUP_SCH"
          "(init-sch-list (nth (atoi (get_tile \"ID_POPUP_SCH\")) aseupd-schlist))"
        )
        (action_tile
          "ID_POPUP_TAB"
          "(init-tab-list (nth (atoi (get_tile \"ID_POPUP_TAB\")) aseupd-tablist))"
        )
        (action_tile
          "ID_POPUP_TAB"
          "(init-tab-list (nth (atoi (get_tile \"ID_POPUP_TAB\")) aseupd-tablist))"
        )
        (action_tile
          "ID_BUTTON_UPDATE"
          "(proceed-update)"
        )
                        
        (start_dialog)		; Display the dialog box
      )
      (princ ASEUPD_MSG_NO_LPNS)
    )  
    (princ ASEUPD_MSG_ASE_NOT_LOADED)
  )  
)

;;;-------------------------------------------------------------------------------------
;;
;;	load ASEUPD.LSP
;;
;;;-------------------------------------------------------------------------------------
(defun load-dcl ( / dlgfile)	
                                           ; Load .dcl file
  (setq ASEUPD-ID (load_dialog (setq dlgfile ASEUPD-FILE)))
  
  (if (= ASEUPD-ID -1)
    (progn
      (setq dlgfile 
        (getfiled ASEUPD_MSG_SELECT_DCL "" "dcl" 10))
      (if dlgfile
          (setq ASEUPD-ID (load_dialog dlgfile))
      )  
    )
    (setq ASEUPD-ID (load_dialog dlgfile))
  )
  dlgfile
)

(defun c:aseupd ()
  (if (load-dcl)
    (aseupd-dialog)
    (unload_dialog ASEUPD-ID)
  )  
  (princ)
)
