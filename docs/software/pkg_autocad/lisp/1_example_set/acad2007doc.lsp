; Next available MSG number is  104
; MODULE_ID ACAD2007doc_LSP_
;;;    ACAD2007DOC.LSP Version 1.0 for AutoCAD 2007
;;;
;;;    Copyright (C) 1994 - 2006 by Autodesk, Inc.
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
;;;
;;;    Note:
;;;            This file is loaded automatically by AutoCAD every time 
;;;            a drawing is opened.  It establishes an autoloader and
;;;            other utility functions.
;;;
;;;    Globalization Note:   
;;;            We do not support autoloading applications by the native 
;;;            language command call (e.g. with the leading underscore
;;;            mechanism.)


;;;===== Raster Image Support for Clipboard Paste Special =====
;;
;; IMAGEFILE
;;
;; Allow the IMAGE command to accept an image file name without
;; presenting the file dialog, even if filedia is on.
;; Example: (imagefile "c:/images/house.bmp")
;;
(defun imagefile (filename / filedia-save cmdecho-save)
  (setq filedia-save (getvar "FILEDIA"))
  (setq cmdecho-save (getvar "CMDECHO"))
  (setvar "FILEDIA" 0)
  (setvar "CMDECHO" 0)
  (command "_.-image" "_attach" filename)
  (setvar "FILEDIA" filedia-save)
  (setvar "CMDECHO" cmdecho-save)
  (princ)
)

;;;=== General Utility Functions ===

;   R12 compatibility - In R12 (acad_helpdlg) was an externally-defined 
;   ADS function.  Now it's a simple AutoLISP function that calls the 
;   built-in function (help).  It's only purpose is R12 compatibility.  
;   If you are calling it for anything else, you should almost certainly 
;   be calling (help) instead. 
 
(defun acad_helpdlg (helpfile topic)
  (help helpfile topic)
)


(defun *merr* (msg)
  (setq *error* m:err m:err nil)
  (princ)
)

(defun *merrmsg* (msg)
  (princ msg)
  (setq *error* m:err m:err nil)
  (princ)
)

;; Loads the indicated ARX app if it isn't already loaded
;; returns nil if no load was necessary, else returns the
;; app name if a load occurred.
(defun verify_arxapp_loaded (app) 
  (if (not (loadedp app (arx)))
      (arxload app f)
  )
)

;; determines if a given application is loaded...
;; general purpose: can ostensibly be used for appsets (arx) or (ads) or....
;;
;; app is the filename of the application to check (extension is required)
;; appset is a list of applications, (such as (arx) or (ads)
;; 
;; returns T or nil, depending on whether app is present in the appset
;; indicated.  Case is ignored in comparison, so "foo.arx" matches "FOO.ARX"
;; Also, if appset contains members that contain paths, app will right-match
;; against these members, so "bar.arx" matches "c:\\path\\bar.arx"; note that
;; "bar.arx" will *not* match "c:\\path\\foobar.arx."
(defun loadedp (app appset)
  (cond (appset  (or 
                     ;; exactly equal? (ignoring case)
                     (= (strcase (car appset))
                        (strcase app))
                     ;; right-matching? (ignoring case, but assuming that
                     ;; it's a complete filename (with a backslash before it)
					 (and 
					     (> (strlen (car appset)) (strlen app))
	                     (= (strcase (substr (car appset) 
	                                         (- (strlen (car appset)) 
	                                            (strlen app) 
	                                         ) 
	                                 )
	                        ) 
	                        (strcase (strcat "\\" app))
	                     )
				     )
                     ;; no match for this entry in appset, try next one....
                     (loadedp app (cdr appset)) )))
)


;;; ===== Single-line MText editor =====
(defun LispEd (contents / fname dcl state)
  (if (not (setq fname (getvar "program")))
     (setq fname "acad")
  )
  (strcat fname ".dcl")
  (setq dcl (load_dialog fname))
  (if (not (new_dialog "LispEd" dcl)) (exit))
  (set_tile "contents" contents)
  (mode_tile "contents" 2)
  (action_tile "contents" "(setq contents $value)")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "mtexted" "(done_dialog 2)" )
  (setq state (start_dialog))
  (unload_dialog dcl)
  (cond
    ((= state 1) contents)
    ((= state 2) -1)
    (t 0)
  )
)

;;; ===== Discontinued commands =====
(defun c:ddselect(/ cmdecho-save)
  (setq cmdecho-save (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "._+options" 8)
  (setvar "CMDECHO" cmdecho-save)
  (princ)
)

(defun c:ddgrips(/ cmdecho-save)
  (setq cmdecho-save (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "._+options" 8)
  (setvar "CMDECHO" cmdecho-save)
  (princ)
)

(defun c:gifin ()
  (alert "\nThe GIFIN command is no longer supported.\nUse the IMAGE command to attach raster image files.\n")
  (princ)
)

(defun c:pcxin ()
  (alert "\nThe PCXIN command is no longer supported.\nUse the IMAGE command to attach raster image files.\n")
  (princ)
)

(defun c:tiffin ()
  (alert "\nThe TIFFIN command is no longer supported.\nUse the IMAGE command to attach raster image files.\n")
  (princ)
)

(defun c:ddemodes()
  (alert "The Object Properties toolbar incorporates DDEMODES functionality.  \nDDEMODES has been discontinued.  \n\nFor more information, select \"Object Properties toolbar\" from the AutoCAD Help Index tab.")
  (princ)
)

(defun c:ddrmodes(/ cmdecho-save)
  (setq cmdecho-save (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "._+dsettings" 0)
  (setvar "CMDECHO" cmdecho-save)
  (princ)
)


;;; ===== AutoLoad =====

;;; Check list of loaded <apptype> applications ("ads" or "arx")
;;; for the name of a certain appplication <appname>.
;;; Returns T if <appname> is loaded.

(defun ai_AppLoaded (appname apptype)
   (apply 'or
      (mapcar 
        '(lambda (j)
	    (wcmatch
               (strcase j T)
               (strcase (strcat "*" appname "*") T)
            )   
         )
	 (eval (list (read apptype)))
      )
   )
)

;;  
;;  Native Rx commands cannot be called with the "C:" syntax.  They must 
;;  be called via (command).  Therefore they require their own autoload 
;;  command.

(defun autonativeload (app cmdliste / qapp)
  (setq qapp (strcat "\"" app "\""))
  (setq initstring "\nInitializing...")
  (mapcar
   '(lambda (cmd / nom_cmd native_cmd)
      (progn
        (setq nom_cmd (strcat "C:" cmd))
        (setq native_cmd (strcat "\"_" cmd "\""))
        (if (not (eval (read nom_cmd)))
            (eval
             (read (strcat
                    "(defun " nom_cmd "()"
                    "(setq m:err *error* *error* *merrmsg*)"
                    "(if (ai_ffile " qapp ")"
                    "(progn (princ initstring)"
                    "(_autoarxload " qapp ") (command " native_cmd "))"
                    "(ai_nofile " qapp "))"
                    "(setq *error* m:err m:err nil))"
                    ))))))
   cmdliste)
  nil
)

(defun _autoqload (quoi app cmdliste / qapp symnam)
  (setq qapp (strcat "\"" app "\""))
  (setq initstring "\nInitializing...")
  (mapcar
   '(lambda (cmd / nom_cmd)
      (progn
        (setq nom_cmd (strcat "C:" cmd))
        (if (not (eval (read nom_cmd)))
            (eval
             (read (strcat
                    "(defun " nom_cmd "( / rtn)"
                    "(setq m:err *error* *error* *merrmsg*)"
                    "(if (ai_ffile " qapp ")"
                    "(progn (princ initstring)"
                    "(_auto" quoi "load " qapp ") (setq rtn (" nom_cmd ")))"
                    "(ai_nofile " qapp "))"
                    "(setq *error* m:err m:err nil)"
                    "rtn)"
                    ))))))
   cmdliste)
  nil
)

(defun autoload (app cmdliste)
  (_autoqload "" app cmdliste)
)

(defun autoarxload (app cmdliste)
  (_autoqload "arx" app cmdliste)
)

(defun autoarxacedload (app cmdliste / qapp symnam)
  (setq qapp (strcat "\"" app "\""))
  (setq initstring "\nInitializing...")
  (mapcar
   '(lambda (cmd / nom_cmd)
      (progn
        (setq nom_cmd (strcat "C:" cmd))
        (if (not (eval (read nom_cmd)))
            (eval
             (read (strcat
                    "(defun " nom_cmd "( / oldcmdecho)"
                    "(setq m:err *error* *error* *merrmsg*)"
                    "(if (ai_ffile " qapp ")"
                    "(progn (princ initstring)"
                    "(_autoarxload " qapp ")"
                    "(setq oldcmdecho (getvar \"CMDECHO\"))"
                    "(setvar \"CMDECHO\" 0)"
                    "(command " "\"_" cmd "\"" ")"
                    "(setvar \"CMDECHO\" oldcmdecho))"
                    "(ai_nofile " qapp "))"
                    "(setq *error* m:err m:err nil)"
                    "(princ))"
                    ))))))
   cmdliste)
  nil
)

(defun _autoload (app)
; (princ "Auto:(load ") (princ app) (princ ")") (terpri)
  (load app)
)

(defun _autoarxload (app)
; (princ "Auto:(arxload ") (princ app) (princ ")") (terpri)
  (arxload app)
)

(defun ai_ffile (app)
  (or (findfile (strcat app ".lsp"))
      (findfile (strcat app ".exp"))
      (findfile (strcat app ".exe"))
      (findfile (strcat app ".arx"))
      (findfile app)
  )
)

(defun ai_nofile (filename)
  (princ
    (strcat "\nThe file "
            filename
            "(.lsp/.exe/.arx) was not found in your search path folders."
    )
  )
  (princ "\nCheck the installation of the support files and try again.")
  (princ)
)


;;;===== AutoLoad LISP Applications =====
;  Set help for those apps with a command line interface
(setfunhelp "c:gotourl" "" "gotourl")

(autoload "edge"  '("edge"))
(setfunhelp "C:edge" "" "edge")

(autoload "3d" '("3d" "3d" "ai_box" "ai_pyramid" "ai_wedge" "ai_dome"
                 "ai_mesh" "ai_sphere" "ai_cone" "ai_torus" "ai_dish")
)
(setfunhelp "C:3d" "" "3d")
(setfunhelp "C:ai_box" "" "3d_box")
(setfunhelp "C:ai_pyramid" "" "3d_pyramid")
(setfunhelp "C:ai__wedge" "" "3d_wedge")
(setfunhelp "C:ai_dome" "" "3d_dome")
(setfunhelp "C:ai_mesh" "" "3d_mesh")
(setfunhelp "C:ai_sphere" "" "3d_sphere")
(setfunhelp "C:ai_cone" "" "3d_cone")
(setfunhelp "C:ai_torus" "" "3d_torus")
(setfunhelp "C:ai_dish" "" "3d_dish")

(autoload "3darray" '("3darray"))
(setfunhelp "C:3darray" "" "3darray")

(autoload "mvsetup" '("mvsetup"))
(setfunhelp "C:mvsetup" "" "mvsetup")

(autoload "attredef" '("attredef"))
(setfunhelp "C:attredef" "" "attredef")

(autoload "tutorial" '("tutdemo" "tutclear"
				       "tutdemo" 
				       "tutclear"))

;;;===== AutoArxLoad Arx Applications =====


;;; ===== Double byte character handling functions =====

(defun is_lead_byte(code)
    (setq asia_cd (getvar "dwgcodepage"))
    (cond
        ( (or (= asia_cd "dos932")
              (= asia_cd "ANSI_932")
          )
          (or (and (<= 129 code) (<= code 159))
              (and (<= 224 code) (<= code 252))
          )
        )
        ( (or (= asia_cd "big5")
              (= asia_cd "ANSI_950")
          )
          (and (<= 129 code) (<= code 254))
        )
        ( (or (= asia_cd "gb2312")
              (= asia_cd "ANSI_936")
          )
          (and (<= 161 code) (<= code 254))
        )
        ( (or (= asia_cd "johab")
              (= asia_cd "ANSI_1361")
          )
          (and (<= 132 code) (<= code 211))
        )
        ( (or (= asia_cd "ksc5601")
              (= asia_cd "ANSI_949")
          )
          (and (<= 129 code) (<= code 254))
        )
    )
)

;;; ====================================================


;;;
;;;  FITSTR2LEN
;;;
;;;  Truncates the given string to the given length. 
;;;  This function should be used to fit symbol table names, that
;;;  may turn into \U+ sequences into a given size to be displayed
;;;  inside a dialog box.
;;;
;;;  Ex: the following string: 
;;;
;;;      "This is a long string that will not fit into a 32 character static text box."
;;;
;;;      would display as a 32 character long string as follows:
;;;
;;;      "This is a long...tatic text box."
;;;

(defun fitstr2len (str1 maxlen)

    ;;; initialize internals
    (setq tmpstr str1)
    (setq len (strlen tmpstr))

    (if (> len maxlen) 
         (progn
            (setq maxlen2 (/ maxlen 2))
            (if (> maxlen (* maxlen2 2))
                 (setq maxlen2 (- maxlen2 1))
            )
            (if (is_lead_byte (substr tmpstr (- maxlen2 2) 1))
                 (setq tmpstr1 (substr tmpstr 1 (- maxlen2 3)))
                 (setq tmpstr1 (substr tmpstr 1 (- maxlen2 2)))
            )
            (if (is_lead_byte (substr tmpstr (- len (- maxlen2 1)) 1))
                 (setq tmpstr2 (substr tmpstr (- len (- maxlen2 3))))
                 (setq tmpstr2 (substr tmpstr (- len (- maxlen2 2))))
            )
            (setq str2 (strcat tmpstr1 "..." tmpstr2))
         ) ;;; progn
         (setq str2 (strcat tmpstr))
    ) ;;; if
) ;;; defun


;;;
;;;  If the first object in a selection set has an attached URL
;;;  Then launch browser and point to the URL.
;;;  Called by the Grips Cursor Menu
;;;
(defun C:gotourl ( / ssurl url i)
   (setq m:err *error* *error* *merrmsg* i 0)

; if some objects are not already pickfirst selected, 
; then allow objects to be selected

  (if (not (setq ssurl (ssget "_I")))
      (setq ssurl (ssget))
  )

; if geturl LISP command not found then load arx application

  (if (/= (type geturl) 'EXRXSUBR)
    (arxload "achlnkui")
  )
  
;  Search list for first object with an URL
  (while (and (= url nil) (< i (sslength ssurl)))
    (setq url (geturl (ssname ssurl i))
	  i (1+ i))
  )

; If an URL has be found, open browser and point to URL
  (if (= url nil)
    (alert "No Universal Resource Locator associated with the object.")
    (command "_.browser" url)
  )

  (setq *error* m:err m:err nil)
  (princ)

)

;; Used by the import dialog to silently load a 3ds file
(defun import3ds (filename / filedia_old render)
  ;; Load Render if not loaded
  (setq render (findfile "acRender.arx"))
  (if render
    (verify_arxapp_loaded render) 
    (quit)
  )

  ;; Save current filedia & cmdecho setting.
  (setq filedia-save (getvar "FILEDIA"))
  (setq cmdecho-save (getvar "CMDECHO"))
  (setvar "FILEDIA" 0)
  (setvar "CMDECHO" 0)

  ;; Call 3DSIN and pass in filename.
  (c:3dsin 1 filename)

  ;; Reset filedia & cmdecho
  (setvar "FILEDIA" filedia-save)
  (setvar "CMDECHO" cmdecho-save)
  (princ)
)

;;;----------------------------------------------------------------------------
; New "Select All" function.  Cannot be called transparently.

(defun c:ai_selall ( / ss old_error a b old_cmd old_hlt)
  (setq a "CMDECHO" b "HIGHLIGHT"
        old_cmd (getvar a)  old_hlt (getvar b)           
        old_error *error* *error* ai_error)
  (if (ai_notrans)
    (progn
      (princ "Selecting objects...")
      (setvar a 0)
      (setvar b 0)
      (command "_.SELECT" "_ALL" "")    ; Create Previous SS
      (setvar a old_cmd)
      (setvar b old_hlt)
      (setq ss (ssget "_P"))
      (sssetfirst ss ss)        ; Non-gripped, but selected (someday!)
      (princ "done.\n")
    )
  )
  (setq *error* old_error old_error nil ss nil)
  (princ)
)

;;;
;;; Routines that check CMDACTIVE and post an alert if the calling routine
;;; should not be called in the current CMDACTIVE state.  The calling 
;;; routine calls (ai_trans) if it can be called transparently or 
;;; (ai_notrans) if it cannot.
;;;
;;;           1 - Ordinary command active.
;;;           2 - Ordinary and transparent command active.
;;;           4 - Script file active.
;;;           8 - Dialogue box active.
;;;
(defun ai_trans ()
  (if (zerop (logand (getvar "cmdactive") (+ 2 8) ))
    T
    (progn 
      (alert "This command may not be invoked transparently.")
      nil
    )
  )
)

(defun ai_transd ()
  (if (zerop (logand (getvar "cmdactive") (+ 2) ))
    T
    (progn 
      (alert "This command may not be invoked transparently.")
      nil
    )
  )
)

(defun ai_notrans ()
  (if (zerop (logand (getvar "cmdactive") (+ 1 2 8) ))
    T
    (progn 
      (alert "This command may not be invoked transparently.")
      nil
    )
  )
)

;;;----------------------------------------------------------------------------
; New function for invoking the product support help through the browser

(defun C:ai_product_support ()
   (setq url "http://www.autodesk.com/autocad-support")
   (command "_.browser" url)
)

(defun C:ai_product_support_safe ()
   (setq url "http://www.autodesk.com/autocad-support")
   (setq 404page "WSProdSupp404.htm")
   (command "_.browser2" 404page url)
)

(defun C:ai_training_safe ()
   (setq url "http://www.autodesk.com/autocad-training")
   (setq 404page "WSTraining404.htm")
   (command "_.browser2" 404page url)
)

(defun C:ai_custom_safe ()
   (setq url "http://www.autodesk.com/developautocad")
   (setq 404page "WSCustom404.htm")
   (command "_.browser2" 404page url)
)

;;; ==== Originally defined in Acad.mnl ====
;;; These were moved to this file to ease migration.
(princ "\nAutoCAD menu utilities ")


;;;=== Icon Menu Functions ===

;;;  View -> Layout -> Tiled Viewports...

(defun ai_tiledvp_chk (new)
  (setq m:err *error* *error* *merrmsg*)

  (if (= (getvar "TILEMODE") 0)
    (progn
      (princ "\n** Command not allowed in a Layout **")
      (princ)
    )
    (progn
      (if new
        (menucmd "I=ACAD.IMAGE_VPORTI")
        (menucmd "I=IMAGE_VPORTI")
      )
      (menucmd "I=*")
    )
  )
  (setq *error* m:err m:err nil)
  (princ)
)

(defun ai_tiledvp (num ori / ai_tiles_g ai_tiles_cmde)
  (setq m:err *error* *error* *merrmsg*
        ai_tiles_cmde (getvar "CMDECHO")
        ai_tiles_g (getvar "GRIDMODE")
  )
  (ai_undo_push)
  (setvar "CMDECHO" 0)
  (setvar "GRIDMODE" 0)
  (cond ((= num 1)
         (command "_.VPORTS" "_SI")
         (setvar "GRIDMODE" ai_tiles_g)
        )
        ((< num 4)
         (command "_.VPORTS" "_SI")
         (command "_.VPORTS" num ori)
         (setvar "GRIDMODE" ai_tiles_g)
        )
        ((= ori nil)
         (command "_.VPORTS" "_SI")
         (command "_.VPORTS" num)
         (setvar "GRIDMODE" ai_tiles_g)
        )
        ((= ori "_L")
         (command "_.VPORTS" "_SI")
         (command "_.VPORTS" "2" "")
         (setvar "CVPORT" (car (cadr (vports))))
         (command "_.VPORTS" "2" "")
         (command "_.VPORTS" "_J" "" (car (cadr (vports))))
         (setvar "CVPORT" (car (cadr (vports))))
         (command "_.VPORTS" "3" "_H")
         (setvar "GRIDMODE" ai_tiles_g)
         (setvar "CVPORT" (car (cadddr (vports))))
         (setvar "GRIDMODE" ai_tiles_g)
         (setvar "CVPORT" (car (cadddr (vports))))
         (setvar "GRIDMODE" ai_tiles_g)
         (setvar "CVPORT" (car (cadddr (vports))))
         (setvar "GRIDMODE" ai_tiles_g)
        )
        (T
         (command "_.VPORTS" "_SI")
         (command "_.VPORTS" "2" "")
         (command "_.VPORTS" "2" "")
         (setvar "CVPORT" (car (caddr (vports))))
         (command "_.VPORTS" "_J" "" (car (caddr (vports))))
         (setvar "CVPORT" (car (cadr (vports))))
         (command "_.VPORTS" "3" "_H")
         (setvar "GRIDMODE" ai_tiles_g)
         (setvar "CVPORT" (car (cadddr (vports))))
         (setvar "GRIDMODE" ai_tiles_g)
         (setvar "CVPORT" (car (cadddr (vports))))
         (setvar "GRIDMODE" ai_tiles_g)
         (setvar "CVPORT" (car (cadddr (vports))))
         (setvar "GRIDMODE" ai_tiles_g)
        )
  )
  (ai_undo_pop)
  (setq *error* m:err m:err nil)
  (setvar "CMDECHO" ai_tiles_cmde)
  (princ)
)


;;;=== General Utility Functions ===

;;; ai_popmenucfg -- retrieve parameter from cfg settings
;;; <param> is a string specifiying the parameter
 
(defun ai_popmenucfg (param)
  (set (read param) (getcfg (strcat "CfgData/Menu/" param)))
)

;;; ai_putmenucfg -- store parameter in cfg settings
;;; <param> is a string specifiying the parameter
;;; <cfgval> is the value for that parameter

(defun ai_putmenucfg (param cfgval)
  (setcfg (strcat "CfgData/Menu/" param) cfgval)
)

(defun *merr* (msg)
  (ai_sysvar nil) ;; reset system variables
  (setq *error* m:err m:err nil)
  (princ)
)

(defun *merrmsg* (msg)
  (princ msg)
  (setq *error* m:err m:err nil)
  (princ)
)


(defun ai_showedge_alert ()
   (alert "Invisible edges will be shown after next Regeneration.")
   (princ)
)

(defun ai_hideedge_alert ()
   (alert "Invisible edges will be HIDDEN after next Regeneration.")
   (princ)
)

(defun ai_viewports_alert ()
   (princ "** Command not allowed in Model Tab **")
   (setq *error* m:err m:err nil)
   (princ)
)

(defun ai_refedit_alert ()
   (princ "\n** Command not allowed unless a reference is checked out with REFEDIT command **")
   (setq *error* m:err m:err nil)
   (princ)
)

;;; --- ai_sysvar ------------------------------------------
;;; Change system variable settings and save current settings
;;; (Note: used by *merr* to restore system settings on error.)
;;;
;;; The <vars> argument is used to... 
;;;   restore previous settings (ai_sysvar NIL)
;;;   set a single sys'var (ai_sysvar  '("cmdecho" . 0))
;;;   set multiple sys'vars (ai_sysvar '(("cmdecho" . 0)("gridmode" . 0)))
;;;
;;; defun-q is needed by Visual Lisp for functions which redefine themselves.
;;; it is aliased to defun for seamless use with AutoLISP.

(defun-q ai_sysvar (vars / savevar pair varname varvalue varlist)

   (setq varlist nil)  ;; place holder for varlist

   (defun savevar (varname varvalue / pair)
      (cond
             ;; if new value is NIL, save current setting
         ((not varvalue) 
            (setq varlist 
               (cons 
                   (cons varname (getvar varname)) 
                    varlist
               )
            )
             )
                 ;; change sys'var only if it's different
         ((/= (getvar varname) varvalue)
                 ;; add current setting to varlist, change setting
             (setq varlist 
                (cons 
                   (cons varname (getvar varname)) 
                    varlist
                )
             )
             (setvar varname varvalue)
             )
                 (T nil)
          )
   )

   (cond
          ;; reset all values
      ((not vars)
         (foreach pair varlist
            (setq  varname (car pair)  
                  varvalue (cdr pair)
            )
            (setvar varname varvalue)
                 )
         (setq varlist nil) 
          )

          ((not (eq 'LIST (type vars)))
              (princ "\nAI_SYSVAR: Bad argument type.\n")
          )

          ;; set a single system variable
      ((eq 'STR (type (car vars)))
         (savevar (car vars) (cdr vars))

          )

          ;; set multiple system variables
      ((and 
                (eq 'LIST (type (car  vars)))
            (eq 'STR  (type (caar vars)))
           )
         (foreach pair vars
            (setq  varname (car pair)  
                  varvalue (cdr pair)
            )
            (if (not (eq 'STR (type varname)))
                (princ "\nAI_SYSVAR: Bad argument type.\n")
                (savevar varname varvalue)
            )
                 )
          )
      
      (T (princ "\nAI_SYSVAR: Error in first argument.\n"))
   
   );cond
   
   ;; redefine ai_sysvar function to contain the value of varlist
   (setq ai_sysvar 
      (cons (car ai_sysvar) 
              (cons (list 'setq 'varlist (list 'quote varlist))
                        (cddr ai_sysvar)
                  )
          )
   )

   varlist  ;; return the list

);sysvar


;;; return point must be on an entity
;;;
(defun ai_entsnap (msg osmode / entpt)
   (while (not entpt)
          (setq entpt (last (entsel msg)))
   )
   (if osmode 
      (setq entpt (osnap entpt osmode))
   )
   entpt
)

;;; 
;;; These UNDO handlers are taken from ai_utils.lsp and copied here to
;;; avoid loading all of ai_utils.lsp. Command echo control has also
;;; been added so that UNDO commands aren't echoed everywhere.
;;;
;;; UNDO handlers.  When UNDO ALL is enabled, Auto must be turned off and 
;;; GROUP and END added as needed. 
;;;
(defun ai_undo_push()
  (ai_sysvar '("cmdecho" . 0))
  (setq undo_init (getvar "undoctl"))
  (cond
    ((and (= 1 (logand undo_init 1))   ; enabled
          (/= 2 (logand undo_init 2))  ; not ONE (ie ALL is ON)
          (/= 8 (logand undo_init 8))   ; no GROUP active
     )
      (command "_.undo" "_group")
    )
    (T)
  )  
  ;; If Auto is ON, turn it off.
  (if (= 4 (logand 4 undo_init))
      (command "_.undo" "_auto" "_off")
  )
  (ai_sysvar NIL)
)

;;;
;;; Add an END to UNDO and return to initial state.
;;;
(defun ai_undo_pop()
  (ai_sysvar '("cmdecho" . 0))
  (cond 
    ((and (= 1 (logand undo_init 1))   ; enabled
          (/= 2 (logand undo_init 2))  ; not ONE (ie ALL is ON)
          (/= 8 (logand undo_init 8))   ; no GROUP active
     )
      (command "_.undo" "_end")
    )
    (T)
  )  
  ;; If it has been forced off, turn it back on.
  (if (= 4 (logand undo_init 4))
    (command "_.undo" "_auto" "_on")
  )  
  (ai_sysvar NIL)
)

;;;=== Menu Functions ======================================

(defun ai_rootmenus ()
  (setq T_MENU 0)
  (menucmd "S=S")
  (menucmd "S=ACAD.S")
  (princ)
)

(defun c:ai_fms ( / fmsa fmsb)
  (setq m:err *error* *error* *merr*)
  (ai_undo_push)
  (if (getvar "TILEMODE") (setvar "TILEMODE" 0))
  (setq fmsa (vports) fmsb (nth 0 fmsa))
  (if (member 1 fmsb)
    (if (> (length fmsa) 1)
      (command "_.mspace")
      (progn
        (ai_sysvar '("cmdecho" . 1))
        (command "_.mview")
        (while (eq 1 (logand 1 (getvar "CMDACTIVE")))
          (command pause)
        )
        (ai_sysvar NIL)
        (command "_.mspace")
      )
    )
  )
  (ai_undo_pop)
  (setq *error* m:err m:err nil)
  (princ)
)

(defun ai_onoff (var)
  (setvar var (abs (1- (getvar var))))
  (princ)
)

;;; go to paper space
(defun c:ai_pspace ()
  (ai_undo_push)
  (if (/= 0 (getvar "tilemode"))
    (command "_.tilemode" 0)
  )
  (if (/= 1 (getvar "cvport"))
    (command "_.pspace")
  )
  (ai_undo_pop)
  (princ)
)

;;; go to tilemode 1
(defun c:ai_tilemode1 ()
  (ai_undo_push)
  (if (/= 1 (getvar "tilemode"))
    (command "_.tilemode" 1)
  )
  (ai_undo_pop)
  (princ)
)

;;; Pop menu Draw/ Dim/ Align Text/ Centered
;;; Toolbar Dimensions/ Align Text/ Centered

(defun ai_dim_cen (/ ai_sysvar ai_dim_ss)
  (setq ai_sysvar (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (cond
    ((setq ai_dim_ss (ssget  "_P" '((0 . "DIMENSION"))))
      (command "_.dimoverride" "_dimjust" 0 "" ai_dim_ss "" 
               "_.dimtedit" ai_dim_ss "_h")
    )
    (T nil)
  )
  (setvar "cmdecho" ai_sysvar)
  (princ)
)

;;; Shortcut menu for Dimension Text Above 

(defun c:ai_dim_textabove (/ ss)
  (ai_sysvar '("cmdecho" . 0))
  (if (setq ss (ssget "_I"))
    (command "_.dimoverride" "_dimtad" 3 "" ss "")
    (if (setq ss (ssget))
      (command "_.dimoverride" "_dimtad" 3 "" ss "") 
    )
  )
  (ai_sysvar NIL)
  (princ)
)

;;; Shortcut menu for Dimension Text Center 

(defun c:ai_dim_textcenter (/ ss)
  (ai_sysvar '("cmdecho" . 0))
  (if (setq ss (ssget "_I"))
    (command "_.dimoverride" "_dimtad" 0 "" ss "")
    (if (setq ss (ssget))
      (command "_.dimoverride" "_dimtad" 0 "" ss "") 
    )
  )
  (ai_sysvar NIL)
  (princ)
)

;;; Shortcut menu for Dimension Text Home 

(defun c:ai_dim_texthome (/ ss)
  (ai_sysvar '("cmdecho" . 0))
  (if (setq ss (ssget "_I"))
    (command "_.dimedit" "_h")
    (if (setq ss (ssget))
      (command "_.dimedit" "_h" ss)
    )
  )
  (ai_sysvar NIL)
  (princ)
)


;;; Screen menu item for CIRCLE TaTaTan option.
;;;     first, get points on entities
(defun ai_circtanstart()
   (setq m:err *error* *error* *merr*)
   (ai_sysvar 
      (list '("cmdecho" . 0)
         ;; make sure _tan pick for CIRCLE gets same entity
         (cons "aperture"  (getvar "pickbox"))
          )
   )
   ;; prompts are the same as CIRCLE/TTR command option
   (setq pt1 (ai_entsnap "\nEnter Tangent spec: "  nil))
   (setq pt2 (ai_entsnap "\nEnter second Tangent spec: " nil))
   (setq pt3 (ai_entsnap "\nEnter third Tangent spec: "  nil))
)
;;; Command-line version
(defun c:ai_circtan (/ pt1 pt2 pt3)
   (ai_circtanstart)

   (ai_sysvar '("osmode" . 256))
   (command "_.circle" "_3p" "_tan" pt1 "_tan" pt2 "_tan" pt3)
   
   (ai_sysvar nil)
   (setq *error* m:err m:err nil)
   (princ)
)
;;; Use this if CMDNAMES == CIRCLE
(defun ai_circtan (/ pt1 pt2 pt3)
   (ai_circtanstart)

   (ai_sysvar '("osmode" . 256))
   (command "_3p" pt1  pt2  pt3)
   
   (ai_sysvar nil)
   (setq *error* m:err m:err nil)
   (princ)
)



;;; Shortcut menu Deselect All item.

(defun ai_deselect ()
   (if (= (getvar "cmdecho") 0)			;start if
       (command "_.select" "_r" "_all" "")
       (progn					;start progn for cmdecho 1
           (setvar "cmdecho" 0)
           (command "_.select" "_r" "_all" "")
           (setvar "cmdecho" 1)
       )					;end progn for cmdecho 1
   )						;end if
   (terpri)
   (prompt "Everything has been deselected")
   (princ)
)

;;;
;;; Enable Draworder to be called from a menu
;;; Checks for Pickfirst selected objects
;;;

(defun ai_draworder (option / ss )

  (setq m:err *error* *error* *merr*)
  (ai_sysvar '("cmdecho" . 0))

  (if (setq ss (ssget "_I"))
    (command "_.draworder" option)
    (if (setq ss (ssget))
      (command "_.draworder" ss "" option)
    )
  )
  (ai_sysvar NIL)
  (setq *error* m:err m:err nil)

  (princ)
)

(defun c:vlisp ()
  (if (/= nil c:vlide) (c:vlide))
)

(princ "loaded.")

;; Silent load.
(princ)

