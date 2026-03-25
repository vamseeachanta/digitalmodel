; Next available MSG number is    86
; MODULE_ID ACADR13_LSP_
;;;    ACADR14.LSP Version 14.1 for Release 14 
;;;
;;;    Copyright (C) 1994 - 1997 by Autodesk, Inc.
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
  (alert "The Object Properties toolbar incorporates DDEMODES functionality.  \nDDEMODES has been discontinued.  \n\nFor more information, select DDEMODES from the AutoCAD Help Index tab.")
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

(defun autoxload (app cmdliste)
  (_autoqload "x" app cmdliste)
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

(defun _autoxload (app)
; (princ "Auto:(xload ") (princ app) (princ ")") (terpri)
  (if (= app "region") (ai_select))
  (xload app)
  (if (= app "region") (ai_amegrey "~"))
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

(autoload "appload" '("appload" "appload"))

(autoload "edge"  '("edge"))
(setfunhelp "C:edge" "" "edge")

(autoload "filter" '("filter " "filter"))

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

(autoload "ddinsert" '("ddinsert"))

(autoload "ddattdef" '("ddattdef"))

(autoload "ddattext" '("ddattext"))

(autoload "3darray" '("3darray"))
(setfunhelp "C:3darray" "" "3darray")

(autoload "ddmodify" '("ddmodify"))

(autoload "ddchprop" '("ddchprop"))

(autoload "ddview" '("ddview"))

(autoload "ddvpoint" '("ddvpoint"))

(autoload "mvsetup" '("mvsetup"))
(setfunhelp "C:mvsetup" "" "mvsetup")

(autoload "ddosnap" '("ddosnap"))

(autoload "ddptype" '("ddptype"))

(autoload "dducsp" '("dducsp"))

(autoload "ddunits" '("ddunits"))

(autoload "ddgrips" '("ddgrips"))

(autoload "ddselect" '("ddselect"))

(autoload "ddrename" '("ddrename"))

(autoload "ddcolor" '("ddcolor"))

(autoload "bmake" '("bmake"))

(autoload "attredef" '("attredef"))
(setfunhelp "C:attredef" "" "attredef")

(autoload "xplode" '("xp" "xplode"))
(setfunhelp "C:xplode" "" "xplode")

(autoload "tutorial" '("tutdemo" "tutclear"
				       "tutdemo" 
				       "tutclear"))

;; CalComp Configuration Command
(autoload "plpccw" '("cconfig"))


;;;===== AutoXLoad ADS Applications =====

(autoxload "hpmplot" ' ("hpconfig" "hpconfig" ))


;;;=========AutoArxLoad OCE Driver ARX applications ===========

(autoarxload "oceconf" '("oceconfig" "oceconfig"))

;;;===== AutoArxLoad Arx Applications =====

(autoarxload "geomcal" '("cal" "cal"))

(autoarxload "geom3d" '("mirror3d" "rotate3d" "align"
		      "mirror3d" "rotate3d" 
                                 "align"))


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
          (and (<= 161 code) (<= code 253))
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

  (if (not (setq ssurl (ssget "I")))
      (setq ssurl (ssget))
  )

; if geturl LISP command not found then load arx application

  (if (/= (type geturl) 'EXRXSUBR)
    (arxload "dwfout")
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
  (setq render (findfile "render.arx"))
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

;; Silent load.
(princ)

;; The following line conditionally loads AutoLISP routines for AutoCAD Release 14
;; Altering this line will affect bonus functionality
(load "bonus.lsp" "")

;Quick Zoom
(defun c:zx ()  (Setvar "cmdecho" 0)(command "'.zoom" ".5x")(Setvar "cmdecho" 1))
(defun c:zp ()  (Setvar "cmdecho" 0)(command "'.zoom" "p")(Setvar "cmdecho" 1))
(defun c:ze ()  (Setvar "cmdecho" 0)(command "'.zoom" "e")(Setvar "cmdecho" 1))
(defun c:za ()  (Setvar "cmdecho" 0)(command "'.zoom" "a")(Setvar "cmdecho" 1))
(defun c:zc ()  (Setvar "cmdecho" 0)(command "'.zoom" "c")(Setvar "cmdecho" 1))
(defun c:zs ()  (Setvar "cmdecho" 0)(command "'.zoom" "s")(Setvar "cmdecho" 1))
(defun c:zw ()  (Setvar "cmdecho" 0)(command "'.zoom" "w")(Setvar "cmdecho" 1))
(defun c:zd ()  (Setvar "cmdecho" 0)(command "'.zoom" "d")(Setvar "cmdecho" 1))
(defun c:+++ () (Setvar "cmdecho" 0) (command "'.zoom" "0.1x")(Setvar "cmdecho" 1))
(defun c:++ () (Setvar "cmdecho" 0) (command "'.zoom" "0.3x")(Setvar "cmdecho" 1))
(defun c:+ () (Setvar "cmdecho" 0) (command "'.zoom" "0.5x")(Setvar "cmdecho" 1))
(defun c:- () (Setvar "cmdecho" 0) (command "'.zoom" "2x")(Setvar "cmdecho" 1))
(defun c:-- () (Setvar "cmdecho" 0) (command "'.zoom" "3x")(Setvar "cmdecho" 1))
(defun c:--- ()  (command "'.zoom" "4x"))
(DEFUN XD ()
 (SETQ CMD (GETVAR "CMDECHO"))
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "W")
 (SETVAR "CMDECHO" 1)
 (PROMPT"\nSelect vertical dimension(s) extensions to be aligned")
 (SETQ SET (SSGET))
 (PROMPT"\nSelect new extension position")
 (SETQ P1 (GETPOINT))
 (SETQ X3 (CAR P1))
 (SETQ QUANT (SSLENGTH SET))
 (SETQ INDEX 0)
  (WHILE (< INDEX QUANT)
   (IF (AND(= "DIMENSION" (CDR (ASSOC 0 (SETQ A (ENTGET (SSNAME SET INDEX))))))
       )
    (PROGN
     (SETQ L13 (ASSOC 13 A))
     (SETQ M13 (CDR L13))
     (SETQ L14 (ASSOC 14 A))
     (SETQ M14 (CDR L14))
     (SETQ P13 (LIST 13 X3 (CADR M13) (CADDR M13)))
     (SETQ P14 (LIST 14 X3 (CADR M14) (CADDR M14)))
     (SETQ A (SUBST P13 L13 A))
     (SETQ A (SUBST P14 L14 A))
     (ENTMOD A)			
    )
   )
  (SETQ INDEX (+ INDEX 1))
 )
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "P")
 (SETVAR "CMDECHO" CMD)
 (PRINC)
)
(DEFUN C:XD () (XD) )
(DEFUN C:XX () (XD) )
(DEFUN C:SDD () (XD) )
(DEFUN YD ()
 (SETQ CMD (GETVAR "CMDECHO"))
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "W")
 (SETVAR "CMDECHO" 1)
 (PROMPT"\nSelect dimension(s) to be aligned")
 (SETQ SET (SSGET))
 (PROMPT"\nSelect new dimension position")
 (SETQ P1 (GETPOINT))
 (SETQ Y3 (CADR P1))
 (SETQ QUANT (SSLENGTH SET))
 (SETQ INDEX 0)
  (WHILE (< INDEX QUANT)
   (IF (AND(= "DIMENSION" (CDR (ASSOC 0 (SETQ A (ENTGET (SSNAME SET INDEX))))))
       )
    (PROGN
     (SETQ L13 (ASSOC 13 A))
     (SETQ M13 (CDR L13))
     (SETQ L14 (ASSOC 14 A))
     (SETQ M14 (CDR L14))
     (SETQ P13 (LIST 13 (CAR M13) Y3 (CADDR M13)))
     (SETQ T13 (CDR P13))
     (SETQ P14 (LIST 14 (CAR M14) Y3 (CADDR M14)))
     (SETQ A (SUBST P13 L13 A))
     (SETQ A (SUBST P14 L14 A))
     (ENTMOD A)			
    )
   )
  (SETQ INDEX (+ INDEX 1))
 )
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "P")
 (SETVAR "CMDECHO" CMD)
)
(DEFUN C:YD () (YD) )
(DEFUN C:YY () (YD) )
(DEFUN C:SDN () (YD) )
(Defun c:CB (/ a)
   (setq olderr *error* *error* myerror)
   (setq ocmd (getvar "cmdecho"))
   (setq oblp (getvar "blipmode"))
   (setvar "cmdecho" 0)
   (setq a (ssget))
   (command "CHPROP" a "" "C" "bylayer" "")
   (setvar "cmdecho" ocmd)
   (setvar "blipmode" oblp)
   (setq *error* olderr)
   (princ))
(Defun c:CHC (/ a)
   (setq olderr *error* *error* myerror)
   (setq ocmd (getvar "cmdecho"))
   (setq oblp (getvar "blipmode"))
   (setvar "cmdecho" 0)
   (setq a (ssget) color (getstring "\nChange to new Color: "))
   (command "CHPROP" a "" "C" color "")
   (setvar "cmdecho" ocmd)
   (setvar "blipmode" oblp)
   (setq *error* olderr)
   (princ))
(Defun c:CL (/ ss layer cmd r)
   (setq olderr *error* *error* myerror)
   (setq ocmd (getvar "cmdecho"))
   (setq oblp (getvar "blipmode"))
   (setvar "cmdecho" 0)
   (setq ss (ssget) layer (getstring "\nChange to new Layer: "))
      (if layer (progn (if (null (tblsearch "layer" layer))
      (progn (alert "Not found layer!")(princ layer)
         (initget "New")
         (setq r (getkword "\nNew/<Make>: "))
            (if (equal r "New")
            (progn (setq layer (getstring "\nLayer name: "))
            (command "layer" "n" layer ""))
         (command "Layer" "m" layer ""))))
      (command "change" ss "" "p" "la" layer "")
   (setvar "cmdecho" ocmd)
   (setvar "blipmode" oblp)
   (setq *error* olderr)
   (princ))))
(defun ai_undo_on ()
  (setq undo_setting (getvar "undoctl"))
  (cond
    ((= 2 (logand undo_setting 2))     ; Undo is one
      (command "_.undo" "_control" "_all" "_.undo" "_auto" "_off")
    )
    ((/= 1 (logand undo_setting 1))    ; Undo is disabled
      (command "_.undo" "_all" "_.undo" "_auto" "_off")
    )
  )
)

;;;f
;;; Return UNDO to the initial setting.  Do not use with new routines as they 
;;; should be designed to operate with any UNDO setting.
;;;
(defun ai_undo_off ()
  (cond 
    ((/= 1 (logand undo_setting 1))
      (command "_.undo" "_control" "_none")
    )
    ((= 2 (logand undo_setting 2))
      (command "_.undo" "_control" "_one")
    )
  )
)

(defun ai_undo_push()
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
)

;;;
;;; Add an END to UNDO and return to initial state.
;;;
(defun ai_undo_pop()
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
)

;===== AUTO HATCH (hh) ========

(defun mkhatch(v_hatchtp v_scale v_angle data_m / i)
	(command "hatch" v_hatchtp v_scale v_angle)
	(setq i 0)
	(while (< i (length data_m)) (progn
		(command (nth i data_m))
		(setq i (+ i 1))
	))
	(command "")  
)

(defun c:hh(/ data_m check)

  
(defun ah_import(/ p1 p2 old1 ent1 ent2 axa)
  (if (= nil hscale_d) (setq hscale_d 1))  
  (setq old1 (getvar "osmode") check 1)
  (setvar "osmode" 0)
  (setq p1 '(0 0 0) p2 p1)
  (command "line"  p1 p2 "")
  
  (setq data_m '())
  (setq ent1 (entlast) ent2 ent1)
  
  (princ ent1)
  
  (setvar "osmode" old1)
  
  (command "boundary")
  (setq p1 (getpoint))
  (while (not (= nil p1)) (progn
	
	(command p1)
	
	
	(setq p1 (getpoint))
	
  ))
  (command "")
  
  (setq ent1 (entnext ent1))
  
  (princ ent1)
  (if (= nil ent1) (setq check 0) (progn
	(while (not (= nil ent1)) (progn
		(setq data_m (append data_m (list ent1)))
		(setq ent1 (entnext ent1))
	))
  ))
  (command "erase" ent2 "")
  (princ)
)

(defun ah_procced(/ i s1)
  
  (if (= 0 check) (princ "\ninvalid data") (progn
	(initget 1 "WALL W CONCRETE C GROUND G FLOOR F")
  	(setq s1 (getkword "\nWall/Concrete/Ground/Floor : "))
	(if (not (= nil s1)) (progn
		(cond
		  ((or (= "W" (strcase s1)) (= "WALL" (strcase s1))) (mkhatch "ansi31" (* 500 hscale_d) 0 data_m))
		  ((or (= "C" (strcase s1)) (= "CONCRETE" (strcase s1))) (progn
				(mkhatch "ansi33" (* 180 hscale_d) 0 data_m)
    			(mkhatch "ar-conc" (* 20 hscale_d) 0 data_m)
																	   
		  ))
		  ((or (= "G" (strcase s1)) (= "GROUND" (strcase s1))) (mkhatch "ansi38" (* 600 hscale_d) 0 data_m))
		  ((or (= "F" (strcase s1)) (= "FLOOR" (strcase s1))) (mkhatch "ar-conc" (* 20 hscale_d) 0 data_m))
		)
    ))						 
	
	(command "erase")
	(setq i 0)
	(while (< i (length data_m)) (progn
		(command (nth i data_m))
		(setq i (+ i 1))
	))
	(command "")
  ))
  
  (princ)
)
  (ai_undo_push)	
  (ah_import)
  
  (ah_procced)
  (ai_undo_pop)
)


(defun c:hscale(/ i)
  (ai_undo_push)
  (if (= nil hscale_d) (setq hscale_d 1))
  (setq i (getreal (strcat (strcat "enter new hatch scale <" (rtos hscale_d 2 5)) "> ")))
  (if (not (= nil i)) (setq hscale_d i))
  (ai_undo_pop)
  (princ)
)
;===== AUTO DRAW DOOR-WINDOW (d1,w1,w2) ========

(defun moveent(ls1 post1 post2 / ls2 ent1 ent2 ent3 i)
    (setq ent1 (nth post1 ls1) ent2 (nth post2 ls1) i 0 ls2 '())
    (while (< i (length ls1)) (progn
        (if (= i post1) (setq ent3 ent2)
            (if (= i post2) (setq ent3 ent1) (setq ent3 (nth i ls1))
            )
        )
        (if (= nil ls2) (setq ls2 (list ent3))
            (setq ls2 (append ls2 (list ent3)))
        )
        (setq i (+ i 1))
    ))
    (setq ls1 ls2)
)

(defun arlst(ls1 / ls2 i j k)
;    (princ ls1)
    (setq i 0 ls2 ls1)
    (if (> (length ls2) 1) (progn
        (while (< i (- (length ls2) 1) ) (progn
            (setq j (+ i 1) k 0)
            (while (and (= 0 k) (< j (length ls2) )) (progn
                (if (< (nth j ls2) (nth i ls2)) (progn
                    ;(setq k 1)
                    (setq ls2 (moveent ls2 i j))
                ))
                (setq j (+ j 1))
            ))
            (setq i (+ i 1))
        ))
    ))
    (setq ls1 ls2)
)

(defun mkline(point1 point2 line1 / line2 i)
    (setq i 0 line2 '())
    (while (< i (length line1)) (progn
        (if (and (not (= 10 (car (nth i line1))) )
                 (not (= 11 (car (nth i line1))))) (progn
                    (if (= nil line2) (setq line2 (list (nth i line1)))
                        (setq line2 (append line2 (list (nth i line1))))
                    )
                 ))
        (setq i (+ i 1))
    ))
    (setq point1 (list 0 (nth 0 point1) (nth 1 point1) (nth 2 point1)))
    (setq point2 (list 0 (nth 0 point2) (nth 1 point2) (nth 2 point2)))
    (setq line2 (append line2 (list (cons 10 (cdr point1)))))
    (setq line2 (append line2 (list (cons 11 (cdr point2)))))
;    (princ line2)
    (entmake line2)
    (princ)
)

(defun drawrec (point1 point2 l1 / point3 point4)
    (setq point3 (list (nth 0 point1) (nth 1 point2) 0))
    (mkline point1 point3 l1) (mkline point2 point3 l1)
    (setq point3 (list (nth 0 point2) (nth 1 point1) 0))
    (mkline point1 point3 l1) (mkline point2 point3 l1)
    
)

(defun drawrt (point1 point2 / point3 point4 x l1)
    (setq l1 (list
        (cons 0 "line")
        (cons 8 (getvar "clayer"))
    ))
    
    (drawrec point1 point2 l1)
    (setq x (abs (- (nth 0 point1) (nth 0 point2))))
    (setq point3 (list (+ (nth 0 point1) (* 0.15 x) ) (nth 1 point1) 0))
    (setq point4 (list (nth 0 point3) (nth 1 point2) 0))
    (mkline point3 point4 l1)

    (setq point3 (list (- (nth 0 point2) (* 0.15 x) ) (nth 1 point1) 0))
    (setq point4 (list (nth 0 point3) (nth 1 point2) 0))
    (mkline point3 point4 l1)

    (setq point3 (list (+ (nth 0 point1) (* 0.15 x) ) (* 0.5 (+(nth 1 point1) (nth 1 point2))) 0))
    (setq point4 (list (- (nth 0 point2) (* 0.15 x) ) (nth 1 point3) 0))
    (mkline point3 point4 l1)

    (princ)
)

(defun drawrt1 (point1 point2 / point3 point4 x l1)
    (setq l1 (list
        (cons 0 "line")
        (cons 8 (getvar "clayer"))
    ))
    
    (drawrec point1 point2 l1)
    (setq x (abs (- (nth 1 point1) (nth 1 point2))))
    (setq point3 (list (nth 0 point1) (+ (nth 1 point1) (* 0.15 x) ) 0))
    (setq point4 (list (nth 0 point2) (nth 1 point3) 0))
    (mkline point3 point4 l1)

    (setq point3 (list (nth 0 point1) (- (nth 1 point2) (* 0.15 x) ) 0))
    (setq point4 (list (nth 0 point2) (nth 1 point3) 0))
    (mkline point3 point4 l1)

    (setq point3 (list (* 0.5 (+ (nth 0 point1) (nth 0 point2))) (+ (nth 1 point1) (* 0.15 x) ) 0))    (setq point4 (list (nth 0 point3) (- (nth 1 point2) (* 0.15 x) ) 0))
    (mkline point3 point4 l1)

    (princ)
)

(defun drawrt2(pt1 pt2 l1 / pt3 pt4 i dy1 dy2 qt)
    (setq pt3 (list (nth 0 pt1) (nth 1 pt2) 0))
    (mkline pt1 pt3 l1)
    (setq pt3 (list (nth 0 pt2) (nth 1 pt1) 0))
    (mkline pt2 pt3 l1)
    (if (< 150 (- (nth 1 pt2) (nth 1 pt1))) (setq dy1 40)
        (setq dy1 (* 0.3 (- (nth 1 pt2) (nth 1 pt1)))) )
    (setq qt (fix (/ (- (- (nth 1 pt2) (nth 1 pt1)) dy1) (+ 650 dy1))))
    (if (= 0 qt) (setq dy2 (- (- (nth 1 pt2) (nth 1 pt1)) (* 2 dy1))) 
    (setq dy2 (/ (- (- (nth 1 pt2) (nth 1 pt1)) (* (+ qt 1) dy1)) qt))
    )
    (if (= 0 qt) (setq qt 1))
    (setq i 0)
    (while (< i qt) (progn
        (setq pt3 (list (nth 0 pt1) (+ (nth 1 pt1) (+ dy1 (* (+ dy1 dy2) i))) 0))
        (setq pt4 (list (nth 0 pt2) (nth 1 pt3) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (nth 0 pt1) (+ (nth 1 pt3) dy2) 0))
        (setq pt4 (list (nth 0 pt2) (nth 1 pt3) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (+ (nth 0 pt1) (* 0.38 (- (nth 0 pt2) (nth 0 pt1)))) (nth 1 pt3) 0))
        (setq pt4 (list (nth 0 pt3) (- (nth 1 pt3) dy2) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (- (nth 0 pt2) (* 0.38 (- (nth 0 pt2) (nth 0 pt1)))) (nth 1 pt3) 0))
        (setq pt4 (list (nth 0 pt3) (- (nth 1 pt3) dy2) 0))
        (mkline pt3 pt4 l1)

        (setq i (+ i 1))
    ))
;    (princ qt)
    (princ)
)

(defun drawrt3(pt1 pt2 l1 / pt3 pt4 i dy1 dy2 qt)
    (setq pt3 (list (nth 0 pt2) (nth 1 pt1) 0))
    (mkline pt1 pt3 l1)
    (setq pt3 (list (nth 0 pt1) (nth 1 pt2) 0))
    (mkline pt2 pt3 l1)
    (if (< 150 (- (nth 0 pt2) (nth 0 pt1))) (setq dy1 60)
        (setq dy1 (* 0.3 (- (nth 0 pt2) (nth 0 pt1)))) )
    (setq qt (fix (/ (- (- (nth 0 pt2) (nth 0 pt1)) dy1) (+ 650 dy1))))
    (if (= 0 qt) (setq dy2 (- (- (nth 0 pt2) (nth 0 pt1)) (* 2 dy1))) 
    (setq dy2 (/ (- (- (nth 0 pt2) (nth 0 pt1)) (* (+ qt 1) dy1)) qt))
    )
    (if (= 0 qt) (setq qt 1))
    (setq i 0)
    (while (< i qt) (progn
        (setq pt3 (list (+ (nth 0 pt1) (+ dy1 (* (+ dy1 dy2) i))) (nth 1 pt1) 0))
        (setq pt4 (list (nth 0 pt3) (nth 1 pt2) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (+ (nth 0 pt3) dy2) (nth 1 pt1) 0))
        (setq pt4 (list (nth 0 pt3) (nth 1 pt2) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (nth 0 pt3) (+ (nth 1 pt1) (* 0.38 (- (nth 1 pt2) (nth 1 pt1)))) 0))
        (setq pt4 (list (- (nth 0 pt3) dy2) (nth 1 pt3) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (nth 0 pt3) (- (nth 1 pt2) (* 0.38 (- (nth 1 pt2) (nth 1 pt1)))) 0))
        (setq pt4 (list (- (nth 0 pt3) dy2) (nth 1 pt3) 0))
        (mkline pt3 pt4 l1)

        (setq i (+ i 1))
    ))
;    (princ qt)
    (princ)
)





(defun c:d1(/ data_m l1 l2 p1 p2 check)

(defun dw_import(/ p3 p4 p5 p6)
    (setq data_m (ssget))
    (setq p1 (getpoint "\nfirst point :") p2 (getpoint "\nsecond point :"))
    (setq l1 nil l2 nil check 1)
    (if (not (= nil data_m)) (progn
        (setq l1 (entget (ssname data_m 0)))
        (setq l2 (entget (ssname data_m 1)))
        (if (or (= nil l1) (not (= "LINE" (cdr (assoc 0 l1))))) (setq check 0))
        (if (or (= nil l2) (not (= "LINE" (cdr (assoc 0 l2))))) (setq check 0))
        (if (not (= 0 (-(sslength data_m) 2))) (setq check 0))
        (if (= 1 check) (progn
            (setq p3 (cdr (assoc 10 l1))) (setq p3 (list (nth 0 p3) (nth 1 p3)))
            (setq p4 (cdr (assoc 11 l1))) (setq p4 (list (nth 0 p4) (nth 1 p4)))
            (setq p5 (cdr (assoc 10 l2))) (setq p5 (list (nth 0 p5) (nth 1 p5)))
            (setq p6 (cdr (assoc 11 l2))) (setq p6 (list (nth 0 p6) (nth 1 p6)))
            (if (not (= nil (inters p3 p4 p5 p6 nil))) (setq check 0))
        ))
    ) (setq check 0))
    (princ)
)

(defun dw_procced()

(defun mkv(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom)

    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 
    (if (> (abs (- (nth 1 p1) (nth 1 p3)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p1) (nth 1 p4)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p5)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p6)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (= 0 check) (princ "\ninvalid data") (progn
        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p3) (nth 1 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p3) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p5) (nth 1 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p5) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p5) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 0 p3) (nth 1 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 2 ls1) 0))
        (mkline p7 p8 l1)

        (setq getom (getvar "osmode"))
        (setvar "osmode" 0)
        (if (< (nth 1 p1) (nth 1 p2)) (progn
            (setq ls2 (arlst (list (nth 0 p1) (nth 0 p3) (nth 0 p5) )))
            ;(princ ls2)
            (if (= (nth 0 p1) (nth 0 ls2)) (progn
                (setq p7 (list (nth 1 ls2) (nth 1 ls1) 0))

                (setq p8 (list (- (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) ))) )
                                  (+ (nth 1 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                ;(princ p8)
                (drawrt p8 p7)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p7) (nth 1 p8) 0))
                (setq p9 (list (nth 1 ls2) (nth 2 ls1) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p9 p8)
                (setvar "cmdecho" 1)
            )(progn
                (setq ls2 (arlst (list (nth 0 p3) (nth 0 p5) )))
                (setq p7 (list (nth 1 ls2) (nth 1 ls1) 0))
                (setq p8 (list (+ (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) ))) )
                                  (+ (nth 1 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                (drawrt p7 p8)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p7) (nth 1 p8) 0))
                (setq p9 (list (nth 1 ls2) (nth 2 ls1) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p8 p9)
                (setvar "cmdecho" 1)
            ))
        ) (progn
            (setq ls2 (arlst (list (nth 0 p1) (nth 0 p3) (nth 0 p5) )))
            (if (= (nth 0 p1) (nth 0 ls2)) (progn
                (setq p7 (list (nth 1 ls2) (nth 2 ls1) 0))

                (setq p8 (list (- (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) ))) )
                                  (- (nth 2 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                ;(princ p8)
                (drawrt p8 p7)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p7) (nth 1 p8) 0))
                (setq p9 (list (nth 1 ls2) (nth 1 ls1) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p8 p9)
                (setvar "cmdecho" 1)
            )(progn
                (setq ls2 (arlst (list (nth 0 p3) (nth 0 p5) )))
                (setq p7 (list (nth 1 ls2) (nth 2 ls1) 0))
                (setq p8 (list (+ (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) ))) )
                                  (- (nth 2 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                (drawrt p7 p8)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p7) (nth 1 p8) 0))
                (setq p9 (list (nth 1 ls2) (nth 1 ls1) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p9 p8)
                (setvar "cmdecho" 1)
            ))

        ))
        (setvar "osmode" getom)
        (setvar "cmdecho" 0)
        (command "erase" data_m "")
        (setvar "cmdecho" 1)
    ))
    (princ)
)



(defun mkh(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom)
    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 

    (if (> (abs (- (nth 0 p1) (nth 0 p3)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p1) (nth 0 p4)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p5)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p6)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))

    (if (= 0 check) (princ "\ninvalid data") (progn
        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p3) (nth 0 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p5) (nth 0 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 1 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 2 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

        (setq getom (getvar "osmode"))
        (setvar "osmode" 0)
        (if (< (nth 0 p1) (nth 0 p2)) (progn
            (setq ls2 (arlst (list (nth 1 p1) (nth 1 p3) (nth 1 p5) )))
            ;(princ ls2)
            (if (= (nth 1 p1) (nth 0 ls2)) (progn
                (setq p7 (list (nth 1 ls1) (nth 1 ls2) 0))

                (setq p8 (list (+ (nth 1 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) ))))
                                (- (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                ;(princ p8)
                (drawrt1 p8 p7)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p8) (nth 1 p7) 0))
                (setq p9 (list (nth 2 ls1) (nth 1 ls2) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p8 p9)
                (setvar "cmdecho" 1)
            )(progn
                (setq ls2 (arlst (list (nth 1 p3) (nth 1 p5) )))
                (setq p7 (list (nth 1 ls1) (nth 1 ls2) 0))

                (setq p8 (list (+ (nth 1 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) ))))
                                (+ (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
;                (princ p7)
;                (princ p8)
                (drawrt1 p7 p8)    
                (setq p9 p7) 
                (setq p7 (list (nth 0 p8) (nth 1 p7) 0))
                (setq p9 (list (nth 2 ls1) (nth 1 ls2) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p9 p8)
                (setvar "cmdecho" 1)
            ))
        ) (progn
            (setq ls2 (arlst (list (nth 1 p1) (nth 1 p3) (nth 1 p5) )))
            ;(princ ls2)
            (if (= (nth 1 p1) (nth 0 ls2)) (progn
                (setq p7 (list (nth 2 ls1) (nth 1 ls2) 0))

                (setq p8 (list (- (nth 2 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) ))))
                                (- (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                ;(princ p8)
                (drawrt1 p8 p7)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p8) (nth 1 p7) 0))
                (setq p9 (list (nth 1 ls1) (nth 1 ls2) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p9 p8)
                (setvar "cmdecho" 1)
            )(progn
                (setq ls2 (arlst (list (nth 1 p3) (nth 1 p5) )))
                (setq p7 (list (nth 2 ls1) (nth 1 ls2) 0))

                (setq p8 (list (- (nth 2 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) ))))
                                (+ (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
;                (princ p7)
;                (princ p8)
                (drawrt1 p7 p8)    
                (setq p9 p7) 
                (setq p7 (list (nth 0 p8) (nth 1 p7) 0))
                (setq p9 (list (nth 1 ls1) (nth 1 ls2) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p8 p9)
                (setvar "cmdecho" 1)
                
            ))

        ))
        (setvar "osmode" getom)
        (setvar "cmdecho" 0)
        (command "erase" data_m "")
        (setvar "cmdecho" 1)
    ))



    (princ) 
)
    (setvar "cmdecho" 0) (command "undo" "mark") (setvar "cmdecho" 1)
    (if (= 0 check) (princ "\ninvalid data") (progn
        (if (< (abs (- (nth 0 (cdr (assoc 10 l1)))
                       (nth 0 (cdr (assoc 11 l1))) )) 0.00001) (mkv))
        (if (< (abs (- (nth 1 (cdr (assoc 10 l1)))
                       (nth 1 (cdr (assoc 11 l1))) )) 0.00001) (mkh))
        

    ))
    
    (princ)
)

    (dw_import)
  	(ai_undo_push)
    (dw_procced)
  	(ai_undo_pop)
    
)


(defun c:w1(/ data_m l1 l2 p1 p2 check)

(defun wd_import(/ p3 p4 p5 p6)
    (setq data_m (ssget))
    (setq p1 (getpoint "\nfirst point :") p2 (getpoint "\nsecond point :"))
    (setq l1 nil l2 nil check 1)
    (if (not (= nil data_m)) (progn
        (setq l1 (entget (ssname data_m 0)))
        (setq l2 (entget (ssname data_m 1)))
        (if (or (= nil l1) (not (= "LINE" (cdr (assoc 0 l1))))) (setq check 0))
        (if (or (= nil l2) (not (= "LINE" (cdr (assoc 0 l2))))) (setq check 0))
        (if (not (= 0 (-(sslength data_m) 2))) (setq check 0))
        (if (= 1 check) (progn
            (setq p3 (cdr (assoc 10 l1))) (setq p3 (list (nth 0 p3) (nth 1 p3)))
            (setq p4 (cdr (assoc 11 l1))) (setq p4 (list (nth 0 p4) (nth 1 p4)))
            (setq p5 (cdr (assoc 10 l2))) (setq p5 (list (nth 0 p5) (nth 1 p5)))
            (setq p6 (cdr (assoc 11 l2))) (setq p6 (list (nth 0 p6) (nth 1 p6)))
            (if (not (= nil (inters p3 p4 p5 p6 nil))) (setq check 0))
        ))
    ) (setq check 0))
    (princ)
)

(defun wd_procced()

(defun mkv(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom ll1)

    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 
    (if (> (abs (- (nth 1 p1) (nth 1 p3)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p1) (nth 1 p4)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p5)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p6)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (= 0 check) (princ "\ninvalid data") (progn
        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p3) (nth 1 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p3) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p5) (nth 1 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p5) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p5) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 0 p3) (nth 1 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 2 ls1) 0))
        (mkline p7 p8 l1)

        (setq getom (getvar "osmode"))
        (setvar "osmode" 0)

        (setq ls2 (arlst (list (nth 0 p3) (nth 0 p5))))
        (setq p7 (list (nth 0 ls2) (nth 1 ls1) 0))
        (setq p8 (list (nth 1 ls2) (nth 2 ls1) 0))

        (setq ll1 (list
            (cons 0 "line")
            (cons 8 (getvar "clayer"))
        ))
        
        (drawrt2 p7 p8 ll1)

        (setvar "osmode" getom)
        (command "erase" data_m "")

    ))
    (princ)
)

(defun mkh(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom ll1)

    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 

    (if (> (abs (- (nth 0 p1) (nth 0 p3)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p1) (nth 0 p4)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p5)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p6)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))

    (if (= 0 check) (princ "\ninvalid data") (progn
        
        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p3) (nth 0 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p5) (nth 0 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 1 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 2 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

        (setq getom (getvar "osmode"))
        (setvar "osmode" 0)

        (setq ls2 (arlst (list (nth 1 p3) (nth 1 p5))))
        (setq p7 (list (nth 1 ls1) (nth 0 ls2) 0))
        (setq p8 (list (nth 2 ls1) (nth 1 ls2) 0))

        (setq ll1 (list
            (cons 0 "line")
            (cons 8 (getvar "clayer"))
        ))

        (drawrt3 p7 p8 ll1)

        (setvar "osmode" getom)
        (command "erase" data_m "")


    ))
    (princ)
)

    (setvar "cmdecho" 0) (command "undo" "mark") (setvar "cmdecho" 1)
    (if (= 0 check) (princ "\ninvalid data") (progn
        (if (< (abs (- (nth 0 (cdr (assoc 10 l1)))
                       (nth 0 (cdr (assoc 11 l1))) )) 0.00001) (mkv))
        (if (< (abs (- (nth 1 (cdr (assoc 10 l1)))
                       (nth 1 (cdr (assoc 11 l1))) )) 0.00001) (mkh))
        

    ))
    
    (princ)
    
)
    (wd_import)
  	(ai_undo_push)
    (wd_procced)
  	(ai_undo_pop)	
)


(defun c:w2(/ data_m l1 l2 p1 p2 check)

(defun wd_import(/ p3 p4 p5 p6)
    (setq data_m (ssget))
    (setq p1 (getpoint "\nfirst point :") p2 (getpoint "\nsecond point :"))
    (setq l1 nil l2 nil check 1)
    (if (not (= nil data_m)) (progn
        (setq l1 (entget (ssname data_m 0)))
        (setq l2 (entget (ssname data_m 1)))
        (if (or (= nil l1) (not (= "LINE" (cdr (assoc 0 l1))))) (setq check 0))
        (if (or (= nil l2) (not (= "LINE" (cdr (assoc 0 l2))))) (setq check 0))
        (if (not (= 0 (-(sslength data_m) 2))) (setq check 0))
        (if (= 1 check) (progn
            (setq p3 (cdr (assoc 10 l1))) (setq p3 (list (nth 0 p3) (nth 1 p3)))
            (setq p4 (cdr (assoc 11 l1))) (setq p4 (list (nth 0 p4) (nth 1 p4)))
            (setq p5 (cdr (assoc 10 l2))) (setq p5 (list (nth 0 p5) (nth 1 p5)))
            (setq p6 (cdr (assoc 11 l2))) (setq p6 (list (nth 0 p6) (nth 1 p6)))
            (if (not (= nil (inters p3 p4 p5 p6 nil))) (setq check 0))
        ))
    ) (setq check 0))
    (princ)
)

(defun wd_procced()

(defun mkv(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom ll1)

    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 
    (if (> (abs (- (nth 1 p1) (nth 1 p3)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p1) (nth 1 p4)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p5)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p6)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (= 0 check) (princ "\ninvalid data") (progn
        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p3) (nth 1 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p3) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p5) (nth 1 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p5) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p5) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 0 p3) (nth 1 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 2 ls1) 0))
        (mkline p7 p8 l1)

        (setq ls2 (arlst (list (nth 0 p3) (nth 0 p5))))
        
        (setq ll1 (list
            (cons 0 "line")
            (cons 8 (getvar "clayer"))
        ))
		
		(if (< (nth 0 p1) (nth 0 ls2)) (progn
        	(setq p7 (list (nth 0 ls2) (nth 1 ls1) 0))
			(setq p8 (list (nth 1 ls2) (nth 2 ls1) 0))
			(setq p7 (list (* (+ (nth 0 p7) (nth 0 p8)) 0.5) (nth 1 p7) 0))
        	(drawrt2 p7 p8 ll1)

			(setq p7 (list (- (nth 0 ls2) 70) (- (nth 1 ls1) 100) 0))
			(setq p8 (list (- (nth 0 ls2) 70) (+ (nth 2 ls1) 100) 0))
			(mkline p7 p8 ll1)

			(setq p9 (list (+ (nth 0 p7) 70) (nth 1 p7) 0))
			(mkline p7 p9 ll1)

			(setq p9 (list (+ (nth 0 p8) 70) (nth 1 p8) 0))
			(mkline p8 p9 ll1)
	    )(progn
		   	(setq p7 (list (nth 0 ls2) (nth 1 ls1) 0))
			(setq p8 (list (nth 1 ls2) (nth 2 ls1) 0))
			(setq p8 (list (* (+ (nth 0 p7) (nth 0 p8)) 0.5) (nth 1 p8) 0))
        	(drawrt2 p7 p8 ll1)

			(setq p7 (list (+ (nth 1 ls2) 70) (- (nth 1 ls1) 100) 0))
			(setq p8 (list (+ (nth 1 ls2) 70) (+ (nth 2 ls1) 100) 0))
			(mkline p7 p8 ll1)

			(setq p9 (list (- (nth 0 p7) 70) (nth 1 p7) 0))
			(mkline p7 p9 ll1)

			(setq p9 (list (- (nth 0 p8) 70) (nth 1 p8) 0))
			(mkline p8 p9 ll1)
		))

        
        (command "erase" data_m "")

    ))
    (princ)
)

(defun mkh(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom ll1)

    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 

    (if (> (abs (- (nth 0 p1) (nth 0 p3)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p1) (nth 0 p4)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p5)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p6)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))

    (if (= 0 check) (princ "\ninvalid data") (progn
        
        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p3) (nth 0 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p5) (nth 0 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 1 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 2 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

		(setq getom (getvar "osmode"))
        (setvar "osmode" 0)
		
		(setq ll1 (list
	            (cons 0 "line")
	            (cons 8 (getvar "clayer"))
	        ))
		(setq ls2 (arlst (list (nth 1 p3) (nth 1 p5))))
		;(princ ls2)
		
		(if (> (nth 1 p1) (nth 1 ls2)) (progn

        	(setq p7 (list (nth 1 ls1) (nth 0 ls2) 0))
        	(setq p8 (list (nth 2 ls1) (nth 1 ls2) 0))
			(setq p8 (list (nth 2 ls1) (* (+ (nth 1 p7) (nth 1 p8)) 0.5) 0))
		
        	(drawrt3 p7 p8 ll1)

			(setq p7 (list (- (nth 1 ls1) 100) (+ (nth 1 ls2) 70) 0))
			(setq p8 (list (+ (nth 2 ls1) 100) (+ (nth 1 ls2) 70) 0))
			(mkline p7 p8 ll1)

			(setq p9 (list (nth 0 p7) (- (nth 1 p7) 70) 0))
			(mkline p7 p9 ll1)

			(setq p9 (list (nth 0 p8) (- (nth 1 p8) 70) 0))
			(mkline p8 p9 ll1)
		)(progn
		  	(setq p7 (list (nth 1 ls1) (nth 0 ls2) 0))
        	(setq p8 (list (nth 2 ls1) (nth 1 ls2) 0))
			(setq p7 (list (nth 1 ls1) (* (+ (nth 1 p7) (nth 1 p8)) 0.5) 0))
			
			(drawrt3 p7 p8 ll1)
			

			(setq p7 (list (- (nth 1 ls1) 100) (- (nth 0 ls2) 70) 0))
			(setq p8 (list (+ (nth 2 ls1) 100) (- (nth 0 ls2) 70) 0))
			(mkline p7 p8 ll1)

			(setq p9 (list (nth 0 p7) (+ (nth 1 p7) 70) 0))
			(mkline p7 p9 ll1)

			(setq p9 (list (nth 0 p8) (+ (nth 1 p8) 70) 0))
			(mkline p8 p9 ll1)
			
			
		))
		
		(setvar "osmode" getom)
	
        (command "erase" data_m "")


    ))
    (princ)
)

    (setvar "cmdecho" 0) (command "undo" "mark") (setvar "cmdecho" 1)
    (if (= 0 check) (princ "\ninvalid data") (progn
        (if (< (abs (- (nth 0 (cdr (assoc 10 l1)))
                       (nth 0 (cdr (assoc 11 l1))) )) 0.00001) (mkv))
        (if (< (abs (- (nth 1 (cdr (assoc 10 l1)))
                       (nth 1 (cdr (assoc 11 l1))) )) 0.00001) (mkh))
        

    ))
    
    (princ)
    
)
    (wd_import)
    (wd_procced)
)

;======= AUTO DIM (h1,v1,h2,v2) =======

(defun c:h1(/ data_m ls1 p1 p2)

(defun import(/ i ent p3 p4)

(defun putnum(/ j k l ls3)
;    (princ p3)
    (if (= nil ls1) (setq ls1 (list (nth 0 p3))) (progn
        (setq ls3 '() j 0 k 0)
        (while (and (< j (length ls1)) (= k 0)) (progn
            (if (< (nth 0 p3) (nth j ls1)) (setq k 1) (progn
                (if (= nil ls3) (setq ls3 (list (nth j ls1)) )
                    (setq ls3 (append ls3 (list (nth j ls1)))))
                (setq j (+ j 1))
;                (princ *x*)
            ))
        ))
;        (princ j)
        (if (= nil ls3) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 0 p3) (nth j ls1)))) )
            (if (or (= nil k) (> k 0.0001))
            (setq ls3 (list (nth 0 p3)))
;            (princ "a")
        )) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 0 p3) (nth j ls1)))) )
            (if (> j 0) 
            (setq l (abs (- (nth 0 p3) (nth (- j 1) ls1)))) ) 
;            (princ k) (princ l)
            (if (and (or (= nil k) (> k 0.0001)) (or (= nil l) (> l 0.0001))) 
            (setq ls3 (append ls3 (list (nth 0 p3)))) )
;            (princ "b")
        ))
;        (princ ls3)
        (while (< j (length ls1)) (progn
            (setq ls3 (append ls3 (list (nth j ls1))))
            (setq j (+ j 1))
;            (princ ls3)
        ))
        (setq ls1 ls3)
    ))
;    (princ ls1)
    (princ)
)

    (setq data_m (ssget) ls1 '())
    (setq p1 (getpoint "\nfirst point") p2 (getpoint "\nsecond point"))

    (setq i 0)
    (while (< i (sslength data_m)) (progn
        (setq ent (entget (ssname data_m i)))
        ;(princ ent)
        (if (= "LINE" (cdr (assoc 0 ent))) (progn
            (setq p3 (cdr (assoc 10 ent))) (putnum)
            (setq p3 (cdr (assoc 11 ent))) (putnum)
            ;(princ p3)
            ;(princ p4)
            ;(if (and (>  0.00001 (- (nth 0 p3) (nth 0 p4)))
            ;         (< -0.00001 (- (nth 0 p3) (nth 0 p4)))) (putnum))
        ))
        (setq i (+ i 1))
    ))
    (princ)
)

(defun procced(/ p3 p4 p5 i omd)
  	(setq omd (getvar "osmode"))
  	(setvar "osmode" 0)
  	(setvar "cmdecho" 0)
    (if (> (length ls1) 1) (progn
        (setq i 0)
        (command "dim")
        (while (< i (- (length ls1) 1)) (progn
            (setq p3 (list (nth i ls1) (nth 1 p1) (nth 2 p1) ))
            (setq p4 (list (nth (+ i 1) ls1) (nth 1 p1) (nth 2 p1)))
            (setq p5 (list (nth 0 p1) (nth 1 p2) (nth 2 p1)) )
            (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 0.0001)
            (command "hor" p3 p4 p5 ""))
            (setq i (+ i 1))

        ))
        (command "exit")
    ))
  	(setvar "osmode" omd)
  	(setvar "cmdecho" 1)
    (princ)
)

    (import)
  	(ai_undo_push)
    (procced)
    (ai_undo_pop)
  	(princ)
)

(defun c:v1(/ data_m ls1 p1 p2 omd)

(defun import(/ i ent p3 p4)

(defun putnum(/ j k l ls3)
;    (princ p3)
    (if (= nil ls1) (setq ls1 (list (nth 1 p3))) (progn
        (setq ls3 '() j 0 k 0)
        (while (and (< j (length ls1)) (= k 0)) (progn
            (if (< (nth 1 p3) (nth j ls1)) (setq k 1) (progn
                (if (= nil ls3) (setq ls3 (list (nth j ls1)) )
                    (setq ls3 (append ls3 (list (nth j ls1)))))
                (setq j (+ j 1))
;                (princ *x*)
            ))
        ))
;        (princ j)
        (if (= nil ls3) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 1 p3) (nth j ls1)))) )
            (if (or (= nil k) (> k 0.0001))
            (setq ls3 (list (nth 1 p3)))
;            (princ "a")
        )) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 1 p3) (nth j ls1)))) )
            (if (> j 0) 
            (setq l (abs (- (nth 1 p3) (nth (- j 1) ls1)))) ) 
;            (princ k) (princ l)
            (if (and (or (= nil k) (> k 0.0001)) (or (= nil l) (> l 0.0001))) 
            (setq ls3 (append ls3 (list (nth 1 p3)))) )
;            (princ "b")
        ))
;        (princ ls3)
        (while (< j (length ls1)) (progn
            (setq ls3 (append ls3 (list (nth j ls1))))
            (setq j (+ j 1))
;            (princ ls3)
        ))
        (setq ls1 ls3)
    ))
;    (princ ls1)
    (princ)
)

    (setq data_m (ssget) ls1 '())
    (setq p1 (getpoint "\nfirst point") p2 (getpoint "\nsecond point"))

    (setq i 0)
    (while (< i (sslength data_m)) (progn
        (setq ent (entget (ssname data_m i)))
        ;(princ ent)
        (if (= "LINE" (cdr (assoc 0 ent))) (progn
            (setq p3 (cdr (assoc 10 ent))) (putnum)
            (setq p3 (cdr (assoc 11 ent))) (putnum)
            ;(princ p3)
            ;(princ p4)
            ;(if (and (>  0.00001 (- (nth 1 p3) (nth 1 p4)))
            ;         (< -0.00001 (- (nth 1 p3) (nth 1 p4)))) (putnum))
        ))
        (setq i (+ i 1))
    ))
    (princ)
)

(defun procced(/ p3 p4 p5 i)
    (if (> (length ls1) 1) (progn
        (setq i 0)
        (command "dim")
        (while (< i (- (length ls1) 1)) (progn
            (setq p3 (list (nth 0 p1) (nth i ls1) 0 ))
            (setq p4 (list (nth 0 p1) (nth (+ i 1) ls1) 0 ))
            (setq p5 (list (nth 0 p2) (nth 1 p2) (nth 2 p1)) )
            (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 0.0001)
            (command "ver" p3 p4 p5 ""))
            (setq i (+ i 1))

        ))
        (command "exit")
    ))
    (princ)
)

    (import)
  	(ai_undo_push)
    (setq omd (getvar "osmode"))
    (setvar "osmode" 0)
    (procced)
    (setvar "osmode" omd)
  	(ai_undo_pop)
    (princ)
)




(defun c:h2(/ data_m ls1 p1 omd)

(defun import(/ i ent p3 p4)

(defun putnum(/ j k l ls3)
;    (princ p3)
    (if (= nil ls1) (setq ls1 (list (nth 0 p3))) (progn
        (setq ls3 '() j 0 k 0)
        (while (and (< j (length ls1)) (= k 0)) (progn
            (if (< (nth 0 p3) (nth j ls1)) (setq k 1) (progn
                (if (= nil ls3) (setq ls3 (list (nth j ls1)) )
                    (setq ls3 (append ls3 (list (nth j ls1)))))
                (setq j (+ j 1))
;                (princ *x*)
            ))
        ))
;        (princ j)
        (if (= nil ls3) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 0 p3) (nth j ls1)))) )
            (if (or (= nil k) (> k 0.0001))
            (setq ls3 (list (nth 0 p3)))
;            (princ "a")
        )) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 0 p3) (nth j ls1)))) )
            (if (> j 0) 
            (setq l (abs (- (nth 0 p3) (nth (- j 1) ls1)))) ) 
;            (princ k) (princ l)
            (if (and (or (= nil k) (> k 0.0001)) (or (= nil l) (> l 0.0001))) 
            (setq ls3 (append ls3 (list (nth 0 p3)))) )
;            (princ "b")
        ))
;        (princ ls3)
        (while (< j (length ls1)) (progn
            (setq ls3 (append ls3 (list (nth j ls1))))
            (setq j (+ j 1))
;            (princ ls3)
        ))
        (setq ls1 ls3)
    ))
;    (princ ls1)
    (princ)
)

    (setq data_m (ssget) ls1 '())
    (setq p1 (getpoint "\nPick point"))

    (setq i 0)
    (while (< i (sslength data_m)) (progn
        (setq ent (entget (ssname data_m i)))
        ;(princ ent)
        (if (= "LINE" (cdr (assoc 0 ent))) (progn
            (setq p3 (cdr (assoc 10 ent)))
            (setq p4 (cdr (assoc 11 ent)))
            (if (and (>  0.00001 (- (nth 0 p3) (nth 0 p4)))
                     (< -0.00001 (- (nth 0 p3) (nth 0 p4)))) (putnum))
        ))
        (setq i (+ i 1))
    ))
    (princ)
)

(defun procced(/ s1)

(defun putnumber(/ i p2)
;    (princ ls1)
    (if (= nil startnb) (setq startnb 1))
    (setq i (getint "\nEnter first number : "))
    (if (not (= nil i)) (setq startnb i))
    (setq i 0)
    (while (< i (- (length ls1) 1)) (progn
        (setq p2 (list (nth i ls1) (nth 1 p1) 0))
        (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 0.0001) (progn
            (command "circle" p2 300)
            (command "text" "j" "mc" p2 300 0 (itoa (+ i startnb)))
        ))
        (setq i (+ i 1))
    ))
    (if (< i (length ls1)) (progn
        (setq p2 (list (nth i ls1) (nth 1 p1) 0))
        (command "circle" p2 300)
        (command "text" "j" "mc" p2 300 0 (itoa (+ i startnb)))

    ))
    (princ)
)

(defun putchar(/ i)
;    (princ ls1)
    (if (= nil startnb) (setq startnb 0))
    (setq i "asd")
    (while (or (= nil i) (or (= " " i) (< 1 (strlen i)) ) ) 
        (setq i (getstring "\nEnter charater : ")))
    (if (not (= nil i)) (setq startnb (ascii (strcase i))))
    (setq i 0)
    (while (< i (- (length ls1) 1)) (progn
        (setq p2 (list (nth i ls1) (nth 1 p1) 0))
        (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 0.0001) (progn
            (command "circle" p2 300)
            (command "text" "j" "mc" p2 300 0 (chr (+ i startnb)))
        ))
        (setq i (+ i 1))
    ))
    (if (< i (length ls1)) (progn
        (setq p2 (list (nth i ls1) (nth 1 p1) 0))
        (command "circle" p2 300)
        (command "text" "j" "mc" p2 300 0 (chr (+ i startnb)))
    ))
    (princ)
)

    (if (> (length ls1) 0) (progn
        (initget 1 "Number Charater N C n c")
        (setq s1 (getkword "\nNumber/Charater : "))
        (if (not (= nil s1)) (progn
            (if (or (= (strcase s1) "NUMBER") (= (strcase s1) "N") ) 
            (putnumber) (putchar))
        ))
    ))
)

    (import)
    
    (setq omd (getvar "osmode"))
    (setvar "osmode" 0)
  	(ai_undo_push)
    (procced)
  	(ai_undo_pop)
    (setvar "osmode" omd)
    
    (princ)
)



(defun c:v2(/ data_m ls1 p1 omd)

(defun import(/ i ent p3 p4)

(defun putnum(/ j k l ls3)
;    (princ p3)
    (if (= nil ls1) (setq ls1 (list (nth 1 p3))) (progn
        (setq ls3 '() j 0 k 0)
        (while (and (< j (length ls1)) (= k 0)) (progn
            (if (< (nth 1 p3) (nth j ls1)) (setq k 1) (progn
                (if (= nil ls3) (setq ls3 (list (nth j ls1)) )
                    (setq ls3 (append ls3 (list (nth j ls1)))))
                (setq j (+ j 1))
;                (princ *x*)
            ))
        ))
;        (princ j)
        (if (= nil ls3) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 1 p3) (nth j ls1)))) )
            (if (or (= nil k) (> k 0.0001))
            (setq ls3 (list (nth 1 p3)))
;            (princ "a")
        )) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 1 p3) (nth j ls1)))) )
            (if (> j 0) 
            (setq l (abs (- (nth 1 p3) (nth (- j 1) ls1)))) ) 
;            (princ k) (princ l)
            (if (and (or (= nil k) (> k 0.0001)) (or (= nil l) (> l 0.0001))) 
            (setq ls3 (append ls3 (list (nth 1 p3)))) )
;            (princ "b")
        ))
;        (princ ls3)
        (while (< j (length ls1)) (progn
            (setq ls3 (append ls3 (list (nth j ls1))))
            (setq j (+ j 1))
;            (princ ls3)
        ))
        (setq ls1 ls3)
    ))
;    (princ ls1)
    (princ)
)

    (setq data_m (ssget) ls1 '())
    (setq p1 (getpoint "\nPick point"))

    (setq i 0)
    (while (< i (sslength data_m)) (progn
        (setq ent (entget (ssname data_m i)))
        ;(princ ent)
        (if (= "LINE" (cdr (assoc 0 ent))) (progn
            (setq p3 (cdr (assoc 10 ent)))
            (setq p4 (cdr (assoc 11 ent)))
            (if (and (>  0.00001 (- (nth 1 p3) (nth 1 p4)))
                     (< -0.00001 (- (nth 1 p3) (nth 1 p4)))) (putnum))
        ))
        (setq i (+ i 1))
    ))
    (princ)
)

(defun procced(/ s1)

(defun putnumber(/ i p2)
;    (princ ls1)
    (if (= nil startnb) (setq startnb 1))
    (setq i (getint "\nEnter first number : "))
    (if (not (= nil i)) (setq startnb i))
    (setq i 0)
    (while (< i (- (length ls1) 1)) (progn
        (setq p2 (list (nth 0 p1) (nth i ls1) 0))
        (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 0.0001) (progn
            (command "circle" p2 300)
            (command "text" "j" "mc" p2 300 0 (itoa (+ i startnb)))
        ))
        (setq i (+ i 1))
    ))
    (if (< i (length ls1)) (progn
        (setq p2 (list (nth 0 p1) (nth i ls1) 0))
        (command "circle" p2 300)
        (command "text" "j" "mc" p2 300 0 (itoa (+ i startnb)))

    ))
    (princ)
)

(defun putchar(/ i)
;    (princ ls1)
    (if (= nil startnb) (setq startnb 0))
    (setq i "asd")
    (while (or (= nil i) (or (= " " i) (< 1 (strlen i)) ) ) 
        (setq i (getstring "\nEnter charater : ")))
    (if (not (= nil i)) (setq startnb (ascii (strcase i))))
    (setq i 0)
    (while (< i (- (length ls1) 1)) (progn
        (setq p2 (list (nth 0 p1) (nth i ls1) 0))
        (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 0.0001) (progn
            (command "circle" p2 300)
            (command "text" "j" "mc" p2 300 0 (chr (+ i startnb)))
        ))
        (setq i (+ i 1))
    ))
    (if (< i (length ls1)) (progn
        (setq p2 (list (nth 0 p1) (nth i ls1) 0))
        (command "circle" p2 300)
        (command "text" "j" "mc" p2 300 0 (chr (+ i startnb)))

    ))
    (princ)
)

    (if (> (length ls1) 0) (progn
        (initget 1 "Number Charater N C n c")
        (setq s1 (getkword "\nNumber/Charater : "))
        (if (not (= nil s1)) (progn
            (if (or (= (strcase s1) "NUMBER") (= (strcase s1) "N") ) 
            (putnumber) (putchar))
        ))
    ))
)

    (import)
  	(ai_undo_push)
    (setq omd (getvar "osmode"))
    (setvar "osmode" 0)
    (procced)
    (setvar "osmode" omd)
  	(ai_undo_pop)
    (princ)
)


;====== OO (oo) =======

(defun c:oo(/ data_m)

(defun import_data(/ i)
    (setq data_m (ssget))
    (if (= nil distan_m) (setq distan_m 110.0))
    (princ "Distance (")
    (princ distan_m)
    (princ "):")
    (setq i (getreal ))
    (if (not (= nil i)) (setq distan_m i))
)

(defun process(/ ent check)

(defun p_check()
    (setq check 0)
    (if (= "LINE" (cdr (assoc 0 ent))) (setq check 1))
    (princ)
)

(defun p_d_offset(/ p1 p2 p3 p4)

(defun makeline(/ e2 e5)
;    (princ ent)
;    (setq e5 nil)
;    (setq e5 (cdr (assoc 5 ent)))
;    (princ e5)
;    (if (= nil e5) (setq e5 ))

    (setq la (list (cons 0 "LINE")
        (cons 5 (cdr (assoc 5 ent)) )
        (cons 8 (cdr (assoc 8 ent)) )
        (cons 10 p3)
        (cons 11 p4)
    ))
;    (princ la)
    (entmake la)
    (princ)
)

    (setq p1 (cdr (assoc 10 ent)) p2 (cdr (assoc 11 ent)) )
    (if (not (= p1 p2)) (progn
        (if (< (abs (- (nth 0 p1) (nth 0 p2))) 0.000001) (progn
            (setq p3 (list (+ (nth 0 p1) distan_m) (nth 1 p1) (nth 2 p1) ) )
            (setq p4 (list (+ (nth 0 p2) distan_m) (nth 1 p2) (nth 2 p2) ) )
            (makeline)
            (setq p3 (list (- (nth 0 p1) distan_m) (nth 1 p1) (nth 2 p1) ) )
            (setq p4 (list (- (nth 0 p2) distan_m) (nth 1 p2) (nth 2 p2) ) )
            (makeline)
        ))
        (if (< (abs (- (nth 1 p1) (nth 1 p2))) 0.000001) (progn
            (setq p3 (list (nth 0 p1) (+ (nth 1 p1) distan_m) (nth 2 p1) ) )
            (setq p4 (list (nth 0 p2) (+ (nth 1 p2) distan_m) (nth 2 p2) ) )
            (makeline)
            (setq p3 (list (nth 0 p1) (- (nth 1 p1) distan_m) (nth 2 p1) ) )
            (setq p4 (list (nth 0 p2) (- (nth 1 p2) distan_m) (nth 2 p2) ) )
            (makeline)
        ))

    ))
    (princ)
)

    (if (not (= nil data_m)) (progn
        (setq i 0)
        (while (< i (sslength data_m)) (progn
            (setq ent (entget (ssname data_m i)))
            (p_check)
            (if (= 1 check) (p_d_offset))
            (setq i (+ i 1))
        ))
    ))
    (princ)
)
    (import_data)
  	(ai_undo_push)
    (process)
  	(ai_undo_pop)
    (princ)
)

 CHW.LSP

(defun echff ()
       (setq scmde (getvar "cmdecho"))
       (setvar "cmdecho" 0)
)
(defun echrs ()
       (setvar "cmdecho" scmde)
)

(defun chwerr (s)
   (if (/= s "Function cancelled")   ; If an error (such as CTRL-C) occurs
      (princ (strcat "\nError: " s)) ; while this command is active...
   )
   (echrs)
   (setq p nil)                      ; Free selection set
   (setq *error* olderr)             ; Restore old *error* handler
   (princ)
)

(defun C:CHW (/ p rw w n entx ename entg pt r olderr)
   (setq olderr  *error*             ; Initialize variables
         *error* chwerr)

       (initget 1)
       (setq p (ssget))
       (setq n 0 entx (ssname p n))
       (if (null w$$)
           (progn
                 (initget 1)
                 (prompt "\nNew width: ")
           )
           (progn
                 (prompt "\nNew width /<")
                 (princ (strcat w$$ ">: "))
           )
       )
       (setq rw (getstring))
       (if (and (= rw "") (/= nil w$$))
           (setq rw w$$)
           (setq w$$ rw)
       )
       (setq w (abs (atof rw)))
       (echff)
       (repeat (sslength p)
               (setq entg (entget entx))
               (setq ename (cdr (assoc 0 entg)))
               (cond ((= ename "POLYLINE")
                      (command "pedit" entx "w" w ""))
                     ((member ename '("LINE" "ARC"))
                      (command "pedit" entx "" "w" w ""))
                     ((= ename "CIRCLE")
                      (setq pt (cdr (assoc 10 entg)))
                      (setq r (cdr (assoc 40 entg)))
                      (command "DONUT" (- (* 2.0 r) w)
                                       (+ (* 2.0 r) w) pt ""
                      )
                      (entdel entx)
                     )
                     (T nil)
               )
               (setq n (1+ n) entx (ssname p n))
       )
       (echrs)
       (setq *error* olderr)             ; Restore old *error* handler
       (prin1)
)

(defun c:zp ()  (command "'.zoom" "p"))
(defun c:ze ()  (command "'.zoom" "e"))
(defun c:za ()  (command "'.zoom" "a"))
(defun c:zd ()  (command "'.zoom" "d"))
(defun c:zv ()  (command "'.zoom" "v"))
(defun c:pu ()  (command "purge" "a"))

;*******************************************************************************
(DEFUN C:CD (/ CMD SS LTH DEM PT DS KDL N70 GOCX GOCY PT13 PT14 PTI PT13I PT14I
                PT13N PT14N O13 O14 N13 N14 OSM OLDERR PT10 PT11)
(SETQ CMD (GETVAR "CMDECHO"))
(SETQ OSM (GETVAR "OSMODE"))
(SETQ OLDERR *error*
      *error* myerror)
(PRINC "Please select dimension object!")
(SETQ SS (SSGET))
(SETVAR "CMDECHO" 0)
(SETQ PT (GETPOINT "Point to trim or extend:"))
(SETQ PT (TRANS PT 1 0))
(COMMAND "UCS" "W")
(SETQ LTH (SSLENGTH SS))
(SETQ DEM 0)
(WHILE (< DEM LTH)
    (PROGN
	(SETQ DS (ENTGET (SSNAME SS DEM)))
	(SETQ KDL (CDR (ASSOC 0 DS)))
	(IF (= "DIMENSION" KDL)
	   (PROGN
		(SETQ PT10 (CDR (ASSOC 10 DS)))
		(SETQ PT11 (CDR (ASSOC 11 DS)))
		(SETQ PT13 (CDR (ASSOC 13 DS)))
		(SETQ PT14 (CDR (ASSOC 14 DS)))
		(SETQ N70 (CDR (ASSOC 70 DS)))
		(IF (OR (= N70 32) (= N70 33) (= N70 160) (= N70 161))
		   (PROGN
			(SETQ GOCY (ANGLE PT10 PT14))
			(SETQ GOCX (+ GOCY (/ PI 2)))
		   )
		)
		(SETVAR "OSMODE" 0)
		(SETQ PTI (POLAR PT GOCX 2))
		(SETQ PT13I (POLAR PT13 GOCY 2))
		(SETQ PT14I (POLAR PT14 GOCY 2))
		(SETQ PT13N (INTERS PT PTI PT13 PT13I NIL))
		(SETQ PT14N (INTERS PT PTI PT14 PT14I NIL))
		(SETQ O13 (ASSOC 13 DS))
		(SETQ O14 (ASSOC 14 DS))
		(SETQ N13 (CONS 13 PT13N))
		(SETQ N14 (CONS 14 PT14N))
		(SETQ DS (SUBST N13 O13 DS))
		(SETQ DS (SUBST N14 O14 DS))
		(ENTMOD DS)
	   )
	)
	(SETQ DEM (+ DEM 1))
    )
)
(COMMAND "UCS" "P")
(SETVAR "CMDECHO" CMD)
(SETVAR "OSMODE" OSM)
(setq *error* OLDERR)               ; Restore old *error* handler
(PRINC)
)
;
;*******************************************************************************
;* WRITTEN BY DAO NGUYEN THANG 94X3 - HANOI ARCHITECTURAL UNIVERSITY (VIETNAM) *
;*******************************************************************************
(defun myerror (s)                    ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (cond
    ((= s "quit / exit abort") (princ))
    ((/= s "Function cancelled") (princ (strcat "\nError: " s)))
  )
  (setvar "cmdecho" CMD)             ; Restore saved modes
  (setvar "osmode" OSM)
  (setq *error* OLDERR)               ; Restore old *error* handler
  (princ)
)
;*******************************************************************************
(DEFUN C:Ct (/ CMD SS LTH DEM PT DS KDL N70 GOCX GOCY PT13 PT14 PTI PT13I PT14I
                PT13N PT14N O13 O14 N13 N14 OSM OLDERR PT10 PT11)
(SETQ CMD (GETVAR "CMDECHO"))
(SETQ OSM (GETVAR "OSMODE"))
(SETQ OLDERR *error*
      *error* myerror)
(PRINC "Please select dimension object!")
(SETQ SS (SSGET))
(SETVAR "CMDECHO" 0)
(SETQ PT (GETPOINT "Point to trim or extend:"))
(SETQ PT (TRANS PT 1 0))
(COMMAND "UCS" "W")
(SETQ LTH (SSLENGTH SS))
(SETQ DEM 0)
(WHILE (< DEM LTH)
    (PROGN
	(SETQ DS (ENTGET (SSNAME SS DEM)))
	(SETQ KDL (CDR (ASSOC 0 DS)))
	(IF (= "DIMENSION" KDL)
	   (PROGN
		(SETQ PT10 (CDR (ASSOC 10 DS)))
		(SETQ PT11 (CDR (ASSOC 11 DS)))
		(SETQ PT13 (CDR (ASSOC 13 DS)))
		(SETQ PT14 (CDR (ASSOC 14 DS)))
		(SETQ N70 (CDR (ASSOC 70 DS)))
		(IF (OR (= N70 32) (= N70 33) (= N70 160) (= N70 161))
		   (PROGN
			(SETQ GOCY (ANGLE PT10 PT14))
			(SETQ GOCX (+ GOCY (/ PI 2)))
		   )
		)
		(SETVAR "OSMODE" 0)
		(SETQ PTI (POLAR PT GOCX 2))
		(SETQ PT13I (POLAR PT13 GOCY 2))
		(SETQ PT14I (POLAR PT14 GOCY 2))
		(SETQ PT13N (INTERS PT PTI PT13 PT13I NIL))
		(SETQ PT14N (INTERS PT PTI PT14 PT14I NIL))
		(SETQ O13 (ASSOC 13 DS))
		(SETQ O14 (ASSOC 14 DS))
		(SETQ N13 (CONS 13 PT13N))
		(SETQ N14 (CONS 14 PT14N))
		(SETQ DS (SUBST N13 O13 DS))
		(SETQ DS (SUBST N14 O14 DS))
		(ENTMOD DS)
	   )
	)
	(SETQ DEM (+ DEM 1))
    )
)
(COMMAND "UCS" "P")
(SETVAR "CMDECHO" CMD)
(SETVAR "OSMODE" OSM)
(setq *error* OLDERR)               ; Restore old *error* handler
(PRINC)
)
;******************************************************************************

(DEFUN C:BD (/ CMD SS LTH DEM PT DS KDL N70 GOCX GOCY PT13 PT14 PTI
                PT10 PT10I PT10N O10 N10 PT11 PT11N O11 N11 KC OSM OLDERR)
(SETQ CMD (GETVAR "CMDECHO"))
(SETQ OSM (GETVAR "OSMODE"))
(SETQ OLDERR *error*
      *error* myerror)
(PRINC "Please select dimension object!")
(SETQ SS (SSGET))
(SETVAR "CMDECHO" 0)
(SETQ PT (GETPOINT "Point to trim or extend:"))
(SETQ PT (TRANS PT 1 0))
(COMMAND "UCS" "W")
(SETQ LTH (SSLENGTH SS))
(SETQ DEM 0)
(WHILE (< DEM LTH)
    (PROGN
	(SETQ DS (ENTGET (SSNAME SS DEM)))
	(SETQ KDL (CDR (ASSOC 0 DS)))
	(IF (= "DIMENSION" KDL)
	   (PROGN
		(SETQ PT13 (CDR (ASSOC 13 DS)))
		(SETQ PT14 (CDR (ASSOC 14 DS)))
		(SETQ PT10 (CDR (ASSOC 10 DS)))
		(SETQ PT11 (CDR (ASSOC 11 DS)))
		(SETQ N70 (CDR (ASSOC 70 DS)))
		(IF (OR (= N70 32) (= N70 33) (= N70 160) (= N70 161))
		   (PROGN
			(SETQ GOCY (ANGLE PT10 PT14))
			(SETQ GOCX (+ GOCY (/ PI 2)))
		   )
		)
		(SETVAR "OSMODE" 0)
		(SETQ PTI (POLAR PT GOCX 2))
		(SETQ PT10I (POLAR PT10 GOCY 2))
		(SETQ PT10N (INTERS PT PTI PT10 PT10I NIL))
		(SETQ KC (DISTANCE PT10 PT10N))
		(SETQ O10 (ASSOC 10 DS))
		(SETQ N10 (CONS 10 PT10N))
		(SETQ DS (SUBST N10 O10 DS))
		(SETQ PT11N (POLAR PT11 (ANGLE PT10 PT10N) KC))
		(SETQ O11 (ASSOC 11 DS))
		(SETQ N11 (CONS 11 PT11N))
		(SETQ DS (SUBST N11 O11 DS))
		(ENTMOD DS)
	   )
	)
	(SETQ DEM (+ DEM 1))
    )
)
(COMMAND "UCS" "P")
(SETVAR "CMDECHO" CMD)
(SETVAR "OSMODE" OSM)
(setq *error* OLDERR)
(PRINC)
)
;Quick Zoom
(defun c:zx ()  (Setvar "cmdecho" 0)(command "'.zoom" ".9x")(Setvar "cmdecho" 1))
(defun c:zp ()  (Setvar "cmdecho" 0)(command "'.zoom" "p")(Setvar "cmdecho" 1))
(defun c:zz ()  (Setvar "cmdecho" 0)(command "'.zoom" "1.2x")(Setvar "cmdecho" 1))

(defun c:ze ()  (Setvar "cmdecho" 0)(command "'.zoom" "e")(Setvar "cmdecho" 1))

(defun c:za ()  (Setvar "cmdecho" 0)(command "'.zoom" "a")(Setvar "cmdecho" 1))
(defun c:zc ()  (Setvar "cmdecho" 0)(command "'.zoom" "c")(Setvar "cmdecho" 1))
(defun c:zs ()  (Setvar "cmdecho" 0)(command "'.zoom" "s")(Setvar "cmdecho" 1))
(defun c:zw ()  (Setvar "cmdecho" 0)(command "'.zoom" "w")(Setvar "cmdecho" 1))
(defun c:zd ()  (Setvar "cmdecho" 0)(command "'.zoom" "d")(Setvar "cmdecho" 1))
(defun c:+++ () (Setvar "cmdecho" 0) (command "'.zoom" "0.1x")(Setvar "cmdecho" 1))
(defun c:++ () (Setvar "cmdecho" 0) (command "'.zoom" "0.3x")(Setvar "cmdecho" 1))
(defun c:+ () (Setvar "cmdecho" 0) (command "'.zoom" "0.5x")(Setvar "cmdecho" 1))
(defun c:- () (Setvar "cmdecho" 0) (command "'.zoom" "2x")(Setvar "cmdecho" 1))
(defun c:-- () (Setvar "cmdecho" 0) (command "'.zoom" "3x")(Setvar "cmdecho" 1))
(defun c:--- ()  (command "'.zoom" "4x"))

