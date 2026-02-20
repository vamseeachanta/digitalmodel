;;;     AC_BONUS.LSP
;;;     Copyright (C) 1997 by Autodesk, Inc.
;;;
;;;     Created 2/21/97 by Randy Kintzley and Dominic Panholzer
;;;                   
;;;
;;;     Permission to use, copy, modify, and distribute this software
;;;     for any purpose and without fee is hereby granted, provided
;;;     that the above copyright notice appears in all copies and 
;;;     that both that copyright notice and the limited warranty and 
;;;     restricted rights notice below appear in all supporting 
;;;     documentation.
;;;
;;;     AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.  
;;;     AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF 
;;;     MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC. 
;;;     DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE 
;;;     UNINTERRUPTED OR ERROR FREE.
;;;
;;;     Use, duplication, or disclosure by the U.S. Government is subject to 
;;;     restrictions set forth in FAR 52.227-19 (Commercial Computer 
;;;     Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;     (Rights in Technical Data and Computer Software), as applicable. 
;;;
;;;   ----------------------------------------------------------------
;;;
;;;    This file is called from ACADR14.LSP. It defines autoloading functions
;;;    as well as library functions used by AutoCAD bonus lisp routines.
;;;


; ------------------------- GLOBAL INFO --------------------------
; Functions created as result of loading file: ac_bonus.lsp
; ----------------------------------------------------------------
; B_LAYER_LOCKED
; B_RESTORE_SYSVARS
; B_RESTORE_UNDO
; B_SET_SYSVARS
; BONUS_ERROR
; C:ALIASEDIT
; C:ARCTEXT
; C:ASCPOINT
; C:ATEXT
; C:ATTLST
; C:BEXTEND
; C:BLKLST
; C:BLKTBL
; C:BLOCK?
; C:BONUSMENU
; C:BTRIM
; C:BURST
; C:CATTL
; C:CHGTEXT
; C:CHT
; C:CLIPIT
; C:CONVERTPLINES
; C:COUNT
; C:CROSSREF
; C:DATE
; C:DIMEX
; C:DIMIM
; C:EXCHPROP
; C:EXTRIM
; C:FIND
; C:FLIPEM
; C:GATTE
; C:GETSEL
; C:KILLSVP
; C:LAYCUR
; C:LAYFRZ
; C:LAYISO
; C:LAYLCK
; C:LAYMCH
; C:LAYOFF
; C:LAYON
; C:LAYTHW
; C:LAYULK
; C:LMAN
; C:LSP
; C:MOCORO
; C:MPEDIT
; C:MSTRETCH
; C:NCOPY
; C:PACK
; C:PQCHECK
; C:QLATTACH
; C:QLATTACHSET
; C:QLDETACHSET
; C:QLEADER
; C:REVCLOUD
; C:SSX
; C:SYSVDLG
; C:TEXTFIT
; C:TEXTMASK
; C:TXTEXP
; C:WIPEOUT
; C:XDATA
; C:XDLIST
; C:XLIST
; DELTA_ANG
; EP_LIST
; GET_ARC_POINTS
; IMAGE_BOUNDS
; INIT_BNSMENU
; INIT_BONUS_ERROR
; LSTTRANS
; M_ASSOC
; MAXMINPNT
; MPOPLST
; P_ISECT
; PIXEL_UNIT
; PL_ARC_INFO
; PL_POINT_LIST
; PLINE
; POSITION
; RESTORE_OLD_ERROR
; ROTATE_PNT
; SINGLE_SELECT
; SS_VISIBLE
; TNLIST
; UCS_2_ENT
; VIEWPNTS
; VTLIST
; ZOOM_4_SELECT
;

; ----------------------------------------------------------------
; Variables created as result of loading file: ac_bonus.lsp
; ----------------------------------------------------------------
; -

; ----------------------------------------------------------------
; Functions created as a result of executing the commands
; in: ac_bonus.lsp
; ----------------------------------------------------------------
; -

; ----------------------------------------------------------------
; Variables created as a result of executing the commands
; in: ac_bonus.lsp
; ----------------------------------------------------------------
; -


; ----------------- LAUNCHING EXTERNAL PROGRAMS ------------------
; Individual functions for running external programs
; ----------------------------------------------------------------

(defun c:aliasedit ()                         ; Alias Editor
  (startapp (findfile "alias.exe"))
  (princ)
)


; ------------------ AUTOLOADING LISP FUNCTIONS ------------------
; Individual autoload functions for bonus lisp routines
; ----------------------------------------------------------------

(autoload "xx.lsp" '("xx"))               
(autoload "x-dimex.lsp" '("x-dimex"))     
(autoload "yy.lsp" '("yy"))               
(autoload "y-dimex.lsp" '("y-dimex"))     
(autoload "LMAN.lsp" '("LMAN"))               ; Layer Manager
(autoload "EXTRIM.lsp" '("EXTRIM"))           ; Trim Around
(autoload "MSTRETCH.lsp" '("MSTRETCH"))       ; Multiple Stretch
(autoload "EXCHPROP.lsp" '("EXCHPROP"))       ; Extended ddchprop
(autoload "CLIPIT" '("CLIPIT"))               ; Extended xclip
(autoload "CHTEXT.lsp" '("CHT" "CHGTEXT"))    ; Change Text
(autoload "MPEDIT.lsp" '("MPEDIT"))           ; Multiple pline edit
(autoload "XLIST.lsp" '("XLIST" "-XLIST"))    ; List Xref Objects
(autoload "BNSLAYER.LSP" '("LAYISO" "LAYFRZ"
          "LAYOFF" "LAYLCK" "LAYULK" "LAYON"  ; Layer tools
          "LAYTHW" "LAYMCH" "LAYCUR"))
(autoload "TEXTMASK.LSP" '("TEXTMASK"))       ; Mask behind text
(autoload "TEXTFIT" '("TEXTFIT"))             ; Fit text between points
(autoload "BURST" '("BURST"))                 ; Explode attributes to text
(autoload "GATTE" '("GATTE"))                 ; Global attribute edit
(autoload "FIND" '("FIND"))                   ; Text search and replace
(autoload "REVCLOUD" '("REVCLOUD"))           ; Revision cloud
(autoload "XPLODE" '("XPLODE" "XP"))          ; Extended explode
(autoload "XDATA" '("XDATA" "XDLIST"))        ; Xdata tools
(autoload "TXTEXP" '("TXTEXP"))               ; Explode text
(autoload "TREXBLK" '("BTRIM" "BEXTEND"       ; Trim and Extend to block entities
                      "NCOPY"))               ; and copy nested entities
(autoload "GETSEL" '("GETSEL"))               ; Get selection set
(autoload "SSX" '("SSX"))                     ; Selection set filter
(autoload "ASCPOINT" '("ASCPOINT"))           ; Read points from file
(autoload "BLK_LST" '("BLKTBL" "BLKLST"
          "CATTL" "ATTLST" ))                 ; Block listing functions
(autoload "COUNT" '("COUNT"))                 ; Block counting
(autoload "BLOCKQ" '("BLOCK?"))               ; Block query
(autoload "CROSSREF" '("CROSSREF"))           ; Block definition query
(autoload "JULIAN" '("DATE"))                 ; Date function library
(autoload "PQCHECK" '("PQCHECK"))             ; LISP file checker


; ---------------------- SETFUNs for HELP ------------------------
; Individual help loading functions for bonus lisp routines
; ----------------------------------------------------------------

(setfunhelp "c:extrim" "AC_BONUS.HLP" "EXTRIM")
(setfunhelp "c:mstretch" "AC_BONUS.HLP" "MSTRETCH")
(setfunhelp "c:exchprop" "AC_BONUS.HLP" "EXCHPROP")
(setfunhelp "c:clipit" "AC_BONUS.HLP" "CLIPIT")
(setfunhelp "c:cht" "AC_BONUS.HLP" "CHT")
(setfunhelp "c:mpedit" "AC_BONUS.HLP" "MPEDIT")
(setfunhelp "c:xlist" "AC_BONUS.HLP" "XLIST")
(setfunhelp "c:-xlist" "AC_BONUS.HLP" "XLIST")
(setfunhelp "c:layiso" "AC_BONUS.HLP" "LAYISO")
(setfunhelp "c:layfrz" "AC_BONUS.HLP" "LAYFRZ")
(setfunhelp "c:layoff" "AC_BONUS.HLP" "LAYOFF")
(setfunhelp "c:laylck" "AC_BONUS.HLP" "LAYLCK")
(setfunhelp "c:layulk" "AC_BONUS.HLP" "LAYULK")
(setfunhelp "c:laymch" "AC_BONUS.HLP" "LAYMCH")
(setfunhelp "c:laycur" "AC_BONUS.HLP" "LAYCUR")
(setfunhelp "c:textmask" "AC_BONUS.HLP" "TEXTMASK")
(setfunhelp "c:textfit" "AC_BONUS.HLP" "TEXTFIT")
(setfunhelp "c:burst" "AC_BONUS.HLP" "BURST")
(setfunhelp "c:gatte" "AC_BONUS.HLP" "GATTE")
(setfunhelp "c:revcloud" "AC_BONUS.HLP" "REVCLOUD")
(setfunhelp "c:xplode" "AC_BONUS.HLP" "XPLODE")
(setfunhelp "c:xdata" "AC_BONUS.HLP" "XDATA")
(setfunhelp "c:txtexp" "AC_BONUS.HLP" "TXTEXP")
(setfunhelp "c:btrim" "AC_BONUS.HLP" "BTRIM")
(setfunhelp "c:bextend" "AC_BONUS.HLP" "BEXTEND")
(setfunhelp "c:ncopy" "AC_BONUS.HLP" "NCOPY")
(setfunhelp "c:getsel" "AC_BONUS.HLP" "GETSEL")
(setfunhelp "c:ssx" "AC_BONUS.HLP" "SSX")
(setfunhelp "c:ascpoint" "AC_BONUS.HLP" "ASCPOINT")
(setfunhelp "c:blktbl" "AC_BONUS.HLP" "BLK_LST.LSP")
(setfunhelp "c:blklst" "AC_BONUS.HLP" "BLK_LST.LSP")
(setfunhelp "c:cattl" "AC_BONUS.HLP" "BLK_LST.LSP")
(setfunhelp "c:attlst" "AC_BONUS.HLP" "BLK_LST.LSP")
(setfunhelp "c:count" "AC_BONUS.HLP" "COUNT")
(setfunhelp "c:block?" "AC_BONUS.HLP" "BLOCK63")
(setfunhelp "c:crossref" "AC_BONUS.HLP" "CROSSREF")
(setfunhelp "c:pqcheck" "AC_BONUS.HLP" "PQCHECK")


; ------------------ AUTOLOADING ARX FUNCTIONS -------------------
; Individual autoload functions for bonus ARX routines
; ----------------------------------------------------------------

(autoarxacedload "ARCTEXT.arx" '("ARCTEXT" "ATEXT"))  ; Text Along an Arc
(autoarxacedload "PACKNGO.ARX" '("PACK"))             ; Pack 'n Go Drawing Resource Packager
(autoarxacedload "PLCONVRT.arx" '("CONVERTPLINES"))   ; Convert old plines to lwplines
(autoarxacedload "WIPEOUT.arx" '("WIPEOUT"))          ; Entity masking
(autoarxacedload "DIMSIO.arx" '("DIMIM" "DIMEX"))     ; Dimstyle import and exprot
(autoarxacedload "LEADEREX.arx" '("QLEADER" "QLATTACH"
                 "QLDETACHSET" "QLATTACHSET"))        ; Leader tools
(autoarxacedload "MOCORO.arx" '("MOCORO"))            ; Move, copy, rotate, and scale
(autoarxacedload "SYSVDLG.arx" '("SYSVDLG"))          ; System variable editor


; ------------------ FIRST TIME MENU LOADING ---------------------
; This program is called from ACAD.MNL. It assures that the bonus
; menu will be loaded the first time AutoCAD is run.
; ----------------------------------------------------------------

(defun init_bnsmenu ()
  (and
    (not (= (getcfg "AppData/AC_Bonus/MenuLoad") "1"))
    (setcfg "AppData/AC_Bonus/MenuLoad" "1")
    (c:bonusmenu)
  )
)


; -------------------- MENU LOADING FUNCTION ---------------------
; Command line routine for loading bonus menu
; ----------------------------------------------------------------

(defun c:bonusmenu ()

  (init_bonus_error 
    (list
      (list "cmdecho" 0
      )

      T     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error

  (and
    (if (menucmd "Gac_bonus.id_mnbnslayr=?")
      (prompt "\nAutoCAD Bonus Menu already loaded.")
      T
    )
    (if (not (or (findfile "ac_bonus.mnu")
                 (findfile "ac_bonus.mnc")
             )
        )
      (prompt "\nAutoCAD Bonus Menu not found.")
      T
    )
    (command "_.menuload" "ac_bonus")
  )

  (restore_old_error)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;FLIPEM and KILLSVP utility commands;;;;;;;;;;;;;;;;;;;;;;
; FLIPEM.LSP
;
; Flips the ECS of all arcs with negative WCS normals.
;
(defun C:FLIPEM ( / ss n)
 (init_bonus_error (list (list "cmdecho" 0 "highlight" 0) 
                         T
                   )
 );
 (cond 
    ((not (setq ss (ssget "x" '((0 . "ARC") (210 0.0 0.0 -1.0)))))
     (princ "\nNo arcs with negative normals were found.")
    ) 
    (T 
     (setq n 0)  
     (repeat (sslength ss)
      (entmod
       (list
        (cons -1 (ssname ss n))
        '(210 0.0 0.0 1.0)
       )
      );entmod
      (setq n (1+ n))
     );repeat
     (command "_.mirror" ss "" "0,0" "0,1" "_y")
    )
 );cond
 (restore_old_error)
 (princ)
);defun c:flipem
      
;;;
;;;
;;;   C.Bethea 5 May 94
;;;
;;;   DESCRIPTION: killsvp Deletes single vertex polylines
;;;
;;;
;--- c:killsvp ----------------------------------------------
; delete single vertex polylines
;
(defun c:killsvp (/ dxf ent i ss1 hdr )

 (defun dxf (x ent) (cdr (assoc x (entget ent))))

 (setq i 0)
 (if (setq ss1 (ssget "x" '((0 . "POLYLINE"))))
     (while (< 0 (sslength ss1))
        (setq hdr (ssname ss1 0)        ; get first pline
              ent hdr
        )
        (ssdel hdr ss1)                 ; remove it from select set
        (setq ent (entnext ent))        ; 1st vertex
        (setq ent (entnext ent))        ; 2nd vertex or seqend
        (if (= "SEQEND" (dxf 0 ent))
            (progn
               (entdel hdr)             ; single vertex pline
               (setq i (1+ i))
            )
        )
     );while
 );if
 (if (> i 0)
     (princ (strcat "\nFound and killed " (itoa i) " single vertex plines."))
     (princ "\nNo single vertex polylines found.")
 );if
 (princ)
);defun c:killsvp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; --------------------- BONUS ERROR HANDLER ----------------------
; Error handler for bonus lisp routines.
; INIT_BONUS_ERROR initializes error routine. RESTORE_OLD_ERROR
; resets environment.
; ----------------------------------------------------------------
;
;INIT_BONUS_ERROR
;This routine initializes the error handler
;It should be called at the top of your lisp routine 
;
;Arguments:
;  init_err Takes a list as an argument. 
;This list can be nil or can contain the following elements:
;(list ("sysvar" value "sysvar" value ...)
;      undo_enable
;      (additional specialized clean up function)
;);list
;  The arguments explained:
;  1. - The first element of the argument list:
;       This is a list of system variables paired with
;       the values you want to set them to. 
;  2. - The second element of the list is a flag
;       If it is true, then in the event of an error 
;       the custom *error* routine will utilize UNDO 
;       as a cleanup mechanism.
;  3. - The third element is a quoted function call.
;       You pass a quoted call to the function you
;       wish to execute if an error occurs. 
;        i.e. '(my_special_stuff arg1 arg2...)
;       Use this arg if you want to do some specialized clean up 
;       things that are not already done by the standard bonus_error 
;       function.
;        
;The reason a list is provided as the argument is for upward
;compatability. Other arguments can be placed in the list at a later
;time and not affect previous versions.
;
(defun init_bonus_error ( lst / ss undo_init)
 
  ;;;;;;;local function;;;;;;;;;;;;;;;;;;;;
  (defun undo_init ( / undo_ctl)
   (b_set_sysvars (list "cmdecho" 0))
   (setq undo_ctl (getvar "undoctl")) 
   (if (equal 0 (getvar "UNDOCTL")) ;Make sure undo is fully enabled.
       (command "_.undo" "_all")
   )
   (if (or (not (equal 1 (logand 1 (getvar "UNDOCTL"))))  
           (equal 2 (logand 2 (getvar "UNDOCTL")))
       );or
       (command "_.undo" "_control" "_all") 
   )
    
   ;Ensure undo auto is off
   (if (equal 4 (logand 4 (getvar "undoctl")))
       (command "_.undo" "_Auto" "_off")
   )
   
   ;Place an end mark down if needed.
   (while (equal 8 (logand 8 (getvar "undoctl")))
        (command "_.undo" "_end")
   );while         
   (while (not (equal 8 (logand 8 (getvar "undoctl"))))
    (command "_.undo" "_begin")                 
   );while
   (b_restore_sysvars) 
   ;return original value of undoctl
   undo_ctl
  );defun undo_init

    ;;;;;;;;;;;;;begin the work of init_bonus error;;;;;;;;;;;;;
 (setq ss (ssgetfirst))
 (if (not bonus_alive)
     (setq bonus_alive 0)
 );if
 (setq bonus_alive (1+ bonus_alive))
 
 (if (and (> bonus_alive 1)                              ;do some double checking to make sure 
          (or (not (equal 'LIST (type *error*)))         ;our error handler is still active.
              (not (equal "bonus_error" (cadr *error*))) ;for nested this call.
          );or
     );and
     (progn
      (princ "\nNested Error trapping is being used incorrectly.")
      (princ "\nResetting the nested index to 1.")
      (setq     *error* bonus_error
            bonus_alive 0
      );setq
      (restore_old_error);quietly restore undo status
      (setq bonus_alive 1)
     );progn then things need to be re-adjusted.
 );if
 (if (<= bonus_alive 0)   
     (progn 
      (setq bonus_alive 0);undo settings will be restored 
                          ;along with setting *error* back to bonus_old_error.
                          ;No call to b_restore_sysvars will be made.
                          ;If it is decided, this thing should do variable clean 
                          ;up also then set bonus_alive to 1 before calling
                          ;restore_old_error
      (restore_old_error);quietly restore bonus_old_error and undo status.
      (setq bonus_alive 1)
     );progn then
 );if
 (if (= bonus_alive 1)
     (progn
      (if (and *error*
               (or (not (equal 'LIST (type *error*)))
                   (not (equal "bonus_error" (cadr *error*)))
               );or 
          );and 
          (setq bonus_old_error *error*);save the *error* only if it 
                                        ;looks like the standard one or is some other 
                                        ;user defined one. Don't want to save it if 
                                        ;it's ours because we already have it.
      );if
      (if (cadr lst)
          (setq bonus_undoctl (undo_init)) 
          (setq bonus_undoctl nil)
      );if
    );progn then this is a top level call, or in other words, the first time through.
 );if
 (b_set_sysvars (car lst))
 (if (= bonus_alive 1)
     (progn
      (setq *error* bonus_error);setq
      (if (caddr lst)
          (setq *error* (append (reverse (cdr (reverse *error*))) 
                                (list (caddr lst)
                                      (last *error*)
                                );list
                        );append
          );setq ;then add additional routine name to the error function.
      );if
     );progn
     (progn
      (if (and (> bonus_alive 1)
               (or (not (equal 'LIST (type *error*)))
                   (not (equal "bonus_error" (cadr *error*)))
               );or
          );and
          (setq *error* bonus_error);setq
      );if
     );progn else double check to make sure the bonus_error is in effect.
 );if
 (if (and ss
          (equal 1 (logand 1 (getvar "pickfirst")))
     );and
     (sssetfirst (car ss) (cadr ss))
 );if
);defun init_bonus_error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bonus_error ( msg / )

"bonus_error"

(setq bonus_alive -1)
(print msg)

;;Get out of any active command.
(while (not (equal (getvar "cmdnames") "")) (command nil))

;If undo global variable flag is set then use undo as a cleanup helper.
(if bonus_undoctl
    (progn
     (setvar "cmdecho" 0)

     (while (not (wcmatch (getvar "cmdnames") "*UNDO*"))
            (command "_.undo")
     );while
     (command "_end")  ;The routine that just failed created an undo 
                       ;begin mark, so we need to close it off with 
                       ;and "end" mark.

     (command "_.undo" "1")   ;now back up to the begining.
     (while (not (equal (getvar "cmdnames") "")) 
      (command nil)
     );while

    );progn
);if

(b_restore_sysvars)
(b_restore_undo)

;Restore original error handler
(if bonus_old_error
    (setq *error* bonus_old_error)
);if

(setq bonus_alive 0)

(princ)
);defun bonus_error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Restore_old_error
;This function should be the last thing called in a lisp 
;defined command. It does a (princ) at the end for a quiet 
;finish.
(defun restore_old_error ( / )

(setq bonus_alive (- bonus_alive 1))
(if (>= bonus_alive 0)
    (b_restore_sysvars)
    (setq bonus_varlist nil)
);if
(if (<= bonus_alive 0)
    (progn
     (b_restore_undo)
     (if bonus_old_error
         (setq *error* bonus_old_error);put the old error routine back.
     );if
    );progn then
);if

(princ)
);defun restore_old_error



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun b_restore_undo ()

(if bonus_undoctl
    (progn
      (b_set_sysvars (list "cmdecho" 0))

      (while (equal 8 (logand 8 (getvar "undoctl")))
         (command "_.undo" "_end")
      );while

      (if (not (equal bonus_undoctl (getvar "undoctl")))
          (progn
           (cond 
            ((equal 0 bonus_undoctl) 
             (command "_.undo" "_control" "_none")
            )
            ((equal 2 (logand 2 bonus_undoctl))
             (command "_.undo" "_control" "_one")
            )	
           );;cond 
           (if (equal 4 (logand 4 bonus_undoctl))
               (command "_.undo" "_auto" "_on") 
           );if 

         );progn then restore undoctl to the status the user had it set to. 
      );if
      (if (not (equal 2 (logand 2 (getvar "undoctl"))))
          (b_restore_sysvars)
      );if
    );progn then restore undo to it's original setting
);if
(setq bonus_undoctl nil)

);defun b_restore_undo


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This has no error checking. You must
;provide a list of even length in the 
;following form
;( "sysvar1" value
;  "sysvar2" value2
;)
(defun b_set_sysvars (lst / lst2 lst3 a b n)

(setq lst3 (car bonus_varlist));setq

(setq n 0)
(repeat (/ (length lst) 2)
 (setq a (strcase (nth n lst))
       b (nth (+ n 1) lst)
 );setq
 (setq lst2 (append lst2
                    (list (list a (getvar a)))
            );append
 );setq 
 (if (and bonus_varlist 
          (not (assoc a lst3))
     );and
     (setq lst3 (append lst3 
                        (list (list a (getvar a)))
                );append
     );setq 
 );if

 (setvar a b)

(setq n (+ n 2));setq
);repeat
(if bonus_varlist
    (setq bonus_varlist (append (list lst3) 
                                (cdr bonus_varlist)
                                (list lst2) 
                        );append
    );setq
    (setq bonus_varlist (list lst2))
);if
);defun b_set_sysvars

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun b_restore_sysvars ( / lst n a b)

 (if (<= bonus_alive 0)
     (setq           lst (car bonus_varlist)
           bonus_varlist (list lst)
     );setq 
     (setq lst (last bonus_varlist)) 
 );if

 (setq n 0);setq
 (repeat (length lst)
 (setq a (nth n lst)
       b (cadr a)
       a (car a)
 )
 (setvar a b)
 (setq n (+ n 1));setq
 );repeat
 (setq bonus_varlist (reverse (cdr (reverse bonus_varlist))))

);defun b_restore_sysvars

;;;;;;;;;;;;;;;;;;;;;;;;;end error handler functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;P_ISECT
;Poly-InterSECT
;takes a list of points and and a flag
;returns true if the 
; segments intersect on themselves.
;If the flag argument is true 
;then error strings will be printed to the command line
;if an intersection is found.
;
(defun p_isect ( lst flag2 / flag lst2 lst3 a b c d n j)

(setq n 0);setq
(repeat (length lst)
(setq    a (nth n lst)
      lst2 (append lst2 (list a))
);setq
(if (equal 2 (length lst2))
    (setq lst3 (append lst3 (list lst2))
          lst2 (list (cadr lst2))
    );setq
);if    
(setq n (+ n 1));setq
);repeat

(if (equal 2 (length lst2))
    (setq lst3 (append lst3 (list lst2)));setq
);if    

(setq n 0);setq
(while (and (< n (length lst3))
            (not flag)
       );and
(setq a (nth n lst3)
      b (cadr a)
      a (car a)
);setq
 (setq j (+ n 1))
 (while (and (< j (length lst3)) 
             (not flag)
        );and 
 (setq c (nth j lst3)
       d (cadr c)
       c (car c) 
 );setq
 (if (and (not (equal b c 0.000001))
          (not (equal a d 0.000001))
     );and
     (progn
      (setq flag (inters a b c d))
      (if (and flag 
               flag2
          );and
          (progn
           (princ "\nInvalid. Crossing polygon cannot self intersect.")
           (princ flag2)
          );progn
      );if
     );progn
 );if
 
 (setq j (+ j 1));setq
 );while

(setq n (+ n 1));setq
);while

flag
);defun p_isect

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;zoom_4_select
;Takes - a list of coordinates. If all coords do not lie in
;the current view, zoom_4_select will zoom to the extents 
;of the coords in the list argument.
;Returns - True in the form of two corners points if a zoom operation needs 
;          to be performed and returns nil if not. 
;
(defun zoom_4_select ( lst / a b)

 (setq  lst (lsttrans lst 1 2) 
          a (maxminpnt (lsttrans (viewpnts) 1 2))
          b (maxminpnt (append a lst))
 );setq 

 (if (not (equal a b))
     (progn
      (setq b (list (trans (append (car b) '(0.0))  2 1)
                    (trans (append (cadr b) '(0.0)) 2 1)
              )
      );setq
     );progn
     (setq b nil)
 );if

 b
);defun zoom_4_select


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;maxminpnt
;takes: a list of points
;returns: a list of 2 points the lower left and the upper right
;
;	maxminpnt	 
(defun maxminpnt ( lst / x n a b c d)

(setq x (car lst)
      a (car x)
      b (cadr x)
      c (car x)
      d (cadr x)
      n 1
);setq
(repeat (max (- (length lst) 1) 0)
(setq x (nth n lst));setq
(setq a (min a (car x))
      b (min b (cadr x))
      c (max c (car x))
      d (max d (cadr x))
);setq
(setq n (+ n 1));setq
);repeat
(list (list a b)
      (list c d)
);list
);defun maxminpnt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;viewpnts
;returns lower left and upper right coords of current view
(defun viewpnts ( / a b c d x)

(setq b (getvar "viewsize")
      c (car (getvar "screensize"))
      d (cadr (getvar "screensize"))
      a (* b (/ c d))
      x (setq x (getvar "viewctr"))
      x (trans x 1 2)
      c (list (- (car x)  (/ a 2.0))
              (- (cadr x) (/ b 2.0))
              0.0
        );list
      d (list (+ (car x)  (/ a 2.0))
              (+ (cadr x) (/ b 2.0))
              0.0
        );list
      c (trans c 2 1)
      d (trans d 2 1) 
);setq

(list c d)
);defun viewpnts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;pixel_unit
;returns the size of a single pixel in drawing units.
;value depends on current zoom factor.
;
;pixunit/viewsize = one pixel/yscreensize
;
;pixunit=viewsize/yscreensize 
;
(defun pixel_unit ( / x y x1 y1)
 (setq  y (getvar "viewsize")
       x1 (car (getvar "screensize"))
       y1 (cadr (getvar "screensize"))
        x (* y (/ x1 y1))
 );setq
 (max (abs (/ y y1))
      (abs (/ x x1))
 );max
);defun pixel_unit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;**PLINE** function takes a list and creates a polyline entity
;the list contains a list of coords.
;and optionaly other lists such as (8 . "LAYER")
;				   (40 . WIDTH)
;				   (62 . COLOR)
;	pline	     n a b flag
(defun pline ( lst / n a b flag)


(if (> (length lst) 1)
    (progn
     (if (setq b (assoc 8 lst));setq
	 (setq a (append a (list b)));setq then
     );if
     (if (setq b (assoc 40 lst));setq
	 (setq a (append a (list b)
			    (list (cons 41 (cdr b))
			    );list
		 );append
	 );setq then
     );if
     (if (setq b (assoc 62 lst));setq
	 (setq a (append a (list b)));setq then
     );if

    );progn then
    (setq flag T
	     b (car lst)
    );setq else only a coord list was provided
);if

(setq n 0)
(while (and (not flag)
	    (< n (length lst))
       );and

(if (not (member (nth n lst) a))
    (setq    b (nth n lst)
	  flag T
    );setq then
);if

(setq n (+ n 1));setq
);while

(entmake (append (list '(0 . "POLYLINE")) a));entmake

(setq n 0)
(repeat (length b)

(entmake (list '(0 . "VERTEX")
	       (append (list 10) (nth n b))
	 );list
);entmake

(setq n (+ n 1));setq
);repeat

(entmake '((0 . "SEQEND")));entmake

(princ)
);defun pline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;**VTLIST** takes a list or a polyline ent name
;RETURNS a list of vertecies in
; WORLD if a list is provided '(na code) and code=0
; CURRENT UCS coords are returned if only na is provided as an argument
;or if a lst is provided and code=1
;
;	
(defun vtlist ( na / dxf e1 lst lst2 code n flag a z)

 ;local function
 (defun dxf (a b / ) (cdr (assoc a b)));defun

(if (equal (type na)
	     (type (list 1))
    );equal
    (setq code (cadr na)
            na (car na)
    );setq then
    (setq code 1);setq else
);if
(setq e1 (entget na));setq
(if (equal 1 (logand 1 (dxf 70 e1)))
    (setq flag 1)
    (setq flag nil)
);if
(if (equal (dxf 0 e1) "POLYLINE")
    (progn
     (setq na (entnext na)
           e1 (entget na)
     );setq
     (while (/= "SEQEND" (dxf 0 e1))    
      (setq lst (append lst 
                        (list (trans (dxf 10 e1) na code));list 
                );append
             na (entnext na)
             e1 (entget na)
      );setq
     );while
    );progn then old polyline
    (progn
     (setq lst e1
             z (dxf 38 e1)
     );setq
     (if (not z) (setq z 0.0)) 
     (setq n 0);setq
     (repeat (length lst)
     (setq    a (nth n lst))
     (if (equal (car a) 10)
         (setq   a (cdr a)
                 a (list (car a) (cadr a) z)
              lst2 (append lst2 
                           (list (trans a na code))
                   );append
         );setq then
     );if
     (setq n (+ n 1));setq 
     );repeat
     (setq  lst lst2 
           lst2 nil
     );setq
    );progn else lwpolyline
);if 
(if (and flag lst)
    (setq lst (append lst (list (car lst))));setq
);if
lst
);defun vtlist

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ep_list
;returns entity point list
;returns a list of points on the entity
;args
; na -is a polyline 'pl_point_list' is called
; alt -is an arc segmant error tolerance (altitude)
;
(defun ep_list ( na alt / dxf a b c d n e1 lst)

 ;local function
 (defun dxf (a b / ) (cdr (assoc a b)));defun

(setq e1 (entget na));setq
(cond
 ((or (equal (dxf 0 e1) "POLYLINE")
      (equal (dxf 0 e1) "LWPOLYLINE")
  );or
  (setq lst (pl_point_list na alt));setq
 );cond #1
 ((equal (dxf 0 e1) "LINE")
  (setq lst (list (trans (dxf 10 e1) na 1)
                  (trans (dxf 11 e1) na 1)
            );list
  );setq
 );cond #2
 ((or (equal (dxf 0 e1) "ARC") 
      (equal (dxf 0 e1) "CIRCLE")
  );or
  (progn
   (setq a (dxf 10 e1)             ;the center point
         b (dxf 40 e1)             ;the radius
         n (dxf 50 e1)             ;the start angle
         c (dxf 51 e1)             ;the end angle
   );setq
   (if (not n)
       (setq n 0
             c (* 2.0 pi)
       );setq then it's a circle
       (if (> n c)
           (setq c (+ c (* 2.0 pi)));setq then
       );if else it's an arc
   );if
   (setq lst (append lst 
                     (get_arc_points a 
                                     (polar a n b) 
                                     (polar a c b)
                                     (- c n) 
                                     alt
                     );get_arc_points
             );append
   );setq
   (setq lst (lsttrans lst na 1))
  );progn
 );cond #3
;add trim capabilities for text here
; ((equal "TEXT" (dxf 0 e1))
;  (setq p1 (textbox e1)
;
;  );setq
; );cond #4  
);cond close

lst
);defun ep_list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pl_point_list ( na alt / dxf na2 a b c z p1 p2 p3 e1 lst lst2 code n flag )

 ;local function
 (defun dxf (a b / ) (cdr (assoc a b)));defun

 (if (equal (type na)
            (type (list 1))
     );equal
     (setq code (cadr na)
             na (car na)
     );setq then
     (setq code 1);setq else
 );if
 (setq e1 (entget na));setq
 (if (equal 1 (logand 1 (dxf 70 e1)))
     (setq flag 1)
     (setq flag nil)
 );if
 (setq na2 na)
 (if (equal (dxf 0 e1) "POLYLINE")
     (progn
      (setq na (entnext na)
            e1 (entget na)
            p1 (dxf 10 e1) 
             b (dxf 42 e1) 
      );setq
      (if (not (equal 16 (logand 16 (dxf 70 e1))))
          (setq lst (list p1))
          (setq lst nil)
      );if     
      (setq na (entnext na)
            e1 (entget na)
      );setq
      (while (/= "SEQEND" (dxf 0 e1 ))    
       (setq p2 (dxf 10 e1));setq
       (if (not (equal 16 (logand 16 (dxf 70 e1))))
           (progn 
            (if (and b 
                     (not (equal b 0)) 
                     ;added below for cloud problems
                     p1
                     (not (equal p1 p2))
                );and
                (setq  p3 (pl_arc_info p1 p2 b)
                      lst (append lst 
                                  (get_arc_points (car p3) p1 p2 (caddr p3) alt)
                          );append
                );setq then
                (setq lst (append lst (list p2)));setq else
            );if
           );progn then not a spline control point
       );if
       (setq  b (dxf 42 e1)
             na (entnext na)
             e1 (entget na)
             p1 p2
       );setq
      );while
     
       (if flag
           (progn
            (setq p2 (car lst))
            (if (and b 
                     (not (equal b 0)) 
                     ;added below for cloud problems
                     p1
                     (not (equal p1 p2))
                );and
                (setq  p3 (pl_arc_info p1 p2 b)
                      lst (append lst 
                                  (get_arc_points (car p3) p1 p2 (caddr p3) alt)
                          );append
                );setq then
                (setq lst (append lst (list p2)));setq else
            );if
            (setq flag nil)
           );progn then it's closed
      );if
     );progn then old polyline
     (progn
      (setq   z (dxf 38 e1)
            lst (member (assoc 10 e1) e1)
      );setq 
      (if (not z) (setq z 0.0))
      (setq n 0);setq
      (repeat (length lst)
       (setq a (nth n lst))
       (if (equal 10 (car a))
           (setq    a (cons 10 (append (cdr a) (list z)))
                 lst2 (append lst2 (list a))
           );setq
           (progn
            (if (equal 42 (car a))
                (setq lst2 (append lst2 (list a)));setq then
            );if
           );progn
       );if 
      (setq n (+ n 1));setq
      );repeat
      
      (setq  b (car lst2)
           lst (list (cdr b))
      );setq
      
      (if flag 
          (setq lst2 (append lst2 (list (car lst2)))
                flag nil 
          );setq
      );if

      (setq n 1);setq
      (repeat (- (length lst2) 1)
      (setq a (nth n lst2));setq
      (if (and (equal 10 (car a))
               (equal 42 (car b))
               (not (equal 0.0 (cdr b)))
               ;added below for cloud problems
               c
               (not (equal (cdr a) (cdr c)))
          );and  
          (progn   
           (setq  p3 (pl_arc_info (cdr c) (cdr a) (cdr b))
                 lst (append lst 
                             (get_arc_points (car p3) (cdr c) (cdr a) (caddr p3) alt)
                     );append  
           );setq
          );progn then get the arc points
          (progn
           (if (equal 10 (car a))
               (setq lst (append lst (list (cdr a)));append
               );setq then
           );if 
          );progn
      );if
      (setq c b
            b a
      );setq
      (setq n (+ n 1));setq 
      );repeat
      (setq lst2 nil);setq
     );progn else lwpolyline
 );if 

 (lsttrans lst na2 code)
);defun pl_point_list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;get_arc_points
;returns a list of points that lie along an arc described 
;by the following arguments:
;takes: 
;  p1 - start point
;  p2 - end point
;  p3 - center point
; ang - delta angle of the arc
; alt - altitude (max error tolerance)
;
(defun get_arc_points ( p1 p2 p3 ang alt / sa ea lst b c d)

 (setq  b (distance p1 p2)       ;the radius
       sa (angle p1 p2)          ;the start angle
       ea (+ sa ang)             ;the end angle
 );setq
 (setq d (- ea sa));setq full delta angle of the arc in radians
 (if (not alt)
     (setq c (/ (* 2.0 pi) 9.0));then use default resolution
     (setq c (delta_ang b alt));setq else use altitute specified. 
 );if
 (setq  c (* c (/ d (abs d))));setq the delta angle increment of the loop
 
 (if (< (abs (/ d c)) 4.0)
     (setq c (/ d 4.0)
     );setq then reset c, the delta angle increment of the loop so 
                          ;at least 4 segments are used
 );if

 (repeat (+ (fix (+ (abs (/ d c))
                    0.000001
                 );plus
            );fix
            1
         );plus
 (if (not (equal (polar p1 sa b) (last lst)))
     (setq lst (append lst
                       (list (polar p1 sa b))
               );append
     );setq then
 );if
 (setq sa (+ sa c));setq
 );repeat

 (if (not (equal p3 (last lst)))
     (setq lst (append lst (list p3)));setq
 );if
 lst
);defun get_arc_points

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes endpoints of an arc and bulge
;returns a list containing:
; Center point
; radius
; Delta angle
; distance (arc length)
; altitude
;
(defun pl_arc_info ( p1 p2 bulge / a b c r ang s)

 (setq b bulge 
       c (distance p1 p2)
       a (abs (/ (* c b) 2.0))
       r (/ (+ (expt (/ c 2.0)
                     2.0
               );expt
               (expt a 2.0)
            );plus
            (* 2.0 a)
         );div
     ang (* 2.0 (atan (/ c 2.0)
                      (- r a)
                );atan
         );mult delta angle
       s (* ang r);the length of the arc
     ang (* ang (/ b (abs b)))
      p3 (polar p1 (angle p1 p2) (/ c 2.0))
      p3 (polar p3 
                (+ (angle p1 p2) (/ pi 2.0)) 
                (* (- r a) 
                   (/ b (abs b))
                );mult
         );polar
 );setq
 (list p3 r ang s a)
);defun pl_arc_info

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;delta_ang
;returns the delta angle of an arc with
;the specified altitude and radius
(defun delta_ang ( r a / c ang)
 (setq c (* 2.0
            (sqrt 
              (abs (- (* 2.0 r a)
                      (expt a 2.0)
                   )
              )
            )
         )
     ang (* 2.0 (atan (/ c 2.0)
                      (- r a)
                );atan
         );mult delta angle
 );setq
 ang
);defun delta_ang

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;**M_ASSOC** Multiple-ASSOC
;TAKES:
;a  - search value for assoc
;lst- a list of sub-lists
;RETURNS: a list of all of the sublists that have 'a' as their first element
;
;	m_assoc b lst2
(defun m_assoc ( a lst / b lst2)

(while (setq b (assoc a lst));setq
(setq  lst (cdr (member b lst))
      lst2 (append lst2 (list b))
);setq
);while

lst2
);defun m_assoc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;lsttrans
;is like standard autolisp trans but takes a 
;LIST of points and returns a list of translated 
;points.
;
(defun lsttrans ( lst a b / lst2 c n)

(setq n 0);setq
(repeat (length lst)
(setq	 c (nth n lst)
	 c (trans c a b)
      lst2 (append lst2 (list c))
);setq
(setq n (+ n 1));setq
);repeat

lst2
);defun lsttrans

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TNLIST
;returns a list of the symbol names found
;in the specified table
;	
(defun tnlist ( tbna / a lst)
(if (and (equal (type tbna) 'LIST) 
         (equal (cadr tbna) 16)          
    );and
    (progn
     (setq tbna (car tbna));setq
     (while (setq a (tblnext tbna (not a))); a acts as a rewind first time
      (if (not (equal 16 (logand 16 (cdr (assoc 70 a)))))
          (setq lst (append lst 
                            (list (cdr (assoc 2 a)))
                    );append
          );setq
      );if
     );while
    );progn then local only
    (progn
     (while (setq a (tblnext tbna (not a))); a acts as a rewind first time
      (setq lst (append lst 
                       (list (cdr (assoc 2 a)))
                );append
      );setq
     );while
    );progn else
);if 
 lst
);defun tnlist

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sets current ucs to be parallel to the extrusion vector 
;specified by p1
;
;This function is used by extrim and exchprop
;
(defun ucs_2_ent ( p1 / )
 (setq p1 (strcat "*"
                  (rtos (car p1) 2 8) ","
                  (rtos (cadr p1) 2 8) ","
                  (rtos (caddr p1) 2 8)
          );strcat
 );setq
 (command "_.ucs" "_za" "*0.0,0.0,0.0" p1)
);defun ucs_2_ent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a layer name and returns 
;True if the layer is locked, and nil if unlocked.
;
(defun b_layer_locked ( la / na e1)
 (setq na (tblobjname "layer" la)
       e1 (entget na)
 );setq
 (equal 4 
        (logand 4 (cdr (assoc 70 e1)))
 );equal
);defun b_layer_locked

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes an element and a list
;if the element is contained in the list
;   then position of the first occurrance is returned (integer)
;   else nil is returned
;
;      position b
(defun position ( a lst / b)
 (if (setq b (member a lst));setq
     (progn
      (setq b (- (length lst) (length b)));setq
     );progn then
 );if
 b
);defun position

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;**MPOPLST Make-POPup-LST take the key and a list
;
(defun mpoplst ( a lst / n )
 (start_list a 3)
 (setq n 0);setq
 (repeat (length lst)
  (add_list (nth n lst))
  (setq n (+ n 1));setq
 );repeat
 (end_list)
);defun mpoplst

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;Just a little util to see what lisp commands are loaded into memory.
;also has an option that calls appload.
;
(defun c:lsp ( / lst n a )
 (initget "Commands Load") 
 (setq a (getkword "\nCommands/Load: "))
 (cond 
   ((equal a "Commands")
    (progn  
     (setq lst (atoms-family 1)
           lst (acad_strlsort lst)
     );setq
     (setq n 0);setq
     (repeat (length lst)
      (setq a (nth n lst));setq
      (if (equal "C:" (strcase (substr a 1 2)))
          (princ (strcat "\n" (substr a 3)))
      );if
     (setq n (+ n 1));setq
     );repeat
    );progn then
   );cond 1 
   ((equal a "Load") 
    (c:appload)
   )
 );cond close
 (princ)
);defun c:lsp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes: A ssget type of filter list and a flag.
;If the flag is true then locked layer selection is allowed. 
;
;Returns: A single entity name that matches the filter list
;
(defun single_select ( flt flag / a p1 p2 ss ss2 flag2)

(while (not flag2)
(setvar "highlight" 0)
(if (entnext)
    (progn
     (command "_.select" (entnext) "")
     (command "_.undo" "1");clear any selection sets
    );progn then
);if
(setvar "highlight" 1)
(command "_.select" "_si")
(setvar "cmdecho" 1)
(command pause)

(setq ss2 (ssget "p")
       p1 (getvar "lastpoint")
       p1 (trans p1 1 (getvar "viewdir"))
        a (* (getvar "pickbox") (pixel_unit))
       p2 (list (- (car p1) a) (- (cadr p1) a) 0.0)
       p1 (list (+ (car p1) a) (+ (cadr p1) a) 0.0)
       p1 (trans p1 (getvar "viewdir") 1)    
       p2 (trans p2 (getvar "viewdir") 1)    
);setq
(if (and ss2
         (or (setq ss (ssget "p" flt))
             (setq ss (ssget "c" p1 p2 flt))   
         );or
    );and 
    (progn
     (setq ss (ssname ss 0))
     (if flag
         (setq flag2 T)
         (progn
          (if (b_layer_locked (cdr (assoc 8 (entget ss))))
              (progn
               (setq flag2 nil) 
               (princ "\nThat object is on a locked layer!")
              );progn
              (setq flag2 T);else got something and its on an UN-locked layer
          );if
         );progn else locked layer selection is not allowed
     );if
    );progn
    (progn
     (if ss2
         (princ "\nInvalid selection.")
         (setq flag2 T);they just exited with enter
     );if
    );progn 
);if
(setvar "cmdecho" 0)
);while
ss
);defun single_select

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ss_visible ( ss code / na e1 n)
 (if ss
     (progn
      (setq n 0)
      (repeat (sslength ss)
       (setq na (ssname ss n)
             e1 (entget na)
       );setq
       (if (not (assoc 60 e1))
           (setq e1 (append e1 (list (cons 60 code))));setq then
           (setq e1 (subst (cons 60 code) (assoc 60 e1) e1));setq else
       );if
       (entmod e1) 
       (entupd na)
       (setq n (+ n 1));setq
      );repeat 
     );progn
 );if
);defun ss_visible

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun image_bounds ( na / dxf p1 p2 p3 p4 p5 p6 p7 p8 a b c x y ang e1)
 (defun dxf ( a b ) (cdr (assoc a b)))
 (setq e1 (entget na)
       p1 (dxf 10 e1);insert point
       p2 (dxf 11 e1)
       p3 (dxf 12 e1)
       p4 (dxf 13 e1)
 );setq
 (setq 
      ang (atan (/ (cadr p2) (car p2)))
        x (sqrt (+ (expt (car p2) 2) (expt (cadr p2) 2)))
        y (sqrt (+ (expt (car p3) 2) (expt (cadr p3) 2) ))
        a (* x (car p4))
        b (* y (cadr p4))
        c (list (+ a (car p1)) 
                (+ b (cadr p1)) 
                (caddr p1) 
          )
       p5 p1               ;lower left
       p6 (list (car c)    ;lower right
                (cadr p1) 
                (caddr p1)
          );list
       p7 c                ;upper right
       p8 (list (car p1)   ;upper left
                (cadr c) 
                (caddr p1)
          )
       p6 (rotate_pnt p6 p1 ang)
       p7 (rotate_pnt p7 p1 ang)
       p8 (rotate_pnt p8 p1 ang)
 );setq
 (list p5 p6 p7 p8 p5)
);defun image_bounds

;
;Rotate 'pnt' from a base point of 'p1' and through an angle 
;of 'ang' (in radians)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rotate_pnt ( pnt p1 ang / )
 (polar p1 
        (+ (angle p1 pnt) ang) 
        (distance p1 pnt) 
 );polar
);defun rotate_pnt 

(princ "\nAutoCAD bonus utilities loaded.\n")
(princ)
