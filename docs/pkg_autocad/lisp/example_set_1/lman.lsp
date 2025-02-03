;;;
;;;    LMAN.LSP - Written by Randy Kintzley 
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
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions created as result of loading file: lman.lsp
; ADD_LAYERSTATE
; C_DL
; C_EXPORT_LAY
; C_IMPORT_LAY
; C_RL
; C_SL
; D_LAYERSTATE
; DL
; DLG_DL
; DLG_EDIT
; DLG_EXPORT
; DLG_IMPORT
; DLG_RL
; DLG_SL
; EXPORT
; GET_F_VP
; GET_LA_STATUS
; GET_LAYERSTATES
; GET_LAYERSTATES_ALL
; GET_LAYERSTATES_ALL2
; GET_NEXTNAME
; IMPORT
; LIST_LAYERSTATES
; LIST_PICK
; LMAN_LTYPEFX
; LOCAL_LTYPE
; NEW_STATE
;-   PSTRIP (removed and added to ac_bonus.lsp)
; PUTINLST
; R_LAYERSTATE
; REG_IT
; RK_REMOVE
; RL
; SL
; SPINNER
; WARNING
; WRITE_IT
;-   XSTRIP (removed and added to ac_bonus.lsp)
;
;Variables created as result of loading file: lman.lsp
;
;Functions created as a result of executing the commands in: lman.lsp
; DOWN_DLG_OPERATION
;
;Variables created as a result of executing the commands in: lman.lsp
; #DLG_PNT
; #LAST_RESTORE
; BONUS_ALIVE
; BONUS_OLD_ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:lman ( /  msg str iv lst lst2 flag );

(if (and (not init_bonus_error) 
         (equal -1 (load "ac_bonus.lsp"  -1)) 
    );and
    (progn (alert "Error:\n     Cannot find AC_BONUS.LSP.")(exit))
);if
(init_bonus_error (list
                   (list   "cmdecho" 0
                         "regenmode" 1 
                            "expert" 0
                           "ucsicon" 0 
                   )
                   nil     ;flag. True means use undo for error clean up.  
                   
                  );list  
);init_bonus_error

(lman_ltypefx)

(reg_it)

(setq  lst (get_layerstates));setq
(if (and (equal (cadr #last_restore) (get_la_status));equal
         (member (car #last_restore) lst)
    );and 
    (setq lst2 (list (strcat "Current layer state: " (car #last_restore)) 
                     (car #last_restore) 
                     (itoa (position (car #last_restore) lst))
               );list
    );setq then 
    (setq lst2 (list "Current layer state: *UNNAMED*" "" ""));setq
);if     

(if (not #dlg_pnt)
    (setq #dlg_pnt '(-1 -1))
);if
(while (or (not flag)
	   (equal flag 2)
       );or
 
(if (> (setq iv (load_dialog "lman.dcl")) 0)
    (progn
     (setq #iv iv);setq
     (if (new_dialog "lman" iv "" #dlg_pnt);NEW_DIALOG DISPLAYS THE DIALOG
	 (progn

	  (if lst 
	      (progn 
	       (mpoplst "list_states" lst);make the pop-up list
	       (set_tile "list_states" (caddr lst2))
	      );progn then
	      (setq lst2 (list (car lst2) (cadr lst2) ""));setq else
	  );if
	  (set_tile "msg" (car lst2))
				 
	  (action_tile "list_states"
		       "(setq lst2 (list_pick lst))"
	  );action_tile
	  ;(setq msg "Create a new layer state"
          ;      str (strcat "(setq lst "
          ;                  "(dlg_sl lst iv "
          ;                  "(new_state lst (get_nextname lst) msg)) "
          ;                  "lst2 (cadr lst) "
          ;                  "lst (car lst))" 
          ;          );strcat
          ;);setq
	  (setq msg "Save current layer status as:"
                str (strcat "(setq lst "
                            "(dlg_sl lst iv "
                            "(if (not (equal (get_tile \"list_states\") \"\")) " 
                                 "(new_state lst "
                                   "(nth (atoi (get_tile \"list_states\")) lst) "
                                   "msg "
                                 ")"
                                 "(new_state lst (get_nextname lst) msg)) "
                             ")"    
                            "lst2 (cadr lst) "
                            "lst (car lst))" 
                    );strcat
          );setq

	  (action_tile "saveas" str);action_tile

	  (action_tile "edit"
		       "(setq lst (dlg_edit lst iv) lst2 (cadr lst) lst (car lst))"
	  );action_tile

	  (action_tile "delete"
		       "(setq lst (dlg_dl lst iv) lst2 (cadr lst) lst (car lst))"
	  );action_tile

	  (action_tile "import"
		       "(dlg_import iv)"
	  );action_tile
	  (action_tile "export"
		       "(dlg_export iv)"
	  );action_tile

	  (action_tile "restore"
		       ;;;;"(setq rk (done_dialog 1))" 
		       "(setq lst (dlg_rl lst iv) lst2 (cadr lst) lst (car lst))"
	  );action_tile
	  (action_tile "close"
		       "(setq #dlg_pnt (done_dialog 0))"
	  );action_tile
	  (mode_tile "restore" 2)

	  (setq flag (start_dialog));setq
	    ;START_DIALOG MAKES THE BUTTONS ACTIVE

	 );progn then initialize the tiles and activate the dialog box
	 (progn
	  (alert "Unable to display dialog box")
	  (setq flag 1)
	 );progn
     );if new dialog

     
     (setq #iv nil);setq
     
     (if (equal flag 2)
	 (progn
	   (setq lst (down_dlg_operation lst))
	   (if (and (equal (length lst) 2)
		    (or (equal (type (list 1)) (type (car lst))) 
			(not (car lst)) 
		    );or
		    ;(or 
			(equal (type (list 1)) (type (cadr lst))) 
		    ;    (not (cadr lst)) 
		    ;);or
	       );and
	       (setq lst2 (cadr lst)
		      lst (car lst)
	       );setq
	   );if
	 );progn
	 (unload_dialog iv);unload it when done
     );if
    );progn then
    (progn
     (alert "Unable to load dialog box")
     (setq flag 1)
    );progn else
);if load

);while

(cond  
 ((equal flag 1)
  (progn
       (rl (nth (atoi (nth 2 lst2)) 
		lst
	   );nth
       );rl
  );progn then
 );cond #1
);cond close

(restore_old_error)

(princ)
);defun c:lman
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun dlg_import ( iv / )
  (setq down_dlg_operation (list '(lst / lst2 ) 
 				 '(setq lst2 (c_import_lay))
                                 '(list (get_layerstates) lst2);'lst 
                           );list
  );setq
  (setq #dlg_pnt (done_dialog 2));setq 
  (unload_dialog iv) 
 );defun dlg_import
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun dlg_export ( iv / )
  (setq down_dlg_operation (list '(lst / lst2) 
                                 '(setq lst2 (c_export_lay))
                                 '(list lst lst2)
                           );list
  );setq
  (setq #dlg_pnt (done_dialog 2)) 
  (unload_dialog iv) 
 );defun dlg_export
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun dlg_sl ( lst iv a / msg b lst2)

  (if (and (not #iv)
           (or (not (equal (cadr #last_restore) (get_la_status)
                    );equal
               );not
               #incomplete
           );or
      );and 
      (progn 
       (setq msg "Save changes as:"
               b a
               a (new_state lst a msg)
       );setq
       (if a 
           (setq #incomplete nil)
       );if
      );progn then
      (progn 
       (if (not #iv)
           (setq b a
                 a nil
           );setq then
       );if
      );progn else
  );if
  (if a
      (progn
       (if (not (member a lst))
           (setq  lst (append lst (list a)) 
                  lst (acad_strlsort lst)
                 lst2 (list (strcat "Created: " a) 
                            a
                            (itoa (position a lst))
                      );list
           );setq then
           (setq lst2 (list (strcat "Redefined: " a) 
                            a
                            (itoa (position a lst))
                      );list
           );setq else
       );if
       (if (not #iv)
           (progn
            (sl a)
           );progn then
           (progn
            (setq down_dlg_operation (list '(lst / ) 
                                            (append '(sl) (list a))
                                           'lst 
                                     );list
            );setq
            (setq #dlg_pnt (done_dialog 2)) 
            (unload_dialog iv) 
           );progn
       );if  
      );progn then 
      (progn
       (if (and lst
                #iv
           );and 
           (progn
            (setq lst2 (list (strcat "Current layer state: "
                                     (nth (atoi (get_tile "list_states")) lst) 
                             );strcat 
                             (nth (atoi (get_tile "list_states")) lst)
                             (get_tile "list_states")
                       );list
            );setq
           );progn then
           (progn

            (if (and (member b lst)
                     (equal (cadr #last_restore) (get_la_status))
                );and 
                (setq lst2 (list (strcat "Current layer state: " b)
                                 b 
                                 (itoa (position b lst)) 
                           )
                );setq then
                (setq lst2 (list "Current layer state: *UNNAMED*"
                                 nil 
                                 ""
                           )
                );setq else
            );if 
           );progn else
       );if
       (setq
        down_dlg_operation (list '(lst / ) 
                                 '(princ)
                                 'lst 
                           );list
       );setq  
      );progn else 
  );if
  (setq lst (list lst lst2))
  lst
 );defun dlg_sl
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun dlg_rl ( lst iv /  lst2 a b)
  
  (if lst
      (progn
       (setq    a (get_tile "list_states")
                b (nth (atoi a) lst)
             lst2 (list (strcat "Restored: " b)
                        b
                        a
                  );list
        down_dlg_operation (list '(lst / ) 
                                 (append '(rl) (list b))
                                'lst 
                           );list
       );setq 
       (setq #dlg_pnt (done_dialog 2)) 
       (unload_dialog iv) 
      );progn then 
      (setq lst2 (list "" nil ""));setq  
  );if
   
  (list lst lst2)
 );defun dlg_rl

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun get_nextname (lst / mx n a )
  (setq  n 0
        mx 0
  );setq
  (repeat (length lst)
   (setq a (nth n lst))
   (if (wcmatch a "LAYER_STATE*")
       (setq mx (max mx
                     (atoi (substr a 12))
                );max
       );setq
   );if 
   (setq n (+ n 1));setq 
  );repeat
  (setq a (strcat "LAYER_STATE" (itoa (+ 1 mx))));setq
  a
 );defun get_nextname


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun dlg_edit ( lst iv / lstate lst2)
  (if (and lst         
           (not (equal "" (get_tile "list_states")))
      );and
      (progn
       (setq lstate (nth (atoi (get_tile "list_states")) lst)
             lstate (strcase (sstrip lstate))
               lst2 (list (strcat  "Current layer state: " lstate)
                          (get_tile "save_name") 
                          (get_tile "list_states")
                    );list  
       );setq
       (setq down_dlg_operation (list
                                      '(lst)
                                      (append '(progn)
                                       (list  
                                        (append '(rl) (list lstate))
                                        (append '(command) (list "_.ddlmodes"))
                                       );list
                                      );append
                                      (append '(dlg_sl) (list 'lst iv lstate))
                                );list
       );setq
      );progn then
      (progn 
       (if (member (car #last_restore) lst)
           (setq   lst2 (list (strcat "Current layer state: " (car #last_restore))
                              (car #last_restore) 
                              (itoa (position (car #last_restore) lst))
                        );list
                 lstate (car #last_restore) 
           );setq then 
           (setq   lst2 (list "Current layer state: *UNNAMED*" nil "")
                 lstate (get_nextname lst)       
           );setq else
       );if
     
       (setq down_dlg_operation (list
                                      '(lst)
                                       (append '(progn)
                                               (list                           
                                                (append '(command) (list "_.ddlmodes"))
                                               );list
                                       );append                             
                                       ;'lst 
                                       (append '(dlg_sl) (list 'lst iv lstate))
                                );list
       );setq
      );progn else
  );if
  (setq #dlg_pnt (done_dialog 2)) 
  (unload_dialog iv) 
 
  (setq lst (list lst lst2))  
 );defun dlg_edit

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun dlg_dl ( lst iv / lstate lst2 n)
  
  (if (and lst
           (not (equal ""
                       (setq n (get_tile "list_states"))
                );equal
           );not
           (setq      n (atoi n)
                 lstate (nth n lst)
                 lstate (strcase (sstrip lstate))
                   lst2 (list (get_tile "msg")
                              lstate
                              (get_tile "list_states")
                        );list
           );setq
           (equal 1 (warning lstate "Are you sure you want to delete"))
      );and 
      (progn 
       (setq lst2 (list (strcat "Deleted: " lstate)));setq
       (setq lst (rk_remove n lst));setq 
       (if (member (car #last_restore) lst)
           (setq lst2 (list (car lst2)
                            (car #last_restore)
                            (itoa (position (car #last_restore) lst))
                      );list
           );setq then
           (setq lst2 (list (car lst2) nil ""));setq 
       );if    
       (setq down_dlg_operation (list '(lst)
                                      (append '(dl) (list lstate))
                                      'lst
                                );list
       );setq 
       (setq #dlg_pnt (done_dialog 2)) 
       (unload_dialog iv) 
       
      );progn then 
      (progn 
       (if (member (car #last_restore) lst)
           (setq lst2 (list (strcat "Current layer state: " (car #last_restore));strcat
                            (car #last_restore)
                            (itoa (position (car #last_restore) lst))
                      );list
           );setq then
           (setq lst2 (list "" nil ""));setq 
       );if 
      );progn else
  );if 
  
  (setq lst (list lst lst2))  
 );defun dlg_dl

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun list_pick ( lst / a) 
  (setq a (list
              (get_tile "msg")
              (get_tile "save_name")
              (get_tile "list_states")
          );list
  );setq
  (if (equal $REASON 4)
      (setq #dlg_pnt (done_dialog 1))    
  );if
  
  a
 );defun list_pick

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun dl ( lstate / lst n)
  (setq lst (tnlist "layer"));setq
  (setq n 0);setq
  (repeat (length lst)
   (d_layerstate (nth n lst) lstate)
   ;(spinner)
   (setq n (+ n 1));setq
  );repeat
  (if (equal lstate (car #last_restore))
      (setq #last_restore nil)
  );if
 (princ)
 );defun dl

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;delete layer state
 (defun d_layerstate ( la lstate / lst lst2 lst3 e1 xd a b)
  (setq     e1 (entget (TBLOBJNAME "layer" la) '("RAK"))
            xd (cdr (assoc -3 e1))
           lst (cdr (assoc "RAK" xd))
             a (cons 1000 (strcat "{" lstate))
             b (cons 1000 (strcat lstate "}")) 
  );setq
  (if (member a lst)
      (progn 
       (setq lst2 (reverse (cdr (member a (reverse lst))))
             lst3 (cdr (member b lst))
       );setq
       (setq
             lst (append lst2 lst3)
             lst (append (list "RAK") lst)
              xd (subst lst (assoc "RAK" xd) xd) 
              xd (append (list -3) xd)
              e1 (subst xd (assoc -3 e1) e1)
       );setq
       (entmod e1) 
      );progn then remove it
  );if
  (princ)
 );defun d_layerstate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun get_f_vp ( na / e1 xd a lst)
  (setq e1 (entget na '("*"))
        xd (cdr (assoc -3 e1))
        xd (cdr (assoc "ACAD" xd))
        xd (cdr (member (assoc 1002 xd) xd))
        xd (cdr (member (assoc 1002 xd) xd)) 
  );setq
  (while (and (not (equal (car xd) (assoc 1002 xd)))
              (setq a (assoc 1003 xd));setq
              (setq xd (cdr (member a xd)));setq
              (setq a (cdr a));setq
         );and
   (setq lst (append lst (list a)));setq
  );while 
  lst
 );defun get_f_vp

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun sl ( lstate / lst lst2 n ss na a vp clayer)
  (if (and (equal (getvar "tilemode") 0)
           (setq  a (getvar "cvport") 
                 ss (ssget "_x" (list '(0 . "VIEWPORT") 
                                     '(67 . 1)
                                     (cons 69 a)
                               );list
                    );ssget
           );setq
      );and
      (progn 
       (setq   na (ssname ss 0)
               ss nil 
             lst2 (get_f_vp na)
       );setq
      );progn then need to add the data to the viewports
  );if
  (setq clayer (getvar "clayer"));setq
  (setq lst (tnlist "layer"))
  (setq n 0);setq
  (repeat (length lst)
   (setq a (nth n lst));setq
   (if (member a lst2)
       (setq vp a)
       (setq vp nil)
   );if 
   (add_layerstate (entget (TBLOBJNAME "layer" a) '("RAK")) 
                   lstate
                   clayer
                   vp
                   nil
   );add_layerstate
   ;(spinner)
   (setq n (+ n 1));setq
  );repeat
  (setq #last_restore (list lstate (get_la_status))
          #incomplete nil
  );setq
 );defun sl

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun add_layerstate ( e1 lstate clayer vp lst5 / a b c d xd lst lst2 lst3 lst4) 
  (setq    a (cons 1000 (strcat "{" lstate))
           d (cons 1000 (strcat lstate "}"))
           c (cdr (assoc 2 e1))                     ;The layer name
          xd (cdr (assoc -3 e1))
         lst (cdr (assoc "RAK" xd))
  );setq
  (if (not lst5)
      (progn
       (setq lst2 (list 
                    a                                           ;The layer_state name
                    (cons 1070 (cdr (assoc 70 e1)))             ;The F/T L/UNlock xref bit
                    (cons 1070 (cdr (assoc 62 e1)))             ;The color and on/off 
                    (cons 1000 (local_ltype (cdr (assoc 6 e1))));The linetype
                  );list  
       );setq
       (if (equal clayer c)
           (setq lst2 (append lst2 (list '(1070 . 1))
                      );append
           );setq then it's the current layer
       );if
       (if vp
           (setq lst2 (append lst2 (list (cons 1000 vp))));setq then save the vp freeze info
       );if
       (setq lst2 (append lst2 (list d)));setq
      );progn then 
      (setq lst2 lst5);setq else info from file import
  );if 
  (if (member a lst)
      (progn 
       (setq lst3 (reverse lst));setq
       (while (setq b (member a lst3))
        (setq    b (cdr b) 
              lst3 b
        );setq
       );while
       (setq lst3 (reverse lst3));setq
       (setq lst4 lst);setq
       (while (setq b (member d lst4))
        (setq    b (cdr b) 
              lst4 b
        );setq
       );while
      );progn then  
      (setq lst3 lst);setq 
  );if
  (setq lst2 (append (list "RAK") lst3 lst2 lst4));setq
  
  (if lst
      (setq lst2 (subst lst2 (assoc "RAK" xd) xd));setq then some xdata was there and my xdata was too
      (progn
       (if xd
           (setq lst2 (append xd (list lst2)));setq then xdata was there but not mine
           (setq lst2 (list lst2));setq else
       );if
      );progn else
  );if
  (setq lst2 (append (list -3) lst2));setq the new xdata
  (if xd
      (setq e1 (subst lst2 (assoc -3 e1) e1));setq
      (setq e1 (append e1 (list lst2)));setq else
  );if
  (entmod e1) 
 );defun add_layerstate

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun list_layerstates ( / lst a b n )
  (setq a (getstring "\nLayer states to list <*>:"));setq
  (if (equal a "") (setq a "*"));if
  (setq a (strcase a));setq
  (if (setq lst (get_layerstates));setq
      (progn
       (setq n 0);setq
       (repeat (length lst) 
        (setq b (nth n lst));setq
        (if (wcmatch b a)
            (princ (strcat "\n" b))
        );if
        (setq n (+ n 1));setq
       );repeat
      );progn then
      (princ "\nNo saved layer states found")  
  );if
  (princ "\n")
 );defun list_layerstates

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun get_layerstates ( / e1 xd lst lst2 lst3 a b n j)
  (setq lst (tnlist "layer"))
  (setq b (length lst));setq
  (setq n 0);setq
  (repeat b  ;;;;(length lst)
  (setq   e1 (entget (TBLOBJNAME "layer" (nth n lst)) '("RAK"))
          xd (cdr (assoc -3 e1))
        lst2 (cdr (assoc "RAK" xd))
        ;lst2 (m_assoc 1000 lst2)
  );setq
  (setq j 0);setq
  (repeat (length lst2)
   (setq a (nth j lst2));setq
   (if (or (not (equal (car a) 1000))
           (not (equal "{" (substr (cdr a) 1 1)))
       );or  
       (setq a nil)
       (progn 
        (setq a (substr (cdr a) 2))
        (if (not (member a lst3))
            (setq lst3 (append lst3 (list a)));setq then
        );if
       );progn else
   );if 
   (setq j (+ j 1));setq
  );repeat 
   
  (setq n (+ n 1));setq
  );repeat
   
  (if lst3 
      (setq lst3 (acad_strlsort lst3));setq then
  );if
  lst3
 );defun get_layerstates

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;a full list of raw xdata from the layer table
 (defun get_layerstates_all ( / e1 xd lst lst2 lst3 a b n j)
  (setq lst (tnlist "layer"))
  (setq b (length lst));setq
  (setq n 0);setq
  (repeat b
   (setq   e1 (entget (TBLOBJNAME "layer" (nth n lst)) '("RAK"))
           xd (cdr (assoc -3 e1))
         lst2 (append lst2 (list
                                (list (cdr (assoc 2 e1)) 
                                      (cdr (assoc "RAK" xd))
                                );list
                           );list
              );append
   );setq
   (setq n (+ n 1));setq
  );repeat
  lst2
 );defun get_layerstates_all

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;returns a full list of xdata that has been remformated
 ;to match the format returned by get_la_status
 ; This will enable the abiltity
 ;to use the member function to determine if the current layer
 ;settings are saved in a layer state.
 ;For example:
 ; (if (member (get_la_status) (get_layerstates_all2))
 ;     ;then The current layer settings are saved in a layer state.
 ;     ;else The current layer settings have NOT been saved to a state.
 ; );if
 (defun get_layerstates_all2 ( / e1 xd lst lst2 lst3 a b n j)
  (setq lst (tnlist "layer"))
  (setq b (length lst));setq
  (setq n 0);setq
  (repeat b
   (setq   e1 (entget (TBLOBJNAME "layer" (nth n lst)) '("RAK"))
           xd (cdr (assoc -3 e1))
         lst2 (append lst2 (list
                                (list (cdr (assoc 2 e1)) 
                                      (cdr (assoc "RAK" xd))
                                );list
                           );list
              );append
   );setq
   (setq n (+ n 1));setq
  );repeat
  lst2
 );defun get_layerstates_all2


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun c_export_lay ( / fna lst2)
  (reg_it)
  (setq fna (getfiled "Export file name"
                      (strcat (getvar "dwgprefix")
                              (pstrip (xstrip (getvar "dwgname"))) 
                              ".lay"
                      )
                      "lay"
                      1
            );getfiled
  );setq
  (if fna
      (progn 
       (export fna)
       (setq lst2 (list (strcat "Exported layer state(s) to " fna)
                        "" ""
                  );list
       );setq
      );progn
      (setq lst2 (list "" "" ""));setq
  );if
  ;(lm_error "\n")
  lst2
 );defun c_export_lay

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun export ( fna / fh lst n a )
  (setq lst (get_layerstates_all)
         fh (open fna "w")
  );setq
  (setq n 0);setq
  (repeat (length lst)
  (setq a (nth n lst));setq
  (write_it a fh)
  (setq n (+ n 1));setq
  );repeat
  (close fh)
 );defun export

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun c_import_lay ( / fna lst2)
  (reg_it)
  (setq fna (getfiled "Import file name"
                      (strcat (getvar "dwgprefix") 
                              (pstrip (xstrip (getvar "dwgname"))) 
                              ".lay"
                      ) 
                      "lay"
                      0
            );getfiled
  );setq
  (if fna
      (progn 
       (import fna)
       (setq lst2 (list (strcat "Imported layer state(s) from " fna)
                        "" ""
                  );list
       );setq
      );progn
      (setq lst2 (list "" "" ""));setq
  );if
  ;(lm_error "\n")
  lst2
 );defun c_import_lay

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun import ( fna / lstate fh lst lst2 lst5 n a la e1 flag x)
  (setq   fh (open fna "r")
        lst2 (tnlist "layer")
  );setq
  (read-line fh)
  (while (setq a (read-line fh));setq
   (setq a (read a));setq
   (if (equal 'STR (type a))
       (progn
         (setq  a (strcase a)
               la a
         );setq
         (if (and (not (member a lst2));not
                  (not (setq x (wcmatch a "*|*")))
             );and
             (progn
              (command "_.layer" "_new" a "")
              (setq lst2 (append lst2 (list a)));setq
             );progn then
         );if
         (if (not x) 
             (setq e1 (entget (tblobjname "layer" a) '("RAK")));setq
             (princ (strcat "\nCannot import xref'd layer: \"" a "\"."))
         );if
       );progn then it's a layer name
       (progn
        (setq flag nil
              lst5 nil
        );setq
        (while (not flag)     
         (setq lst5 (append lst5 (list a)));setq 
         (if (and 
                  ;(not x)
                  (equal 'LIST (type a))
                  (equal 1000 (car a))
                  (equal "}" (substr (cdr a) (strlen (cdr a)) 1))
             );and 
             (progn
              (setq flag T)
              (if (not x)
                  (progn
                   (setq lstate (substr (cdr a) 1 (- (strlen (cdr a)) 1)));setq
                   (add_layerstate e1 lstate "0" nil lst5)
                   (setq e1 (entget (tblobjname "layer" la) '("RAK")));setq
                  );progn not an unresolved xref layer
              );if 
             );progn then
             (progn
              (if (not (setq a (read-line fh)))
                  (setq flag T);setq 
                  (setq a (read a));setq
              );if
             );progn
         );if 
        );while
       );progn else it's layer state data
   );if
  );while
  (close fh)
 );defun import

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun write_it ( lst fh / n a b)
  (setq   a (car lst)
        lst (cadr lst)
  );setq
  (print a fh)
  (setq n 0);setq
  (repeat (length lst)
   (setq b (nth n lst));setq
   (print b fh)
   (setq n (+ n 1));setq
  );repeat
 );defun write_it 

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun get_la_status ( / lst a ss)
  (while (setq a (tblnext "LAYER" (not a)))
   (setq lst (append lst (list a)));setq
  );while
  (list (getvar "clayer") 
        lst 
        (if (and (equal (getvar "tilemode") 0)
                 (setq  a (getvar "cvport") 
                       ss (ssget "_x" (list '(0 . "VIEWPORT") 
                                           '(67 . 1)
                                           (cons 69 a)
                                     );list
                          );ssget
                 );setq
           );and
           (list a (get_f_vp (ssname ss 0)))
           nil
        );if
  );list
 );defun get_la_status

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun rl ( lstate / na ss lst lst2 lst3 lst4 a b n clayer)
  
  (if (and (equal (getvar "tilemode") 0)
           (setq  a (getvar "cvport") 
                 ss (ssget "_x" (list '(0 . "VIEWPORT") 
                                     '(67 . 1)
                                     (cons 69 a)
                               );list
                    );ssget
           );setq
      );and
      (progn 
       (setq na (ssname ss 0)
             ss nil 
       );setq
      );progn then need to add the data to the viewports
  );if
  (if (not (equal #last_restore (list lstate (get_la_status))))
      (progn
       (setq #incomplete nil
                  clayer (getvar "clayer")
                     lst (tnlist "layer")
       );setq
       (setq n 0);setq
       (repeat (length lst)
        (setq lst2 (r_layerstate (nth n lst) lstate lst2 na lst4)
              lst4 (cadr lst2)
              lst2 (car lst2)
        );setq 
        (setq n (+ n 1));setq
       );repeat  

       (if (and na lst4)
           (progn
            (command "_.vplayer" "_T" "*" "")
            (setq n 0);setq
            (repeat (length lst4)
             (command (nth n lst4)) 
            (setq n (+ n 1));setq 
            );repeat
            (command "")
           );progn then do the vport thang
       );if
       (command "_.layer")
       (setq n 0);setq
       (repeat (length lst2)
        (setq lst3 (nth n lst2)
                 a (car lst3)
              lst3 (cdr lst3)   
        );setq

        (while lst3
         (setq b (car lst3));setq 
         (if (or (equal "_C" b)
                 (equal "_LT" b)
             );or
             (progn 
              (command b (cadr lst3) a)
              (setq lst3 (cdr lst3));setq
             );progn
             (progn
              (if (and (or (equal b "_F")
                           (equal b "_OFF")
                       );or           
                       (equal a (getvar "clayer"))
                  );and       
                  (progn
                   (if (equal b "_OFF")
                       (command b a "_Y")
                       (princ "\nCan't freeze the current layer.")
                   );if
                  );progn then
                  (progn
                   (command b a)
                  );progn else
              );if
             );progn
         );if 
         (setq lst3 (cdr lst3));setq
        );while  
       
       (setq n (+ 1 n));setq
       );repeat  
       (command "")
       (if (not #incomplete)  
           (setq #last_restore (list lstate (get_la_status)));setq
       );if
      );progn then need to perform a restore
  );if
  
  (princ)
 );defun rl
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun r_layerstate (la lstate lst2 na lst4 / 
                      lstate2 e1 xd lst lst3 a b c d vp flag 
                      aa bb cc 
                     )
  (setq     e1 (entget (TBLOBJNAME "layer" la) 
                       '("RAK")
                       ;'("*")
               );entget
            xd (cdr (assoc -3 e1))
           lst (cdr (assoc "RAK" xd))
       lstate2 (strcat lstate "}")
        lstate (strcat "{" lstate)
  );setq
  (if (setq lst (member (cons 1000 lstate) lst));setq
      (progn
       (setq lst (reverse (member (cons 1000 lstate2) (reverse lst)))
             lst (cdr lst) 
               a (cdr (nth 0 lst));the 70 code
              aa (cdr (assoc 70 e1)) 
               b (cdr (nth 1 lst));the color
              bb (cdr (assoc 62 e1))
               c (cdr (nth 2 lst));the linetype
              cc (cdr (assoc 6 e1))
               d (nth 3 lst)      ;possibly the clayer
             lst (cdr (cdr (cdr (cdr lst))))
       );setq

       (if (equal (car d) 1000)
           (setq vp d
                  d nil
           );setq
           (setq vp (car lst));setq        
       );if
       (if (and na 
                (equal (car vp) 1000)
                (setq vp (cdr vp));setq
                (not (equal "}" 
                            (substr vp 
                                    (strlen vp) 1
                            );substr
                     );equal
                );not      
           );and 
           (setq lst4 (append lst4 (list "_F" la "")));setq
           (progn
            (if (equal (type vp) 'STR)
                (setq lst4 (append lst4 (list "_T" la "")));setq
            );if 
           );progn then do the vport stuff
       );if
       (if (and (equal d '(1070 . 1))
                (not (equal 16 
                            (logand 16 (cdr (assoc 70 e1)))
                     );equal
                );not
           );and
           (progn 
            (setq lst3 (list la "_T" "_ON" "_Set")
                  flag 99
            );setq
           );progn then current layer set stuff
           (setq lst3 (list la)
                 flag nil 
           );setq else
       );if
       (if (not (equal (abs b) (abs bb)))
           (setq lst3 (append lst3 
                              (list "_C" (itoa (abs b)))
                      );append
           );setq then a color change is needed.
       );if
       (if (or (and flag
                    (< b 0)
               );and         ; it's going to be current and off
               (< (/ b bb) 0);or it's on/off needs to be changed
               (not (equal (abs b) (abs bb)))
           );or
           (progn
            (if (< b 0)
                (setq lst3 (append lst3 (list "_OFF")));setq then turn it off
                (progn
                 (if (equal 1 (logand 1 aa))
                     (setq lst3 (append lst3 
                                        (list "_T" "_ON" "_F"))
                     );setq then it's frozen so, turn it on quietly.
                     (setq lst3 (append lst3 (list "_ON")));setq else
                 );if
                );progn else turn it on
            );if
           );progn then it's on/off status needs to be changed
       );if 
       (if (and (not flag)          ;don't mess with f/t if it's going to be current
                (not (equal (logand 1 a) (logand 1 aa)))      
           );and
           (progn 
            (if (equal 1 (logand 1 a))
                (setq lst3 (append lst3 (list "_F")));setq then freeze it
                (setq lst3 (append lst3 (list "_T")));setq thaw it 
            );if
           );progn then needs to be frzn or thawed and it's not already that way.
       );if
       (if (not (equal (logand 4 a) (logand 4 aa)))      
           (progn 
            (if (equal 4 (logand 4 a))
                (setq lst3 (append lst3 (list "_LO")));setq then
                (setq lst3 (append lst3 (list "_UN")));setq else
            );if
           );progn then needs to be locked/unlocked and it's not already that way.
       );if
       (if (not (equal c (local_ltype cc)))
           (setq lst3 (append lst3 (list "_LT" c)));setq
       );if
       (if (> (length lst3) 1)
           (progn
            (if flag
                (setq lst2 (append (list lst3) lst2)) 
                (setq lst2 (append lst2 (list lst3)))
            );if
           );progn then
       );if
      );progn then the layer state is defined
      (progn
       (princ (strcat "\nWarning. Layer "
                      "\"" la "\""
                      " is not defined in "
                      "\"" (substr lstate 2)  "\"."
                      " Save the layer state to update it."
              );strcat 
       )
       (setq #incomplete T)
      );progn
  );if
  (list lst2 lst4)
 );defun r_layerstate


 ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun new_state ( lst new_name msg /  lstate iv str flag);
  (if (> (setq iv (load_dialog "lman.dcl")) 0)
      (progn
       (if (new_dialog "new_lman" iv);NEW_DIALOG DISPLAYS THE DIALOG
           (progn
            (set_tile "new_msg" msg)
            (set_tile "new_name" new_name)
            (setq str (strcat 
                        "(if (equal $reason 1) "
                            "(progn (setq lstate (get_tile \"new_name\")) "
                                   "(done_dialog 1) "
                             ")"  
                         ")"
                      );strcat
            );setq 
            (action_tile "new_name" str)
            (action_tile "accept"
                         "(setq lstate (get_tile \"new_name\")) (done_dialog 1)"              
            );action_tile
            (action_tile "cancle"
                         "(done_dialog 0)"
            );action_tile
             (mode_tile "new_name" 2)
            (setq flag (start_dialog));setq
            ;START_DIALOG MAKES THE BUTTONS ACTIVE
           );progn then initialize the tiles and activate the dialog box
           (progn
            (alert "Unable to display dialog box")
            (setq flag 1)
           );progn
       );if new dialog
      );progn then
      (progn
       (alert "Unable to load dialog box")
       (setq flag 1)
      );progn else
  );if load
  (if lstate
      (setq lstate (sstrip (strcase lstate)))
  );if
  (if (and (not (equal lstate new_name)) 
           (member lstate lst)
      );and
      (progn
       (if (equal 0 (warning lstate ""))
           (setq lstate (new_state lst new_name msg));setq
       );if  
     );progn
  );if
  lstate
 );defun new_state

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun warning ( name msg2 /  iv flag);
  (if (> (setq iv (load_dialog "lman.dcl")) 0)
      (progn
       (if (new_dialog "warning" iv);NEW_DIALOG DISPLAYS THE DIALOG
           (progn
            (if (not (equal msg2 ""))
                (progn 
                 (set_tile "warn_msg" (strcat msg2 " " name "?"))
                 (set_tile "warn_msg2" "")
                );progn
                (progn
                 (set_tile "warn_msg" (strcat "\"" name "\" already exist."))
                 (set_tile "warn_msg2" "Do you want to overwrite it?")
                );progn
            );if
            (action_tile "accept"
                         "(done_dialog 1)"
            );action_tile
            (action_tile "cancle"
                         "(done_dialog 0)"
            );action_tile
            (mode_tile "accept" 2)
            (setq flag (start_dialog));setq
            ;START_DIALOG MAKES THE BUTTONS ACTIVE
           );progn then initialize the tiles and activate the dialog box
           (progn
            (alert "Unable to display dialog box")
            (setq flag 1)
           );progn
       );if new dialog
      );progn then
      (progn
       (alert "Unable to load dialog box")
       (setq flag 1)
      );progn else
  );if load
  flag
 );defun warning

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
(defun reg_it ( )

  (if (not (tblsearch "appid" "RAK"))
      (if (=  (regapp "RAK") nil)               
         (princ (strcat "\nCan't register XDATA for RAK. "))
      );if 
  );if
  ;(setq *error* lm_error);setq
 );defun reg_it


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun c_dl ( / lstate lst2)
  (reg_it)
  (while (not lstate)
   (setq lstate (strcase (getstring T "\n?/Enter a name the for layer state to delete:"))
         lstate (sstrip lstate)
   );setq
   (if (equal lstate "?")
       (progn 
        (list_layerstates) 
        (setq lstate nil);setq
       );progn then
       (progn
        (if (not lst2)
            (setq lst2 (get_layerstates)) 
        );if
        (if (not (member lstate lst2))
            (progn
             (princ (strcat "\nCan't find saved layer state: " lstate))
             (setq lstate nil);setq
            );progn
        );if 
       );progn else
   );if
  );while
  (dl lstate)
  (princ)
 );defun c_dl

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun c_rl ( / lstate lst2)
  (reg_it)
  (while (not lstate)
   (setq lstate (strcase (getstring T "\n?/Enter a name the for layer state to restore:"))
         lstate (sstrip lstate)
   );setq
   (if (equal lstate "?")
       (progn 
        (list_layerstates) 
        (setq lstate nil);setq
       );progn then
       (progn
        (if (not lst2)
            (setq lst2 (get_layerstates)) 
        );if
        (if (not (member lstate lst2))
            (progn
             (princ (strcat "\nCan't find saved layer state: " lstate))
             (setq lstate nil);setq
            );progn
        );if 
       );progn else
   );if
  );while
  (rl lstate)
  (princ)
 );defun c_rl

 ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ;
 ;
 (defun c_sl ( / lstate )
  (reg_it)
  (while (not lstate)
   (setq lstate (strcase (getstring T "\n?/Enter a name the for current layer state:"))
         lstate (sstrip lstate)
   );setq
   (if (equal lstate "?")
       (progn 
        (list_layerstates) 
        (setq lstate nil);setq
       );progn then
       (progn
        (if (equal "" lstate) ;(not (snvalid lstate))
            (progn
             (princ "\nInvalid name.")
             (setq lstate nil)
            );progn      
        );if    
       );progn else
   );if
  );while
  (sl lstate)
  (princ)
 );defun c_sl



 ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 (defun putinlst ( a lst n / lst2 j)
  (setq j 0);setq
  (repeat n
  (setq lst2  (append lst2 (list (nth j lst))));setq
  (setq j (+ j 1));setq
  );repeat
  (setq lst2 (append lst2 (list a))
           j (+ j 1)
  );setq
  (repeat (- (length lst) (+ n 1))
  (setq lst2 (append lst2 (list (nth j lst))));setq
  (setq j (+ j 1));setq
  );repeat
  lst2
 );defun putinlst

 ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ;**rk_REMOVE**
 ;TAKES: an index number and a list and returns a list with the indexed
 ;       element removed
 ;
 ;      remove n lst2
 (defun rk_remove ( a lst / n lst2)
  (setq n 0);setq
  (repeat a
   (setq lst2 (append lst2 (list (nth n lst))
              );append
   );setq
   (setq n (+ n 1));setq
  );repeat
  (setq n (+ n 1));setq
  (repeat (- (length lst) a 1)
   (setq lst2 (append lst2 (list (nth n lst))
              );append
   );setq
   (setq n (+ n 1));setq
  );repeat
  lst2
 );defun rk_remove

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lman_ltypefx ( / lst n j a b na e1 )

(setq lst (tnlist "ltype"))
(setq n 0)
(repeat (length lst)
(setq  a (nth n lst)
      na (tblobjname "ltype" a)
      e1 (entget na)
);setq
(if (and (equal 16 (logand 16 (cdr (assoc 70 e1))))
         (wcmatch a "*|*")
    );and 
    (progn

     (while (and (not (equal (substr a 1 1) "|"))
                 (> (strlen a) 1)
            );and 
       (setq a (substr a 2))
     );while
     (setq a (substr a 2));setq
     (if (and (not (tblobjname "ltype" a))
              (not (equal "" a))
         );and
         (progn 
           (setq b (cdr (assoc 70 e1)) 
                 b (- b 16)
           );setq 
           (if (equal 32 (logand 32 b))
               (setq b (- b 32))
           );if  
           (setq e1 (subst (cons 2 a) (assoc 2 e1) e1)
                 e1 (subst (cons 70 b) (assoc 70 e1) e1)
           );setq

           (if (assoc 340 e1)
               (progn
                 (setq b (substr (nth n lst)
                                 1 
                                 (- (strlen (nth n lst)) (strlen a) 1)
                         );substr
                       j 0
                 );setq
                 (while (tblobjname "ltype" (strcat b "$" (itoa j) "$" a))
                      (setq j (+ j 1))
                 );while
                 (command "_.xbind" "_lt" (nth n lst))
                 (command "_.rename" "_lt" 
                          (strcat b "$" (itoa j) "$" a)
                          a
                 );command
               );progn
               (entmake e1)
           );if
           
         );progn
     );if 
    );progn
);if
(setq n (+ n 1))
);repeat

);defun lman_ltypefx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun local_ltype ( a / na e1 )

(if (and (setq na (tblobjname "ltype" a))
         (setq e1 (entget na))
         (equal 16 (logand 16 (cdr (assoc 70 e1))))
         (wcmatch a "*|*")
    );and
    (progn
     (while (and (not (equal (substr a 1 1) "|"))
                 (> (strlen a) 1)
            );and 
       (setq a (substr a 2))
     );while
     (setq a (substr a 2)) 
    );progn 
);if
a
);defun local_ltype
(princ "\n \"LMAN\" loaded.")
(princ)
