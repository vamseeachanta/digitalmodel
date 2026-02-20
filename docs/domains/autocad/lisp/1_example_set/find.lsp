;;;     FIND.LSP
;;;     Copyright (C) 1997 by Autodesk, Inc.
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
;;;     Credits: Bill Kramer, Q.C.
;;;              Dominic Panholzer 
;;;              Randy Kintzley, Error Handling
;;;              Greg Robinson



;|   Check to see if AI_UTILS is loaded, If not, try to find it,
     and then try to load it.

    If it can't be found or it can't be loaded, then abort the
    loading of this file immediately, preserving the (autoload)
    stub function.
|;

(cond
   (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.

   (  (not (findfile "ai_utils.lsp"))                     ; find it
      (ai_abort "DDCHPROP"
         (strcat "Can't locate file AI_UTILS.LSP."
   "\n Check support directory.")))

   (  (eq "failed" (load "ai_utils" "failed"))            ; load it
   (ai_abort "DDCHPROP" "Can't load file AI_UTILS.LSP"))
);end conditional

(if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
   (ai_abort "DDCHPROP" nil)        ; a Nil <msg> supresses
)                                    ; ai_abort's alert box dialog.




;    metering prompt
(defun gc_meter (gcm_pr gc_num gc_max)
   (prompt 
      (strcat "\r" gcm_pr " ("
         (rtos(*(/(float(1+ gc_num))(float gc_max))100)2 0)
         "%)"
      ) ;strcat
   ) ;prompt   
) ;defun

(defun C:FIND
   (/ mode replace_loc _accept _replace text text_string
      index pointer sset id text_string_length *olderror*
   )
(init_bonus_error 
       (list
         (list "cmdecho" 0
         )
          T     ;flag. True means use undo for error clean up.  
       );list  
);init_bonus_error

   ;process an ok
   (defun _accept ()
      (cond  
         (
            (or 
               (= 
                  (get_tile "find")
                  ""
               )
              ; (= 
              ;    (get_tile "replace")
              ;    ""
              ; )
            )  
            (set_tile "error" "Empty or invalid input")
         )
         (
            (=
               (get_tile "find")
               (get_tile "replace")
            )
            (set_tile "error" "Find and replace are identical")
         )    
         (T
            (setq #find_string (get_tile "find"))
            (setq #replace_string (get_tile "replace"))
            (setq #case_sensitive (get_tile "case"))
            (setq #global (get_tile "global"))
            (done_dialog 1)
         )   
      )  
   )

   ;function for prompt to replace
   (defun _replace ()
      (new_dialog "find2" id "" replace_loc)
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "accept" "(setq replace_loc (done_dialog 1))")
      (action_tile "skip"   "(setq replace_loc (done_dialog 0))")
      (action_tile "auto"   "(done_dialog 2)")
      (set_tile
         "error" 
         (strcat 
            (rtos (1+ index) 2 0)
            " of "
            (rtos (sslength sset) 2 0)
         )
      )
      (start_dialog)
   )  

   ;set up the dialog identification
   (setq id (load_dialog "find"))

   ;open dialog and store location as a global
   (new_dialog "find" id)

   ;set those defaults
   (if
      #find_string
      (set_tile "find" #find_string)
   )
   (if 
      #replace_string
      (set_tile "replace" #replace_string)
   )
   (if
      #case_sensitive
      (set_tile "case" #case_sensitive)
   )
   (if
      #global
      (set_tile "global" #global)
   )

   ;set up callbacks
   (action_tile "accept"  "(_accept)")
   (mode_tile "find" 2)

   ;process the look for the callbacks
   (if
      ;make changes if ok is picked
      (=
         (start_dialog)
         1
      )
      (progn
         ;get a selection set if not global
         (if (/= #global "1")
            (while
               (=
                  (setq sset (ssget (list (cons 0 "TEXT"))))
                  nil
               )
               (prompt "\nNo entities selected")
            )
            (setq sset (ssget "_X"(list(cons 0 "TEXT"))))
         )
         (setq index 0)
         ;go through the selection set
         (while (and sset
            (/=
               (setq text (ssname sset index))
            nil))
            ;go through each string
            (setq pointer 1)
            (setq text_string (cdr (assoc 1 (entget text))))
            (if 
               (<=
                  (strlen #find_string)
                  (strlen text_string)
               )
               (progn
                  (setq text_string_length (strlen text_string))
                  ;go until you reach the end of the string
                  (while
                     (< 
                        pointer
                        (+ (- text_string_length (strlen #find_string)) 2)
                     )
                     (if 
                        (= 
                           (if
                              (= #case_sensitive "1")
                              (substr
                                 text_string
                                 pointer
                                 (strlen #find_string)
                              )
                              (strcase 
                                 (substr
                                    text_string 
                                    pointer 
                                    (strlen #find_string)
                                 )
                              )
                           )
                           (if (= #case_sensitive "1")
                              #find_string
                              (strcase #find_string)
                           )
                        )
                        (progn
                           (redraw text 3)
                           (if (/= mode 2)
                              (if (/= #global "1")
                                 (setq mode (_replace))
                                 (setq mode 2)
                              )
                           )   
                           (if (> mode 0)
                              (progn
                                 (setq text_string
                                    (if (= pointer 1)
                                       (strcat
                                          #replace_string
                                          (substr
                                             text_string
                                             (+ pointer (strlen #find_string))
                                          )
                                       )
                                       (strcat 
                                          (substr 
                                             text_string
                                             1
                                             (- pointer 1)
                                          )
                                          #replace_string
                                          (substr
                                             text_string
                                             (+ pointer (strlen #find_string))
                                          )
                                       )
                                    )
                                 )
                                 (entmod 
                                    (subst 
                                       (cons 1 text_string)
                                       (assoc 1 (entget text))
                                       (entget text)
                                    )
                                 )
                                 (setq 
                                    pointer 
                                    (+ pointer (strlen #replace_string))
                                 )
                                 (setq
                                    text_string_length
                                    (strlen text_string)
                                 )
                              )
                           )
                           (redraw text 4)
                           (setq pointer (1+ pointer))
                        )
                        (setq pointer (1+ pointer))
                     )
                  )
               )
            )
            (gc_meter "Changing text" index (sslength sset))
            (setq index (1+ index))
         )
      )
   )
   (unload_dialog id)
(restore_old_error)
   (princ)
) ;defun find 
(princ "FIND Loaded. Find and Replace DText Strings. FINDHELP for Help\n")                   
(princ)

(defun c:findhelp (/)
(prompt "The find command preforms a find and replace on Dtext strings.\n")
(prompt "This version does not support Mtext.\n")
(prompt "Usage is through the Dialog box, note the options! \n")
(prompt "User can preform Selected or Global replacement.\n")
(prompt "Case is supported, check option in Dialog.\n")
(prompt "If the Dialog fails to appear, check your Support path.\n")
(prompt "Files used: Find.lsp & Find.dcl\n")
(prompt "Type FIND to begin command.\n")
(textscr)
(princ)
)
