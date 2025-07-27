;;;     TEXTFIT.LSP
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
;;;     Credits:
;;;              Bill Kramer, Q.C.
;;;              Phil Kreiker Q.C.
;;;              Greg Robinson 03/21/97
;;; 
;;;


;Main*************************************************************************************
(Defun c:textfit ( /
        setsnapang
        arg LL-xyz UR-x LL-y LR-xy
        ename
        TextEnt
        NewEnd
        TMP
        START NewPt
        Val LTC_%
        )
  (init_bonus_error
    (List
        (List "cmdecho" 0 "snapang" 0)
        T     ;flag. True means use undo for error clean up.  
     ) ;list  
  ) ;init_bonus_error

;;;End Error control

  (defun setsnapang (arg /)
    (setvar "snapang" (angtof (angtos (cdr (assoc 50 arg)) 0 8) 0 ))
  );end defun setsnapang

  (Defun LL-xyz (arg)                             ;Lower Left xyz coord
     (CAR (TextBox arg))
  )
  (Defun UR-x (arg)                               ;Upper Right x coord
     (CAADR (TextBox arg))
  )
  (Defun LL-y (arg)                               ;Lower left y coord
     (CADAR (TextBox arg))
  )
  (Defun LR-xy (arg)                              ;Lower right xy coord
     (List (UR-x arg) (LL-y arg))
  )

   (Setq
      ename (CAR
               (EntSel
                  "\nSelect Text to stretch/shrink:"
               )
            )

      Textent (If ename (EntGet ename))
   )   

   (If (= (CDR (Assoc 0 textent)) "TEXT")
      (Progn
         (initget 0 "Start")
         (Setq
            NewEnd  (Distance
                           (LR-xy Textent)
                           (LL-xyz Textent)
                        )
         )
         (setsnapang Textent)                 ;set snap along text entity
         (setvar "ORTHOMODE" 1)                   ;drag along the text
         (setq 
            TMP (getpoint (cdr (assoc 10 Textent)) "\nStarting Point/<Pick new ending point>: ")
         )
         (setvar "snapang" 0)
         (cond
           ((= (type TMP) 'STR) ;;new starting point to be selected
               (setq Start (getpoint "\nPick new starting point: "))
               (if Start 
                  (progn
                  (command "_UCS" "_E" (cdr (assoc -1 textent)))
                  (setvar "ORTHOMODE" 1)
                  (setq NewPt
                        (if Start
                            (getpoint (trans Start 0 1) " ending point: ")
                            nil
                        )
                  )
                  (if NewPt (setq NewPt (trans NewPt 1 0)))
                  (setvar "ORTHOMODE" 0)
                  (command "_UCS" "_W")
                  )
                )
           )
           ((not (null TMP))    ;;new ending point selected

               (setq Start (cdr (assoc 10 Textent))
                     NewPt TMP)
           )
           (t  (setq Start nil
                     NewPt nil)
           )
         )
         (if (and Start NewPt) (progn
           (setq Val (Assoc 41 Textent) ;;current width factor
                 Val (if Val (cdr Val) 1.0)
            
                 LTC_%       
                        (*
                           (/
                              (Distance Start NewPt)
                              NewEnd
                           )
                           Val
                        )
                 textent (Subst (cons 41 LTC_%)
                                    (assoc 41 textent)
                                    textent)
                 textent (subst (cons 10 Start)
                                    (assoc 10 textent)
                                    textent)
                 textent (subst (cons 11 NewPt)
                                    (assoc 11 textent)
                                    textent)
           )
           (EntMod textent)
           (EntUpd (cdr (assoc -1 textent)))
         ))  ;;end of points check
   ) ;End do if T progn
 )
 (restore_old_error)
 (Princ)
) ;end defun

(defun c:TFHELP (/)

(prompt " TEXTFIT will change the width factor of the selected text, \n")
(prompt " to fit within the user specified points.\n")
(prompt "\n")
(prompt " TEXTFIT will prompt:  Select Text to stretch/shrink:\n")
(prompt " The user is expected to select the text.\n")
(prompt "\n")
(prompt " TEXTFIT will then prompt:  Starting Point/<Pick new ending point>: \n")
(prompt " At which time the user can specify a new ending point \n")
(prompt "                          or\n")
(prompt " The user can provide the letter \"S\" for a new starting point\n")
(prompt " to which TEXTFIT will prompt:  Pick new starting point:  \n")
(prompt " and:  ending point: \n")
(textscr)
(princ)
)


(Prompt
   "\n TEXTFIT loaded.  Type TEXTFIT to stretch/shrink text between points"
)
(prompt "\n    Type TFHELP for help")
(Princ)
