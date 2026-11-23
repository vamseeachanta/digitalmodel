;;;     GETSEL.LSP
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
;;;
;;;     Credits: Dominic Panholzer
;;;              Randy Kintzley, Error Control
;;;              Greg Robinson

(defun c:GETSEL (/ LAY    ;; Layer of the selected entity
                   ENT    ;; Entity type of seleted entity
                   SS     ;; Selection set
                   SSLST  ;; Filter list
                )

  (init_bonus_error 
         (list
           (list "cmdecho" 0
                 "expert"  0
           )
           T     ;flag. True means use undo for error clean up.  
         )       ;list  
  )              ;init_bonus_error


  (setq LAY (car(entsel "\nSelect Object on layer to Select from <*>: ")))

  (if LAY
    (setq LAY  (cdr(assoc 8 (entget LAY)))
          SSLST (list (cons 8 LAY ))
    )
  )


  (setq ENT (car(entsel "\nSelect type of entity you want <*>: ")))

  (if ENT
    (progn
      (setq ENT  (cdr(assoc 0 (entget ENT))))
      (if SSLST
         (setq SSLST (list (cons 0 ENT )))
         (setq SSLST (append (list (cons 0 ENT )) SSLIST))
      )
    )
  )

  (if SSLST
    (progn
      (cond
        ((and LAY ENT)
           (prompt (strcat "\nCollecting all " ENT " items on layer " LAY "..."))
        )
        (LAY
           (prompt (strcat "\nCollecting all entities on layer " LAY "..."))
        )
        (T
           (prompt (strcat "\nCollecting all " ENT " items in the drawing..."))
        )
      )
      (setq SS (ssget "_X" SSLST))

      (setq SS (sslength SS))

      (if (> SS 1)
        (prompt (strcat "\n" (itoa SS)
                        " items have been placed in the active selection set."
                )
        )
        (prompt "\nOne item has been placed in the active selection set.")
      )
    )
    (prompt"\nNothing selected.")
  )
  
  (restore_old_error)

  (princ)
  
);end defun
 

(prompt "\nGETSEL.LSP loaded, type GETSEL to place items in the active selection set.")

(princ)
