;;;   BLOCKQ.lsp
;;;   Copyright (C) 1991-92 by Autodesk, Inc.
;
;     Permission to use, copy, modify, and distribute this software 
;     for any purpose and without fee is hereby granted, provided 
;     that the above copyright notice appears in all copies and that 
;     both that copyright notice and this permission notice appear in 
;     all supporting documentation.
;
;     THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED
;     WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR
;     PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.
;     ****************************************************************
;;; 
;;;   by Carl B. Bethea
;;;   24 April 1991
;;; 
;;;---------------------------------------------------------
;;;   DESCRIPTION
;;;   
;;;   (listb <block name> <entity type>)
;;;   LISTB walks through the entities in a block definition. It also lets
;;;   you specify only one entity type to report from the definiton. For 
;;;   instance, (listb "myblock" "attdef") will display only the attribute 
;;;   definitons in the block. To list all of the entities in the block, 
;;;   supply a NIL argument for <entity type>, as in (listb "myblock" nil).
;;;
;;;   C:BLOCK? serves as a front-end for LISTB. It lets you either supply a 
;;;   block name or pick an insterted block. Then you can specify an entity 
;;;   type to search for, or accept the default to list all entities in 
;;;   the definition.
;;; 
;;;-- listb ------------------------------------------------
;;;   list the entities in a block definition <bname>
;;;
(defun listb (bname etype / dxf data wait)

   ;; return value from a dotted pair
   (defun dxf (x)(cdr (assoc x data)))

   ;; wait for key press
   ;; if ESC, then stop
   (defun wait ()
      (print data)
      (grread (grread T)); clear the buffer
      (terpri)
      (if (and 
             (setq data (entnext (dxf -1)))   
             (/= 27 (cadr (grread)))
          )
          (setq data (entget data '("*")))
          (setq data nil)
      )
   );wait

   ;; begin the main program
   (textscr)
   (prompt "\nPress ESC to exit or any key to continue.")
   (terpri)
   (print (setq data (tblsearch "block" bname)))
   (terpri)
   (setq data (dxf -2)               ; get first entity
         data (entget data '("*"))   ; get assoc list
   )
   (if etype (setq etype (strcase etype)))
   (while data
      (cond
         (etype 
            (if (= etype (dxf 0))
                (wait)      
                (setq data
                   (if (setq data (entnext (dxf -1)))   
                       (entget data '("*"))
                   )
                )
            );if
         );etype
         (T (wait))
      );cond
   );while
   (princ)
)
;;;
;;;-- c:block? -----------------------------------------------
;;;   display a block definition,
;;;   optionally show only certain components
;;;
(defun c:block? (/ bname etype dxf data)

   (defun dxf (x)(cdr (assoc x data)))

   (if (= "" (setq bname 
         (getstring "\nBlock name/<Return to select>: ")
       ))
       (if (setq bname (entsel "Pick a block: "))
           (if (and
                  (setq  data (entget (car bname)))
                  (or (= "INSERT" (dxf 0))
                      (= "DIMENSION" (dxf 0))
                  )
               );and        
               (setq bname (dxf 2))
               (setq bname nil)
           );if    
       );if
   );if
   (cond 
      (bname 
         (if 
            (= "" (setq etype 
                  (getstring "\nAn entity type/<Return for all>: ")
            ))
            (setq etype nil)
         );if
         (listb bname etype)
      )
      (T  (print " no block found."))
   );cond
)  
;;;--- end of file -----------------------------------------
