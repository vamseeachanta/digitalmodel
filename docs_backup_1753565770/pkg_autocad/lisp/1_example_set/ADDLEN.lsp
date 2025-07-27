;;;--- ADDLEN.lsp - Total lengths of objects. 
;;;    Polylines, LWPolylines, Splines, Arcs, Circles, Lines, and Ellipse
;;;
;;;
;;;
;;;--- Copyright 2005 by JefferyPSanders.com
;;;    All rights reserved.
;;;
;;;
;;;
;;;--- Created on 11/25/05
;;;


(defun C:ADDLEN()

  (setvar "cmdecho" 0)

  ;;;--- Function to get the length of an ARC entity
  (defun getArc(en)
    (command "lengthen" en "")
    (getvar "perimeter")
  )

  ;;;--- Function to get the length of a LINE entity
  (defun getLine(en)
    (setq enlist(entget en))
    (distance (cdr(assoc 10 enlist)) (cdr(assoc 11 enlist)))
  )

  ;;;--- Function to get the length of a POLY, CIRCLE, SPLINE, OR ELLIPSE
  (defun getPoly(en)
    (command "area" "Object" en)
    (getvar "perimeter")
  )  

  ;;;--- Main application

  ;;;--- Let the user select objects
  (if(setq eset(ssget))
    (progn

      ;;;--- Set up a variable to hold the length
      (setq totalLen 0)

      ;;;--- Set up a counter
      (setq cntr 0)

      ;;;--- Cycle through each entity in the selection set
      (while(< cntr (sslength eset))

        ;;;--- Get the first entity's name
        (setq en(ssname eset cntr))

        ;;;--- Get the DXF group codes
        (setq enlist(entget en))

        ;;;--- Get the type of entity
        (setq enType(cdr(assoc 0 enlist)))

        ;;;--- Get the length based on entity type
        (cond
          ((= enType "ARC"       )(setq len(getArc en)))
          ((= enType "CIRCLE"    )(setq len(getPoly en)))
          ((= enType "ELLIPSE"   )(setq len(getPoly en)))
          ((= enType "LINE"      )(setq len(getLine en)))
          ((= enType "LWPOLYLINE")(setq len(getPoly en)))
          ((= enType "POLYLINE"  )(setq len(getPoly en)))
          ((= enType "SPLINE"    )(setq len(getPoly en)))
          (T (setq len 0.0))
        )

        ;;;--- Format the entity type to be 12 characters long
        (while(< (strlen enType) 12)(setq enType(strcat enType " ")))

        ;;;--- Inform the user of progress
        (princ "\n Found ")
        (princ enType)
        (princ " with a length of: ")
        (princ (rtos len))

        ;;;--- Total the length
        (setq totalLen(+ totalLen len))
         

        ;;;--- Increment the counter to get the next entity
        (setq cntr (+ cntr 1))
      )
    )
  )

  (setvar "cmdecho" 1)
  
  ;;;--- Inform the user of the results
  (alert (strcat "\n Found " (itoa cntr) " entitie(s) with a Total Length of " (rtos totalLen)))

  ;;;--- Suppress the last echo for a clean exit
  (princ)
)