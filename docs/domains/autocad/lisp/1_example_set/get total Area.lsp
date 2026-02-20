;GetArea.lsp - Total the areas of selected polyline entities.
;Warning....This will also return an area for an entity that is not enclosed.

;By: Jeffery P Sanders  7/18/02   jeffery_p_sanders@yahoo.com

(defun C:GetArea()

  ;turn off the system echo
  (setvar "cmdecho" 0)

  ;set up a variable to hold the accumulated areas
  (setq myArea 0)

  ;while the user keeps making a selection
  (while(setq ent(entsel))

    ;if an entity was selected and not a point in space     
    (if(car ent)
       (progn

          ;let AutoCAD get the area of the object...cheap yet effective way out...
          ;Note: AutoCAD stores the area in the system variable "Area"
          (command "area" "Object" (car ent))

          ;print the area to the command line
          (princ (strcat "\n Total Area for this Object = " (rtos (getvar "Area"))))

          ;accumulate the area if it exist
          (if (getvar "Area")(setq myArea(+ myArea (getvar "Area"))))
       )
    )
  )

  ;ask for a text insertion point
  (setq pt1(getpoint "\n Insertion Point: "))

  ;print the area in the drawing
  (command "text" pt1 "" "" (strcat "Total Area: " (rtos myArea)))

  ;print the exit message to the command line
  (princ "\n  ...GetArea.lsp Complete.  \n ")

  ;suppress the last echo
  (princ)
)