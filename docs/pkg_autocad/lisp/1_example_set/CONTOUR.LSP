;===============================================================================
;     CONTOUR - Plot middle points of 2 coincident curved polylines
;===============================================================================

(defun C:CONTOUR ()

 (prompt "\nCONTOUR - Plot middle points of 2 coincident curved polylines")

 (setq OLD_CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq PLINE1 (entsel "\nSelect first polyline ..."))
 (setq PLINE2 (entsel "\nSelect second polyline ..."))

 (command ".DIVIDE" PLINE1 101)
 (setq PLINE1_POINTS (list))
 (repeat 100
      (setq   LAST_POINT_ENTITY (entlast))
      (setq   LAST_POINT        (cdr (assoc 10 (entget LAST_POINT_ENTITY))))
      (setq   PLINE1_POINTS     (cons LAST_POINT PLINE1_POINTS))
      (entdel LAST_POINT_ENTITY)
 )

 (command ".DIVIDE" PLINE2 101)
 (setq PLINE2_POINTS (list))
 (repeat 100
      (setq   LAST_POINT_ENTITY (entlast))
      (setq   LAST_POINT        (cdr (assoc 10 (entget LAST_POINT_ENTITY))))
      (setq   PLINE2_POINTS     (cons LAST_POINT PLINE2_POINTS))
      (entdel LAST_POINT_ENTITY)
 )


 (setq INDEX 0)

 (command ".PLINE")
 (repeat 100
     (setq FIRST_PT  (nth INDEX PLINE1_POINTS))
     (setq SECOND_PT (nth INDEX PLINE2_POINTS))
     (setq MIDDLE_PT (list (/ (+ (car   FIRST_PT) (car   SECOND_PT)) 2.0)
                           (/ (+ (cadr  FIRST_PT) (cadr  SECOND_PT)) 2.0)
                           (/ (+ (caddr FIRST_PT) (caddr SECOND_PT)) 2.0)
                     )
     )
     (command MIDDLE_PT)
     (setq INDEX (1+ INDEX))
 )
 (command "")

 (setvar "CMDECHO" OLD_CMDECHO)

 (prompt " Program complete.")
 (princ)
)
