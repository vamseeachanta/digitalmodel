;===============================================================================
;     FLEXHOSE - Draw flexible hose
;===============================================================================

(defun C:FLEXHOSE ()

   (prompt "\nFLEXHOSE - Draw flexible hose")

   (setq OLD_CMDECHO (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)

   (setq DIAMETER    (getdist "\nWhat is the hose diameter ? "))

   (setq BEND_RADIUS (getdist "\nWhat is the bend radius ? "))

   (prompt "\nDraw the center line of the hose routing ...")

   (setq PT (getpoint "\nFrom point : "))

   (command ".PLINE" PT)

   (while (setq PT (getpoint PT "\nTo point : "))
          (command PT)
          (setq LASTPT PT)
   )
   (command)

   (setq CENTER_LINE (entlast))

   (command ".FILLET" "R" BEND_RADIUS)
   (command ".FILLET" "P" CENTER_LINE)

   (setq OLD_PICKBOX (getvar "PICKBOX"))
   (setvar "PICKBOX" 0)

   (command ".OFFSET" (/ DIAMETER 2.0) LASTPT "@-1,-1" LASTPT "@1,1" "")

   (entdel CENTER_LINE)

   (setvar "CMDECHO" OLD_CMDECHO)
   (setvar "PICKBOX" OLD_PICKBOX)

   (prompt "\nProgram complete.")

   (princ)

)
