;===============================================================================
;     LAYSET - Set layer by picking an existing entity
;===============================================================================

(defun C:LC()

   (prompt "\nLAYSET - Set layer by picking an existing entity")

   (setq OLD_CMDECHO (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)

   (setq EXISTING_PICK (entsel "\nPick an entity on an existing layer : "))

   (if EXISTING_PICK
       (progn
          (setq EXISTING_ENTITY  (car EXISTING_PICK))
          (setq EXISTING_ENTLIST (entget EXISTING_ENTITY))
          (setq EXISTING_LAYER   (cdr (assoc 8 EXISTING_ENTLIST)))

          (command ".LAYER" "S" EXISTING_LAYER "")
          (prompt (strcat "\n" EXISTING_LAYER " is now the current layer."))
       )
   )

   (setvar "CMDECHO" OLD_CMDECHO)

   (prompt "\nProgram complete.")
   (princ)
)
