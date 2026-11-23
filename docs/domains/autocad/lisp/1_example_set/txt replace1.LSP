;===============================================================================
;     TREPLACE - Text replace
;===============================================================================

(defun C:TR ()

      (prompt "\nTREPLACE - Text replace")

      (setq SELECTION_SET (ssget))
      (setq NO_OF_ITEMS (sslength SELECTION_SET))

      (setq REPLACE_STRING (getstring T "\nReplacement text string : "))

      (setq INDEX 0)

      (repeat NO_OF_ITEMS

          (setq ENTITY_NAME (ssname SELECTION_SET INDEX))
          (setq ORIGINAL_ENTITY_LIST (entget ENTITY_NAME))
          (setq ENTITY_TYPE (cdr (assoc 0 ORIGINAL_ENTITY_LIST)))

          (if (equal ENTITY_TYPE "TEXT")
              (progn
                 (setq OLD_STRING (assoc 1 ORIGINAL_ENTITY_LIST))
                 (setq NEW_STRING (cons  1 REPLACE_STRING))
                 (setq NEW_ENTITY_LIST
                    (subst NEW_STRING OLD_STRING ORIGINAL_ENTITY_LIST)
                 )
                 (entmod NEW_ENTITY_LIST)
              )
          )

          (setq INDEX (1+ INDEX))
      )

      (prompt "\nProgram complete.")
      (princ)
)
