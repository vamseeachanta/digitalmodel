(DEFUN C:Yy ()
 (SETQ CMD (GETVAR "CMDECHO"))
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "W")
 (SETVAR "CMDECHO" 1)
 (PROMPT"   Sua duong giong kich thuoc theo truc Y ")
 (SETQ SET (SSGET))
 (PROMPT"   Ban hay chon duong giong moi ")
 (SETQ P1 (GETPOINT))
 (SETQ Y3 (CADR P1))
 (SETQ QUANT (SSLENGTH SET))
 (SETQ INDEX 0)
  (WHILE (< INDEX QUANT)
   (IF (AND(= "DIMENSION" (CDR (ASSOC 0 (SETQ A (ENTGET (SSNAME SET INDEX))))))
       )
    (PROGN
     (SETQ L13 (ASSOC 13 A))
     (SETQ M13 (CDR L13))
     (SETQ L14 (ASSOC 14 A))
     (SETQ M14 (CDR L14))
     (SETQ P13 (LIST 13 (CAR M13) Y3 (CADDR M13)))
     (SETQ T13 (CDR P13))
     (SETQ P14 (LIST 14 (CAR M14) Y3 (CADDR M14)))
     (SETQ A (SUBST P13 L13 A))
     (SETQ A (SUBST P14 L14 A))
     (ENTMOD A)			
    )
   )
  (SETQ INDEX (+ INDEX 1))
 )
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "P")
 (SETVAR "CMDECHO" CMD)
)
