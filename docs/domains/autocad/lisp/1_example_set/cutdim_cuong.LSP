 


******************************************************************************
;*************************************************************************** *
;*                                                                         * * 
;* NAME     :        TRIM DIMENSION                                        * *
;*                                                                         * * 
;* FUNCTION :        ALLOWS TOU TO REVISE POSITION OF DIMENSION EXTENSION  * *
;*                   "DEFINING POINT" ALONG ITS "X" AXIS                   * *
;*                                                                         * *
;* WRITTEN  :        NGUYEN HAI NAM ACT (VIETNAM)                          * *
;*                   1-1-2000                                              * *
;*                                                                         * *
;*************************************************************************** * 
******************************************************************************
(DEFUN C:YY ()
 (SETQ CMD (GETVAR "CMDECHO"))
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "W")
 (SETVAR "CMDECHO" 1)
 (PROMPT"\nSelect vertical dimension(s) extensions to be aligned")
 (SETQ SET (SSGET))
 (PROMPT"\nSelect new extension position")
 (SETQ P1 (GETPOINT))
 (SETQ X3 (CAR P1))
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
     (SETQ P13 (LIST 13 X3 (CADR M13) (CADDR M13)))
     (SETQ P14 (LIST 14 X3 (CADR M14) (CADDR M14)))
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
 (PRINC)
)
*************************************
(DEFUN C:XX ()
 (SETQ CMD (GETVAR "CMDECHO"))
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "W")
 (SETVAR "CMDECHO" 1)
 (PROMPT"\nSelect dimension(s) to be aligned")
 (SETQ SET (SSGET))
 (PROMPT"\nSelect new dimension position")
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
******************************************************
;*************************************************************************
;*                                                                       *
;* NAME     :        X,Y-ALDIM.LSP                                       *
;*                                                                       *
;* FUNCTION :        ALLOWS GROUPS OF DIMENSIONS TO BE ALIGNED ALONG A   *
;*                   COMMON "X" AXIS.                                    *
;*                                                                       *
;* WRITTEN  :       NGUYEN HAI NAM  (VIET NAM)                           *
;*                   1-1-2000                                            *
;*                                                                       *
;*************************************************************************
(DEFUN C:DY ()
 (SETQ CMD (GETVAR "CMDECHO"))
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "W")
 (SETVAR "CMDECHO" 1)
 (PROMPT"\nSelect dimension(s) to be aligned")
 (SETQ SET (SSGET))
 (PROMPT"\nSelect new dimension position")
 (SETQ P1 (GETPOINT))
 (SETQ X3 (CAR P1))
 (SETQ QUANT (SSLENGTH SET))
 (SETQ INDEX 0)
  (WHILE (< INDEX QUANT)
   (IF (AND(= "DIMENSION" (CDR (ASSOC 0 (SETQ A (ENTGET (SSNAME SET INDEX))))))
       )
    (PROGN
     (SETQ L10 (ASSOC 10 A))
     (SETQ M10 (CDR L10))
     (SETQ L11 (ASSOC 11 A))
     (SETQ M11 (CDR L11))
     (SETQ P10 (LIST 10 X3 (CADR M10) (CADDR M10)))
     (SETQ A (SUBST P10 L10 A))
     (ENTMOD A)			
    )
   )
  (SETQ INDEX (+ INDEX 1))
 )
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "P")
 (SETVAR "CMDECHO" CMD)
)

************************************
(DEFUN C:DX ()
 (SETQ CMD (GETVAR "CMDECHO"))
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "W")
 (SETVAR "CMDECHO" 1)
 (PROMPT"\nSelect dimension(s) to be aligned")
 (SETQ SET (SSGET))
 (PROMPT"\nSelect new dimension position")
 (SETQ P1 (GETPOINT))
 (SETQ Y3 (CADR P1))
 (SETQ QUANT (SSLENGTH SET))
 (SETQ INDEX 0)
  (WHILE (< INDEX QUANT)
   (IF (AND(= "DIMENSION" (CDR (ASSOC 0 (SETQ A (ENTGET (SSNAME SET INDEX))))))
       )
    (PROGN
     (SETQ L10 (ASSOC 10 A))
     (SETQ M10 (CDR L10))
     (SETQ P10 (LIST 10 (CAR M10) Y3 (CADDR M10)))
     (SETQ A (SUBST P10 L10 A))
     (ENTMOD A)			
    )
   )
  (SETQ INDEX (+ INDEX 1))
 )
 (SETVAR "CMDECHO" 0)
 (COMMAND "UCS" "P")
 (SETVAR "CMDECHO" CMD)
)


************************************************************

  






