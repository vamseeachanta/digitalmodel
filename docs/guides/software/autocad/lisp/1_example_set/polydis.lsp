; ----------------------------------------------------------------------
;               (Returns the length of a selected polyline)
;            Copyright (C) 1998 DotSoft, All Rights Reserved
;                      Website: www.dotsoft.com
; ----------------------------------------------------------------------
; DISCLAIMER:  DotSoft Disclaims any and all liability for any damages
; arising out of the use or operation, or inability to use the software.
; FURTHERMORE, User agrees to hold DotSoft harmless from such claims.
; DotSoft makes no warranty, either expressed or implied, as to the
; fitness of this product for a particular purpose.  All materials are
; to be considered ‘as-is’, and use of this software should be
; considered as AT YOUR OWN RISK.
; ----------------------------------------------------------------------

(defun c:polydis ()
  (setq cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq tmp (entsel "\nSelect Polyline: "))
  (if (/= tmp nil)
    (progn
      (setq hnd (car tmp))
      (command "_.AREA" "_E" hnd)
      (princ "\nPolyline Length: ")
      (princ (getvar "PERIMETER"))
    )
  )
  (setvar "CMDECHO" cmdecho)
  (princ)
)
