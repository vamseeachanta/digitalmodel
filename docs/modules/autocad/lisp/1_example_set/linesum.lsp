; ----------------------------------------------------------------------
;               (Returns the sum of selected line objects)
;            Copyright (C) 1997 DotSoft, All Rights Reserved
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

; watch out for lines with diffent elevations at the endpoints

(defun C:LINESUM ()
  (setq sset (ssget '((0 . "LINE"))))
  (if sset
    (progn
      (setq tot 0.0)
      (setq num (sslength sset) itm 0)
      (while (< itm num)
        (setq hnd (ssname sset itm))
        (setq ent (entget hnd))
        (setq pt1 (cdr (assoc 10 ent)))
        (setq pt2 (cdr (assoc 11 ent)))
        (setq dis (distance pt1 pt2))
        (setq tot (+ tot dis))
        (setq itm (1+ itm))
      )
      (princ (strcat "\nTotal Distance = " (rtos tot)))
    )
  )
  (princ)
)
