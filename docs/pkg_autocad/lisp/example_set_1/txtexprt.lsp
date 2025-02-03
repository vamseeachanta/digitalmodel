; ----------------------------------------------------------------------
;               (Exports a selection set of TEXT as picked)
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

(defun c:TEX ()
  (setq sset (ssget '((0 . "TEXT"))))
  (if sset
    (progn
      (setq itm 0)
      (setq num (sslength sset))
      (setq fn (getfiled "Text Export File" "" "txt" 1))
      (if (/= fn nil)
        (progn
          (setq fh (open fn "w"))
          (while (< itm num)
            (setq hnd (ssname sset itm))
            (setq ent (entget hnd))
            (setq stv (cdr (assoc 1 ent)))
            (princ (strcat stv "\n") fh)
            (setq itm (1+ itm))
          )
          (close fh)
        )
      )
    )
  )
  (setq sset nil)
  (princ)
)
