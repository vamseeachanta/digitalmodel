; ----------------------------------------------------------------------
;                   (Restacks a selection set of TEXT)
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

(defun c:txtstack ( / tmp ntht bitm bent sset bins oins oang itm num done
                      ndis nhnd nent chnd cent cins cdis ht)
  (setq cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "UNDO" "G")
  (setq tmp (getreal (strcat "\nDS> Interline Scale Factor <1.62>: ")))
  (if (/= tmp nil)(setq vscl tmp)(setq vscl 1.62))
  (setq tmp (getreal "\nDS> New Text Height <Unchanged>: "))
  (if (/= tmp nil)(setq ntht tmp)(setq ntht 0))
  (setq bitm (car (entsel "\nDS> Pick Base String: ")))
  (setq bent (entget bitm))
  (if (= "TEXT" (cdr (assoc 0 bent)))
    (progn
      (redraw bitm 3)
      (princ "\nDS> Select Text to Align: ")
      (setq sset (ssget '((0 . "TEXT"))))
      (redraw bitm 4)
      (if sset
        (progn
          (if (> ntht 0)
            (progn
              (setq bent (subst (cons 40 ntht)(assoc 40 bent) bent))
              (entmod bent)
            )
          )
          (if (> (cdr (assoc 72 bent)) 0)
            (setq bins (cdr (assoc 11 bent)))
            (setq bins (cdr (assoc 10 bent)))
          )
          (setq bang (cdr (assoc 50 bent)))
          (setq tang (- bang (/ PI 2)))
          (setq nins bins)
          (ssdel bitm sset)
          (while (> (sslength sset) 0)
            (setq num (sslength sset) itm 0)
            (setq ndis 99999999.9)
            (while (< itm num)
              (setq chnd (ssname sset itm))
              (setq cent (entget chnd))
              (if (> (cdr (assoc 72 cent)) 0)
                (setq cins (cdr (assoc 11 cent)))
                (setq cins (cdr (assoc 10 cent)))
              )
              (setq cdis (distance bins cins))
              (if (< cdis ndis)
                (setq ndis cdis nhnd chnd nent cent)
              )
              (setq itm (1+ itm))
            )
            (if (> ntht 0)
              (progn
                (setq nent (subst (cons 40 ntht)(assoc 40 nent) nent))
                (setq txht ntht)
              )
              (setq txht (cdr (assoc 40 nent)))
            )
            (setq nins (polar nins tang (* txht vscl)))
            (if (> (cdr (assoc 72 nent)) 0)
              (setq nent (subst (cons 11 nins)(assoc 11 nent) nent))
              (setq nent (subst (cons 10 nins)(assoc 10 nent) nent))
            )
            (setq nent (subst (cons 50 bang)(assoc 50 nent) nent))
            (entmod nent)
            (ssdel nhnd sset)
          )
        )
      )
    )
  )
  ;
  (setq sset nil)
  (command "UNDO" "E")
  (setvar "CMDECHO" cmdecho)
  (princ)
)
