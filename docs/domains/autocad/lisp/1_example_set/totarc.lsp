;                         TOTARC.LSP
;
;This AutoLISP routine adds up all the line and arc lengths selected.
;
;                                             Peter C.Hagan
;                                             Coral Sea Drafting
;                                             16 July 1995
;
; ---------------------------------------------------------------

;                         DISCLAIMER
; Users of this program acknowledge this disclaimer of warranty:
; This program is supplied as is.  Coral Sea Drafting and
; P.C.Hagan disclaim all warranties, expressed or implied, including, 
; without limitation, the warranties of merchantability and of
; fitness of this program for any purpose.  Coral Sea Drafting and
; P.C.Hagan assume no liability for damages direct, indirect, or
; consequential, which may result from the use of this program.
; We repeat Coral Sea Drafting and P.C.Hagan 
; do not warrant this program.  

;----------------------------------------------------------------
(defun c:TOTARC ();(/ ss ssl K sum n PT1 PT2 L ea sa r rad)
  (princ "\nPick the lines to be totalled... ")
  (setq ss (ssget)
       ssl (sslength ss)
         K -1
        sum 0
  )
  (repeat ssl
    (setq K (+ K 1))
    (setq n (ssname ss K))
    (setq e (entget n))
    (if (setq PT2 (cdr (assoc 11 e)))
      (progn
        (setq PT1 (cdr (assoc 10 e)))
        (setq L (distance PT1 PT2))
      )
    )
    (if (setq ea (cdr (assoc 51 e)))
      (progn
        (setq r (cdr (assoc 40 e)))
        (setq sa (cdr (assoc 50 e)))
        (if (< ea sa)
          (setq ea (+ ea 6.28319))
        )
        (setq rad (- ea sa))
        (setq L (* r rad))
      )
    )
    (setq sum (+ sum L))
  )
  (princ "\nThe total length = ")
  (princ sum)
  (prin1)
)
