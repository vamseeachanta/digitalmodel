;Tip1369B.LSP:   SUB.LSP   Subtract Selected Numbers   (P.K.Yousuf)

; To substract numerical text entities from another
; set of numerical
; entities.
; -----------------------------------------------------------------------
(defun C:- (/ MS ML INDEXM CM MA MD MB CM S LG INDEX
              A B C C1 D
             )
  (prompt "SELECT the MAIN Number(s): ")
  (setq MS     (ssget)
        ML     (sslength MS)
        INDEXM 0
        CM     0
  ) ;_ end of setq
  (while (/= INDEXM ML)
    (setq MA (entget (ssname MS INDEXM))
          MD (cdr (assoc 0 MA))
    ) ;_ end of setq
    (if (= MD "TEXT")
      (progn
        (setq MB (atof (cdr (assoc 1 MA)))
              CM (+ CM MB)
        ) ;_ end of setq
      ) ;_ end of progn
    ) ;_ end of if
    (setq INDEXM (1+ INDEXM))
  ) ;_ end of while
  (prompt "\nSelect the NUMBERS to be DUDUCTED: ")
  (setq S     (ssget)
        LG    (sslength S)
        INDEX 0
        C     0
  ) ;_ end of setq
  (while (/= INDEX LG)
    (setq A (entget (ssname S INDEX))
          D (cdr (assoc 0 A))
    ) ;_ end of setq
    (if (= D "TEXT")
      (progn
        (setq B (atof (cdr (assoc 1 A)))
              C (+ C B)
        ) ;_ end of setq
      ) ;_ end of progn
    ) ;_ end of if
    (setq INDEX (1+ INDEX))
  ) ;_ end of while
  (setq C1 (- CM C))
  (princ "\nNET VALUE = ")
  (princ C1)
  (princ)
    ; --------------------------------------------------------------------
)   ;---end defun 

