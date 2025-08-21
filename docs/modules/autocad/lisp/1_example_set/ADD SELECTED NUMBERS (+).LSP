;Tip1369A.LSP:   ADD.LSP   Add Selected Numbers   (P.K.Yousuf)

;Prepared by P.K.Yousuf 

(defun C:+ (/ S1 LG INDEX A B C D)
  (setq S1    (ssget)
        LG    (sslength S1)
        INDEX 0
        C     0
  ) ;_ end of setq
  (while (/= INDEX LG)
    (setq A (entget (ssname S1 INDEX))
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
  (princ "\nTOTAL VALUE = ")
  (princ C)
  (princ)
)   ;end defun
