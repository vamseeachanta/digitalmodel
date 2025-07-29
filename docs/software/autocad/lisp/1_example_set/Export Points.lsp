(defun c:PO2TXT (/ file points c i) ;POints to TeXT 
  (setq file (open (getfiled "specify output file" "c:/" "TXT" 1) "w"))
  (setq points (ssget) i 0)
  (repeat (sslength points)
    (if (= "POINT" (cdr (assoc 0 (entget (ssname points i)))))
      (setq c (cdr (assoc 10 (entget (ssname points i))))
     i (1+ i)
      )
    )
    (write-line
      (strcat (rtos (car c)) " ; "
       (rtos (cadr c)) " ; "
       (rtos (caddr c))
      ) file)
  )
  (close file)
  (Princ)
)