(defun c:HatchAreas (/ sset i area obj)
(if (>= (atof (substr (getvar "acadver") 1 4)) 16.2)
(progn
(prompt "\nSelect hatches: ")
(if (setq sset (ssget '((0 . "hatch"))))
(progn
(setq
i (1- (sslength sset))
area 0)
(while (>= i 0)
(setq
obj (vlax-ename->vla-object (ssname sset i))
area (+ area (vla-get-area obj)))
(setq i (1- i)))
(alert
(strcat
"\nTotal area = "
(if (or (= (getvar "lunits") 3) (= (getvar "lunits") 4))
(strcat
(rtos area 2)
" sq. in. ("
(rtos (/ area 144) 2)
" sq. ft.)")
(rtos area))))))))
(princ))