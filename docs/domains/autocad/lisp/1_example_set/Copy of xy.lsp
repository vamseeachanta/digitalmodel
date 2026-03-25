
(defun c:xy()
(setvar "cmdecho" 1)

(setq b1(getpoint "\nClick point: "))

(setq sb(strcat(rtos (car b1))))
(setq sb1(strcat(rtos (cadr b1))))



(setq b2(getpoint "\nBasepoint:" ))

(command "text" b2 "" "" sb)
(command "text" (list (car b2)(- (cadr b2) (* (getvar "TEXTSIZE") 1.6)))
             "" "" sb1)

(setq ra(* (getvar "TEXTSIZE") 0.09))
(command "CIRCLE" b2 ra)
(command "line" b1 b2 "")
(prompt "\nwritten by Tariq and company")
(prompt "\nEnter xy  to start")

(princ)
)
