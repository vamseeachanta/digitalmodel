(defun c:xyz()
(setvar "cmdecho" 0)
(setvar "osmode" 1)
(setvar "LUPREC" 3)
(setq b1(getpoint "\nClick point: "))

(setq sb(strcat(rtos (car b1))))
(setq sb1(strcat(rtos (cadr b1))))
(setq sb2(strcat(rtos (caddr b1))))

(setvar "osmode" 0)
(setq b2(getpoint "\nBasepoint: "))

(command "text" b2 "" "" sb)
(command "text" (list (car b2)(- (cadr b2) (* (getvar "TEXTSIZE") 1.5)))
             "" "" sb1)
(command "text" (list (car b2)(- (cadr b2) (* (getvar "TEXTSIZE") 3)))
             "" "" sb2)

(setq ra(* (getvar "TEXTSIZE") 0.1))
(command "CIRCLE" b1 ra)
(command "line" b1 b2 "")

(princ)
)
(prompt "\nwritten by IMRAN and company")
(prompt "\nEnter xyz to start")