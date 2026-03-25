
(defun c:z()
(setvar "cmdecho" 0)

(setq b1(getpoint "\nClick point: "))

(setq sb(strcat(rtos (car b1))))
(setq sb1(strcat(rtos (cadr b1))))



(setq b2(getpoint "\nBasepoint: "))


(command "text" (list (car b2)(- (cadr b2) (* (getvar "TEXTSIZE") 0)))
             "" "" sb1)

(setq ra(* (getvar "TEXTSIZE") 0.1))

(command "line" b1 b2 "")
(prompt "\nwritten by IMRAN HALIM")
(prompt "\nEnter xy to start")

(princ)
)
