(defun c:txt()
(setq pt (getpoint"\nENTER POINT TO BE IDENTIFIED:"))
(setq st (getpoint"\nENTER TEXT START POINT:"))
(setq x (car pt))
(setq a (abs x))
(setq offset (rtos a 2 2))
(setq y (cadr pt))
(setq elev (rtos y 2 2))
(setq b"%%u")
(setq c"")
(command"text" "s" "levels" st  "0.0" (strcat b elev))
(command"text" "" (strcat c offset))
)
