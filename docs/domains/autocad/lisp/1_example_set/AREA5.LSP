:::ortho;off;-osnap;off;-boundary;\;move;l;^C_area;o;l;erase;l;;a1 
;;;Area calculation
;;;Designed by Qaisar Malik.)
(defun c:q()
(COMMAND "ORTHO" "OFF")
(COMMAND "OSNAP" "OFF")
(setq d1 (getpoint "\nClick inside the object"))
(COMMAND "-boundary" d1 "")
(COMMAND "area" "o" "l" "")
(setq p2 (getpoint "\npoint1"))
(setq str (getvar "area"))
(setq str2 (rtos str 2 2))
(command "text" "M" p2 "2.5" "0" str2)
)

