(defun c:td()
; hoi toa do mot diem
(command "ID" pause)
(setq i (getvar "lastpoint"))
(command "color" "bylayer")
(command "text" pause pause 0 (strcat "" (rtos (car i))))
(command "text" "" (rtos (cadr i)))
(command "line" i pause pause "" )
)

;;;                 --------------- end------------------
(defun c:dt()
;tinh dien tich 1 hinh khep kin
        (setq cc 0)
        (while (= cc 0)
        (setq kt (getreal "kich thuoc chu:"))
        (setq pt2 (getpoint "diem thu:"))
        (command "text" "j" "bc" pt2 kt 0.0 "12345")
        (setq cc (getint "duoc chua (0/1):"))
        (command "u" "")
        )
        (setq fw (open "C:AAAarea.txt" "w"))
        (setq soluong (getint "bao nhieu o:"))
        (setq c 0)
        (while (< c soluong)
        (setq pt1 (getpoint "chon khu vuc can tinh :"))
        (command "boundary" pt1 "")
        (command "area" "e" "l")
        (command "erase" "l" "" )
        (setq dientich (getvar "area"))
        ;(setq dientich (/ dientich ))
        (setq st1 (rtos (+ c 1) 2 0))
        (setq st2 (rtos dientich 2 2))
        (command "circle" pt1 kt "") 
        (command "text" "j" "mc" pt1 kt 0.0 st1)
        (command "text" "j" "bl" pt1 kt 0.0 (strcat "  " st2 "m2"))
	(command "redraw" )
        (write-line (strcat st1 " " st2) fw)
        (setq c (+ c 1))
        )
        (close fw)
)
;;;               -------------------end------------------
(defun c:noi()
;chuyen "line" thanh "pline
	(command "pedit" pause "" "j" pause  )
)
;;               ------------------  end -------------------
(defun c:kc1()
;xac dinh chieu dai 1 doan thang
        (setq pt1 (getpoint "diem 1 :"))
        (setq pt2 (getpoint "diem 2 :"))
        (setq chieudai (distance pt1 pt2))
        (setq pt3 (getpoint "diem 3:"))
        (setq chieudai (+ chieudai (distance pt2 pt3)))
        (setq pt4 (getpoint "diem 4:"))
        (setq chieudai (+ chieudai (distance pt3 pt4))) 
        (setq alfa (angle pt1 pt2))
        (setq st1 (rtos chieudai 2 0))
        (command "st1" "j" "bc" pt3 (/ (* alfa 180) pi) st1)
		
)
;;;             ---------------- end -----------------
(defun c:kc2()
;xac dinh chieu dai 1 doan thang
        (setq pt1 (getpoint "diem 1 :"))
        (setq pt2 (getpoint "diem 2 :"))
        (setq chieudai (distance pt1 pt2))
        (setq alfa (angle pt1 pt2))
        (setq pt3 (polar pt1 alfa (/ chieudai 2)))
        (setq st1 (rtos chieudai 2 0))
        (command "text" "j" "bc" pt3 (/ (* alfa 180) pi) st1)
)


(defun c:xoachu()
(setq ss1 (ssget "X" (list (cons 0 "text"))))
(setq c 0)
(if (/= ss1 nil)
(while (< c (sslength ss1 ))
(setq st1 (ssname ss1 c))
(entdel st1)
(setq c (+ c 1))
)
)
)

(defun c:xoanut()
(setq ss1 (ssget "X" (list (cons 0 "circle"))))
(setq c 0)
(if(/= ss1 nil)
(while (< c (sslength ss1))
(setq e1 (ssname ss1 c))
(entdel e1)
(setq c (+ c 1))
)
)
)
