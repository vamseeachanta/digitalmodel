; pa is stands for pick area
(defun c:pa (/ ss vevo area)
  (while
    (setq ss (car (entsel "\nSelect a object")))
    (setq vevo (vlax-ename->vla-object ss))
    (setq area (vlax-get vevo 'area))
    (alert (strcat "\nThis object have area = " (rtos area)))
    )                    ; while
  (princ)
  )                      ; defun
; pao is stand for Pick Area Object
;        Design by  : Adesu <Ade Suharna>
;        Email      : mteybid@yuasabattery.co.id
;        Homepage   : http://www.yuasa-battery.co.id
;        Create     : 03 November 2006
;        Program no.: 0455/11/2006
;        Edit by    :
(defun c:pao (/ area cth loc ss str th vevo)
  (while
    (setq ss (car (entsel "\nSelect a object")))
    (setq vevo (vlax-ename->vla-object ss))
    (setq area (vlax-get vevo 'area))
    (setq loc (getpoint "\nClick location for text<0,0,0>: "))
    (if (= loc nil)(setq loc '(0 0 0)))
    (setq str (strcat "" (rtos area) ""))
    (setq cth (rtos (getvar "textsize")))
    (setq th (getdist (strcat "\nEnter new text height < " cth " > : ")))
    (if (= th nil)(setq th cth))
    (command "_text" loc th "" str 0)
    )                    ; while
  (princ)
  )                      ; defun