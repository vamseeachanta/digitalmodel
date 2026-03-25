
; ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
; ³         3 D L  v1.0          ³± 
; ³      Copyright (c) 1991      ³±
; ³      by Kurtis J. Jones      ³± 
; ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±
;  ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;
;
; This simple LISP program allows you to draw 3-D lines by selecting the 
; "X,Y" coordinates with a mouse or digitizer, and entering the elevation,
; or "Z" coordinate through the keyboard.  I have found the program useful
; for digitizing 3-D feature lines for earthwork/contour programs that run
; under AutoCAD, but I'm certain that it may also be used for other     
; purposes.
;
; If you have any questions are comments about 3DL, I can be reached
; via THE SPECTRUM BBS, (501) 521-5639.
;

(defun c:3dl()
     (setq xy (getpoint "\nStart point of 3-D line: "))
     (setq z (getreal "\nEnter elevation: "))
     (setq x (car xy))
     (setq y (cadr xy))
     (setq pt1 (list x y z))
     (setq xy (getpoint pt1 "\nEnd point of 3-D line: "))
     (setq z (getreal "\nEnter elevation: "))
     (setq x (car xy))
     (setq y (cadr xy))
     (setq pt2 (list x y z))
     (while (/= z nil)
          (command "line" pt1 pt2)
          (command "")
          (setq pt1 pt2)
          (setq xy (getpoint pt1 "\nEnd point of 3-D line: "))
          (setq z (getreal "\nEnter elevation: "))
          (setq x (car xy))
          (setq y (cadr xy))
          (setq pt2 (list x y z))
     )
     (princ)
)