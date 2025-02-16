;;; DOOR.LSP 1.0, Copyright (C) 1998 by CAD Department, HUCE.
;;; =========================================================
(defun C:D1 (/ p1 p2 p3 p4 side dis wid ang angsile old)
 (setq old (getvar "OSMODE")) 
 (setvar "OSMODE" 177)

 (setq p1 (getpoint "\nStart point:"))
 (setq p2 (getpoint p1 "\nEnd point:"))
 (setq side (getpoint p2 "\nOpen side"))

 (setq dis (distance p1 p2))
 (setq ang (angle p1 p2))
 (if (< ang (angle p1 side))
  (setq angside    (/ pi 2) )
  (setq angside (- (/ pi 2)))
 )
 (if  (>= (- (angle p1 side) ang) pi)
  (setq angside (- (/ pi 2)))
 )
 (if  (>= (- ANG (angle p1 side) ) pi)
  (setq angside (/ pi 2))
 )

 (setq p3 (polar p2 (+ ang angside) dis) )
 (setq p4 (polar p1 (+ ang angside) dis) )
 (setq wid (/ dis 15))

 (setvar "OSMODE" 0)

 (COMMAND "LAYER" "M" "DOOR" "") 
 (command "PLINE" p1 "A" "D" p4 p3 "L" p2 
                  (polar p2 (+ ang pi) wid) 
                  (polar p3 (+ ang pi) wid) 
 "")

 (setvar "OSMODE" old)
 (princ)
)
;;; =========================================================
(defun C:D2 (/ p1 p2 p3 p4 side dis wid ang angsile old)
 (defun cua1 (pt1 pt2)
  (setq p1 pt1
        p2 pt2)

  (setq ang (angle p1 p2))
  (if (< ang (angle p1 side))
   (setq angside    (/ pi 2) )
   (setq angside (- (/ pi 2)))
  )
  (if  (>= (- (angle p1 side) ang) pi)
   (setq angside (- (/ pi 2)))
  )
  (if  (>= (- ANG (angle p1 side) ) pi)
   (setq angside (/ pi 2))
  )

  (setq p3 (polar p2 (+ ang angside) dis) )
  (setq p4 (polar p1 (+ ang angside) dis) )
  (setq wid (/ dis 15))

  (command "layer" "m" "door" "") 
  (command "PLINE" p1 "A" "D" p4 p3 "L" p2 
                   (polar p2 (+ ang pi) wid) 
                   (polar p3 (+ ang pi) wid) 
  "")
 )
 ;; ------------------------------------
 (setq old (getvar "OSMODE")) 
 (setvar "OSMODE" 177)

 (setq p1 (getpoint "\nStart point:"))
 (setq p2 (getpoint p1 "\nEnd point:"))
 (setq side (getpoint p2 "\nOpen side"))

 (setq dis (distance p1 p2))
 (setq dis (/ dis 2))

 (setvar "OSMODE" 0)

 (setq p1 (polar p1 (angle p1 p2) dis))
 (cua1 p1 p2)  

 (setq ang (+ ang pi))
 (setq p2 (polar p1 ang dis))
; (setq side (polar p1 () dis))
 (cua1 p1 p2)  

 (setvar "OSMODE" old)
 (princ)
)
