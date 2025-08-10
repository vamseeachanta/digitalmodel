;;; DIM.LSP Version 2.2, Copyright (C) (1997) by Pham Hoang Anh, K37.X6
;==============================================================================
(DEFUN C:HC ( / dist p1 p2 p3  p4 pp1 pp2 x1 x2 y txt)
  (setq p1 (getpoint "\nPick first  origin point : "))
  (setq p2 (getpoint p1 "\nPick second origin point : "))
  (setq p3 (getpoint "\nExtension point : "))
  (setq p4 (getpoint p3 "\nDimension location : "))
  (setq i 1)

  (while (/= p2 nil)
  (setq y (cadr p3))
  (setq x1 (car p1))
  (setq x2 (car p2))
  (setq pp1 (list x1 y))
  (setq pp2 (list x2 y))
  (setq dist (distance pp1 pp2))
  (setq txt (getstring T (strcat "\nText <"
                         (rtos (* dist (getvar "USERI1"))) "> " )))
  (if (or (null txt)
          (= txt "")
          (= txt " ")
          (= txt "Enter")
      )
      (setq txt (rtos (* dist (getvar "USERI1")) 2 0) )
  )
  (setvar "OSMODE" 0)
  (setvar "BLIPMODE" 0)
  (COMMAND "DIM1" "HOR" pp1 pp2 p4 txt )
  (COMMAND "CHPROP" "LAST" "" "LA" "DIM" "")
  (setvar "OSMODE" 177)
  (setvar "BLIPMODE" 1)

  (setq p1 p2)
  (setq p2 (getpoint "\nNext point : "))

  );;while

  (princ)
  )
;==============================================================================
(DEFUN C:VC ( / dist p1 p2 p3 p4 pp1 pp2 x y1 y2  txt )
  (setq p1 (getpoint "\nPick first  origin point : "))
  (setq p2 (getpoint p1 "\nPick second origin point : "))
  (setq p3 (getpoint "\nExtension point : "))
  (setq p4 (getpoint p3 "\nDimension location : "))

  (while (/= p2 nil)

  (setq x (car p3))
  (setq y1 (cadr p1))
  (setq y2 (cadr p2))
  (setq pp1 (list x y1))
  (setq pp2 (list x y2))
  (setq dist (distance pp1 pp2))
  (setq txt (getstring T (strcat "\nText <"
                         (rtos (* dist (getvar "USERI1"))) "> " )))
  (if (or (null txt)
          (= txt "")
          (= txt " ")
          (= txt "Enter")
      )
      (setq txt (rtos (* dist (getvar "USERI1")) 2 0) )
  )
  (setvar "OSMODE" 0)
  (setvar "BLIPMODE" 0)
  (COMMAND "DIM1" "VER" pp1 pp2 p4 txt )
  (COMMAND "CHPROP" "LAST" "" "LA" "DIM" "")
  (setvar "OSMODE" 177)
  (setvar "BLIPMODE" 1)

  (setq p1 p2)
  (setq p2 (getpoint "\nNext point : "))

  );;while

  (princ)
  )
;==============================================================================
(DEFUN C:AC ( / p1 p2 p3  p4 p5 pp1 pp2 x1 x2 x3 y1 y2 y3  txt i
                a1 a2 a3 ap ang dis)
  (setq p1 (getpoint "\nPick first  origin point : "))
  (setq p2 (getpoint p1 "\nPick second origin point : "))
  (setq p3 (getpoint "\nExtension point : "))
  (setq p4 (getpoint p3 "\nDimension location : "))
;  (setq i nil)

  (while (/= p2 nil)

;  (if (/= i nil)
;    (progn
     (setq a1 (angle p1 p2))
     (setq a2 (angle p1 p3))
     (setq ap (- (- a1 (/ PI 2)) a2))
;     )
;   )
  (setq dis (* (distance p1 p3) (cos ap)))
  (setq pp1 (polar p1 (- a1 (/ PI 2)) dis))
  (setq pp2 (polar pp1 a1 (distance p1 p2)))
  (setq dist (distance pp1 pp2))
  (setq txt (getstring T (strcat "\nText <"
                         (rtos (* dist (getvar "USERI1"))) "> " )))
  (if (or (null txt)
          (= txt "")
          (= txt " ")
          (= txt "Enter")
      )
      (setq txt (rtos (* dist (getvar "USERI1")) 2 0) )
  )
  (setvar "OSMODE" 0)
  (setvar "BLIPMODE" 0)
  (COMMAND "DIM1" "ALI" pp1 pp2 p4 txt )
  (COMMAND "CHPROP" "LAST" "" "LA" "DIM" "")
  (setvar "OSMODE" 177)
  (setvar "BLIPMODE" 1)

;  (setq i 1)
  (setq p1 p2)
  (setq p5 (getpoint "\nNext point : "))
  (setq a3 (angle p2 p5)
        ang (- a3 a1)
        dis (* (distance p2 p5) (cos ang))
        p2 (polar p2 a1 dis)
   )    

  );;while

  (princ)
  )
;=============================================================================