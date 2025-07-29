;;;	boxcircle.lsp - a program that draws a box and a circle

;;;	cooyright 2010 by jamil tayyab.

(alert "boxcircle.lsp . copyright 1996 by jamil tayyab. Type bc to start")

;;;	start program

(defun c:bc ()
;;;	drawing setup
(setq osm (getvar "osmode"))	;gets osnap settings and assigns to osm
(setvar "osmode" 0)		;turns osnap settings off
;;;	user input
(setq sp (getpoint "\nPick the starting point	"))
(setq width (getreal "\nWhat is the widhth of the box 	"))
;;;	do the math
(setq x1 (car sp)
      x2 (+ x1 (/ width 2.0))
      x3 (+ x1 width)
      y1 (cadr sp)
      y2 (+ y1 (/ width 2.0))
      y3 (+ y1 width)
)  
;;;	point assignment
(setq p1(list x1 y1)
      p2(list x3 y1)
      p3(list x3 y3)
      p4(list x1 y3)
      p5(list x2 y2)
)
;;;	let's draw
(command "line" p1 p2 p3 p4 "c")
(command "circle" p5 "d" width)
;;;	end the program
(setvar "osmode" osm)
; turns osnap settings back on
)
  