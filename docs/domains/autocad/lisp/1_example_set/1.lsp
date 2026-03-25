;;;	anglemaker.lsp

;;;	a program that draws the side vies of an extruded angle
;;;	the user will be able to andividually control the size of
;;;	each leg, the starting point and the interior radius at the
;;;	junction of the two legs

;;;	copyright 1996 by charles robbins

;;;	start program
(defun c:am ()
(setq osm (getvar "osmode"))
(setvar "osmode" 0)
;;;	user input
  (setq sp (getpoint "\nPick the starting point "))
  (setq leg1 (getreal "\nWhat is the length of leg1? "))
(setq thk1 (getreal "\nWhat is the thickness of leg1? "))
  (setq leg2 (getreal "\nWhat is the length of leg2? "))
(setq thk2 (getreal "\nWhat is the thickness of leg2? "))
  (setq rad (getreal "\nThe angle.s interior radius is? "))
;;;	do the math
  (setq x1 (car sp)
	x2 (+ x1 thk2)
	x3 (+ x2 rad)
	x5 (+ x1 leg1)
	x4 (- x5 thk1)
	y1 (cadr sp)
	y2 (+ y1 thk1)
	y3 (+ y2 rad)
	y5 (+ y1 leg2)
	y4 (- y5 thk2)
	)  
  ;;;	point assignments
(setq p1 (list x1 y1)
      p2 (list x5 y1)
      p3 (list x3 y2)
      p4 (list x4 y2)
      p5 (list x2 y3)
      p6 (list x2 y4)
      p7 (list x1 y5)
      )
  ;;;	let's draw
  (command "line" p1 p2 "")
  (command "line" p3 p4 "")
  (command "line" p5 p6 "")
  (command "line" p7 p1 "")
  (command "arc" p2 "e" p4 "r" thk1)
  (command "arc" p6 "e" p7 "r" thk2)
  (command "arc" p5 "e" p3 "r" rad)
  ;;;	end the ptogram
  (setvar "osmode" osm)
  ;;;	turns osnap settings back on
  )