; https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/additional-geometry-by-autolisp/td-p/12733197

(defun c:rectholes ( / AH:getvalsm ans len tab num1 num2 edge rad pt1 pt2 pt3 pt4 pt5 pt6 oldsnap pi2 ang off off2)

(defun chklay (lay col lt / )
(if(not(tblsearch "LAYER" lay))
(command "-layer" "m" lay "c" col lay "lt" lt lay "")
(princ "exist")
)
(setvar 'clayer lay)
)

(if (tblsearch "dimstyle" "MD5")
 (command "dimstyle" "R" "MD5")
 (progn (Alert "dimension style MD5 is not available\n\n Will exit now please get correct dim style ")(exit))
)

(setq oldsnap (getvar 'osmode))
(setvar 'osmode 0)

(setq pi2 (/ pi 2.0))

(chklay "GREEN" 3 "Continuous")
(chklay "WHITE" 7 "Continuous")
(chklay "DIMS" 1 "Continuous")

(if (not AH:getvalsm)(load "Multi Getvals.lsp"))

(setq ans (AH:getvalsm (list "Enter values " "Length " 5 4 "1000" "height " 5 4 "1000" "Tab size " 5 4 "25"
"No of holes Top Bot" 5 4 "3" "No of holes Left Right" 5 4 "4"
"Edge offset " 5 4 "25" "Hole radius " 5 4 "2.5"))
)

(setq len (atof (nth 0 ans)) 
  wid  (atof (nth 1 ans))
  tab  (atof (nth 2 ans))
  num1 (atoi (nth 3 ans))
  num2 (atoi (nth 4 ans))
  edge (atof (nth 5 ans))
  rad  (atof (nth 6 ans))
)

(setvar 'clayer "WHITE")

(setq pt1 (getpoint "\nPick lower left point "))
(setq pt2 (list (+ (car pt1) len) (+ wid (cadr pt1))))
(command "rectang" pt1 pt2)

(setq co-ord (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget (entlast)))))
(setq pt1 (nth 0 co-ord)
  pt2 (nth 1 co-ord)
  pt3 (nth 2 co-ord)
  pt4 (nth 3 co-ord)
)

; draw tabs
(setq ang (- (angle pt1 pt2) pi2))
(command "Pline" pt1 (polar pt1 ang tab) (polar pt2 ang tab) pt2 "")

(setq ang (- (angle pt2 pt3) pi2))
(command "Pline" pt2 (polar pt2 ang tab) (polar pt3 ang tab) pt3 "")

(setq ang (- (angle pt3 pt4) pi2))
(command "Pline" pt3 (polar pt3 ang tab) (polar pt4 ang tab) pt4 "")

(setq ang (- (angle pt4 pt1) pi2))
(command "Pline" pt4 (polar pt4 ang tab) (polar pt1 ang tab) pt1 "")

(setq off (/ tab 2.0))
(setq off2 94)

(setvar 'clayer "HOLES")

; T & B
(setq pt5 (polar (polar pt1 (- (angle pt1 pt2) pi2) off) (angle pt1 pt2) edge))
(command "circle" pt5 rad)

(setq dist (/ (- len (* 2.0 edge))(- num1 1)))

(repeat (- num1 1)
(setq pt5 (polar pt5 (angle pt1 pt2) dist))
(command "circle" pt5 rad)
)

(setvar 'clayer "DIMS")
(setq pt6 (polar pt1 (- (angle pt1 pt2) pi2) off2))
(command "dim" "aligned" (setq pt5 (polar pt1 (- (angle pt1 pt2) pi2) edge)) (setq pt5 (polar pt5 (angle pt1 pt2) edge)) pt6  "" "exit")

(repeat (- num1 1)
(command "dim" "aligned" pt5 (setq pt5 (polar pt5 (angle pt1 pt2) dist)) pt6  "" "exit")
)
(command "dim" "aligned" pt5 (setq pt5 (polar pt2 (- (angle pt1 pt2) pi2) edge)) pt6  "" "exit")

(setvar 'clayer "HOLES")
(setq pt5 (polar (polar pt3 (- (angle pt3 pt4) pi2) off) (angle pt3 pt4) edge))
(command "circle" pt5 rad)
(repeat (- num1 1)
(setq pt5 (polar pt5 (angle pt3 pt4) dist))
(command "circle" pt5 rad)
)

(setq pt6 (polar pt3 (- (angle pt3 pt4) pi2) off2))
(command "dim" "aligned" (setq pt5 (polar pt3 (- (angle pt3 pt4) pi2) tab)) (setq pt5 (polar pt5 (angle pt3 pt4) tab)) pt6  "" "exit")
(repeat (- num1 1)
(command "dim" "aligned" pt5 (setq pt5 (polar pt5 (angle pt3 pt4) dist)) pt6  "" "exit")
)
(command "dim" "aligned" pt5 (setq pt5 (polar pt4 (- (angle pt3 pt4) pi2) tab)) pt6  "" "exit")

; L & R
(setvar 'clayer "GREEN")


(setq dist (/ (- wid (* 2.0 edge))(- num2 1)))

(setq pt5 (polar (polar pt4 (- (angle pt4 pt1) pi2) tab) (angle pt4 pt1) edge))
(command "circle" pt5 rad)
(repeat (- num2 1)
(setq pt5 (polar pt5 (angle pt4 pt1) dist))
(command "circle" pt5 rad)
)

(setq pt5 (polar (polar pt2 (- (angle pt2 pt3) pi2) off) (angle pt2 pt3) edge))
(command "circle" pt5 rad)
(repeat (- num2 1)
(setq pt5 (polar pt5 (angle pt2 pt3) dist))
(command "circle" pt5 rad)
)

(setq pt6 (polar pt4 (- (angle pt4 pt1) pi2) off2))
(command "dim" "aligned" (setq pt5 (polar pt4 (- (angle pt4 pt1) pi2) tab)) (setq pt5 (polar pt5 (angle pt4 pt1) tab)) pt6  "" "exit")
(repeat (- num2 1)
(command "dim" "aligned" pt5 (setq pt5 (polar pt5 (angle pt4 pt1) dist)) pt6  "" "exit")
)
(command "dim" "aligned" pt5 (setq pt5 (polar pt1 (- (angle pt4 pt1) pi2) tab)) pt6  "" "exit")

(setq pt6 (polar pt2 (- (angle pt2 pt3) pi2) off2))
(command "dim" "aligned" (setq pt5 (polar pt2 (- (angle pt2 pt3) pi2) tab)) (setq pt5 (polar pt5 (angle pt2 pt3) tab)) pt6  "" "exit")
(repeat (- num2 1)
(command "dim" "aligned" pt5 (setq pt5 (polar pt5 (angle pt2 pt3) dist)) pt6  "" "exit")
)
(command "dim" "aligned" pt5 (setq pt5 (polar pt3 (- (angle pt2 pt3) pi2) tab)) pt6  "" "exit")

(setvar 'osmode oldsnap)
(princ)
)
(c:rectholes)