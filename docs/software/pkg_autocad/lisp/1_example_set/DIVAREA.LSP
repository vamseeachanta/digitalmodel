;;;DIVAREA.LSP  ver 2.0   Plot division utility (metric units)
;;;                       by Yorgos Angelopoulos
;;;                       agior1959@gmail.com
;;;
;;;  Suppose that you have to split a big part into 2, 3, 4 (even works for 5.014)
;;;  or you want to cut a smaller part out of the parent one.
;;;
;;;  All you need is a CLOSED polyline to define the parent part.
;;;
;;;  Load the utility, after placing it into an appropriate folder,
;;;  let's say \Program Files\Acad2000\Support, invoke "APPLOAD" command
;;;  or invoke (LOAD"DIVAREA") and run it by entering DIVAREA.
;;;
;;;  
;;;  For proper execution please note that:
;;;
;;;     1. The area which is enclosed by the poly must be FREE of
;;;          entities which could cause unexpected behaviour of the BOUNDARY
;;;          command (this command is the key to the solution). ERASE ALL lines,
;;;          polylines, circles, etc. inside the lwpoly. Text, nodes and attribs
;;;          may not interfere with the BOUNDARY command
;;;
;;;     2. The DIVISION LINE must CROSS THE BOUNDARY polyline,
;;;          is imaginary, you MUST NOT draw it, let the routine draw it,
;;;          just indicate its endpoints  
;;;          (you may have to draw auxilliary entities BEFORE you start DIVAREA)
;;;
;;;     3. Bear in mind that this DIVISION LINE will be rotated (or be offseted) and
;;;         neither of its endpoints should be inside the boundary, at
;;;         any moment, or else the result will be unexpected.
;;;
;;;     4. An easy way to help things going, is to indicate the two
;;;         end-points as FAR OUT from the boundary as possible, not exceeding
;;;         of course, your current visibe area.
;;;
;;;     5. The only exception is for the FIXED POINT, in case that 
;;;          you prefer "F" rather than "P" as an answer in the relevant question.
;;;          Fixed point must be ON or OUTSIDE the polyline, NEVER INSIDE.
;;;
;;;     6. Next, pick a point into the part which will obtain the desired
;;;         area. You have to indicate INTO it, NOT ON the boundary and AS FAR
;;;         from division line as possible, so this point will not be outside of
;;;         the desired part while the division line is moving towards the point.
;;;
;;;     7. Finally, you have to indicate the remaining part, exactly 
;;;          by the same way, FAR FROM DIVISION line and INTO the remaining piece.
;;;
;;;     8. For better area approximation you can decrease local vars
;;;         stp2 and stp1 in the following program-lines accordingly.
;;;
;;;******************UTILITY STARTS HERE*******************************
(defun getver_poly (entnme / subent pllst vertex)    ;;POLYLINE VERTICES LIST
 (setq subent (entnext entnme))
 (setq pllst '())
 (while subent
  (setq vertex (cdr (assoc 10 (entget subent))))
  (setq pllst (append pllst (list vertex)))
  (setq subent (entnext subent))
 )
 pllst
)
;;*********************************************************************
(defun getver_lwpoly (entnme / oldpl nodpl ptyp i n pllst)    ;;LWPOLYLINE VERTICES LIST
 (setq oldpl(entget entnme))
 (setq nodpl(cdr(assoc 90 oldpl)))
 (setq ptyp (cdr(assoc 70 oldpl)))
 (setq pllst '())
 (setq i 0)
 (setq n 0)
 (while (car(nth i oldpl))
        (if (= (car(nth i oldpl)) 10) 
               (progn
                     (setq pllst (append pllst (list (cdr(nth i oldpl)))))
                     (setq n(+ 1 n))
               );endprogn
        );endif
         (setq i (+ i 1))
 );endwhile
 (if (= ptyp 1)
   (progn
         (setq pllst (append pllst (list(nth 0 pllst))))
         (setq pllst (cdr pllst))
   );endprogn     
 );endif
 pllst
)
;;*********************************************************************
(defun prerr (s)
 (if (/= s "Function cancelled")
     (princ (strcat "\nError: " s))
 );endif
 (setq *error* olderr)
 (princ)
);close defun
;;*********************************************************************
(Defun C:DIVAREA (/ osm strpf strdc ex arxent arx arxon pllst k scl ok 
                    d p1 p2 pta pts ptb deln ar par tem
                    stp stp1 stp2                
                 )
 (setq olderr *error*
       *error* prerr)
 (setq osm(getvar "osmode"))
 (setvar "osmode" 0)
 (setvar "cmdecho" 0)
 (setq ex 0
       stp  0.10
       stp1 0.05
       stp2 0.005
 )
 (setq arxent (car(entsel "\nSelect a CLOSED polyline : "))
       arx (entget arxent)
       arxon  (cdr (assoc -1 arx))
 )
 (if (not
         (and
             (or
             (equal (cdr(assoc 0 arx)) "LWPOLYLINE")
             (equal (cdr(assoc 0 arx)) "POLYLINE")
             )
             (= (cdr(assoc 70 arx)) 1)
         )
     )
     (progn
           (princ "\nSORRY, ONLY CLOSED POLYLINES ALLOWED...")
           (setq ex 1)
     )
 )
 (if (= ex 0)
     (progn
       (command "_undo" "m") ;if something goes bad, you may return here
       (if (equal (cdr(assoc 0 arx)) "LWPOLYLINE")
           (setq pllst (getver_lwpoly arxent))
           (setq pllst (getver_poly arxent))
       )
       (command "_layer" "m" "Area_Division" "")
       (command "_area" "e" arxon)
       (setq ar(getvar "area"))
       (initget "Divide Cut" 1)
       (setq strdc(getkword "\nDIVIDE by number or CUT a part ? (D/C) :")) 
       (if (= strdc "Divide")
           (progn
                 (setq k  (getreal "\nEnter number to divide the whole part by : "))
                 (setq tem(/ ar k))
           )
           (setq tem (getreal "\nEnter area to cut from the whole part (m2) : "))
       )
       (initget "Parallel Fixed" 1)
       (setq strpf(getkword "\nPARALLEL to a direction or FIXED side? (P/F) :")) 
       (if (= strpf "Fixed")
           (fixpt)
           (parpt)
       )
     )
     (ready)
 )
)
;******************************************************************************
(defun fixpt ()
 (setvar "osmode" osm)
 (setq scl    0.05
       p1     (getpoint "\nPick fixed point of the division line - OUTSIDE of boundary: ")
       p2     (getpoint p1 "\nPick second point of division line - must completly CROSS boundary: ")
 )
 (setvar "osmode" 0)
 (command "_line" p1 p2 "")
 (setq deln (entlast))
 (setq pts (getpoint "\nPick any point into the REST of the piece, FAR from division line - INSIDE of boundary: "))
 (setq ptb (getpoint "\nPick any point into the FIRST piece, FAR from division line - INSIDE of boundary: "))
 (setvar "blipmode" 0)
 (princ "\nPlease wait...")
 (command "_boundary" pts "")
 (command "_area" "e" "l")
 (setq par(getvar "area"))
 (setq ok -1)
 (if (< par tem)
  (progn
        (while (< par tem)
         (entdel (entlast))
         (if (< (- tem par) 50)(setq scl stp))
         (if (< (- tem par) 10)(setq scl stp2))
         (command "_rotate" deln "" p1 (* scl ok))
         (command "_boundary" pts "")
         (command "_area" "e" "l")
         (if (< (getvar "area") par)
             (setq ok(* ok -1))
         )
         (setq par(getvar "area"))
        );endwhile
        (entdel deln)
 )
  (progn
        (while (> par tem)
         (entdel (entlast))
         (if (< (- par tem) 50)(setq scl stp))
         (if (< (- par tem) 10)(setq scl stp2))
         (command "_rotate" deln "" p1 (* scl ok))
         (command "_boundary" pts "")
         (command "_area" "e" "l")
         (if (> (getvar "area") par)
             (setq ok(* ok -1))
         )
         (setq par(getvar "area"))
        );endwhile
        (entdel deln)
 )
 )
 (command "_change" "l" "" "p" "c" "green" "")
 (command "_boundary" ptb "")
 (command "_change" "l" "" "p" "c" "red" "")
 (ready)
)
;******************************************************************************
(defun parpt ()
 (setvar "osmode" osm)
 (setq scl    0.25
       p1     (getpoint "\nPick one point of division line (far from lwpoly) : ")
       p2     (getpoint p1 "\nPick other point of division line (far from lwpoly) : ")
 )
 (setvar "osmode" 0)
 (command "_line" p1 p2 "")
 (setq deln(entlast))
 (setq pta (getpoint "\nPick any point into the FIRST piece, FAR from division line - INSIDE of boundary: "))
 (setq pts (getpoint "\nPick any point into the REST of the piece, FAR from division line - INSIDE of boundary: "))
 (setq ptb (getpoint "\nPick any point from the REST of the piece, FAR from division line - OUTSIDE of boundary: "))
 (setvar "blipmode" 0)
 (princ "\nPlease wait...")
 (command "_boundary" pts "")
 (command "_area" "e" "l")
 (setq par(getvar "area"))
 (if (< par tem)
  (progn
        (while (< par tem)
         (entdel (entlast))
         (if (< (- tem par) 50)(setq scl stp1))
         (if (< (- tem par) 10)(setq scl stp2))
         (command "_offset" scl deln ptb "")
         (entdel deln)
         (setq deln(entlast))
         (command "_boundary" pts "")
         (command "_area" "e" "l")
         (setq par(getvar "area"))
        )
        (entdel deln)
  )
  (progn
        (while (> par tem)
         (entdel (entlast))
         (if (< (- par tem) 50)(setq scl stp1))
         (if (< (- par tem) 10)(setq scl stp2))
         (command "_offset" scl deln ptb "")
         (entdel deln)
         (setq deln(entlast))
         (command "_boundary" pts "")
         (command "_area" "e" "l")
         (setq par(getvar "area"))
        )
        (entdel deln)
  )
 )
 (command "_change" "l" "" "p" "c" "green" "")
 (command "_boundary" pta "")
 (command "_change" "l" "" "p" "c" "red" "")
 (ready)
)
;******************************************************************************
(defun ready ()
  (princ scl)
  (princ "\nActual : ")
  (princ par)
  (princ "\nMust be: ")
  (princ tem)
 (setq *error* olderr)
 (setvar "osmode" osm)
 (setvar "cmdecho" 1)
 (setvar "blipmode" 1)
 (princ "\nThanks...")
 (princ)
);close defun
