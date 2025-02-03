;;-----------------------=={ Elevation Marker }==-----------------------;;
;;                                                                      ;;
;;  This program continuously prompts the user to specify a point and   ;;
;;  constructs an elevation marker composed of a variable width         ;;
;;  polyline & single-line text object at the specified point, with     ;;
;;  the text content displaying the UCS Y-coordinate of the point.      ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;

(defun c:em ( / *error* ang hgt len ocs pt1 pt2 pt3 pt4 str )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq hgt (getvar 'textsize)
          ocs (trans '(0.0 0.0 1.0) 1 0 t)
          ang (angle '(0.0 0.0) (trans (getvar 'ucsxdir) 0 ocs t))
    )
    (terpri)
    (while (setq pt1 (getpoint "\rSpecify point <exit>: "))
        (setq str (rtos (cadr pt1))
              len (strlen str)
              pt2 (list (car  pt1) (+ (cadr pt1) (* hgt 0.5 (sqrt 3))))
              pt3 (list (- (car pt1) (* hgt len)) (cadr pt2))
              pt4 (list (- (car pt2) (* hgt 0.5 len)) (+ (cadr pt2) hgt))
        )
        (foreach sym '(pt1 pt2 pt3 pt4)
            (set sym (trans (eval sym) 1 ocs))
        )
        (entmake
            (list
               '(000 . "LWPOLYLINE")
               '(100 . "AcDbEntity")
               '(100 . "AcDbPolyline")
               '(090 . 3)
               '(070 . 0)
                (cons 038 (caddr pt1))
                (cons 010 pt1)
               '(040 . 0.0)
                (cons 041 hgt)
                (cons 010 pt2)
                (cons 040 (* hgt 0.05))
                (cons 041 (* hgt 0.05))
                (cons 010 pt3)
                (cons 210 ocs)
            )
        )
        (entmake
            (list
               '(000 . "TEXT")
                (cons 007 (getvar 'textstyle))
                (cons 001 str)
                (cons 050 ang)
                (cons 040 hgt)
                (cons 010 pt4)
                (cons 011 pt4)
               '(072 . 1)
               '(073 . 2)
                (cons 210 ocs)
            )
        )
    )
    (princ)
)