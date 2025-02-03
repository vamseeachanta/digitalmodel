;;; Layer list
;;;
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2000-03-15
;;;
;;; c:llfp <LayerListFilePrint>
;;; Save the layer list to a file, (can be imported to Excel)
;;;
;;; Example: (ax:layer-list)
;;; Return values: list of layers and all layerstates
;;;    (<Layer Name> <On/Off> <Frozen/Thawed> <Locked/Not locked> <Color> <Linetype>
;;;        <Lineweight> <Plotstylename> <Plottable/Not plottable> <Viewportdefault=Frozen/Not frozen>)
(vl-load-com)
(defun ax:layer-list (/ lst layer colors color lw)
  (setq colors '("Red" "Yellow" "Green" "Cyan" "Blue" "Magenta" "White"))
  (vlax-for layer (vla-get-Layers
                    (vla-get-ActiveDocument
                      (vlax-get-acad-object)
                    )
                  )
    (setq color (vla-get-color layer))
    (if (< color 8) (setq color (nth (1- color) colors)) (setq color (itoa color)))
    (setq lw (vla-get-lineweight layer))
    (if (= lw -3) (setq lw "Default") (setq lw (rtos (/ lw 100.0) 2 2)))
    (setq lst (cons
                (list
                  (vla-get-name layer)
                  (if (= (vla-get-layeron layer) :vlax-true) "On" "Off")
                  (if (= (vla-get-freeze layer) :vlax-true) "Frozen" "Thawed")
                  (if (= (vla-get-lock layer) :vlax-true) "Locked" "Not locked")
                  color
                  (vla-get-linetype layer)
                  lw
                  (vla-get-plotstylename layer)
                  (if (= (vla-get-plottable layer) :vlax-true) "Plottable" "Not plottable")
                  (if (= (vla-get-viewportdefault layer) :vlax-true) "Frozen" "Not frozen")
                ) lst))
  )
  (vl-sort lst
           (function (lambda (e1 e2)
                       (< (strcase (car e1)) (strcase (car e2)))
                     )
           )
  ) 
)

;;; Writes layer list to specified file
;;; (layer-list-fprint "test.txt")
;;; return: T if file was created, else nil
(defun layer-list-fprint (fn / f row col)
  (if (setq f (open fn "w"))
    (progn
      ; print header
      (princ "\"Layer Name\" \"On\" \"Frozen\" \"Locked\" " f)
      (princ "\"Color\" \"Linetype\" \"Lineweight\" \"Plotstylename\" " f)
      (princ "\"Plottable\" \"Viewportdefault\"\n" f)
      (foreach row (ax:layer-list)
        (foreach col row
          (prin1 col f)
          (princ " " f) ; for tabulated (princ "\t" f)
        )
        (princ "\n" f)
      )
      (close f)
      T
    )
    nil
  )
)

(defun c:llfp (/ fn)
  (if (setq fn
             (getfiled "Save layer list as"
                       (strcat (vl-filename-base (getvar "dwgname")) ".txt")
                       "txt"
                       1
             )
      )
    (if (layer-list-fprint fn)
      (princ "\nLayer list created.")
      (princ "\nError: Layer list not created!")
    )
  )
  (princ)
)



