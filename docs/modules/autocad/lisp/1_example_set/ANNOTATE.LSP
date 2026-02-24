; From the Desk of Paul Standing
; Somewhere deep in space
; Annotate.lsp vs 1.0 April 18th 1995
; program allows the user to annotate direction and distance to either
; side of a line drawn on the drawing screen.

(defun text_line (en txt side / txt_size el aa dd aa2 p1)
  (if (null txt_size)(setq txt_size (getvar "textsize")))
  (if (and (= (type txt) 'str)
           (= (type en) 'ename))
           
  (if (= "LINE"
            (cdr (assoc 0
                     (setq el (entget en)))))
        (progn
           (setq aa (angle (cdr (assoc 10 el))
                           (cdr (assoc 11 el)))
                 dd (distance (cdr (assoc 10 el))
                              (cdr (assoc 11 el)))
           )
           (if (< (/ pi 2) aa (* pi 1.5))
               (setq p1 (cdr (assoc 11 el))
                     aa (rem (+ aa pi)(* pi 2)))
               (setq p1 (cdr (assoc 10 el))))

        (setq p1 (polar
        (polar p1 aa (/ dd 2))
                  (if (null side)
                  (+ aa (/ pi 2))
                  (- aa (/ pi 2)))
               txt_size))
          (command "text" "m" p1 txt_size (angtos aa) txt) 
          txt))))


 (defun C:annotate (/ en el) 
        (setvar "cmdecho" 0)
        (setq en (car (entsel "\nPick a Line: "))
              el (entget en))
        (if (= (cdr (assoc 0 el)) "LINE")
            (progn
            (text_line en 
                        (rtos (distance (cdr (assoc 10 el))
                                        (cdr (assoc 11 el)))
                                   3 4)
                          nil)
            (text_line en
                        (angtos (angle (cdr (assoc 10 el))
                                       (cdr (assoc 11 el)))
                                       4 3)
                                       1)
            )           
        )
        (setvar "cmdecho" 1)
        (princ)
)
