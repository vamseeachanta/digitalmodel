;;-----------------------=={ Box Text }==---------------------;;
;;                                                            ;;
;;  Frames Text or MText objects with an LWPolyline, with     ;;
;;  optional offset. Works in all UCS/Views.                  ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.1    -    26-05-2013                            ;;
;;------------------------------------------------------------;;

(defun c:bt ( / *error* ent enx lst off )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (initget 4)
    (if (setq off (getreal (strcat "\nSpecify Offset Factor <" (rtos (cond (*off*) ((setq *off* 0.35))) 2 2) ">: ")))
        (setq *off* off)
        (setq off *off*)
    )
    
    (while
        (progn (setvar 'errno 0) (setq ent (car (entsel "\nSelect Text or MText <Exit>: ")))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (= 'ename (type ent))
                    (if (setq lst (text-box (setq enx (entget ent)) (* off (cdr (assoc 40 enx)))))
                        (entmake
                            (append
                               '(
                                    (000 . "LWPOLYLINE")
                                    (100 . "AcDbEntity")
                                    (100 . "AcDbPolyline")
                                    (090 . 4)
                                    (070 . 1)
                                )
                                (list (cons 38 (caddar lst)))
                                (mapcar '(lambda ( x ) (cons 10 x)) lst)
                                (list (assoc 210 enx))
                            )
                        )
                        (princ "\nInvalid object selected.")
                    )
                )
            )
        )
    )
    (princ)
)

;; Text Box  -  gile / Lee Mac
;; Returns an OCS point list describing a rectangular frame surrounding
;; the supplied text or mtext entity with optional offset
;; enx - [lst] Text or MText DXF data list
;; off - [rea] offset (may be zero)

(defun text-box ( enx off / b h j l m n o p r w )
    (if
        (setq l
            (cond
                (   (= "TEXT" (cdr (assoc 0 enx)))
                    (setq b (cdr (assoc 10 enx))
                          r (cdr (assoc 50 enx))
                          l (textbox enx)
                    )
                    (list
                        (list (- (caar  l) off) (- (cadar  l) off))
                        (list (+ (caadr l) off) (- (cadar  l) off))
                        (list (+ (caadr l) off) (+ (cadadr l) off))
                        (list (- (caar  l) off) (+ (cadadr l) off))
                    )
                )
                (   (= "MTEXT" (cdr (assoc 0 enx)))
                    (setq n (cdr (assoc 210 enx))
                          b (trans  (cdr (assoc 10 enx)) 0 n)
                          r (angle '(0.0 0.0 0.0) (trans (cdr (assoc 11 enx)) 0 n))
                          w (cdr (assoc 42 enx))
                          h (cdr (assoc 43 enx))
                          j (cdr (assoc 71 enx))
                          o (list
                                (cond
                                    ((member j '(2 5 8)) (/ w -2.0))
                                    ((member j '(3 6 9)) (- w))
                                    (0.0)
                                )
                                (cond
                                    ((member j '(1 2 3)) (- h))
                                    ((member j '(4 5 6)) (/ h -2.0))
                                    (0.0)
                                )
                            )
                    )
                    (list
                        (list (- (car o)   off) (- (cadr o)   off))
                        (list (+ (car o) w off) (- (cadr o)   off))
                        (list (+ (car o) w off) (+ (cadr o) h off))
                        (list (- (car o)   off) (+ (cadr o) h off))
                    )
                )
            )
        )
        (   (lambda ( m ) (mapcar '(lambda ( p ) (mapcar '+ (mxv m p) b)) l))
            (list
                (list (cos r) (sin (- r)) 0.0)
                (list (sin r) (cos r)     0.0)
               '(0.0 0.0 1.0)
            )
        )
    )
)
 
;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n
 
(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

(princ)