;;-----------------------=={ Draw Grid }==--------------------;;
;;                                                            ;;
;;  Dynamically generates a grid with a specified number of   ;;
;;  rows and columns.                                         ;;
;;  Works in all UCS/Views.                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.1    -    13-10-2011                            ;;
;;------------------------------------------------------------;;

(defun c:dgrid  nil (LM:grid nil))  ;; Standard
(defun c:dgridd nil (LM:grid   t))  ;; Dynamic 

;;------------------------------------------------------------;;

(defun LM:grid

    ( dyn / *error* _getIntwithDefault _getosmode _parsepoint _makegrid _grX g1 gr ls ms os p1 p3 st )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )

    (defun _getIntwithDefault ( msg sym ) (initget 6)
        (set sym
            (cond
                (
                    (getint
                        (strcat msg "<"
                            (itoa
                                (set sym
                                    (cond ((eval sym)) ( 2 ))
                                )
                            )
                            ">: "
                        )
                    )
                )
                (   (eval sym)   )
            )
        )
    )

    (defun _getosmode ( os / lst )
        (foreach mode
           '(
                (0001 . "_end")
                (0002 . "_mid")
                (0004 . "_cen")
                (0008 . "_nod")
                (0016 . "_qua")
                (0032 . "_int")
                (0064 . "_ins")
                (0128 . "_per")
                (0256 . "_tan")
                (0512 . "_nea")
                (1024 . "_qui")
                (2048 . "_app")
                (4096 . "_ext")
                (8192 . "_par")
            )
            (if (not (zerop (logand (car mode) os)))
                (setq lst (cons "," (cons (cdr mode) lst)))
            )
        )
        (apply 'strcat (cdr lst))
    )

    (defun _parsepoint ( pt str / _str->lst lst )

        (defun _str->lst ( str / pos )
            (if (setq pos (vl-string-position 44 str))
                (cons (substr str 1 pos) (_str->lst (substr str (+ pos 2))))
                (list str)
            )
        )

        (if (wcmatch str "`@*")
            (setq str (substr str 2))
            (setq pt '(0.0 0.0 0.0))
        )           

        (if
            (and
                (setq lst (mapcar 'distof (_str->lst str)))
                (vl-every 'numberp lst)
                (< 1 (length lst) 4)
            )
            (mapcar '+ pt lst)
        )
    )

    (defun _makegrid ( p1 p3 mode / hd vd hs vs pt )
        (setq hd (- (car  p3) (car  p1))
              vd (- (cadr p3) (cadr p1))
              hs (/ hd *cols*)
              vs (/ vd *rows*)
        )
        (cond
            (   (= 5 mode)
                (setq pt p1)
                (repeat (1+ *cols*)
                    (grdraw pt (list (car pt) (+ (cadr pt) vd)) 1 1)
                    (setq pt (list (+ (car pt) hs) (cadr pt)))
                )
                (setq pt p1)
                (repeat (1+ *rows*)
                    (grdraw pt (list (+ (car pt) hd) (cadr pt)) 1 1)
                    (setq pt (list (car pt) (+ (cadr pt) vs)))
                )
                t
            )
            (   (= 3 mode)
                (setq pt p1)
                (repeat (1+ *cols*)
                    (entmakex
                        (list
                            (cons 0 "LINE")
                            (cons 10 (trans pt 1 0))
                            (cons 11 (trans (list (car pt) (+ (cadr pt) vd)) 1 0))
                        )
                    )
                    (setq pt (list (+ (car pt) hs) (cadr pt)))
                )
                (setq pt p1)
                (repeat (1+ *rows*)
                    (entmakex
                        (list
                            (cons 0 "LINE")
                            (cons 10 (trans pt 1 0))
                            (cons 11 (trans (list (+ (car pt) hd) (cadr pt)) 1 0))
                        )
                    )
                    (setq pt (list (car pt) (+ (cadr pt) vs)))
                )
                nil
            )
        )
    )

    (defun _grX ( p s c / -s r q )
        (setq -s (- s)
               r (/ (getvar 'VIEWSIZE) (cadr (getvar 'SCREENSIZE)))
               p (trans p 1 3)
        )
        (grvecs
            (list c
                (list -s      -s)  (list s       s)
                (list -s  (1+ -s)) (list (1- s)  s)
                (list (1+ -s) -s)  (list s   (1- s))
                
                (list -s       s)  (list s      -s)
                (list -s   (1- s)) (list (1- s) -s)
                (list (1+ -s)  s)  (list s  (1+ -s))
            )
            (list
                (list r  0. 0. (car  p))
                (list 0. r  0. (cadr p))
                (list 0. 0. r  0.)
                (list 0. 0. 0. 1.)
            )
        )
        p
    )
        
    (_getIntwithDefault "\nSpecify Number of Rows "    '*rows*)
    (_getIntwithDefault "\nSpecify Number of Columns " '*cols*)

    (if (setq p1 (getpoint "\nSpecify Base Point: "))
        (cond
            (   dyn
                (setq os (_getosmode (getvar 'OSMODE))
                      st ""
                )
                (princ (setq ms "\nSpecify Opposite Corner: "))
                (while
                    (progn (setq gr (grread t 15 0) g1 (car gr) p3 (cadr gr))
                        (cond
                            (   (member g1 '(3 5)) (redraw)
                                (if
                                    (and
                                        (zerop (logand 16384 (getvar 'OSMODE)))
                                        (setq op (osnap p3 os))
                                    )
                                    (_grX (setq p3 op) 6 20)
                                )
                                (_makegrid p1 p3 g1)
                            )
                            (   (= g1 2)
                                (cond
                                    (   (= 6 p3)
                                        (if (zerop (logand 16384 (setvar 'OSMODE (boole 6 16384 (getvar 'OSMODE)))))
                                            (princ (strcat "\n<Osnap on>"  ms st))
                                            (princ (strcat "\n<Osnap off>" ms st))
                                        )                                    
                                    )
                                    (   (= 8 p3)
                                        (if (< 0 (strlen st))
                                            (progn
                                                (princ (vl-list->string '(8 32 8)))
                                                (setq st (substr st 1 (1- (strlen st))))
                                            )
                                        )
                                        t
                                    )
                                    (   (< 32 p3 127)
                                        (setq st (strcat st (princ (chr p3))))
                                    )
                                    (   (member p3 '(13 32))
                                        (if (< 0 (strlen st))
                                            (if (setq p3 (_parsepoint p1 st))
                                                (_makegrid p1 p3 3)
                                                (princ (strcat (setq st "") "\n2D / 3D Point Required." ms))
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (   (setq p3 ((if (zerop (getvar 'WORLDUCS)) getpoint getcorner) p1 "\nSpecify Opposite Corner: "))
                (_makegrid p1 p3 3)
            )                 
        )
    )
    (redraw) (princ)
)
(vl-load-com) (princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;