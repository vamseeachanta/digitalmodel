;;--------------------=={ CurveText.lsp }==-------------------;;
;;                                                            ;;
;;  Positions Text along a curve object (arc, circle, spline, ;;
;;  ellipse, line, lwpolyline, polyline), and rotates text    ;;
;;  to fit to the curve accordingly.                          ;;
;;                                                            ;;
;;  If run in versions > AutoCAD2000, the resultant text will ;;
;;  form an anonymous group.                                  ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version:  1.4    -    22-02-2012                          ;;
;;------------------------------------------------------------;;

(defun c:CurveText ( / sel str obj )
    (cond
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar 'CLAYER))))))
            (princ "\nCurrent Layer Locked.")
        )
        (   (and
                (setq sel
                    (LM:SelectionOrText "\nSpecify or Select Text String: "
                        (function
                            (lambda ( x ) (wcmatch (cdr (assoc 0 (entget x))) "*TEXT,ATTRIB"))
                        )
                    )
                )
                (or
                    (and
                        (eq 'STR (type sel))
                        (setq str sel)
                    )
                    (setq str (cdr (assoc 1 (entget sel))))
                )
                (setq obj (LM:SelectIf "\nSelect Curve: " 'LM:CurveObject-p))
            )
            (LM:CurveText str obj)
        )
    )
    (princ)
)

(defun LM:CurveText ( str ent / *error* 3pi/2 a1 a2 a3 acdoc acspc df di dr g1 g2 gr in ln lst ms obj p1 p2 p3 pi/2 ts )

    (defun *error* ( msg )
        (foreach obj lst
            (if
                (and
                    (not (vlax-erased-p obj))
                    (vlax-write-enabled-p obj)
                )
                (vla-delete obj)
            )
        )
        (if (null (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
          acspc (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'paperspace 'modelspace))
    )
    (or *offset* (setq *offset* 0.0))
    (or *spacin* (setq *spacin* 1.1))

    (setq ts
        (/  (getvar 'textsize)
            (if (LM:isAnnotative (getvar 'textstyle))
                (cond ((getvar 'cannoscalevalue)) ( 1.0 ))
                1.0
            )
        )
    )

    (setq lst
        (mapcar
            (function
                (lambda ( c )
                    (setq obj (vla-addtext acspc (chr c) (vlax-3D-point (getvar 'VIEWCTR)) ts))
                    (vla-put-alignment obj acalignmentmiddlecenter)
                    obj
                )
            )
            (vl-string->list str)
        )
    )

    (setq ms    (princ "\nPosition Text: [+/-] Offset, [</>] Spacing")
          ln    (- (/ (1+ (strlen str)) 2.0))
          pi/2  (/ pi 2.0)
          3pi/2 (/ (* 3.0 pi) 2.0)
    )
    (while
        (progn
            (setq gr (grread t 15 0)
                  g1 (car  gr)
                  g2 (cadr gr)
            )
            (cond
                (   (or (= 05 g1) (= 03 g1))
                    (setq p1 (trans g2 1 0)
                          p2 (vlax-curve-getclosestpointto ent p1)
                          a1 (angle p2 p1)
                          di (vlax-curve-getdistatpoint ent p2)
                          dr (angle '(0.0 0.0 0.0) (vlax-curve-getfirstderiv ent (vlax-curve-getparamatpoint ent p2)))
                          df (- a1 dr)
                          in ln
                          a2 (cond
                                 (   (and (> dr pi/2) (<= dr pi))
                                     (- pi)
                                 )
                                 (   (and (> dr pi) (<= dr 3pi/2))
                                     pi
                                 )
                                 (   0.0   )
                             )
                    )
                    (foreach obj
                        (if (and (< pi/2 dr) (<= dr 3pi/2))
                            (reverse lst)
                            lst
                        )
                        (if (setq p3 (vlax-curve-getPointatDist ent (+ di (* (setq in (1+ in)) *spacin* ts))))
                            (progn
                                (setq a3 (angle '(0. 0. 0.) (vlax-curve-getfirstderiv ent (vlax-curve-getparamatpoint ent p3))))
                                (vla-put-TextAlignmentPoint obj
                                    (vlax-3D-point (polar p3 (+ a3 df) (* ts *offset*)))
                                )
                                (vla-put-rotation obj (+ a2 a3))
                            )
                        )
                    )
                    (= 05 g1)
                )
                (   (= 25 g1)
                    nil
                )
                (   (= 02 g1)
                    (cond
                        (   (member g2 '(13 32))
                            nil
                        )
                        (   (member g2 '(43 61))
                            (setq *offset* (+ *offset* 0.1))
                        )
                        (   (member g2 '(45 95))
                            (setq *offset* (- *offset* 0.1))
                        )
                        (   (member g2 '(46 62))
                            (setq *spacin* (+ *spacin* 0.05))
                        )
                        (   (member g2 '(44 60))
                            (setq *spacin* (- *spacin* 0.05))
                        )
                        (   (princ (strcat "\nInvalid Keypress." ms))
                        )
                    )
                )
                (   t  )
            )
        )        
    )
    
    (if (< 15.0 (atof (getvar 'ACADVER)))
        (vla-appenditems (vla-add (vla-get-groups acdoc) "*")
            (vlax-make-variant
                (vlax-safearray-fill
                    (vlax-make-safearray vlax-vbObject (cons 0 (1- (length lst))))
                    lst
                )
            )
        )
    )
    (princ)
)

(defun LM:CurveObject-p ( ent )
    (null
        (vl-catch-all-error-p
            (vl-catch-all-apply 'vlax-curve-getEndParam (list ent))
        )
    )
)

(defun LM:SelectIf ( msg pred )
    (
        (lambda ( f / e )
            (while
                (progn (setvar 'ERRNO 0) (setq e (car (entsel msg)))
                    (cond
                        (   (= 7 (getvar 'ERRNO))
                            (princ "\nMissed, try again.")
                        )
                        (   (eq 'ENAME (type e))
                            (if (and f (null (f e)))
                                (princ "\nInvalid Object.")
                            )
                        )
                    )
                )
            )
            e
        )
        (eval pred)
    )
)

(defun LM:isAnnotative ( style / obj xdt )
    (and
        (setq obj (tblobjname "STYLE" style))
        (setq xdt (cadr (assoc -3 (entget obj '("AcadAnnotative")))))
        (= 1 (cdr (assoc 1070 (reverse xdt))))
    )
)

(defun LM:SelectionOrText ( msg pred / en g1 g2 gr result )
    (setq pred (eval pred))
    
    (if msg
        (princ msg)
        (princ (setq msg "\nSelect Objects or Enter Text: "))
    )
    (setq result "")

    (while
        (progn
            (setq gr (grread t 13 2)
                  g1 (car  gr)
                  g2 (cadr gr)
            )
            (cond
                (   (= 03 g1)
                    (if (setq en (car (nentselp g2)))
                        (if (pred en)
                            (not (setq result en))
                            (princ (strcat "\nInvalid Object Selected." msg))
                        )
                        (princ (strcat "\nMissed, try again." msg))
                    )
                )
                (   (= 02 g1)
                    (cond
                        (   (< 31 g2 127)
                            (setq result (strcat result (princ (chr g2))))
                        )
                        (   (= 13 g2)
                            nil
                        )
                        (   (= 08 g2)
                            (if (< 0 (strlen result))
                                (progn
                                    (setq result (substr result 1 (1- (strlen result))))
                                    (princ (vl-list->string '(8 32 8)))
                                )
                            )
                            t
                        )
                        (   t   )
                    )
                )
                (   (= 25 g1)
                    nil
                )
                (   t   )
            )
        )
    )
    result
)

(vl-load-com)
(princ "\n:: CurveText.lsp | Version 1.4 | © Lee Mac 2012 www.lee-mac.com ::")
(princ "\n:: Type \"CurveText\" to Invoke ::")
(princ)