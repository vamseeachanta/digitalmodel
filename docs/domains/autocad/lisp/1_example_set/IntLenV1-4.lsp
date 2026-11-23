;;-------------=={ Length Between Intersections }==-----------;;
;;                                                            ;;
;;  Displays the length of segments of a curve divided at     ;;
;;  intersections with other objects.                         ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.4    -    26-04-2011                            ;;
;;------------------------------------------------------------;;

(defun c:IntLen ( / *error* _iscurveobject e )

  (defun *error* ( msg )
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _IsCurveObject ( entity / param )
    (and
      (not
        (vl-catch-all-error-p
          (setq param
            (vl-catch-all-apply 'vlax-curve-getendparam (list entity))
          )
        )
      )
      param
    )
  )

  (if (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar 'CLAYER))))))
    (princ "\n--> Current Layer Locked.")  
    (while
      (progn (setvar 'ERRNO 0) (setq e (car (entsel)))
        (cond
          (
            (= 7 (getvar 'ERRNO))

            (princ "\n--> Missed, Try again.")
          )
          (
            (eq 'ENAME (type e))

            (if (_iscurveobject e)
              (LM:IntersectionLengths e)
              (princ "\n--> Invalid Object Selected.")
            )
            t
          )
        )
      )
    )
  )
  (princ)
)

;;------------------------------------------------------------;;

(defun c:IntLenM ( / *error* ss i )

  (defun *error* ( msg )
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (if (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar 'CLAYER))))))
    (princ "\n--> Current Layer Locked.")
    (if (setq ss (ssget '((0 . "ARC,CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE"))))
      (repeat (setq i (sslength ss))
        (LM:IntersectionLengths (ssname ss (setq i (1- i))))
      )
    )
  )

  (princ)
)

;;------------------------------------------------------------;;

(defun LM:IntersectionLengths

  ( e  ;; Entity name
    
    / *error* _startundo _endundo _groupbynum _sortbyparam _makereadable _isannotative _uniquefuzz
      a acspc c d d1 d2 da e i l ll m o ss ta to ts ur x y
  )

  (setq acdoc (cond ( acdoc ) ( (vla-get-activedocument (vlax-get-acad-object)) ))
        acspc (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'Paperspace 'Modelspace))
  )

  (defun *error* ( msg )
    (if acdoc (_EndUndo acdoc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL))) (vla-EndUndoMark doc))
  )

  (defun _GroupByNum ( l n / r)
    (if l
      (cons
        (reverse (repeat n (setq r (cons (car l) r) l (cdr l)) r))
        (_GroupByNum l n)
      )
    )
  )

  (defun _SortbyParam ( e l )
    (vl-sort l '(lambda ( a b ) (< (vlax-curve-getParamatPoint e a) (vlax-curve-getParamatPoint e b))))
  )

  (defun _MakeReadable ( a )
    (
      (lambda ( a )
        (cond
          ( (and (> a (/ pi 2)) (<= a pi))

            (- a pi)
          )
          ( (and (> a pi) (<= a (/ (* 3 pi) 2)))

            (+ a pi)
          )
          ( a )
        )
      )
      (rem a (* 2 pi))
    )
  )

  (defun _isAnnotative ( style / object annotx )
    (and
      (setq object (tblobjname "STYLE" style))
      (setq annotx (cadr (assoc -3 (entget object '("AcadAnnotative")))))
      (= 1 (cdr (assoc 1070 (reverse annotx))))
    )
  )

  (defun _uniquefuzz ( lst fuzz )
    (if lst
      (cons (car lst)
        (_uniquefuzz
          (vl-remove-if '(lambda ( x ) (equal x (car lst) fuzz)) (cdr lst)) fuzz
        )
      )
    )
  )

  (setq ts
    (/ (getvar 'textsize)
      (if (_isAnnotative (getvar 'textstyle))
        (cond ( (getvar 'cannoscalevalue) ) ( 1.0 )) 1.0
      )
    )
  )

  (_StartUndo acdoc)
  
  (vla-getBoundingBox (setq o (vlax-ename->vla-object e)) 'll 'ur)

  (mapcar '(lambda ( x ) (set x (vlax-safearray->list (eval x)))) '(ll ur))

  (if
    (setq l
      (_sortbyparam e
        (_uniquefuzz
          (apply 'append
            (repeat
              (setq i
                (sslength
                  (ssdel e
                    (setq ss
                      (ssget "_C" (trans ur 0 1) (trans ll 0 1) '((0 . "ARC,CIRCLE,ELLIPSE,*LINE")))
                    )
                  )
                )
              )
              (setq l
                (cons
                  (_groupbynum
                    (vlax-invoke o 'intersectwith
                      (vlax-ename->vla-object (ssname ss (setq i (1- i)))) acextendnone
                    )
                    3
                  )
                  l
                )
              )
            )
          )
          1e-8
        )
      )
    )
    (if (not (vlax-curve-isClosed e))
      (progn
        (or
          (equal (vlax-curve-getStartParam e) (vlax-curve-getParamatPoint e (car l)) 0.001)
          (setq l (cons (vlax-curve-getStartPoint e) l))
        )
        (or
          (equal (vlax-curve-getEndParam e) (vlax-curve-getParamatPoint e (last l)) 0.001)
          (setq l (append l (list (vlax-curve-getEndPoint e))))
        )
      )
      (setq c l)
    )
    (if (vlax-curve-isClosed e)
      (setq l (list (vlax-curve-getStartPoint e)) c l)
      (setq l (list (vlax-curve-getStartPoint e) (vlax-curve-getEndPoint e)))
    )
  )

  (while (cadr l) (setq x (car l) y (cadr l) l (cdr l))
    (setq m
      (vlax-curve-getPointatDist e
        (/ (+ (vlax-curve-getDistatPoint e y) (vlax-curve-getDistAtPoint e x)) 2.)
      )
    )
    (setq d
      (abs
        (- (vlax-curve-getDistatPoint e y) (vlax-curve-getDistAtPoint e x))
      )
    )
    (setq a
      (angle '(0. 0. 0.)
        (vlax-curve-getFirstDeriv e (vlax-curve-getParamatPoint e m))
      )
    )
    (setq ta (_makereadable a))

    (setq to (vla-AddText acspc (rtos d) (vlax-3D-point '(0. 0. 0.)) ts))
    (vla-put-Alignment to acAlignmentMiddleCenter)
    (vla-put-TextAlignmentPoint to (vlax-3D-point (polar m (+ ta (/ pi 2.)) (* 1.1 ts))))
    (vla-put-rotation to ta)    
  )
  
  (if (vlax-curve-isclosed e)
    (progn
      (if (= 1 (length c)) (setq c (append c c)))
      (setq d
        (+
          (setq d1 (vlax-curve-getDistatPoint e (car c)))
          (setq d2 (- (vlax-curve-getdistatparam e (vlax-curve-getendparam e)) (vlax-curve-getdistatpoint e (last c))))
        )
      )                  
      (setq m
        (vlax-curve-getPointatDist e
          (if (< d1 (setq da (/ (+ d1 d2) 2.)))
            (setq da (- (vlax-curve-getdistatparam e (vlax-curve-getendparam e)) (- da d1)))
            (setq da (- da d2))
          )
        )
      )
      (setq a
        (angle '(0. 0. 0.)
          (vlax-curve-getFirstDeriv e (vlax-curve-getParamatPoint e m))
        )
      )
      (setq ta (_makereadable a))

      (setq to (vla-AddText acspc (rtos d) (vlax-3D-point '(0. 0. 0.)) ts))
      (vla-put-Alignment to acAlignmentMiddleCenter)
      (vla-put-TextAlignmentPoint to (vlax-3D-point (polar m (+ ta (/ pi 2.)) (* 1.1 ts))))
      (vla-put-rotation to ta)
    )
  )

  (_EndUndo acdoc)
  (princ)
)

;;------------------------------------------------------------;;

(vl-load-com)
(princ)
(princ "\n:: IntLen.lsp | Version 1.4 | © Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Type \"IntLen\" or \"IntLenM\" to Invoke ::")
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;