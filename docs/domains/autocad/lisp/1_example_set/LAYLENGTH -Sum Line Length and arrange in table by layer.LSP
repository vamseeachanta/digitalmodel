(defun C:LAYLENGTH ( / *error* acdoc ss p i e a d l) (vl-load-com)
  (setq acdoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark acdoc)

  (defun *error* (msg)
    (and
      msg
      (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (if
      (= 8 (logand (getvar 'undoctl) 8))
      (vla-endundomark acdoc)
    )
    (princ)
    )
  
  (if
    (and
      (setq ss (ssget ":L" '((0 . "LINE,POLYLINE,LWPOLYLINE,ARC,CIRCLE,ELLIPSE,SPLINE"))))
      (setq p (getpoint "\nTable scale depend on annotation scale.\nSpecify table insert point: "))
      )
    (progn
      (repeat
        (setq i (sslength ss))
        (setq e (ssname ss (setq i (1- i)))
              a (cdr (assoc 8 (entget e)))
              d (vlax-curve-getdistatparam e (vlax-curve-getendparam e))
        )
        (if
          (setq o (assoc a l))
          (setq l (subst (list a (+ (cadr o) d)) o l))
          (setq l (cons (list a d) l))
        )
      )
      (setq l (vl-sort l '(lambda (a b) (< (car a) (car b)))))
      (insert_table l p)
      )
    )
  (*error* nil)
  (princ)
  )

(defun insert_table (lst pct / tab row col ht i n space)
  (setq space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace))
        ht  (/ 2.5 (cond ((getvar 'cannoscalevalue)) (1.0)))
        pct (trans pct 1 0)
        n   (trans '(1 0 0) 1 0 T)
        tab (setq tab (vla-addtable space (vlax-3d-point pct) (+ 2 (length lst)) (length (car lst)) (* 2.5 ht) ht))
        )
  (vlax-put tab 'direction n)
  
  (mapcar
    (function
      (lambda (rowType)
        (vla-SetTextStyle  tab rowType (getvar 'textstyle))
        (vla-SetTextHeight tab rowType ht)
      )
    )
   '(2 4 1)
  )
  
  (vla-put-HorzCellMargin tab (* 0.14 ht))
  (vla-put-VertCellMargin tab (* 0.14 ht))

  (setq lst (cons (mapcar '(lambda (a) (strcat "Col" (itoa (1+ (vl-position a (car lst)))))) (car lst)) lst))

  (setq i 0)
  (foreach col (apply 'mapcar (cons 'list lst))
    (vla-SetColumnWidth tab i
      (apply
        'max
        (mapcar
          '(lambda (x)
             ((lambda (txb) (+ (abs (- (caadr txb) (caar txb))) (* 2.0 ht)))
              (textbox (list (cons 1 (vl-princ-to-string x)) (cons 7 (getvar 'textstyle)) (cons 40 ht)))
              )
             )
          col
          )
        )
      )
    (setq i (1+ i))
    )
  
  (setq lst (cons '("TITLE") lst))
  
  (setq row 0)
  (foreach r lst
    (setq col 0)
    (vla-SetRowHeight tab row (* 1.5 ht))
    (foreach c r
      (vla-SetText tab row col (if (numberp c) (rtos c) (vl-princ-to-string c)))
      (setq col (1+ col))
      )
    (setq row (1+ row))
    )
  )