(defun C:LEGEND ( / *error* acdoc acobj an co e hs ht i la lst lt p p1 p2 p3 space ss st ro dr)
  (vl-load-com)
  (setq acObj (vlax-get-acad-object)
        acDoc (vla-get-activedocument acObj)
        space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace))
  )
  (vla-startundomark acDoc)
  
  ;;;;;; Error function ;;;;;;;;;
  (defun *error* (msg)
    (and
      msg
      (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
      (princ (strcat "\nError: " msg))
      )
    (if (and a (not (vlax-erased-p a))) (vla-delete a))
    (vla-endundomark acDoc)
    (princ)
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq st (entget (tblobjname "style" (getvar 'textstyle)) '("AcadAnnotative"))
        an (member '(1070 . 1) (cdr (member '(1070 . 1) (cadr (assoc -3 st)))))
        hs (cdr (assoc 40 st))
        ro (angle '(0 0 0) (trans (getvar "UCSXDIR") 0 (trans '(0 0 1) 1 0 T)))
        dr (trans '(0 0 1) 1 0 T)
        )
  (if
    an
    (setq ht (/ (if (> hs 0) hs 3.0) (cond ((getvar 'cannoscalevalue)) (1.0))))
    (setq ht (* (if (> hs 0) hs 3.0) (getvar 'ltscale)))
    )
  (if
    (setq ss (ssget))
    (progn
      (repeat (setq i (sslength ss))
        (setq
          e  (entget (ssname ss (setq i (1- i))))
          la (cdr (assoc 8  e))
          lt (cdr (assoc 6  e))
          co (cdr (assoc 62 e))
        )
        (if
          (not (member (list la lt co) lst))
          (setq lst (cons (list la lt co) lst))
        )
      )
      (setq lst (vl-sort lst '(lambda (a b) (< (car a) (car b)))))
      (if
        (setq p (getpoint "\nSpecify insert point: "))
        (foreach x lst
          (setq p1 (trans p 1 0)
                p2 (trans (polar p 0.0 (* 10 ht)) 1 0)
                p3 (trans (polar p 0.0 (* 11 ht)) 1 0)
                )
          (entmake
            (list
              '(0 . "LINE")
              (cons 8 (car x))
              (cons 10 p1)
              (cons 11 p2)
              (cons 6  (cond ((cadr  x)) ("ByLayer")))
              (cons 62 (cond ((caddr x)) (256)))
              )
            )
          (vla-put-textalignmentpoint
            (vlax-ename->vla-object
              (entmakex
                (list
                  '(0 . "TEXT")
                  (cons 8 (car x))
                  (cons 6 (cond ((cadr x)) ("ByLayer")))
                  (cons 62 (cond ((caddr x)) (256)))
                  '(100 . "AcDbText")
                  (list 10 0 0 0)
                  (cons 40 ht)
                  (cons 1 (car x))
                  (cons 50 ro)
                  (cons 7 (getvar 'textstyle))
                  (cons 72 0)
                  (list 10 0 0 0)
                  (cons 210 dr)
                  (cons 73 2)
                )
              )
            )
            (vlax-3d-point p3)
          )
          (setq p (polar p (/ pi -2.0) (* 2 ht)))
          )
        )
      )
    )
  (*error* nil)
  (princ)
  )
