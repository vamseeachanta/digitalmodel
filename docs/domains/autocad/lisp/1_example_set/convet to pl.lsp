(defun c:Segs ( / *error* doc ss undo )
  ; Lee Mac  ~  29.01.10
  (vl-load-com) 

  (defun *error* ( msg )
    (and undo (vla-EndUndomark doc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (setq doc   (vla-get-ActiveDocument (vlax-get-acad-object))
        *segs (cond (*segs) (10)))

  (if (and (setq ss (ssget "_:L" '((0 . "ARC,CIRCLE,*POLYLINE,SPLINE,LINE,ELLIPSE"))))
           (not (initget 6))
           (setq *segs (cond ((getint (strcat "\nSpecify Number of Segments <"
                                              (itoa *segs) "> : "))) (*segs))))
    (
      (lambda ( j / ent inc i pts )
        (setq undo (not (vla-StartUndoMark doc)))
        (while (setq ent (ssname ss (setq j (1+ j))))

          (setq inc (/ (vlax-curve-getDistatParam ent
                         (vlax-curve-getEndParam ent)) (float *segs)) i -1)
          
          (repeat (1+ *segs)
            (setq pts (cons (vlax-curve-getPointatDist ent (* (setq i (1+ i)) inc)) pts))
          )          
          (entmake (append (list (cons 0   "LWPOLYLINE")
                                 (cons 100 "AcDbEntity")
                                 (cons 100 "AcDbPolyline")
                                 (cons 90 (length pts))
                                 (cons 70 0))
                           (vl-remove 'nil
                             (mapcar
                               '(lambda ( d ) (assoc d (entget ent))) '(6 8 39 48 62)
                             )
                           )                     
                           (mapcar (function (lambda ( a ) (cons 10 a))) pts)))
          
          (entdel ent) (setq pts nil)
        )
        (setq undo (vla-EndUndoMark doc))
      )
      -1
    )
  )
  (princ)
)