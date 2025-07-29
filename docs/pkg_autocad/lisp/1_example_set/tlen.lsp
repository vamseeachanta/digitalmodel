;|

TLEN.LSP - Total LENgth of selected objects
(c) 1998 Tee Square Graphics

|;

(defun C:TLEN (/ ss tl n ent itm obj l)
  (setq ss (ssget)
        tl 0
        n (1- (sslength ss)))
  (while (>= n 0)
    (setq ent (entget (setq itm (ssname ss n)))
          obj (cdr (assoc 0 ent))
          l (cond
              ((= obj "LINE")
                (distance (cdr (assoc 10 ent))(cdr (assoc 11 ent))))
              ((= obj "ARC")
                (* (cdr (assoc 40 ent))
                   (if (minusp (setq l (- (cdr (assoc 51 ent))
                                          (cdr (assoc 50 ent)))))
                     (+ pi pi l) l)))
              ((or (= obj "CIRCLE")(= obj "SPLINE")(= obj "POLYLINE")
                   (= obj "LWPOLYLINE")(= obj "ELLIPSE"))
                (command "_.area" "_o" itm)
                (getvar "perimeter"))
              (T 0))
          tl (+ tl l)
          n (1- n)))
  (alert (strcat "Total length of selected objects is " (rtos tl)))
  (princ)
)
