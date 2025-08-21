(defun c:txt2pt (/ Point i ss ent pt)

  (defun Point (pt) (entmakex (list (cons 0 "POINT") (cons 10 pt))))

  (if (setq i -1 ss (ssget '((0 . "TEXT,MTEXT"))))
    (while (setq ent (ssname ss (setq i (1+ i))))
      (setq pt (cdr (assoc 10 (entget ent))))

      (Point (list (car pt) (cadr pt)
                   (cond ((distof (cdr (assoc 1 (entget ent))))) (0.0))))))

  (princ))