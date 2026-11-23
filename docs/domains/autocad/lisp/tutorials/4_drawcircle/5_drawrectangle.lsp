; Draw Rectangle using polyline command

(defun c:drawrectagle () 
  (setq point_1 (list 0 0))
  (setq point_2 (list 0 500))
  (setq point_3 (list 500 500))
  (setq point_4 (list 500 0))
  (command "pline" point_1 point_2 point_3 point_4 "c")
)

(c:drawrectagle)