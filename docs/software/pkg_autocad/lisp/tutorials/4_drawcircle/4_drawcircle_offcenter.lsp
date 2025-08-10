(defun c:drawcircle_offcenter () 
  (setq centrepoint (list 300 0))
  (setq radius 200)

  (command "circle" centrepoint radius "")
)

