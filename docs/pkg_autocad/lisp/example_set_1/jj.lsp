(defun c:jj ()
(repeat
(setq x1 (getreal "\nEnter offset 	")
      y1 (getreal "\nEnter the level 	")
)  
(setq p1 (list x1 y1)
)
(command "line" p1)))