(defun c:ex ()
  (setq os (getvar "osmode"))
  (setvar "osmode" 512)
  (setq pt1 (getpoint "\nPick point near end of line  "))
  (setvar "osmode" os)
  (setq dis (getdist pt1 "\nDistance to extend  "))
  (command "circle" "endpoint" pt1 dis)
  (command "extend" "last" "" pt1 "")
  (command "erase" "last" "")
 )
