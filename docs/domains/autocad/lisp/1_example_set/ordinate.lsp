
(defun c:or()

;;;  Get the current Settings
  (setq oColor (getvar "CECOLOR"))
  (setq oLayer (getvar "CLAYER"))
  (setq oLtype (getvar "CELTYPE"))

  (command "-LAYER" "M" "30" "")
  (setvar "CECOLOR" "RED") 
  (setvar "CELTYPE" "CONTINUOUS") 

  (setq pt (getpoint "\nEnter Point for coordinate:"))
	
  (setq pt1 (trans pt 1 0))
        (setq ox (car pt1))
        (setq nx (rtos ox 2 4))
	(setq nx1 (strcat "E " nx))

          
        (setq oy (cadr pt1))
        (setq ny (rtos oy 2 4))
	(setq ny1 (strcat "N " ny ))

               
        (command "leader" pt pause pause "" ny1 nx1 "" )
	(princ)

(setvar "CECOLOR" oColor)
(setvar "CLAYER"  oLayer)
(setvar "CELTYPE" oLtype)  
  
)
