;****     Object Numbering Routine     ****
;****  Copyright 1997  Neil Angelette  ****
;
;This program is shareware and has not been registered.
;See the readme.txt and register.txt files.
;All registered users will receive product update information
;and a free copy of the next object numbering routine
;which will have additional features for object numbering.
;
;Angular units must be set to decimal degrees
;
(defun c:objno ()
(setq a (getint "Enter First Number:"))(terpri)
(setq h (getreal "Enter Text Height:"))(terpri)
(setq x 1)
(while x
(command "text" "j" "mc" pause h "0" a)(terpri)
(setq a (+ a 1))
(princ"This program has not yet been registered")(terpri)
(princ"See the readme.txt and register.txt files")(terpri)
)
)
