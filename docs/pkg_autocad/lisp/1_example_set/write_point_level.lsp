;usefull of this program suppose you have topographic drawing and you want to print level
;(z-coordinate)of a point near the point this program provide this service and make layer for
;text level ,to use the program load it then write at command line:wpl
(defun c:wpl(/)
  (while
  (setq pxn (getpoint "\n click the point to write its level:"))
  (setq osmod (getvar "osmode"))
  (setq hx (last pxn))
  (setq pxn1 (list (car pxn)(-(cadr pxn)1.32)(last pxn)))
  (command "layer" "m" "NEW LEVEL" "")
  (command "layer" "c" "5" "NEW LEVEL" "")
  (command "color" "bylayer")
  (setq obm (getvar "blipmode")) 
(setq oom (getvar "osmode"))
  (setq andr(getvar"angdir"))
 (setvar "angdir"1)
 (setq andb(getvar"angbase")) 
(setvar "angbase"(/ pi 2))
(setq aunt(getvar"aunits"))  
(setvar "aunits"2)
  (setvar "dimaunit"2)
  (setvar"dimadec"4)

  (setvar "osmode"0)
(command "text" pxn1 "4.2" 100 (rtos hx 2 2))
  (setvar"angbase"andb)
  (setvar "angdir"andr)
 (setvar"aunits"aunt)
   (setvar "osmode" osmod)
  )
  )