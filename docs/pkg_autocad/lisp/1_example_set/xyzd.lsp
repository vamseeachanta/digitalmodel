;load program then enter wrco at command line
(defun c:11 (/)
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
  
(while
    (setq osmd (getvar "osmode"))
    (initget 1)
    (setq px (getpoint "\n click the point "))
    (setq osmd (getvar "osmode"))
    (initget 1)
    (setq pt (getpoint px "\n click for place for writing"))
    (setq y (cadr px))
    (setq x (car px))
    (setq z(last px))
  (if(null z)
    (setq z z))
  (setq znew(getreal(strcat"\n enter level or fix reading level<"(rtos z 2 3)">:")))
(if znew(setq z znew))
(setvar "osmode"0)
    (setvar"textstyle" "standard")
    (setq dscrp (getstring 1"\n description of the point(press enter for nothing):"))
    (if(null dscrp)(progn
    (command "_qleader" px pt""""(strcat"x="(rtos x 2 3 ))
	     (strcat"y="(rtos y 2 3 ))(strcat"z="(rtos z 2 2 )) "" "")));end of if
    (command "_qleader" px pt""""(strcat"x="(rtos x 2 3 ))
	     (strcat"y="(rtos y 2 3 ))(strcat"z="(rtos z 2 2 )) dscrp"" "")
  (setq tds(entlast))
(if (null sca)
      (setq sca 1)
      )
    (initget 6)
    (setq scan (getreal (strcat"\n enter scale factor<"(rtos sca 2 3)">:")))
    (if scan(setq sca scan))
  (command"scale"tds"" pt sca) 
  (setvar"angbase"andb)
  (setvar "angdir"andr)
  (setvar"aunits"aunt)
  (setvar"osmode"osmd)
  
    )
      )