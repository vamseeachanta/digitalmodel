
(defun c:nm (/)

  (setq osm (getvar "osmode"))

  (setvar "osmode" 0)


  (setq sp (getpoint "\nPick the starting point? "))
  (setq txtht (getreal "\nWhat is the text height? "))
  (setq coats (getint "\nHow many coats of enamel? "))
  (setq matl (getstring T "\nWhat is the material? "))
  (setq c_color (getstring T "\nWhat is color of enamel?	"))
  (setq c_texture (getstring T "\nWhat is the enamel texture? "))
  (initget 1 "y n")
  (setq q1 (getkword "\nDo you want standard tolerances? [y n] "))
  (if (= q1 "y")
    (setq frac "1/6"
	  1dec "0.06"
	  2dec "0.03"
	  3dec "0.010"
	  ang  "0.5%%d"
    )
  )
  (if (= q1 "n")
        (setq frac (getstring "\nfraction ")
	  1dec (getstring "\n1 dec? ")
	  2dec (getstring "\n2 dec? ")
	  3dec (getstring "\n3 dec? ")
	  ang  (getstring "\nangular? ")
    )
  )


  (setq pt (strcat "@0,-" (rtos (* 2 txtht))))
  (command "text" sp txtht "0" "Notes: ")
  (command "text" pt "" "" (strcat "1. Materials: " (strcase matl T) "."))
  (command "text" pt ""	"" "2. Remove all sharp edges and burrs. " )
  (command "text" pt "" "" "3. Paint with one coat of primer and with " (itoa Coats) "coat(s) of" (strcase c_texture T)" " (strcase c_color T) " enamel." )
    (command "text" pt "" "" "4. Tolerances unless otherwise specified: ")
	   (command "text" pt "" "" (strcat " Fraction: %%p" (strcase frac T)))
	   (command "text" pt "" "" (strcat " 1 decimal: %%p" (strcase 1dec T)))
	   (command "text" pt "" "" (strcat " 2 decimal: %%p" (strcase 2dec T)))
	   (command "text" pt "" "" (strcat " 3 decimal: %%p" (strcase 3dec T)))
	   (command "text" pt "" "" (strcat " Angular: %%p" (strcase ang T) "%%d"))
	   
	   (setvar "osmode" osm)
	   (princ)
  )





