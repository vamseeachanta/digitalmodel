;|

NEW-LIN.LSP -- (c) 2000 Tee Square Graphics

	NEW-LIN is a useful AutoLISP routine that extracts parameters for
	unknown LineTypes in a drawing, and creates entries in a new LineType
	definition file, NEW-ACAD.LIN. After extraction, the LineType definitions
	may be moved to ACAD.LIN or any other *.LIN file desired by the user.

	This version of NEW-LIN.LSP functions fully with simple LineTypes, and
	Complex LineTypes composed of linear elements and Text objects. Because
	of difficulty in extracting Shape data from shape definition (*.shx)
	files, the user may, for the time being, have to supply the appropriate
	name for the Shape represented by {Shape #nnn} in NEW-ACAD.LIN, in cases
	where the associated Shape Source File (*.shp) is unavailable.

|;

(defun C:NEW-LIN (/ flag outf ltname tblent tblist i desc alist acode value
										rot shpno shxfl shpfl inf dat n shpnm flg txt sty)
	(setq flag (findfile "new-acad.lin")
				outf (open (if flag flag "new-acad.lin") "w"))
	(write-line ";;" outf)
	(write-line ";; New LineType descriptions extracted" outf)
	(write-line ";; from existing drawing(s) by NEW-LIN.LSP." outf)
	(write-line ";;" outf)
	(write-line ";; NEW-LIN.LSP (c) 2000 Tee Square Graphics" outf)
	(write-line ";;\n" outf)
	(setvar "luprec" 8)
	(setvar "auprec" 8)
	(tblnext "ltype" T)
	(while (setq tblent (tblnext "ltype"))
		(setq ltname (cdr (assoc 2 tblent))
					tblent (tblobjname "ltype" ltname)
					tblist (entget tblent)
					i 1
					desc "A,")
		(write-line (strcat "*" (cdr (assoc 2 tblist)) "," (cdr (assoc 3 tblist))) outf)
		(while (< i (length tblist))
			(setq alist (nth i tblist)
						acode (car alist)
						value (cdr alist))
			(cond
				((= acode 49)
					(setq desc (strcat desc (trim (rtos value 2 8)) ",")))
				((= acode 74)
					(setq flag (if (= (logand value 4) 4) T nil)
								rot (if (= (logand value 1) 1) "a" "r")))
				((= acode 75)
					(setq shpno (itoa value)))
				((= acode 340)
					(if flag
						(progn
							(setq shxfl (cdr (assoc 3 (entget value)))
										shpfl (strcat (substr shxfl 1 (- (strlen shxfl) 3)) "shp"))
							(if (setq inf (findfile shpfl))
								(progn
									(setq inf (open inf "r"))
									(while (setq dat (read-line inf))
										(if (wcmatch dat (strcat "`*" shpno "*"))
											(progn
												(setq n 1)
												(repeat 2
													(while (/= (substr dat n 1) ",")
														(setq n (1+ n)))
													(setq n (1+ n)))
												(setq shpnm (substr dat n)))))
									(close inf)))))
					(setq flg flag
								txt (if flag
											(if shpnm shpnm (strcat "{Shape #" shpno "}"))
											(strcat "\"" (cdr (assoc 9 (member alist tblist))) "\""))
								sty (if flag
											(cdr (assoc 3 (entget value)))
											(cdr (assoc 2 (entget value))))
								desc (strcat desc "\n[" txt "," sty ",s="
														 (trim (rtos (cdr (nth (1+ i) tblist)) 2 8)) "," rot "="
														 (trim (angtos (cdr (nth (+ i 2) tblist)) 0 8)) ",x="
														 (trim (rtos (cdr (nth (+ i 3) tblist)) 2 8)) ",y="
														 (trim (rtos (cdr (nth (+ i 4) tblist)) 2 8)) "],\n")
								i (+ i 4)))
				(T nil))
			(setq i (1+ i)))
		(write-line (substr desc 1 (1- (strlen desc))) outf)
		(write-line " " outf))
	(close outf)
	(alert (strcat "All loaded LineTypes in the current drawing database have been\n"
								 "duplicated in a new LineType definition file, NEW-ACAD.LIN.\n"
								 "Any complex LineTypes using Shape Definitions for which no\n"
								 "source file (*.shp) could be found will contain a reference in\n"
								 "curly braces { }; the user must supply the correct shape name\n"
								 "before NEW-ACAD.LIN can be used to load these LineTypes."))
	(princ)
)
(defun trim (x / i)
	(setq i (strlen x))
	(while (= (substr x i) "0")
		(setq i (1- i)
					x (substr x 1 i)))
	(if (= (substr x i) ".")
		(substr x 1 (1- i))
		x)
)