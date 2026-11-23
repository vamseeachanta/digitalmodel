(vmon)
(princ "welcome to yaser lisp any time")
(vmon)
(vmon)
(vmon)
(vmon)
(vmon)
(vmon)
(vmon)
(vmon)
(vmon)
(vmon)
(setvar "CMDECHO" 0)
;--------------------------- INTERNAL ERROR HANDLER ---------------

(defun ssx-err (s)            ; If an error (such as CTRL-C) occurs
			      ; while any command is active...
(princ "\nError: function cancelled!")
(command "OSNAP" "NONE")
(princ)
)
(setq *error* ssx-err)
;_____________________________________________________________
;returns point  relative to an offset from a given point.
(defun C:NOSTART ()
(setq BAS (getpoint "\nEnter base point for offset: "))
(setq hordist (getdist BAS "\nEnter horizintal offset: "))
(IF (= hordist nil) (progn
       (setq incang (angle BAS (osnap (getpoint BAS "\nInclination angle: ") "NEAR")))
       (setq INCDIST (getdist BAS "\nInclined offset: "))
	(setq BAS (polar BAS INCANG INCDIST))
    )
    (PROGN
(setq verdist (getdist BAS "\nenter vertical offset: "))
(setq BAS (list (+ (car BAS) hordist) (+ (cadr BAS) verdist)))
))
)
(DEFUN C:RECT ()
(setq BPT (getpoint "\nEnter base point:  "))
(if (= BPT nil) (setq BPT (NOSTART)))
(setq DG (getangle BPT "\nInclination angle for rectangle<0>: "))
(if (= DG nil) (setq DG 0.0))
(setq SD (getdist BPT "\nRectangle base width: ")
      FOURPT (getdist BPT "\nRectangle Height: ")
      SECPT (polar BPT DG SD)
      FOURPT (polar BPT (+ (/ pi 2) DG) FOURPT)
      THIRPT (polar FOURPT DG SD)
)
(command "pline" BPT SECPT THIRPT FOURPT "C")
)

(defun C:RECTMID ()
(setq first  (getpoint "\nenter opposite corners"))
(setq second (getpoint first))
(setq basept (list (/ (+ (car first) (car second)) 2) (/ (+ (cadr first) (cadr second)) 2)))
)
;lisp program to change text entities-text
(defun C:G ()
(setq en (entsel "\nSelect text to be changed"))
(while en
(setq oldtxt (cdr (assoc 1 (entget (car en)))))
(setq newtxt (getstring T (strcat "\nNew text<" oldtxt ">: ")))
(if (= newtxt "") (setq newtxt oldtxt))
(entmod (subst (cons 1 newtxt) (assoc 1 (entget (car en))) (entget (car en))))
(setq en (entsel "\nSelect text to be changed"))
)
)

(DEFUN C:LG()
(SETQ E (CAR (ENTSEL "SELECT LINE TO BE MEASURED")))
(SETQ X (CDR (ASSOC 11 (ENTGET E))))
(SETQ Y (CDR (ASSOC 10 (ENTGET E))))
(SETQ LEN (ABS (DISTANCE Y X)))
(PRINC "\nLENGTH IS=>")
(PRINC LEN)
(PRINC)
)

(defun C:MATCHL (/ x )
(setq x (entsel "\n Select object whose layer is to be matched:"))
(command "LAYER" "S" (cdr (assoc 8 (entget (car x)))) "" )
)

(defun C:OFLAYER ()
(setq x (entsel "\n Select object whose layer is to be OFF:"))
(setq curlay (getvar "clayer"))
(setq entlay (cdr (assoc 8 (entget (car x)))))
(if (= curlay entlay) (command "layer" "off" entlay "y" "")
		      (command "layer" "off" entlay ""))
(princ)
)

;THIS PROGRAM DRAWS LINES DEPENDING ON INPUTING THE INCLUDED
;ANGLES.

(defun C:INCLINE()
(setq pt1 (getpoint "\nENTER STARTING POINT>") AP 180)
(command "LINE" PT1 PT1 "")
(setq A (getreal "\nENTER INCLUDED ANGLE [CW]> "))
(setq LEN (getdist "\nENTER LENGTH OF LINE> "))
(while (/= LEN nil)
  (setq B (- 360 A AP) AP (- 180 B))
  (setq ST (strcat "@" (rtos LEN) "<" (rtos B)))
  (command "LINE" "" ST "")
  (setq A (getreal "\nENTER INCLUDED ANGLE [CW]> "))
  (setq LEN (getdist "\nENTER LENGTH OF LINE> "))
)
)
(defun c:CHGCOLOR()
(PROMPT "\nSELECT ENTITIES TO BE MODIFIED:>")
(SETQ ENT (SSGET))
(SETQ COL (GETSTRING "\nCOLOR REQUIRED"))
(COMMAND "CHANGE" "P" "" "PROP" "COLOR" COL "")
)
(defun C:LAYMAT()
(setq TAR (entsel "\nSelect an entity on target layer: "))
(prompt "\nSelect entities to be transferred:")
(setq ENT (ssget))
(setq TARL (cdr (assoc 8 (entget (car TAR)))))
(command "CHANGE" "P" "" "LA" TARL)
)

;-------------------------- ADD AND REMOVE FUNCTIONS -------------

(defun r() (command "r") (ssx))
(defun a() (command "a") (ssx))

;-------------------------------- MAIN PROGRAM ----------------

(defun ssx (/ t1 t2 t3 f1 f2)
(setq t1 T f2 'f1)
(while t1
  (initget "Block Color Entity LAyer LType Style Thickness")
  (setq t1 (getkword
     "\n>>Block name/Color/Entity/LAyer/LType/Style/Thickness: "))
  (setq t2
     (cond
	((eq t1 "Block")      2)   ((eq t1 "Color")     62)
	((eq t1 "Entity")     0)   ((eq t1 "LAyer")      8)
	((eq t1 "LType")      6)   ((eq t1 "Style")      7)
	((eq t1 "Thickness") 39)))
  (initget 1)
  (setq t3
     (cond
	((= t2  2)  (getstring "\n>>Block name: "))
	((= t2 62)
		    (while
		       (or
			   (null t3)
			   (> t3 256)
			   (< t3 0))
		   (setq t3 (getint "\n>>Color number: "))) t3)
	((= t2  0)  (getstring "\n>>Entity type: "))
	((= t2  8)  (getstring "\n>>Layer name: "))
	((= t2  6)  (getstring "\n>>Linetype name: "))
	((= t2  7)  (getstring "\n>>Text style name: "))
	((= t2 39)  (getreal   "\n>>Thickness: "))
	(T          nil)))
  (if t3 (setq f1 (cons (cons t2 t3) f1)))
)
(setq f2 (ssget "x" (eval f2)))
(if (and f1 f2) f2 (progn (princ "\n0 found.") (prin1)))
)
(defun c:odist()
(setvar "cmdecho" 0)
(command "osnap" "near")
(setq pt1 (getpoint "\n Enter first point?"))
(command "osnap" "per")
(setq pt2 (getpoint pt1 "\n Perpendicular to point:"))
(command "osnap" "none")
(setq odist (abs (distance pt1 pt2)))
(princ "\nDistance is=")
(princ odist)
(princ)
)
(defun C:CLEAN ()
(setq atomlist (member 'C:CLEAN atomlist))
'DONE
)
(defun c:q()
     (command ".quit"))

(defun C:B()
     (command ".break"))

(defun C:CH()
     (command ".change"))

(defun C:C()
     (command ".copy"))

(defun C:E()
     (command ".erase"))

(defun C:X()
     (command ".extend"))

(defun C:F()
     (command ".fillet"))

(defun C:M()
     (command ".move"))

(defun C:MN()
     (command ".mirror"))

(defun C:OF()
     (command ".offset"))

(defun C:T()
     (command ".trim"))

(defun C:A()
     (command ".arc"))

(defun C:L()
     (command ".line"))

(defun C:CI()
     (command ".circle"))

(defun C:P()
     (command ".pan"))

(defun C:Z()
     (command ".zoom"))

(defun C:1()
     (command ".zoom" "w"))

(defun C:2()
     (command ".zoom" "p"))

(defun C:3()
     (command ".zoom" "e"))

(defun C:4()
     (command ".zoom" "a"))

(defun C:5()
     (command ".zoom" "d"))

(defun C:LA()
     (command ".ddlmodes"))

(defun C:LS()
     (setq s (getstring "New layer: "))
     (command "layer" "s" s ""))

(defun C:R()
     (command ".redraw"))

(defun C:I()
     (command ".insert"))

(defun C:BL()
     (command ".block"))

(defun C:W()
     (command ".wblock"))

(defun C:XD()
     (command ".explode"))

(defun C:D()
     (command ".dist"))

(defun C:LI()
     (command ".List"))

(defun C:Dt()
     (command ".dtext"))

(defun C:TX()
     (command ".text"))

(defun C:H()
     (command ".hatch"))
(DEFUN C:ST()
     (COMMAND ".STRETCH" "C"))
(defun c:ZA()
     (command "ZOOM" "A"))
(defun c:zw()
     (command "zoom" "w"))
(defun c:zx()
     (command "zoom" "p")) 
(defun c:ze()
     (command "zoom" "e"))
(defun c:zd()
     (command "zoom" "d"))
(defun c:do()
     (command "donut"))
(DEFUN C:TC()
     (COMMAND "TRIM" "C"))
(DEFUN C:XC()
       (COMMAND "EXTEND" "C"))
(DEFUN C:MC()
     (COMMAND "MOVE" "C"))
(DEFUN C:MW()
     (COMMAND "MOVE" "W"))
(DEFUN C:EC()
     (COMMAND "ERASE" "C"))
(DEFUN C:EW()
     (COMMAND "ERASE" "W"))
(DEFUN C:QY()
     (COMMAND "QUIT" "Y"))
(defun c:cw()
     (command "copy" "w"))
(DEFUN C:CGC()
     (COMMAND "CHANGE" "C"))
(DEFUN C:CGW()
     (COMMMAND "CHANGE" "W"))
(DEFUN C:PE()
     (COMMAND "PEDIT"))
(DEFUN C:RTC()
     (COMMAND "ROTATE" "C"))
(DEFUN C:RTW()
     (COMMAND "ROTATE" "W"))
(DEFUN c:RTL()
     (COMMAND "ROTATE" "L"))
(DEFUN C:RTP()
     (COMMAND "ROTATE" "P"))
(DEFUN C:ML()
     (COMMAND "MOVE" "L"))
(DEFUN C:MP()
     (COMMAND "MOVE" "P"))
(DEFUN C:EP()
     (COMMAND "ERASE" "P"))
(DEFUN C:SY()
    (COMMAND "STYLE"))
(DEFUN C:V()
    (COMMAND "VIEW"))
(DEFUN C:VW()
    (COMMAND "VIEW" "W"))
(DEFUN C:LN()
    (COMMAND "LAYER" "ON"))
(DEFUN C:LO()
    (COMMAND "LAYER" "OFF"))
(DEFUN C:LT()
    (COMMAND "LAYER" "THAW"))
(DEFUN C:LF()
    (COMMAND "LAYER" "FREEZE"))
(DEFUN C:QS()
    (COMMAND "SAVE"))
(DEFUN C:AR()
    (COMMAND "ARRAY"))
(DEFUN C:EL()
    (COMMAND "ERASE" "L"))
(defun c:cgw()
    (command "change" "w"))
(defun C:tl()
    (command "trim" "l"))
(defun c:zz()
    (command "zoom" "w"))
(DEFUN C:CC()
    (COMMAND "COPY" "C"))
(DEFUN C:CL()
    (COMMAND "COPY" "L"))
(DEFUN C:FI()
    (COMMAND "FILES"))
(DEFUN C:CP()
    (COMMAND "COPY" "P"))
(DEFUN C:SCC()
    (COMMAND "SCALE" "C"))
(DEFUN C:SCW()
    (COMMAND "SCALE" "W"))
(DEFUN C:SCP()
    (COMMAND "SCALE" "P"))
(DEFUN C:SCL()
    (COMMAND "SCALE" "L"))
(DEFUN C:CGP()
    (COMMAND "CHANGE" "P"))
(DEFUN C:CGL()
    (COMMAND "CHANGE" "L"))
(DEFUN C:XL()
    (COMMAND "EXTEND" "L"))
(defun c:rt()
   (command "rotate"))
(defun c:cg()
   (command "change"))
(DEFUN C:RR()
   (COMMAND "REGEN"))
(DEFUN C:XZ()
   (COMMAND "ZOOM" "P"))
(DEFUN C:N()
   (COMMAND "MOVE"))
(DEFUN C:RR()
   (COMMAND "REGEN"))
(DEFUN C:SC()
   (COMMAND "SCALE"))
(DEFUN C:XX()
   (COMMAND "ZOOM" "W"))
(DEFUN C:CF()
   (COMMAND "CHAMFER"))
(DEFUN C:FC()
   (COMMAND "FILLET" "C"))
(DEFUN C:612165()
   (COMMAND "QUIT" "Y"))
(DEFUN C:DD()
   (COMMAND "DDEDIT"))
(DEFUN C:MNL()
   (COMMNAD "MIRROR" "L"))
(DEFUN C:MNP()
   (COMMNAD "MIRROR" "P"))
(DEFUN C:MNC()
   (COMMAND "MIRROR" "C"))
(DEFUN C:MNW()
   (COMMAND "MIRROR" "W"))
(DEFUN C:NC()
   (COMMAND "MOVE" "C"))
(DEFUN C:NX()
   (COMMAND "MOVE" "C"))
(DEFUN C:MVC()
   (COMMAND "MOVE" "C"))
(DEFUN C:SA()
   (COMMAND "SAVEAS"))
(DEFUN C:OP()
   (COMMAND "OPEN"))
(DEFUN C:XDL()
   (COMMAND "EXPLODE" "L"))
(DEFUN C:XDP()
   (COMMAND "EXPLODE" "P"))
(DEFUN C:LL()
  (LOAD "DDMODIFY.LSP"))
(DEFUN C:DL()
   (COMMAND "DIM" "L"))
(DEFUN C:VR()
   (COMMAND "VIEW" "R" "Z"))
;--------------------------------------------------------------------------------------------------------------
(defun ziaderr (s_msg)
   (if (/= s_msg "Function cancelled")   
      (princ (strcat "\nError: " s_msg)) 
   )
   (setq *error* olderr) 
   (setvar "CMDECHO" s_cmd)
   (setvar "BLIPMODE" s_blp)
   (setvar "OSMODE" s_osn)
   (setq *error* olderr)
   (princ)
)

(defun start ()
   (setq s_cmd (getvar "CMDECHO")
	 s_blp (getvar "BLIPMODE")
	 s_osn (getvar "OSMODE")
     *error* ziaderr 
   )
   (setvar "CMDECHO" 0)
   (setvar "BLIPMODE" 0)
   (setvar "OSMODE" 0)
)

(defun finish ()
   (setvar "CMDECHO" s_cmd)
   (setvar "BLIPMODE" s_blp)
   (setvar "OSMODE" s_osn)
   (setq *error* olderr)
   (princ)
)

;==================================================================
(defun maklyr (lyr)
  (if (not (tblsearch "layer" lyr)) (progn
    (command "LAYER" "N" lyr "") (princ (strcat "\nLayer " lyr " Created"))
  ))
)
;=================================================================
(defun gtver (sent) 
    (setq las (if  (= 1 (cdr (assoc 70 (entget sent)))) 
		 (cdr (assoc 10 (entget (entnext sent))))
	      )
	 vlis '() sent (entnext sent)) 
    (while  (cdr (assoc 10 (entget sent)))
	  (setq vlis (append vlis (list (cdr (assoc 10 (entget sent)))))
		sent (entnext sent)
	  )
    )
    (if las (setq vlis (append vlis (list las)))
    ) 
    vlis
)

;=================================================================
(defun betwn (a b c / frg)
      (setq frg 
	     (- (+ (abs (distance a b)) (abs (distance b c))) 
		   (abs (distance a c))
	     )
       )
      (if (<= frg 0.05) b )
)
	
;=================================================================
(defun C:fp ( / pp dd)
   (setq pp (getpoint   "\n>> From Point :")
	 dd (getdist pp "\n>> Distance :")
   )
   (polar pp (getangle pp "\n>> Direction :") dd)
)

;=================================================================
(defun C:frompt (/ pp dd)
   (setq pp (getpoint "\nFrom Point :")
	 dd (getdist pp "\nDistance :")
   )
   (command "OSNAP" "NEA" )
   (setq pp (polar pp (getangle pp "\nDirection : Near to ") dd))
   (command "OSNAP" "NONE" )
   pp 
)

;==========================================================
(DEFUN C:RECT (/ IPT ANG B H BB HH)
  (start)
  (setq IPT (getpoint "\nInsertion point:  "))
  (if (null IPT) (setq IPT (NOSTART)))
  (setq ANG (getangle IPT "\nInclination angle of rectangle <0>: "))
  (if (null ANG) (setq ANG 0.0))
  (setq B (getdist IPT "\nRectangle Base Width: ")
	H (getdist IPT "\nRectangle Height: ")
  )
  (rect IPT ANG H B 1)     
  (finish)
)

(defun rect (IPT ANG H B CLOZ)     
   (setq    BB (polar IPT ANG B)
	H (polar IPT (+ (dtr 90) ANG) H)
       HH (polar H ANG B)
   )
 (IF (= CLOZ 1) (command "pline" IPT BB HH H "C") 
	  (command "pline" IPT BB HH H "")
 )
)
;==========================================================
(DEFUN c:LEN (/ ENT spt nam t X Y a b R)
  (start)
  (SETQ ENT  (ENTSEL "Select Entity to be measured")
	spt  (cadr  ENT)
	ENT  (car ENT)
	nam (cdr (assoc 0  (ENTGET ENT)))
	  t (CDR (ASSOC 1 (ENTGET ENT)))
	  X (CDR (ASSOC 10 (ENTGET ENT)))
	  Y (CDR (ASSOC 11 (ENTGET ENT)))
	  a (CDR (ASSOC 50 (ENTGET ENT)))
	  b (CDR (ASSOC 51 (ENTGET ENT)))
	  R (CDR (ASSOC 40 (ENTGET ENT)))  ;Raduis or text Hight
  )
  
  (PRINC (strcat "\nLENGTH of " nam ))
 
 (cond 
   ((= nam "LINE") (progn ( princ " = ") (PRINC (ABS (DISTANCE Y X)))))
   ((= nam "ARC") (progn 
		(princ " = ")
		(PRINC (* r (if (> a b) (+ (* 2.0 PI) (- b a)) (- b a))))
		(princ "      Radius = ") (princ r)
   ))
   ((= nam "CIRCLE") (progn (princ " Perimeter = ") (princ (* 2.0 PI r))
		     (princ "      Radius = ") (princ r)
   ))
   ((= nam "TEXT") (progn (princ " = ") (princ (strlen t))
			(prompt " Characters     Hight = ")
			(princ r)
   ))
   ((= nam "POLYLINE") (progn 
       (setq 
	     lis (gtver ent)
	     l   (length lis)
	     co 0
	     leng 0.0
	     tot 0
       )
      (while (< co (- l 1) ) (progn
	  (if (betwn  (nth co lis) spt (nth (1+ co) lis) )
	    (setq leng (abs (distance (nth co lis) (nth (1+ co) lis) ) ) )  
	  )    
      
	  (setq tot (+ tot (abs (distance (nth co lis) (nth (1+ co) lis))))
		co (1+ co)
	  )
      ))
      (princ " Segment = ") (princ leng) (princ "   Total Length = ") 
      (princ tot) (princ)
   ))
   ((= nam nam) (princ " ??!!! "))
  )
(finish)
) 

;==========================================================
(defun c:ELV (/ RPT orpts EPT ELV TPT)
       (start)
       (if sldflg (princ "\nSolid") (princ "\nHollow"))
       (if lftflg (princ " Left Sign") (princ " Right Sign"))
       (setq orpts (if (null ORPT) " " orpt ))
       (while (null RPT) 
	  (princ "\nRiference Point ") (princ orpts) 
	  (setq RPT (getpoint " : "))
	  (if (null RPT) (setq RPT ORPT))
       )
     (setq ORPT RPT
	   EPT (getpoint "Elevation Point :")
	   ELV (- (CADR EPT) (CADR RPT))
	   ELV (if (and sgnflg (> ELV 0)) (strcat "+" (rtos ELV)) (rtos ELV))
	   TPT (if lftflg (polar EPT (* PI 0.7) 0.32) 
			  (polar EPT (* PI 0.3) 0.32))
     )
     (if lftflg 
      (command "PLINE" (polar EPT (dtr 45) 0.2) EPT "@0.2<135" "@1.1<0" ""
	       "TEXT"  TPT  "0" ELV 
      )
      
      (command "PLINE" (polar EPT (dtr 135) 0.2) EPT "@0.2<45" "@1.1<180" ""
		"TEXT"  "j" "r"  TPT "0" ELV 
      )
     
     )
     
     (if sldflg (command "SOLID" EPT "@0.2<45" EPT "@0.2<135"  ""))
     (if (> slbthk 0) (rect (polar EPT PI 0.3) 0 (* -1.0 slbthk) 0.6 1))
     (finish)
)
(defun stsbtk () (setq slbthk (getdist "\n Slab Thich < 0 for none> :")))
;==========================================================
(defun c:LL (/ i objs obj2 alst)
       (start)
       (setq objs  (ssget)
	     obj2 (car (entsel "\nChange to Layer, Like what object :"))
	   i 0
       )
    (while (< i (sslength objs)) (progn
      (setq alst
	(subst (assoc 8 (entget obj2)) 
	      (assoc 8 (entget (ssname objs i))) 
	      (entget (ssname objs i))
	)
	i (+ i 1)
      )
      (entmod alst)
    ))  
   (princ "\n\n\n")
   (prompt (strcat "Changed to Layer [ " (cdr (assoc 8 (entget obj2))) " ]"))
   (finish)
)

;==========================================================
(defun c:SL (/ obj)
   (start)
   (setq obj (car (entsel "\nSet Layer, Of what object :")))
   (if obj (command "LAYER" "S" (cdr (assoc 8 (entget obj))) ""))
   (finish)
)

;==========================================================
(defun c:IC ()
    (if (= (getvar "ucsicon") 0) 
	(command  "UCSICON" "ON")
	(command  "UCSICON" "OFF")
    )
)

;==================================================================
(defun c:ons (/ lays obj cula)
 (start)
 (setq lays "")
 (while (setq obj (entsel "\nSelect object on Layer(s) to keep ON: "))
   
	 (setq lays (strcat lays "," (cdr(assoc 8 (entget (car obj))))))
	 (princ "\n\n\n")
	 (princ lays)
	 
 )
 (setq lays (substr lays 2)
       cula (entsel "\nSelect Object in New Current Layer: ")
       cula (cdr(assoc 8 (cdr(entget(car cula)))))
 )
 (command"layer" "s" cula "of" "*" "y" "on" lays "")
 (finish)
)

;==================================================================
(defun C:onl (/ obj )
   (start)
   (setq obj (entsel "\nSelect object in Layer to be ON ONLY :"))
   (if obj (progn
    (command "layer" "S" (cdr (assoc 8 (entget (car obj)))) "")
    (command "layer" "of" "*" "" "")
   ))
   (finish)
)

;==================================================================
(defun C:off (/ lay )
   (start)
   (setq lay (entsel "\nSelect Objet in The Layer to be OFF:")
	 lay (cdr (assoc 8 (entget (car lay ))))
   )
   (if (= (getvar "clayer") lay)
       (progn (command "layer" "off" lay "Y" "" )
	      (princ "WARNING Current Layer Set to OFF")
       )
       (progn (command "layer" "off" lay "" )
	      (prompt (strcat "\nLayer " lay " Set to Off"))
       )

   )
   (finish)
)

;==================================================================
(defun C:frz (/ lay )
   (start)
   (setq lay (entsel "\nSelect Objet in The Layer to be FROZEN:")
	 lay (cdr (assoc 8 (entget (car lay ))))
   )
   (if (= (getvar "clayer") lay)
       (prompt "\nCan't Freez CURRENT Layer")
       (command "layer" "F" lay "" )
   )
   (finish)
   
   (if (/= (getvar "clayer") lay)
	(prompt (strcat "\n \n Layer " lay " Frozen"))
   )
   (princ)
)

;==================================================================
(defun dtr (ag) (* ag (/ PI 180.0)))
(defun rtd (ag) (* ag (/ 180.0 PI)))

;==========================================================
(defun C:DOOR (/ INSPT WIDTH THETAL THETA THICK)
  (start)
   
   (setq   A90 (/ PI 2.0) A27 (* A90 3.0)
	 INSPT (getpoint "\nInsertion Point of Door:")
   )
    (if (null INSPT) (setq INSPT (frompt)))
   ( setq WIDTH (getdist INSPT "\nDoor Width <0.90>: "))
       (if (null WIDTH) (setq WIDTH 0.90))
       (prompt "\nOther Side Of Wall :")
	(command "LINE" inspt "PER" pause "")
   
  (setq  PT21 (cdr (assoc 11 (entget (entlast)))) 
	 THETA  (getpoint  "\nDirection of Door:")
	 THETAL (angle INSPT PT21)
	 THETA  (angle (inters INSPT PT21 THETA 
			     (polar THETA (+ THETAL A90) WIDTH)
			      nil
			)
		       THETA
		 )
	 THICK  (getdist INSPT "\nDOOR thick <0.05>: ")

  )
       (if (null THICK) (setq THICK 0.05))
  (setq PT12 (polar INSPT THETA THICK)
	PT13 (polar INSPT  THETA WIDTH)
	PT14 (polar PT12 (+ THETAL PI) WIDTH)
	PT15 (polar INSPT (+ THETAL PI) WIDTH)
	PT22 (polar PT21 THETA WIDTH)
  )
  (SBREAK PT13 INSPT) (SBREAK PT22 PT21)
  (maklyr "a-doors")
  (command          
	   "LINE" PT13 PT22 ""
	   "PLINE" INSPT PT12 PT14 PT15 INSPT ""
	   "CHANGE" (entlast) "" "P" "LA" "a-doors" "C" "BYLAYER" ""
	   "ARC" PT13  PT14  PT15 
	   "CHANGE" (entlast) "" "P" "LA" "A-DOORS" "C" "BYLAYER" ""
   )
(finish)
)
;==================================================================
(defun C:DDOOR ();(/ PT WID ORI ANG TH)
  (start)
  (setq  PT (getpoint "\nInsertion Point of Door:"))
  (if (null PT) (setq PT (frompt)))
  (prompt "\nOther Side Of Wall :")
  (command "LINE" PT "PER" pause "")
  (setq PT21 (cdr (assoc 11 (entget (entlast)))) 
	ANG (getpoint  "\nDirection of Door:")
	ORI (angle PT PT21)
	WID (getdist PT "\nLeft Door Width <0.80>: ")
  )     (if (null WID) (setq WID 0.80))
  (setq ANG (angle (inters PT PT21 ANG (polar ANG (+ ORI (dtr 90)) WID) nil) 
		    ANG)
       PT13 (polar PT  ANG WID) 
	WID2 (getdist PT13 "\nRight Door Width <0.40>: ")
  )     (if (null WID2) (setq WID2 0.40))
  (setq  TH (getdist PT "\nDOOR thick <0.05>: "))
       (if (null TH) (setq TH 0.05))
  (setq PT12 (polar PT ANG TH)    PT2 (polar PT  ANG (+ WID WID2))
				   PT122 (polar PT13 ANG (- WID2 TH)) 
	PT14 (polar PT12 (+ ORI PI) WID) PT142 (polar PT122 (+ ORI PI) WID2)
	PT15 (polar PT (+ ORI PI) WID)   PT152 (polar PT2 (+ ORI PI) WID2)
	PT22 (polar PT21 ANG WID)   PT222 (polar PT21 ANG (+ WID WID2))
  )
  (SBREAK PT2 PT) (SBREAK PT222 PT21)
  
  (maklyr "A-DOORS") 
  (command          
	   "LINE" PT2 PT222 ""
	   "PLINE" PT PT12 PT14 PT15 PT ""
	   "CHANGE" (entlast) "" "P" "LA" "a-doors" "C" "BYLAYER" ""
	   "ARC" PT13  PT14  PT15 
	   "CHANGE" (entlast) "" "P" "LA" "a-doors" "C" "BYLAYER" ""
   )
  (command "PLINE" PT2 PT122 PT142 PT152 PT2 ""
	   "CHANGE" (entlast) "" "P" "LA" "a-doors" "C" "BYLAYER" ""
	   "ARC" PT13  PT142  PT152 
	   "CHANGE" (entlast) "" "P" "LA" "a-doors" "C" "BYLAYER" ""
   )
(finish)
)


;==================================================================                  
(defun c:stair (/ pt tr ri no )
  (start)
  
    (setq pt (getpoint "\n Insertion Point :")
	  tr (rtos (getdist  pt "\n Tread Length :"))
	  ri (rtos (getdist  pt "\n Rise  :"))
	  no (getint   "\n No. of Steps :")
    )
    (command "PLINE" pt )
    (repeat no (command  (strcat "@" ri "<90") (strcat "@" tr "<0")) )
    (command "")

  (finish)
)

;==================================================================
(DEFUN C:WALL (/ PT PTL SID) 
  (start)
  (setq PT  (entsel "\nSELECT OUTER CONCRETE LINE: "))
  (setq SID  (getpoint "\ndirection of WALL")
	sw 0.07 cw 0.18 iw 0.05 bw 0.10 ccw 0.20
  )
  (maklyr "concrete") (maklyr "brick") (maklyr "stone")
  
  (command  "CHANGE" PT "" "P" "LA" "STONE" ""
	    "OFFSET" sw  PT SID "")
	    (setq PTL (entlast))
  (command  "CHANGE" PTL "" "P" "LA" "CONCRETE" ""
	    "OFFSET"  (+ sw cw) PT SID "")
	    (setq PTL (entlast))
  (command  "CHANGE" PTL "" "P" "LA" "CONCRETE" ""
	    "OFFSET"  (+ sw cw iw) PT SID "")
	    (setq PTL (entlast))
  (command "CHANGE" PTL "" "P" "LA" "BRICK" ""
	   "OFFSET" (+ cw sw iw bw) PT SID "")
	    (setq PTL (entlast))         
  (command "CHANGE" PTL "" "P" "LA" "BRICK" "")
  
  (finish)
)
;==================================================================
(defun C:WIN (/ PT WD ang sw cw iw bw ccw P2 PP2 P1 P3 P4 P5 P6 obj)
  (START)
  (setq LAY (getvar "CLAYER")    
	 PT (getpoint "\nInsertion point of window: ")
  )
  (if (null PT) (setq PT (frompt)))
  (setq WD (getdist PT "\nWindow width: "))
  (command "OSNAP" "NEAR")
  (setq ang (getangle PT "\nDirection of window : Near to "))
  (command "OSNAP" "NONE")
  (setq  sw 0.07 cw 0.18 iw 0.05 bw 0.10 ccw 0.20
	 P2 (polar PT (+ ang (dtr 270)) (+ sw cw))
	PP2 (polar P2 (+ ang PI) sw)
	 P1 (polar PP2 (+ ang (dtr 90)) cw)
	 P3 (polar P2 (+ ang PI) ccw )
	 P4 (polar P3 (+ ang (dtr 270)) iw)
	 P5 (polar P4 (+ ang (dtr 270)) bw)
	 P6 (polar P5  ang  ccw)
  )
   (setvar "PICKBOX" 0)
   (SBREAK  PT (polar PT ang wd))
   (SBREAK  P1 (polar P1 ang (+ wd sw sw)))
   (SBREAK  P3 (polar P3 ang (+ wd ccw ccw)))
   (SBREAK  P4 (polar P4 ang (+ wd ccw ccw)))
   (SBREAK  P5 (polar P5 ang (+ wd ccw ccw)))
   (setvar "PICKBOX" 4)
  (maklyr "concrete") (maklyr "brick") (maklyr "stone")
  (maklyr "a-out-windows") (maklyr "A-WINDOWS")
  (command "PLINE" P1 PP2 P2 P6 P5 P3 ""
	   "CHANGE" (entlast) "" "P" "LA" "concrete" "C" "BYLAYER" ""
	   "MIRROR" (entlast) "" (polar PT  ang (/ wd 2.0)) 
				 (polar P6  ang (/ wd 2.0)) ""
	   "LINE" PT P2 ""
	   "CHANGE" (entlast) "" "P" "LA" "stone" "C" "BYLAYER" ""
	   "LINE" (polar PT ang wd) (polar P2 ang wd ) ""
	   "CHANGE" (entlast) "" "P" "LA" "stone" "C" "BYLAYER" ""
	   "LAYER" "S" "a-out-windows" ""
	   "LINE" PT (polar PT ang wd ) ""
	   "LINE" P6 (polar P6 ang wd ) ""
  )
  (setq P6 (polar P6 (+ (dtr 90) ang) 0.02) 
	P2 (polar P6 (+ (dtr 90) ang) 0.10) 
  )
  (command "LAYER" "S" "a-a-windows" ""
	   "LINE" P2 (polar P2  ang wd)  ""
	   "LINE" P6 (polar P6  ang wd)  ""
	   "LINE" (polar P2  ang 0.10) (polar P6  ang 0.10) ""
	   "LINE" (polar P2  ang (- wd 0.10)) 
		  (polar P6  ang (- wd 0.10)) ""
	   "LINE" (polar P2  ang (- (/ wd 2.0) 0.05)) 
		  (polar P6  ang (- (/ wd 2.0) 0.05)) ""
	   "LINE" (polar P2  ang (+ (/ wd 2.0) 0.05)) 
		  (polar P6  ang (+ (/ wd 2.0) 0.05)) ""
  )
  (setq P2 (polar P2 Ang 0.10) 
	P2 (polar P2 (- Ang (dtr 90)) 0.05) 
	P3 (polar P2 (+ Ang (dtr 90)) 0.025) 
	P6 (polar P2 Ang (- wd 0.20))
	P5 (polar P6 (- Ang (dtr 90)) 0.025) 
  )
  (command "LINE" P2 p6 "" 
	   "LINE" p3 (polar P3  ang (/ (- wd 0.3) 2.0))  ""
	   "LINE" p5 (polar P5  (- ang PI) (/ (- wd 0.3) 2.0))  ""        
	   "LAYER" "S" (eval LAY) ""
  )
  (FINISH)
)
;=================================================================
(defun C:CLEAR(/ i item)
   (setq i 0)
   (while (not (equal (setq item (nth i atomlist)) 'C:CLEAR))
      (if (= (type (eval item)) 'FILE) (close (eval item)))
      (setq i (1+ i))
   )
   (setq atomlist (member 'c:clear atomlist))
   'done
)

(defun sbreak (bpt spt)
 (if (ssget bpt) (command "break" "nea" bpt spt))
)

; **********************************************************************

(defun C:MT ( / edg objs sid ssl cou)
   (start)
   (prompt "\nSelect edge(s) for cutting...  ")
   (setq edg (ssget))                    
   (prompt "\n\nSelect object(s) to cut......  ")
   (setq objs (ssget)                   
	 sid (getpoint "\nSelect side to cut:  ")
	 ssl (sslength objs)                
	 cou -1                               
   )                                             
   (command "TRIM" edg "")                      
   (repeat ssl
	  (setq cou (1+ cou))
	  (command (list (ssname objs cou) sid ))

   )
   (command "")
   (finish)
);defun
; **********************************************************************
(defun C:ME (/ bou ent pt len cou)
 (start)
 (setq cou -1)
 (prompt "Select boundary edge(s)...")
 (setq bou (ssget))(terpri)(terpri)
 (prompt "\n\nSelect object(s) to extend.....")
 (setq ent (ssget))(terpri)(terpri)
 (setq len (sslength ent))                        
 (setq pt (getpoint "\nPick side to extend: "))  
 (command "EXTEND" bou "")                        
 (repeat len
  (setq cou (1+ cou))
  (command (list (ssname ent cou) pt ))
  )
  (command "")
 (finish)
 
);defun
; **********************************************************************
			    AUTO DIMENSION
; **********************************************************************

(defun smax (xlis lt / xco big)
	   (setq  xco 0 big nil)
    (while (/= xco (length xlis))
	(setq corr (rtos (nth xco xlis) 2 4)
	      corr (atof corr)
	)
	(if (and (< big corr)  (< corr lt ))

	    (setq big corr)
	)
	
	(setq xco (1+ xco))
    )
	big
)  

;----------------------------------------------------
(defun addvrt (pnt) 
     (if (= opt "Vertical")   
	   (progn
	       (setq lis (append  lis (list (cadr pnt ))))
	       (if (> (cadr pnt) bigest) (setq bigest (cadr pnt)) )
	   )
	   (progn
	       (setq lis (append  lis (list (car pnt ))))
	       (if (> (car pnt) bigest) (setq bigest (car pnt)) )
	   )
	 
     )
)

;----------------------------------------------------
(defun gtvrts ()

 (setq grb (ssget) cou 0 )

 (while (< cou (sslength grb)) (progn
       
     (setq ent (ssname grb cou )
	     nam (cdr (assoc 0  (ENTGET ENT)))
	     lyr (CDR (assoc 8  (ENTGET ENT)))
	       t (CDR (ASSOC 1 (ENTGET ENT)))
	       X (CDR (ASSOC 10 (ENTGET ENT)))
	       Y (CDR (ASSOC 11 (ENTGET ENT)))
	       a (CDR (ASSOC 50 (ENTGET ENT)))
	       b (CDR (ASSOC 51 (ENTGET ENT)))
	       R (CDR (ASSOC 40 (ENTGET ENT)))  ;Raduis or text Hight
     )  
     (if (NOT (or (= lyr "EX-DIM") (= lyr "IN-DIM"))) (PROGN
     (cond 
      ((= nam "LINE") (progn (addvrt x) (addvrt y) ))
      ((= nam "ARC") (progn (addvrt (polar x a r)) (addvrt (polar x b r))))
      ((= nam "CIRCLE") (progn 
		 (addvrt (polar x zawdm r)) (addvrt (polar x (+ PI zawdm) r))
      ))
      ((= nam "POLYLINE") (progn 
	       (setq ilis (gtver ent) l (length ilis) co 0 )
	       (while (< co l) (addvrt (nth co ilis)) (setq co (1+ co)))
      ))
     ); cond

  )) ;progn IF
   (setq cou (1+ cou))           
  ));WHILE progn

)

;----------------------------------------------------
(defun gtpts ()
	   (command "OSNAP" "INT")
	   (while (setq pt (getpoint "\nPick Point : INT of "))
		  (if pt (addvrt pt))
	   ) 
	   (command "OSNAP" "NONE")
)

;----------------------------------------------------
(defun adim (/ lis lis2 pt coun corbig last bigest co)

   (setq lis '() bigest nil )
   (initget "Points Vertices")
   (while (setq ptorvr (getkword "\nPoints / Vertices <ENTER to draw DIM>: "))
	  (cond ((= ptorvr "Points") (gtpts))
		((= ptorvr "Vertices") (gtvrts))
	  )
	  (initget "Points Vertices")
   )      
	  
   (setq   pt (getpoint "\n Position of DIM ")
	 coun 1
	 corbig (rtos bigest 2 4)
	 corbig (atof corbig)
	 lis2 (list corbig)
	 last corbig
   )
   (if (= opt "Horizontal") (setq pt (cadr pt)) (setq pt (car pt)) )
    
   (while (/= coun (length lis))
	  (setq lis2 (append lis2 (list (setq last (smax lis last))))
		coun (+ coun 1)
	  )
   )
  (IF (cadr lis2) (progn 
    (if (= opt "Vertical") 
	(progn   ;v
	   ( command "DIM" "VER" (list (- pt (getvar "DIMEXE")) (car lis2))
			       (list (- pt (getvar "DIMEXE")) (nth 1 lis2))
			       (list pt (cadr lis2 )) 
			       "" "exit"
	   )
	   (setq co 2)
	   (while (/= co (length lis2))
	    (if  (nth co lis2) 
	      ( command "DIM" "cont" (list (- pt (getvar "DIMEXE"))
					   (nth co lis2)) "" "exit"
	      )
	    )
	      (setq co (1+ co))
	   );while       
	);progn v
	(progn ;h
	  ( command "DIM" "HOR" (list (car lis2) (- pt (getvar "DIMEXE")))
			       (list (nth 1 lis2) (- pt (getvar "DIMEXE")))
			       (list (car lis2 ) pt) 
			       "" "exit"
	  )
	   (setq co 2)
	   (while (/= co (length lis2))
	      (if  (nth co lis2)
		   ( command "DIM" "cont" (list (nth co lis2) 
						(- pt (getvar "DIMEXE")))
				   "" "exit"
	      )
	   )
	      (setq co (1+ co))
	   )       
	
	);progn h
      
      ) ; if
  ));progn IF                
)

;--------------------------------------------------------------------------
(defun c:adim (/ opt)
    (start)
    (initget "Horizontal Vertical")
    (setq opt 
	 (getkword "\n>>  Horizontal / Vertical :" )
    )
    (cond 
	((eq opt "Horizontal")  (progn (setq zawdm 0) (adim)))
	((eq opt "Vertical")  (progn (setq zawdm (dtr 90)) (adim)))
    )
    (finish)
)          

;--------------------------------------------------------------------------
(defun zadvar ()
(start)        
(command "dim"   
	 "dimsho"  "on"    "dimsoxd" "off"    "dimtad"  "on"
	 "dimlfac" "1"     "dimtix"  "on"     "dimtofl" "on"
	 "dimtoh"  "off"   "dimtol"  "off"    "dimtih" "off"
	 "dimzin" "8"      "dimblk" "dot"     "dimasz" "0.1"
	 "dimaso" "on"     "dimtsz" "0"       "dimdli" "0"
	 "dimexo" "0"      "dimexe" "0.2"     "dimse1" "off"
	 "dimse2" "off"  "exit"
)
(finish)
)
; **********************************************************************

(defun C:FWIN (/ PT P6 WD ORI ANG)
  (start)
  (setq LAY (getvar "CLAYER")    
	 PT (getpoint "\nInsertion Point of Window :")
  )
   (if (null PT) (setq PT (frompt)))
  (setq WD (getdist PT "\nWindow Width <0.90>: "))
   (if (null WD) (setq WD 0.90))
   (prompt "\nOther Side Of Wall :") (command "LINE" pt "PER" pause "")
  
  (setq P6 (cdr (assoc 11 (entget (entlast)))) 
       ANG (getpoint  "\nDirection of Window:")
       ORI (angle PT P6)
       ANG (angle (inters PT P6 ANG (polar ANG (+ ORI (DTR 90)) WD) nil) ANG)
  )
   (SBREAK (polar PT  ANG WD) PT) (SBREAK (polar P6 ANG WD) P6)
   (maklyr "a-windows") (maklyr "a-out-windows")

   (COMMAND "LINE" (polar PT ang wd) (polar P6 ang wd) ""
	    "LAYER" "S" "a-out-windows" ""
	    "LINE" PT (polar PT ang wd) ""
	    "LINE" P6 (polar P6 ang wd) ""
   )
  (setq P6 (polar P6 (+ PI ORI) (/ (- (distance PT P6) 0.1) 2.0)) 
	P2 (polar P6 (+ PI ORI) 0.10) 
  )
  (command "LAYER" "S" "a-windows" ""
	   "LINE" P2 (polar P2  ang wd)  ""
	   "LINE" P6 (polar P6  ang wd)  ""
	   "LINE" (polar P2  ang 0.10) (polar P6  ang 0.10) ""
	   "LINE" (polar P2  ang (- wd 0.10)) 
		  (polar P6  ang (- wd 0.10)) ""
	   "LINE" (polar P2  ang (- (/ wd 2.0) 0.05)) 
		  (polar P6  ang (- (/ wd 2.0) 0.05)) ""
	   "LINE" (polar P2  ang (+ (/ wd 2.0) 0.05)) 
		  (polar P6  ang (+ (/ wd 2.0) 0.05)) ""
  )
  (setq P2 (polar P2 Ang 0.10) 
	P2 (polar P2 ORI 0.05) 
	P3 (polar P2 (+ ORI PI) 0.025) 
	P6 (polar P2 Ang (- wd 0.20))
	P5 (polar P6 ORI 0.025) 
  )
  (command "LINE" P2 p6 "" 
	   "LINE" p3 (polar P3  ang (/ (- wd 0.3) 2.0))  ""
	   "LINE" p5 (polar P5  (- ang PI) (/ (- wd 0.3) 2.0))  ""        
	   "LAYER" "S" (eval LAY) ""
  )
  (FINISH)
)

; **********************************************************************
(defun ssx_fe (/ x data fltr ent)
  (setq ent (car (entsel "\nSelect object/<None>: ")))
  (if ent
    (progn
      (setq data (entget ent))
      (foreach x '(0 2 6 7 8 39 62 66 210) ; do not include 38
	(if (assoc x data)
	  (setq fltr 
	    (cons (assoc x data) fltr)
	  )
	)
      )    
      (reverse fltr)
    )
  ) 
)

(defun ssx_re (element alist)
  (append
    (reverse (cdr (member element (reverse alist))))
    (cdr (member element alist))   
  )
)

(defun ssx_er (s)                     ; If an error (such as CTRL-C) occurs
				      ; while this command is active...
  (if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
  )
  (if olderr (setq *error* olderr))   ; Restore old *error* handler
  (princ)
)

(defun ssx (/ olderr)
  (gc)                                ; close any sel-sets            
  (setq olderr *error* 
	*error* ssx_er 
  )
  (setq fltr (ssx_fe)) 
  (ssx_gf fltr) 
)

(defun ssx_gf (f1 / t1 t2 t3 f1 f2)
  (while 
    (progn
      (cond (f1 (prompt "\nFilter: ") (prin1 f1)))
      (initget 
	"Block Entity LAyer LType Pick Style Thickness")
      (setq t1 (getkword (strcat
	"\n>>Block name/Entity/"
	"LAyer/LType/Pick/Style/Thickness: "))) 
    )
    (setq t2
      (cond
	((eq t1 "Block")      2)   ((eq t1 "LAyer")      8)   
	((eq t1 "LType")      6)   ((eq t1 "Style")      7)   
	((eq t1 "Thickness") 39)   (T t1)
      )
    )
    (setq t3
      (cond
	((= t2  2)  (getstring "\n>>Block name to add/<RETURN to remove>: "))
	((= t2  8) (getstring "\n>>Layer name to add/<RETURN to remove>: "))
	((= t2  6) (getstring "\n>>Linetype name to add/<RETURN to remove>: "))
	((= t2  7) 
	  (getstring "\n>>Text style name to add/<RETURN to remove>: ")
	)
	((= t2 39)  (getreal   "\n>>Thickness to add/<RETURN to remove>: "))
	(T          nil)
      )
    )
    (cond
      ((= t2 "Pick") (setq f1 (ssx_fe) t2 nil)) ; get entity
      ((and f1 (assoc t2 f1))         ; already in the list
	(if (and t3 (/= t3 ""))
	  ;; Replace with a new value...             
	  (setq f1 (subst (cons t2 t3) (assoc t2 f1) f1)) 
	  ;; Remove it from filter list...
	  (setq f1 (ssx_re (assoc t2 f1) f1)) 
	)  
      )
      ((and t3 (/= t3 ""))
	(setq f1 (cons (cons t2 t3) f1))
      )
      (T nil)
    )
  )
  (if f1 (setq f2 (ssget "x" f1)))
  (setq *error* olderr)
  (if (and f1 f2) 
    (progn
      (princ (strcat "\n" (itoa (sslength f2)) " found. "))
      (setq ss f2) 
    )
    (progn (princ "\n0 found.") (prin1))
  )
)

(defun c:ssx () (ssx)(princ))

; **********************************************************************
(defun mid (/ mpt mp2) (setq pt (getpoint " First Point : ")
			     p2 (getpoint " Second Point : ")
	      ) 
	      (polar pt (angle pt p2) (/ (distance pt p2) 2.0))
)
; **********************************************************************
(defun C:coord (/ RPT ORPTS EPT YELV XELV TPT)       
      (START)
       (setq orpts (if (null CORPT) " " corpt ))
       (while (null RPT) 
	  (princ "\nRiference Point ") (princ orpts) 
	  (setq RPT (getpoint " : "))
	  (if (null RPT) (setq RPT CORPT))
       )
  (setq CORPT RPT
       EPT  (getpoint " Pick Point :")
       YELV (- (CADR EPT) (CADR RPT))
       YELV (if (and sngflg (> YELV 0)) (strcat "+" (rtos YELV)) (rtos YELV))
       XELV (- (CAR EPT) (CAR RPT))
       XELV (if (and sngflg (> XELV 0)) (strcat "+" (rtos XELV)) (rtos XELV))
       TPT (if ltfflg (polar EPT (* PI 0.7) 0.32) 
		      (polar EPT (* PI 0.3) 0.32))
  )
     (if (NOT ltfflg)
      (command "TEXT"  TPT  "0" (STRCAT "(" XELV "," YELV ")"))
      (command "TEXT"  "j" "r"  TPT "0" (STRCAT "(" XELV "," YELV ")"))
     )
(FINISH)
)

; **********************************************************************
(defun KLER ()

(setq ipt (getpoint "\n Insertion Point : ")
      pt2 (getcorner ipt "\n Other Corner : ")
      B (- (car pt2) (car ipt))
      H (- (cadr pt2) (cadr ipt))
)
(rect IPT 0 H B 1)     
    
    (setq edg (entlast)
	 objs (ssget "C" ipt pt2)                   
	 objs (ssdel edg objs)
	 sid  (polar ipt (angle ipt pt2) (/ (distance ipt pt2) 2.0))
	 ssl (sslength objs)                
	 cou -1                               
   )                                             
   (command "TRIM" edg "")                      
   (repeat ssl
	  (setq cou (1+ cou))
	  (command (list (ssname objs cou) sid ))

   )
   (command "" )
   (setq objs (ssget "W" ipt pt2)
	 objs (ssdel edg objs)
   )
   (command "ERASE" OBJS "" )
)
;--------------------------------------------------------------------------
(DEFUN C:ELWIN ()
(START)
(KLER)
(SETQ TR 0.06 
      IPT (POLAR IPT 0 (/ (- B TR) 2.0) )
      B (/ (- B (* TR 3.0)) 2.0)  
)
(command "LINE" IPT (POLAR IPT (DTR 90) H) "" )
(SETQ IPT (POLAR IPT (DTR 90) TR)
      H (- H (* TR 2.0)) 
)
(rect IPT 0 H (* -1 B) 0)
(rect (POLAR IPT 0 TR) 0  H B 1)
(FINISH)
)

; **********************************************************************
(defun C:CHT (/ p l n e os as ns st s nsl osl sl si chf chm )
   (start)
   (setq chm     0)
   (setq p (ssget))
   (if p (progn    
      (while (= 0 (setq osl (strlen (setq os (getstring t "\nOld string: ")))))
	    (princ "No Text !!...INVALID")
      )
      (setq nsl (strlen (setq ns (getstring t "\nNew string: "))))
      (setq l 0 n (sslength p))
      (while (< l n)
	 (if (= "TEXT"
		(cdr (assoc 0 (setq e (entget (ssname p l))))))
	    (progn
	       (setq chf nil si 1)
	       (setq s (cdr (setq as (assoc 1 e))))
	       (while (= osl (setq sl (strlen
			     (setq st (substr s si osl)))))
		  (if (= st os)
		      (progn
			(setq s (strcat (substr s 1 (1- si)) ns
					(substr s (+ si osl))))
			(setq chf t) ; Found old string
			(setq si (+ si nsl))
		      )
		      (setq si (1+ si))
		  )
	       )
	       (if chf (progn
		  (setq e (subst (cons 1 s) as e))
		  (entmod e) 
		  (setq chm (1+ chm))
	       ))
	    )
	 )
	 (setq l (1+ l))
      )
   ))
   (princ "Changed ")
   (princ chm)
   (princ " text lines.")
(finish)
)

;----------------------------------------------------------------------------
(defun C:DELLAYER (/ lyr s)
   (start)
   (setq lyr (strcase (getstring "\nLayer to delete: ")))
   (setq s (ssget "X" (list (cons 8 lyr))))
   (if s
      (command "ERASE" s "")
      (princ "Layer empty or not a valid layer name.")
   )
(finish)
)
;----------------------------------------------------------------------------
(defun C:MO ( / ent spt dist)
  (setq #mdist (udist 1 "" "Offset distance" #mdist nil) 
	ent (entsel "\nSelect object to offset: ")       
	spt (upoint 1 "" "Select side" nil (cadr ent))   
	#mnum (uint 5 "" "How many times" #mnum)         
  )
  (setq dist #mdist)
  (repeat #mnum
     (command "offset" dist ent spt "")
     (setq dist (+ dist #mdist))
  )
  (finish)
)
;----------------------------------------
(defun udist (bit kwd msg def bpt / inp)
  (if def
    (setq msg (strcat "\n" msg " <" (rtos def) ">: ")
	  bit (* 2 (fix (/ bit 2)))
    )
    (setq msg (strcat "\n" msg ": "))
  )
  (initget bit kwd)
  (setq inp
    (if bpt
      (getdist msg bpt) 
      (getdist msg)
  ) )
  (if inp inp def)
)
;----------------------------------------
(defun uint (bit kwd msg def / inp)
  (if def
    (setq msg (strcat "\n" msg " <" (itoa def) ">: ")
	 bit (* 2 (fix (/ bit 2)))
    )
    (setq msg (strcat "\n" msg ": "))
  )
  (initget bit kwd)
  (setq inp (getint msg))
  (if inp inp def)
)
;---------------------------------------
(defun upoint (bit kwd msg def bpt / inp)
  (if def
    (setq pts (strcat
		(rtos (car def)) "," (rtos (cadr def))
		(if
		  (and (caddr def) (= 0 (getvar "FLATLAND")))
		  (strcat "," (rtos (caddr def)))
		  ""
	      ) )
	  msg (strcat "\n" msg " <" pts ">: ")
	  bit (* 2 (fix (/ bit 2)))
    )
    (setq msg (strcat "\n" msg ": "))
  )
  (initget bit kwd)
  (setq inp
    (if bpt
      (getpoint msg bpt)
      (getpoint msg)
  ) )
  (if inp inp def)
)
;----------------------------------------------------------------------------


(defun c:ziad () (alert "
1=ddlmodes
2=appload
3=object snap
4=ddptype
5=ddmodify")
)

;;*************************************************************************
(defun c:stno (/ pt1 pt2 pt3 pt4 pt5 pt6 c c1 c2 l l1 l2)
(setq oldly (getvar "clayer"))
(command "layer" "New" "E-STONE" "C" 4 "E-STONE" "S" "E-STONE" "")
(setq pt1     (getpoint "\nPick start point: "))
(setq c       (getint "\nEnter first No.: "))
(setq l       (getreal "\nENTER last No.: "))
(setq l0 (+ (- l c) 1))
(setq c1      (getreal "\nEnter Course Hight: "))
(setq c2 (* c1 2))
(setq c3 (/ (- c1 0.15) 2))
(setq pt2 (list (+ (car pt1) 0.5)(cadr pt1)))
(setq pt3 (list (car pt1)(+ (cadr pt1) c1)))
** (setq pt4 (list (+ (car pt1) 0.5)(+ (cadr pt2) c1)))
(setq pt5 (list (+ (car pt1) 0.5)(+ (cadr pt1) c2)))
(setq pt6 (list (+ (car pt1) 0.15)(+ (cadr pt1) c3)))
(setq l2 (/ l0 2))    
(setq l2 (fix l2))
(setq l1 (+ l0 1))
(setq l1 (fix l1))
(setq l3 (/ l1 2))
(setq l3 (fix l3))
(if (not (= l2 l3))(setq l4 l3)(setq l4 l2))
(command "line" pt1 pt2 ""
	 "array" "L" "" "R" l1 "" c1 
	 "line" pt1 pt3 ""
	 "array" "L" "" "R" l4 "" c2 
	 "line" pt4 pt5 ""
	 "array" "L" "" "R" l2 "" c2) 
(command "text" pt6 "0.15" "" c)
  (while (< c l)
    (setq c (+ c 1))
    (setq pt6 (list (car pt6)(+ (cadr pt6) c1)))
    (command "text" pt6 "0.15" "0.0" c)
  )
  (command "layer" "S" oldly "")
)
(defun C:WINDOW()
(setq oldlayer (getvar "clayer"))
(setq PT1 (getpoint "\nOutside left corner of window: "))
(if (= PT1 nil) (setq PT1 (NOSTART)))
(setq X (getreal "\nEnter window length: "))
(if (= X nil) (setq X (getdist PT1)))
(command "OSNAP" "PERPEND")
(setq PT2 (getpoint  PT1 "\nEnter opposite wall: "))
(command "OSNAP" "NONE")
(setq PT4 (polar PT1 (abs (- (abs (- (angle PT1 PT2) PI)) (/ (* 90 PI) 180))) X)
      PT3 (polar PT2 (abs (- (abs (- (angle PT1 PT2) PI)) (/ (* 90 PI) 180))) X)
      PT5 (polar PT1 (abs (- (abs (- (angle PT1 PT2) PI)) (/ (* 90 PI) 180))) 0.1)
      PT6 (polar PT1 (abs (- (abs (- (angle PT1 PT2) PI)) (/ (* 90 PI) 180))) (- X 0.1)))
(setq PT31 (polar PT3 (abs (- (abs (- (angle PT1 PT2) PI)) (/ (* 90 PI) 180 ))) 0.1))
(setq PT21 (polar PT2 (abs (+ (abs (- (angle PT1 PT2) PI)) (/ (* 90 PI) 180))) 0.1))
(setq d1 (distance PT1 PT2))
(setq d11 (polar PT4 (abs (+ (abs (- (angle PT1 PT2) PI)) (/ (* 90 PI) 180))) D1))
(command "BREAK" PT21 PT31)
(command "BREAK" PT1 PT4)
(command "layer" "n" "a-windows" "s" "a-windows" "C" "2" "a-windows" "")
(command "line" PT31 PT3 "")
(command "LINE" PT21 PT2 "")
(command "LINE" PT1 PT2 PT3 PT4 "")
(command "LINE" PT1 PT5 "@0.1<90" "@-0.1,0" "")
(command "LINE" PT4 PT6 "@0.1<90" "@0.1,0" "")
(command "LINE" PT5 PT6 "")
(command "OFFSET" "0.05" PT5 PT2 "")
(command "OFFSET" "0.05" PT5 PT2 "")
(command "layer" "s" oldlayer "")
(command "line" pt31 d1 "")
(command "line" pt21 d11 "")
)
;-------------------------------------------------------------------------------------------------------
(princ)
(defun m  (/ x p v)
(setq x (entsel "\nSelect object whose property is to be matched:"))
(initget 1 "Layer Elev Thickness")
(setq p (getkword"\nLayer/Elev/Thickness of object : "))
   (cond ((= p "Layer") (setq v 8 ))
	 ((= p "Elev") (setq v 38 ))
	 ((= p "Thickness") (setq v 39))
   )
   (command "prop" p)
   (command (cdr (assoc v (entget (car x)))) "")
   )

(defun C:s ( / x )
   (setq x (entsel "\nSelect object whose layer is to be matched:"))
   (command "layer" "S" (cdr (assoc 8 (entget (car x)))) "")
)
(defun c:on ()
 (setq cmd (getvar"cmdecho"))
 (setvar"cmdecho" 0)
 (setq lays "")
 (while (setq ent (entsel "\nSelect entity on layer(s) to keep on: "))
   (setq lays (strcat lays "," (cdr(assoc 8 (entget (car ent))))))
 )
 (setq lays (substr lays 2))
 (setq cur (entsel "\nSelect entity on new current layer: "))
 (setq cur (cdr(assoc 8 (cdr(entget(car cur))))))
 (command"layer" "s" cur "of" "*" "y" "on" lays "")
 (setvar "cmdecho" cmd)
)
(defun C:o ( / x )
   (setq x (entsel "\nSelect object whose layer is to be on only:"))
   (command "layer" "S" (cdr (assoc 8 (entget (car x)))) "")
   (command "layer" "of" "*" "" "")
)
(defun C:off ( / lay lays )
(setq lays "" )
(while (setq lay (entsel "\nselect layer :"))
(setq lay (cdr(assoc 8 (entget (car lay )))))
(print lay)
(setq yn (getstring "\nOff<Y> ? "))
(if(/= (strcase yn) "N")
(setq lays (strcat lays "," lay ))
)
)
(setq lays (substr lays 2 1000 ))
(command "layer" "off" lays "" )
)
(DEFUN C:OA ()
(COMMAND "LAYER" "ON" "*" "" )
)
(defun C:frz ( / lay lays )
(setq lays "" )
(while (setq lay (entsel "\nselect layer :"))
(setq lay (cdr(assoc 8 (entget (car lay )))))
(print lay)
(setq yn (getstring "\nOff<Y> ? "))
(if(/= (strcase yn) "N")
(setq lays (strcat lays "," lay ))
)
)
(setq lays (substr lays 2 1000 ))
(command "layer" "F" lays "" )
)
(defun c:fz ()
(command "layer" "f" "??arttext,??arnote,??artag,??ttext" "" "")
)
; ********************************************************************
;                           DELLAYER.LSP

;  This program deletes all entities on a specified layer.

; ********************************************************************

(defun dellerr (s)                    ; If an error (such as CTRL-C) occurs
				      ; while this command is active...
   (if (/= s "Function cancelled")
       (princ (strcat "\nError: " s))
   )
   (setq S nil)                       ; Free selection-set if any
   (setvar "CMDECHO" ocmd)            ; Restore saved mode
   (setq *error* olderr)              ; Restore old *error* handler
   (princ)
)

(defun C:DELLAYER (/ olderr ocmd L S)

   (setq olderr  *error*
	 *error* dellerr)
   (setq ocmd (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)
   (setq L (strcase (getstring "\nLayer to delete: ")))
   (setq S (ssget "X" (list (cons 8 L))))  ; Get all entities on layer
   (if S
      (command "ERASE" S "")          ; Delete 'em!
      (princ "Layer empty or not a valid layer name.")
   )
   (setq S nil)                       ; Free selection-set
   (setvar "CMDECHO" ocmd)            ; Restore saved mode
   (setq *error* olderr)              ; Restore old *error* handler
   (princ)
)

;-----------------------------------------------------------------------------+
;                               LEXPLODE.LSP                                  |
;                                                                             |
;    Larry Knott                Version 1.0                  5/25/88          |
;                                                                             |
;    Explode a BLOCK, POLYLINE, or DIMENSION and copy the entities            |
;    that replace it to the layer that the original entity was on.            |
;                                                                             |
;-----------------------------------------------------------------------------+

;-------------------------- INTERNAL ERROR HANDLER ---------------------------|

(defun lexerr (s)                     ; If an error (such as CTRL-C) occurs
				      ; while this command is active...
  (if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
  )
  (setvar "highlight" ohl)            ; restore old highlight value
  (setvar "cmdecho" oce)              ; restore old cmdecho value
  (setq *error* olderr)               ; restore old *error* handler
  (princ)
)
;------------------------------ COMMON FUNCTION ------------------------------|

(defun getval (n e) (cdr (assoc n e)))

;--------------------------- GET ENTITY TO EXPLODE ---------------------------|

(defun getent (t1 / no_ent e0)
(setq no_ent T)
(while no_ent
  (if (setq e0 (entsel "\nSelect block reference, polyline, dimension, or mesh: "))
    (if (member (getval 0 (setq e1 (entget (car e0)))) t1)
      (if (equal (getval 0 e1) "INSERT")
	(if (and (equal (getval 41 e1) (getval 42 e1))
		 (equal (getval 42 e1) (getval 43 e1)))
	  (setq no_ent nil)
	  (princ "\nX, Y, and Z scale factors must be equal."))
	(setq no_ent nil))
      (princ "\nNot a block reference, polyline, or dimension."))
    (princ " No object found."))
))

;-------------------------------- MAIN PROGRAM -------------------------------|

(defun c:LX (/ oce ohl e0 en e1 s0)
(setq olderr *error*
      *error* lexerr)
(setq oce (getvar "cmdecho"))         ; save value of cmdecho
(setq ohl (getvar "highlight"))       ; save value of highlight
(setvar "cmdecho" 0)                  ; turn cmdecho off
(setvar "highlight" 0)                ; turn highlight off
(setq e0 (entlast))
(setq en (entnext e0))
(while (not (null en))                ; find the last entity
  (setq e0 en)
  (setq en (entnext e0))
)
(getent '("INSERT" "DIMENSION" "POLYLINE"))
(command "explode" (getval -1 e1))    ; explode the entity
(setq s0 (ssadd))
(while (entnext e0) (ssadd (setq e0 (entnext e0)) s0))
(command "chprop" s0 ""               ; change entities to the proper layer
	 "c"   "bylayer"              ; regardless of their extrusion direction
	 "lt"  "bylayer"
	 "la"  (getval 8 e1) "")
(princ (strcat "\nEntities exploded onto layer " (getval 8 e1) "."))
(setvar "highlight" ohl)              ; restore old highlight value
(setvar "cmdecho" oce)                ; restore old cmdecho value
(setq *error* olderr)                 ; restore old *error* handler
(prin1))

;------------------------------------ END ------------------------------------|

;************************ BOX.LSP *****************************

;  By Simon Jones   Autodesk Ltd, London    February 1987

; This macro implements the GETCORNER function to "drag" a
; polyline rectangle.

;**************************************************************

(DEFUN C:BOX (/ a b c d sblip scmde)
  (setq scmde (getvar "CMDECHO"))
  (setq sblip (getvar "BLIPMODE"))
  (setvar "CMDECHO" 0)
  (setq a (getpoint "\nFirst corner: "))
  (setq c (getcorner a "\nSecond corner: "))
  (setvar "BLIPMODE" 0)
  (setq b (list (car c) (cadr a)))
  (setq d (list (car a) (cadr c)))
  (command "PLINE" a b c d a "")
  (setvar "CMDECHO" scmde)
  (setvar "BLIPMODE" sblip)
  (princ)
)
; **********************************************************************
;                             CHGTEXT.LSP

;  This program will replace every occurrence of an "old string" with a
;  "new string".  You will be prompted to select the text you wish
;  to change.  Then you will be asked to enter the "old string" and
;  the "new string".  After the text has been changed, the total number
;  of changed lines is displayed.
; **********************************************************************

(defun chgterr (s)
   (if (/= s "Function cancelled")   ; If an error (such as CTRL-C) occurs
      (princ (strcat "\nError: " s)) ; while this command is active...
   )
   (setq p nil)                      ; Free selection set
   (setq *error* olderr)             ; Restore old *error* handler
   (princ)
)

(defun C:CHGTEXT (/ p l n e os as ns st s nsl osl sl si chf chm olderr)
   (setq olderr  *error*             ; Initialize variables
	 *error* chgterr
	 chm     0)
   (setq p (ssget))                  ; Select objects
   (if p (progn                      ; If any objects selected
      (while (= 0 (setq osl (strlen (setq os (getstring t "\nOld string: ")))))
	    (princ "Null input invalid")
      )
      (setq nsl (strlen (setq ns (getstring t "\nNew string: "))))
      (setq l 0 n (sslength p))
      (while (< l n)                 ; For each selected object...
	 (if (= "TEXT"               ; Look for TEXT entity type (group 0)
		(cdr (assoc 0 (setq e (entget (ssname p l))))))
	    (progn
	       (setq chf nil si 1)
	       (setq s (cdr (setq as (assoc 1 e))))
	       (while (= osl (setq sl (strlen
			     (setq st (substr s si osl)))))
		  (if (= st os)
		      (progn
			(setq s (strcat (substr s 1 (1- si)) ns
					(substr s (+ si osl))))
			(setq chf t) ; Found old string
			(setq si (+ si nsl))
		      )
		      (setq si (1+ si))
		  )
	       )
	       (if chf (progn        ; Substitute new string for old
		  (setq e (subst (cons 1 s) as e))
		  (entmod e)         ; Modify the TEXT entity
		  (setq chm (1+ chm))
	       ))
	    )
	 )
	 (setq l (1+ l))
      )
   ))
   (princ "Changed ")                ; Print total lines changed
   (princ chm)
   (princ " text lines.")
   (terpri)
   (setq *error* olderr)             ; Restore old *error* handler
   (princ)
)
(defun di (/ b )
(setq b (getpoint "\nPick reference point:"))
(getpoint b "\nEnter distance from reference point:")
)
;************************* READTEXT.LSP *****************************

;   Adapted from LTEXT.LSP
;       By Simon Jones  Autodesk Ltd, London  4 July 1986

;       Modified for use with AutoCAD 2.6     23 April 1987

;   Use LTEXT.LSP to insert ASCII text files into AutoCAD drawings.
;   The blocks of text can either Left, Centre, Middle or Right
;   Justified.

;   The file name must include an extension and may include a
;   directory prefix, as in /acad/sample.txt or \\acad\\sample.txt.

;   Apart from height (unless fixed) & rotational angle,
;   "Text options" include:
;       Define distance between lines.
;       Define opening line for reading.
;       Define number of lines to read.
;       Global under/overscoring of lines.
;       Global upper/lower case change.
;    &  Column definitions.

;********************************************************************

;--------:GLOBAL variables

; a$$    :Last angle (for default value)
; f$$    :Last file  (for default value)
; rtfile :Read File
; ang    :Rotation angle
; c      :Total line count
; cd     :Column distance
; d      :Distance between lines
; eof    :End of file flag
; l1     :First line to read
; h      :Text height
; j      :Text justification
; lc     :Column line count
; n      :Number of lines to read
; nl     :Number of lines per column
; opt    :Options list
; pt     :Text insertion point
; pt1    :First text insertion point
; rf     :File to read
; s      :Text string
; ts     :Text style list
; ul     :Upper/lower case flag

(defun MODES (a)
   (setq MLST '())
   (repeat (length a)
      (setq MLST (append MLST (list (list (car a) (getvar (car a))))))
      (setq a (cdr a)))
)

(defun MODER ()
   (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
   )
)


(defun *ERROR* (st)
  (moder)
  (if (= (type rtfile) 'FILE) (close rtfile))
  (setq rtfile nil)
  (terpri)
  (princ "\nerror: ")
  (princ st)
  (princ)
)


   ;Function for inserting text a recalculation of insertion
   ;point.

   (defun 1LTXT ()
      (if (member '1 opt) (setq s (strcat "%%u" s "%%u")))
      (if (member '2 opt) (setq s (strcat "%%o" s "%%o")))
      (if (member '4 opt) (setq s (strcase s)))
      (if (member '8 opt) (setq s (strcase s T)))
      (if (and (= lc (1+ nl)) (/= nl 0))
	  (progn
	   (setq lc 1)
	   (setq pt (polar pt1 ang cd))
	   (setq pt1 pt)
	  )
      )
      (cond ((and (= j "L") h)
	     (command "TEXT" pt h (rtd ang) s)
	    )
	    ((and (/= j "L") h)
	     (command "TEXT" j pt h (rtd ang) s)
	    )
	    ((and (= j "L") (null h))
	     (command "TEXT" pt (rtd ang) s)
	    )
	    ((and (/= j "L") (null h))
	     (command "TEXT" j pt (rtd ang) s)
	    )
      )
      (if (/= d "Auto")
	  (if (= (cdr (assoc 70 ts)) 4)
	      (setq pt (polar pt (+ (dtr 90) ang) d))
	      (setq pt (polar pt (+ (dtr 270) ang) d))
	  )
      )
      (setq c (1+ c))
      (if (= c n)
	  (setq eof T)
      )
   )

   ;Degrees to radians conversion
   (defun DTR (y)
    (* pi (/ y 180.0))
   )
   ;Radians to degrees conversion
   (defun RTD (Y)
    (* 180.0 (/ y pi))
   )

;********************** MAIN PROGRAM ***************************


(defun C:LTEXT (/ ang c cd d eof rtfile rf h
		  j l1 opt pt pt1 ts n nl lc s ul)

  ; a$$ holds default ANGLE
  ; f$$ holds default fILE

  (modes '("BLIPMODE" "CMDECHO" "HIGHLIGHT"))

  (while (null rtfile)
	 ;Prompt for file to be inserted
	 (if (null f$$)
	     (progn
	      (initget 1)
	      (prompt "\nFile to read (including extension): ")
	     )
	     (progn
	      (prompt "\nFile to read (including extension)/<")
	      (princ (strcat f$$ ">: "))
	     )
	 )
	 (setq rf (getstring))
	 (if (= rf "")
	     (setq rf f$$)
	 )
	 (setq rtfile (open rf "r"))
	 (if rtfile
	     (setq f$$ rf)
	     (prompt "\nFile not found. ")
	 )
  )

  ;Prompt for start point or justification
  (initget 1  "Centre Middle Right")
  (setq pt (getpoint
	     "\nStart point or Centre/Middle/Right: "
	   )
  )
  (if (/= (type pt) 'LIST)
      (setq j (substr pt 1 1))
      (setq j "L")
  )


  ;Prompt for an insertion point
  (if (/= (type pt) 'LIST)
      (progn
       (initget 1)
       (setq pt (getpoint (strcat "\n" pt " point: ")))
      )
  )
  (setq pt1 pt)  ; First insertion point

  ;Prompt for a text height
  (setq ts (tblsearch "STYLE" (getvar "TEXTSTYLE"))
	h nil
  )
  (if (= (cdr (assoc 40 ts)) 0.0)
      (progn
	(setq h (getdist pt (strcat "\nHeight <"
				    (rtos (getvar "TEXTSIZE"))
				    ">: "
			    )
		)
	)
	(if (null h) (setq h (getvar "TEXTSIZE")))
      )
  )

  ;Prompt for rotational angle of text
  (if (null a$$)
      (progn
       (if (= (cdr (assoc 70 ts)) 4)  ; Vertical style text
	   (progn
	    (setq a$$ 270)
	    (prompt "\nRotational angle <270>: ")
	   )
	   (progn
	    (setq a$$ 0)
	    (prompt "\nRotational angle <0>: ")
	   )
       )
      )
      (progn
       (prompt "\nRotational angle <")
       (princ (strcat (angtos a$$) ">: "))
      )
  )
  (setq ang (getangle pt))
  (if (null ang) (setq ang a$$))
  (setq a$$ ang)

  (setq d "Auto" l1 1 n "All" opt nil lc 0 nl 0 c 0)

  (initget "Yes No")
  (if (= "Yes"  (getkword "\nChange text options? <No>: "))
     (progn

       ;Prompt for distance between lines.
       (initget "Auto")
       (setq d (getdist pt "\nDistance between lines/<Auto>: "))
       (if (= d nil) (setq d "Auto"))

       ;Prompt for first line to read.
       (initget (+ 2 4))
       (setq l1 (getint "\nFirst line to read/<1>: "))
       (if (null l1) (setq l1 1))

       ;Prompt for number of following lines.
       (initget (+ 2 4) "All")
       (setq n (getint "\nNumber of lines to read/<All>: "))
       (if (= n "All") (setq n nil))

       (initget "Yes No")
       (if (= "Yes" (getkword "\nUnderscore each line <No>: "))
	   (setq opt (append opt '(1)))
       )
       (initget "Yes No")
       (if (= "Yes" (getkword "\nOverscore each line <No>: "))
	   (setq opt (append opt '(2)))
       )

       ; Option for global redefinition of text case.
       (initget "Upper Lower No")
       (prompt "\nChange text case? ")
       (setq ul (getkword "  Upper/Lower/<No>: "))
       (cond ((= ul "Upper") (setq opt (append opt '(4))))
	     ((= ul "Lower") (setq opt (append opt '(8))))
       )

       ; Option for setting up columns.
       (initget "Yes No")
       (if (= "Yes" (getkword "\nSet up columns? <No>: "))
	   (progn
	    (setq opt (append opt '(16)))
	    (initget (+ 1 2))
	    (setq cd (getdist pt "\nDistance between columns: "))
	    (initget (+ 1 2 4))
	    (setq nl (getint "\nNumber of lines per column: "))
	   )
       )

     )
  )
  (setvar "BLIPMODE" 0)
  (setvar "HIGHLIGHT" 0)
  (setvar "CMDECHO" 0)

  (setq eof nil)
  (setq s (repeat l1 (read-line rtfile)))

  (setq lc (1+ lc))
  (1ltxt)
  (while (null eof)
    (if (= d "Auto")
	(progn
	 (setq s (read-line rtfile))
	 (setq lc (1+ lc))
	 (if s
	   (progn
	    (if (= lc (1+ nl))
		(1ltxt)
		(progn
		 (if (member '1 opt) (setq s (strcat "%%u" s "%%u")))
		 (if (member '2 opt) (setq s (strcat "%%o" s "%%o")))
		 (if (member '4 opt) (setq s (strcase s)))
		 (if (member '8 opt) (setq s (strcase s T)))
		 (command "TEXT" "" s)
		 (setq c (1+ c))
		 (if (= c n) (setq eof T))
		)
	    )
	   )
	   (setq eof T)
	 )
	)
	(progn
	 (setq s (read-line rtfile))
	 (setq lc (1+ lc))
	 (if s (1ltxt) (setq eof T))
	)
    )
  )
  (close rtfile)
  (setq rtfile nil)
  (moder)
  (princ)
)
;************************** ROD-END.LSP ***********************************

;    By Simon Jones   Autodesk Ltd, London    March 1987

; This macro constructs a "broken shaft end" at the end of two
; specified lines. The "S" shaped curve starts at a point on the
; "nearest" line to the first pick-point. Both lines will be trimmed
; or extended to neatly intersect the "S" shape.

;******************* SYSTEM VARIABLE STORE *********************

; Store system variables

(defun MODES (a)
   (setq MLST '())
   (repeat (length a)
      (setq MLST (append MLST (list (list (car a) (getvar (car a))))))
      (setq a (cdr a)))
)

; Reset System variables

(defun MODER ()
   (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
   )
)

;****************** ANGLE CONVERTERS ***************************

(defun DTR (a)
   (* pi (/ a 180.0))
)

(defun RTD (a)
  (/ (* a 180.0) pi)
)

;********************** CON-LST ********************************

; Add new point to list of previous points

(defun CON-LST (a d)
   (append l (list (polar pt1 a d)))
)

;******************* LINE SELECTION ****************************

(defun PICK (prmpt / e)
   (setq e (entsel prmpt))
   (if (null e)
       (pick prmpt)
       (cond
	   ((/= (cdr (assoc 0 (entget (car e)))) "LINE")
	    (prompt "\nError: Entity is a ")
	    (princ (cdr (assoc 0 (entget (car e)))))
	    (pick prmpt)
	   )
	   (T
	    (eval 'e)
	   )
       )
   )
)

;********************** MAIN PROGRAM ****************************

(defun C:ROD-END (/ p1 p2 pt1 pt2 dist ang l)

   (modes '("BLIPMODE" "CMDECHO" "HIGHLIGHT" "ORTHOMODE"))
   (setvar "CMDECHO" 0)
   (setvar "HIGHLIGHT" 0)
   (setvar "ORTHOMODE" 0)

   (setq p1 (pick "\nSelect first line: "))
   (setq p2 (pick "\nSelect second line: "))
   (setvar "BLIPMODE" 0)
   (setq pt1 (osnap (cadr p1) "near"))
   (setq pt2 (inters
	       (cdr (assoc 10 (entget (car p2))))
	       (cdr (assoc 11 (entget (car p2))))
	       pt1
	       (polar pt1
		      (+ (angle
			   (cdr (assoc 10 (entget (car p1))))
			   (cdr (assoc 11 (entget (car p1))))
			 )
			 (dtr 90)
		      )
		      1
	       )
	       nil
	     )
   )
   (command "CHANGE" (car p1) "" pt1)
   (command "CHANGE" (car p2) "" pt2)
   (setq dist (distance pt1 pt2)
	 ang (angle pt1 pt2)
	 l (list pt1)
	 l (con-lst  (- ang (dtr 31))   (* dist 0.29))
	 l (con-lst  ang                (* dist 0.5))
	 l (con-lst  (+ ang (dtr 11.3)) (* dist 0.76))
	 l (append l (list pt2))
	 l (con-lst  (- ang (dtr 11.3)) (* dist 0.76))
	 l (con-lst  ang                (* dist 0.5))
   )

   (command "PLINE")
   (foreach n l (command n))
   (command "")

   (command "PEDIT" "L" "E" "T" (- (rtd ang) 90) "X" "F" "")
   (moder)
   (princ)
)
;-----------------------------------------------------------------------------+
;                                                                             |
;                                  SSX.LSP                                    |
;                                                                             |
;           Larry Knott          Version 2.0             7/18/88              |
;                                                                             |
;    "(SSX)" -  Easy SSGET filter routine.                                    |
;                                                                             |
;    Creates a selection set.  Either type "(SSX)" at the "Command:" prompt   |
;    to create a "previous" selection set or type "(SSX)" in response to any  |
;    "Select objects:" prompt.  You may use the functions "(A)" to add        |
;    entities and "(R)" to remove entities from a selection set during object |
;    selection.  More than one filter criteria can be used at a time.         |
;                                                                             |
;-----------------------------------------------------------------------------|

;--------------------------- INTERNAL ERROR HANDLER --------------------------|

(defun ssx-err (s)                    ; If an error (such as CTRL-C) occurs
				      ; while this command is active...
(if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
)
(setq *error* olderr)                 ; Restore old *error* handler
(princ)
)

;-------------------------- ADD AND REMOVE FUNCTIONS -------------------------|

(defun r() (command "r") (ssx))
(defun a() (command "a") (ssx))

;-------------------------------- MAIN PROGRAM -------------------------------|

(defun ssx (/ olderr t1 t2 t3 f1 f2)
(setq olderr *error* *error* ssx-err t1 T f2 'f1)
(while t1
  (initget "Block Color Entity LAyer LType Style Thickness")
  (setq t1 (getkword
     "\n>>Block name/Color/Entity/LAyer/LType/Style/Thickness: "))
  (setq t2
     (cond
	((eq t1 "Block")      2)   ((eq t1 "Color")     62)
	((eq t1 "Entity")     0)   ((eq t1 "LAyer")      8)
	((eq t1 "LType")      6)   ((eq t1 "Style")      7)
	((eq t1 "Thickness") 39)))
  (initget 1)
  (setq t3
     (cond
	((= t2  2)  (getstring "\n>>Block name: "))
	((= t2 62)  (initget "?")
		    (while
		       (or (eq (setq t3 (getint "\n>>Color number/<?>: ")) "?")
			   (null t3)
			   (> t3 256)
			   (< t3 0))
			   (textscr)
	  (princ "\n                                                     ")
	  (princ "\n                 Color number   |   Standard meaning ")
	  (princ "\n                ________________|____________________")
	  (princ "\n                                |                    ")
	  (princ "\n                       0        |      <BYBLOCK>     ")
	  (princ "\n                       1        |      Red           ")
	  (princ "\n                       2        |      Yellow        ")
	  (princ "\n                       3        |      Green         ")
	  (princ "\n                       4        |      Cyan          ")
	  (princ "\n                       5        |      Blue          ")
	  (princ "\n                       6        |      Magenta       ")
	  (princ "\n                       7        |      White         ")
	  (princ "\n                    8...255     |      -Varies-      ")
	  (princ "\n                      256       |      <BYLAYER>     ")
	  (princ "\n                                               \n\n\n")
			  (initget "?")) t3)
	((= t2  0)  (getstring "\n>>Entity type: "))
	((= t2  8)  (getstring "\n>>Layer name: "))
	((= t2  6)  (getstring "\n>>Linetype name: "))
	((= t2  7)  (getstring "\n>>Text style name: "))
	((= t2 39)  (getreal   "\n>>Thickness: "))
	(T          nil)))
  (if t3 (setq f1 (cons (cons t2 t3) f1)))
)
(setq f2 (ssget "x" (eval f2)))
(setq *error* olderr)
(if (and f1 f2) f2 (progn (princ "\n0 found.") (prin1)))
)
(defun c:REC ( / DX DY PT1t2 pt3 pt4 )
(setq DX (getreal  "Enter horizontal dimension of box:"))
(setq Dy (getreal  "Enter vertical dimension of box:"))
(setq pt1 (getpoint "Starting corner:"))
(setq pt2 (list (+ (car pt1) dx) (cadr pt1)))
(setq pt3 (list (car pt2) (+ (cadr pt1) dy)))
(setq pt4 (list (car pt1) (cadr pt3)))
(command "pline" pt1 pt2 pt3 pt4 "C")
)
(defun C:MT ( / edge linset side setsiz looper)
   (setvar "cmdecho" 0)
   (prompt "\nSelect edge for cutting:  ")
   (setq edge (ssget))                                ;get the edge
   (prompt "\nSelect lines to cut:  ")
   (setq linset (ssget)                               ;get the lines
	 side (getpoint "\Select side to cut:  ")     ;get the side
	 setsiz (sslength linset)                     ;size of line set
	 looper -1                                    ;initialize loop ptr
   )                                                  ;setq calculations
   (command "TRIM" edge "")                           ;trim it
   (repeat setsiz                                     ;for all lines
      (setq looper (1+ looper))                       ;increment loop ptr
      (command (list (ssname linset looper) side))
   )                                                  ;repeat
   (command "")

   (setvar "cmdecho" 1)
)                                                     ;defun
(defun C:Mextend(/ bou ent pt len cou)
 (setvar "CMDECHO" 0)
 (setq cou -1)                                  ;set counter
 (prompt "Select boundary edge(s)...")          ;prompt user
 (setq bou (ssget))(terpri)(terpri)             ;boundary edge
 (prompt "Select object(s) to extend...")
 (setq ent (ssget))(terpri)(terpri)             ;entities
 (setq len (sslength ent))                        ;number of entities
 (setq pt (getpoint "Pick side to extend..."))  ;reference point for extend
 (command "EXTEND" bou "")                        ;command
 (repeat len
  (setq cou (1+ cou))
  (command (list (ssname ent cou) pt))
 )
 (command "")
 (setvar "CMDECHO" 1)
 (prin1)
)                                               ;defun
;* UDIST User interface distance function
;* BIT (0 for none) and KWD key word ("" for none) are same as for INITGET.
;* MSG is the prompt string, to which a default real is added as <DEF> (nil
;* for none), and a : is added. BPT is base point (nil for none).
;*
(defun udist (bit kwd msg def bpt / inp)
  (if def                                              ;test for a default
    (setq msg (strcat "\n" msg " <" (rtos def) ">: ")  ;string'em  with default
	  bit (* 2 (fix (/ bit 2))) ;a default and no null bit code conflict so
    );setq                          ;this reduces bit by 1 if odd, to allow null
    (setq msg (strcat "\n" msg ": "))                  ;without default
  );if
  (initget bit kwd)
  (setq inp
    (if bpt                  ;check for a base point
      (getdist msg bpt)      ;and use it in the GET commands
      (getdist msg)
  ) );setq&if
  (if inp inp def)           ;compare the results, return appropriate value
);defun
;*
;* UINT User interface integer function
;* BIT (0 for none) and KWD key word ("" for none) are same as for INITGET.
;* MSG is the prompt string, to which a default integer is added as <DEF> (nil
;* for none), and a : is added.
;*
(defun uint (bit kwd msg def / inp)
  (if def                                                 ;test for a default
    (setq msg (strcat "\n" msg " <" (itoa def) ">: ")     ;string'em with default
	 bit (* 2 (fix (/ bit 2)))  ;a default and no null bit code conflict so
    )                               ;this reduces bit by 1 if odd, to allow null
    (setq msg (strcat "\n" msg ": "))                     ;without default
  );if
  (initget bit kwd)
  (setq inp (getint msg))       ;use the GETINT function
  (if inp inp def)              ;compare the results, return appropriate value
);defun
;*
;* UPOINT User interface point function
;* BIT (1 for no null, 0 for none) and KWD key word ("" for none) are same as
;* for INITGET. MSG is the prompt string, to which a default point variable is
;* added as <DEF> (nil for none), and a : is added. BPT is base point (nil for none).
;*
(defun upoint (bit kwd msg def bpt / inp)
  (if def                                              ;check for a default
    (setq pts (strcat
		(rtos (car def)) "," (rtos (cadr def)) ;formats X,Y 2D pt as string
		(if                 ;formats 3D ,Z if supplied and FLATLAND off
		  (and (caddr def) (= 0 (getvar "FLATLAND")))
		  (strcat "," (rtos (caddr def)))
		  ""
	      ) );if&strcat
	  msg (strcat "\n" msg " <" pts ">: ")         ;string them with default
	  bit (* 2 (fix (/ bit 2)))   ;a default and no null bit code conflict so
    )                                 ;this reduces bit by 1 if odd, to allow null
    (setq msg (strcat "\n" msg ": "))                         ;or without
  );if a default was specified
  (initget bit kwd)
  (setq inp
    (if bpt                           ;check for base point
      (getpoint msg bpt)              ;and use it
      (getpoint msg)                  ;but not if nil
  ) );setq&if
  (if inp inp def)                    ;evaluate results and return proper value
);defun
;*
;*    AutoLISP Files for Inside AutoLISP IL DISK - (c) 1987, 1988, 1989
;* New Riders Publishing. All Rights Reserved. Version 10.00 for PC-DOS/MS-DOS
;*       Developed by Rustin Gesner, Patrick Haessly and Joseph Smith
;*
(if (not (and udist upoint uint))                       ;test subroutines
  (prompt "\nRequires UDIST UPOINT UINT functions. Load aborted. ")
  (progn                                                ;else load OK

;* C:MOFFSET is a multiple offset command. It requires the user to show the
;* offset direction, select one object, tell how many times to offset it
;* and give a direction for the offset. It requires the User GET functions.
;*
(defun C:MOFFSET( / ent spt dist)
  (setq #mdist (udist 1 "" "Offset distance" #mdist nil) ;the distance to offset
	ent (entsel "\nSelect object to offset: ")       ;get the object
	spt (upoint 1 "" "Select side" nil (cadr ent))   ;get which side
	#mnum (uint 5 "" "How many times" #mnum)         ;# times
  );setq
  (setq dist #mdist)                    ;set a variable to the distance interval
  (repeat #mnum                         ;program loop
     (command "offset" dist ent spt "") ;run the offset command
     (setq dist (+ dist #mdist))        ;increment the offset distance
  );repeat
  (princ)        ;clean ending
);defun
(princ)          ;clean loading
;*
));test subroutines
(defun C:E-ATTR ( / selct indx entnm)
   (setvar "cmdecho" 0)
   (command "UNDO" "GROUP")
   (prompt "\nSelect Attribute Blocks to Edit:  ")    ;create selection set
   (setq selct (ssget)
	 indx 0                                       ;initialize pointer
	 entnm (ssname selct indx)                    ;get entity name
   )                                                  ;setq calculations

   (while entnm                                       ;until no entities
      (command "DDATTE" entnm)                        ;DDATTE command
      (setq indx (1+ indx)                            ;increment index
	    entnm (ssname selct indx)                 ;get next attribute
      )                                               ;setq calculations
   )                                                  ;while loop

  (command "UNDO" "END")
  (setvar "cmdecho" 1)
)                                                     ;defun

(defun C:TXTHGT( / curr th th1 ss1 sl cnt mark e)
(setvar "CMDECHO" 0)
(setvar "EXPERT" 3)
(setq mark 0)

(setq curr (getvar "TEXTSIZE"))                       ;select new and old text height
(prompt "\nNew text height <")
(prompt (rtos curr (getvar "LUNITS") (getvar "LUPREC")))
(prompt ">: ")
    (setq th (getdist))
    (setq th (if (null th) curr th))
    (setq th1 (getdist "\nOld text height <*>: "))
    (setq th1 (if (null th1) "*" th1))

(prompt "\nSelect objects or Return for all text:")   ;select objects or text
    (setq ss1 (ssget))
	(if (null ss1) (setq ss1 (ssget "X" '((0 . "TEXT")))))

    (setq sl (sslength ss1))                          ;determine loop count
    (setq cnt 0)

(prompt "\nUpdating text height...")                  ;change text
    (setvar "highlight" 0)
    (repeat sl
	  (if (= "TEXT" (cdr (assoc 0 (setq e (entget (ssname ss1 cnt))))))
	  (if (or (= (cdr (assoc 40 e)) th1) (= th1 "*"))
	      (progn
		  (entmod (subst (cons 40 th) (assoc 40 e) e))
		  (setq mark (+ mark 1))
	      )
	  )
    )
(setq cnt (+ cnt 1))
)
(prompt "\n")
    (prin1 mark) (prompt "  Lines of text updated")
(setvar "cmdecho" 1)
(setvar "highlight" 1)
(gc)(princ)
)


(defun C:TXTSTY (/ mark tst curr new old ss1 sl tbl e cnt)
(setvar "CMDECHO" 0)
(setvar "EXPERT" 3)
(setq mark 0)

(setq tst 0)                                          ;select and check for style
(setq curr (getvar "TEXTSTYLE"))
(while (= tst 0)
  (prompt "\nNew text style <")
  (princ curr)
  (prompt ">: ")
  (setq new (strcase (getstring)))
  (setq new (if (= "" new) curr new))
  (if (null (tblsearch "STYLE" new))
    (prompt "\nThat style does not exist")
    (setq tst 1)
  )
)                                                     ;end while statement
(setq old (strcase (getstring "\nOld text style <*>: "))) ;select and check old style
(while (and (/= old "") (null (tblsearch "STYLE" old)))
  (progn
    (prompt "That style does not exist")
    (setq old (strcase (getstring "\nOld text style: <*>: ")))
  )
)                                                     ;end while statement
(setq old (if (= old "") "*" old))

(prompt "\nSelect objects or Return for all text:")   ;select objects or text
(setq ss1 (ssget))
(if (null ss1) (setq ss1 (ssget "X" '((0 . "TEXT")))))

(setq sl (sslength ss1))                              ;determine loop count
(setq cnt 0)

(setq tbl (tblsearch "STYLE" new))                    ;change text
(prompt "\nUpdating text style...")
(setvar "highlight" 0)
(repeat sl
  (if (= "TEXT" (cdr (assoc 0 (setq e (entget (ssname ss1 cnt))))))
    (if (or (= (cdr (assoc 7 e)) old) (= old "*"))
      (progn
	(setq e (subst (cons 7 new) (assoc 7 e) e))
	(setq e (subst (assoc 41 tbl) (assoc 41 e) e))
	(setq e (subst (cons 51 (cdr (assoc 50 tbl))) (assoc 51 e) e))
	(setq e (subst (assoc 71 tbl) (assoc 71 e) e))
	(entmod e)
	(setq mark (+ mark 1))
      )
    )
  )
  (setq cnt (+ cnt 1))
)
(prompt "\n")
(prin1 mark) (prompt "  Lines of text updated")

(setvar "CMDECHO" 1)
(setvar "HIGHLIGHT" 1)
(gc) (princ)
)
;* UDIST User interface distance function
;* BIT (0 for none) and KWD key word ("" for none) are same as for INITGET.
;* MSG is the prompt string, to which a default real is added as <DEF> (nil
;* for none), and a : is added. BPT is base point (nil for none).
;*
(defun udist (bit kwd msg def bpt / inp)
  (if def                                              ;test for a default
    (setq msg (strcat "\n" msg " <" (rtos def) ">: ")  ;string'em  with default
	  bit (* 2 (fix (/ bit 2))) ;a default and no null bit code conflict so
    );setq                          ;this reduces bit by 1 if odd, to allow null
    (setq msg (strcat "\n" msg ": "))                  ;without default
  );if
  (initget bit kwd)
  (setq inp
    (if bpt                  ;check for a base point
      (getdist msg bpt)      ;and use it in the GET commands
      (getdist msg)
  ) );setq&if
  (if inp inp def)           ;compare the results, return appropriate value
);defun
;*
;* DXF takes an integer dxf code and an entity data list.
;* It returns the data element of the association pair.
;*
(defun dxf(code elist)
  (cdr (assoc code elist))   ;finds the association pair, strips 1st element
);defun
;*

(if (getvar "FLATLAND") ;to allow single prototype dwg for R9 and R10 Il-DISK users
  (progn (setvar "FLATLAND" 0) (setvar "UCSICON" 0))
)

(princ)
;*    AutoLISP Files for Inside AutoLISP IL DISK - (c) 1987, 1988, 1989
;* New Riders Publishing. All Rights Reserved. Version 10.00 for PC-DOS/MS-DOS
;*       Developed by Rustin Gesner, Patrick Haessly and Joseph Smith
;*
(if (not (and udist dxf))                                  ;test subroutines
  (prompt "\nRequires UDIST and DXF functions. Load aborted. ")
  (progn                                                   ;else load OK

;* C:CSCALE changes the scale of selected text and blocks based on a new scale
;* factor and the current drawing scale factor. It uses the global #DWGSC scale
;* as a default. It requires the UDIST user GET function and the DXF function.

(defun C:CSCALE ( / ss1 oldsc fact count emax en ed et dxf72)
  (prompt "\nSelect text and blocks to rescale...")       ;tell what to pick
  (if (setq ss1 (ssget))                                  ;get the selections
    (progn
      (setq fact                                          ;ratio new/old scale
	    (/ (udist 1 "" "New scale factor" #dwgsc nil) ;get new scale factor
	       (udist 1 "" "Old scale factor" #dwgsc nil) ;get old scale factor
	    )
	    count 0                                   ;initialize counter
	    emax (sslength ss1)                       ;find max no. of entities in set
      );setq
      (prompt "\nSearching...")                       ;keep user informed
      (while (< count emax)                           ;start program loop
	(setq en (ssname ss1 count)                   ;get an entity name
	      ed (entget en)                          ;entity data
	      et (dxf 0 ed)                           ;entity type
	);setq
	(prompt ".")                                  ;give a dot each loop
	(cond                                         ;filter entity type
	  ( (= et "TEXT" )                            ;check for Text
	    (setq dxf72 (dxf 72 ed))                  ;store the alignment flag
	    (cond                                     ;filter on text alignment
	      ( (= dxf72 0)                           ;starting point
		(command "scale" en "" (dxf 10 ed) fact) ;rescale it
	      )
	      ( (< dxf72 3)                           ;1 & 2 are center and right
		(command "scale" en "" (dxf 11 ed) fact) ;use 2nd point
	      )
	      ( (= dxf72 3)                               ;skip it, prompt
		(prompt "\nIgnoring ALIGNED text string")
	      )
	      ( (= dxf72 4)                               ;middle text
		(command "scale" en "" (dxf 11 ed) fact)  ;use 2nd point
	      )
	      ( (= dxf72 5)                               ;fit text, skip it, prompt
		(prompt "\nIgnoring FIT text string")
	      )
	    );cond alignment
	  );text
	  ( (= et "INSERT")                          ;check for block inserts
	    (command "scale" en "" (dxf 10 ed) fact) ;use dxf 10 insertion point
	  );insert
	);cond
	(setq count (1+ count))
      );while
    );progn
  );if
  (princ)                                            ;finish cleanly
);defun
(princ)                                              ;load cleanly
;*
));test subroutines

(defun c:zl (/ p1 p2)
(setq p1 (getvar "limmin"))
(setq p2 (getvar "limmax"))
(command "zoom" "W" p1 p2 )
)

(defun c:zv (/ p1 p2)
(setq p1 (getvar "vsmin"))
(setq p2 (getvar "vsmax"))
(command "zoom" "W" p1 p2 )
)


(VMON)
(DEFUN *ERROR* (MSG)
  (PRINC "ERROR : ")
  (PRINC MSG)
  (TERPRI))

(defun dtr (a)
(* pi (/ a 180.0)))

;-----------------------------------------------------------------------------
(defun clay3 (/ tst tst1 tst2)
 (setvar "CMDECHO" 0)
;-----------------------------------------------------------------------------
 (setq tst (tblsearch "LAYER" "wl"))
 (if (= tst nil) (progn
   (prompt "\n*** CREATING [WL] LAYER ***")
   (command "LAYER" "N" "wl" "C" "7" "wl" "")))
 (if (/= tst nil) (progn
    (if (= (cdr (assoc 70 tst)) 65) (command "LAYER" "T" "wl" ""))
    (if (or (= (cdr (assoc 70 tst)) 64) (= (cdr (assoc 70 tst)) 0))
     (command "LAYER" "ON" "wl" ""))))
;-----------------------------------------------------------------------------
  (setq tst1 (tblsearch "LAYER" "do"))
   (if (= tst1 nil) (progn
     (prompt "\n*** CREATING [DO] LAYER ***")
     (command "LAYER" "N" "do" "C" "2" "DO" "")))
   (if (/= tst1 nil) (progn
      (if (= (cdr (assoc 70 tst1)) 65) (command "LAYER" "T" "do" ""))
      (if (or (= (cdr (assoc 70 tst1)) 64) (= (cdr (assoc 70 tst1)) 0))
       (command "LAYER" "ON" "do" ""))))
;-----------------------------------------------------------------------------
 (setq tst2 (tblsearch "LAYER" "don"))
  (if (= tst2 nil) (progn
    (prompt "\n*** CREATING [DON] LAYER ***")
    (command "LAYER" "N" "don" "c" "15" "don" "")))
  (if (/= tst2 nil) (progn
     (if (= (cdr (assoc 70 tst2)) 65) (command "LAYER" "T" "don" ""))
     (if (or (= (cdr (assoc 70 tst2)) 64) (= (cdr (assoc 70 tst2)) 0))
      (command "LAYER" "ON" "don" ""))))
)
;****************************************************************************

(defun c:ld (/ hng mdl1 mdl2 hnd ohng ohnd hd h1 h2 h3 h4 oh1 ohd
	       p1 p2 p3 p4 lastecho lastblip lastla)
  (if (= dd nil)
   (while (= dd nil)
    (setq dd
     (abs
      (getreal  "\nEnter wall thickness :  " )
     )
    )
   )
    (setq dd
     (getreal
	  (strcat "\nEnter wall thickness <" (rtos ddd) "> : " )
     )
    )
  )
  (if
   (= dd nil)
    (setq dd (abs ddd))
    (setq ddd (abs dd))
  )
;-----------------------------------------------------------------------------
(clay3)
;-----------------------------------------------------------------------------
  (setq
	hng (getpoint "\nPick left door hinge :")
	hnd (getpoint hng "\nPick left door handle :")
	 dc (distance hng hnd)
       ohng (polar hng (+ (angle hnd hng) 1.57079) ddd)
       ohnd (polar hnd (angle hng ohng) ddd)
	 h1 (polar hng (angle hng hnd) 35.0)
	 h2 (polar h1 (angle hng ohng) 45.0)
	 hd (polar h1 (angle ohng hng) (- dc 70.0))
	 h3 (polar h2 (angle hng hnd) 15.0)
	 h4 (polar h3 (angle hng ohng) (- dd 90.0))
	 h5 (polar h4 (angle hnd hng) 15.0)
	 h6 (polar h5 (angle hng ohng) 45.0)
	oh1 (polar h1 (angle hng hnd) 50.0)
	ohd (polar hd (angle hng hnd) 50.0)
       mdl1 (polar hng (angle hng hnd) (/ dc 2.0))
       mdl2 (polar ohng (angle ohng ohnd) (/ dc 2.0))
	 p0 (polar mdl2 (angle hng ohng) 500.0)
	 p1 (polar h1 (angle h1 hd) (/ (- dc 70.0) 5.0))
	 p2 (polar oh1 (angle oh1 ohd) (/ (- dc 70.0) 5.0))
	 p3 (polar hd (angle hd h1) (/ (- dc 70.0) 5.0))
	 p4 (polar ohd (angle ohd oh1) (/ (- dc 70.0) 5.0))
	 hnd1 (polar hnd (angle hnd hng) 35.0)
	lastecho (getvar "CMDECHO")
	lastla (getvar "CLAYER")
  )
;-----------------------------------------------------------------------------
  (setvar "CMDECHO" 0)
;-----------------------------------------------------------------------------
 (if (and (>= dc 400.0) (<= dc 3000.0) )
 (progn
  (command "BREAK" hnd hng "BREAK" ohnd ohng
	   "LAYER" "S" "DO" ""
	   "LINE" ohd oh1 h1 ""
	   "PLINE" h1 "w" "0" "0" h2 h3 h4 h5 h6 ohng hng "c"
	   "MIRROR" "L" "" mdl1 mdl2 "N"
	   "PLINE" hnd "W" "0" "0" hnd1 "A" "CE" h1 hd "L" h1 hng ""
	   "LINE" p1 p2 "" "LINE" p3 p4 ""
	   "LAYER" "S" "WL" ""
	   "LINE" hnd ohnd "" "LINE" hng ohng ""
;          "LAYER" "S" "DON" ""
;          "CIRCLE"  p0  500.0
	   "REDRAW"
	   "LAYER" "S" lastla ""
 )))
;-----------------------------------------------------------------------------
  (setvar "CMDECHO" lastecho)
  (prompt "\n                                ***  DONE  ***                ")
)

;****************************************************************************

(defun c:pd (/ hng mdl1 mdl2 hnd hnd1 ohng ohnd hd h1 h2 h3 h4 oh1 ohd
	       p1 p2 p3 p4 lastecho lastblip lastla)
  (if (= dd nil)
   (while (= dd nil)
    (setq dd
     (abs
      (getreal  "\nEnter wall thickness :  " )
     )
    )
   )
    (setq dd
     (getreal
	  (strcat "\nEnter wall thickness <" (rtos ddd) "> : " )
     )
    )
  )
  (if
   (= dd nil)
    (setq dd (abs ddd))
    (setq ddd (abs dd))
  )
;-----------------------------------------------------------------------------
(clay3)
;-----------------------------------------------------------------------------
  (setq
	hng (getpoint "\nPick right leaf hinge :")
	hnd (getpoint hng "\nPick left leaf hinge :")
	 dc (distance hng hnd)
       ohng (polar hng (- (angle hnd hng) 1.57079) ddd)
       ohnd (polar hnd (angle hng ohng) ddd)
	 h1 (polar hng (angle hng hnd) 35.0)
	 h2 (polar h1 (angle hng ohng) 45.0)
	 hd (polar h1 (angle ohng hng) (/ (- dc 70.0) 2.0))
	 h3 (polar h2 (angle hng hnd) 15.0)
	 h4 (polar h3 (angle hng ohng) (- dd 90.0))
	 h5 (polar h4 (angle hnd hng) 15.0)
	 h6 (polar h5 (angle hng ohng) 45.0)
	oh1 (polar h1 (angle hng hnd) 45.0)
	ohd (polar hd (angle hng hnd) 45.0)
       mdl1 (polar hng (angle hng hnd) (/ dc 2.0))
       mdl2 (polar ohng (angle ohng ohnd) (/ dc 2.0))
	 p0 (polar mdl2 (angle hng ohng) 500.0)
	 p1 (polar h1 (angle h1 hd) (/ (- dc 70.0) 10.0))
	 p2 (polar oh1 (angle oh1 ohd) (/ (- dc 70.0) 10.0))
	 p3 (polar hd (angle hd h1) (/ (- dc 70.0) 10.0))
	 p4 (polar ohd (angle ohd oh1) (/ (- dc 70.0) 10.0))
	lastecho (getvar "CMDECHO")
	lastla (getvar "CLAYER")
  )
;-----------------------------------------------------------------------------
  (setvar "CMDECHO" 0)
;-----------------------------------------------------------------------------
 (if (and (>= dc 800.0) (<= dc 6000.0) )
 (progn
  (command "BREAK" hnd hng "BREAK" ohnd ohng
	   "LAYER" "S" "DO" ""
	   "PLINE" ohd "W" "0" "0" oh1 h1 ""
	   "MIRROR" "L" "" mdl1 mdl2 "N"
	   "PLINE" h1 "w" "0" "0" h2 h3 h4 h5 h6 ohng hng "c"
	   "MIRROR" "L" "" mdl1 mdl2 "N"
	   "PLINE" hng "W" "0" "0" h1 hd "A" "CE" h1 hnd ""
	   "MIRROR" "L" "" mdl1 mdl2 "N"
	   "PEDIT" "L" "J" "L" hd "" ""
	   "LINE" p1 p2 ""
	   "MIRROR" "L" "" mdl1 mdl2 "N"
	   "LINE" p3 p4 ""
	   "MIRROR" "L" "" mdl1 mdl2 "N"
	   "LAYER" "S" "WL" ""
	   "LINE" hnd ohnd "" "LINE" hng ohng ""
;          "LAYER" "S" "DON" ""
;          "CIRCLE"  p0 500.0
	   "REDRAW"
	   "LAYER" "S" lastla ""
 )))
;-----------------------------------------------------------------------------
  (setvar "CMDECHO" lastecho)
  (prompt "\n                                ***  DONE  ***                ")
)
;****************************************************************************
(defun c:rd (/ hng mdl1 mdl2 hnd ohng ohnd hd h1 h2 h3 h4 oh1 ohd
	       p1 p2 p3 p4 lastecho lastblip lastla)
  (if (= dd nil)
   (while (= dd nil)
    (setq dd
     (abs
      (getreal  "\nEnter wall thickness :  " )
     )
    )
   )
    (setq dd
     (getreal
	  (strcat "\nEnter wall thickness <" (rtos ddd) "> : " )
     )
    )
  )
  (if
   (= dd nil)
    (setq dd (abs ddd))
    (setq ddd (abs dd))
  )
;-----------------------------------------------------------------------------
(clay3)
;-----------------------------------------------------------------------------
  (setq
	hng (getpoint "\nPick right door hinge :")
	hnd (getpoint hng "\nPick right door handle :")
	 dc (distance hng hnd)
       ohng (polar hng (- (angle hnd hng) 1.57079) ddd)
       ohnd (polar hnd (angle hng ohng) ddd)
	 h1 (polar hng (angle hng hnd) 35.0)
	 h2 (polar h1 (angle hng ohng) 45.0)
	 hd (polar h1 (angle ohng hng) (- dc 70.0))
	 h3 (polar h2 (angle hng hnd) 15)
	 h4 (polar h3 (angle hng ohng) (- dd 90.0))
	 h5 (polar h4 (angle hnd hng) 15.0)
	 h6 (polar h5 (angle hng ohng) 45.0)
	oh1 (polar h1 (angle hng hnd) 45.0)
	ohd (polar hd (angle hng hnd) 45.0)
       mdl1 (polar hng (angle hng hnd) (/ dc 2.0))
       mdl2 (polar ohng (angle ohng ohnd) (/ dc 2.0))
	 p0 (polar mdl2 (angle hng ohng) 500.0)
	 p1 (polar h1 (angle h1 hd) (/ (- dc 70.0) 5.0))
	 p2 (polar oh1 (angle oh1 ohd) (/ (- dc 70.0) 5.0))
	 p3 (polar hd (angle hd h1) (/ (- dc 70.0) 5.0))
	 p4 (polar ohd (angle ohd oh1) (/ (- dc 70.0) 5.0))
	lastecho (getvar "CMDECHO")
	lastla (getvar "CLAYER")
  )
;-----------------------------------------------------------------------------
  (setvar "CMDECHO" 0)
;-----------------------------------------------------------------------------
 (if (and (>= dc 400.0) (<= dc 3000.0) )
 (progn
  (command "BREAK" hnd hng "BREAK" ohnd ohng
	   "LAYER" "S" "DO" ""
	   "LINE" ohd oh1 h1 ""
	   "PLINE" h1 "w" "0" "0" h2 h3 h4 h5 h6 ohng hng "c"
	   "MIRROR" "L" "" mdl1 mdl2 "N"
	   "PLINE" hng "W" "0" "0" h1 hd "A" "CE" h1 hnd "L" hnd ""
	   "LINE" p1 p2 "" "LINE" p3 p4 ""
	   "LAYER" "S" "WL" ""
	   "LINE" hnd ohnd "" "LINE" hng ohng ""
;          "LAYER" "S" "DON" ""
;          "CIRCLE" p0  500.0
	   "REDRAW"
	   "LAYER" "S" lastla ""
 )))
;-----------------------------------------------------------------------------
  (setvar "CMDECHO" lastecho)
  (prompt "\n                                ***  DONE  ***                ")
)



(defun C:LE (/ lefile ans loa lspfile)
 (setvar "CMDECHO" 0)
 (command "LEZ")
 (setq lefile (open "B:/AUTOLE.SCR" "r"))
 (if (/= lefile nil)
  (progn
   (setq loa (read-line lefile)
	 lspfile (substr loa 7 (- (strlen loa) 8))
   )
   (close lefile)
   (initget "Yes No")
   (setq ans (getkword (strcat "Would you like to LOAD this file " lspfile " ? <Y>: ")))
   (if (/= ans "No")
    (command "script" "B:/AUTOLE")
   )
  )
 )
 (princ)
)

(defun C:LE (/ lefile ans loa lspfile)
 (setvar "CMDECHO" 0)
 (command "LEZ")
 (setq lefile (open "B:/AUTOLE.SCR" "r"))
 (if (/= lefile nil)
  (progn
   (setq loa (read-line lefile)
	 lspfile (substr loa 7 (- (strlen loa) 8))
   )
   (close lefile)
   (initget "Yes No")
   (setq ans (getkword (strcat "Would you like to LOAD this file " lspfile " ? <Y>: ")))
   (if (/= ans "No")
    (command "script" "B:/AUTOLE")
   )
  )
 )
 (princ)
)

(defun C:LE (/ lefile ans loa lspfile)
 (setvar "CMDECHO" 0)
 (command "LEZ")
 (setq lefile (open "B:/AUTOLE.SCR" "r"))
 (if (/= lefile nil)
  (progn
   (setq loa (read-line lefile)
	 lspfile (substr loa 7 (- (strlen loa) 8))
   )
   (close lefile)
   (initget "Yes No")
   (setq ans (getkword (strcat "Would you like to LOAD this file " lspfile " ? <Y>: ")))
   (if (/= ans "No")
    (command "script" "B:/AUTOLE")
   )
  )
 )
 (princ)
)

(defun C:LE (/ lefile ans loa lspfile)
 (setvar "CMDECHO" 0)
 (command "LEZ")
 (setq lefile (open "A:/AUTOLE.SCR" "r"))
 (if (/= lefile nil)
  (progn
   (setq loa (read-line lefile)
	 lspfile (substr loa 7 (- (strlen loa) 8))
   )
   (close lefile)
   (initget "Yes No")
   (setq ans (getkword (strcat "Would you like to LOAD this file " lspfile " ? <Y>: ")))
   (if (/= ans "No")
    (command "script" "A:/AUTOLE")
   )
  )
 )
 (princ)
)

(defun C:LE (/ lefile ans loa lspfile)
 (setvar "CMDECHO" 0)
 (command "LEZ")
 (setq lefile (open "C:/10/LE/AUTOLE.SCR" "r"))
 (if (/= lefile nil)
  (progn
   (setq loa (read-line lefile)
	 lspfile (substr loa 7 (- (strlen loa) 8))
   )
   (close lefile)
   (initget "Yes No")
   (setq ans (getkword (strcat "Would you like to LOAD this file " lspfile " ? <Y>: ")))
   (if (/= ans "No")
    (command "script" "C:/10/LE/AUTOLE")
   )
  )
 )
 (princ)
)
;************************ MCLGRID.LSP ***************************
;
; This macro implements the GETCORNER function to "drag" a
; polyline rectangle and create a grid of given tile size or
; given spaces/divisions along each sides.
; 20 August 1991, Jeddah - MPR 221 ( for ceiling and electrical
; fixture layout, etc)
;
;**************************************************************

(DEFUN C:MCLGRID ()
  (setq scmde (getvar "CMDECHO"))
  (setq sblip (getvar "BLIPMODE"))
  (setvar "CMDECHO" 0)
  (setvar "BLIPMODE" 0)
  (setq mod (strcase (getstring "\n(D)ivided or (T)iled  (D/T) <T> ")))
  (if (= mod "") (setq  mod "T"))
  (if (= mod "T")
      (progn
		(setq tlb (getreal "\nEnter tile base dimension   > "))
		(setq tlh (getreal "\nEnter tile height dimension > "))
		)
      (if (= mod "D")
			(progn
			(setq nobs (getint "\nEnter spaces along base   > "))
			(setq nohs (getint "\nEnter spaces along height > "))
	)
		)
	)
  (setq pt1 (getpoint "\nPick first corner: "))
  (setq pt3 (getcorner pt1 "\nPick diagonal corner: "))
  (setq pt2 (getpoint pt1 "\nPick second corner for orientation: "))
  (setq b (distance pt1 pt2))
  (setq h (distance pt2 pt3))
  (setq angr (angle pt1 pt2))
  (setq hpi 1.57079627)
  (setq angr2 (+ angr hpi))
  (setq pt4 (polar pt1 angr2 h))
;;;Along base
(if (= mod "T")
  (progn
  (setq bn (float (/ b tlb)))
  (setq bno (fix bn))
  (setq bfr (- bn bno))
  (setq hbfr (/ bfr 2.0))
  (if ( < hbfr 0.00000001)
	 (progn
       (setq bno (- bno 1))
       (setq x1 tlb)
    )
    (if ( > hbfr 0.00000001)
	(progn
	   (setq bno bno)
	   (setq x1 (+ (* hbfr  tlb) (/ tlb 2.0)))
	)   ;progn
      )     ;end if
    )       ;end if
  )        ;progn
)           ;end if
(if (= mod "D")
	(progn
	(setq bno (- nobs 1))
	(setq tlb (float (/ b nobs)))
	(setq x1 tlb)
	)
)
	(setq pt5 (polar pt1 angr x1))
	(setq pt6 (polar pt5 angr2 h))
	(setvar "snapang" angr)
	(command "line" pt5 pt6 "")
	(command "array" (ssget "L") "" "r" "1" bno tlb )

;;;Along height
(if (= mod "T")
  (progn
  (setq hn (float (/ h tlh)))
  (setq hno (fix hn))
  (setq hfr (- hn hno))
  (setq hhfr (/ hfr 2.0))
  (if ( < hhfr 0.00000001)
	 (progn
       (setq hno (- hno 1))
       (setq y1 tlh)
    )
    (if ( > hhfr 0.00000001)
	(progn
	   (setq hno hno)
	   (setq y1 (+ (* hhfr tlh) (/ tlh 2.0)))
	)
       )
     )
  )
)
(if (= mod "D")
	(progn
	(setq hno (- nohs 1))
	(setq tlh (float (/ h nohs)))
	(setq y1 tlh)
	)
)
	(setq pt7 (polar pt1 angr2 y1))
	(setq pt8 (polar pt7 angr b))
	(command "line" pt7 pt8 "")
	(command "array" (ssget "L") "" "r" hno "1" tlh )
   (setvar "CMDECHO" scmde)
   (setvar "BLIPMODE" sblip)
   (setvar "snapang" 0)
   (princ)
)
;*****************************************************************
; MDOOR.LSP  8 AUGUST 1991
; SINGLE OR DOUBLE LEAF DOOR , LEFT OR RIGHT SWING
; WITH OPTION TO MOVE BY RELATIVE POSITION
; 28 OCT 92 - REVISED
;*****************************************************************

(defun dswing ()
(setq pt15 (polar pt3 angr lw))
(command "layer" "m" "A-DR-LEF" "LT" "CONTINUOUS" "" "C" 11 "" "")
(command "pline" pt3 pt5 pt6 pt7 pt3 "")
(command "arc" pt15 "E" pt7 "A" 90 )
(command "pline" pt11 pt12 pt13 pt14 pt11 "")
(command "arc" pt12 "E" pt15 "A" 90)
)

(defun lswing ()
(command "layer" "m" "A-DR-LEF" "LT" "CONTINUOUS" "" "C" 11 "" "")
(command "pline" pt3 pt5 pt6 pt7 pt3 "")
(command "arc" pt11 "E" pt7 "A" 90 )
)

(defun rswing ()
(command "layer" "m" "A-DR-LEF" "LT" "CONTINUOUS" "" "C" 11 "" "")
(command "pline" pt11 pt12 pt13 pt14 pt11 "")
(command "arc" pt12 "E" pt3 "A" 90 )
)

(defun c:mdoor ()
(setq ce (getvar "CMDECHO"))
(setq blm (getvar "BLIPMODE"))
(setq osm (getvar "OSMODE"))
(setvar "CMDECHO" 0)
(setvar "BLIPMODE" 0)
(setq clyr (getvar "CLAYER"))
(setq vhpi 1.57079627)        ;90
(setq vpi (* vhpi 2.0))       ;180
(setq wpi (* vhpi 4.0))       ;360
(setq jw 35.0)                ;jamb width
(setq dw 45.0)                ;leaf thk
(setq dor (strcase(getstring "\n(S)ingle (D)ouble  (S/D) > ")))
(setq lw (getreal "\nEnter Door leaf width    >"))
(if (= dor "S")
(setq swng (strcase(getstring "\n(L)eft swing (R)ight swing (L/R) >"))))
(setvar "OSMODE" 512)
(setq pt1 (getpoint "\nPick entry lower left corner  "))
(setq pt1a (getpoint pt1 "\nNext point for orientation  "))
(setvar "OSMODE" 128)
(setq pt4 (getpoint pt1 "\nPick other side of wall  "))
(setvar "OSMODE" osm)
(setq jd (distance pt1 pt4))
(setq mw 15.0)                ;jamb mould thk
(setq ml (- jd (* dw 2.0)))   ;jamb mould with
(setq angr (angle pt1 pt1a))
(setq angd (* angr 57.29577951))
(if (= dor "S") (setq do lw)
    (if (= dor "D") (setq do (* 2.0 lw))
    )
)
(setq doo (+ do (* 2.0  jw)))
(setq dow (+ do jd))
(setq dno (- do (* 2.0  dw)))
(setq dod (- do (* 2.0  mw)))
(setq pt2 (polar pt1 angr jw))
(setq pt8 (polar pt2 angr do))
(setq pt9 (polar pt8 angr jw))
(setq pt3 (polar pt2 (+ angr vhpi) jd))
(setq pt7 (polar pt3 (+ angr vhpi) lw))
(setq pt5 (polar pt3  angr dw))
(setq pt6 (polar pt5 (+ angr vhpi) lw))
(setq pt11 (polar pt8 (+ angr vhpi) jd))
(setq pt12 (polar pt11 (+ angr vhpi) lw))
(setq pt10 (polar pt9 (+ angr vhpi) jd))
(setq pt14 (polar pt5  angr dno))
(setq pt13 (polar pt14 (+ angr vhpi) lw))
(setq ptx1 (polar pt2 (+ angr vhpi) dw))
(setq ptx2 (polar ptx1 angr mw))
(setq ptx3 (polar ptx2 (+ angr vhpi) ml))
(setq ptx4 (polar ptx3 (+ angr  vpi) mw))
(setq ptx6 (polar pt8  (+ angr vhpi) dw))
(setq ptx5 (polar ptx6 (+ angr  vpi) mw))
(setq ptx8 (polar ptx5 (+ angr vhpi) ml))
(setq ptx7 (polar ptx8 angr mw)) 
(command "break" pt1 pt9)
(command "break" pt4 pt10)
(command "layer" "t" "A-DR-JMB" "")
(command "layer" "t" "A-DR-LEF" "")
(command "layer" "t" "A-DR-HED" "")
(command "layer" "m" "A-DR-JMB" "LT"  "CONTINUOUS" "" "C" 11 "" "")
(command "pline" pt1 pt2 ptx1 ptx2 ptx3 ptx4 pt3 pt4 pt1 "")
(command "pline" pt8 pt9 pt10 pt11 ptx7 ptx8 ptx5 ptx6 pt8 "")
(if (= dor "S")
	(progn
	(if (= swng "L") (lswing)
		(if (= swng "R") (rswing)
		)       
	)
   )
)
(if (= dor "D") (dswing))
(command "layer" "m" "A-DR-HED" "LT" "CONTINUOUS" "" "C" "WHITE" "" "")
(command "line" pt1 pt9 "")
(command "line" pt4 pt10 "")
(command "layer" "s" clyr "")
(command "layer" "f" "A-DR-HED" "" )
(setq ans (strcase (getstring "\nMove door  (Y/N)  <N> ")))
   (if (= ans "Y")
      (progn
      (setq pt20 (getpoint  "\nPick relative position  "))
      (setq pt21 (getpoint pt20 "\nEnter relative distance or coordinates  "))
      (princ "\n....Window door, <Ret>, pick endpoint of wall near jamb")
      (command "stretch" "c" pause pause pause pause pt21)
      )
   )
(setvar "CMDECHO" ce)
(setvar "BLIPMODE" blm)
(princ)
)
;MDWGRID.LSP - DRAW INITIAL DRAWING GRIDLINES - 8 AUG 1991
;GUIDE FOR DIMS IS ALSO DRAWN, LATER SHOULD BE ERASED.
;Revised 20 jun 1992

(defun c:mdwgrid ()
(setq ce (getvar "CMDECHO"))
(setq blm (getvar "BLIPMODE"))
(setq cly (getvar "CLAYER"))
(setq cbl "bylayer")
(setvar "CMDECHO" 0)
(setvar "BLIPMODE" 0)
(setq pt1 (getpoint "\nPick upper left of grids  "))
(setq pt12 (getcorner pt1 "\nPick approximate lower right of grids "))
(setq cdi (getreal "\nInput circle diameter based 1:100  <1200.0>"))
(if (= cdi nil) (setq cdi 1200.0) (setq cdi cdi))
(setq txtsi (getreal "\nInput grid mark ht. based 1:100  <500.0>"))
(if (= txtsi nil) (setq txtsi 500.0) (setq txtsi txtsi))
(setq pi 3.141592654)
(setq pi2 (/ pi 2.0))
(setq pi4 (/ pi 4.0))
(setq usr (getvar "USERR3"))
(setq fac (/ usr 100.0))
(setq cd (* fac cdi))
(setq cr (/ cd 2.0))
(setq ofd (* fac 7100.0))
(setq txts (* fac txtsi))
(setq tmd1 (* fac 700.0))
(setq tmd2 (* fac 1400.00))
(setq tmd3 (* fac 212.0))
(setq pt2 (list (car pt1) (- (cadr pt1) (+ ofd cr))))
(setq pt3 (list (+ (car pt2) cr) (cadr pt2)))
(setq pt4 (list (+ (car pt3) ofd) (cadr pt3)))
(setq pt5 (list (car pt4) (+ (cadr pt4) ofd)))
(setq pt6 (list (car pt5) (+ (cadr pt5) cr)))
(setq pt7 (list (car pt4) (cadr pt12)))
(setq pt8 (list (car pt12) (cadr pt4)))
(setq pt9 (list (car pt5)  (- (cadr pt5) tmd1)))
(setq pt10 (list (- (car pt9) tmd3 ) (cadr pt9)))
(setq pt11 (list (+ (car pt9) (* 2.0 tmd2)) (cadr pt9)))
(setq pt12 (polar pt9 (+ pi4 pi) tmd3))
(setq pt13 (polar pt9 pi4 tmd3))
(setq pt14 (list (+ (car pt3) tmd1) (cadr pt3)))
(setq pt15 (list (car pt14) (+ (cadr pt14) tmd3)))
(setq pt16 (list (car pt14) (- (cadr pt14) (* 2.0 tmd2))))
(setq pt17 (polar pt14 (* 3.0 pi4)  tmd3))
(setq pt18 (polar pt14 (+ (* 3.0 pi4) pi) tmd3)) 
(command "layer" "m" "G-GN-GRD" "LT" "GRID" "" "C" 11 "" "")
(command "color" cbl "")
(command "line" pt3 pt8 "")
(command "line" pt5 pt7 "")
(command "layer" "m" "G-GN-GMK" "LT" "CONTINUOUS" "" "C" "YELLOW" "" "")
(command "color" cbl "")
(command "circle" pt2 "D" cd)
(command "circle" pt6 "D" cd)
(command "layer" "m" "G-GN-GTX" "LT" "CONTINUOUS" "" "C" "WHITE" "" "")
(command "color" 7 "")
(command "text" "m" pt2 txts 0 "A")
(command "text" "m" pt6 txts 0 "1")
(command "color" cbl "")
(command "layer" "m" "G-GN-DIM" "LT" "CONTINUOUS" "" "C" "CYAN" "" "")
(command "line" pt12 pt13 "")
(command "line" pt17 pt18 "")
(command "color" 15 "" )
(command "line" pt15 pt16 "")
(command "line" pt10 pt11 "")
(command "layer" "s" cly "")
(command "color" cbl "")
(setvar "CMDECHO" ce)
(setvar "BLIPMODE" blm)
(princ)
)
;************************************************************************
;            Msetup.lsp  12 July 1991 - mpr221
;   Sets selected paper size, scale, style and dimvars. This is set to a
;   standard case. Dimvars can easily be modified to suit requirement
;   To draw a 1:20 in a 1:100 dwg, set msetup to a paper size with scale
;   setting to 1:20, then scale to 5 times (100/20). If done msetup to
;   original 1:100 scale. Added default values - 5'92
;************************************************************************

(defun dmvars ()
   (setq rltx (/ txsz 100.0))
	(setvar "userr3" scale)
	(setvar "coords" 1)
	(setq scale10 (* scale 10.0))
	(setvar "ltscale"  scale10)
	(setvar "lunits" 2)
	(setvar "luprec" 0)
	(setvar "textsize" 0)
	(setvar "mirrtext" 0)
	(setvar "dimalt" 0)
	(setvar "dimaltd" 0)
	(setvar "dimaltf" 25.4)
	;(setvar "dimapost" "")   ;<RO>
	(setvar "dimaso" 1.0000)
	(setvar "dimasz" 3.000)   ;default = 0.1800
	;(setvar "dimblk" "")     ;<RO>
	;(setvar "dimblk1" "")    ;<RO>
	;(setvar "dimblk2" "")    ;<RO>
	(setvar "dimcen" 1.0000)  ;default = 0.0900
	(setvar "dimdle" 2.0000)  ;default = 0.0000
	(setvar "dimdli" 0.0000)  ;default = 0.3800
	(setvar "dimexe" 2.000)   ;default = 0.1800
	(setvar "dimexo" 2.000)   ;default = 0.0625
	(setvar "dimlfac" 1.000)  ;default = 1.0000
	(setvar "dimlim" 0)
	;(setvar "dimpost" "")    ;<RO>
	(setvar "dimrnd" 0.0000)
	(setvar "dimsah" 1.0000)       ;default = 0
	(setvar "dimscale" scale) ;default = 1.0000
	(setvar "dimse1" 0)
	(setvar "dimse2" 0)
	(setvar "dimsho" 0)
	(setvar "dimsoxd" 0)
	(setvar "dimtad" 1.000)   ;default = 0
	(setvar "dimtih" 0.000)   ;default = 1
	(setvar "dimtix" 0)
	(setvar "dimtm" 0.0000)
	(setvar "dimtofl" 1.000)  ;default = 0
	(setvar "dimtoh" 0.000)   ;default = 1
	(setvar "dimtol" 0)
	(setvar "dimtp" 0.0000)
	(setvar "dimtsz" 1.5000)  ;default = 0        with draw arrow = 0
	(setvar "dimtvp" 0.0000)
	(setvar "dimtxt" rltx)    ;default = 0.1800
	(setvar "dimzin" 0)
	(princ)
)

(defun setscale ()
		(if (= size "A0") (setq x 1189.0 y 841.0)
			(if (= size "A1") (setq x 841.0 y 594.0)
				(if (= size "A2") (setq x 594.0 y 420.0)
					(if (= size "A3") (setq x 420.0 y 297.0)
						(if (= size "A4") (setq x 297.0 y 210.0)
							(if (= size "O")
								(progn
									(setq x (getreal "Enter base length > "))
									(setq y (getreal "Enter height      > "))
								)
							)
						)
					)
				)
			)
		)
	(setq dx (float (* x scale)))
	(setq dy (float (* y scale)))
	(setq pt1 '(0.0 0.0))
	(setq pt2 (list (+ (car pt1) dx) (cadr pt1)))
	(setq pt3 (list (car pt2) (+ (cadr pt1) dy)))
	(setq pt4 (list (car pt1) (cadr pt3)))
   (command "limits" pt1 pt3 )
	(command "pline" pt1 pt2 pt3 pt4 "c")
	(command "zoom" "a" )
	(command "style" "standard" txfnt  0  txwfc  0  "N" "N" "N" )
	(command "ucsicon" "off" )
)



(defun c:msetup ()
(setq ce (getvar "CMDECHO"))
(setvar "CMDECHO" 0)
(setq st (getstring "Type set (S)cale or (D)imvars (B)oth  (S/D/B)  <B> "))
(setq st (strcase st))
(if (= st "")(setq st "B")(setq st st))
(setq size (getstring "Enter paper size, A0, A1, A2, A3, A4 or (O)thers  <A0> "))
(setq size (strcase size))
(if (= size "")(setq size "A0")(setq size size))
(setq scale (getreal "Enter scale 1:___ (1,5,10,20,50,100,200, etc.)   <100> "))
(if (= scale nil)(setq scale 100)(setq scale scale))
(setq txsz (getreal "Enter text size based on 1:100 scale, say 300   <350> "))
(if (= txsz nil)(setq txsz 350)(setq txsz txsz))
(setq txwfc (getreal "Enter text width factor  <0.80>                 > "))
(if (= txwfc nil)(setq txwfc 0.80)(setq txwfc txwfc))
(setq txfnt (getstring "Enter text font   <romans>                      > "))
(if (= txfnt "")(setq txfnt "romans")(setq txfnt txfnt))
   (if (= st "B")
	(progn
	      (dmvars)
	      (setscale)
	 )
	(if (= st "D") (dmvars)
	    (if (= st "S") (setscale)
	    )
	)
   )
(setvar "CMDECHO" ce)
(princ)
)
; **********************************************************************
;                            MWINDOW.LSP   (c)mpr221
; This is use to draw a window with given jamb width, single or multiple
; jamb out to out opening.
; MPR 221 - 8 August 1991 - Jeddah
; Revised 28 Oct 92, Freeze A-WW-GLS on ceiling plan
; **********************************************************************

(defun C:MWINDOW ()
   (setq clyr (getvar "CLAYER"))
   (setq blm  (getvar "blipmode"))
   (setvar "blipmode" 0)
   (setq ce (getvar "cmdecho"))
   (setvar "cmdecho" 0)
   (setq osm (getvar "OSMODE"))
   (setq wop (getreal "\nEnter window opening width:  "))
   (setq wno (getint "\nEnter No. of panels:          "))
   ;initialize variables
   (setq vhpi 1.57079627)
	(setq vpi (* vhpi 2.0))
   (setq jw 50.0)
   (setq gt 6.0)
   (setvar "OSMODE" 512)
   (setq pt1 (getpoint "\nPick lower left of window: "))
   (setq pt1a (getpoint pt1 "\nNext point for orientation: "))
   (setvar "OSMODE" 128)
   (setq pt4 (getpoint pt1 "\nPick other side of wall  "))
   (setvar "OSMODE" osm)
   (setq jd (distance pt1 pt4))
   (setq jhd (/ (- jd gt) 2.0))
   (setq wnop (- wop (* jw 2.0)))
   (setq wopa (- wop jw))   (setq angr (angle pt1 pt1a))
	(setq wnopp (+ wnop jw))
	(setq wnoppp (* wnopp wno))
   (setq angd (* angr 57.2957795 ))
	(setvar "snapang" angr)
   (setq pt2 (polar pt1 angr jw))
   (setq pt3 (polar pt2 (+ angr vhpi) jd))
   (setq pt5 (polar pt2 (+ angr vhpi) jhd))
   (setq pt6 (polar pt5 (+ angr vhpi) gt))
   (setq pt7 (polar pt6 angr wnop))
   (setq pt8 (polar pt5 angr wnop))
   (setq pt8a (polar pt8 angr 5))
	(setq pt9 (polar pt2 angr wnop))
	(setq pt10 (polar pt9 angr jw))
	(setq pt12 (polar pt3 angr wnop))
	(setq pt11 (polar pt12 angr jw))
	(setq pt13 (polar pt3  angr wnoppp))
	(setq pt14 (polar pt2  angr wnoppp))
	(setq pt15 (polar pt3  angr jw))
	(setq pt16 (polar pt2  angr jw))
	(setq pt17 (polar pt13 (+ angr vpi) jw))
	(setq pt18 (polar pt14 (+ angr vpi) jw))  
   (command "layer" "m" "A-WW-JMB" "LT" "CONTINUOUS" "" "C" "YELLOW" "" "")
   ; drawjamb
      (command "pline" pt1 pt2 pt3 pt4 "c" )
      (command "pline" pt9 pt10 pt11 pt12 "c")
   ; drawglas
	   (command "layer" "m" "A-WW-GLS" "LT" "CONTINUOUS" "" "C" "YELLOW" "" "")
      (command "pline" pt5 pt6 pt7 pt8 "c")
	 (command "array" (ssget "C" pt8 pt8a) "" "R" "" wno wopa "")
	(command "break" pt15 "f" pt4 pt13)
	(command "break" pt16 "f" pt1 pt14)
	(command "line"  pt4 pt13 "")
	(command "line"  pt1 pt14 "")
	(command "layer" "t" "A-WW-HED" "")
   (command "layer" "m" "A-WW-HED" "LT" "CONTINUOUS" "" "C" "WHITE" "" "")
	(command "line" pt3 pt17 "")
	(command "line" pt2 pt18 "")
   (command "layer" "s" clyr "")
	(command "layer" "f" "A-WW-HED" "")
	(setvar "snapang" 0)
   (setvar "blipmode" blm)
	(setvar "cmdecho" ce)
   (princ)
)

;************************************************************************
;            Gsetup12.lsp  17 August 1991 - mpr221
;   Sets selected paper size, scale, style and dimvars. This is set to a
;   standard case. Dimvars can easily be modified to suit requirement
;   To draw a 1:20 in a 1:100 dwg, set msetup to a paper size with scale
;   setting to 1:20, then scale to 5 times (100/20). If done msetup to
;   original 1:100 scale.  ( FOR ACAD RELEASE 11 ). Revised 29.01.92
;************************************************************************

(defun dmvars ()
     (setq rltx (/ txsz 100.0))
	(setvar "userr3" scale)
     (setvar "COORDS" 1)
	(setq scale10 (* scale 10.0))
	(setvar "ltscale"  scale10)
	(setvar "lunits" 2)
	(setvar "luprec" 0)
;       (setvar "TEXTSIZE" 0)
	(setvar "MIRRTEXT" 0)
	(setvar "dimalt" 0)
	(setvar "dimaltd" 0)
	(setvar "dimaltf" 25.4)
	;(setvar "dimapost" "")   ;<RO>
	(setvar "dimaso" 1.0000)
	(setvar "dimasz" 3.000)   ;default = 0.1800
	;(setvar "dimblk" "")     ;<RO>
	;(setvar "dimblk1" "")    ;<RO>
	;(setvar "dimblk2" "")    ;<RO>
	(setvar "dimcen" 1.0000)  ;default = 0.0900
;   (setvar "dimclrd" "byblock") ;default = "byblock" **** R11 ***
;   (setvar "dimclre" "byblock") ;default = "byblock" **** R11 ***
;   (setvar "dimclrt" "byblock") ;default = "byblock" **** R11 ***
	(setvar "dimdle" 2.0000)  ;default = 0.0000
	(setvar "dimdli" 0.0000)  ;default = 0.3800
	(setvar "dimexe" 2.000)   ;default = 0.1800
	(setvar "dimexo" 2.000)   ;default = 0.0625
	(setvar "dimlfac" 1.000)  ;default = 1.0000
	(setvar "dimlim" 0)
	;(setvar "dimpost" "")    ;<RO>
	(setvar "dimrnd" 0.0000)
	(setvar "dimsah" 1.0000)       ;default = 0
	(setvar "dimscale" scale) ;default = 1.0000
	(setvar "dimse1" 0)
	(setvar "dimse2" 0)
	(setvar "dimsho" 0)
	(setvar "dimsoxd" 0)
	(setvar "dimtad" 1.000)   ;default = 0
	(setvar "dimtih" 0.000)   ;default = 1
	(setvar "dimtix" 0)
	(setvar "dimtm" 0.0000)
	(setvar "dimtofl" 1.000)  ;default = 0
	(setvar "dimtoh" 0.000)   ;default = 1
	(setvar "dimtol" 0)
	(setvar "dimtp" 0.0000)
	(setvar "dimtsz" 1.5000)  ;default = 0        with draw arrow = 0
	(setvar "dimtvp" 0.0000)
	(setvar "dimtxt" rltx)    ;default = 0.1800
	(setvar "dimzin" 0)
	(princ)
)

(defun setscale ()
		(if (= size "A0") (setq x 1189.0 y 841.0)
			(if (= size "A1") (setq x 841.0 y 594.0)
				(if (= size "A2") (setq x 594.0 y 420.0)
					(if (= size "A3") (setq x 420.0 y 297.0)
						(if (= size "A4") (setq x 297.0 y 210.0)
							(if (= size "O")
								(progn
									(setq x (getreal "Enter base length > "))
									(setq y (getreal "Enter height      > "))
								)
							)
						)
					)
				)
			)
		)
     (setq dx (float (* x scale)))
	(setq dy (float (* y scale)))
	(setq pt1 '(0.0 0.0))
	(setq pt2 (list (+ (car pt1) dx) (cadr pt1)))
	(setq pt3 (list (car pt2) (+ (cadr pt1) dy)))
	(setq pt4 (list (car pt1) (cadr pt3)))
     (command "limits" pt1 pt3 )
	(command "pline" pt1 pt2 pt3 pt4 "c")
	(command "zoom" "a" )
     (command "style" "standard" txfnt  0  txwfc  0  "N" "N" "N" )
     (command "ucsicon" "off" )
)



(defun c:gsetup12 ()
(setq ce (getvar "CMDECHO"))
(setvar "CMDECHO" 0)
(setvar "TILEMODE" 1)
(setq st (getstring "Type set (S)cale or (D)imvars (B)oth  (S/D/B)  <B> "))
(setq st (strcase st))
(if (= st "")(setq st "B")(setq st st))
(setq size (getstring "Enter paper size, A0, A1, A2, A3, A4 or (O)thers  <A0>"))
(setq size (strcase size))
(if (= size "")(setq size "A0")(setq size size))
(setq scale (getreal "Enter scale 1:___ (1,5,10,20,50,100,200, etc.)  <100> "))
(if (= scale nil)(setq scale 100)(setq scale scale))
(setq txsz (getreal "Enter text size based on 1:100 scale, say 300   <350>"))
(if (= txsz nil)(setq txsz 350)(setq txsz txsz))
(setq txwfc (getreal "Enter text width factor   <0.80>                > "))
(if (= txwfc nil)(setq txwfc 0.80)(setq txwfc txwfc))
(setq txfnt (getstring "Enter text font   <romans>                      > "))
(if (= txfnt "")(setq txfnt "romans")(setq txfnt txfnt))
    (if (= st "B")
	(progn
	      (dmvars)
	      (setscale)
	 )
	(if (= st "D") (dmvars)
	    (if (= st "S") (setscale)
	    )
	)
   )
(setvar "CMDECHO" ce)
(princ)
)
(defun mid() (setq os (getvar "osmode"))
	     (setvar "osmode" 512)
	     (setq p1 (getpoint "\nPick first point on line:"))
	     (setvar "osmode" 128)
	     (setq p2 (getpoint "\nPick second point on line:"))
	     (setq ang (angle p1 p2))
	     (setq d (/ (distance p1 p2) 2.0))(setvar "osmode" os)
	     (polar p1 ang d)             
)(defun c:oo()
	 (command "offset" pause pause pause "")
	 (command "chPROP" "l" "" "layer")
)(defun c:idp() (setvar "pdmode" 1)
  (setq a (getdist "\nHeight of text:"))
  (setq b (getpoint"\nENTER POINT:"))
  (setq r1  (strcat " X=" (rtos (car b))))
  (setq r2 (strcat " Y=" (rtos (cadr b))))
  (setq r3 (strcat " Z=" (rtos (caddr b))))
  (while b       
   (setq l (getpoint b "\nPick point for leader length:"))
   (setq ang1 (angle b l))
   (setq lenx (strlen r1)
	 leny (strlen r2)
	 leny (strlen r3) 
   )
   (setq r2a (strcat "%%o" r2))
   (command "dim" "lea" b l ^c ^c)
   (command "point" b)
   (command "text" (setq d (polar l (/ pi 2.0)
	      (/ a 2))) a 0 r1) 
   (command "text" (polar d (/ (* 3 pi) 2.0)  (* 1.70
	      a))  a 0 r2a) 
   
   (setq r3a (strcat "%%o" r3))
   (command "dim" "lea" b l ^c ^c)
   (command "point" b)
   (command "text" (setq d (polar l (/ pi 2.0)
	      (/ a 2))) a 0 r1) 
   (command "text" (polar d (/ (* 3 pi) 2.0)  (* 3.40
	      a))  a 0 r3a) 
   
   (setq b (getpoint "\nEnter next point :")) 
   (setq r1 (strcat " X=" (rtos (car b))))
   (setq r2 (strcat " Y=" (rtos (cadr b))))
   (setq r3 (strcat " Z=" (rtos (caddr b))))
  )
   (princ) 
)(Defun c:wall()
   (setq layc (getvar "clayer"))
   (setq dis (getdist "\nEnter wall thichness:"))
   (setq lay (getstring "\nEnter layer name for wall:"))
   (setq dist (/ dis 2.0))
   (prompt "\nSelect lines for wall:")
   (setq ss (ssget))
   (setq n (sslength ss))
   (setq i 0)
   (command "layer" "s" lay "")
   (repeat n (setq p1 (cdr (assoc 10 (entget (ssname ss i)))))
	     (setq p2 (cdr (assoc 11 (entget (ssname ss i)))))
	     (setq m1 (polar p1 (+ (/ pi 2.0) (angle p1 p2)) dist))
	     (setq n1 (polar m1 (angle p1 p2) (distance p1 p2)))
	     (setq m2 (polar p1 (- (angle p1 p2) (/ pi 2.0))
	     dist))
	      (setq n2 (polar m2 (angle p1 p2) (distance p1
	     p2))) (command "line" m1 n1 "") (command "line" m2
	     n2 "") (setq i (1+ i)) )
   (princ)
   )(princ)
