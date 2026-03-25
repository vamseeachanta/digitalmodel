               
(princ "\n*...****BISMILLAH**** LA-EE-LAHA IL-LALLA MOHAMMADUR RASUL-LALLA...*")
                      (GRTEXT -1 "BISMILLAH...")
;;;           Copyright (C) 1997-2004 by PERVEZ
;;;                      (For AutoCAD r2004)
;---------------------------------------------------------------------------------------------------------------!!!!
                                mohd altaf9.LSP
;                        Developed by mohd altaf
;---------------------------------------------------------------------------------------------------------------!!!!
;(vmon)
(defun dtr (a)
  (* (/ a 180) pi))
(defun rtd (a)
  (/ (* a 180) pi))

;ORDINARY LSP
*********
 
 (DEFUN C:reco()(COMMAND "recover" ))
 (DEFUN C:sd()(COMMAND "spline" )) 
 (DEFUN C:DG()(COMMAND "DIM" "GAP"))
 (DEFUN C:DF()(COMMAND "DIM" "STYLE"))
 (DEFUN C:EH()(COMMAND "_hatchedit"))
 (DEFUN C:pp()(COMMAND "_pasteclip")) 
 (DEFUN C:cp()(COMMAND "_copyclip"))
 (DEFUN C:F0()(COMMAND "FILLET" "R" "0" "fillet"))
 (DEFUN C:F1()(COMMAND "FILLET" "R" "1" "fillet"))
 (DEFUN C:F2()(COMMAND "FILLET" "R" "2" "fillet"))
 (DEFUN C:F3()(COMMAND "FILLET" "R" "3" "fillet"))
 (DEFUN C:F4()(COMMAND "FILLET" "R" "4" "fillet")) 
 (DEFUN C:F5()(COMMAND "FILLET" "R" "5" "fillet"))
 (DEFUN C:F6()(COMMAND "FILLET" "R" "6" "fillet"))
 (DEFUN C:F7()(COMMAND "FILLET" "R" "7" "fillet"))
 (DEFUN C:F8()(COMMAND "FILLET" "R" "8" "fillet"))
 (DEFUN C:F9()(COMMAND "FILLET" "R" "9" "fillet"))
 (DEFUN C:F10()(COMMAND "FILLET" "R" "10" "fillet"))
 (DEFUN C:F11()(COMMAND "FILLET" "R" "11" "fillet"))
 (DEFUN C:F12()(COMMAND "FILLET" "R" "12" "fillet"))
 (DEFUN C:F13()(COMMAND "FILLET" "R" "13" "fillet"))
 (DEFUN C:F14()(COMMAND "FILLET" "R" "14" "fillet"))
 (DEFUN C:F15()(COMMAND "FILLET" "R" "15" "fillet"))
 (DEFUN C:F16()(COMMAND "FILLET" "R" "16" "fillet"))
 (DEFUN C:F17()(COMMAND "FILLET" "R" "17" "fillet"))
 (DEFUN C:F18()(COMMAND "FILLET" "R" "18" "fillet"))
 (DEFUN C:F19()(COMMAND "FILLET" "R" "19" "fillet"))
 (DEFUN C:F20()(COMMAND "FILLET" "R" "20" "fillet"))
 (DEFUN C:F21()(COMMAND "FILLET" "R" "21" "fillet"))
 (DEFUN C:F22()(COMMAND "FILLET" "R" "22" "fillet"))
 (DEFUN C:F23()(COMMAND "FILLET" "R" "23" "fillet"))
 (DEFUN C:F24()(COMMAND "FILLET" "R" "24" "fillet"))
 (DEFUN C:F25()(COMMAND "FILLET" "R" "25" "fillet"))
 (DEFUN C:F26()(COMMAND "FILLET" "R" "26" "fillet"))
 (DEFUN C:F27()(COMMAND "FILLET" "R" "27" "fillet"))
 (DEFUN C:F28()(COMMAND "FILLET" "R" "28" "fillet"))
 (DEFUN C:F29()(COMMAND "FILLET" "R" "29" "fillet"))
 (DEFUN C:F30()(COMMAND "FILLET" "R" "30" "fillet"))
 (DEFUN C:F31()(COMMAND "FILLET" "R" "31" "fillet"))
 (DEFUN C:F32()(COMMAND "FILLET" "R" "32" "fillet"))
(DEFUN C:F33()(COMMAND "FILLET" "R" "33" "fillet"))
(DEFUN C:F34()(COMMAND "FILLET" "R" "34" "fillet"))
(DEFUN C:F35()(COMMAND "FILLET" "R" "35" "fillet"))
(DEFUN C:F36()(COMMAND "FILLET" "R" "36" "fillet"))
(DEFUN C:F37()(COMMAND "FILLET" "R" "37" "fillet"))
(DEFUN C:F38()(COMMAND "FILLET" "R" "38" "fillet"))
(DEFUN C:F39()(COMMAND "FILLET" "R" "39" "fillet"))
(DEFUN C:F40()(COMMAND "FILLET" "R" "40" "fillet"))
(DEFUN C:F41()(COMMAND "FILLET" "R" "41" "fillet"))
(DEFUN C:F42()(COMMAND "FILLET" "R" "42" "fillet"))
(DEFUN C:F43()(COMMAND "FILLET" "R" "43" "fillet"))
(DEFUN C:F44()(COMMAND "FILLET" "R" "44" "fillet"))
(DEFUN C:F45()(COMMAND "FILLET" "R" "45" "fillet"))
(DEFUN C:F46()(COMMAND "FILLET" "R" "46" "fillet"))
(DEFUN C:F47()(COMMAND "FILLET" "R" "47" "fillet"))
(DEFUN C:F48()(COMMAND "FILLET" "R" "48" "fillet"))
(DEFUN C:F49()(COMMAND "FILLET" "R" "49" "fillet"))
(DEFUN C:F50()(COMMAND "FILLET" "R" "50" "fillet"))
(DEFUN C:F51()(COMMAND "FILLET" "R" "51" "fillet"))
(DEFUN C:F52()(COMMAND "FILLET" "R" "52" "fillet"))
(DEFUN C:F53()(COMMAND "FILLET" "R" "53" "fillet"))
(DEFUN C:F54()(COMMAND "FILLET" "R" "54" "fillet"))
(DEFUN C:F55()(COMMAND "FILLET" "R" "55" "fillet"))
(DEFUN C:F56()(COMMAND "FILLET" "R" "56" "fillet"))
(DEFUN C:F57()(COMMAND "FILLET" "R" "57" "fillet"))
(DEFUN C:F58()(COMMAND "FILLET" "R" "58" "fillet"))
(DEFUN C:F59()(COMMAND "FILLET" "R" "59" "fillet"))
(DEFUN C:F60()(COMMAND "FILLET" "R" "60" "fillet"))
(DEFUN C:F61()(COMMAND "FILLET" "R" "61" "fillet"))
(DEFUN C:F62()(COMMAND "FILLET" "R" "62" "fillet"))
(DEFUN C:F63()(COMMAND "FILLET" "R" "63" "fillet"))
(DEFUN C:F64()(COMMAND "FILLET" "R" "64" "fillet"))
(DEFUN C:F65()(COMMAND "FILLET" "R" "65" "fillet"))
(DEFUN C:F70()(COMMAND "FILLET" "R" "70" "fillet"))
(DEFUN C:F80()(COMMAND "FILLET" "R" "80" "fillet"))
(DEFUN C:F90()(COMMAND "FILLET" "R" "90" "fillet"))
(DEFUN C:F100()(COMMAND "FILLET" "R" "100" "fillet"))
(DEFUN C:F125()(COMMAND "FILLET" "R" "125" "fillet"))
 (DEFUN C:L0 () (COMMAND "'LUPREC" "0"))
 (DEFUN C:L2 () (COMMAND "'LUPREC" 2))
 (DEFUN C:A0 () (COMMAND "'AUPREC" "0"))
 (DEFUN C:A2 () (COMMAND "'AUPREC" "2"))
 (DEFUN C:OSF () (COMMAND "'OSNAP" "NONE" ))
 (DEFUN C:OP () (COMMAND "'OSNAP" "END,INT,CEN,MID,QUA,PER,NODE" ))
 (DEFUN C:RS () (COMMAND "STYLE"  "romans" "romans" "" "" "" "" "" "")) 
 (DEFUN C:RD () (COMMAND "STYLE"  "romand" "romand" "" "" "" "" "" ""))
 (DEFUN C:Ret ()(COMMAND "STYLE"  "romant" "romant" "" "" "" "" "" ""))
 (DEFUN C:Rc () (COMMAND "STYLE"  "romanc" "romanc" "" "" "" "" "" ""))
 (defun c:ds()(setvar "cmdecho" 0)(command "dimscale"))
 (defun c:lts()(command "ltscale"))
 (defun c:a()(command "array"))
 (defun c:ar()(command "arc"))
 (defun c:tR()(command "trim"))
 (defun c:pt()(command "_plot"))
 (defun c:q()(command "quit" "n"))
 (defun c:AU()(command "AUDIT" "Y" "QSAVE")(princ)) 
 (defun c:up()(command "dim1" "up"))
 (defun c:co()(command "dim1" "co"))
 (defun c:ddl()(command "ddlmodes"))
 (defun c:os()(command "ddosnap"))
 (defun c:dI()(command "dist"))
 (defun c:da()(command "ddatte"))
 (defun c:dIV()(command "DIVIDE")) 
 (defun c:sa()(command "qsave"))
 (defun c:Dal () (command "dim" "ali"))
 (defun c:dh () (command "dim" "hor"))
 (defun c:dv () (command "dim" "ver"))
 (defun c:dN () (command "dim" "ang"))
 (defun c:dl () (command "dim" "lea"))
 (defun c:de () (command "dim" "te")) 
 (defun c:dr()(command "dim1" "res"))
 (defun c:ho()(command "dim1" "hom"))
 (defun c:n()(command "dim1" "n"))
 (defun c:z3()(command "zoom" ".3x"))
 (defun c:z5()(command "zoom" ".5x"))
 (defun c:zR()(command "zoom" "R"))
 (defun c:zx() (command "zoom" "p"))
 (defun c:pd()(command "pedit"))
 (defun c:pu()(command "purge" "all" "" "n" "QSAVE")(princ))
 (defun c:bh()(command "bhatch"))
 (defun c:e()(command "erase"))
 (defun c:r()(Command "rotate"))
 (defun c:ye()(command "ucs" "e"))
 (defun c:yw()(command "ucs" "w"))
 (DEFUN C:X  () (COMMAND "EXPLODE" ))
 (DEFUN C:SA  () (COMMAND "QSAVE"))
 (DEFUN C:SV  () (COMMAND "SAVEAS"))
 (defun c:vr  () (command "viewres" "y" "20000"))
 (DEFUN C:Z   () (COMMAND "ZOOM"))
 (DEFUN C:mt1   () (COMMAND "mirrtext" "1"))
 (DEFUN C:mt0   () (COMMAND "mirrtext" "0"))
 (defun c:da  () (command "ddatte"))
 (DEFUN C:ZA  () (COMMAND "ZOOM" "A"))
 (DEFUN C:ZC  () (COMMAND "ZOOM" "C"))
 (DEFUN C:ZD  () (COMMAND "ZOOM" "D"))
 (DEFUN C:ZE  () (COMMAND "ZOOM" "E"))
 (DEFUN C:ZL  () (COMMAND "ZOOM" "L"))
 (DEFUN C:ZX  () (COMMAND "ZOOM" "P"))
 (DEFUN C:ZW  () (COMMAND "ZOOM" "W"))
 (DEFUN C:Y   () (COMMAND "UCS" "AUTO"))
 (DEFUN C:OO  () (COMMAND "OOPS"))
 (DEFUN C:E   () (COMMAND "ERASE"))
 (DEFUN C:c   () (COMMAND "copy"))
 (DEFUN C:cC   () (COMMAND "CIRCLE"))
 (defun c:bf()(command "break" pause "f" pause "@" ""))
 (defun c:eli()(command "erase" "l"))
 (defun c:res()(command "dim1" "res"))
 (defun c:v()(command "view" "r" "bl"))
 (defun c:vw()(command "view" "w"))
 (defun c:rdd()(command "redraw"))
 (defun c:CAL()(command "sh" "calc"))
 
(DEFUN C:T25 ()
(SETVAR "CMDECHO" 0)
(SETQ SC (GETVAR "DIMSCALE"))
(COMMAND "LAYER" "S" "revision" "")
(SETQ HT (* SC 2.5))
(SETQ SP (GETPOINT "\n * Start Point Of Text ==> "))(princ)
(setvar "textstyle" "romans")
(COMMAND "DTEXT" SP HT)
)

(DEFUN C:T35 ()
(SETVAR "CMDECHO" 0)
(SETQ SC (GETVAR "DIMSCALE"))
(setq os (getvar "textstyle"))
(SETQ CL (GETVAR "CLAYER"))
(COMMAND "LAYER" "S" "revision" "")
(SETQ HT (* SC 3.5))
(SETQ SP (GETPOINT "\n * Start Point Of Text ==> "))(princ)
(setvar "textstyle" "romand")  
(COMMAND "DTEXT" SP HT pause)
(setvar "textstyle" os)(princ)
)
======================================================


**************************************************************** 
 ;MULTIPLE TEXT EDIT.
 (DEFUN C:11()(princ "\n ** Multi Text Change Program by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
 
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (COMMAND "DDEDIT" ee1 "")
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

********************************************************************
;CHANGE PROP.LISP
 (defun c:22()(princ "\n ** Change Entity Prop. Program by GANESH AGARE.")
 (setvar "cmdecho" 0)
 (setq ss (ssget))
 (setq nt (entsel "\n * Pick The Object To Adpt. "))
 (setq snt (car nt))
 (setq ntx (entget snt))
 (setq llt (cdr (assoc 6 ntx)))
 (setq lla (cdr (assoc 8 ntx)))
 (setq lc (cdr (assoc 62 ntx)))
   (if (= llt nil)
    (setq llt "bylayer")
   )
 
   (if (= lc nil)
    (setq lc "bylayer")
   )
 (command "chprop" ss "" "la" lla "lt" llt "c" lc "")
 (PRINC)
 )

 
********************************************************************** 
;CHANGE TEXT HEIGHT IN ONE COMMAND
 (Defun C:33 () (princ "\n***  program by GANESH AGARE ....") 
  (Setvar "Cmdecho" 0) 
  (Initget 1 "S H  ")
  (Setq G (Strcase (Substr (Getkword "Height/<Select>: ")1 1)))
  (Cond ((Or (= G "S") (= G ""))
	 (Setq A (Ssget))
	 (Setq B (Sslength A))
	 (Initget 7)
	 (Setq C (Getreal "\nEnter new text size: "))
	 (While (> B 0)
	     (Setq B (1- B))
	     (Setq D (Ssname A B))
	     (Setq D (Entget D))
	     (Setq E (Assoc 40 D))
	     (Setq F (cons 40 C))
	     (Entmod (Setq D (Subst F E D)))
	 )
	 (Setq A nil)
       )
       ((= G "H")
	(Initget 7)
	(Setq A (Getreal "\nEnter text size to change: ")) 
	(Setq B (Fix (* 100 A)))
	(Initget 7)
	(Setq C (Getreal "\nEnter new text height: "))
	(Setq D (Entnext)) 
	(Setq E (Cons 40 C))
	(While D (Princ ".")
	    (Setq F (Assoc 0 (Entget D)))
	    (If (= "TEXT" (Cdr F)) (Progn
		(Setq F (Assoc 40 (Entget D))) 
		(Setq G (Fix (* 100 (Cdr F))))
		(If (= B G) (Entmod (Subst E F (Entget D))))
	    ))
	    (Setq D (Entnext D))
	)
      )
  ) 
    (Princ)
)
****************************************************************
;MATCH PROP.FOR TEXT
(defun C:44 () (princ "\n***  program  by GANESH AGARE ....")
(chgtext nil))
(defun chgtext (objs / last_o tot_o ent o_str n_str st s_temp 
                       n_slen o_slen si chf chm cont ans)
  (if (null objs)
    (setq objs (ssget))               ; Select objects if running standalone
  )
  (setq chm 0)
  (if objs 
    (progn                   ; If any objects selected
      (if (= (type objs) 'ENAME) 
        (progn
          (setq ent (entget objs))
          (princ (strcat "\nExisting string: " (cdr (assoc 1 ent))))
        )
        (if (= (sslength objs) 1)
          (progn
            (setq ent (entget (ssname objs 0)))
            (princ (strcat "\nExisting string: " (cdr (assoc 1 ent))))
          )
        )
      )
      (setq o_str (getstring "\nMatch string   : " t))
      (setq o_slen (strlen o_str))
      (if (/= o_slen 0)
        (progn
          (setq n_str (getstring "\nNew string     : " t))
          (setq n_slen (strlen n_str))
          (setq last_o 0 
                tot_o  (if (= (type objs) 'ENAME)
                         1
                         (sslength objs)
                       )
          )
          (while (< last_o tot_o)     ; For each selected object...
            (if (= "TEXT"             ; Look for TEXT entity type (group 0)
                   (cdr (assoc 0 (setq ent (entget (ssname objs last_o))))))
              (progn
                (setq chf nil si 1)
                (setq s_temp (cdr (assoc 1 ent)))
                (while (= o_slen (strlen (setq st (substr s_temp si o_slen))))
                  (if (= st o_str)
                    (progn
                      (setq s_temp (strcat 
                                     (if (> si 1)
                                       (substr s_temp 1 (1- si)) 
                                       ""
                                     )
                                     n_str
                                     (substr s_temp (+ si o_slen))
                                   )
                      )
                      (setq chf t)    ; Found old string
                      (setq si (+ si n_slen))
                    )
                    (setq si (1+ si))
                  )
                )
                (if chf 
                  (progn              ; Substitute new string for old
                    ; Modify the TEXT entity
                    (entmod (subst (cons 1 s_temp) (assoc 1 ent) ent))
                    (setq chm (1+ chm))
                  )
                )
              )
            )
            (setq last_o (1+ last_o))
          )
        )
        ;; else go on to the next line...
      )
    )
  )
  (if (/= (type objs) 'ENAME)
    (if (/= (sslength objs) 1)        ; Print total lines changed
      (princ (strcat "Changed "
                     (rtos chm 2 0)
                     " text lines."
             )
      )
    )
  )
  (terpri)
)

(defun myerror (msg)
(if (= msg "Function cancelled")
 (progn
  (command "undo" "end")
  (setvar "clayer" *cly)
  (mapcar 'setvar *mo *gv)
  (princ "\nCloud teriminated.....")
 )
)
  (princ)
)

*********************************************************************
;GET TOTAL .LISP
 (DEFUN C:55 ()(princ "\n*** Program by GANESH AGARE...")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
     (setq rval 0)
      (WHILE (< CTR SSl)
       (setq ls1 (ssname SS1 ctr))
       (setq eg (entget ls1))
       (setq val (cdr (assoc 1 eg)))
       (setq str (atof val))
       (setq rval (+ str rval))
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (setq nstr (rtos rval))
 (setq astr (strcat "Total Value = " nstr))
 ;;;(PRINT STR )
 (alert astr)
 (PRINC)
 )
**********************************************************************
GET TOTAL WEIGHT IN KG & ADDS THE TEXT.
 (DEFUN C:66()(setvar "cmdecho" 0)
        (setq temp (getvar "textstyle"))
        (princ "\n*** Program by GANESH AGARE...")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
     (setq rval 0)
      (WHILE (< CTR SSl)
       (setq ls1 (ssname SS1 ctr))
       (setq eg (entget ls1))
       (setq val (cdr (assoc 1 eg)))
       (setq str (atof val))
       (setq rval (+ str rval))
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (setq nstr (rtos rval))
 (setq astr (strcat "TOTAL WT. = " nstr " Kgs."))
 (setq p1 (getpoint "\nLocate text...."))
 (command "text" p1 "" "" astr)
 (setvar "textstyle" temp)(princ))
*****************************************************************
GET TOTAL LENGHT IN MM & ADDS THE TEXT.

(DEFUN C:77()(setvar "cmdecho" 0)
        (setq temp (getvar "textstyle"))
        (princ "\n*** Program by GANESH AGARE...")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
     (setq rval 0)
      (WHILE (< CTR SSl)
       (setq ls1 (ssname SS1 ctr))
       (setq eg (entget ls1))
       (setq val (cdr (assoc 1 eg)))
       (setq str (atof val))
       (setq rval (+ str rval))
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (setq nstr (rtos rval))
 (setq astr (strcat "TOTAL LENGHT. = " nstr " MM."))
 (setq p1 (getpoint "\nLocate text...."))
 (command "text" p1 "" "" astr)
 (setvar "textstyle" temp)(princ))
*********************************************************************       
;SPECIAL LISPS
****************
(defun C:MODI()
(setvar "cmdecho" 0)
(setq olds(getREAL "\nPlease Enter Old Scale......:"))
(setq news(getREAL "\nPlease Enter New Scale......:"))
(setq scm(ssget))
(setq pole(getpoint "\nPlease Enter Base Point......:"))
(setq txsize (getdist "\nEnter New Text Height.....:"))
(PROMPT "<<<----PLEASE WAIT...PROCESSING..---->>>")
(SETQ pst(/ olds news))
(COMMAND "SCALE" scm "" pole pst)
(blmod)
(tmod)
(COMMAND "REDRAW")
(SETQ MODI NIL)
)
(defun tmod()
        (setq n (sslength scm))
        (setq index 0)
   (repeat n
      (setq ent (entget (ssname scm index)))
      (setq type (assoc 0 ent))
      (if (= "TEXT" (cdr type))
              (progn
              (setq oldsize (assoc 40 ent))
              (setq newsize (cons (car oldsize) txsize))
              (setq ent1 (subst newsize oldsize ent))
              (entmod ent1)
              )
      )
       (setq index (+ 1 index))
   )
(terpri)
)
(defun blmod()
(SETQ X1(/ news olds))
(setq blk(ssget "p"))
(setq num(sslength blk))
         (setq ind 0)
 (repeat num
    (setq mat (entget (ssname blk ind)))
     (SETQ A1(SSNAME BLK IND))
         (setq IP(assoc 10 mat))
         (SETQ IP1(LIST (CADR IP) (CADDR IP)))
         (setq wht(assoc 0 mat))
         (setq wt(assoc 2 mat))
        (if (AND (= "INSERT" (cdr wht))
                (OR
                (= "GR" (cdr wt))
                (= "GRA" (cdr wt))
                (= "TICK" (cdr wt))
                (= "GRB" (cdr wt))
                (= "GRC" (cdr wt))
                (= "S" (cdr wt))
                (= "BRKL" (cdr wt))
                (= "SUR" (cdr wt))
                (= "SUL" (cdr wt))
                (= "SRT" (cdr wt))
                (= "SRB" (cdr wt))
                (= "EL" (cdr wt))
                (= "ELP" (cdr wt))
                (= "ROCK" (cdr wt))
                (= "WAT" (cdr wt))
                (= "GL" (cdr wt))
            ))
            (COMMAND "SCALE" A1 "" IP1 X1)
        )
         (SETQ IND(+ 1 IND))
 )
)
************************************************************************************


(cond
   (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.

   (  (not (findfile "ai_utils.lsp"))                     ; find it
      (ai_abort "DDCHPROP"
         (strcat "Can't locate file AI_UTILS.LSP."
   "\n Check support directory.")))

   (  (eq "failed" (load "ai_utils" "failed"))            ; load it
   (ai_abort "DDCHPROP" "Can't load file AI_UTILS.LSP"))
);end conditional

;(if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
;   (ai_abort "DDCHPROP" nil)        ; a Nil <msg> supresses
;)                                    ; ai_abort's alert box dialog.




;    metering prompt
(defun gc_meter (gcm_pr gc_num gc_max)
   (prompt 
      (strcat "\r" gcm_pr " ("
         (rtos(*(/(float(1+ gc_num))(float gc_max))100)2 0)
         "%)"
      ) ;strcat
   ) ;prompt   
) ;defun

(defun C:FG
   (/ mode replace_loc _accept _replace text text_string
      index pointer sset id text_string_length *olderror*
   )
(init_bonus_error 
       (list
         (list "cmdecho" 0
         )
          T     ;flag. True means use undo for error clean up.  
       );list  
);init_bonus_error

   ;process an ok
   (defun _accept ()
      (cond  
         (
            (or 
               (= 
                  (get_tile "find")
                  ""
               )
              ; (= 
              ;    (get_tile "replace")
              ;    ""
              ; )
            )  
            (set_tile "error" "Empty or invalid input")
         )
         (
            (=
               (get_tile "find")
               (get_tile "replace")
            )
            (set_tile "error" "Find and replace are identical")
         )    
         (T
            (setq #find_string (get_tile "find"))
            (setq #replace_string (get_tile "replace"))
            (setq #case_sensitive (get_tile "case"))
            (setq #global (get_tile "global"))
            (done_dialog 1)
         )   
      )  
   )

   ;function for prompt to replace
   (defun _replace ()
      (new_dialog "find2" id "" replace_loc)
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "accept" "(setq replace_loc (done_dialog 1))")
      (action_tile "skip"   "(setq replace_loc (done_dialog 0))")
      (action_tile "auto"   "(done_dialog 2)")
      (set_tile
         "error" 
         (strcat 
            (rtos (1+ index) 2 0)
            " of "
            (rtos (sslength sset) 2 0)
         )
      )
      (start_dialog)
   )  

   ;set up the dialog identification
   (setq id (load_dialog "find"))

   ;open dialog and store location as a global
   (new_dialog "find" id)

   ;set those defaults
   (if
      #find_string
      (set_tile "find" #find_string)
   )
   (if 
      #replace_string
      (set_tile "replace" #replace_string)
   )
   (if
      #case_sensitive
      (set_tile "case" #case_sensitive)
   )
   (if
      #global
      (set_tile "global" #global)
   )

   ;set up callbacks
   (action_tile "accept"  "(_accept)")
   (mode_tile "find" 2)

   ;process the look for the callbacks
   (if
      ;make changes if ok is picked
      (=
         (start_dialog)
         1
      )
      (progn
         ;get a selection set if not global
         (if (/= #global "1")
            (while
               (=
                  (setq sset (ssget (list (cons 0 "TEXT"))))
                  nil
               )
               (prompt "\nNo entities selected")
            )
            (setq sset (ssget "X"(list(cons 0 "TEXT"))))
         )
         (setq index 0)
         ;go through the selection set
         (while (and sset
            (/=
               (setq text (ssname sset index))
            nil))
            ;go through each string
            (setq pointer 1)
            (setq text_string (cdr (assoc 1 (entget text))))
            (if 
               (<=
                  (strlen #find_string)
                  (strlen text_string)
               )
               (progn
                  (setq text_string_length (strlen text_string))
                  ;go until you reach the end of the string
                  (while
                     (< 
                        pointer
                        (+ (- text_string_length (strlen #find_string)) 2)
                     )
                     (if 
                        (= 
                           (if
                              (= #case_sensitive "1")
                              (substr
                                 text_string
                                 pointer
                                 (strlen #find_string)
                              )
                              (strcase 
                                 (substr
                                    text_string 
                                    pointer 
                                    (strlen #find_string)
                                 )
                              )
                           )
                           (if (= #case_sensitive "1")
                              #find_string
                              (strcase #find_string)
                           )
                        )
                        (progn
                           (redraw text 3)
                           (if (/= mode 2)
                              (if (/= #global "1")
                                 (setq mode (_replace))
                                 (setq mode 2)
                              )
                           )   
                           (if (> mode 0)
                              (progn
                                 (setq text_string
                                    (if (= pointer 1)
                                       (strcat
                                          #replace_string
                                          (substr
                                             text_string
                                             (+ pointer (strlen #find_string))
                                          )
                                       )
                                       (strcat 
                                          (substr 
                                             text_string
                                             1
                                             (- pointer 1)
                                          )
                                          #replace_string
                                          (substr
                                             text_string
                                             (+ pointer (strlen #find_string))
                                          )
                                       )
                                    )
                                 )
                                 (entmod 
                                    (subst 
                                       (cons 1 text_string)
                                       (assoc 1 (entget text))
                                       (entget text)
                                    )
                                 )
                                 (setq 
                                    pointer 
                                    (+ pointer (strlen #replace_string))
                                 )
                                 (setq
                                    text_string_length
                                    (strlen text_string)
                                 )
                              )
                           )
                           (redraw text 4)
                           (setq pointer (1+ pointer))
                        )
                        (setq pointer (1+ pointer))
                     )
                  )
               )
            )
            (gc_meter "Changing text" index (sslength sset))
            (setq index (1+ index))
         )
      )
   )
   (unload_dialog id)
(restore_old_error)
   (princ)
)  

(defun c:findhelp (/)
(prompt "The find command preforms a find and replace on Dtext strings.\n")
(prompt "This version does not support Mtext.\n")
(prompt "Usage is through the Dialog box, note the options! \n")
(prompt "User can preform Selected or Global replacement.\n")
(prompt "Case is supported, check option in Dialog.\n")
(prompt "If the Dialog fails to appear, check your Support path.\n")
(prompt "Files used: Find.lsp & Find.dcl\n")
(prompt "Type FIND to begin command.\n")
(textscr)
(princ)
)
****************************************************************************8
 ; TO GIVE THE DESIRED THICKNESS TO THE SELECTED POLYLINES
 (defun c:wd()
         (prompt "\nSelect the polylines ..Program by GANESH AGARE.")
         (setq set (ssget)
                 len (sslength set)
                 cou 0
                 nwd (getreal "\nEnter new Width for all segments ..."))
         (repeat len
                 (setq tnam (ssname set cou))
                 (command "pedit" tnam "w" nwd "")
                 (setq cou (+ cou 1))
                 (entupd tnam))
         (princ))
-------------------------------------------------------------------
(defun c:LN()(princ "\n ** Line Program by GANESH AGARE")
 (setq fp nil)
 (setq fp (getpoint "\n * Point For Line: "))
 (if (not(= fp nil))
   (progn
 (setq an 0)
   (while (not(= an nil))
     (pro)
     (if (not(= an nil))
       (progn
 (setq dt (getdist "\n * Enter distance Or Second Point: "))
   (if (not(= dt nil))
     (progn
       (setq npt (polar fp an dt))
       (command "line" fp npt "")
       (setq fp (getvar "lastpoint"))
     )
     (setq an nil)  
  )
       )
     ) 
   )
  )
 )
 (PRINC)
 )
------------------------------------------------------------------
CONVERTS MM INTO INCHES & INCHES INTO MM.
****************************************
(defun c:c1()(princ "\nProgram to convert INCH'S into MM by GANESH AGARE***")
       (setq p1 (getreal "\nEnter value in Inch...."))
       (setq p2 (* p1 25.4 ))                                 
       (setq p3 (rtos p2 ))
       (princ (strcat "The answer is  " p3  "mm"))(princ))

 (defun c:c2()(princ "\nProgram to convert MM'S into FEET by GANESH AGARE***")
       (setq p1 (getreal "\nEnter value in MM...."))
       (setq p2 (* p1 0.039370078))                                 
       (setq p3 (rtos p2 ))
       (princ (strcat "The answer is  " p3 "inc"))(princ))

(defun c:c3()(princ "\nProgram to convert INCH'S into FEET by GANESH AGARE***")
       (setq p1 (getreal "\nEnter value in INCH...."))
       (setq p2 (* p1 0.083333333))                                 
       (setq p3 (rtos p2 ))
       (princ (strcat "The answer is  " p3 "..FEET.."))(princ))

 
---------------------------------------------------------------------
;PUT BRACKET ON TEXT
***********************
(defun c:brc()(setvar "cmdecho" 0)
        (princ "\nAdd bracket to Text, program by GANESH AGARE")
        (setq p1 (car (entsel "\n *** Select a text for Bracket...")))
        (setq p2 (entget p1))
        (setq p3 (cdr (assoc 1 p2)))
        (setq p4 "(")
        (setq p5 ")")
        (setq ans (strcat p4 p3 p5))
        (command "change" p1 "" "" "" "" "" "" ans))
---------------------------------------------------------------------                                            
; UNDERLINES THE TEXT 
*********************
 (defun c:Ul()(princ "\n Program by GANESH AGARE")
  (prompt "\nSelect the text entities ...")
  (setq set (ssget)
   len (sslength set)
   cou 0
  )
  (repeat len
   (setq enam (ssname set cou)
    eget (entget enam)
    etyp (cdr (assoc 0 eget))
   )
   (if (= etyp "TEXT")
    (progn
     (setq ostr (cdr (assoc 1 eget))
      nstr (strcat "%%u" ostr)
     )
     (setq eget (subst (cons 1 nstr) (cons 1 ostr) eget))
     (entmod eget)
     (entupd enam)
    )
   )
   (setq cou (1+ cou))
  )
  (princ)
 )
-----------------------------------------------------------------------------
(defun c:rUl()(princ "\n Program by GANESH AGARE")
  (prompt "\nSelect the text entities ...")
  (setq set (ssget)
   len (sslength set)
   cou 0
  )
  (repeat len
   (setq enam (ssname set cou)
    eget (entget enam)
    etyp (cdr (assoc 0 eget))
   )
   (if (= etyp "TEXT")
    (progn
     (setq ostr (cdr (assoc 1 eget))
      nstr (strcat "" ostr)
     )
     (setq eget (subst (cons 1 nstr) (cons 1 ostr) eget))
     (entmod eget)
     (entupd enam)
    )
   )
   (setq cou (1+ cou))
  )
  (princ)
 )

-----------------------------------------------------------------
(defun c:dwg()(setq dw1 (getvar "dwgname"))
        (setq dw2 (substr dw1 17 8))
        (grtext 24 dw2))
 (defun c:hyp()
 (princ "\n*** Program by GANESH AGARE")
        (setq en1 (getreal "\nLength of first side: "))
        (setq en2 (getreal "\nLength of second side: "))
        (setq ans (sqrt (+ (* en1 en1) (* en2 en2))))
        (setq ans1 (rtos ans))
        (princ (strcat "The Hypotenuse is " ans1))
        (princ))
 
 (Defun c:lr()(setvar "cmdecho" 0)
       (setq se1 (car (entsel "\n ***Select Line to Rotate..."))
      get1 (entget se1)
      fp (cdr (assoc 10 get1))
      sp (cdr (assoc 11 get1))
      fa (angle fp sp)
      fa1 (angtos fa 0 5)
      se2 (car (entsel "\n ***Select Line for Ref. angle..."))      
      get2 (entget se2)
      fp1 (cdr (assoc 10 get2))
      sp1 (cdr (assoc 11 get2))
      sa (angle fp1 sp1)
      sa1 (angtos sa 0 5))
      (command "rotate" se1 "" fp "r" fa1 sa1))
---------------------------------------------------------------
ROTATES TEXT IN ANY ANGLE.
************************** 
 (defun c:trt()
 (princ "\n*** Program by GANESH AGARE.")
  (setq tnam (car (entsel "\nSelect the text to change ..."))
   tget (entget tnam)
   tang (assoc 50 tget)
   lnam (car (entsel "\n Select the line for orientation : "))
   lget (entget lnam)
   fp (cdr (assoc 10 lget))
   sp (cdr (assoc 11 lget))
   lang (cons 50 (angle fp sp))
  )
  (setq tget (subst lang tang tget))
  (entmod tget)
  (entupd tnam)
  (princ)
 )
------------------------------------------------------------------
 ; TO CHANGE THE SELECTED TEXT STRINGS TO UPPER CASE
 
 (defun c:UC()(princ "\n*** Program by GANESH AGARE.")
  (prompt "\nSelect the text entities ...")
  (setq set (ssget)
   len (sslength set)
   cou 0
  )
  (repeat len
   (setq tnam (ssname set cou)
    tget (entget tnam)
    olds (cdr (assoc 1 tget))
    news (strcase olds)
    cou (1+ cou)
   )
   (setq tget (subst (cons 1 news) (cons 1 olds) tget))
   (entmod tget)
   (entupd tnam)
  )
  (princ)
 )
 
 ; TO CHANGE THE SELECTED TEXT STRINGS TO LOWER CASE
 (defun c:LC()(princ "\n*** Program by GANESH AGARE.")
  (prompt "\nSelect the text entities ...")
  (setq set (ssget)
   len (sslength set)
   cou 0
  )
  (repeat len
   (setq tnam (ssname set cou)
    tget (entget tnam)
    olds (cdr (assoc 1 tget))
    news (strcase olds t)
    cou (1+ cou)
   )
   (setq tget (subst (cons 1 news) (cons 1 olds) tget))
   (entmod tget)
   (entupd tnam)
  )
  (princ)
 )
------------------------------------------------------------------
; ADD TEXT
***********
 (defun c:BB()(princ "\n Program by GANESH AGARE")
  (prompt "\nSelect the text entities ...")
  (setq set (ssget)
   len (sslength set)
   cou 0
  )
  (repeat len
   (setq enam (ssname set cou)
    eget (entget enam)
    etyp (cdr (assoc 0 eget))
   )
   (if (= etyp "TEXT")
    (progn
     (setq ostr (cdr (assoc 1 eget))
      nstr (strcat "B" ostr)
     )
     (setq eget (subst (cons 1 nstr) (cons 1 ostr) eget))
     (entmod eget)
     (entupd enam)
    )
   )
   (setq cou (1+ cou))
  )
  (princ)
 )


 (defun c:ADS()(setq gets (getstring T "\nEnter String to join...."))
  (prompt "\nSelect the text entities ...")
  (setq set (ssget)
   len (sslength set)
   cou 0
  )
  (repeat len
   (setq enam (ssname set cou)
    eget (entget enam)
    etyp (cdr (assoc 0 eget))
   )
   (if (= etyp "TEXT")
    (progn
     (setq ostr (cdr (assoc 1 eget))
      nstr (strcat ostr gets)
     )
     (setq eget (subst (cons 1 nstr) (cons 1 ostr) eget))
     (entmod eget)
     (entupd enam)
    )
   )
   (setq cou (1+ cou))
  )
  (princ)
 )
----------------------------------------------------------------------
;MAKES TWO TEXT ENTITY IN ONE (IN ONE LINE).
 (defun c:M1()
     (setvar "cmdecho" 0)
     (SETVAR "HIGHLIGHT" 1)
     (princ "\n***Makes One Text***")
     (setq h  (entsel "\nSelect The First Text:")
    H1 (CAR H)
     )
     (redraw h1 3)
 (if h1
     (progn
  (setq h21 (entget h1))
  (setq TX1 (cdr (assoc 1 h21)))
     )
 )
 (setq A (entsel "\nSelect The Second Text:"))
 (SETQ H2 (CAR A))
 (if h2
     (progn
  (setq h22  (entget h2)
        tX2 (cdr (assoc 1 h22))
  )
     )
 )
    (setq txm (strcat " " tx2)
   TXA (STRCAT TX1 TXm)
    )
    (if (= (cdr (assoc 0 H21)) "TEXT")
        (progn
      (setq h21 (subst (cons 1 TXA) (assoc 1 h21) h21))
      (entmod h21)
        )
    )
 (setq sss  (car a))
 (entdel sss)
 (princ )
 )
----------------------------------------------------------------
;FINDS THE TEXT
*****************
 (defun c:FT()
  (setvar "cmdecho" 0)
  (if (= findscl nil)
  (setq findscl (getvar "dimscale"))
  (setq magni (* findscl 1))
  )
  (setq ss (ssget "x" (list (cons 0 "TEXT"))))
  (setq cnt 0 cnt1 0)
  (setq len (sslength ss))
   (if SS (progn
      (setq cont t)
      (while cont
  (setq st (getstring "\nEnter Text To Find:" t))
   (if (= st 0)
      (princ "Null input invalid")
      (setq cont nil)
   )
       )
     )
   )
     (while (< cnt len)
     (setq en (ssname ss cnt))
     (setq einfo (entget en))
     (setq txt (cdr (assoc 1 einfo)))
        (if (= txt st)
      (progn
     (setq a (cdr (assoc 10 einfo)))
     (setq b (car a))
     (setq c (car (cdr a)))
     (setq pt (list b c))
     (command "zoom" "c" pt magni)
     (command "ddedit" (ssname ss cnt) "")
     (setq cnt (+ 1 cnt))
      )
      (setq cnt (+ 1 cnt))
      )
    )
    (setvar "cmdecho" 1)
  )
-----------------------------------------------------------------
 (defun c:rect()
 (setq pt1 (GETPOINT "\nfirst corner: "))
 (setq l (GETDIST pt1 "\n length of rect: "))
 (setq pt2 (list (+ (car pt1) l) (cadr pt1) (caddr pt1)))
 (grdraw pt1 pt2 2)
 (setq w (GETDIST pt1 "\n width of rect: "))
 (setq pt3 (list (car pt2) (+ (cadr pt2) w) (caddr pt2)))
 (setq pt4 (list (car pt1) (+ (cadr pt1) w) (caddr pt1)))
 (grdraw pt2 pt3 2)
 (grdraw pt3 pt4 2)
 (grdraw pt4 pt1 2)
 (command "pline" pt1 pt2 pt3 pt4 "c")
 )
------------------------------------------------------------------
;CONVERTS THE TEXT IN ANY ANGLE
 (defun c:oba()
    (if (null  oba_ang_1)
     (setq oba_ang_1 (getreal "\nENTER OBLIQUE ANGLE : "))
     (progn
       (setq oba_ang_2 oba_ang_1)
       (setq oba_ang_1 (getreal (STRCAT "\nENTER OBLIQUE ANGLE < " (rtos oba_ang_2) " > : " )))
       (if (= oba_ang_1 nil)
    (setq oba_ang_1 oba_ang_2)
    (setq oba_ang_2 oba_ang_1)        
       )
    )
 )
    (setq oba_ang (* (/ pi 180) oba_ang_1)
   ss_get_set (ssget)
   ss_get_length (sslength ss_get_set)
   count_ssget 0
    )
    (while (< count_ssget ss_get_length)
        (setq ss_get_ent (ssname ss_get_set count_ssget)
       ss_get_entget (entget ss_get_ent)
        )
        (if (= (cdr (assoc 0 ss_get_entget)) "TEXT")
     (progn
        (setq ss_get_entget (subst (cons 51 oba_ang) (assoc 51 ss_get_entget)
        ss_get_entget
       )
        )
        (entmod ss_get_entget)
     )                                      ; end of progn
        )                                          ; end of if
        (setq count_ssget (+ count_ssget 1))
    )
    (princ)
 )
-----------------------------------------------------------
;SHOWS CURENT DATE
*********************
 (defun c:dat()
  (setq dda (rtos (getvar "cdate") 2 9)
   yer (substr dda 3 2)
   mon (substr dda 5 2)
   day (substr dda 7 2)
   wat (strcat day ":" mon ":" yer)
  )
  (princ " ... Today's date is --> ")
  (princ wat)
  (princ)
 )
-----------------------------------------------------------------------
;NO.LISP- CHANGES THE TEXT IN NUMBERS
********************************* 
(Defun c:no()
 (Setq No (Getstring "\nGive starting no. : "))
 (Setq A(ssget))
 (Setq count 0)
 (If (/= A nil)
 (While (< Count (sslength a))
 (Command "Change" (ssname a count) "" "" "" "" "" "" (Atoi no))
 (Setq No (Itoa (1+ (Atoi No))))
 (If (< (Strlen no) 2)
   (Setq No (Strcat "0" No))
 )
 (Setq count (1+ count))
  ))
 (princ)
 )

 ;REP.LSP-->INTERCHANGES TWO TEXT LINES
 (defun C:REP()
    (setvar "cmdecho" 0)
    (PRINC "\n***REPLACES THE SELECTED TEXT***")
    (setq h (car (entsel "\nSELECT THE FIRST TEXT:")))
     (redraw H 3)
    (SETQ HH (ENTGET H))
    (setq st (cdr (assoc 1 hH)))
    (setq h1 (car (entsel "\nSELECT THE SECOND TEXT:")))
      (REDRAW H1 3)
    (SETQ HH1 (ENTGET H1))
    (setq st1 (cdr (assoc 1 hH1)))
    (setq HH (subst (cons 1 st1) (assoc 1 HH) HH))
      (ENTMOD HH)
    (setq HH1 (subst (cons 1 st) (assoc 1 HH1) HH1))
      (ENTMOD HH1)
 (PRINC)
 )
 ;QA.LSP --> TO DRAW A PIPE UP SYMBOL IN PIPING LAYOUT
 (defun c:QA (/ e f cen rad rby2 cx cy cz)
   (setq e (entsel "Select Circle to Quarter:"))
   (setq e (entget (car e)))
   (if (= (cdr (assoc 0 e)) "CIRCLE")
     (progn
        (setq cen (cdr (assoc 10 e)))
        (setq rad (cdr (assoc 40 e)))
        (setq cx (car cen))
        (setq cy (cadr cen))
        (setq cz (caddr cen))
        (setq rby2 (/ rad 2.))
        (setq e (list (+ cx rby2) cy cz))
        (setq f (list cx (+ cy rby2) cz))
        (setvar "CMDECHO" 0)
        (Command "PLINE" e "w" rad rad "a" "CE" cen f "")
        (setq e (list (- cx rby2) cy cz))
        (setq f (list cx (- cy rby2) cz))
        (Command "PLINE" e "w" rad rad "a" "CE" cen f "")
        (setvar "CMDECHO" 1)
     )
   )
 )
----------------------------------------------------------------------
;CLL.LISP--> DRAWS CENTER LINES IN CIRCLE.
 (defun C:CCL (/ olderr clay sblip scmde sgrid shl sucsf e cen rad d ts xx) 
   (setq olderr *error* 
  *error* clerr)
   (setq scmde (getvar "CMDECHO"))
   (command "_.UNDO" "_GROUP") 
   (setq clay (getvar "CLAYER"))
   (setq sblip (getvar "BLIPMODE"))
   (setq sgrid (getvar "GRIDMODE"))
   (setq shl (getvar "HIGHLIGHT"))
   (setq sucsf (getvar "UCSFOLLOW"))
   (setvar "CMDECHO" 0)
   (setvar "GRIDMODE" 0)
   (setvar "UCSFOLLOW" 0)
   (setq e nil 
  xx "Yes")
   (setq ts (tblsearch "LAYER" "3"))
   (if (null ts) 
     (prompt "\nCreating new layer - 3. ") 
     (progn
       (if (= (logand 1 (cdr (assoc 70 ts))) 1) 
  (progn
    (prompt "\nLayer 3 is frozen. ") 
    (initget "Yes No") 
    (setq xx (getkword "\nProceed? <N>: "))
    (if (= xx "Yes") 
      (command "_.LAYER" "_T" "3" "")
    )
  )
       )
     )
   ) 
   (if (= xx "Yes") 
     (progn
       (while (null e) 
  (setq e (entsel "\nSelect arc or circle: "))
  (if e 
    (progn
      (setq e (car e))
      (if (and (/= 
     (cdr (assoc 0 (entget e))) "ARC") 
     (/= (cdr (assoc 0 (entget e))) "CIRCLE")
   ) 
        (progn
   (prompt "\nEntity is a ") 
   (princ (cdr (assoc 0 (entget e)))) 
   (setq e nil)
        )
      )
      (if e (redraw e 3))
    )
  )
       ) 
       (command "_.UCS" "_E" e) 
       (setq cen (trans (cdr (assoc 10 (entget e))) e 1))
       (setq rad (cdr (assoc 40 (entget e))))
       (prompt "\nRadius is ") 
       (princ (rtos rad)) 
       (initget 7 "Length") 
       (setq d (getdist "\nLength/<Extension>: "))
       (if (= d "Length") 
  (progn
    (initget 7) 
    (setq d (getdist cen "\nLength: "))
  ) 
  (setq d (+ rad d))
       ) 
       (setvar "BLIPMODE" 0)
       (setvar "HIGHLIGHT" 0)
       (command "_.LAYER" "_M" "3" "") 
       (command "_.LINE" (list (car cen) (- (cadr cen) d) (caddr cen)) 
         (list (car cen) (+ (cadr cen) d) (caddr cen)) ""
       ) 
       (command "_.CHANGE" "_L" "" "_P" "_LT" "CENTER" "") 
       (command "_.LINE" (list (- (car cen) d) (cadr cen) (caddr cen)) 
         (list (+ (car cen) d) (cadr cen) (caddr cen)) ""
       ) 
       (command "_.CHANGE" "_L" "" "_P" "_LT" "CENTER" "") 
       (command "_.LAYER" "_S" clay "")
     )
   ) 
   (redraw e 4)
   (command "_.UCS" "_P")                 ; Restore previous UCS
   (setvar "BLIPMODE" sblip)           ; Restore saved modes
   (setvar "GRIDMODE" sgrid)
   (setvar "HIGHLIGHT" shl)
   (setvar "UCSFOLLOW" sucsf)
   (command "_.UNDO" "_E") 
   (setvar "CMDECHO" scmde)
   (setq *error* olderr)               ; Restore old *error* handler
   (princ)
 )
----------------------------------------------------------------------
(Defun c:cn()(setq sel1 (car (entsel "\n** Select Hyp.")))
       (setq get1 (entget sel1))
       (setq get2 (atof (cdr (assoc 1 get1))))
       (setq sel2 (car (entsel "\n** Select First Dim.")))
       (setq get3 (entget sel2))
       (setq get4 (atof (cdr (assoc 1 get3))))
       (setq sel3 (car (entsel "\n** Select Second Dim.")))
       (setq get5 (entget sel3))
       (setq get6 (atof (cdr (assoc 1 get5))))
       (setq pl1 (+ get4  get6))
       (setq sub (- get2 pl1))
       (setq ans1 (rtos sub))
       (setq ans2 (strcat "Bracing Lenght is " ans1))
       (alert ans2)
       (princ))

 (defun c:bk()(setq sl (entsel "\nSelect Line to Break..."))
       (setq pt1 (getpoint "\nPick first point"))
       (setq pt2 (getpoint "\nPick Second point"))
       (command "break" sl "f" pt1 pt2)
       (command  "Line" pt1 pt2 "")
       (command "Change" "l" "" "p" "la" "4" "lt" "hidden" "")
       (princ))

************************************************************************
(defun bf()
   (setq cm (getvar "cmdecho"))
	(setq bm (getvar "blipmode"))
	(setq os (getvar "osmode"))
	(setvar "cmdecho" 0)
   (setvar "blipmode" 0)
   (setvar "osmode" 0)
      (command "mirror" e3 e4 e5 e6 e7 e8 "" pt15 pt16 "")
	(setvar "cmdecho" cm)
   (setvar "blipmode" bm)
   (setvar "osmode" os)
)
-----------------------------------------------------------------------
(DEFUN C:IN()
(SETQ A (GETREAL "\nENTER VALUE IN FEET....."))
(SETQ B (GETREAL "\nTYPE INCHES TO ADD....."))
(SETQ C (* A 12))
(SETQ D (+ B C))
)
-----------------------------------------------------
 ; FOR INCRIMENTING THE SELECTED TEXT ENTITIES
 (defun c:inc()
  (setq in (getint "\nEnter the incriment --> ")
   jn in
  )
  (while (/= a 0)
   (setq tnam (car (entsel "select the text : "))
    tget (entget tnam)
    tstr (cdr (assoc 1 tget))
    tval (atoi tstr)
    nstr (itoa (+ tval in))
    tget (subst (cons 1 nstr) (cons 1 tstr) tget)
    in (+ in jn)
   )
   (entmod tget)
   (entupd tnam)(princ)
  )
  (princ)
 )
--------------------------------------------------------------------
 (defun c:MTC()(princ "\n*** Program by GANESH AGARE.")
  (prompt "\nSelect the text entities ...")
  (setq set (ssget)
   len (sslength set)
   cou 0
  )
  (repeat len
   (setq tnam (ssname set cou)
    tget (entget tnam)
    olds (cdr (assoc 1 tget))
    cou (1+ cou)
    se1 (car(entsel "\nSelect Text to Adopt..."))
    get1 (entget se1)
    news (cdr (assoc 1 get1))
   ) 
   (setq tget (subst (cons 1 news) (cons 1 olds) tget))
   (entmod tget)
   (entupd tnam)
  )
  (princ)
 )
----------------------------------------------------------
(DEFUN C:TYP()(princ "\n ** Program for text (TYP)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "(typ)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------
(DEFUN C:8100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:8125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:8150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:8175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:8200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:8250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:8230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:8d100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127D'ble@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:8d125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127D'ble@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:8d150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127D'ble@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:8d175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127D'ble@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:8d200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127D'ble@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:8d250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127D'ble@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:8d230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%127D'ble@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:84100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1274LEG@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:84125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1274LEG@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:84150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1274LEG@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:84175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1274LEG@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:84200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1274LEG@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:84250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1274LEG@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:84230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1274LEG@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------------------
(DEFUN C:86100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1276LEG@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:86125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1276LEG@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:86150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1276LEG@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:86175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1276LEG@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:86200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1276LEG@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:86250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1276LEG@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:86230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "8%%1276LEG@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:10100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:10125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:10150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:10175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:10200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:10250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:10230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:10d100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127D'ble@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:10d125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127D'ble@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:10d150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127D'ble@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:10d175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127D'ble@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:10d200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127D'ble@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:10d250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127D'ble@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:10d230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%127D'ble@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------
(DEFUN C:104100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1274LEG@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:104125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1274LEG@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:104150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1274LEG@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:104175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1274LEG@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:104200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1274LEG@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:104250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1274LEG@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:104230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1274LEG@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------------------
(DEFUN C:106100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1276LEG@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:106125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1276LEG@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:106150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1276LEG@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:106175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1276LEG@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:106200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1276LEG@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:106250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1276LEG@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:106230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "10%%1276LEG@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:12100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:12125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:12150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:12175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:12200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:12250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:12230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:12d100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127D'ble@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:12d125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127D'ble@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:12d150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127D'ble@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:12d175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127D'ble@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:12d200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127D'ble@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:12d250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127D'ble@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:12d230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%127D'ble@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:124100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1274LEG@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:124125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1274LEG@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:124150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1274LEG@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:124175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1274LEG@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:124200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1274LEG@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:124250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1274LEG@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:124230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1274LEG@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------------------
(DEFUN C:126100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1276LEG@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:126125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1276LEG@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:126150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1276LEG@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:126175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1276LEG@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:126200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1276LEG@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:126250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1276LEG@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:126230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "12%%1276LEG@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:16100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:16125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:16150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:16175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:16200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:16250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:16230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:16d100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127D'ble@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:16d125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127D'ble@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:16d150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127D'ble@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:16d175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127D'ble@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:16d200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127D'ble@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:16d250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127D'ble@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:16d230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%127D'ble@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------
(DEFUN C:164100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1274LEG@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:164125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1274LEG@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:164150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1274LEG@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:164175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1274LEG@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:164200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1274LEG@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:164250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1274LEG@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:164230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1274LEG@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------------------
(DEFUN C:166100()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1276LEG@100c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:166125()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1276LEG@125c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:166150()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1276LEG@150c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:166175()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1276LEG@175c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:166200()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1276LEG@200c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:166250()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1276LEG@250c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:166230()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "16%%1276LEG@230c/c")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:2-8()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-8%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:2-10()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-10%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:2-12()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-12%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:2-16()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-16%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:2-20()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-20%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:2-25()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-25%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:2-32()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-32%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:2-36()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-36%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:2-40()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-40%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:3-8()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-8%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:3-10()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-10%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:3-12()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-12%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:3-16()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-16%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:3-20()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-20%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:3-25()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-25%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:3-32()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-32%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:3-36()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-36%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:3-40()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-40%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:4-8()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-8%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:4-10()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-10%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:4-12()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-12%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:4-16()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-16%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:4-20()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-20%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:4-25()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-25%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:4-32()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-32%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:4-36()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-36%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:4-40()(princ "\n ** Program for text (TOR)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-40%%127")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------
(DEFUN C:S1()(princ "\n ** Program for text (SECTION)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECTION A-A")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-----------------------------------------------------------------------------------
(DEFUN C:%TYP()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UTYPICAL FLOOR LAYOUT")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-----------------------------------------------------------------------------------
(DEFUN C:%1()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UFIRST FLOOR LAYOUT")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------------------
(DEFUN C:%2()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECOND FLOOR LAYOUT")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:%3()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UTHIRD FLOOR LAYOUT")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------------------------
(DEFUN C:%4()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UFOURTH FLOOR LAYOUT")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

-----------------------------------------------------------------------
(DEFUN C:%5()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UFIFTH FLOOR LAYOUT")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------------------------
(DEFUN C:%6()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USIXTH FLOOR LAYOUT")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------------------------------
(DEFUN C:%P()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UPLINTH FLOOR LAYOUT")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------------
(DEFUN C:%T()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UTERRACE FLOOR LAYOUT")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

------------------------------------------------------------------------------
(DEFUN C:%L()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULINTEL FLOOR LAYOUT")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

------------------------------------------------------------------------------
(DEFUN C:%a()(princ "\n ** Program for text (ELEVATION MARK)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECTION A-A")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

---------------------------------------------------------------------------------
(DEFUN C:%B()(princ "\n ** Program for text (ELEVATION MARK)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECTION B-B")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
---------------------------------------------------------------------------
(DEFUN C:%C()(princ "\n ** Program for text (ELEVATION MARK)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECTION C-C")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )




-------------------------------------------------------------------------
(DEFUN C:B1()(princ "\n ** Program for text (LAY)  by GANESH AGARE.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "B1(150x600)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )



----------------------------------------------------------
LL.LISP--> GETS LENGHT OF LINE & TYPES IT.
 (DEFUN C:LL()(princ "\n*** Program by GANESH AGARE...")
 (SETQ SS2 (SSGET))
 (IF SS2
   (PROGN
     (SETQ SSL (SSLENGTH SS2))
     (setq ctr 0)
     (setq ddt 0)
      (WHILE (< CTR SSl)
       (setq ls1 (ssname SS2 ctr))
       (setq eg (entget ls1))
       (setq ls (cdr (assoc 10 eg)))
       (setq lss (cdr (assoc 11 eg)))
       (setq dt (distance ls lss))
       (setq ddt (+ dt ddt))
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (setq ans (rtos ddt))
 (setq str (strcat "Total Length = " ans))
 ;;;(PRINT STR )
 (alert str)
 (PRINC)
 )
------------------------------------------------------------------
 ;CLD.LSP--> TO DRAW REVISION CLOUD
(defun c:Cv1( / P0 P1 R D MR P2 DD)(Princ "\n!!!..PROGRAMED BY GANESH AGARE..!!!\n")
(setq lay (getvar "clayer"))
(setq os (getvar "osmode"))
(setvar "cmdecho" 0)
(setvar "osmode" 0)
(if (= (tblsearch "layer" "revision") nil) (command "layer" "N" "revision" "C" "" "8" ""))
(COMMAND "LAYER" "S" "revision" "")
(SETQ MR (if smcdd smcdd 0.08))
(SETQ dd (GETREAL (strcat "\nGive radius <" (rtos mr) "> :")))
(setq mr (if dd dd mr))
(setq smcdd mr)
(WHILE (NOT P0)
(setq p0 (getpoint "\nEnter start point: "))
)
(setq p1 (getpoint p0 "\nEnter end point: "))
(while p1
   (SETQ D (DISTANCE P0 P1))
   (IF ( > D MR)
       (PROGN
	 (SETQ A (ANGLE P0 P1))
	   (REPEAT (FIX (/ D MR))
	       (SETQ P2 (POLAR P0 A MR))
	       (COMMAND "ARC" P0 "E" P2 "R" (/ MR 2.0))
	       (SETQ P0 P2)
	   )
       )
   )
   (setq r (* (distance p0 p1) 0.50))
   (IF (> R 0)
     (command "arc" p0 "e" p1 "r" R)
   )
   (setq p0 p1)
   (setq p1 (getpoint p0 "\nEnter end point: "))

)
(COMMAND "LAYER" "S" "0" "")
(setvar "osmode" os)
(princ)
)
	

	  
(defun c:CV (/ p0 p1 pe r clay)(Princ "\n!!!..PROGRAMED BY GANESH AGARE..!!!\n")
    (setq oldecho (getvar "cmdecho")
   clay    (getvar "clayer"))
    (setvar "cmdecho" 0) (setvar "orthomode" 0) (setvar "osmode" 0)
    (command "layer" "s" "revision" "")
    (setq p0 (getpoint "\n  Start point: ")
   pe p0
   p1 (getpoint P0 "\n  Next point : ")
    )
    (setq cl (ssadd))
 3   (while p1
    (command "pline" p0 "a" "r" (* (distance p0 p1) 0.75) p1 "")
    (setq p0  p1
   cl1 (entlast)
   cl  (ssadd cl1 cl)
    )
    (setq p1  (getpoint P0 "\n  Next point <Close>: "))
    )
    (command "pline" p0 "a" "r" (* (distance p0 pe) 0.75) pe ""
      "pedit" "l" "j" cl "" ""
      "layer" "s" clay ""
    )
    (setvar "cmdecho" oldecho)
    (princ)
 )
-----------------------------------------------------------------------
;TO GET LENGHT OF LINE
(defun c:TT ()
(princ "\n*** Program by GANESH AGARE")
(setq ds (getvar "dimscale"))
(setq unt (getvar "luprec"))
(setvar "luprec" 1)
(setq ln (entsel "\nSelect line :"))
(setq pp1 (getpoint "\nPick Point for Text..."))
(setq pt1 (cdr (assoc 10 (entget (car ln))))
      pt2 (cdr (assoc 11 (entget (car ln))))  
      agl (angle (trans pt1 0 1) (trans pt2 0 1)))
  (if (and (> agl (dtr 90)) (<= agl (dtr 270))) (setq agl (- agl pi)))
  (setq dist (distance pt1 pt2))
  (Command "text" pp1 (rtos(* ds 2.5)) (rtos(rtd agl)) (RTOS DIST))
  (setvar "luprec" unt)
 ;;(command "change" "L" ""  "" "" ""  "" (rtd agl)"")
  (PRINC))

------------------------------------------------------------------------
;SNO.LISP ---> TO GET SERIAL NO ACCORDINGLY
(Defun c:99()(princ "\n *** Program by GANESH AGARE*** ")
 (Setq No (Getstring "\nEnter starting no. : "))
 (Setq apo (Getint "\nEnter Last no. : "))
 (Setq offd (Getreal "\nGap between Text : "))
 (setq ans (strcase (getstring "\nTop to Bottom / Bottom to Top...? ")))
 (if (= ans "T")(setq ans1 -))
 (if (= ans "B")(setq ans1 +))
 (setq se (car (entsel "\n *** Select a Text...")))
 (setq getx (entget se))
 (setq cpt (cdr (assoc 10 getx)))
 (setq spt (cdr cpt))
 (setq sptx (car cpt))
 (setq spt1 (car spt))
 (setq spt2 (ans1 spt1 offd))
 (setq npt (list sptx spt2 0.0))
 (setq nox (- apo (atoi no)))
 (setq noy (+ 1 nox))
 (repeat noy
 (command "copy" "L" "" cpt npt)
 (Command "Change" "L" "" "" "" "" "" "" (Atoi no))
 (Setq count 0)
 (Setq No (Itoa (1+ (Atoi No))))
 (If (< (Strlen no) 2)
   (Setq No (Strcat "0" No))
 )
 (Setq count (1+ count))
 (princ)
 ))
----------------------------------------------------------------------
 ;;; Pipe cross section !.
 (defun c:CT2(/ osm e ent1 e1 pt1 r1 pt2 pt3 pt4 pt5 pt6 en1 en2 en3 en4)
      (setvar "cmdecho" 0)
      (setq osm (getvar "osmode"))
      (setvar "osmode" 8)
      (command "ucs" "w")
      (progn
      (while (not e) 
      (setq e (entsel "\nSelect Circle:"))
      (setq ent1 e)
      (if (not e)
      (princ "\nNo Object Found."))
      (if e 
      (progn
         (setq e (car e))
         (if (/= (cdr (assoc 0 (entget e))) "CIRCLE")
      (progn
      (prompt "\nEntity is a ") 
      (princ (cdr (assoc 0 (entget e)))) 
      (setq e nil))))))
      (setq cen (trans (cdr (assoc 10 (entget e))) e 1))
      (setq rad (cdr (assoc 40 (entget e)))))
      (setq e1 (entget (car ent1)))
      (if (= "CIRCLE" (cdr (assoc 0 e1)))
   (progn
   (setq pt1 (cdr (assoc 10 e1)))
   (setq r1 (cdr (assoc 40 e1)))
   (setq pt2 (polar pt1 (dtr 90) r1))
   (setq pt3 (polar pt1 (dtr 270) r1))
   (setq pt4 (polar pt1 (dtr 90) (/ r1 2)))
   (setq pt5 (polar pt4 (dtr 90) (/ r1 2)))  
   (setq pt6 (polar pt1 (dtr 270) (/ r1 2)))  
   (command "arc" pt5 "c" pt4 pt1)
   (setq en1 (entlast))
   (command "arc" pt3 "c" pt6 pt1)
   (setq en2 (entlast))
   (command "arc" pt3 "c" pt1 pt2)
   (setq en3 (entlast))
   (command "pedit" "l" "" "j" en1 en2 "" "")
   (setq en4 (entlast))
                 (command "chprop" (cdr (assoc -1 e1)) "" "la" "2" "")
                 (command "hatch" "SOLID" (* 9 (getvar "dimscale")) "45" en4 "")
                 (redraw (cdr (assoc -1 e1)) 4)))
      (setvar "osmode" 101)
      (command "ucs" "p")
      (setvar "cmdecho" 1)
      (princ))
**********************************************************************
;TO DRAW A CUT LINE OF CIRCLE IN ELEVATION
 (DEFUN C:CT1()
 (SETVAR "CMDECHO" 0)
 (SETQ PT1 (GETPOINT "\nPICK FIRST POINT...."))
 (SETQ PT2 (GETPOINT PT1 "\nPICK SECOND POINT...."))
 (SETQ DIS (DISTANCE PT1 PT2))
 (SETQ CA1 (/ DIS 2))
 (SETQ CA2 (/ CA1 2))
 (SETQ CA3 (/ CA2 2))
 (SETQ ANG (ANGLE PT1 PT2))
 (SETQ PT3 (POLAR PT1 ANG CA1))
 (SETQ PT4 (POLAR PT1 ANG CA2))
 (SETQ PT5 (POLAR PT1 ANG (+ CA1 CA2)))
 (SETQ HYP (SQRT (+ (* CA2 CA2) (* CA3 CA3))))
 (SETQ PT6 (POLAR PT1 (- ANG 0.4642) HYP))
 (SETQ PT7 (POLAR PT3 (+ ANG 0.4642) HYP))
 (SETQ PT8 (POLAR PT3 (- ANG 0.4642) HYP))
 (COMMAND"ARC" PT1 PT6 PT3)
 (SETQ ARC1 (SSGET "L"))
 (COMMAND"ARC" PT3 PT7 PT2)
 (SETQ ARC2 (SSGET "L"))
 (COMMAND"ARC" PT2 PT8 PT3)
 (SETQ ARC3 (SSGET "L"))
 (COMMAND"PEDIT" ARC1 "" "J" ARC1 ARC2 ARC3 "" "")
 (SETQ JOI (SSGET "L"))
 (PRINC)
 )

CUT.LISP---> TO CUT FOR RECTANGLE
(setq bk1 0)
(defun dtr (a) (* pi (/ a 180.0)))
(defun rtd (b) (* 180 (/ b pi)))
(defun c:ct ()
       (setvar "cmdecho" 0)
       (setvar "blipmode" 0)
       (setq sc1 (/ (getvar "dimscale") 10))
       (command "osnap" "end")
       (setq pt1 (getpoint "\nFirst point :"))
       (setq pt2 (getpoint "\nSecond point :"))
       (prompt (strcat "\nBreaking distance <" (itoa bk1) ">"))
       (setq bk (getint))
       (if (= bk nil) (setq bk bk1))
       (setq bk1 bk)
       (setq an1 (angle pt1 pt2)) ;angle between pt1 & pt2
       (setq an2 (dtr (+ (rtd an1) 90)))  ;90 degree fron angle an1
       (setq an3 (dtr (+ (rtd an1) 180))) ;180 degree fron angle an1
       (setq an4 (dtr (+ (rtd an1) 270))) ;270 degree fron angle an1
       (setq pt3a (/ (distance pt1 pt2) 2.0))
       (setq pt3 (polar pt1 an1 pt3a))
       (setq pt4 (polar pt3 an3 (/ bk 2.0)))
       (setq pt5 (polar pt4 an1 bk))
       (setq pt6 (polar pt4 an2 bk))
       (setq pt7 (Polar pt5 an4 bk))
       (setq pt8 (polar pt1 an3 (* 6 sc1)))
       (setq pt9 (polar pt2 an1 (* 6 sc1)))
       (command "osnap" "")
       (command "pline" pt8 pt4 pt6 pt7 pt5 pt9 "")
       (command "osnap" "END,MID,PER,NODE,INT,QUA,CEN")
); end of cut
***********************************************************
 (defun c:FB()
  (setvar "cmdecho" 0)
  (if (= findscl nil)
  (setq findscl (getreal "\nEnter your drawing scale :- "))
  (setq magni (* findscl 100))
  )
  (setq ss (ssget "x" (list (cons 0 "INSERT"))))
  (setq cnt 0 cnt1 0)
  (setq len (sslength ss))
   (if SS (progn
      (setq cont t)
      (while cont
  (setq st (getstring "\nEnter Block name to Find:" t))
   (if (= st 0)
      (princ "Null input invalid")
      (setq cont nil)
   )
       )
     )
   )
     (while (< cnt len)
     (setq en (ssname ss cnt))
     (setq einfo (entget en))
     (setq txt (cdr (assoc 2 einfo)))
        (if (= txt st)
      (progn
     (setq a (cdr (assoc 10 einfo)))
     (setq b (car a))
     (setq c (car (cdr a)))
     (setq pt (list b c))
     (command "zoom" "c" pt magni)
     (setq cnt (+ 1 cnt))
      )
      (setq cnt (+ 1 cnt))
      )
    )
    (setvar "cmdecho" 1)
  )
 (defun c:rt()(setq pt (getpoint "\nGive Insertion Point: "))
 (setq ang (getangle "\nRequired Angle Is: "))
 (setq a (getreal "\nWidth Required For Rectangle: "))
 (setq b (getreal "\nHeight Required For Rectangle: "))
 (setq c (/ a 2)
       d (/ b 2)
 )
 (setq pt1 (polar pt(+ ang (dtr 0.00)) d)
       pt2 (polar pt(+ ang (dtr 180.0)) d)
       pt3 (polar pt1(+ ang (dtr 90.0)) c)
       pt4 (polar pt1(+ ang (dtr 270.0)) c)
       pt5 (polar pt2(+ ang (dtr 90.0)) c)
       pt6 (polar pt2(+ ang (dtr 270.0)) c)
 )
 (command "pline" pt3 pt4 pt6 pt5 "c")(princ))
 






*********************************************************************
SPECIAL LAYER.LISP

(defun c:K1 () (command "LINETYPE" "LOAD" "CENER,HIDDEN,DOT" "" "")(princ))
(defun c:R1 () (command "layer" "M" "ARCH_WALL" "COL" "8" "" "")(princ))
(defun c:R2 () (command "layer" "M" "BEAM" "COL" "7" "" "")(princ))
(defun c:R3 () (command "layer" "M" "BEAM_BAR" "COL" "4" "" "")(princ))
(defun c:R4 () (command "layer" "M" "BEAM_center_line" "COL" "1" "" "")(princ))
(defun c:R5 () (command "layer" "M" "BEAM_DETAILING_DIMENSION_ARROW" "COL" "9" "" "")(princ))
(defun c:R5A () (command "layer" "M" "BEAM_DETAILING_STIRRUPS_DIM" "COL" "9" "" "")(princ))
(defun c:R6 () (command "layer" "M" "BEAM_DETAILING_EXTRA_DIMENSION" "COL" "9" "" "")(princ))
(defun c:R7 () (command "layer" "M" "BEAM_DETAILING_TITLE" "COL" "7" "" "")(princ))
(defun c:R8 () (command "layer" "M" "BEAM_LAYOUT_DIMENSION_ARROW" "COL" "9" "" "")(princ))
(defun c:R9 () (command "layer" "M" "BEAM_LAYOUT_STAIRCASE" "COL" "8" "" "")(princ))
(defun c:R10 () (command "layer" "M" "BEAM_LAYOUT_TEXT" "COL" "7" "" "")(princ))
(defun c:R1A () (command "layer" "M" "BEAM_NUMBER_LAYOUT" "COL" "7" "" "")(princ))
(defun c:R12 () (command "layer" "M" "BEAM_SCHEDULE" "COL" "7" "" "")(princ))
(defun c:R13 () (command "layer" "M" "BEAM_SIZE_LAYOUT" "COL" "7" "" "")(princ))
(defun c:R13A () (command "layer" "M" "BEAM_LAYOUT_TITLE" "COL" "7" "" "")(princ))
(defun c:R14 () (command "layer" "M" "BEAM_STEEL_TEXT" "COL" "7" "" "")(princ))
(defun c:R14A () (command "layer" "M" "BEAM_STIRRUPS_TEXT" "COL" "7" "" "")(princ))
(defun c:R15 () (command "layer" "M" "BORDER" "COL" "5" "" "")(princ))
(defun c:R16 () (command "layer" "M" "COLUMN" "COL" "2" "" "")(princ))
(defun c:R17 () (command "layer" "M" "COLUMN_CENTER" "COL" "1" "" "")(princ))
(defun c:R18 () (command "layer" "M" "COLUMN_NUMBER_CIRCLE" "COL" "7" "" "")(princ))
(defun c:R19 () (command "layer" "M" "COLUMN_SCHEDULE" "COL" "7" "" "")(princ))
(defun c:R20 () (command "layer" "M" "COLUMN_SIZE_LAYOUT" "COL" "7" "" "")(princ))
(defun c:R21 () (command "layer" "M" "CUTOUT" "COL" "8" "" "")(princ))
(defun c:R2A () (command "layer" "M" "DUCTS" "COL" "8" "" "")(princ))
(defun c:R23 () (command "layer" "M" "ELEVATION" "COL" "8" "" "")(princ))
(defun c:R24 () (command "layer" "M" "FOOTING" "COL" "3" "" "")(princ))
(defun c:R25 () (command "layer" "M" "FOOTING_DIMENSION" "COL" "9" "" "")(princ))
(defun c:R26 () (command "layer" "M" "FOOTING_PEDESTIAL" "COL" "8" "" "")(princ))
(defun c:R27 () (command "layer" "M" "HANGER" "COL" "2" "" "")(princ))
(defun c:R28 () (command "layer" "M" "HARD_STRATA" "COL" "8" "" "")(princ))
(defun c:R29 () (command "layer" "M" "HATCH" "COL" "8" "" "")(princ))
(defun c:R30 () (command "layer" "M" "HOLD" "COL" "8" "" "")(princ))
(defun c:R31 () (command "layer" "M" "LINE" "COL" "181" "" "")(princ))
(defun c:R32 () (command "layer" "M" "LINE" "COL" "paper" "" "")(princ))
(defun c:R34 () (command "layer" "M" "REVISION" "COL" "8" "" "")(princ))
(defun c:R35 () (command "layer" "M" "RUBBLE" "COL" "8" "" "")(princ))
(defun c:R37 () (command "layer" "M" "SECTION_MARK" "COL" "7" "" "")(princ))
(defun c:R36 () (command "layer" "M" "SECTION_DETAILING_TITLE" "COL" "7" "" "")(princ))
(defun c:R38 () (command "layer" "M" "SLAB" "COL" "1" "" "")(princ))
(defun c:R39 () (command "layer" "M" "SLAB_DIMENSION" "COL" "9" "" "")(princ))
(defun c:R40 () (command "layer" "M" "SLAB_MARK" "COL" "7" "" "")(princ))
(defun c:R41 () (command "layer" "M" "SLAB_SCHEDULE" "COL" "7" "" "")(princ))
(defun c:R42 () (command "layer" "M" "SOLID" "COL" "11" "" "")(princ))
(defun c:R43 () (command "layer" "M" "TITLE" "COL" "7" "" "")(princ))
(defun c:R4A () (command "layer" "M" "PILE" "COL" "9" "" "")(princ))
(defun c:R45 () (command "layer" "M" "PILECAP" "COL" "3" "" "")(princ))
(defun c:R46 () (command "layer" "M" "PILEDIA" "COL" "7" "" "")(princ))
(defun c:R47 () (command "layer" "M" "PILE_CENTER" "COL" "1" "" "")(princ))
(defun c:R48 () (command "layer" "M" "PILE_DIMENSION" "COL" "9" "" "")(princ))
(defun c:R49 () (command "layer" "M" "PILE_TYPE" "COL" "7" "" "")(princ))
(defun c:R50 () (command "layer" "M" "PILE_NUMBER_CIRCLE" "COL" "7" "" "")(princ))
(defun c:R51 () (command "layer" "M" "PILE_SIZE_CIRCLE" "COL" "7" "" "")(princ))
(defun c:R52 () (command "layer" "M" "AREA" "COL" "181" "" "")(princ))
(defun c:R53 () (command "layer" "M" "AREATEXT" "COL" "221" "" "")(princ))
(defun c:A101 () (command "layer" "M" "Area" "COL" "7" "" "")(princ))
(defun c:A102 () (command "layer" "M" "BOUNDARY" "COL" "5" "" "")(princ))
(defun c:A103 () (command "layer" "M" "CHAJJA_ELEVATION" "COL" "8" "" "")(princ))
(defun c:A104 () (command "layer" "M" "CHAJJA_PROJECTION" "COL" "8" "" "")(princ))
(defun c:A105 () (command "layer" "M" "COLUMN" "COL" "2" "" "")(princ))
(defun c:A106 () (command "layer" "M" "COLUMN_CENTER" "COL" "1" "" "")(princ))
(defun c:A107 () (command "layer" "M" "COLUMN_NUMBER_CIRCLE" "COL" "7" "" "")(princ))
(defun c:A108 () (command "layer" "M" "CUTOUT" "COL" "8" "" "")(princ))
(defun c:A109 () (command "layer" "M" "DETAILING_TEXT" "COL" "9" "" "")(princ))
(defun c:A110 () (command "layer" "M" "DUCTS" "COL" "8" "" "")(princ))
(defun c:A111 () (command "layer" "M" "ELEVATION_DIMENSION_TEXT" "COL" "9" "" "")(princ))
(defun c:A112 () (command "layer" "M" "ELEVATION_WALL" "COL" "8" "" "")(princ))
(defun c:A113 () (command "layer" "M" "FURNITURE" "COL" "8" "" "")(princ))
(defun c:A114 () (command "layer" "M" "GRIDE" "COL" "1" "" "")(princ))
(defun c:A115 () (command "layer" "M" "HARD_STRATA" "COL" "9" "" "")(princ))
(defun c:A116 () (command "layer" "M" "IPS_FLOORING" "COL" "9" "" "")(princ))
(defun c:A117 () (command "layer" "M" "LAYOUT_ELEVATION_MARK" "COL" "8" "" "")(princ))
(defun c:A118 () (command "layer" "M" "LAYOUT_EXTRA_DIMENSION_TEXT" "COL" "9" "" "")(princ))
(defun c:A119 () (command "layer" "M" "LAYOUT_ROOM_DISCRIPTION_SIZE" "COL" "7" "" "")(princ))
(defun c:A120 () (command "layer" "M" "LAYOUT_STAIRCASE" "COL" "8" "" "")(princ))
(defun c:A121 () (command "layer" "M" "LAYOUT_WALL" "COL" "4" "" "")(princ))
(defun c:A122 () (command "layer" "M" "LAYOUT_WALL_DIMENSION_TEXT" "COL" "9" "" "")(princ))
(defun c:A123 () (command "layer" "M" "LEVEL_MARKING" "COL" "7" "" "")(princ))
(defun c:A124 () (command "layer" "M" "LINTEL" "COL" "7" "" "")(princ))
(defun c:A125 () (command "layer" "M" "PAPER" "COL" "7" "" "")(princ))
(defun c:A126 () (command "layer" "M" "PAPER_PLOTTING_SCALE" "COL" "11" "" "")(princ))
(defun c:A127 () (command "layer" "M" "RAMP_STEPS" "COL" "8" "" "")(princ))
(defun c:A128 () (command "layer" "M" "REVISION" "COL" "8" "" "")(princ))
(defun c:A129 () (command "layer" "M" "ROOM_DISCRIPTION_SIZE" "COL" "7" "" "")(princ))
(defun c:A130 () (command "layer" "M" "RUBBLE" "COL" "8" "" "")(princ))
(defun c:A131 () (command "layer" "M" "SECTION_DETAILING_TITLE" "COL" "7" "" "")(princ))
(defun c:A132 () (command "layer" "M" "SECTION_DIMENSION_TEXT" "COL" "9" "" "")(princ))
(defun c:A133 () (command "layer" "M" "SECTION_MARKING" "COL" "7" "" "")(princ))
(defun c:A134 () (command "layer" "M" "SLAB" "COL" "7" "" "")(princ))
(defun c:A135 () (command "layer" "M" "SLOP" "COL" "8" "" "")(princ))
(defun c:A136 () (command "layer" "M" "STAIRCASE_ELEVATION" "COL" "9" "" "")(princ))
(defun c:A137 () (command "layer" "M" "STAIRCASE_LAYOUT" "COL" "8" "" "")(princ))
(defun c:A138 () (command "layer" "M" "TITLE_LAYOUT_SECTION_ELEVATION" "COL" "7" "" "")(princ))
(defun c:A139 () (command "layer" "M" "TITLE" "COL" "7" "" "")(princ))
(defun c:A140 () (command "layer" "M" "VENDILATOR" "COL" "8" "" "")(princ))
(defun c:A141 () (command "layer" "M" "WATER_PROFFING_SHADE" "COL" "11" "" "")(princ))
(defun c:A142 () (command "layer" "M" "WINDOWS" "COL" "8" "" "")(princ))


(defun c:HC()(ssget) 
(prompt "\nSELECT OBJECT TO CHANGED ..Program by GANESH AGARE.")
(COMMAND "CHPROP" "P" "" "layer" "3" "LType" "CENTER" ""))

(defun c:DD()(ssget) 
(prompt "\nSELECT OBJECT TO CHANGED ..Program by GANESH AGARE.")
(COMMAND "CHPROP" "P" "" "layer" "5" "LType" "DOT" "")) 

(defun c:Hd()(ssget) 
(prompt "\nSELECT OBJECT TO CHANGED ..Program by GANESH AGARE.")
(COMMAND "CHPROP" "P" "" "layer" "5" "LType" "hidden" ""))


(defun c:by()(ssget) 
(prompt "\nSELECT OBJECT TO CHANGED ..Program by GANESH AGARE.")
(COMMAND "CHPROP" "P" "" "layer" "" "" "LType" "bylayer""" ))
     

      
**********************************************************************************

; -------------------- ISOLATE LAYER FUNCTION --------------------
; Isolates selected object's layer by turning all other layers off
; ----------------------------------------------------------------

(Defun C:LP (/ SS CNT LAY LAYLST VAL)(princ "\n ** Layer Off Program by GANESH AGARE.")

  (init_bonus_error 
    (list
      (list "cmdecho" 0
            "expert"  0
      )
      T     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error

  (if (not (setq SS (ssget "i")))
    (progn
      (prompt "\nSelect object(s) on the layer(s) to be ISOLATED: ")
      (setq SS (ssget))
    )
  )

  (if SS
    (progn

      (setq CNT 0)

      (while (setq LAY (ssname SS CNT))
        (setq LAY (cdr (assoc 8 (entget LAY))))
        (if (not (member LAY LAYLST))
          (setq LAYLST (cons LAY LAYLST))
        )
        (setq CNT (1+ CNT))
      )

      (if (member (getvar "CLAYER") LAYLST)
        (setq LAY (getvar "CLAYER"))
        (setvar "CLAYER" (setq LAY (last LAYLST)))
      )

      (command "_.-LAYER" "_OFF" "*" "_Y")
      (foreach VAL LAYLST (command "_ON" VAL))
      (command "")
      
      (if (= (length LAYLST) 1)
        (prompt (strcat "\nLayer " (car LAYLST) " has been isolated."))
        (prompt (strcat "\n" (itoa (length LAYLST)) " layers have been isolated. "
                        "Layer " LAY " is current."
                )
        )
      )
    )
  )

  (restore_old_error)

  (princ)
)

; -------------------- LAYER FREEZE FUNCTION ---------------------
; Freezes selected object's layer
; ----------------------------------------------------------------

(defun C:Lf ()(princ "\n ** Layer Off Program by GANESH AGARE.")
  (layproc "frz")
  (princ)
)

; ---------------------- LAYER OFF FUNCTION ----------------------
; Turns selected object's layer off
; ----------------------------------------------------------------

(defun C:oF ()(princ "\n ** Layer Off Program by GANESH AGARE.")
  (layproc "off")
  (princ)
)

; ------------- LAYER PROCESSOR FOR LAYOFF & LAYFRZ --------------
; Main program body for LAYOFF and LAYFRZ. Provides user with
; options for handling nested entities.
; ----------------------------------------------------------------

(defun LAYPROC ( TASK / NOEXIT OPT BLKLST CNT EN PMT ANS LAY NEST BLKLST)


; --------------------- Error initialization ---------------------

  (init_bonus_error 
    (list
      (list "cmdecho" 0
            "expert"  0
      )

      nil     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error

; -------------------- Variable initialization -------------------

  (setq NOEXIT T)

  (setq OPT (getcfg (strcat "AppData/AC_Bonus/Lay" TASK)))    ; get default option setting
  (if (not (or (null OPT) (= OPT ""))) (setq OPT (atoi OPT)))

  (setq CNT 0)                                                ; cycle counter


  (while NOEXIT

    (initget "Options Undo")
    (if (= TASK "off")
      (setq EN (nentsel "\nOptions/Undo/<Pick an object on the layer to be turned OFF>: "))
      (setq EN (nentsel "\nOptions/Undo/<Pick an object on the layer to be FROZEN>: "))
    )

; ------------------------- Set Options --------------------------

    (While (= EN "Options")
      (initget "No Block Entity")
      (cond
        ((= OPT 1)
          (setq PMT "\nBlock level nesting/Entity level nesting/<No nesting>: ")
        )
        ((= OPT 2)
          (setq PMT "\nBlock level nesting/No nesting/<Entity level nesting>: ")
        )
        (T
          (setq PMT "\nEntity level nesting/No nesting/<Block level nesting>: ")
        )
      )
      (setq ANS (getkword PMT))

      (cond
        ((null ANS)
          (if (or (null OPT) (= OPT ""))
            (progn
              (print ANS)
              (setq OPT 3)
              (setcfg (strcat "AppData/AC_Bonus/Lay" TASK) "3")
            )
          )
        )
        ((= ANS "No")
          (setq OPT 1)
          (setcfg (strcat "AppData/AC_Bonus/Lay" TASK) "1")
        )
        ((= ANS "Entity")
          (setq OPT 2)
          (setcfg (strcat "AppData/AC_Bonus/Lay" TASK) "2")
        )
        (T
          (setq OPT 3)
          (setcfg (strcat "AppData/AC_Bonus/Lay" TASK) "3")
        )
      )

      (initget "Options")
      (if (= TASK "off")
        (setq EN (nentsel "\nOptions/Undo/<Pick an object on the layer to be turned OFF>: "))
        (setq EN (nentsel "\nOptions/Undo/<Pick an object on the layer to be FROZEN>: "))
      )
    )

; ------------------------- Find Layer ---------------------------

    (if (and EN (not (= EN "Undo")))
      (progn

        (setq BLKLST (last EN))
        (setq NEST (length BLKLST))

        (cond

      ; If the entity is not nested or if the option for entity
      ; level nesting is selected.
    
          ((or (= OPT 2) (< (length EN) 3))
            (setq LAY (entget (car EN)))
          )
  
      ; If no nesting is desired

          ((= OPT 1)
            (setq LAY (entget (car (reverse BLKLST))))
          )

      ; All other cases (default)

          (T
            (setq BLKLST (reverse BLKLST))
            
            (while (and                         ; strip out xrefs
                ( > (length BLKLST) 0)
                (assoc 1 (tblsearch "BLOCK" (cdr (assoc 2 (entget (car BLKLST))))))
                   );and
              (setq BLKLST (cdr BLKLST))
            )
            (if ( > (length BLKLST) 0)          ; if there is a block present
              (setq LAY (entget (car BLKLST)))  ; use block layer
              (setq LAY (entget (car EN)))      ; else use layer of nensel
            )
          )
        )

; ------------------------ Process Layer -------------------------

        (setq LAY (cdr (assoc 8 LAY)))
  
        (if (= LAY (getvar "CLAYER"))
          (if (= TASK "off")
            (progn
              (prompt (strcat "\nReally want layer " LAY " (the CURRENT layer) off? <N>: "))
              (setq ANS (strcase (getstring)))
              (if (not (or (= ANS "Y") (= ANS "YES")))
                (setq LAY nil)
              )
            )
            (progn
              (prompt (strcat "\nCannot freeze layer " LAY".  It is the CURRENT layer."))
              (setq LAY nil)
            )
          )
          (setq ANS nil)
        )
  
        (if LAY
          (if (= TASK "off")
            (progn
              (if ANS
                (command "_.-LAYER" "_OFF" LAY "_Yes" "")
                (command "_.-LAYER" "_OFF" LAY "")
              )
              (prompt (strcat "\nLayer " LAY " has been turned off."))
              (setq CNT (1+ CNT))
            )
            (progn
              (command "_.-LAYER" "_FREEZE" LAY "")
              (prompt (strcat "\nLayer " LAY " has been frozen."))
              (setq CNT (1+ CNT))
            )
          )
        )
      )

; -------------- Nothing selected or Undo selected ---------------

      (progn
        (if (= EN "Undo")
          (if (> CNT 0)
            (progn
              (command "_.u")
              (setq CNT (1- CNT))
            )
            (prompt "\nEverything has been undone.")
          )
          (setq NOEXIT nil)
        )
      )
    )
  )

  (restore_old_error)

)

; --------------------- LAYER LOCK FUNCTION ----------------------
; Locks selected object's layer
; ----------------------------------------------------------------

(Defun C:LAYLCK (/ LAY)(princ "\n ** Layer Off Program by GANESH AGARE.")

  (init_bonus_error 
    (list
      (list "cmdecho" 0
            "expert"  0
      )

      T     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error

  (setq LAY (entsel "\nPick an object on the layer to be LOCKED: "))

  (if LAY
    (progn
      (setq LAY (cdr (assoc 8 (entget (car LAY)))))
      (Command "_.-LAYER" "_LOCK" LAY "")
      (prompt (strcat "\nLayer " LAY " has been locked."))
    )
  )

  (restore_old_error)

  (princ)
)

; -------------------- LAYER UNLOCK FUNCTION ---------------------
; Unlocks selected object's layer
; ----------------------------------------------------------------

(Defun C:LAYULK (/ LAY)(princ "\n ** Layer Off Program by GANESH AGARE.")

  (init_bonus_error 
    (list
      (list "cmdecho" 0
            "expert"  0
      )

      T     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error

  (setq LAY (entsel "\nPick an object on the layer to be UNLOCKED: "))

  (if LAY
    (progn
      (setq LAY (cdr (assoc 8 (entget (car LAY)))))
      (Command "_.-LAYER" "_UNLOCK" LAY "")
      (prompt (strcat "\nLayer " LAY " has been unlocked."))
    )
  )

  (restore_old_error)

  (princ)
)

; ---------------------- LAYER ON FUNCTION -----------------------
; Turns all layers on
; ----------------------------------------------------------------

(Defun C:LO ()(princ "\n ** Layer Off Program by GANESH AGARE.")

  (init_bonus_error 
    (list
      (list "cmdecho" 0)
      nil     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error

  (Command "_.-LAYER" "_ON" "*" "")
  (prompt "\nAll layers have been turned on.")

  (restore_old_error)

  (princ)
)


; --------------------- LAYER THAW FUNCTION ----------------------
; Thaws all layers 
; ----------------------------------------------------------------

(Defun C:LAYTHW ()(princ "\n ** Layer Off Program by GANESH AGARE.")

  (init_bonus_error 
    (list
      (list "cmdecho" 0)
      nil     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error

  (Command "_.-LAYER" "_THAW" "*" "")
  (prompt "\nAll layers have been thawed.")

  (restore_old_error)

  (princ)
)


; --------------------- LAYER MATCH FUNCTION ---------------------
; Changes the layer of selected object(s) to the layer of a
; selected destination object.
; ----------------------------------------------------------------

(Defun C:LM (/ SS CNT LOOP LAY ANS)(princ "\n ** Layer Off Program by GANESH AGARE.")

  (init_bonus_error 
    (list
      (list "cmdecho" 0)
      T     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error


  (if (not (setq SS (ssget "i")))
    (progn
      (prompt "\nSelect objects to be changed: ")
      (setq SS (ssget))
    )
  )

  (if SS
    (progn
      (setq CNT (sslength SS))
      (princ (strcat "\n" (itoa CNT) " found."))      ; Report number of items found

      (command "_.move" SS "")                         ; filter out objects on locked layers

      (if (> (getvar "cmdactive") 0)                   ; if there are still objects left
        (progn
          (command "0,0" "0,0")
          (setq SS  (ssget "p")
                CNT (- CNT (sslength SS))              ; count them
          )
        )
        (setq SS nil)                                  ; else abort operation
      ) 

      (if (> CNT 0)                                    ; if items where filtered out
        (if (= CNT 1)
          (princ (strcat "\n" (itoa CNT) " was on a locked layer."))   ; report it.
          (princ (strcat "\n" (itoa CNT) " were on a locked layer."))
        )
      )
    )
  )


  (if SS
    (progn
      (initget "Type")
      (setq LAY  (entsel "\nType name/Select entity on destination layer: ")
            LOOP T
      )
      
      (while LOOP
        (cond
          ((not LAY)
            (prompt "\nNothing selected.")
            (prompt "\nUse current layer? <Y> ")
            (setq ANS (strcase (getstring)))
            (if (or (= ANS "") (= ANS "Y") (= ANS "YES"))
              (setq LAY  (getvar "clayer")
                    LOOP nil
              )
            )
          )
          ((listp LAY)
            (setq LOOP nil)
          )
          ((= LAY "Type")
            (setq LAY (getstring "\nEnter layer name: "))
            (cond
              ((tblobjname "LAYER" LAY)
                (setq LOOP nil)
              )
              ((/= LAY "")
                (prompt "\nLayer does not exist. Would you like to create it? <Y>: ")
                (setq ANS (strcase (getstring)))
                (if (or (= ANS "") (= ANS "Y") (= ANS "YES"))
                  (if
                    (entmake (list
                              '(0 . "LAYER")
                              '(100 . "AcDbSymbolTableRecord")
                              '(100 . "AcDbLayerTableRecord")
                              '(6 . "CONTINUOUS")
                              '(62 . 7)
                              '(70 . 0)
                               (cons 2 LAY)
                             )
                    )
                    (setq LOOP nil)
                    (prompt "\nInvalid Layer name.")
                  )
                )
              )
            )
          )
        )
        (if LOOP
          (progn
            (initget "Type")
            (setq LAY (entsel "\nType name/Select entity on destination layer: "))
          )
        )
      ); while LOOP
        

      (if (listp LAY)
        (setq LAY (cdr (assoc 8 (entget (car LAY)))))
      )

      (command "_.change" SS "" "_p" "_la" LAY "")

      (if (= (sslength SS) 1)
        (prompt (strcat "\nOne object changed to layer " LAY ))
        (prompt (strcat "\n" (itoa (sslength SS)) " objects changed to layer " LAY ))
      )
      (if (= LAY (getvar "clayer"))
        (prompt " (the current layer).")
        (prompt ".")
      )
    )
  )

  (restore_old_error)

  (princ)
)

; --------------- CHANGE TO CURRENT LAYER FUNCTION ---------------
; Changes the layer of selected object(s) to the current layer
; ----------------------------------------------------------------

(Defun C:LAYCUR (/ SS CNT LAY)(princ "\n ** Layer Off Program by GANESH AGARE.")
  (init_bonus_error 
    (list
      (list "cmdecho" 0)
      T     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error


  (if (not (setq SS (ssget "i")))
    (progn
      (prompt "\nSelect objects to be CHANGED to the current layer: ")
      (setq SS (ssget))
    )
  )

  (if SS
    (progn
      (setq CNT (sslength SS))
      (princ (strcat "\n" (itoa CNT) " found."))      ; Report number of items found

      (command "_.move" SS "")                         ; filter out objects on locked layers

      (if (> (getvar "cmdactive") 0)                   ; if there are still objects left
        (progn
          (command "0,0" "0,0")
          (setq SS  (ssget "p")
                CNT (- CNT (sslength SS))              ; count them
          )
        )
        (setq SS nil)                                  ; else abort operation
      ) 

      (if (> CNT 0)                                    ; if items where filtered out
        (if (= CNT 1)
          (princ (strcat "\n" (itoa CNT) " was on a locked layer."))   ; report it.
          (princ (strcat "\n" (itoa CNT) " were on a locked layer."))
        )
      )
    )
  )

  (if SS
    (progn
      (setq LAY (getvar "CLAYER"))
      (command "_.change" SS "" "_p" "_la" LAY "")
      (if (= (sslength SS) 1)
        (prompt (strcat "\nOne object changed to layer " LAY " (the current layer)."))
        (prompt (strcat "\n" (itoa (sslength SS)) " objects changed to layer " LAY " (the current layer)."))
      )
    )
  )

  (restore_old_error)

  (princ)
)
*****************************************************************************
 (defun c:CL()(princ "\n ** Change Current Layer Program by GANESH AGARE.")
 (setvar "cmdecho" 0)
 (setq lo (entsel "\n * Pick The Object For Current Layer: "))
 (setq slo (car lo))
 (setq nlo (entget slo))
 (setq llo (cdr (assoc 8 nlo)))
 (command "'layer" "s" llo "")
 )
 
 (defun c:LOF ()(princ "\n ** Layer Off Program by GANESH AGARE.")
 (setvar "cmdecho" 0)
 (setq lo (entsel "\n * Pick The Object To Off Layer: "))
 (setq slo (car lo))
 (setq nlo (entget slo))
 (setq llo (cdr (assoc 8 nlo)))
 (command "layer" "OF" llo)
 )
 
 (DEFUN C:DAS2()(princ "\n ** Dashline Program by GANESH AGARE")
 (SETVAR "CMDECHO" 0)
 (setq so (ssget))
 (command "chprop" so "" "la" "dash" "lt" "dashed2" "")
 )
 
 (DEFUN C:COL()(princ "\n ** Color Change Program by GANESH AGARE")
 (SETVAR "CMDECHO" 1)
 (setq cl (ssget))
 (command "chprop" cl "" "c" pause "")
 )
 (defun pro ()
 (setq an nil)
 (setq dt nil)
 (setq an (getangle "\n * Enter Angle OR Second Point For Angle: " fp))
 )
 

 (defun c:L0()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `0' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "0" "")(princ))

 (defun c:1()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `1' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "ARCH_WALL" "")(princ))

 (defun c:2()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `2' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "BEAM" "")(princ)) 

 (defun c:3()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `3' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "BEAM_BAR" "")(princ))

 (defun c:4()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `4' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "BEAM_CENTER_LINE" "")(princ))

 (defun c:5()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `5' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "BEAM_DETAILING_DIMENSION_ARROW" "")(princ))

 (defun c:5A()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `5' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "BEAM_DETAILING_STIRRUPS_DIM" "")(princ))

 (defun c:6()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `6' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "BEAM_DETAILING_EXTRA_DIMENSION" "")(princ))

 (defun c:7()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `7' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "BEAM_DETAILING_TITLE" "")(princ))

 (defun c:8()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `8' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "BEAM_LAYOUT_DIMENSION_ARROW" "")(princ))

 (defun c:9()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `9' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "BEAM_LAYOUT_STAIRCASE" "")(princ))

 (defun c:10()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `10' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "BEAM_LAYOUT_TEXT" "")(princ))

 (defun c:1A()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `1A' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "BEAM_NUMBER_LAYOUT" "")(princ))

 (defun c:12()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `1A' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "BEAM_SCHEDULE" "")(princ))

 (defun c:13()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `13' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "BEAM_SIZE_LAYOUT" "")(princ))

 (defun c:13A()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `13A' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "BEAM_LAYOUT_TITLE" "")(princ))

 (defun c:14()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `14' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "BEAM_STEEL_TEXT" "")(princ))

(defun c:14A()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `14' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "BEAM_STIRRUPS_TEXT" "")(princ))

 (defun c:15()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `15' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "BORDER" "")(princ))

 (defun c:16()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `16' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "COLUMN" "")(princ))

 (defun c:17()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `17' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "COLUMN_CENTER" "")(princ))

 (defun c:18()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `18' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "COLUMN_NUMBER_CIRCLE" "")(princ))

 (defun c:19()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `19' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "COLUMN_SCHEDULE" "")(princ))

 (defun c:20()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `20' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "COLUMN_SIZE_LAYOUT" "")(princ))

 (defun c:21()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `21' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "CUTOUT" "")(princ))

 (defun c:2A()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `2A' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "DUCTS" "")(princ))

 (defun c:23()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `23' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "ELEVATION" "")(princ))

 (defun c:24()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `24' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "FOOTING" "")(princ))

 (defun c:25()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `25' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "FOOTING_DIMENSION" "")(princ))

 (defun c:26()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `26' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "FOOTING_PEDESTIAL" "")(princ))

 (defun c:27()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `27' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "HANGER" "")(princ))

 (defun c:28()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `28' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "HARD_STRATA" "")(princ))

 (defun c:29()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `29' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "HATCH" "")(princ))

 (defun c:30()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `30' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "HOLD" "")(princ))

 (defun c:31()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `31' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "LINE" "")(princ))

(defun c:32()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `32' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "paper" "")(princ))

 (defun c:34()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `34' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "REVISION" "")(princ))

 (defun c:35()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `35' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "RUBBLE" "")(princ))

 (defun c:36()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `36' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SECTION_DETAILING_TITLE" "")(princ))

 (defun c:37()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `37' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SECTION_MARK" "")(princ))

 (defun c:38()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `38' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SLAB" "")(princ))

 (defun c:39()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `39' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SLAB_DIMENSION" "")(princ))

 (defun c:40()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `40' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SLAB_MARK" "")(princ))

 (defun c:41()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `41' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SLAB_SCHEDULE" "")(princ))

 (defun c:42()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `42' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SOLID" "")(princ))

 (defun c:43()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `43' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "TITLE" "")(princ))

 (defun c:4A()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `4A' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "PILE" "")(princ))

 (defun c:45()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `45' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "PILECAP" "")(princ))

 (defun c:46()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `46' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "PILEDIA" "")(princ))

 (defun c:47()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `47' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "PILE_CENTER" "")(princ))

 (defun c:48()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `48' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "PILE_DIMENSION" "")(princ))

 (defun c:49()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `49' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "PILE_TYPE" "")(princ))

 (defun c:50()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `50' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "PILE_NUMBER_CIRCLE" "")(princ))

 (defun c:51()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `51' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "PILE_SIZE_CIRCLE" "")(princ))

(defun c:52()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `52' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "AREA" "")(princ))

(defun c:53()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `53' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "AREA_TEXT" "")(princ))

 (defun c:101()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `101' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "AREA" "")(princ))

 (defun c:102()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `102' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "BOUNDARY" "")(princ)) 

 (defun c:103()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `103' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "CHAJJA_ELEVATION" "")(princ))

 (defun c:104()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `104' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "CHAJJA_PROJECTION" "")(princ))

 (defun c:105()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `105' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "COLUMN" "")(princ))

 (defun c:106()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `106' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "COLUMN_CENTER" "")(princ))

 (defun c:107()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `107' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "COLUMN_NUMBER_CIRCLE" "")(princ))

(defun c:108()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `108' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "CUTOUT" "")(princ))

 (defun c:109()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `109' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "DETAILING_TEXT" "")(princ))

 (defun c:110()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `110' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "DUCTS" "")(princ))

 (defun c:111()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `111' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "ELEVATION_DIMENSION_TEXT" "")(princ))

 (defun c:112()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `112' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "ELEVATION_WALL" "")(princ))

 (defun c:113()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `113' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "FURNITURE" "")(princ))

 (defun c:114()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `114' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "GRIDE" "")(princ))

 (defun c:115()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `115' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "HARD_STRATA" "")(princ))

 (defun c:116()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `116' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "IPS_FLOORING" "")(princ))

 (defun c:117()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `117' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "LAYOUT_ELEVATION_MARK" "")(princ))

 (defun c:118()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `118' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "LAYOUT_EXTRA_DIMENSION_TEXT" "")(princ))

 (defun c:119()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `119' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "LAYOUT_ROOM_DISCRIPTION_SIZE" "")(princ))

 (defun c:120()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `120' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "LAYOUT_STAIRCASE" "")(princ))

 (defun c:121()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `121' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "LAYOUT_WALL" "")(princ))

 (defun c:122()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `122' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "LAYOUT_WALL_DIMENSION_TEXT" "")(princ))

 (defun c:123()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `123' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "LEVEL_MARKING" "")(princ))

 (defun c:124()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `124' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "LINTEL" "")(princ))

 (defun c:125()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `125' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "PAPER" "")(princ))

 (defun c:126()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `126' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "PAPER_PLOTTING_SCALE" "")(princ))

 (defun c:127()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `127' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "RAMP_STEPS" "")(princ))

 (defun c:128()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `128' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "REVISION" "")(princ))

 (defun c:129()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `129' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "ROOM_DISCRIPTION_SIZE" "")(princ))

 (defun c:130()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `130' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "RUBBLE" "")(princ))

 (defun c:131()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `131' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SECTION_DETAILING_TITLE" "")(princ))

 (defun c:132()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `132' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SECTION_DIMENSION_TEXT" "")(princ))

 (defun c:133()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `133' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SECTION_MARKING" "")(princ))

 (defun c:134()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `134' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SLAB" "")(princ))

 (defun c:135()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `135' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "SLOP" "")(princ))

 (defun c:136()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `136' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "STAIRCASE_ELEVATION" "")(princ))

 (defun c:137()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `137' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "STAIRCASE_LAYOUT" "")(princ))

 (defun c:138()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `138' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "TITLE_LAYOUT_SECTION_ELEVATION" "")(princ))

 (defun c:139()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `139' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "TITLE" "")(princ))

 (defun c:140()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `140' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "VENDILATOR" "")(princ))

 (defun c:141()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `141' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "WATER_PROFFING_SHADE" "")(princ))

 (defun c:142()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `142' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "WINDOWS" "")(princ))

 (defun c:cd() (setvar "cmdecho" 0)
        (princ "\n***  Change objects into Dim layer...")
        (setq select3 (ssget))
        (command "change" select3 "" "P" "LA" "3" "")(princ))
 
 (defun c:hid() (setvar "cmdecho" 0)
        (princ "\n***  Change objects into Dim layer...")
        (setq select4 (ssget))
        (command "change" select4 "" "P" "LA" "4" "")(princ))
 
 (defun c:HH() (setvar "cmdecho" 0)
        (princ "\n***  Change objects into Hatch layer...")
        (setq select3 (ssget))
        (command "change" select3 "" "P" "LA" "3" "")(princ))
 
 (defun c:ls()(setq lay (getstring "\nEnter Layer name --->"))
       (if (= lay "")
       (progn (setq lay "0")))
       (command "layer" "s" lay ""))
 
 (defun c:on()(setq layx (getstring "\nEnter Layer to turn ON <*>..."))
       (if (= layx "")
       (progn (setq layx "*")))
       (command "layer" "on" layx ""))
 
 (defun c:off()(setq lay2 (getstring "\nEnter Layer to turn OFF <*>..."))
       (if (= lay2 "")
       (progn (setq lay2 "*")))
       (command "layer" "off" lay2 "" ""))



*********************************************************************

*******************************************************************
;DIMENSION UPDATED LISP. 
**************************       
 (defun c:dda() (setq p1 (car (entsel)))
        (setq p2 (entget p1))
        (setq p3 (cdr (assoc 0 p2)))
        (if (= p3 "ATTDEF")
        (command "ddedit" p1))
               (if (= p3 "MTEXT")
        (command "ddedit" p1))
               (if (= p3 "DIMENSION")
        (command "dim1" "n"))
        (if (= p3 "TEXT")
        (command "ddedit" p1))
        (if (= p3 "INSERT")
               (command "ddatte" p1))
               (if (= p3 "MTEXT")
               (command "ddedit" p1))(PRINC))
 
 
 
 (defun c:mts()(princ "\n*** Program by GANESH AGARE")
       (setvar "cmdecho" 0)
       (setq p1 (car (entsel "\n *** Select text to change style")))
       (setq p2 (car (entsel "\n *** Select text to adopt style")))
       (setq p3 (entget p2))
       (setq p4 (cdr (assoc 7 p3)))
       (command "change" p1 "" "" "" p4 "" "" ""))

 (Defun c:dc()(setvar "cmdecho" 0)
  (setq news (Getstring "\n *** Enter New Dimension Style....")
        dime (car (entsel "\n *** Select Dimension to change Style..."))
        gete (entget dime)
        olds (assoc 3 gete)
        upd (cons 3 news)
        gete (subst upd olds gete))
        (entmod gete)(princ))
     
 (defun c:md() (setvar "cmdecho" 0)
        (setq get (car (entsel "\n *** Select Dimension to adopt Text...")))
        (setq get1 (entget get))
        (setq get2 (cdr (assoc 1 get1)))
        (command "dim1" "n" get2)
        (princ "\n *** Dimension to change....")(princ))

(defun c:oc() (setvar "cmdecho" 0)
        (setq get (car (entsel "\n *** Select Dimension...")))
        (setq get1 (entget get))
        (setq get2 (cdr (assoc 1 get1)))
        (if (= get2 "")
        (progn (setq msg "Selected Dimension Text is not User's input...!")
        (alert msg)(exit)))
        (setq get3 (atof get2))
        (setq ans (/ get3 2 ))
        (command "offset" ans)
        (princ))

(defun c:lea()(setvar "cmdecho" 0)
        (setq pnt (getpoint "\n Leader start point...."))
        (command "dim1" "lea" pnt pause pause "")
        (initget 1 "Copytext Item No.")
        (setq opt (getkword "\nCopytext or Item no. < > ? "))
        (if (= opt "Copytext")
        (setq get (car (entsel "\nSelect Text..."))
       getx (entget get)
       tex (cdr (Assoc 1 getx))))
        (command "u")(command "u")
        (setq lp (getvar "lastpoint"))
        (command "dim1" "lea" pnt lp "" tex)(princ))
-----------------------------------------------------------------
(defun c:io ()(setvar "cmdecho" 0)
(princ "\n ** Place Item No. Program By GANESH AGARE")
(command "layer" "s" "3" "")
(setq fp1 (getpoint "\n ** Pick First Point For Leader: "))
(setq sp1 nil)
(initget (+ 1 2 4 32))
(setq sp1 (getpoint "\n ** Pick Second Point For Leader: " fp1))
(if (/= fp1 nil)
(progn
(setq sc1 (getvar "dimscale"))
(setq dnod1 (* 1.25 sc1))
(command "line" fp1 sp1 "")
(setq lan1 (getvar "lastangle"))
(setq pla1 (abs lan1))
(setq la1 nil)
(setq la1 (* 180(/ pla1 3.1415927)))
(setq rd1 (* 3.5 sc1))
(setq ln1 (* 3.5 sc1))
(setq rtln1 (+ ln1 rd1))
(setq hrtln1 (/ rtln1 2))
(setq tln1 (rtos hrtln1))
(setq zdir1 (strcat "@" tln1 "<0"))
(setq ozdir1 (strcat "@" tln1 "<180"))

(initget 1 "Arrow Donut")
(setq ad1 (getkword "\nArrow Or Donut ? "))
(if (= ad1 "Donut")
(progn
 (command "donut" "0" dnod1 fp1 "")
)
)

(if (= ad1 "Arrow")
(progn
(command "insert" "AR" fp1 sc1 "" sp1)
))
  
 (if (< la1 90)
  (progn  
    (zdr1)
  )
  (if (> la1 270)
   (progn
    (zdr1)
   )
   (if (> la1 90)
  (progn  
    (ozdr1)
  )
   (if (< la1 270)
     (progn
      (ozdr1)   
     )
) 
)
)
)

)
(princ " ** Program Terminated ...")
)

)
-------------------------------------------------------------------
;;LSP FOR ISOMETRIC'S DIMENSIONS
(DEFUN DIM1()(D1)(SETQ STY "LV" DIMROT "90" OBLANG "210")(D2))
(DEFUN DIM2()(d1)(SETQ STY "RV" DIMROT "90" OBLANG "330")(D2))
(DEFUN DIM3()(d1)(SETQ STY "RV" DIMROT "30" OBLANG "270")(D2))
(DEFUN DIM4()(d1)(SETQ STY "LV" DIMROT "330" OBLANG "90")(D2))
(DEFUN DIM5()(d1)(SETQ STY "RV" DIMROT "30" OBLANG "330")(D2))
(DEFUN DIM6()(d1)(SETQ STY "LV" DIMROT "330" OBLANG "210")(D2))

(DEFUN D1()
(SETVAR "DIMTAD" 1)(SETVAR "DIMTIH" 0)
(COMMAND "LAYER" "S" "DIM" "")
(SETQ P1 (GETPOINT "\nFirst Point or press ENTER to select: "))
(IF (= P1 NIL)
(PROGN (SETQ LL (CAR(ENTSEL "\nSelect Line to Dimension...")))
       (SETQ LL1 (ENTGET LL))
       (SETQ P1 (CDR (ASSOC 10 LL1)))
       (SETQ P2 (CDR (ASSOC 11 LL1)))))
(IF (= P2 NIL)(SETQ P2 (GETPOINT P1 "\nSecond Point: ")))
(SETQ P3 (GETPOINT P2 "\nDimension Location: "))
(SETQ TXT (GETSTRING "\nDimension Text: "))
(setq dimang (angle p1 p2) dimdis (distance p1 p2)))

(DEFUN D2()
(COMMAND ".DIM" "DIMTXSTY" STY "ROT" DIMROT P1 P2 P3 TXT "OBLI" "L" "" 
OBLANG "EX")(setq P2 NIL))
(princ)



 
---------------------------------------------------------------------- 
 ; TO CHANGE A TEXT'S HEIGHT WRT ANOTHER TEXT'S HEIGHT
 
 (defun c:HTT()(princ "\n*** Program by GANESH AGARE.")
  (setq tnam1 (car (entsel "\nSelect the text to change ..."))
   tget1 (entget tnam1)
   htr1  (assoc 40 tget1)
  )
  (setq tnam2 (car (entsel "\nSelect the text of desired string ..."))
   tget2 (entget tnam2)
   htr2  (assoc 40 tget2)
  )
  (setq tget1 (subst htr2 htr1 tget1))
  (entmod tget1)
  (entupd tnam1)
  (princ)
 )
 
 ;TJ.LSP-->TEXT JUSTFICATION
 ;         CHANGES INSERTION POINT OF TEXT
 (defun c:tj(/ J E P10 P11 F EN P11N P10N FN Y)
    (princ "\nChange Justification program by GANESH AGARE...")
    (initget 1 "L C M R")
    (setq j (getkword "\nLeft /Center /Middle /Right : "))
    (setq e (entsel"\nSelect Text To Aligned: "))
       (if (/= e NIL)
     (progn
   (while e
   (setq enam (car e))
   (setq e (entget enam))
   (setq p11 (assoc 11 e))
   (setq p10 (assoc 10 e))
   (setq f (assoc 72 e))
   (setq en e)
   (if
     (= (cdr f) 0)
     (setq p11n (cons 11 (cdr p10)))
   )
   (if
   (= (cdr f) 0)
   (setq en (subst p11n p11 en))
   )
   (if
       (and (= j "L") (/= (cdr f) 0))
       (progn
     (setq p10n (cons 10 (cdr p11)))
     (setq en (subst p10n p10 en))
        )
   )
   (if (= j "L") (setq fn 0))
   (if (= j "C") (setq fn 1))
   (if (= j "M") (setq fn 4))
   (if (= j "R") (setq fn 2))
   (setq fn (cons 72 fn))
   (setq en (subst fn f en ))
   (entmod en)
   (entupd enam)
   (setq e (entsel"\nSelect Text To Aligned: "))
       )
       )
       (princ "\nSelect Object Properly !")
     )
 (princ)
 )
 ;  TEDITOR.LSP--> THIS MACRO CHANGES DRAWING TEXT IN Norton Editor,
 ;                 AND INSERT EDITED TEXT AGAIN IN THE DRAWING
 (defun c:tedit(/ a b c d o)
   (setq ss (ssget)
  ssl (sslength ss)
  count 0
  o (open "useless" "w")
   )
     (while (< count ssl)
        (setq a (cdr (assoc 1 (entget (ssname ss count))))
       b (cdr (assoc 0 (entget (ssname ss count))))
        )
        (if (= b "TEXT")
     (write-line a o)
        )
        (setq count (1+ count))
    )
    (close o)
 ;------------------------------------------------------------------------------------------!!!!
    (initget "NE K WS")
    (setq edtor (getkword "\nWhich Editor Would You Like To Use? [ Kedit,WS or < NE > ] : "))
    (IF (= EDITOR NIL)
   (command "shell" (strcat "ne useless"))
   (command "shell" (strcat edtor " useless"))
    )
 
   (setq o (open "useless" "r")
  count 0)
   (while (< count ssl)
      (setq txt (read-line o))
      (setq ed (entget (ssname ss count)))
      (setq ed
  (subst (cons 1 txt) (assoc 1 ed)
   ed
  )
      )
      (entmod ed)
   (setq count (1+ count))
   )
      (close o)
 ;  (command "shell" (strcat "del useless"))
   (redraw)
 )


 ;CIRTXT.LSP--> TO TYPE TEXT IN CIRCLE MANNER
 (defun inpWR(/ cp sp endang h dir str r stang strlength tang
 sign pt cnt txt rotang ang)
 (graphscr)
 (initget 1)
 (setq cp (getpoint "\nPick Center Point of Circular Text :"))
 (initget 1)
 (setq sp (getpoint "\nPick Middle of First Character     :"))
 (initget 1)
 (setq endang (getangle cp "\nAngle From center Point      :"))
 (initget 1)
 (setq h (getdist  "\nGive Text Height                   :"))
 (initget 1 "CW CCW")
 (setq dir (getint "\nClockwise OR Counter Clockwise  (CW or 1 / CCW or 2):"))
 (prompt  "\nEnter Text :")
 (setq str (read-line))
 (setq   r         (distance cp sp )
  stang     (angle cp sp)
  strlength (strlen str)
 )
 
 (if (= strlength 1)
  (setq strlength 2)
 ); to avoid division by zero
 (if (> stang endang)
  (setq tang (- stang endang))
  (setq tang (- (* 2 pi) (abs (- stang endang))))
 );compute total angle
 (if (or (= dir 1) (= dir "CW"))
  (setq   pt  sp sign -)
  (setq pt (polar cp stang r) sign +)
 )
 (setq
  ang     (/  (abs tang)  (- strlength 1) )
  cnt     1
  txt     (substr str 1 1)
  rotang   (/(sign stang (/ pi 2)) (/ pi 180))
 )
 
 (repeat strlength
  (command "text" "m" pt h rotang txt)
  (setq   stang  (sign stang ang)
   pt     (polar cp stang r)
   cnt   (+ cnt 1)
   txt    (substr str cnt 1)
   rotang  (/(sign stang (/ pi 2)) (/ pi 180))
  )
 )
 )
 (defun C:CIRT(/ oangbase oangdir oblipmode)
 (setvar "cmdecho" 0)
 (setq   oangbase (getvar "angbase") oangdir (getvar "angdir")
  oblipmode (getvar "blipmode")
 )
 (setvar "angbase" 0)
 (setvar "angdir" 0)
 (setvar "blipmode" 0)
 (inpwr)
 (setvar "angbase" oangbase)
 (setvar "angdir" oangdir)
 (setvar "blipmode" oblipmode)
 (princ)
 )
 (defun clerr (s) 
   (if (/= s "Function cancelled")     ; If an error (such as CTRL-C) occurs
     (princ (strcat "\nError: " s))    ; while this command is active...
   ) 
   (if e (redraw e 4))
   (command "_.UCS" "_P")                 ; Restore previous UCS
   (setvar "BLIPMODE" sblip)           ; Restore saved modes
   (setvar "GRIDMODE" sgrid)
   (setvar "HIGHLIGHT" shl)
   (setvar "UCSFOLLOW" sucsf)
   (command "_.LAYER" "_S" clay "") 
   (command "_.UNDO" "_E") 
   (setvar "CMDECHO" scmde)
   (setq *error* olderr)               ; Restore old *error* handler
   (princ)                             
 ) 
 
 ;;; --------------------------- Main Program ---------------------------------;
 

 (defun c:CDT()(princ "\n ** Copy & trim Program by GANESH AGARE.")
 (princ "\n * Entities Must Be Selected By Window Crossing")
 (setq fc (getpoint "\n * Enter The Lower Left Corner Of Window Crossing: "))
 (setq sc (getcorner "\n * Enter The Upper Right Corner Of Window Crossing: " fc))
 (setq wl (- (car sc)(car fc)))
 (setq wh (- (cadr sc)(cadr fc)))
 (setq sr (list (+ (car fc) wl) (cadr fc)))
 (setq tr (list (+ (car fc) wl) (+ (cadr fc) wh)))
 (setq fr (list (car fc) (+ (cadr fc) wh)))
 (command "pline" fc sr tr fr "c")
 (setq te (ssget "l"))
 (setq se (ssget "c" fc sc))
 ;;;(setq mpt (getpoint "\n * Enter Base Point For Copy & Scale: "))
 (command "move" se "" fc pause)
 (setq lp (getvar "lastpoint"))
 (command "copy" "p" "r" te "" lp fc)
 (setq ffp (list (- (car lp) 1) (- (cadr lp) 1)))
 (setq sfp (list (+ (car ffp) wl 2) (cadr ffp)))
 (setq tfp (list (car sfp) (+ (cadr sfp) wh 2)))
 (setq lfp (list (car ffp) (cadr tfp)))
 (command "trim" te "" "f" ffp sfp "" "f" sfp tfp "" "f" tfp lfp "" "f" lfp ffp "" "")
 (command "erase" te "")
 (setq scf (getreal "\n * Scale Factor : "))
 (command "scale" "C" ffp tfp "" lp scf)
 )
 
 

 ; TO CHANGE A TEXT'S STRING WRT ANOTHER TEXT'S STRING                
 (defun c:Tc()
         (setq tnam1 (car (entsel "\nSelect the text to change ..."))
                 tget1 (entget tnam1)
                 str1  (assoc 1 tget1)
         )
         (setq tnam2 (car (entsel "\nSelect the text of desired string ..."))
                 tget2 (entget tnam2)
                 str2  (assoc 1 tget2)
         )
         (setq tget1 (subst str2 str1 tget1))
         (entmod tget1)
         (entupd tnam1)
         (princ))
 ; TO CHANGE A TEXT'S HEIGHT WRT ANOTHER TEXT'S HEIGHT            
 (defun c:TH()
         (setq tnam1 (car (entsel "\nSelect the text to change ..."))
                 tget1 (entget tnam1)
                 str1  (assoc 40 tget1)
         )
         (setq tnam2 (car (entsel "\nSelect the text to Adopt ..."))
                 tget2 (entget tnam2)
                 str2  (assoc 40 tget2)
         )
         (setq h1 (cdr str1))
         (setq h2 (cdr str2))
         (if (= h1 h2)
         (princ "\n               Both Text's heigth are same...!"))
         (setq tget1 (subst str2 str1 tget1))
         (entmod tget1)
         (entupd tnam1)
         (princ))
 
 

****************************************************************
(defun c:TEJ()(princ "\nChange Justification program by GANESH AGARE...")
    (initget 1 "L C M R")
    (setq j (getkword "\nLeft /Center /Middle /Right : "))
    (princ "\nSelect Text To Aligned: ")
    (setq e (ssget))
    (Setq count 0)
    (if (/= e NIL)
    (progn
    (setq ssl (sslength e))
    (While (< Count (sslength e))
    (setq enam (ssname e count))
                 (setq te (entget enam))
   (setq p11 (assoc 11 te))
   (setq p10 (assoc 10 te))
   (setq f (assoc 72 te))
   (setq en te)
   (if
     (= (cdr f) 0)
     (setq p11n (cons 11 (cdr p10)))
   )
   (if
   (= (cdr f) 0)
   (setq en (subst p11n p11 en))
   )
   (if
       (and (= j "L") (/= (cdr f) 0))
       (progn
     (setq p10n (cons 10 (cdr p11)))
     (setq en (subst p10n p10 en))
        )
   )
   (if (= j "L") (setq fn 0))
   (if (= j "C") (setq fn 1))
   (if (= j "M") (setq fn 4))
   (if (= j "R") (setq fn 2))
   (setq fn (cons 72 fn))
   (setq en (subst fn f en ))
   (entmod en)
   (entupd enam)
                 (Setq count (1+ count)))))
 (princ))
 
----------------------------------------------------------------- 
(DEFUN C:J()
    (setvar "CMDECHO" 0)
    (setvar "osmode" 0)
    (setvar "orthomode" 0)
    (princ "\nSelect two lines to join:\n ")
    (setq ss (ssget) line1 (ssname ss 0) line2 (ssname ss 1))
    (if (AND (and line1 line2) (= (SSLENGTH SS) 2))
       (progn
          (setq line3 (entget line1))
          (setq ep1 (cdr (assoc 10 line3)))
          (setq ep2 (cdr (assoc 11 line3)))
          (setq line4 (entget line2))
          (setq ep3 (cdr (assoc 10 line4)))
          (setq ep4 (cdr (assoc 11 line4)))
          (setq d1 (distance ep1 ep3))
          (setq d2 (distance ep1 ep4))
          (setq d3 (distance ep2 ep3))
          (setq d4 (distance ep2 ep4))
          (setq smallest d1)
          (if (< d2 smallest) (setq smallest d2))
          (if (< d3 smallest) (setq smallest d3))
          (if (< d4 smallest) (setq smallest d4))
          (if (or (= smallest d1) (= smallest d2))
             (progn
                (setq p1 ep2)
                (if (= smallest d1)
                   (setq p2 ep4)
                   (setq p2 ep3)
                )
             )
          )
          (if (or (= smallest d3) (= smallest d4))
             (progn
                (setq p1 ep1)
                (if (= smallest d3)
                   (setq p2 ep4)
                   (setq p2 ep3)
                )
             )
          )
          (entdel line2)
          (command "change" line1 "" p2)
       )
       (prompt "\007\nRequires two lines - try again ")
 ))
   

**********************************************************************
;WEIGHT CALCULATION LISP.*********************************************
************************* 
 ;WEIGHT CALCULATION FOR RCC BARS
 (Defun c:RW()(setvar "cmdecho" 0)
              (setq tl (getvar "luprec"))
              (setvar "luprec" 2)
              (if (= typ nil)
              (setq typ (getreal "\n *** Enter DIA OF BAR...")))
              (setq typ1 typ)
              (setq typ (getint (strcat "\n *** Enter DIA OF BAR < TOR "(rtos typ1)" >: ")))
              (if (= typ nil) (setq typ typ1))
              (setq lenc (getreal "\n *** Enter bar length in mm...."))
              (setq cqty (getreal "\n *** Enter NO OF BARS...."))
              (setq lenc1 (/ lenc 1000))
              (if (= typ 6) (setq pw 0.225))
              (if (= typ 8) (setq pw 0.400))  
              (if (= typ 10) (setq pw 0.625))  
              (if (= typ 12) (setq pw 0.900))  
              (if (= typ 16) (setq pw 1.600))  
              (if (= typ 20) (setq pw 2.500))  
              (if (= typ 25) (setq pw 3.906))  
              (if (= typ 30) (setq pw 5.625))  
              (if (= typ 32) (setq pw 6.400))  
              (if (= typ 34) (setq pw 7.225)) 
              (if (= typ 36) (setq pw 8.100))  
              (setq ans (* lenc1 cqty pw))
              (setq tc (entsel "\n ** Select Text...."))
              (command "change" tc "" "" "" "" "" "" (rtos ans))
              (setvar "luprec" tl)(princ))
**********************************************************************
  ; TO CALCULATE THE WELD SIZE WHEN FORCES ARE GIVEN 
(DEFUN C:WELD ()
(SETQ R NIL L NIL) 
(WHILE (= R NIL) (SETQ R (GETREAL "\nEnter reaction:")))
(SETQ N (GETREAL "\nEnter axial force <0>:"))
(WHILE (= L NIL) (SETQ L (GETREAL "\nEnter length of plate:")))
(SETQ W (GETREAL "\nEnter width of plate <13>:"))
(IF (= W NIL) (SETQ W 13))
(IF (= N NIL) (SETQ N 0))
(SETQ M  (* R W)
      A  (* 2 0.707 L)
      Z  (/ (* 0.707 L L) 3)
      RS (/ R A)
      MS (/ M Z)
      NS (/ N A)
      HS (+ MS NS)
      S  (SQRT (+ (* HS HS) (* RS RS)))
      WD (* 10 (/ S 0.88))
      WD1 (FIX WD))                                                     
(IF (/= WD WD1) (IF (> WD 20) (SETQ WD1 (+ 1 WD1)) (SETQ WD1 (* 2 (1+ (FIX (/ WD1 2)))))))
(IF (< WD 6) (SETQ WD1 6))
(PRINC "\nCorrect value is ") (princ WD) 
(PRINC "\nProvide ") (princ wd1) (princ "mm weld.") (PRINC))
______________________________________________________________
(defun c:Ow ()
 (princ " ** Weight Calculation Program By GANESH AGARE.")
 (setq thk (getreal "\n * Enter Plate Thickness : "))
 (setq qty (getreal "\n * Enter Quantity : "))
 (setq scl (getreal "\n * Scale Of View <Ref. 1:ACTUAL OBJECT SCALE> : "))
 (setvar "cmdecho" 1)
 (Command "area" "e" pause)
 (setvar "cmdecho" 0)
 (setvar "luprec" 2)
 (setq ar (getvar "area"))
 (setq vr1 (/ ar 1000000))
 (setq wt (* vr1 thk 7.85 qty))
 (setq dsc (* scl scl))
 (setq awt (/ wt dsc))
 (setq 1p (/ awt qty))
 (princ "\n * Weight = \n")
 (print awt)
 (princ "\n 1 qty = \n")
 (print 1p)
 (setvar "luprec" 0))
--------------------------------------------------------------------
 WEIGHT CALCULATION FOR CHANNEL
 (Defun c:cw()(setvar "cmdecho" 0)
              (setq tl (getvar "luprec"))
              (setvar "luprec" 1)
              (if (= typ nil)
              (setq typ (getreal "\n *** Enter Channel size...")))
              (setq typ1 typ)
              (setq typ (getint (strcat "\n *** Enter Channel size < ISMC "(rtos typ1)" >: ")))
              (if (= typ nil) (setq typ typ1))
              (setq lenc (getreal "\n *** Enter Channel Lenght...."))
              (setq cqty (getreal "\n *** Enter Channel Quantity's...."))
              (setq lenc1 (/ lenc 1000))
              (if (= typ 75) (setq pw 6.8))
              (if (= typ 100) (setq pw 9.2))  
              (if (= typ 125) (setq pw 12.7))  
              (if (= typ 150) (setq pw 16.4))  
              (if (= typ 175) (setq pw 19.1))  
              (if (= typ 200) (setq pw 22.1))  
              (if (= typ 225) (setq pw 25.9))  
              (if (= typ 250) (setq pw 30.4))  
              (if (= typ 300) (setq pw 35.8))  
              (if (= typ 350) (setq pw 42.1)) 
              (if (= typ 400) (setq pw 49.4))  
              (setq ans (* lenc1 cqty pw))
              (setq tc (entsel "\n ** Select Text...."))
              (command "change" tc "" "" "" "" "" "" (rtos ans))
              (setvar "luprec" tl)(princ))
-----------------------------------------------------------------------
;WEIGHT CALCULATION FOR ISMB
 (Defun c:bw()(setvar "cmdecho" 0)
              (setq tl (getvar "luprec"))
              (setvar "luprec" 2)
              (if (= typ nil)
              (setq typ (getreal "\n *** Enter Beam size...")))
              (setq typ1 typ)
              (setq typ (getint (strcat "\n *** Enter Beam size < ISMB "(rtos typ1)" >: ")))
              (if (= typ nil) (setq typ typ1))
              (setq lenc (getreal "\n *** Enter Beam Lenght...."))
              (setq cqty (getreal "\n *** Enter Beam Quantity's...."))
              (setq lenc1 (/ lenc 1000))
              (if (= typ 100) (setq pw 11.5))
              (if (= typ 125) (setq pw 13.0))  
              (if (= typ 150) (setq pw 14.9))  
              (if (= typ 175) (setq pw 19.3))  
              (if (= typ 200) (setq pw 25.4))  
              (if (= typ 225) (setq pw 31.2))  
              (if (= typ 250) (setq pw 37.3))  
              (if (= typ 300) (setq pw 44.2))  
              (if (= typ 350) (setq pw 52.4))  
              (if (= typ 400) (setq pw 61.6)) 
              (if (= typ 450) (setq pw 72.4))  
              (setq ans (* lenc1 cqty pw))
              (setq tc (entsel "\n ** Select Text...."))
              (command "change" tc "" "" "" "" "" "" (rtos ans))
              (setvar "luprec" tl)(princ))
*--------------------------------------------------------------------
;WEIGHT CALCULATION FOR ANGLE
 (Defun c:Aw()(setvar "cmdecho" 0)
              (setq tl (getvar "luprec"))
              (setvar "luprec" 2)
              (if (= typ nil)
              (setq typ (getreal "\n *** Enter ANGLE size...")))
              (setq typ1 typ)
              (setq typ (getint (strcat "\n *** Enter ANGLE size < ANG "(rtos typ1)" >: ")))
              (if (= typ nil) (setq typ typ1))
              (setq lenc (getreal "\n *** Enter ANGLE Lenght...."))
              (setq cqty (getreal "\n *** Enter ANGLE Quantity's...."))
              (setq lenc1 (/ lenc 1000))
              (if (= typ 203) (setq pw 0.9))
              (if (= typ 204) (setq pw 1.1))
              (if (= typ 253) (setq pw 1.1))  
              (if (= typ 254) (setq pw 1.4))  
              (if (= typ 255) (setq pw 1.8))  
              (if (= typ 303) (setq pw 1.4))  
              (if (= typ 304) (setq pw 1.8))  
              (if (= typ 305) (setq pw 2.2))
              (if (= typ 353) (setq pw 1.6))
              (if (= typ 354) (setq pw 2.1))
              (if (= typ 355) (setq pw 2.6))
              (if (= typ 356) (setq pw 3.0))
              (if (= typ 403) (setq pw 1.8))  
              (if (= typ 404) (setq pw 2.4))  
              (if (= typ 405) (setq pw 3.0)) 
              (if (= typ 406) (setq pw 3.5)) 
              (if (= typ 453) (setq pw 2.1))
              (if (= typ 454) (setq pw 2.7)) 
              (if (= typ 455) (setq pw 3.4))
              (if (= typ 456) (setq pw 4.0))
              (if (= typ 503) (setq pw 2.3))
              (if (= typ 504) (setq pw 3.0))
              (if (= typ 505) (setq pw 3.8))
		  (if (= typ 506) (setq pw 4.5))
		  (if (= typ 555) (setq pw 4.1))
		  (if (= typ 556) (setq pw 4.9))
		  (if (= typ 558) (setq pw 6.4))
		  (if (= typ 5510) (setq pw 7.9))
		  (if (= typ 605) (setq pw 4.5))
		  (if (= typ 606) (setq pw 5.4))
		  (if (= typ 608) (setq pw 7.0))
		  (if (= typ 6010) (setq pw 8.6))
		  (if (= typ 655) (setq pw 4.9))
		  (if (= typ 656) (setq pw 5.8))
		  (if (= typ 658) (setq pw 7.7))
		  (if (= typ 6510) (setq pw 9.4))
		  (if (= typ 705) (setq pw 5.3))
		  (if (= typ 706) (setq pw 6.3))
		  (if (= typ 708) (setq pw 8.3))
		  (if (= typ 7010) (setq pw 10.2))
		  (if (= typ 755) (setq pw 5.7))
		  (if (= typ 756) (setq pw 6.8))
		  (if (= typ 758) (setq pw 8.9))
		  (if (= typ 7510) (setq pw 11.0))
		  (if (= typ 806) (setq pw 7.3))
		  (if (= typ 808) (setq pw 9.6))
		  (if (= typ 8010) (setq pw 11.8))
 		  (if (= typ 8012) (setq pw 14.0))
		  (if (= typ 906) (setq pw 8.2))
		  (if (= typ 908) (setq pw 10.8))
		  (if (= typ 9010) (setq pw 13.4))
		  (if (= typ 9012) (setq pw 15.8))
		  (if (= typ 1006) (setq pw 9.2))
		  (if (= typ 1008) (setq pw 12.1))
		  (if (= typ 10010) (setq pw 14.9))
		  (if (= typ 10012) (setq pw 17.7))
		  (if (= typ 1108) (setq pw 13.4))
		  (if (= typ 11010) (setq pw 16.5))
		  (if (= typ 11012) (setq pw 19.7))
		  (if (= typ 11016) (setq pw 25.7))
		  (if (= typ 1308) (setq pw 15.9))
		  (if (= typ 13010) (setq pw 19.7))
		  (if (= typ 13012) (setq pw 23.5))
		  (if (= typ 13016) (setq pw 30.7))
		  (if (= typ 15010) (setq pw 22.9))
		  (if (= typ 15012) (setq pw 27.3))
		  (if (= typ 15016) (setq pw 35.8))
		  (if (= typ 15020) (setq pw 44.1))
		  (if (= typ 20012) (setq pw 36.9))
		  (if (= typ 20016) (setq pw 48.5))
		  (if (= typ 20020) (setq pw 60.0))
		  (if (= typ 20025) (setq pw 73.9))
              (setq ans (* lenc1 cqty pw))
              (setq tc (entsel "\n ** Select Text...."))
              (command "change" tc "" "" "" "" "" "" (rtos ans))
              (setvar "luprec" tl)(princ))
------------------------------------------------------------
(Defun c:gr()(princ "\n Programe for Grating weight...40kg.pm.")
         (setq len (getreal "\n Enter Lenght..."))
         (setq wd (getreal "\n Enter Width..."))
              (setq qt (getint "\n Enter Quantity..."))
              (setq len1 (/ len 1000))
         (setq wd1 (/ wd 1000))
              (setq ans1 (* len1 wd1 qt 40))
              (setq ans (rtos ans1))
              (setq ch (entsel "\n Select Text to Change..."))
              (command "change" ch "" "" "" "" "" "" ans)(princ))
*---------------------------------------------------------------------
 ;TO CALCULATE BASE PLATE,INSERTPLATE,CAP PLATE,ETC
 (defun c:FW ()
 (princ " ** Weight Calculation Program By GANESH AGARE")
 (setvar "luprec" 2)
 (setq len (getreal "\n * Length = "))
 (setq wdt (getreal "\n * Width = "))
 (setq tk (getreal "\n * Thickness = "))
 (initget (+ 1 2 4))
 (setq qt (getreal "\n * Quantity = "))
 (setq wt (* len wdt tk 7.85 qt))
 (setq ewt (/ wt 1000000))
 (setq swt (rtos ewt))
 (setq 1f (rtos (/ ewt qt)))
 (setvar "luprec" 0)
 (setq sqt (rtos qt))
 (setq nas (strcat " * " swt " kgs. for Qty. " sqt " no(s) & "  1f " for 1 no. !!"))
 (princ nas)
 (princ))
*--------------------------------------------------------------------
;TO CALCULATE BASE PLATE,INSERTPLATE,CAP PLATE,ETC
(defun c:Pw ()
(princ " ** Weight Calculation Program By GANESH AGARE")
(setvar "luprec" 2)
(setq len (getreal "\n * Length = "))
(setq wdt (getreal "\n * Width = "))
(setq tk (getreal "\n * Thickness = "))
(initget (+ 1 2 4))
(setq qt (getreal "\n * Quantity = "))
(setq wt (* len wdt tk 7.85 qt))
(setq ewt (/ wt 1000000))
(setq swt (rtos ewt))
(setq 1f (rtos (/ ewt qt)))
(setvar "luprec" 0)
(setq sqt (rtos qt))
(setq obj (ssget))
(command "change" obj "" "" "" "" "" "" swt)
(setq nas (strcat " * " swt " kgs. for Qty. " sqt " no(s) & "  1f " for 1 no. !!"))
(princ nas)
(princ))

 (defun c:REw ()
 (princ " ** Weight Calculation Program By GANESH AGARE.")
 (setvar "luprec" 2)
 (setq ood (getreal "\n * o/d = "))
 (setq id (getreal "\n * i/d = "))
 (setq tk (getreal "\n * Thickness = "))
 (initget (+ 1 2 4))
 (setq qt (getreal "\n * Quantity = "))
 (setq wt1 (* (* 0.7854 (* ood ood)) tk 7.85 qt))
 (setq wt2 (* (* 0.7854 (* id id)) tk 7.85 qt))
 (setq ewt1 (/ wt1 1000000))
 (setq ewt2 (/ wt2 1000000))
 (setq twt (- ewt1 ewt2))
 (setq swt (rtos twt))
 (setq 1rw (rtos (/ twt qt)))
 (setvar "luprec" 0)
 (setq sqt (rtos qt))
 (setq nas (strcat " * " swt " kgs. for Qty. " sqt " no(s) & "  1rw " for 1 no. !!"))
 (princ nas)(princ)
 )
*********************************************************************
;TWO IN ONE COMMAND(MUTIPLE).lisp
---------------------------------
 (DEFUN C:CMm ()(princ "\n*** Copy & Mirror Program by GANESH AGARE.")
(PRINC"\n * Select Entities ...")
(SETQ SS (SSGET))
(SETQ BP (GETPOINT "\n * Enter Base Point For Copy & First Point For Mirror:"))
(PRINC "\n * Enter Second Point For Copy :")
(COMMAND "MOVE" SS "" BP pause)
(SETQ LP (GETVAR "LASTPOINT"))
(COMMAND "COPY" "P" "" LP BP)
(PRINC "\n * Enter Second Point For Mirror :")
(princ)
(COMMAND "MIRROR" SS "" LP pause "y")
)

(DEFUN C:CR ()(princ "\n*** Copy & Rotate Program by GANESH AGARE")
(PRINC"\n * Select Entities ...")
(SETQ SS (SSGET))
(SETQ BP (GETPOINT "\n * Enter Base Point For Copy & Rotate :"))
(PRINC "\n * Enter Second Point For Copy :")
(COMMAND "MOVE" SS "" BP pause)
(SETQ LP (GETVAR "LASTPOINT"))
(COMMAND "COPY" "P" "" LP BP)
(princ)
(COMMAND "Rotate" SS "" LP)
)
 
(DEFUN C:CS()(princ "\n*** Copy & Scale Program by GANESH AGARE.")
(PRINC"\n * Select Entities ...")
(SETQ SS (SSGET))
(SETQ BP (GETPOINT "\n * Enter Base Point For Copy & Scale :"))
 (PRINC "\n * Enter Second Point For Copy :")
 (COMMAND "MOVE" SS "" BP pause)
 (SETQ LP (GETVAR "LASTPOINT"))
 (COMMAND "COPY" "P" "" LP BP)

 (princ)

 (COMMAND "Scale" SS "" LP)
 )

(DEFUN C:CE()(princ "\n*** Copy & EDIT Program by GANESH AGARE.")
(PRINC"\n * Select Entities ...")
(SETQ SS (SSGET))
(SETQ BP (GETPOINT "\n * Enter Base Point For Copy & EDIT :"))
 (PRINC "\n * Enter Second Point For Copy :")
 (COMMAND "COPY" SS "" BP pause)
 (SETQ LP (GETVAR "BASEPOINT"))
 (COMMAND "DDEDIT" "P" "" LP BP)

 (princ)

 (COMMAND "DDEDIT" SS "" LP)
 )

 
 (DEFUN C:MR ()(princ "\n*** Move & Rotate  Program by GANESH AGARE.")
 (PRINC"\n * Select Entities ...")
 (SETQ SS (SSGET))
 (SETQ BP (GETPOINT "\n * Enter Base Point For Move & Rotate:"))
 (PRINC "\n * Enter Second Point For Move :")
 (COMMAND "MOVE" SS "" BP pause)
 (SETQ RP (GETVAR "LASTPOINT"))
 (princ)
 (COMMAND "Rotate" SS "" Rp)
 )
 
 (DEFUN C:MBR ()(princ "\n*** Multiline Break Program by GANESH AGARE.")
 (SETVAR "CMDECHO" 0)
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (SETQ FP (GETPOINT "\n * FIRST BREAK POINT :"))
     (SETQ SP (GETPOINT "\n * SECOND BREAK POINT :"))
     (setq ctr 0)
 
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (COMMAND "BREAK" ee1 fp sp)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
;multiple move
--------------
(defun c:mm()(princ "\nSelect First Set....")
       (setq se1 (ssget))
       (princ "\nSelect Second Set....")
       (setq se2 (ssget))
       (command "move" se1 "" pause pause) 
       (command "move" se2 "" pause pause))
----------------------------------------------------------
 ;MEXTEND.LSP---->MULTIPLE EXTEND
 (defun c:me (/ CE SS LS NO Pnt1 Pnt2)
    (command "undo" "g")
    (prompt "\nSelect Edge for Multiple extend... ")(prin1)
    (setq CE (ssget))
    (setvar "highlight" 1)
 (prompt "\nSelect Objects to extend by Crossing Window... ")(prin1)
    (
 setq Pnt1 (getpoint "\nFirst Corner... "))
    (setq Pnt2 (getcorner "\nOther Corner... " Pnt1))
    (setq SS (ssget "c" Pnt1 Pnt2)
   LS (sslength SS)
   NO -1
    )
    (command "extend" CE "")
    (repeat LS
      (setq NO (+ 1 NO))
      (command (list (ssname SS NO) Pnt2))
    )
    (command "")
    (command "undo" "e") (prin1)
    (prompt "\nME Complete.") (prin1)
 )
 ;---------------------------------------------------------------------------------!!!
 ;MTRIM.LSP----> MULTIPLE TRIM
 (defun c:mt (/ CE SS LS NO Pnt1 Pnt2)
    (command "undo" "g")
    (prompt "\nSelect Edge for MT... ")(prin1)
    (setq CE (ssget))
    (setvar "highlight" 1)
 (prompt "\nSelect Objects to Trim by Crossing Window... ")(prin1)
    (
 setq Pnt1 (getpoint "\nFirst Corner... "))
    (setq Pnt2 (getcorner "\nOther Corner... " Pnt1))
    (setq SS (ssget "c" Pnt1 Pnt2)
   LS (sslength SS)
   NO -1
    )
    (command "trim" CE "")
    (repeat LS
      (setq NO (+ 1 NO))
      (command (list (ssname SS NO) Pnt2))
    )
    (command "")
    (command "undo" "e") (prin1)
    (prompt "\nMT Complete.") (prin1)
 )
 -----------------------------------------------------------
  (defun c:cm()(setq se1 (ssget))(command "copy" se1 "" "m"))
  (defun c:cmE()(setq se1 (ssget))(command "copy" se1 "" "m" "DDEDIT" "P")) 
  ---------------------------------------------------------------
(defun c:MF()(princ "\n ** Multiple Fillet Program, By GANESH AGARE.")
 (setvar "cmdecho" 0)
 (princ "\n *** Select First Set....")
 (SETQ SS1 (ssget))
 (princ "\n *** Select Second Set....")    
 (SETQ SS2 (ssget))
 (IF SS1 
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (SETQ SSL1 (SSLENGTH SS2))     
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq ee2 (ssname SS2 ctr))       
       (COMMAND "Fillet" (princ ee1) "" (princ ee2))(princ)
       (setq ctr (+ 1 ctr))
 ) 
  )  
      )  
 (princ))
 
***************************************************************** 
(defun c:DEV()
 (graphscr)
 (setvar "cmdecho" 0)
 (princ "\n ** Cone Develpment Lisp Program, By GANESH AGARE.")
 (setq bdia (getreal "\n * Enter Bigger Dia. : "))
 (setq sdia (getreal "\n * Enter Smaller Dia. : "))
 (setq tht (getreal "\n * Enter The Height : "))
 (setq dia (- bdia sdia))
 ;;;(print dia)
 (setq dht (* 2 tht))
 ;;;(print dht)
 (setq vtan (/ dia dht))
 ;;;(print vtan)
 (setq itan (atan vtan))
 (setq antan (* 180(/ itan 3.1415927)))
 ;;;(print antan)
 (setq rtan (* 3.1415927(/ antan 180)))
 ;;;(print rtan)
 (setq sang (sin rtan))
 ;;;(print sang)
 (setq dsin (* 2 sang))
 ;;;(print dsin)
 (setq brad (/ bdia dsin))
 ;;;(print brad)
 (setq srad (/ sdia dsin))
 ;;;(print srad)
 (setq afa (* sang 360))
 ;;;(print afa)
 (setq safa (sin afa))
 (setq hafa (/ afa 2))
 (setq shafa (rtos hafa))
 (setq nhafa (strcat "-" shafa))
 (setq diff (- brad srad))
 (setq dst (rtos diff))
 (setq llen (strcat "@" dst "<270"))
 (setq pt (getpoint "\n * Enter Location Point For Development : "))
 (setq xpt (car pt))
 (setq ypt (cadr pt))
 (setq cypt (+ ypt brad))
 (setq sxpt (rtos xpt))
 (setq scypt (rtos cypt))
 (setq cxy (strcat sxpt "," scypt))
 (command "circle" pt brad)
 (setq cs1 (ssget "l"))
 (command "circle" pt srad)
 (setq cs2 (ssget "l"))
 (command "line" cxy llen "")
 (command "rotate" "l" "" pt nhafa)
 (setq ss1 (ssget "p"))
 (command "copy" "p" "" pt pt)
 (command "rotate" "l" "" pt afa)
 (setq ss2 (ssget "p"))
 (command "redraw")
 (setq ns1 (ssname ss1 0))
 (setq ns2 (ssname ss2 0))
 (setq es1 (entget ns1))
 (setq es2 (entget ns2))
 (setq fltp (cdr (assoc 10 es1)))
 (setq flbp (cdr (assoc 11 es1)))
 (setq sltp (cdr (assoc 10 es2)))
 (setq slbp (cdr (assoc 11 es2)))
 (command "osnap" "int")
 (command "break" cs1 sltp fltp)
 (command "break" cs2 slbp flbp)
 (command "osnap" "off"))
 ;;***
_____________________________________________________________
 ;MC.LISP--> DRAWS ALL CHANNELS (ISMC)
 (defun C:MC()
   (setq   propmc  ' ( ( 75.0    40.0   7.3    4.4   8.5   4.5)
         (100.0    50.0   7.5    4.7   9.0   4.5)
         (125.0    65.0   8.1    5.0   9.5   5.0)
         (150.0    75.0   9.0    5.4  10.0   5.0)
         (175.0    75.0  10.2    5.7  10.5   5.0)
         (200.0    75.0  11.4    6.1  11.0   5.5)
         (250.0    80.0  14.1    7.1  12.0   6.0)
         (300.0    90.0  13.6    7.6  13.0   6.5)
         (350.0   100.0  13.5    8.1  14.0   7.0)
         (400.0   100.0  15.3    8.6  15.0   7.5)
       )
      )
   (setq d        (getreal "Enter Channel size..."))
   (setq wf     (nth 1 (assoc d propmc)))
   (setq tf     (nth 2 (assoc d propmc)))
   (setq tw     (nth 3 (assoc d propmc)))
   (setq frroot (nth 4 (assoc d propmc)))
   (setq frtoe  (nth 5 (assoc d propmc))) 
   (setq rpt (getstring "reference point ? H (for heel) or C (for web centre)"))
   (setq ornt(getstring "Orientation ? U (|___|) D ( |~~~|) L ( ]) R ( [ )"))
   (setq v2 (- wf tw )   v3 (* v2 0.1051042 )   )
   (setq x  (- tf (* 0.1051042 0.5 v2)))
   (setq v4 (- d x x v3 v3)     )
   (setq wtd (getpoint "Where to draw ? "))
   (if  ( or  (= ornt "L")  (= ornt "l") )
     (progn
         
       (setq xc (car wtd) )
       (if (or (= rpt "H")  (= rpt "h") ) (setq yc (cadr wtd)) 
       (setq yc (- (cadr wtd) (* 0.5 d)))
       )
       (setq p7 (list xc yc) )
       (setq xc (- xc wf)                                  p0 (list xc yc) )
       (setq  yc (+ yc x )                                 p1 (list xc yc) )
       (setq     xc (+ xc v2)     yc (+ yc v3)             p2 (list xc yc) )
       (setq                      yc (+ yc v4)             p3 (list xc yc) )
       (setq     xc (- xc v2  )   yc (+ yc v3)             p4 (list xc yc) )
       (setq                      yc (+ yc x )             p5 (list xc yc) )
       (setq     xc (+ xc wf)                              p6 (list xc yc) )
     )
   )
   (if  (or  (= ornt "R")  (= ornt "r") )
     (progn
     
       (setq xc (car wtd) )
       (if  ( or (= rpt "H") (= rpt "h") ) (setq yc (cadr wtd)) 
        (setq yc (- (cadr wtd) (* 0.5 d)))
       )
       (setq p7 (list xc yc) )
       (setq xc (+ xc wf)                                  p0 (list xc yc) )
       (setq  yc (+ yc x )                                 p1 (list xc yc) )
       (setq     xc (- xc v2)     yc (+ yc v3)             p2 (list xc yc) )
       (setq                      yc (+ yc v4)             p3 (list xc yc) )
       (setq     xc (+ xc v2  )   yc (+ yc v3)             p4 (list xc yc) )
       (setq                      yc (+ yc x )             p5 (list xc yc) )
       (setq     xc (- xc wf)                              p6 (list xc yc) )
     )
   )
   (if  (or (= ornt "U") (= ornt "u") )
     (progn
     
       (setq yc (cadr wtd))
       (if ( or (= rpt "H") (= rpt "h") )   (setq xc (car wtd)) 
         (setq xc (- (car wtd) (* 0.5 d)))
       )
       (setq p7 (list xc yc) )
       (setq yc (+ yc wf)                                  p0 (list xc yc) )
       (setq  xc (+ xc x )                                 p1 (list xc yc) )
       (setq     xc (+ xc v3)     yc (- yc v2)             p2 (list xc yc) )
       (setq     xc (+ xc v4)                              p3 (list xc yc) )
       (setq     xc (+ xc v3)     yc (+ yc v2)             p4 (list xc yc) )
       (setq     xc (+ xc x )                              p5 (list xc yc) )
       (setq                      yc (- yc wf)             p6 (list xc yc) )
     )
   )
   (if ( or (= ornt "D") (= ornt "d") )
     (progn
     
       (setq yc (cadr wtd))
       (if ( or (= rpt "H") (= rpt "h") )  (setq xc (car wtd)) 
        (setq xc (- (car wtd) (* 0.5 d)))
       )
       (setq p7 (list xc yc) )
       (setq yc (- yc wf)                                  p0 (list xc yc) )
       (setq  xc (+ xc x )                                 p1 (list xc yc) )
       (setq     xc (+ xc v3)     yc (+ yc v2)             p2 (list xc yc) )
       (setq     xc (+ xc v4)                              p3 (list xc yc) )
       (setq     xc (+ xc v3)     yc (- yc v2)             p4 (list xc yc) )
       (setq     xc (+ xc x )                              p5 (list xc yc) )
       (setq                      yc (+ yc wf)             p6 (list xc yc) )
     )
   )
     (command "line" p0 p1 "")
     (setq e1 (entlast)     ss1 (ssadd)  )
     (ssadd e1 ss1 )
     (command "line" p1 p2 "")
     (setq e2 (entnext e1))
     (ssadd e2 ss1)
     (command "fillet" "r" frtoe)
     (command "fillet" e1 e2)
     (ssdel e1 ss1)
     (command "line" p2 p3 "")
     (setq e3 (entlast))
     (ssadd e3 ss1)
     (command "fillet" "r" frroot)
     (command "fillet" e2 e3)
     (ssdel e2 ss1)
     (command "line" p3 p4 "")
     (setq e4 (entlast))
     (ssadd e4 ss1)
     (command "fillet" e3 e4)
     (ssdel e3 ss1)
     (command "line" p4 p5 "")
     (setq e5 (entlast))
     (ssadd e5 ss1)
     (command "fillet" "r" frtoe)
     (command "fillet" e4 e5)
     (ssdel e4 ss1)
     (ssdel e5 ss1)
     (command "line" p5 p6 p7 p0 "")
     (command "redraw") 
 )
 ;;;-----------------
;MB.LISP--> DRAWS ALL BEAMS (ISMB)

 (defun c:MB()
   (setq   propmb  ' ( (100.0    75.0    7.2   4.0   5.0   5.0)
                       (125.0    75.0    7.6   4.4   9.5   9.5)
                       (150.0    80.0    7.6   4.8   9.0   4.5)
                       (175.0    90.0    8.6   5.5  10.0   5.0)
                       (200.0   100.0   10.8   5.7  11.0   5.5)
                       (225.0   110.0   11.8   6.5  12.0   6.0)
                       (250.0   125.0   12.5   6.9  13.0   6.5)
         (300.0   140.0   12.4   7.5  14.0   7.0)
         (350.0   140.0   14.2   8.1  14.0   7.0)
         (400.0   140.0   16.0   8.9  14.0   7.0)
         (450.0   150.0   17.4   9.4  15.0   7.5)
         (500.0   180.0   17.2  10.2  17.0   8.5)
         (550.0   190.0   19.3  11.2  18.0   9.0) 
         (600.0   210.0   20.8  12.0  20.0  10.0) )
               
   )
   (setq d  (getreal "which one "))
   (setq wf     (nth 1 (assoc d propmb)))
   (setq tf     (nth 2 (assoc d propmb)))
   (setq tw     (nth 3 (assoc d propmb)))
   (setq frroot (nth 4 (assoc d propmb)))
   (setq frtoe  (nth 5 (assoc d propmb))) 
     (setq v1 (- wf tw ) v2 (* 0.5 v1 ) v3 (* v2 0.1405408 )   )
     (setq x  (- tf (* 0.1405408 0.5 v2)))
     (setq v4 (- d x x v3 v3)     hf (* 0.5 wf)  )
     (setq wtd (getpoint "where to draw ? "))
     (setq xc (- (car wtd) hf)   yc (cadr wtd) )
     (setq p0 (list xc yc) )
     (setq  yc (+ yc x )   )
     (setq p1 (list xc yc) )
     (setq     xc (+ xc v2)       yc (+ yc v3)             p2 (list xc yc) )
     (setq                        yc (+ yc v4)             p3 (list xc yc) )
     (setq     xc (- xc v2  )     yc (+ yc v3)             p4 (list xc yc) )
     (setq                        yc (+ yc x )             p5 (list xc yc) )
     (setq     xc (+ xc wf)                                p6 (list xc yc) )
     (setq                        yc (- yc x)              p7 (list xc yc) )
     (setq     xc (- xc v2 )      yc (- yc v3)             p8 (list xc yc) )
     (setq                        yc (- yc v4)             p9 (list xc yc) )
     (setq     xc (+ xc v2)       yc (- yc v3)            p10 (list xc  yc )) 
     (setq                        yc (- yc x)             p11 (list xc  yc )) 
     (command "line" p0 p1 "")
     (setq e1 (entlast)     ss1 (ssadd)  )
     (ssadd e1 ss1 )
     (command "line" p1 p2 "")
     (setq e2 (entnext e1))
     (ssadd e2 ss1)
     (command "fillet" "r" frtoe)
     (command "fillet" e1 e2)
     (ssdel e1 ss1)
     (command "line" p2 p3 "")
     (setq e3 (entlast))
     (ssadd e3 ss1)
     (command "fillet" "r" frroot)
     (command "fillet" e2 e3)
     (ssdel e2 ss1)
     (command "line" p3 p4 "")
     (setq e4 (entlast))
     (ssadd e4 ss1)
     (command "fillet" e3 e4)
     (ssdel e3 ss1)
     (command "line" p4 p5 "")
     (setq e5 (entlast))
     (ssadd e5 ss1)
     (command "fillet" "r" frtoe)
     (command "fillet" e4 e5)
     (ssdel e4 ss1)
     (ssdel e5 ss1)
     (command "line" p5 p6 p7 "")
     (setq e6 (entlast))
     (ssadd e6 ss1)
     (command "line" p7 p8 "")
     (setq e7 (entlast))
     (ssadd e7 ss1)
     (command "fillet" e6 e7)
     (ssdel e6 ss1)
     (command "line" p8 p9 "")
     (setq e8 (entlast))
     (ssadd e8 ss1)
     (command "fillet" "r" frroot)
     (command "fillet" e7 e8)
     (ssdel e7 ss1)
     (command "line" p9 p10 "")
     (setq e9 (entlast))
     (ssadd e9 ss1)
     (command "fillet" e8 e9)
     (ssdel e8 ss1)
     (command "line" p10 p11 "")
     (setq e10 (entlast))
     (ssadd e10 ss1)
     (command "fillet" "r" frtoe)
     (command "fillet" e9 e10)
     (ssdel e9 ss1)
     (ssdel e10 ss1)
     (command "line" p11 p0 "")
     (command "redraw") 
 )\'1a

 
;******************************************************************************
;                 PROGRAMME TO DRAW SORF OR WNRF NOZZLE.
;******************************************************************************
(defun dtr (a)
  (* (/ a 180.0) pi)
)
 
(defun sorf()
   (setq cm (getvar "cmdecho"))
	(setq bm (getvar "blipmode"))
	(setq os (getvar "osmode"))
	(setvar "cmdecho" 0)
   (setvar "blipmode" 0)
   (setvar "osmode" 0)
 (initget 1 "150 300")
 (setq lbs (getkword "\nSpecify rating in Lbs. < 150 or 300 > : "))
      (if (= lbs "150")
          (setq data (list '(0.5  21.3  88.9   11.1  15.9  30.2  34.9  60.3)
                           '(0.75 26.7  98.4   12.7  15.9  38.1  42.9  69.8)
                           '(1    33.4  107.9  14.3  17.5  49.2  50.8  79.4)
                           '(1.25 42.2  117.5  15.9  20.6  58.7  63.5  88.9)
                           '(1.5  48.3  127.0  17.5  22.2  65.1  73.0  98.4)
                           '(2    60.3  152.4  19.0  25.4  77.8  92.1  120.6)
                           '(2.5  73.0  177.8  22.2  28.6  90.5  104.8 139.7)
                           '(3    88.9  190.5  23.8  30.2  107.9 127.0 152.4)
                           '(3.5  101.6 215.9  23.8  31.7  122.2 139.7 177.8)
                           '(4    114.3 228.6  23.8  33.3  134.9 157.2 190.5)
                           '(5    141.3 254.0  23.8  36.5  163.9 185.7 215.9)
                           '(6    168.3 279.4  25.4  39.7  192.1 215.9 241.3)
                           '(8    219.1 342.9  28.6  44.4  246.1 269.9 298.4)
                           '(10   273.0 406.4  30.2  49.2  304.8 323.8 361.9)
                           '(12   323.8 482.6  31.7  55.6  365.1 381.0 431.8)
                           '(14   355.6 533.4  34.9  57.1  400.1 412.7 476.2)
                           '(16   406.4 596.9  36.5  63.5  457.2 469.9 539.7)
                           '(18   457.2 635.0  39.7  68.3  504.8 533.4 577.8)
                           '(20   508.0 698.5  42.9  73.0  558.8 584.2 635.0)
                           '(24   609.6 812.8  47.6  82.5  663.6 692.1 749.3)
                       )
         );setq ends
         (setq data (list '(0.5   21.3  95.2   14.3  22.2  38.1  34.9  66.7)
                          '(0.75  26.7  117.5  15.9  25.4  47.6  42.9  82.5)
                          '(1     33.4  123.8  17.5  27.0  54.0  50.8  88.9)
                          '(1.25  42.2  133.3  19.0  27.0  63.5  63.5  98.4)
                          '(1.5   48.3  155.6  20.6  30.2  69.8  73.0  114.3)
                          '(2     60.3  165.1  22.2  33.3  84.1  92.1  127.0)
                          '(2.5   73.0  190.5  25.4  38.1  100.0 104.8 149.2)
                          '(3     88.9  209.5  28.6  42.9  117.5 127.0 168.3)
                          '(3.5   101.6 228.6  30.2  44.4  133.3 139.7 184.1)
                          '(4     114.3 254.0  31.7  47.6  146.0 157.2 200.1)
                          '(5     141.3 279.4  34.9  50.8  177.8 185.7 234.9)
                          '(6     168.3 317.5  36.5  52.4  206.4 215.9 269.9)
                          '(8     219.1 381.0  41.3  61.9  260.3 269.9 330.2)
                          '(10    273.0 444.5  47.6  66.7  320.7 323.8 387.7)
                          '(12    323.8 520.7  50.8  73.0  374.6 381.0 450.8)
                          '(14    355.6 584.2  54.0  76.2  425.4 412.7 514.3)
                          '(16    406.4 647.7  57.1  82.5  482.6 469.9 571.5)
                          '(18    457.2 711.2  60.3  88.9  533.4 533.4 628.6)
                          '(20    508.0 774.7  63.5  95.2  587.4 584.2 685.8)
                          '(24    609.6 914.4  69.8  106.4 701.7 692.1 812.8)
                     )
         );setq ends
      );if ends
    (setq od (getreal "\nGive Nozzle Size [Inches]: ")
        data1 (assoc od data)
    )
  (if data1
    (progn
    (setq pt (getpoint "\nGive insertion point: "))
   (initget 1 "P E") 
   (setq que (getkword "\nSpecify Plan or Elev. < P or E > : "))
   (if (= que "P")
       (plan) 
       (progn 
         (setq ang (getangle "\nGive rotation angle: " pt))
         (if (not scl)
	  (setq scl 1)
   )
   (setq prj (/ (getreal "\nGive projection [ mm ]: ") scl)
         od (/ (/ (nth 1 data1) 2.0) scl)
         d1 (/ (/ (nth 2 data1) 2.0) scl)
         d2 (/ (/ (nth 6 data1) 2.0) scl)
         d3 (/ (/ (nth 5 data1) 2.0) scl)
         rf (/ 1.6 scl)
         t  (- (/ (nth 3 data1) scl) rf)
         web (- (/ (nth 4 data1) scl) t)
         pt1 (polar pt (+ ang (dtr 90.0)) od )
         pt2 (polar pt (- ang (dtr 90.0)) od )
         pt3 (polar pt1 ang (- prj (+ web rf t)))
         pt4 (polar pt2 ang (- prj (+ web rf t)))
         pt5 (polar pt3 (+ ang (dtr 90.0)) (- d3 od))
         pt6 (polar pt4 (- ang (dtr 90.0)) (- d3 od))
         pt7 (polar pt5 ang web)
         pt8 (polar pt6 ang web)
         pt9 (polar pt7 (+ ang (dtr 90.0)) (- d1 d3))
         pt10 (polar pt8 (- ang (dtr 90.0)) (- d1 d3))
         pt11 (polar pt9 ang t)
         pt12 (polar pt10 ang t)
         pt13 (polar pt11 (- ang (dtr 90.0)) (- d1 d2))
         pt14 (polar pt12 (+ ang (dtr 90.0)) (- d1 d2))
         pt15 (polar pt13 ang rf)
         pt16 (polar pt14 ang rf)
         pt17 (polar pt ang (+ prj 20.0))
         pt18 (polar pt (+ ang (dtr 180.0)) 5)
         pt19 (polar pt ang (+ prj 25.0))
   );setq ends
   (command "line" pt1 pt3 "")(setq e1 (entlast))
   (command "line" pt2 pt4 "")(setq e2 (entlast))
   (command "line" pt5 pt7 ""
            "line" pt6 pt8 "")
   (command "line" pt9 pt11 "")(setq e3 (entlast))
   (command "line" pt10 pt12 "")(setq e4 (entlast))
   (command "line" pt13 pt15 "")(setq e5 (entlast))
   (command "line" pt14 pt16 "")(setq e6 (entlast))
   (command "line" pt5 pt6 "")
   (command "line" pt9 pt10 "")(setq e7 (entlast))
   (command "line" pt11 pt12 "")(setq e8 (entlast))
   (command "line" pt15 pt16 "")
;           "line" pt17 pt18 ""
   (command "change" "L" "" "P" "lt" "center" "c" "m" ""
            "change" "l" "" "p" "c" "w" ""
   )
    );progn ends
   );if ends
  );progn ends
 );if ends
   (if (= data1 nil)
     (princ "Non Standard Pipe O.D. ")
        (progn
         (initget 1 "Y N")
           (setq BFLG(getkword "\nDo You Want B.F. ? [ Yes Or No ] : "))
                     (if (= bflg "Y")
                          (bf)
                     )
                      (if (= bflg "N")
                             (setq data nil)
                      )
    (initget 1 "Y N")
        (setq rfpad (getkword "\nDo You Want R.F. Pad ? [ Yes Or No ] : "))
                      (if (= rfpad "Y")
                        (pad)
                      )
                    (if (= rfpad "N")
                          (setq data nil)
                     )
          );progn ends
   );if ends
	(setvar "cmdecho" cm)
   (setvar "blipmode" bm)
   (setvar "osmode" os)
     (setq data1 nil)
     (princ)
);func ends

(defun wnrf()
   (setq cm (getvar "cmdecho"))
	(setq bm (getvar "blipmode"))
	(setq os (getvar "osmode"))
	(setvar "cmdecho" 0)
   (setvar "blipmode" 0)
   (setvar "osmode" 0)
    (initget 1 "150 300")
    (setq lbs (getkword "\nSpecify Rating in Lbs. < 150 or 300 > : "))
    (if (= lbs "150")
       (setq data (list '(0.5  21.3  88.9   11.1  47.6  30.2  34.9  60.3)
                        '(0.75 26.7  98.4   12.7  52.4  38.1  42.9  69.8)
                        '(1    33.4  107.9  14.3  55.6  49.2  50.8  79.4)
                        '(1.25 42.2  117.5  15.9  57.1  58.7  63.5  88.9)
                        '(1.5  48.3  127.0  17.5  61.9  65.1  73.0  98.4)
                        '(2    60.3  152.4  19.0  63.5  77.8  92.1  120.6)
                        '(2.5  73.0  177.8  22.2  69.8  90.5  104.8 139.7)
                        '(3    88.9  190.5  23.8  69.8  107.9 127.0 152.4)
                        '(3.5  101.6 215.9  23.8  71.4  122.2 139.7 177.8)
                        '(4    114.3 228.6  23.8  76.2  134.9 157.2 190.5)
                        '(5    141.3 254.0  23.8  88.9  163.9 185.7 215.9)
                        '(6    168.3 279.4  25.4  88.9  192.1 215.9 241.3)
                        '(8    219.1 342.9  28.6  101.6  246.1 269.9 298.4)
                        '(10   273.0 406.4  30.2  101.6  304.8 323.8 361.9)
                        '(12   323.8 482.6  31.7  114.3  365.1 381.0 431.8)
                        '(14   355.6 533.4  34.9  127.0  400.1 412.7 476.2)
                        '(16   406.4 596.9  36.5  127.0  457.2 469.9 539.7)
                        '(18   457.2 635.0  39.7  139.7  504.8 533.4 577.8)
                        '(20   508.0 698.5  42.9  144.5 558.8 584.2 635.0)
                        '(24   609.6 812.8  47.6  152.4 663.6 692.1 749.3)
                   )
       );setq ends.
    (setq data (list '(0.5  21.3  95.2   14.3  52.4  38.1  34.9  66.7)
                     '(0.75 26.7  117.5  15.9  57.1  47.6  42.9  82.5)
                     '(1    33.4  123.8  17.5  61.9  54.0  50.8  88.9)
                     '(1.25 42.2  133.3  19.0  65.1  63.5  63.5  98.4)
                     '(1.5  48.3  155.6  20.6  68.3  69.8  73.0  114.3)
                     '(2    60.3  165.1  22.2  69.8  84.1  92.1  127.0)
                     '(2.5  73.0  190.5  25.4  76.2  100.0 104.8 149.2)
                     '(3    88.9  209.5  28.6  79.4  117.5 127.0 168.3)
                     '(3.5  101.6 228.6  30.2  81.0  133.3 139.7 184.1)
                     '(4    114.3 254.0  31.7  85.7  146.0 157.2 200.1)
                     '(5    141.3 279.4  34.9  98.4  177.8 185.7 234.9)
                     '(6    168.3 317.5  36.5  98.4  206.4 215.9 269.9)
                     '(8    219.1 381.0  41.3  111.1 260.3 269.9 330.2)
                     '(10   273.0 444.5  47.6  117.5 320.7 323.8 387.7)
                     '(12   323.8 520.7  50.8  130.2 374.6 381.0 450.8)
                     '(14   355.6 584.2  54.0  142.9 425.4 412.7 514.3)
                     '(16   406.4 647.7  57.1  146.0 482.6 469.9 571.5)
                     '(18   457.2 711.2  60.3  158.7 533.4 533.4 628.6)
                     '(20   508.0 774.7  63.5  161.9 587.4 584.2 685.8)
                     '(24   609.6 914.4  69.8  168.3 701.7 692.1 812.8)
                 )
    );setq ends
   );if ends
  (setq od (getreal "\nGive Nozzle Size [Inches]: ")
        data1 (assoc od data)
  )
  (if data1
   (progn
   (setq pt (getpoint "\nGive insertion point: "))
   (initget 1 "P E") 
   (setq que (getkword "\nSpecify Plan or Elev < P or E > : "))
   (if (= que "P")
       (plan) 
       (progn
          (setq ang (getangle "\nGive rotation angle: " pt))
          (if (not scl)
		(setq scl 1.0)
	  )
   (setq prj (/ (getreal "\nGive projection: ") scl)
         od (/ (/ (nth 1 data1) 2.0) scl)
         d1 (/ (/ (nth 2 data1) 2.0) scl)
         d2 (/ (/ (nth 6 data1) 2.0) scl)
         d3 (/ (/ (nth 5 data1) 2.0) scl)
         rf (/ 1.6 scl)
         t (- (/ (nth 3 data1) scl) rf)
         neck (- (/ (nth 4 data1) scl) (+ t rf))
         pipe (- prj (/ (nth 4 data1) scl))
         pt1 (polar pt (+ ang (dtr 90.0)) od )
         pt2 (polar pt (- ang (dtr 90.0)) od )
         pt3 (polar pt1 ang (- prj (+ neck rf t)))
         pt4 (polar pt2 ang (- prj (+ neck rf t)))
         pt5 (polar pt ang (+ pipe neck))
         pt7 (polar pt5 (+ ang (dtr 90.0)) d3)
         pt8 (polar pt5 (- ang (dtr 90.0)) d3)
         pt9 (polar pt7 (+ ang (dtr 90.0)) (- d1 d3))
         pt10 (polar pt8 (- ang (dtr 90.0)) (- d1 d3))
         pt11 (polar pt9 ang t)
         pt12 (polar pt10 ang t)
         pt13 (polar pt11 (- ang (dtr 90.0)) (- d1 d2))
         pt14 (polar pt12 (+ ang (dtr 90.0)) (- d1 d2))
         pt15 (polar pt13 ang rf)
         pt16 (polar pt14 ang rf)
         pt17 (polar pt ang (+ prj 20.0))
         pt18 (polar pt (+ ang (dtr 180.0)) 5.0)
         pt19 (polar pt ang (+ prj 25.0))
   )
	(command "line" pt1 pt3 "")(setq e1 (entlast))
   (command "line" pt2 pt4 "")(setq e2 (entlast))
   (command "line" pt3 pt7 ""
            "line" pt4 pt8 "")
   (command "line" pt9 pt11 "")(setq e3 (entlast))
   (command "line" pt10 pt12 "")(setq e4 (entlast))
   (command "line" pt13 pt15 "")(setq e5 (entlast))
   (command "line" pt14 pt16 "")(setq e6 (entlast))
   (command "line" pt3 pt4 "")
   (command "line" pt9 pt10 "")(setq e7 (entlast))
   (command "line" pt11 pt12 "")(setq e8 (entlast))
   (command "line" pt15 pt16 "")
   (command "change" "L" "" "P" "lt" "center" "c" "m" ""
            "change" "l" "" "p" "c" "w" ""
   )
      );progn ends
     );if ends
    );progn ends
  );if ends
    (if (= data1 nil)
     (princ "Non Standard Pipe O.D. ")
          (progn
           (initget 1 "Y N")
             (setq bflg(getkword "\nDo You Want B.F. ? [ Yes Or No ] : "))
                     (if (= bflg "Y")
                          (bf)
                     )
                      (if (= bflg "N")
                             (setq data nil)
                      )
    (initget 1 "Y N")
        (setq rfpad (getkword "\nDo You Want R.F. Pad ? [ Yes Or No ] : "))
                      (if (= rfpad "Y")
                        (pad)
                      )
                    (if (= rfpad "N")
                          (setq data nil)
                     )
       );progn ends
   );if ends
	(setvar "cmdecho" cm)
   (setvar "blipmode" bm)
   (setvar "osmode" os)
     (setq data1 nil)
     (princ)
);func ends

(defun PAD()
   (setq cm (getvar "cmdecho"))
	(setq bm (getvar "blipmode"))
	(setq os (getvar "osmode"))
	(setvar "cmdecho" 0)
   (setvar "blipmode" 0)
   (setvar "osmode" 0)
    (setq padw (/ (getint "\nGive Pad Width :") scl)
          padth (/ (getint "\nGive Pad Thickness :") scl)
          pad1 (polar pt1 (+ ang (dtr 90.0)) padw)
          pad2 (polar pad1 ang padth)
          pad3 (polar pt2 (- ang (dtr 90.0)) padw)
          pad4 (polar pad3 ang padth)
          wl1 (polar pad1 (+ ang (dtr 90.0)) padth)
          wl2 (polar pad3 (- ang (dtr 90.0)) padth)
          wl3 (polar pt1 ang padth)
          wl4 (polar pt2 ang padth)
          wl5 (polar wl3 (+ ang (dtr 90.0)) padth)
          wl6 (polar wl4 (- ang (dtr 90.0)) padth)
          wl7 (polar wl3 ang padth)
          wl8 (polar wl4 ang padth)
     )
         (command "line" pad1 pad2 pad4 pad3 ""
               "break" e1 pad1 pad2
               "break" e2 pad3 pad4
               "solid" pad1 wl1 pad2 "" ""
               "solid" pad3 wl2 pad4 "" ""
               "solid" wl3 wl5 wl7 "" ""
               "solid" wl4 wl6 wl8 "" ""
         )
);func ends

   (defun plan()
      (setq scl 1.0) 
      (setq od (/ (/ (nth 1 data1) 2.0) scl)) 
      (setq d1 (/ (/ (nth 2 data1) 2.0) scl))
      (command "circle" pt od
               "circle" pt d1)
   )

(defun C:NOZ()
 
    (initget 1 "S W")
       (setq type (getkword "\nType Of Flange [ Wnrf or Sorf ] : "))
          (if (= type "W")
            (wnrf)
          )
            (if (= type "S")
               (sorf)
            )
 )
(princ)
; *****************************************************************************
 
 (DEFUN C:DISH()
   (SETVAR "ORTHOMODE" 0)
   (COMMAND "OSNAP" "")
   (SETQ  P1 (GETPOINT "\nENTER DEEPEST POINT:")
          ID (GETREAL "\nENTER I/D OF DISH:")
          SF (GETREAL "\nENTER SF:")
          TH (GETREAL "\nENTER THK:")
          CR (* 0.9045 ID)
          KR (* 0.1727 ID)
          D1 (- (/ ID 2.0) KR)
          R1 (- CR KR)
          R2 (EXPT (- (* R1 R1) (* D1 D1)) 0.5)
          AG (ATAN D1 R2)
                  CE (LIST (CAR P1) (- (CADR P1) CR))
          P2 (LIST (+ (CAR P1) (* CR (SIN AG))) (- (CADR P1) (- CR (* CR (COS AG)))))
          P3 (LIST (+ (CAR P1) (/ ID 2)) (- (CADR P1) (- cr r2)))
          P4 (LIST (+ (CAR P1) (/ ID 2)) (- (CADR P3) SF))
                  P22 (LIST (+ (CAR P2) 20) (CADR P2))
                  p44 (list (+ (car p4) th) (cadr p4))
   )
     (COMMAND "PLINE" P1 "A" "D" P1 P2 P3 "L" P4 ""
              "OFFSET" TH P2 P22 ""
                          "line" p44 (list (car p1) (cadr p4)) ""
              "MIRROR" "C" P4 P1 "" P1 CE ""
              "OSNAP" "INT,CEN"
         )
          (SETVAR "ORTHOMODE" 1)
)

	(defun ozdr1 () 
	(command "line" "" ozdir1 ozdir1 "")
	(setq ip1 (getvar "lastpoint"))
	(command "erase" "l" "")
        (command "insert" "ITEM" ip1 sc1 "" "")
	)

	(defun zdr1 ()
	(command "line" "" zdir1 zdir1 "")
	(setq ip1 (getvar "lastpoint"))
	(command "erase" "l" "")
        (command "insert" "ITEM" ip1 sc1 "" "")
	)
-------------------------------------------------------------------------
;;;chtext.lisp 

 ; Next available MSG number is   83 
; MODULE_ID CHTEXT_LSP_
;;;
;;;    CHTEXT.lsp - change text
;;;
;;;    Copyright 1997 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;  
;;;--------------------------------------------------------------------------;
;;; DESCRIPTION
;;;   This is a "text processor" which operates in a global manner 
;;;   on all of the text entities that the user selects; i.e., the
;;;   Height, Justification, Location, Rotation, Style, Text, and
;;;   Width can all be changed globally or individually, and the
;;;   range of values for a given parameter can be listed.
;;;   
;;;   The command is called with CHT from the command line at which
;;;   time the user is asked to select the objects to change.
;;;   
;;;     Select text to change. 
;;;     Select objects:
;;;  
;;;   If nothing is selected the message "ERROR: Nothing selected." 
;;;   is displayed and the command is terminated.  If more than 25
;;;   entities are selected the following message is displayed while
;;;   the text entities are sorted out from the non-text entities.
;;;   A count of the text entities found is then displayed.
;;;   
;;;     Verifying the selected entities...
;;;     nnn  text entities found.
;;;     CHText:  Height/Justification/Location/Rotation/Style/Text/Undo/Width:
;;;   
;;;   A typical example of the prompts you may encounter follows:
;;;   
;;;   If you select a single text entity to change and ask to change
;;;   the height, the prompt looks like this:
;;;   
;;;     CHText:  Height/Justification/Location/Rotation/Style/Text/Undo/Width:
;;;     New text height for text entity. <0.08750000>:
;;;   
;;;   If you select more than one text entity to change and ask to change
;;;   the height, the prompt looks like this:
;;;   
;;;     CHText:  Height/Justification/Location/Rotation/Style/Text/Undo/Width:
;;;     Individual/List/<New height for all entities>:
;;;   
;;;   Typing "L" at this prompt returns a prompt showing you the range of
;;;   values that you are using for your text.
;;;   
;;;     Height -- Min: 0.05000000  Max: 0.10000000  Ave: 0.08392857
;;;   
;;;   Typing "I" at this prompt puts you in a loop, processing the text
;;;   entities you have selected one at a time, and giving the same prompt
;;;   you get for a single text entity shown above.
;;;   
;;;   Pressing ENTER at this point puts you back at the Command: prompt.
;;;   Selecting any of the other options allows you to change the text
;;;   entities selected. 
;;;   
;;;---------------------------------------------------------------------------;

(defun cht_Main ( / sset opt ssl nsset temp unctr ct_ver sslen style hgt rot
                    txt ent loc loc1 just-idx justp justq orthom
                    cht_ErrorHandler cht_OrgError cht_OrgCmdecho
                    cht_OrgTexteval cht_OrgHighlight)

  ;; Reset if changed
  (setq ct_ver "2.00")

  ;; Internal error handler defined locally
  (defun cht_ErrorHandler (s)
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ)
        (princ (strcat "\nError: " s))
      )
    )
    (eval (read U:E))
    ;;  Reset original error handler if there
    (if cht_OrgError (setq *error* cht_OrgError))
    (if temp (redraw temp 1))
    (ai_undo_off) ;; restore undo state
    (if cht_OrgCmdecho (setvar "cmdecho" cht_OrgCmdecho))
    (if cht_OrgTexteval (setvar "texteval" cht_OrgTexteval)) 
    (if cht_OrgHighlight (setvar "highlight" cht_OrgHighlight)) 
    (princ)
  )

  ;; Set error handler
  (if *error*
    (setq cht_OrgError *error*
          *error* cht_ErrorHandler)
    (setq *error* cht_ErrorHandler) 
  )

  ;; Set undo groups and ends with (eval(read U:G)) or (eval(read U:E))
  (setq U:G "(command \"_.undo\" \"_group\")"
        U:E "(command \"_.undo\" \"_en\")"
  )
  
  (ai_undo_on)       ;; enable undo
  
  (setq cht_OrgCmdecho (getvar "cmdecho"))
  (setq cht_OrgHighlight (getvar "highlight")) 
  (setvar "cmdecho" 0)
  
  (princ (strcat "\nChange text, Version "
                 ct_ver 
                 ", Copyright © 1997 by Autodesk, Inc."))
  (prompt "\nSelect annotation objects to change.")
  (setq sset (ai_aselect))
  (if (null sset) 
    (progn
      (princ "\nNo objects selected.")
      (exit)
    )
  )
  ;; Validate selection set
  (setq ssl   (sslength sset)
        nsset (ssadd))
  (if (> ssl 25)
    (princ "\nVerifying selected objects...")
  )
  (while (> ssl 0)
    (setq temp (ssname sset (setq ssl (1- ssl))))
    (if (or
          (= (cdr (assoc 0 (entget temp))) "TEXT")
          (= (cdr (assoc 0 (entget temp))) "ATTDEF")
          (= (cdr (assoc 0 (entget temp))) "MTEXT")
        )
      (ssadd temp nsset)
    )
  )
  (setq ssl (sslength nsset)
        sset nsset
        unctr 0
  )
  (print ssl)
  (princ "annotation objects found.")

  ;; Main loop
  (setq opt T)
  (while (and opt (> ssl 0))
    (setq unctr (1+ unctr))
    (command "_.UNDO" "_GROUP")
    (initget "Location Justification Style Height Rotation Width Text Undo")
    (setq opt (getkword 
      "\nHeight/Justification/Location/Rotation/Style/Text/Undo/Width: "))
    (if opt
      (cond
        ((= opt "Undo")
          (cht_Undo)
        )
        ((= opt "Location")
          (cht_Location)
        )
        ((= opt "Justification")
          (cht_Justification)
        )
        ((= opt "Style")
          (cht_Property "Style"    "New style name"      7) )
        ((= opt "Height")
          (cht_Property "Height"   "New height"         40) )
        ((= opt "Rotation")
          (cht_Property "Rotation" "New rotation angle" 50) )
        ((= opt "Width")
          (cht_Property "Width"    "New width factor"   41) )
        ((= opt "Text")
          (cht_Text)
        )
      )
      (setq opt nil)
    )
    (command "_.UNDO" "_END")
  )

  ;; Restore
  (if cht_OrgError (setq *error* cht_OrgError))
  (eval (read U:E))
  (ai_undo_off) ;; restore undo state
  (if cht_OrgTexteval (setvar "texteval" cht_OrgTexteval)) 
  (if cht_OrgHighlight (setvar "highlight" cht_OrgHighlight)) 
  (if cht_OrgCmdecho (setvar "cmdecho" cht_OrgCmdecho))
  (princ)
)

;;; Undo an entry
(defun cht_Undo ()
  (if (> unctr 1)
    (progn
      (command "_.UNDO" "_END")
      (command "_.UNDO" "2")
      (setq unctr (- unctr 2))
    )
    (progn
      (princ "\nNothing to undo. ")
      (setq unctr (- unctr 1))
    )
  )
)

;;; Change the location of an entry
(defun cht_Location ()
  (setq sslen (sslength sset)
        style ""
        hgt   ""
        rot   ""
        txt   ""
  )
  (command "_.CHANGE" sset "" "")
  (while (> sslen 0)
    (setq ent (entget(ssname sset (setq sslen (1- sslen))))
          opt (list (cadr (assoc 11 ent))
                    (caddr (assoc 11 ent))
                    (cadddr (assoc 11 ent)))
    )
    (prompt "\nNew text location: ")
    (command pause)
    (if (null loc)
      (setq loc opt)
    )
    (command style hgt rot txt)
  )
  (command)
)

;;; Change the justification of an entry
(defun cht_Justification ()
  (initget "TL TC TR ML MC MR BL BC BR Align Center Fit Left Middle Right ?")
  (setq sslen (sslength sset))
  (setq justp (getkword "\nAlign/Fit/Center/Left/Middle/Right/TL/TC/TR/ML/MC/MR/BL/BC/BR/<?>: "))
  (cond
    ((= justp "Left")    (setq justp 0 justq 0 just-idx 4) )
    ((= justp "Center")  (setq justp 1 justq 0 just-idx 5) )
    ((= justp "Right")   (setq justp 2 justq 0 just-idx 6) )
    ((= justp "Align")   (setq justp 3 justq 0 just-idx 1) )
    ((= justp "Fit")     (setq justp 5 justq 0 just-idx 1) )
    ((= justp "TL")      (setq justp 0 justq 3 just-idx 1) )
    ((= justp "TC")      (setq justp 1 justq 3 just-idx 2) )
    ((= justp "TR")      (setq justp 2 justq 3 just-idx 3) )
    ((= justp "ML")      (setq justp 0 justq 2 just-idx 4) )
    ((= justp "Middle")  (setq justp 4 justq 0 just-idx 5) )
    ((= justp "MC")      (setq justp 1 justq 2 just-idx 5) )
    ((= justp "MR")      (setq justp 2 justq 2 just-idx 6) )
    ((= justp "BL")      (setq justp 0 justq 1 just-idx 7) )
    ((= justp "BC")      (setq justp 1 justq 1 just-idx 8) )
    ((= justp "BR")      (setq justp 2 justq 1 just-idx 9) )
    ((= justp "?")       (setq justp nil)       )
    (T                   (setq justp nil)       )
  )   
  (if justp
    (progn ;; Process them...
      (while (> sslen 0)
        (setq ent (entget (ssname sset (setq sslen (1- sslen)))))
        (cond
          ((= (cdr (assoc 0 ent)) "MTEXT")
            (setq ent (subst (cons 71 just-idx) (assoc 71 ent) ent))
          )
          ((= (cdr (assoc 0 ent)) "TEXT")
            (setq ent (subst (cons 72 justp) (assoc 72 ent) ent)
                  opt (trans (list (cadr (assoc 11 ent))
                                   (caddr (assoc 11 ent))
                                   (cadddr (assoc 11 ent))) 
                             (cdr (assoc -1 ent)) ;; from ECS
                             1)               ;; to current UCS
            )
            (setq ent (subst (cons 73 justq) (assoc 73 ent) ent))
            (cond
              ((or (= justp 3) (= justp 5))
                (prompt "\nNew text alignment points: ")
                (if (= (setq orthom (getvar "orthomode")) 1)
                  (setvar "orthomode" 0)
                )
                (redraw (cdr (assoc -1 ent)) 3)
                (initget 1)
                (setq loc (getpoint))
                (initget 1)
                (setq loc1 (getpoint loc))
                (redraw (cdr (assoc -1 ent)) 1)
                (setvar "orthomode" orthom)
                (setq ent (subst (cons 10 loc) (assoc 10 ent) ent))
                (setq ent (subst (cons 11 loc1) (assoc 11 ent) ent))
              )
              ((or (/= justp 0) (/= justq 0))
                (redraw (cdr (assoc -1 ent)) 3)
                (prompt "\nNew text location: ")
                (if (= (setq orthom (getvar "orthomode")) 1)
                  (setvar "orthomode" 0)
                )
                (setq loc (getpoint opt))
                (setvar "orthomode" orthom)
                (redraw (cdr (assoc -1 ent)) 1)
                (if (null loc)
                  (setq loc opt)
                  (setq loc (trans loc 1 (cdr (assoc -1 ent))))
                )
                (setq ent (subst (cons 11 loc) (assoc 11 ent) ent))
              )
            )
          )
        )
        (entmod ent)
      )
    )
    (progn	;; otherwise list options
      (textpage)
      (princ "\nAlignment options:\n")
      (princ "\t  TL     TC      TR\n")
      (princ "\t  ML     MC      MR\n")
      (princ "\t  BL     BC      BR\n")
      (princ "\t Left   Center  Right\n")
      (princ "\tAlign   Middle  Fit\n")
      (princ "\nPress ENTER to continue: ")
      (grread)
      (princ "\r                                                            ")
      (graphscr)
    )
  )
  (command)
)

;;; Change the text of an object
(defun cht_Text ( / ans)
  (setq sslen (sslength sset))
  (initget "Globally Individually Retype")
  (setq ans (getkword 
    "\nFind and replace text.  Individually/Retype/<Globally>:"))
  (setq cht_OrgTexteval (getvar "texteval"))
  (setvar "texteval" 1)
  (cond 
    ((= ans "Individually")
      (progn
        (initget "Yes No")
        (setq ans (getkword "\nEdit text in dialog? <Yes>:"))
      )
  
      (while (> sslen 0)
        (redraw (setq sn (ssname sset (setq sslen (1- sslen)))) 3)
        (setq ss (ssadd))
        (ssadd (ssname sset sslen) ss)
        (if (= ans "No") 
          (cht_Edit ss)
          (command "_.DDEDIT" sn "")
        )
        (redraw sn 1)
      )
    )
    ((= ans "Retype")
      (while (> sslen 0)
        (setq ent (entget (ssname sset (setq sslen (1- sslen)))))
        (redraw (cdr (assoc -1 ent)) 3)
        (prompt (strcat "\nOld text: " (cdr (assoc 1 ent))))
        (setq nt (getstring  T "\nNew text: "))
        (redraw (cdr (assoc -1 ent)) 1)
        (if (> (strlen nt) 0)
          (entmod (subst (cons 1 nt) (assoc 1 ent) ent))
        )
      )
    )
    (T
      (cht_Edit sset)   ;; Change all
    )
  )
  (setvar "texteval" cht_OrgTexteval)
)

;;; The old CHGTEXT command - rudimentary text editor
(defun C:CHGTEXT () (cht_Edit nil))

(defun cht_Edit (objs / last_o tot_o ent o_str n_str st s_temp 
                       n_slen o_slen si chf chm cont ans class)
  ;; Select objects if running standalone
  (if (null objs)
    (setq objs (ssget))
  )
  (setq chm 0)
  (if objs 
    (progn                   ;; If any objects selected
      (if (= (type objs) 'ENAME) 
        (progn
          (setq ent (entget objs))
          (princ (strcat "\nExisting string: " (cdr (assoc 1 ent))))
        )
        (if (= (sslength objs) 1)
          (progn
            (setq ent (entget (ssname objs 0)))
            (princ (strcat "\nExisting string: " (cdr (assoc 1 ent))))
          )
        )
      )
      (setq o_str (getstring "\nMatch string   : " t))
      (setq o_slen (strlen o_str))
      (if (/= o_slen 0)
        (progn
          (setq n_str (getstring "\nNew string     : " t))
          (setq n_slen (strlen n_str))
          (setq last_o 0 
                tot_o  (if (= (type objs) 'ENAME)
                         1
                         (sslength objs)
                       )
          )
          ;; For each selected object...
          (while (< last_o tot_o)
            (setq class (cdr (assoc 0 (setq ent (entget (ssname objs last_o))))))
            (if (or (= "TEXT" class)
                    (= "MTEXT" class) )
              (progn
                (setq chf nil si 1)
                (setq s_temp (cdr (assoc 1 ent)))
                (while (= o_slen (strlen (setq st (substr s_temp si o_slen))))
                  (if (= st o_str)
                    (progn
                      (setq s_temp (strcat 
                                     (if (> si 1)
                                       (substr s_temp 1 (1- si)) 
                                       ""
                                     )
                                     n_str
                                     (substr s_temp (+ si o_slen))
                                   )
                      )
                      (setq chf t)    ;; Found old string
                      (setq si (+ si n_slen))
                    )
                    (setq si (1+ si))
                  )
                )
                (if chf 
                  (progn              ;; Substitute new string for old
                    ;; Modify the TEXT entity
                    (entmod (subst (cons 1 s_temp) (assoc 1 ent) ent))
                    (setq chm (1+ chm))
                  )
                )
              )
            )
            (setq last_o (1+ last_o))
          )
        )
        ;; else go on to the next line...
      )
    )
  )
  (if (/= (type objs) 'ENAME)
    ;; Print total lines changed
    (if (/= (sslength objs) 1)
      (princ (strcat (rtos chm 2 0) " text lines changed."))
    )
  )
  (terpri)
)

;;; Main procedure for manipulating text entities
(defun cht_Property (typ prmpt fld / temp ow nw ent tw sty w hw lw 
                              sslen n sn ssl)
  (if (= (sslength sset) 1)           ;; Special case if there is only
                                      ;; one entity selected
    ;; Process one entity.
    (cht_ProcessOne)
    ;; Else
    (progn
      ;; Set prompt string.
      (cht_SetPrompt)
      (if (= nw "List")
        ;; Process List request.
        (cht_ProcessList)
        (if (= nw "Individual")
          ;; Process Individual request.
          (cht_ProcessIndividual)
          (if (= nw "Select")
            ;; Process Select request.
            (cht_ProcessSelect)
            ;; Else
            (progn
              (if (= typ "Rotation")
                (setq nw (* (/ nw 180.0) pi))
              )
              (if (= (type nw) 'STR)
                (if (not (tblsearch "style" nw))
                  (progn
                    (princ (strcat nw ": Style not found. "))
                  )
                  (cht_ProcessAll)
                )
                (cht_ProcessAll)
              )
            )
          )
        )
      )
    )
  )
)

;;; Change all of the entities in the selection set
(defun cht_ProcessAll (/ hl temp)
  (setq sslen (sslength sset))
  (setq hl (getvar "highlight"))
  (setvar "highlight" 0)
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent ) )
  )
  (setvar "highlight" hl)
)

;;; Change one text entity
(defun cht_ProcessOne ()
  (setq temp (ssname sset 0))
  (setq ow (cdr (assoc fld (entget temp))))
  (if (= opt "Rotation")
    (setq ow (/ (* ow 180.0) pi))
  )
  (redraw (cdr (assoc -1 (entget temp))) 3)
  (initget 0)
  (if (= opt "Style")
    (setq nw (getstring (strcat prmpt " <" ow ">: ")))
    (setq nw (getreal (strcat prmpt " <" (rtos ow 2) ">: ")))
  )
  (if (or (= nw "") (= nw nil))
    (setq nw ow)
  )
  (redraw (cdr (assoc -1 (entget temp))) 1)
  (if (= opt "Rotation")
    (setq nw (* (/ nw 180.0) pi))
  )
  (if (= opt "Style")
    (if (null (tblsearch "style" nw))
      (princ (strcat nw ": Style not found. "))
      (entmod (subst (cons fld nw)
                     (assoc fld (setq ent (entget temp)))
                     ent
              )
      )
    )
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent
            )
    )
  )
)

;;; Set the prompt string
(defun cht_SetPrompt ()
  (if (= typ "Style")
    (progn
      (initget "Individual List New Select ")
      (setq nw (getkword (strcat "\nIndividual/List/Select style/<"
                                 prmpt
                                 " for all text objects" ">: ")))
      (if (or (= nw "") (= nw nil) (= nw "Enter"))
        (setq nw (getstring (strcat prmpt
                                    " for all text objects" ": ")))
      )
    )
    (progn
      (initget "List Individual" 1)
      (setq nw (getreal (strcat "\nIndividual/List/<"
                                 prmpt
                                 " for all text objects" ">: ")))
    )
  )
)

;;; Process List request
(defun cht_ProcessList ()
  (setq unctr (1- unctr))
  (setq sslen (sslength sset))
  (setq tw 0)
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (if (= typ "Style")
      (progn
        (if (= tw 0)
          (setq tw (list (cdr (assoc fld (entget temp)))))
          (progn
            (setq sty (cdr (assoc fld (entget temp))))
            (if (not (member sty tw))
              (setq tw (append tw (list sty)))
            )
          )
        )
      )
      (progn
        (setq tw (+ tw (setq w (cdr (assoc fld (entget temp))))))
        (if (= (sslength sset) (1+ sslen)) (setq lw w hw w))
        (if (< hw w) (setq hw w))
        (if (> lw w) (setq lw w))
      )
    )
  )
  (if (= typ "Rotation")
    (setq tw (* (/ tw pi) 180.0)
          lw (* (/ lw pi) 180.0)
          hw (* (/ hw pi) 180.0))
  )
  (if (= typ "Style")
    (progn
      (princ (strcat "\n" typ "(s) -- "))
      (princ tw)
    )
    (princ (strcat "\n" typ
             " -- Min: " (rtos lw 2)
             "\t Max: " (rtos hw 2)
             "\t Avg: " (rtos (/ tw (sslength sset)) 2) ) )
  )
)

;;; Process Individual request
(defun cht_ProcessIndividual ()
  (setq sslen (sslength sset))
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (setq ow (cdr (assoc fld (entget temp))))
    (if (= typ "Rotation")
      (setq ow (/ (* ow 180.0) pi))
    )
    (initget 0)
    (redraw (cdr (assoc -1 (entget temp))) 3)
    (if (= typ "Style")
      (progn
        (setq nw (getstring (strcat "\n" prmpt " <" ow ">: ")))
      )
      (progn
        (setq nw (getreal (strcat "\n" prmpt " <" (rtos ow 2) ">: ")))
      )
    )
    (if (or (= nw "") (= nw nil))
      (setq nw ow)
    )
    (if (= typ "Rotation")
      (setq nw (* (/ nw 180.0) pi))
    )
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent
            )
    )
    (redraw (cdr (assoc -1 (entget temp))) 1)
  )
)

;;; Process the Select option
(defun cht_ProcessSelect ()
  (princ "\nSearch for which Style name?  <*>: ")
  (setq sn  (xstrcase (getstring))
        n   -1
        nsset (ssadd)
        ssl (1- (sslength sset))
        )
  (if (or (= sn "*") (null sn) (= sn ""))
    (setq nsset sset sn "*")
    (while (and sn (< n ssl))
      (setq temp (ssname sset (setq n (1+ n))))
      (if (= (cdr (assoc 7 (entget temp))) sn)
        (ssadd temp nsset)
      )
    )
  )

  (princ (strcat "\nStyle: " sn))
  (print (setq ssl (sslength nsset)))
  (princ "objects found.")
)

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.  If it can't be found or can't be
;;; loaded, then abort the loading of this file immediately.
(cond
  ((and ai_dcl (listp ai_dcl)))		; it's already loaded.
  ((not (findfile "ai_utils.lsp"))		; find it
    (ai_abort "CHT" nil)
  )
  ((eq "failed" (load "ai_utils" "failed"))	; load it
    (ai_abort "CHT" nil)
  )
)

;;; If we get this far, then AI_UTILS.LSP is loaded and it can
;;; be assumed that all functions defined therein are available.

;;; Next, check to see if ACADAPP.EXP has been xloaded, and abort
;;; if the file can't be found or xloaded.  Note that AI_ACADAPP
;;; does not abort the running application itself (so that it can
;;; also be called from within the command without also stopping
;;; an AutoCAD command currently in progress).
;(if (not (ai_acadapp)) (ai_abort "CHT" nil))

;;; The C: function definition
(defun c:88 () (cht_Main))
(princ)
------------------------------------------------------------------------------------------------
*********************************************************************************

(DEFUN DTR(a)
           (* pi (/ a 180.0 )) 
)
(DEFUN C:FD() 
(SETQ OSM (GETVAR "OSMODE"))

(setq p1 (getpoint "\nGIVE INTERSECTION PLease....: "))
(setq FOW (getreal "\nGIVE FOUNDATION WIDTH......:"))
(setq Foh (getreal "\nGIVE FOUNDATION HEIGHT..(d)....:"))
(setq Poh (getreal "\nGIVE PEDESTAL HEIGHT..(D)....:"))
(setq GAP (getreal "\nGIVE CLEAR COVER......:"))
(setq Coh (getreal "\nGIVE COLUMN HEIGHT......:"))
(setq COW (getreal "\nGIVE COLUMN SIZE......:"))
(setq GAP (getreal "\nGIVE CLEAR COVER......:"))
(setq Poh (getreal "\nGIVE PEDESTAL HEIGHT..(D).(*LEFT SIDE*)...:"))
(setq Foh (getreal "\nGIVE FOUNDATION HEIGHT..(d).(*LEFT SIDE*)...:"))
(command "osmode" 0 )
(setq aw (/ FOW 2 ))
(setq ah (/ FOH 2 ))
(setq p2 (POLAR P1 (DTR 270 ) AH ))
(setq p3 (POLAR P2 (DTR 0 ) AW ))
(setq p4 (POLAR P3 (DTR 90 ) FOH ))
(setq p5 (POLAR P4 (DTR 145 ) POH ))
(setq p6 (POLAR P5 (DTR 180 ) GAP ))
(setq p7 (POLAR P6 (DTR 90 ) COH ))
(setq p8 (POLAR P7 (DTR 180 ) COW ))
(setq p9 (POLAR P8 (DTR 270 ) COH ))
(setq p10 (POLAR P9 (DTR 180 ) GAP ))
(setq p11 (POLAR P10 (DTR 215 ) POH ))
(setq p12 (POLAR P11 (DTR 270 ) FOH ))
(command "PLINE" P3 P4 P5 P6 P7 P8 P9 P10 P11 P12 "C" "")
(SETVAR "osmode" OSM )
(COMMAND "REDRAW")
)

************************************************************************
(defun c:an ()
(setq p1 (getpoint "\n Enter lower left corner "))
(setq f (getdist "\n Enter flange width "))
(setq p2 (polar p1 0 f))
(setq p3 (polar p2 (/ pi 2) 3))
(setq c3 (polar p3 pi 3))
(setq p4 (polar c3 (/ pi 2) 3))
(setq p5 (polar p4 pi (- f 15)))
(setq c1 (polar p5 (/ pi 2) 6))
(setq p6 (polar c1 pi 6))
(setq p7 (polar p6 (/ pi 2) (- f 15)))
(setq c2 (polar p7 pi 3))
(setq p8 (polar c2 (/ pi 2) 3))
(setq p9 (polar p8 pi 3))
(command "pline" p1 p2 p3 "arc" p4 "line" p5 "arc" p6 "line" p7 "arc" p8 "line" p9 p1 "")
)


***********************************************************
streches text but keeps height of text as it is.

(Defun c:tf ( /
        setsnapang
        arg LL-xyz UR-x LL-y LR-xy
        ename
        TextEnt
        NewEnd
        TMP
        START NewPt
        Val LTC_%
        )
  (init_bonus_error
    (List
        (List "cmdecho" 0 "snapang" 0)
        T     ;flag. True means use undo for error clean up.  
     ) ;list  
  ) ;init_bonus_error

;;;End Error control

  (defun setsnapang (arg /)
    (setvar "snapang" (angtof (angtos (cdr (assoc 50 arg)) 0 8) 0 ))
  );end defun setsnapang

  (Defun LL-xyz (arg)                             ;Lower Left xyz coord
     (CAR (TextBox arg))
  )
  (Defun UR-x (arg)                               ;Upper Right x coord
     (CAADR (TextBox arg))
  )
  (Defun LL-y (arg)                               ;Lower left y coord
     (CADAR (TextBox arg))
  )
  (Defun LR-xy (arg)                              ;Lower right xy coord
     (List (UR-x arg) (LL-y arg))
  )

   (Setq
      ename (CAR
               (EntSel
                  "\nSelect Text to stretch/shrink:"
               )
            )

      Textent (If ename (EntGet ename))
   )   

   (If (= (CDR (Assoc 0 textent)) "TEXT")
      (Progn
         (initget 0 "Start")
         (Setq
            NewEnd  (Distance
                           (LR-xy Textent)
                           (LL-xyz Textent)
                        )
         )
         (setsnapang Textent)                 ;set snap along text entity
         (setvar "ORTHOMODE" 1)                   ;drag along the text
         (setq 
            TMP (getpoint (cdr (assoc 10 Textent)) "\nStarting Point/<Pick new ending point>: ")
         )
         (setvar "snapang" 0)
         (cond
           ((= (type TMP) 'STR) ;;new starting point to be selected
               (setq Start (getpoint "\nPick new starting point: "))
               (if Start 
                  (progn
                  (command "_UCS" "_E" (cdr (assoc -1 textent)))
                  (setvar "ORTHOMODE" 1)
                  (setq NewPt
                        (if Start
                            (getpoint (trans Start 0 1) " ending point: ")
                            nil
                        )
                  )
                  (if NewPt (setq NewPt (trans NewPt 1 0)))
                  (setvar "ORTHOMODE" 0)
                  (command "_UCS" "_W")
                  )
                )
           )
           ((not (null TMP))    ;;new ending point selected

               (setq Start (cdr (assoc 10 Textent))
                     NewPt TMP)
           )
           (t  (setq Start nil
                     NewPt nil)
           )
         )
         (if (and Start NewPt) (progn
           (setq Val (Assoc 41 Textent) ;;current width factor
                 Val (if Val (cdr Val) 1.0)
            
                 LTC_%       
                        (*
                           (/
                              (Distance Start NewPt)
                              NewEnd
                           )
                           Val
                        )
                 textent (Subst (cons 41 LTC_%)
                                    (assoc 41 textent)
                                    textent)
                 textent (subst (cons 10 Start)
                                    (assoc 10 textent)
                                    textent)
                 textent (subst (cons 11 NewPt)
                                    (assoc 11 textent)
                                    textent)
           )
           (EntMod textent)
           (EntUpd (cdr (assoc -1 textent)))
         ))  ;;end of points check
   ) ;End do if T progn
 )
 (restore_old_error)
 (Princ)
) ;end defun

(defun c:TFHELP (/)

(prompt " TEXTFIT will change the width factor of the selected text, \n")
(prompt " to fit within the user specified points.\n")
(prompt "\n")
(prompt " TEXTFIT will prompt:  Select Text to stretch/shrink:\n")
(prompt " The user is expected to select the text.\n")
(prompt "\n")
(prompt " TEXTFIT will then prompt:  Starting Point/<Pick new ending point>: \n")
(prompt " At which time the user can specify a new ending point \n")
(prompt "                          or\n")
(prompt " The user can provide the letter \"S\" for a new starting point\n")
(prompt " to which TEXTFIT will prompt:  Pick new starting point:  \n")
(prompt " and:  ending point: \n")
(textscr)
(princ)
)
*********************************************************************
********************************************************************************************
;Functions created as result of loading file: exchprop.lsp
; DDCHPROP2
; DDCHPROP2_INIT
; DDCHPROP2_SELECT
;
;Variables created as result of loading file: exchprop.lsp
; OLD_ALLOC
;
;Functions created as a result of executing the commands in: exchprop.lsp
;
;Variables created as a result of executing the commands in: exchprop.lsp
; AI_SELTYPE
; BONUS_ALIVE
; BONUS_OLD_ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Avoid (gc)s on load to improve load time.
;;;
(defun do_alloc (/ old_allod new_alloc)
  (setq old_alloc (alloc 2000) new_alloc (alloc 2000))
  (expand (1+ (/ 4750 new_alloc)))
  (alloc old_alloc)
);defun

;runs at load time - rk
(do_alloc)
(setq do_alloc nil)
;;;
;;; ===========================================================================
;;; ===================== load-time error checking ============================
;;;

  (defun ai_abort (app msg) 
     (defun *error* (s)
      (if old_error (setq *error* old_error))
      (princ)
     );defun
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       );alert
     );if
     ;(*error* msg)
     (exit)
  );defun ai_abort

;runs at load time - rk
;(if (and *error*      ;added the if wrapper around this - rk.
;         (not old_error)       
;    );and 
;    (setq old_error *error*);setq
;);if


;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

;runs at load time - rk.
(cond
 (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.
 (  (not (findfile "ai_utils.lsp"))                     ; find it
    (ai_abort "EXCHPROP"
              (strcat "Can't locate file AI_UTILS.LSP."
                      "\n Check support directory.")
    );ai_abort
 )
 (  (eq "failed" (load "ai_utils" "failed"))            ; load it
    (ai_abort "EXCHPROP" "Can't load file AI_UTILS.LSP")
 )
);cond close

;(if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
;    (ai_abort "EXCHPROP" nil)       ; a Nil <msg> supresses
;);if                                 ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================

;;; Initialize program subroutines and variables.

(defun ddchprop2_init()



  ;;
  ;; Define buttons and set values in CHPROP dialogue box
  ;;
  (defun call_chp2 (/ cmdact p1 p2)

    (if (not (new_dialog "ch_prop" dcl_id)) 
        (exit)
    )

    (set_tile "error" "")
    ;; Set initial dialogue tile values
    (set_col_tile)
    (set_tile "t_layer" elayer)

    (cond
      ((= lt-idx nil)
        (set_tile "t_ltype" "Varies")
      )
      ((= lt-idx 0) ; set tile "By layer & layer linetype"
        (set_tile "t_ltype" (bylayer_lt))
      )
      (T
        (set_tile "t_ltype" (nth lt-idx ltnmlst))
      )
    )

    (if (or (= ethickness nil)
            (= ethickness "")
            (= ethickness "Varies") 
        );or
        (set_tile "eb_thickness" "")
        (set_tile "eb_thickness" (ai_rtos ethickness))
    );if
    (if (or (= eltscale nil)
            (= eltscale "")
            (= eltscale "Varies")
 
        )
        (set_tile "eb_ltscale" "")
        (set_tile "eb_ltscale" (ai_rtos eltscale))
    )
    (if
      (numberp ewidth)
      (set_tile "poly_wid" (ai_rtos ewidth))
      (set_tile "poly_wid" ewidth)
    )
    (if
      (numberp eelevation)
      (set_tile "poly_elev" (ai_rtos eelevation))
      (set_tile "poly_elev" eelevation)
    )
    (if
      (numberp eheight)
      (set_tile "text_hgt" (ai_rtos eheight))
      (set_tile "text_hgt" eheight)
    )
    (if (not estyle)
        (setq estyle "")
    );if 
    (setq hair_style_list (tnlist '("style" 16)));setq
    (if (not (member estyle hair_style_list))
        (setq hair_style_list (append hair_style_list (list estyle)));setq
    );if 

    (setq hair_style_list (acad_strlsort hair_style_list));setq
    (mpoplst "text_style" hair_style_list)
    (set_tile "text_style" 
              (itoa (position estyle hair_style_list))
    );set_tile 

    ;; Disable tiles if need be...  ;@RK
    (setq a 0)
    (while ( < a  (sslength ss))
      (setq which_tiles
            (ai_common_state (cdr (assoc '0 (entget (ssname ss a))))))
    
      ;; If all fields are enabled, don't bother checking anymore.
      (if (/= which_tiles (logior 1 2 4 8 16))
        (setq a (1+ a))
        (setq a (sslength ss))
      )
    )
    ;; Layer Button and Text Field

    (if (/= 1 (logand 1 which_tiles))
      (progn
        (mode_tile "t_layer" 1)
        (mode_tile "b_name" 1)
      )
    )

    ;; Color Button and Text Field
    (if (/= 2 (logand 2 which_tiles))
      (progn
        (mode_tile "t_color" 1)
        (mode_tile "b_color" 1)
        (mode_tile "show_image" 1)
      )
    )
    ;; Linetype Button and Text Field
    (if (/= 4 (logand 4 which_tiles))
      (progn
        (mode_tile "t_ltype" 1)
        (mode_tile "b_line" 1)
      )
    )
    ;; Linetype Scale Edit Field
    (if (/= 8 (logand 8 which_tiles))
      (progn
        (mode_tile "eb_ltscale" 1)
      )
    )
    ;; Thickness Edit Field.
    (if (/= 16 (logand 16 which_tiles))
      (progn
        (mode_tile "eb_thickness" 1)
      )
    )

    ;; Polyline box and tiles          @rk 10:05 AM 1/30/97
    (if (not (= 2 (logand 2 eflag)))
        (progn
         (mode_tile "text_hgt" 1)
         (mode_tile "text_style" 1)
        );progn
    );if
    (if (not (= 1 (logand 1 eflag)))
        (progn 
         (mode_tile "poly_wid" 1)
         (mode_tile "poly_elev" 1)
        );progn then disable polyline fields
    );if

    ;; Define action for tiles
    (action_tile "b_color" "(setq ecolor (getcolor))")
    (action_tile "show_image" "(setq ecolor (getcolor))")
    (action_tile "b_name" "(setq elayer (getlayer))")
    (action_tile "b_line" "(setq eltype (getltype))")
    (action_tile "eb_ltscale"  "(getscale $value)")
    (action_tile "eb_thickness"  "(getthickness $value)")
    (action_tile "poly_wid" "(getwidth $value)")
    (action_tile "poly_elev" "(getelevation $value)")
    (action_tile "text_hgt" "(getheight $value)")
    (action_tile "text_style" "(getstyle $value hair_style_list)") 
    (action_tile "help" "(help \"AC_BONUS.HLP\" \"EXCHPROP\")")
    (action_tile "accept" "(test-main-ok)")

    (if (= (start_dialog) 1)
      (progn
        ; Update special properties for polyline and text selection-sets.
        
        (if (and (= 1 (logand 1 eflag))   ; polylines rk chprop
                 (or ewidth
                     eelevation
                 );or
            );and
            (progn
             (setq  ss-index 0 
                   ss-length (sslength poly_ss)
             );setq
             (while (< ss-index ss-length)
               (setq ename (ssname poly_ss ss-index)
                     elist (entget ename)
               );setq
               (ucs_2_ent (cdr (assoc 210 elist)));this function lives in ac_bonus.lsp
               (if ewidth 
                   (command "_.pedit" ename "_W" ewidth "_x")
               );if 
               (if eelevation 
                   (progn
                    (setq p2 (list 0.0 0.0 eelevation));setq
                    (if (equal (cdr (assoc 0 elist)) "POLYLINE")
                        (setq p1 (list 0.0 0.0
                                       (caddr (cdr (assoc 10 elist)))
                                 );list
                        );setq
                        (progn
                         (if (assoc 38 elist)
                             (setq p1 (list 0.0 0.0
                                            (cdr (assoc 38 elist))
                                      );list
                             );setq
                             (setq p1 '(0.0 0.0 0.0))
                         );if
                        );progn 
                    );if
                    (command "_.move" ename "" p1 p2)
                   );progn then change the elevation of the polyline
               );if
               (command "_.ucs" "_p")
               (setq ss-index (1+ ss-index))
             );while
           );progn then polylines are in the selset
         );if
         (if (and (= 2 (logand 2 eflag))     ; text
                  (or eheight 
                      estyle
                  );or
             );and
             (progn
              (setq ss-index 0 ss-length (sslength txt_ss))
              (while
                (< ss-index ss-length)
                (setq elist (entget (setq ename (ssname txt_ss ss-index))))
                (if (numberp eheight)
                    (setq elist (subst (cons 40 eheight)
                                       (assoc 40 elist)
                                       elist
                                );subst
                    );setq
                );if
                (if (and estyle ;(not (equal estyle ""))
                         (not (equal estyle (cdr (assoc 7 elist))))
                    );and
                    (progn  
                     ;rk. This needs to be implemented as per mtext font issue in                                                                                                          ;    ddchprop_notes.txt 
                     ;(if (equal "MTEXT" (cdr (assoc 0 elist)))
                     ;    (setq elist (fix_mtext_fonts estyle 
                     ;                                 elist
                     ;                )
                     ;    );setq then
                         (setq elist (subst (cons 7 estyle)
                                            (assoc 7 elist)
                                            elist
                                      );subst
                         );setq else
                     ;);if 
                    );progn
                );if
                (entmod elist)
                (setq ss-index (1+ ss-index))
              );while
             );progn then
         );if

        (setq cmdact (getvar "cmdactive"))
        (command "_.chprop" ss "")
        (if (/= cmdact (getvar "cmdactive"))  ; Is CHPROP actually running?
          (progn
            (if ecolor
                (progn
                 (if (= 0 ecolor)   (setq ecolor ;|MSG0|;"BYBLOCK"))
                 (if (= 256 ecolor) (setq ecolor ;|MSG0|;"BYLAYER"))
                 (command "_c" ecolor)
                );progn then
            )
            (if (and lt-idx
                     (/= eltype ;|MSG0|;"Varies")
                )
                (command "_lt" eltype)
            )
            (if (and lay-idx 
                     (/= elayer ;|MSG0|;"Varies")
                )
                (command "_la" elayer)
            )
            (if (and ethickness 
                     (/= ethickness "")
                     (/= ethickness "Varies") 
                )
                (command "_t" ethickness)
            )
            (if (and  eltscale (/= eltscale ""))
                (command "_lts" eltscale)
            );if
            (command "")
          )
          (princ "\nProperties unchanged")  ; CHPROP didn't like our SS set
        )

      );progn then OK was picked in the dialog.
      
      ;; Fred GERBER - 25-AUG-94
      ;; Don't print the "Properties unchanged" message when the user cancels
      ;; the dialog because he knows that already (otherwise he would have
      ;; hit the "OK" button). Display the message only if CHPROP fails for
      ;; some reason, because it is not the expected behavior of the command.
      ;;
      ;; (princ ;|DDCHPROP2_LSP_8|;"\nProperties unchanged")
    );if
    (princ)
  );defun call_chp2
  ;;
  ;; Function to set the Color text tile and swab to the current color value.
  ;;
  (defun set_col_tile()
    (cond
      ((= ecolor nil)
        (set_tile "t_color" "Varies")
        (col_tile "show_image" 0 nil)
      )
      ((= ecolor 0)
        (set_tile "t_color" "BYBLOCK")
        (col_tile "show_image" 7 nil)    ; show BYBLOCK as white
      )
      ((= ecolor 1)
        (set_tile "t_color" "1 red")
        (col_tile "show_image" 1 nil)
      )
      ((= ecolor 2)
        (set_tile "t_color" "2 yellow")
        (col_tile "show_image" 2 nil)
      )
      ((= ecolor 3)
        (set_tile "t_color" "3 green")
        (col_tile "show_image" 3 nil)
      )
      ((= ecolor 4)
        (set_tile "t_color" "4 cyan")
        (col_tile "show_image" 4 nil)
      )
      ((= ecolor 5)
        (set_tile "t_color" "5 blue")
        (col_tile "show_image" 5 nil)
      )
      ((= ecolor 6)
        (set_tile "t_color" "6 magenta")
        (col_tile "show_image" 6 nil)
      )
      ((= ecolor 7)
        (set_tile "t_color" "7 white")
        (col_tile "show_image" 7 nil)
      )
      ;; If the color is "BYLAYER", then set the tile to
      ;; show it's set By layer, but also indicate the
      ;; color of the layer - i.e. By layer (red)
      ((= ecolor 256)
        (set_tile "t_color" (bylayer_col))
        (col_tile "show_image" cn nil)
      )
      (T
        (set_tile "t_color" (itoa ecolor))
        (col_tile "show_image" ecolor nil)
      )
    )
  )
  ;;
  ;;  Function to put up the standard color dialogue.
  ;;
  (defun getcolor(/ col_def lay_clr temp_color)
    ;; col_def is the default color used when rq_color is called.  If ecolor
    ;; is nil (varies) then set it to 1, else use the value of ecolor.
    (if ecolor
      (setq col_def ecolor)
      (setq col_def 1)
    )

    ;; If we're working with a single layer, get its color
    ;; for use in the color swatch if the user selects color BYLAYER.
    (if (/= elayer ;|MSG0|;"Varies")
      (setq lay_clr (cdr (assoc 62 (tblsearch "layer" elayer))))
      (setq lay_clr 0)
    )
    (if (numberp (setq temp_color (acad_colordlg col_def T lay_clr)))
      (progn
        (setq ecolor temp_color)
        (set_col_tile)
        ecolor
      )
      ecolor
    )
  )
  ;;
  ;; This function pops a dialogue box consisting of a list box, image tile,
  ;; and edit box to allow the user to select or type a linetype.  It returns
  ;; the linetype selected.
  ;;
  (defun getltype (/ old-idx ltname)
    ;; Initialize a dialogue from dialogue file
    (if (not (new_dialog "setltype" dcl_id)) (exit))
    (start_list "list_lt")
    (mapcar 'add_list ltnmlst)         ; initialize list box
    (end_list)
    (setq old-idx lt-idx)
    ;; Show initial ltype in image tile, list box, and edit box
    (if (/= lt-idx nil)
        (ltlist_act (itoa lt-idx))
        (progn
         (set_tile "edit_lt" "")
         (col_tile "show_image" 0 nil)
        );progn else
    );if
    (action_tile "list_lt" "(ltlist_act $value)")
    (action_tile "edit_lt" "(ltedit_act)")
    (action_tile "accept" "(test-ok)")
    (action_tile "cancel" "(reset-lt)")
    (if (= (start_dialog) 1)           ; User pressed OK
        (cond
         ((or (= lt-idx nil)
              (= lt-idx (1- (length ltnmlst)))
          );or
          (set_tile "t_ltype" "Varies")
          ;|MSG0|;"Varies"
         )
         ((= lt-idx 0)
          (set_tile "t_ltype" (bylayer_lt))
           ;|MSG0|;"BYLAYER"
         )
         ((= lt-idx 1)
           (set_tile "t_ltype" "BYBLOCK")
           ;|MSG0|;"BYBLOCK"
         )
         (T
           (set_tile "t_ltype" ltname)
           ltname
         )
        );cond then
        eltype
    );if
  );defun
  ;;
  ;; Edit box entries end up here
  ;;
  (defun ltedit_act ( / flag)
    ;; If linetype name,is valid, then clear error string,
    ;; call ltlist_act function, and change focus to list box.
    ;; Else print error message.
    
    (setq ltvalue (xstrcase (get_tile "edit_lt")))
    (if (or (= ltvalue ;|MSG0|;"BYLAYER")
            (= ltvalue "BY LAYER")
        )
        (setq ltvalue "BYLAYER")
    )
    (if (or (= ltvalue ;|MSG0|;"BYBLOCK")
            (= ltvalue "BY BLOCK")
        )
        (setq ltvalue "BYBLOCK")
    )
    (if (setq lt-idx (getindex ltvalue ltnmlst))
        (progn
         (set_tile "error" "")
         (ltlist_act (itoa lt-idx))
         ;(mode_tile "list_lt" 2)
        );progn then
        (progn
         (if (/= ltvalue "")
             (progn
               (set_tile "error" "Invalid linetype.")
               (setq flag T)
             );progn
         )
         (setq lt-idx old-idx)
        );progn else
    );if
    (if (and (not flag) ;added so a return will take you out of the dialog.
             (= $reason 1)
        );and
        (done_dialog 1) 
    );if 
  );defun ltedit_act
  ;;
  ;; List selections end up here
  ;;
  (defun ltlist_act (index / dashdata)
    ;; Update the list box, edit box, and color tile
    (set_tile "error" "")
    (setq lt-idx (atoi index))
    (setq ltname (nth lt-idx ltnmlst))
    (setq dashdata (nth lt-idx mdashlist))
    (col_tile "show_image" 0 dashdata)
    (set_tile "list_lt" (itoa lt-idx))
    (set_tile "edit_lt" ltname)
  )
  ;;
  ;; Reset to original linetype when cancel it selected
  ;;
  (defun reset-lt ()
    (setq lt-idx old-idx)
    (done_dialog 0)
  )
  ;;
  ;; This function pops a dialogue box consisting of a list box and edit box to
  ;; allow the user to select or type a layer name.  It returns the layer name
  ;; selected.  It also the status (On, Off, Frozen, etc.) of all layer in the
  ;; drawing.
  ;;
  (defun getlayer (/ old-idx layname on off frozth linetype colname)
    ;; Create layer list the first time the layer
    ;; dialogue is called.
    (if (not lay-idx)
        (progn
         (makelaylists)                     ; layer list - laynmlst
         ;rk
         (setq lay-idx (getindex elayer laynmlst))
        );progn
    );if

    ;; Load a dialogue from dialogue file
    (if (not (new_dialog "setlayer" dcl_id)) (exit))
    (start_list "list_lay")
    (mapcar 'add_list longlist)        ; initialize list box
    (end_list)
    ;; Display current layer, show initial layer name in edit
    ;; box, and highlight list box.
    (setq old-idx lay-idx)
    (if (/= lay-idx nil) (laylist_act (itoa lay-idx)))
    (set_tile "cur_layer" (getvar "clayer"))
    (action_tile "list_lay" "(laylist_act $value)")
    (action_tile "edit_lay" "(layedit_act)")
    (action_tile "accept" "(test-ok)")
    (action_tile "cancel" "(reset-lay)")
    (if (= (start_dialog) 1)           ; User pressed OK
       (progn
         (if (or (= lay-idx nil)
                 (= lay-idx (1- (length laynmlst)))
             );or
             (progn
              (setq lay-idx nil)  
              (setq layname ;|MSG0|;"VARIES")
              (set_tile "t_layer" "Varies")
              (setq layname "")
             );progn
             (set_tile "t_layer" layname)
         );if
         ; If layer or ltype equals bylayer reset their tiles
         (if (= lt-idx 0)
             (set_tile "t_ltype" (bylayer_lt))
         );if
         (if (= ecolor 256)
             (progn
              (set_tile "t_color" (bylayer_col))
              (col_tile "show_image" cn nil)
             )
         );if
         layname
       );progn
       elayer
    );if
  )
  ;;
  ;; Edit box selections end up here
  ;;
  (defun layedit_act()
    ;; Convert layer entry to upper case.  If layer name is
    ;; valid, clear error string, call (laylist_act) function,
    ;; and change focus to list box.  Else print error message.
    (setq layvalue (xstrcase (get_tile "edit_lay")))
    (if (setq lay-idx (getindex layvalue laynmlst))
        (progn
         (set_tile "error" "")
         (laylist_act (itoa lay-idx))
        )
        (progn
         (set_tile "error" "Invalid layer name.")
         (mode_tile "edit_lay" 2)
         (setq lay-idx old-idx)
        )
    );if
  );defun
  ;;
  ;; List entry selections end up here
  ;;
  (defun laylist_act (index / layinfo color dashdata)
    ;; Update the list box, edit box, and color tile
    (set_tile "error" "")
    (setq lay-idx (atoi index))
    (if (not (equal lay-idx (1- (length laynmlst))))
        (progn 
         (setq layname (nth lay-idx laynmlst))
         (setq layinfo (tblsearch "layer" layname))
         (setq color (cdr (assoc 62 layinfo)))
         (setq color (abs color))
         (setq colname (colorname color))
         (set_tile "list_lay" (itoa lay-idx))
         (set_tile "edit_lay" layname)
         ;(mode_tile "list_lay" 2)
        );progn then 
        (set_tile "edit_lay" "") 
    );if 
  );defun laylist_act
  ;;
  ;; Reset to original layer when cancel is selected
  ;;
  (defun reset-lay ()
    (setq lay-idx old-idx)
    (done_dialog 0)
  )

  ;; Checks validity of linetype scale from edit box.  It checks to
  ;; see if the value equals "Varies".

  (defun getscale (value / rval)
    (setq value (strcase value)
           rval (distof value)
    );setq
    (if (or (= value "")
            (> rval 0.0)
        )
        (progn
          (set_tile "error" "")
          (if (= value "")
              (progn
               (set_tile "eb_ltscale" "")
               (setq eltscale nil)
              );progn then
              (progn
               (setq eltscale (distof value))
               (set_tile "eb_ltscale" (ai_rtos eltscale))
               eltscale
              );progn else
          );if
        );progn
        (progn
         (set_tile "error" "Invalid ltscale.")
         nil
        );progn else
    );if
  );defun
  ;;
  ;; Checks validity of thickness from edit box. Since (atof) returns 0 when a
  ;; string can't be converted to a real, this routine checks if the first
  ;; character is "0".  It also checks to see if the value equals "Varies".
  ;;
  (defun getthickness (value)
    (setq value (strcase value))
    (if (or (= value "")
            (distof value)
        );or
        (progn
         (set_tile "error" "")
         (if (= value "")
             (progn
              (set_tile "eb_thickness" "")
              (setq ethickness nil)
             );progn
             (progn
              (setq ethickness (distof value))
              (set_tile "eb_thickness" (ai_rtos ethickness))
              ethickness
             );progn
         );if
        );progn
        (progn
          (set_tile "error" "Invalid thickness.")
          nil
        );progn
    );if
  );defun 
  ;;
  ;; Checks validity of polyline elevation from edit box.
  ;;
  (defun getelevation (value)
    (setq value (strcase value))
    (if (or (= value "")
            (distof value)
        )
        (progn
         (set_tile "error" "")
         (if (= value "")
             (progn
              (set_tile "poly_elev" "")
              (setq eelevation nil)
             );progn
             (progn
              (setq eelevation (distof value))
              (set_tile "poly_elev" (ai_rtos eelevation))
              eelevation
             );progn
         );if
        );progn
        (progn
         (set_tile "error" "Invalid elevation.")
         nil
        );progn
    );if
  );defun
  ;;
  ;; Checks validity of polyline width from edit box.
  ;;
  (defun getwidth (value / rval)
    (setq value (strcase value)
           rval (distof value)
    )
    (if (or (= value "")
            (>= rval 0.0)
        )
        (progn
         (set_tile "error" "")
         (if (= value "")
             (progn
              (set_tile "poly_wid" "")
              (setq ewidth nil)
             );progn
             (progn
              (setq ewidth (distof value))
              (set_tile "poly_wid" (ai_rtos ewidth))
              ;width
             );progn
         );if
        );progn
        (progn
         (set_tile "error" "Invalid width.")
         ;(setq ewidth nil)
         nil 
        );progn
    );if
  );defun
  ;;
  ;; Checks validity of text height from edit box.
  ;;
  (defun getheight (value / rval)
    (setq value (strcase value)
           rval (distof value)
    )
    (if (or (= value "")
            (> rval 0.0)
        )
        (progn
         (set_tile "error" "")
         (if (= value "")
             (progn
              (set_tile "text_hgt" "")
              (setq eheight nil)
             );progn
             (progn
              (setq eheight (distof value))
              (set_tile "text_hgt" (ai_rtos eheight))
              eheight
             );progn
         );if
        );progn
        (progn
         (set_tile "error" "Invalid height.")
         nil
        );progn
    );if
  );defun

  (defun getstyle (value lst / rval)
    ;(setq value (strcase value))
    (set_tile "error" "")
    (setq estyle (nth (atoi value) lst))
    (if (equal estyle "")
        (setq estyle nil)
    );if
 
  );defun getstyle

  ;;
  ;; This function make a list called laynmlst which consists of all the layer
  ;; names in the drawing.  It also creates a list called longlist which
  ;; consists of strings which contain the layer name, color, linetype, etc.
  ;; Longlist is later mapped into the layer listbox.  Both are ordered the
  ;; same.
  ;;
  (defun makelaylists (/ layname onoff frozth color linetype lock vpf vpn ss cvpname
                         xdlist vpldata sortlist name templist bit-70
                         layer_number
                      )
    (if (= (setq tilemode (getvar "tilemode")) 0)
      (progn
        (setq ss (ssget "_x" (list (cons 0 "VIEWPORT")
                                  (cons 69 (getvar "CVPORT"))
                            )
                 )
        )
        (setq cvpname (ssname ss 0))
        (setq xdlist (assoc -3 (entget cvpname '("acad"))))
        (setq vpldata (cdadr xdlist))
      )
    )
    (setq sortlist nil)
    (setq templist (tblnext "LAYER" T))
    (setq layer_number 1)
    (while templist
      (if (not (equal 16 (logand 16 (cdr (assoc 70 templist)))))
          (progn  
           (setq name (cdr (assoc 2 templist)))
           (setq sortlist (cons name sortlist))
           ;; Not dead message...
           (setq layer_number (1+ layer_number))
          );progn
      );if
      (setq templist (tblnext "LAYER"))
      (if (= (/ layer_number 50.0) (fix (/ layer_number 50.0)))
          (set_tile "error" (strcat "Collecting..." (itoa layer_number)))
      );if
    )
    (set_tile "error" "")
    (if (>= (getvar "maxsort") (length sortlist))
      (progn
        (if (> layer_number 50)
          (set_tile "error" "Sorting...")
        )
        (setq sortlist (acad_strlsort sortlist))
      )
      (setq sortlist (reverse sortlist))
    )
    (set_tile "error" "")
    (setq laynmlst sortlist)
    ;rk 
    (setq laynmlst (append laynmlst (list "")))     

    (setq longlist nil)
    (setq layname (car sortlist))
    (setq layer_number 1)
    (while layname
      (if (= (/ layer_number 50.0) 
             (fix (/ layer_number 50.0))
          )
          (set_tile "error" (strcat "Analyzing..." (itoa layer_number)))
      );if
      (setq layer_number (1+ layer_number))
      (setq laylist (tblsearch "LAYER" layname))
      (setq color (cdr (assoc 62 laylist)))
      (if (minusp color)
        (setq onoff ".")
        (setq onoff "On")
      )
      (setq color (abs color))
      (setq colname (colorname color))
      (setq bit-70 (cdr (assoc 70 laylist)))
      (if (= (logand bit-70 1) 1)
        (setq frozth "F" fchk laylist)
        (setq frozth ".")
      )
      (if (= (logand bit-70 2) 2)
        (setq vpn "N")
        (setq vpn ".")
      )
      (if (= (logand bit-70 4) 4)
        (setq lock "L")
        (setq lock ".")
      )
      (setq linetype (cdr (assoc 6 laylist)))
      (setq layname (substr layname 1 31))
      (if (= tilemode 0)
        (progn
          (if (member (cons 1003 layname) vpldata)
            (setq vpf "C")
            (setq vpf ".")
          )
        )
        (setq vpf ".")
      )
      (setq ltabstr (strcat layname "\t"
                              onoff "\t"
                             frozth "\t"
                               lock "\t"
                                vpf "\t"
                                vpn "\t"
                            colname "\t"
                           linetype
                    )
      )
      (setq longlist (append longlist (list ltabstr)))
      (setq sortlist (cdr sortlist))
      (setq layname (car sortlist))
    );while
    (setq longlist (append longlist (list "")))
    (set_tile "error" "")
  )
  ;;
  ;; This function makes 2 lists - ltnmlst & mdashlist.  Ltnmlst is a list of
  ;; linetype names read from the symbol table.  Mdashlist is list consisting
  ;; of lists which define the linetype pattern - numbers that indicate dots,
  ;; dashes, and spaces taken from group code 49.  The list corresponds to the
  ;; order of names in ltnmlst.
  ;;
  (defun makeltlists (/ ltlist ltname)
    (setq mdashlist nil)
    (setq ltlist (tblnext "LTYPE" T))
    (setq ltname (cdr (assoc 2 ltlist)))
    (setq ltnmlst (list ltname))
    (while (setq ltlist (tblnext "LTYPE"))
     (if (not (equal 16 (logand 16 (cdr (assoc 70 ltlist)))))
         (progn 
          (setq ltname (cdr (assoc 2 ltlist)))
          (setq ltnmlst (append ltnmlst (list ltname)))
         );progn
     );if 
    );while
    (setq ltnmlst (acad_strlsort ltnmlst))
    (setq ltnmlst (append ltnmlst (list "")));add by rk 
    (foreach ltname ltnmlst
      (setq ltlist (tblsearch "LTYPE" ltname))
      (if (= ltname "CONTINUOUS")
        (setq mdashlist (append mdashlist (list "CONT")))
        (setq mdashlist
            (append mdashlist (list (add-mdash ltlist)))
        )
      )
    )
    (setq ltnmlst (cons "BYBLOCK" ltnmlst))
    (setq mdashlist  (cons nil mdashlist))
    (setq ltnmlst (cons "BYLAYER" ltnmlst))
    (setq mdashlist  (cons nil mdashlist))
  )
  ;;
  ;; Get all the group code 49 values for a linetype and put them in a list
  ;; (pen-up, pen-down info)
  ;;
  (defun add-mdash (ltlist1 / dashlist assoclist dashsize)
    (setq dashlist nil)
    (while (setq assoclist (car ltlist1))
      (if (= (car assoclist) 49)
        (progn
          (setq dashsize (cdr assoclist))
          (setq dashlist (cons dashsize dashlist))
        )
      )
      (setq ltlist1 (cdr ltlist1))
    )
    (setq dashlist (reverse dashlist))
  )
  ;;
  ;; Color a tile, draw linetype, and draw a border around it
  ;;
  (defun col_tile (tile color patlist / x y)
    (setq x (dimx_tile tile))
    (setq y (dimy_tile tile))
    (start_image tile)
    (fill_image 0 0 x y color)
    (if (= color 7)
      (progn
        (if patlist (drawpattern x (/ y 2) patlist 0))
        (tile_rect 0 0 x y 0)
      )
      (progn
        (if patlist (drawpattern x (/ y 2) patlist 7))
        (tile_rect 0 0 x y 7)
      )
    )
    (end_image)
  )
  ;;
  ;; Draw a border around a tile
  ;;
  (defun tile_rect (x1 y1 x2 y2 color)
    (setq x2 (- x2 1))
    (setq y2 (- y2 1))
    (vector_image x1 y1 x2 y1 color)
    (vector_image x2 y1 x2 y2 color)
    (vector_image x2 y2 x1 y2 color)
    (vector_image x1 y2 x1 y1 color)
  )
  ;;
  ;; Draw the linetype pattern in a tile.  Boxlength is the length of the image
  ;; tile, y2 is the midpoint of the height of the image tile, pattern is a
  ;; list of numbers that define the linetype, and color is the color of the
  ;; tile.
  ;;
  (defun drawpattern (boxlength y2 pattern color / x1 x2
                      patlist dash)
    (setq x1 0 x2 0)
    (setq patlist pattern)
    (setq fx 30)
    (if (= patlist "CONT")
      (progn
        (setq dash boxlength)
        (vi)
        (setq x1 boxlength)
      )
      (foreach dash patlist
        (if (> (abs dash) 2.5)
          (setq fx 2)
        )
      )
    )
    (while (< x1 boxlength)
      (if (setq dash (car patlist))
        (progn
          (setq dash (fix (* fx dash)))
          (cond
            ((= dash 0)
              (setq dash 1)
              (vi)
            )
            ((> dash 0)
              (vi)
            )
            (T
              (if (< (abs dash) 2) (setq dash 2))
              (setq x2 (+ x2 (abs dash)))
            )
          )
          (setq patlist (cdr patlist))
          (setq x1 x2)
        )
        (setq patlist pattern)
      )
    )
  )
  ;;
  ;; Draw a dash or dot in image tile
  ;;
  (defun vi ()
    (setq x2 (+ x2 dash))
    (vector_image x1 y2 x2 y2 color)
  )

  ;; This function takes a selection and returns a list of the color,
  ;; linetype, layer, linetype scale, and thickness properties that
  ;; are common to every entities in the selection set - (color
  ;; linetype layer thickness).  If all entities do not share the same
  ;; property value it returns "Varies" in place of the property
  ;; value.  i.e.  ("BYLAYER" "DASHED" "Varies" 0)
  ;; The last item in the return list is an integer flag for the
  ;; homegenity of the selection-set object types.
  ;;   1 = All polylines
  ;;   2 = All text or mtext or attdef, or a combination of the three
  ;;  -1 = Any other mix of objects

  (defun getprops (selset / sslen elist color ltype layer ltscale thickness
                          width elevation height go ctr 
                          eflag 
                          etype temp 
                          txt_ss     ;;;;rk 11:24 AM 1/30/97
                          tmp 
                          poly_ss   
                          style
                  )


    (setq     sslen (sslength selset)
              elist (entget (ssname selset 0))
              etype (strcase (cdr (assoc 0 elist)))
              color (cdr (assoc 62 elist))
              ltype (cdr (assoc 6 elist))
              layer (cdr (assoc 8 elist))
          thickness (cdr (assoc 39 elist))
            ltscale (cdr (assoc 48 elist))
    );setq

    (if (not color)     (setq color 256))
    (if (not ltype)     (setq ltype "BYLAYER"))
    (if (not thickness) (setq thickness 0))
    (if (not ltscale)   (setq ltscale 1))

    (if (not width)     (setq width ""))
    (if (not elevation) (setq elevation ""))
    (if (not height)    (setq height ""))

    (setq      go T 
          chk-col T 
           chk-lt T 
          chk-lay T 
          chk-lts T 
           chk-th T 
              ctr 0
    );setq

    ;; Page through the selection set.  When a property
    ;; does not match, stop checking for that property.
    ;; When the selection set is not homogenous, stop checking.
    ;; If all properties vary and the set is not a type 1
    ;; (polyline) or type 2 (attdef/text/mtext) set, stop paging.

    ;Lets set the eflag so we know if the selection set includes any 
    ;combination of polylines, lwpolylines, text, mtext or attdefs.

    ;watch out for 3dpolyline and 3dmeshes here! @rk 10:17 AM 1/30/97
    (setq eflag 0)
    (if (setq poly_ss 
              (ssget "P" (list '(0 . "*POLYLINE")
                               '(-4 . "<AND") 
                                  '(-4 . "<NOT") '(-4 . "&=") '(70 . 8)  '(-4 . "NOT>")
                                  '(-4 . "<NOT") '(-4 . "&=") '(70 . 16) '(-4 . "NOT>")
                                  '(-4 . "<NOT") '(-4 . "&=") '(70 . 64) '(-4 . "NOT>")
                               '(-4 . "AND>") 
                         );list
              );ssget get 2d polylines (legacy and lw)
        );setq
        (progn
         (setq eflag (+ eflag 1))
         
         (setq tmp (entget (ssname poly_ss 0)));setq

         (if (equal "POLYLINE" (cdr (assoc 0 tmp)))
             (setq elevation (last (cdr (assoc 10 tmp))))
             (setq elevation (cdr (assoc 38 tmp)))
         );if
         (if (not elevation)
             (setq elevation 0.0);setq
         );if

         (setq tmp (ssget "P" 
                          (list 
                             '(-4 . "<OR")
                               '(-4 . "<AND") 
                                 '(0 . "LWPOLYLINE")
                                  (cons 38 elevation)
                               '(-4 . "AND>")
                               '(-4 . "<AND")
                                 '(0 . "POLYLINE")
                                 '(-4 . "*,*,=") 
                                  (cons 10 (list 1.0 1.0 elevation))
                               '(-4 . "AND>") 
                              '(-4 . "OR>")
                         );list
                   );ssget
         );setq

         (if (and tmp 
                  (equal (sslength tmp) (sslength poly_ss))
             );and
             (setq elevation (ai_rtos elevation));setq
             (setq elevation "")
         );if
          
         (setq width (pl_width_getter poly_ss));setq
         
        );progn then 2d polylines and/or lwpolylines are in the HHHHHOUSE!
    );if
    (command "_.select" selset "")
     
    (if (setq txt_ss 
              (ssget "P" '((-4 . "<OR") (0 . "TEXT") (0 . "MTEXT") 
                           (0 . "ATTDEF") (-4 . "OR>"))
              )  
        );setq
        (progn
         (setq eflag (+ eflag 2))
         (setq height (cdr (assoc 40 (entget (ssname txt_ss 0))))
                  tmp (ssget "P" (list (cons 40 height))) 
         );setq 
         (if (and tmp
                  (equal (sslength txt_ss) (sslength tmp))                    
             );and
             (setq height (ai_rtos height));setq  ;@rk need to translate from float 
                                                   ;to string and back again easily
             (setq height "");setq else the height varies 
         );if
         (command "_.select" txt_ss "") 
         (setq style (cdr (assoc 7 (entget (ssname txt_ss 0))))
                 tmp (ssget "P" (list (cons 7 style))) 
         );setq 
         (if (not (and tmp
                       (equal (sslength txt_ss) (sslength tmp))                    
                  );and
             );not
             (setq style "");setq then the style varies 
         );if
        );progn the text type objects are in the selection set
    );if
    (command "_.select" selset "")
    
    (while (and (> sslen ctr) 
                go
           );and
      (setq elist (entget (setq en (ssname selset ctr))))

      (if chk-col (match-col))
      (if chk-lt (match-lt))
      (if chk-lay (match-lay))
      (if chk-lts (match-lts))
      (if chk-th (match-th))

      ;(if chk-etype (match-etype))

      (setq ctr (1+ ctr))
      (if (and (not chk-col)
               (not chk-lt)
               (not chk-lay)
               (not chk-lts)
               (not chk-th)
               ;(not chk-etype)
          );and
          (setq go nil)
      );if
    );while
    
    (list color ltype layer thickness ltscale
          width elevation height eflag 
          style poly_ss txt_ss
    )
  );defun getprops

  ;  This is a speedy little routine to tell whether the polylines in 
  ;the selection set argument are of varying width or a constant value. 
  ;Looping through the vertex's has to be done for old polylines when 
  ;the polyline header has width values of 0.0. Basically, in this case, 
  ;information in the polyline entity header is abmiguous. Width values 
  ;of 0.0 in the header entity could mean the polyline has a constant 
  ;width of 0.0 or it could mean that the polyline has vertex's of varying 
  ;width.
  ;
  ;  It's all in wrist. Err a.., I mean it's all in the 'if'
  ;;
  (defun pl_width_getter ( ss / ss2 ss3 n na e1 width width_a width_b flag flag2)

   (if ss 
       (command "_.select" ss "")
   );if
   (setq width ""
          flag nil
         flag2 nil
   );setq
   (if (not 
         (and ss
              (setq ss2 (ssget "p" '((0 . "LWPOLYLINE"))));setq
              (setq    na (ssname ss2 0)
                    width (cdr (assoc 43 (entget na)))
              );setq
              (setq ss3 (ssget "p" (list '(0 . "LWPOLYLINE")
                                          (cons 43 width)
                                   );list
                        );ssget
              );setq
              (setq flag T) 
              (equal (sslength ss2) (sslength ss3))
         );and
       );not
       (progn
        (if flag
            (setq width nil) 
        );if
       );progn
   );if 
       
   (if (not 
        (and 
             ss
             (progn (command "_.select" ss "") 
                    (setq ss2 (ssget "p" '((0 . "POLYLINE"))));setq
             ) 
             (setq      na (ssname ss2 0)
                        e1 (entget na)
                   width_a (cdr (assoc 40 e1))
                   width_b (cdr (assoc 41 e1))
             );setq
             (equal width_a width_b)
             (setq ss3 (ssget "p" (list '(0 . "POLYLINE")
                                         (cons 40 width_a)
                                         (cons 41 width_b)
                                  );list
                       );ssget
             );setq
             (setq flag2 T)
             (equal (sslength ss2) (sslength ss3))
        );and 
       );not 
       (progn
        (if flag2
            (setq width nil);setq
        );if
       );progn then
       (progn
        (if (or (equal width "")
                (not flag)
            );or
            (setq width width_a)
            (progn
             (if (not (equal width width_a))
                 (setq width "")
             );if  
            );progn
        );if
       );progn
   );if
      

   ;now for the special handling for old polylines
   (if (and width 
            (equal width 0.0)
            flag2
       );and 
       (progn
        
        (setq n 0);setq
        (while (and (equal width 0.0)
                    (< n (sslength ss3))
               );and
         (setq flag nil
                 na (ssname ss3 n)
                 na (entnext na)
                 e1 (entget na)
         );setq
         (while (not flag)
          (if (or (equal (cdr (assoc 0 e1)) "SEQEND")
                  (not (equal (cdr (assoc 40 e1)) 0.0))
                  (not (equal (cdr (assoc 41 e1)) 0.0))
              );or
              (progn
               (setq flag T);
               (if (not (equal (cdr (assoc 0 e1)) "SEQEND"))
                   (setq width nil) 
               );if
              );progn then jump out of the loop
              (setq na (entnext na)
                    e1 (entget na)
              );setq 
          );if 
         );while
        (setq n (+ n 1));setq  
        );while 
           
       );progn then it's a legacy 
   );if    ;legacy polylines that may have varying widths  

   (if (not width)
       (setq width "");setq
   );if
   (if (not (equal 'STR (type width)))
       (setq width (ai_rtos width));setq 
   );if 
    
   width   
  );defun pl_width_getter

    
  (defun match-col (/ ncolor)
    (setq ncolor (cdr (assoc 62 elist)))
    (if (not ncolor) (setq ncolor 256))
    (if (/= color ncolor)
      (progn
        (setq chk-col nil)
        (setq color nil)
      )
    )
  )

  (defun match-lt (/ nltype)
    (setq nltype (cdr (assoc 6 elist)))
    (if (not nltype) (setq nltype "BYLAYER"))
    (if (/= ltype nltype)
      (progn
        (setq chk-lt nil)
        (setq ltype ;|MSG0|;"Varies")
      )
    )
  )

  (defun match-lay (/ nlayer)
    (setq nlayer (cdr (assoc 8 elist)))
    (if (/= layer nlayer)
      (progn
        (setq chk-lay nil)
        (setq layer ;|MSG0|;"Varies")
      )
    )
  )

  (defun match-th (/ nthickness)
    (setq nthickness (cdr (assoc 39 elist)))
    (if (not nthickness) (setq nthickness 0))
    (if (/= thickness nthickness)
      (progn
        (setq chk-th nil)
        (setq thickness ;|MSG0|;"Varies")
      )
    )
  )

  (defun match-lts (/ nltscale)
    (setq nltscale (cdr (assoc 48 elist)))
    (if (not nltscale) (setq nltscale 1))
    (if (/= ltscale nltscale)
      (progn
        (setq chk-lts nil)
        (setq ltscale ;|MSG0|;"Varies")
      )
    )
  )

  ;;
  ;; If an item is a member of the list, then return its index number, else
  ;; return nil.
  ;;
  (defun getindex (item itemlist / m n)
    (setq n (length itemlist))
    (if (> (setq m (length (member item itemlist))) 0)
        (- n m)
        nil
    )
  )
  ;;
  ;; This function is called if the linetype is set "BYLAYER". It finds the
  ;; ltype of the layer so it can be displayed beside the linetype button.
  ;;
  (defun bylayer_lt (/ layname layinfo ltype)
    (if lay-idx
      (progn
        (setq layname (nth lay-idx laynmlst))
        (setq layinfo (tblsearch "layer" layname))
        (setq ltype (cdr (assoc 6 layinfo)))
        (strcat "BYLAYER" " (" ltype ")")
      )
      "BYLAYER"
    )
  )
  ;;
  ;; This function is called if the color is set "BYLAYER".  It finds the
  ;; color of the layer so it can be displayed  beside the color button.
  ;;
  (defun bylayer_col (/ layname layinfo color)
    (if lay-idx
      (progn
        (setq layname (nth lay-idx laynmlst))
        (setq layinfo (tblsearch "layer" layname))
        (setq color (abs (cdr (assoc 62 layinfo))))
        (setq cn color)
        (strcat "BYLAYER" " (" (colorname color) ")")
      )
      (progn
        (setq layname elayer)
        (if (and (/= elayer "")
                 (/= elayer "Varies")
            );and
            (progn
             (setq layinfo (tblsearch "layer" elayer))
             (setq color (abs (cdr (assoc 62 layinfo))))
             (setq cn color)
             (strcat "BYLAYER" " (" (colorname color) ")")
            )
            (progn
             (setq cn 0)
             "BYLAYER"
            )
        );if
      );progn
    );if
  )
  ;;
  ;; If there is no error message, then close the dialogue
  ;;
  ;; If there is an error message, then set focus to the tile
  ;; that's associated with the error message.
  ;;
  (defun test-ok ( / errtile)
    (setq errtile (get_tile "error"))
    (cond
      (  (= errtile "")
         (done_dialog 1))
      (  (= errtile "Invalid thickness.")
         (mode_tile "eb_thickness" 2))
    )
  )
  ;;
  ;; OK in main dialogue.
  ;;
  (defun test-main-ok ( / flag)
   (setq flag T) 
   (if (not (or (distof (get_tile "eb_thickness"))
                (= "" (get_tile "eb_thickness"))
            );or
       );not
       (progn
        (set_tile "error" "Invalid thickness.")
        (mode_tile "eb_thickness" 2)
        (setq flag nil);setq
       );progn
   );if
   (if (and flag
            (not (or (< 0 (distof (get_tile "eb_ltscale")))
                     (= "" (get_tile "eb_ltscale"))
                 );or      
            );not
       );and 
       (progn
        (set_tile "error" "Invalid ltscale.")
        (mode_tile "eb_ltscale" 2)
        (setq flag nil); 
       );progn then
   );if
   (if (and flag
          ; Don't test the tile's value unless it's enabled.
          ; We're not set up for the display-only value
          ; of "" here in the error handler.
          (= 2 (logand 2 eflag))
          (not (or (< 0 (distof (get_tile "text_hgt")))
                   (= "" (get_tile "text_hgt"))
               );or
          );not
        );and
        (progn
         (set_tile "error" "Invalid height.")
         (mode_tile "text_hgt" 2)
         (setq flag nil);
        );progn then
   );if
   (if (and flag
            (= 1 (logand 1 eflag))
            (not (or (<= 0 (distof (get_tile "poly_wid")))
                     (= "" (get_tile "poly_wid"))
                 );or
            );not
       );and
       (progn
        (set_tile "error" "Invalid width.")
        (mode_tile "poly_wid" 2)
        (setq flag nil)
       );progn then
   );if 
   (if (and flag
            (= 1 (logand 1 eflag))
            (not (or (distof (get_tile "poly_elev"))
                     (= "" (get_tile "poly_elev"))
                 );or
            );not
        );and
        (progn
         (set_tile "error" "Invalid elevation.")
         (mode_tile "poly_elev" 2)
         (setq flag nil)
        );progn then
   );if  
   (if flag 
       (done_dialog 1)
   );if
  );defun test-main-ok

  ;;
  ;; A color function used by getlayer.
  ;;
  (defun colorname (colnum)
    (setq cn (abs colnum))
    (cond ((= cn 1) "red")
          ((= cn 2) "yellow")
          ((= cn 3) "green")
          ((= cn 4) "cyan")
          ((= cn 5) "blue")
          ((= cn 6) "magenta")
          ((= cn 7) "white")
          (T (itoa cn))
    )
  );defun

;;; Construct layer and ltype lists and initialize all
;;; program variables:

;  (makelaylists)                     ; layer list - laynmlst


  (makeltlists)                      ; linetype lists - ltnmlst, mdashlist

  ;; Find the property values of the selection set.
  ;; (getprops ss) returns a list of properties from
  ;; a selection set - (color ltype layer thickness HEGHT STYLE WIDTH).

  (setq proplist (getprops ss));@rk interesting things happen here

  (setq
            ecolor (car proplist)
            eltype (nth 1 proplist)
            elayer (nth 2 proplist)
        ethickness (nth 3 proplist)
          eltscale (nth 4 proplist)
            ewidth (nth 5 proplist)
        eelevation (nth 6 proplist)
           eheight (nth 7 proplist)
            ;etype (nth 8 proplist);commented out and replaced with the line below. RK.
             eflag (nth 8 proplist)
            estyle (nth 9 proplist)
           poly_ss (nth 10 proplist)
           txt_ss  (nth 11 proplist)
  );setq

  ;; Find index of linetype, and layer lists
  (cond
    ((= eltype "Varies") (setq lt-idx nil))
    ((= eltype "BYLAYER")
     (setq lt-idx (getindex "BYLAYER" ltnmlst)))
    ((= eltype "BYBLOCK")
     (setq lt-idx (getindex "BYBLOCK" ltnmlst)))
    (T (setq lt-idx (getindex eltype ltnmlst)))
  )
  (if (= elayer "Varies")
      (setq lay-idx nil)
      (setq lay-idx (getindex elayer laynmlst))
  );if
  (if (= ethickness "")
      (setq ethickness nil)
  );if
  (if (= eltscale "")
      (setq eltscale nil)
  );if

);defun ddchprop2_init   ; end (ddchprop2_init)

;;; (ddchprop2_select)
;;;
;;; Aquires selection set for DDCHPROP2, in one of three ways:
;;;
;;;   1 - Autoselected.
;;;   2 - Prompted for.
;;;   3 - Passed as an argument in a call to (ddchprop2 <ss> )
;;;
;;; The (ddchprop2_select) function also sets the value of the
;;; global symbol AI_SELTYPE to one of the above three values to
;;; indicate the method thru which the entity was aquired.


(defun ddchprop2_select ( / )

 
;returns only entities in ss that are in the current space. 
(defun ss_in_current_space ( ss / a cur_space ss2)

 (if ss
     (progn
      (if (and (equal (getvar "tilemode") 0)
               (equal (getvar "cvport") 1)
          );and 
          (setq cur_space 1);then paper space is where we are.
          (setq cur_space 0);else model space.
      );if
      (command "_.select" ss "")
      (setq ss2 (ssget "p" (list (cons 67 cur_space))));setq
      (cond                              ;;;;;tell the user what's going on.
       ((not ss2) (princ "\nNo objects found in current space."))
       ((not (equal (sslength ss) (sslength ss2)))
        (princ (strcat "\n" (itoa (- (sslength ss) (sslength ss2)))
                       " object(s) were not in current space."
               )
        )
       ) 
      );cond 
     );progn then
 );if  
 ss2
);defun ss_in_current_space


;;;begin the work of ddchprop2_select
 
 ;; temporarily restore original highlight setting.
 (b_set_sysvars (assoc "HIGHLIGHT" (car bonus_varlist)))
 (cond
   ((and ss 
         (eq (type ss) 'pickset)
    )        ; selection set passed to
    (cond                                   ; (ddchprop2) as argument
     ((not (zerop (sslength ss)))       ;   If not empty, then
      (setq ai_seltype 3)               ;   then return pickset.
      (ai_return ss)
     )
    );cond close
   );cond #1
   ((setq ss (ai_aselect)))                          ; Use current selection
                                                     ; set or prompt for objects
   (T (princ "\nNothing selected.")
      (ai_return nil)
   )
 );cond close
 (b_restore_sysvars)

 ;(if ss
 ;    (setq ss (ss_remove_locked ss)) 
 ;);if
 (if ss
     (setq ss (ss_in_current_space ss)) 
 );if

 ss
);defun ddchprop2_select


;;; Define command function.
(defun C:cg ()
  (ddchprop2 nil)
  (princ)
);defun


;;; Main program function - callable as a subroutine.
;;;
;;; (ddchprop2 <pickset> )
;;;
;;; <pickset> is the selection set of objects to be changed.
;;;
;;; If <pickset> is nil, then the current selection set is
;;; aquired, if one exists.  Otherwise, the user is prompted
;;; to select the objects to be changed.
;;;
;;; Before (ddchprop2) can be called as a subroutine, it must
;;; be loaded first.  It is up to the calling application to
;;; first determine this, and load it if necessary.

(defun ddchprop2 (ss  /

                  a
                  add-mdash
                  assoclist
                  bit-70
                  boxlength
                  bylayer-lt
                  bylayer_col
                  bylayer_lt
                  call_chp2
                  chk-col
                  ;chk-etype        ;var removed by rk 
                  chk-lay    
                  chk-lt
                  chk-lts           ;var added by rk 
                  chk-th
                  cmd
                  cmdecho
                  cn
                  cnum
                  col-idx
                  col_def
                  col_tile
                  colname
                  colnum
                  color
                  colorname
                  cvpname
                  dash
                  dashdata
                  dashlist
                  dashsize
                  dcl_id
                  ddchprop-err
                  drawpattern
                  ecolor
                  eelevation
                  eflag
                  eheight
                  elayer
                  elevation
                  elist
                  eltscale
                  eltype
                  en
                  ename
                  ESTYLE             ;var added by rk
                  ethickness
                  ;etype             ;var removed by rk
                  ewidth
                  fchk
                  frozth
                  fx
                  getcolor
                  GETELEVATION      ;function added by rk
                  GETHEIGHT         ;function added by rk
                  getindex
                  getlayer
                  ;get_locked_layers ;function added and then removed by rk 
                  getltype
                  getprops
                  getscale          ;function added by rk
                  GETSTYLE          ;function added by rk
                  getthickness
                  GETWIDTH          ;function added by rk
                  globals
                  HAIR_STYLE_LIST   ;var added by rk
                  height
                  index
                  item
                  item1
                  item2
                  itemlist
                  lay-idx
                  layedit_act
                  layer
                  layinfo
                  laylist
                  laylist_act
                  layname
                  laynmlst
                  layvalue
                  linetype
                  list1
                  longlist
                  lt-idx
                  ltabstr
                  ltedit_act
                  ltidx
                  ltlist
                  ltlist1
                  ltlist_act
                  ltname
                  ltnmlst
                  ltvalue
                  ltype
                  m
                  makelaylists
                  makeltlists
                  match-col
                  ;match-etype          ;function removed by rk
                  match-in
                  match-lay
                  match-lt
                  match-lts
                  match-th
                  match_col
                  mdashlist
                  ;MPOPLST               ;function added and then move to ac_bonus.lsp by rk
                  n
                  name
                  ncolor
                  nlayer
                  nltype
                  nthickness
                  off
                  old-idx
                  olderr
                  on
                  onoff
                  patlist
                  pattern
                  PL_WIDTH_GETTER       ;function added by rk
                  POLY_SS               ;var added by rk
                  ;POSITION              ;function added by rk and then moved to ac_bonus.lsp
                  proplist
                  reset-lay
                  reset-lt
                  s
                  selset
                  set_col_tile
                  sortlist
                  ss
                  ss_in_current_space  ;function add by rk.
                  ss-index
                  ss-length
                  ;ss_remove_locked     ;function added and then removed by rk.
                  sslen
                  temp_color
                  templist
                  test-main-ok
                  test-ok
                  testidx
                  testlay
                  th-value
                  thickness
                  tile
                  tile_rect
                  tilemode
                  ;TNLIST           ;function removed and moved to ac_bonus.lsp by rk
                  TXT_SS            ;var added by rk
                  ;undo_init        ;removed by rk.
                  vi
                  vpf
                  vpldata
                  vpn
                  which_tiles
                  width
                  x
                  x1
                  x2
                  xdlist
                  y
                  y1
                  y2
                )

  (if (and (not init_bonus_error) 
           (equal -1 (load "ac_bonus.lsp"  -1)) 
      );and
      (progn (alert "Error:\n     Cannot find AC_BONUS.LSP.")(exit))
  );if
  (init_bonus_error (list
                     (list   "cmdecho" 0 
                           "highlight" 0
                           "regenmode" 1
                             "ucsicon" 0
                     ) 
                     T     ;flag. True means use undo for error clean up.                    
                    );list  
  );init_bonus_error

  (cond
     (  (not (ai_notrans)))                       ; Not transparent?
     ;(  (not (ai_acadapp)))                       ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "exchprop")))) ; is .DCL file loaded?
     (  (not (setq ss (ddchprop2_select))))       ; objects to modify?
     (t 
        ;(ai_undo_push)
        (ddchprop2_init)                          ; Everything's cool,
        (call_chp2)                               ; so proceed!
        ;(ai_undo_pop)
     )
  );cond close

  (restore_old_error)

 (princ)
);defun ddchprop2

;;;----------------------------------------------------------------------------
;;;------------------------------------------------------------------------
;;;   DLGTEST.LSP
;;;   (C) Copyright 1990-1994 by Autodesk, Inc.
;;;
;;;   Permission to use, copy, modify, and distribute this software and its
;;;   documentation for any purpose and without fee is hereby granted.
;;;
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED
;;;   WARRANTY. ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.
;;;
;;;------------------------------------------------------------------------
;
;
; Programmable Dialog Box Test Program
;
; This program is the AutoLISP counterpart to the ADS test
; program, dlgtest.c.  It provides a simple dimensioning
; dialog invoked with the command "dimen" and a simple color
; dialog invoked with the command "setcolor".
;
; The purposes of providing this program:
; 1) Demonstrate Programmable Dialog Box use with minimum of code
;       to sort through
; 2) Demonstrate differences between LISP and ADS dialog programming
; 3) Use as a starting point for testing new dialog functions
;
; Dlgtest uses the file dlgtest.dcl as the DCL (Dialog Control Language) file.
; LISP functions are associated with dialog tiles (buttons, edit boxes,
;   etc) with the "action_tile" statements.  These actions are evaluated 
;   when the user presses buttons during the start_dialog function.
;
; Special tile names (keys): 
;   "accept" - Ok button
;   "cancel" - Cancel button

; Initialization--set the dialog position to default (centered).  
;   Only required if you want to reposition it where the user last left it.
(setq dim_pos '(-1 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; DIMEN -- AutoCAD dimensioning variables.  Set AutoCAD variables 
;   only if OK pressed, by defining the action for the "accept"
;   tile.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:dm ( / chklist realist)
  ;load DCL file
  (setq di_dcl_id (load_dialog "dlgtest.dcl"))
  (if (< di_dcl_id 0) (exit))

  ; display dialog
  (if (not (new_dialog "dimensions" di_dcl_id "" dim_pos)) (exit))

  ; Create list of button names to match AutoCAD variables
  (setq chklist '("dimse1"  "dimse2" "dimtih" "dimtoh" "dimtad"  "dimtol"
            "dimlim"  "dimalt" "dimaso" "dimsho")
  )
  (setq realist '("dimasz" "dimtsz" "dimtxt" "dimcen" "dimexo" "dimexe"
            "dimdle")
  )
  ; Send the current value of AutoCAD variables to the dialog
  (mapcar 'set_tile_int chklist)
  (mapcar 'set_tile_real realist)


  ; Define the action to take when the user presses OK, which
  ;   is to call the LISP function "dimen_ok".  If the user
  ;   terminates the dialog with CANCEL, no action will be taken.
  ;   "accept" is the key name of the OK button (tile).

  (action_tile "accept" "(dimen_ok)")


  (start_dialog)                ;returns after OK or CANCEL selected
  (unload_dialog di_dcl_id)     ;free DCL from memory
)

;If the user selects OK, this function will be called to update
;  data, etc.

(defun dimen_ok ()
  ; Get values from dialog, update AutoCAD
  (mapcar 'get_tile_int chklist)
  (mapcar 'get_tile_real realist)

  ;return 1 to start_dialog (Ok).  "dim_pos" contains the position
  ;  of the dialog.  Next call will use that position.
  (setq dim_pos (done_dialog 1)) 
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; SETCOLOR -- Test Various PDB Functions
;
;            This is a COLOR dialog that sets AutoCAD's current 
;            color using (command "_.color" color_num).  The color
;            names are displayed in a list box, color codes in an 
;            edit box, and actual color in an image tile.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:stc ( / ok coloridx clist colorstr)
  ;load DCL file
  (setq sc_dcl_id (load_dialog "dlgtest.dcl"))
  (if (< sc_dcl_id 0) (exit))

  ; get current color
  (setq colorstr (getvar "cecolor"))
  (setq coloridx (atoi colorstr))

  ; load a dialog from dialog file
  (if (not (new_dialog "setcolor" sc_dcl_id)) (exit))
                                        ; load a dialog from dialog file

  ; Set up dialog list box

  (setq clist '("255"))
  (setq idx 254)
  (while (> idx 7) 
    (setq clist (cons (itoa idx) clist))
    (setq idx (1- idx))
  )
  (setq clist (cons "White" clist))
  (setq clist (cons "Magenta" clist))
  (setq clist (cons "Blue" clist))
  (setq clist (cons "Cyan" clist))
  (setq clist (cons "Green" clist))
  (setq clist (cons "Yellow" clist))
  (setq clist (cons "Red" clist))
  (setq clist (cons "By layer" clist))
 
  (start_list "list_col")
  (mapcar 'add_list clist)
  (end_list)

  ; show initial color in image tile, list box, and edit box
  (clist_act colorstr)
  (cedit_act colorstr)

  ; Define the action to take when the user presses various buttons.
  ;   $value will be substituted with the current value from the
  ;   dialog widget, such as "4" from the 5th list box item
  ;   (zero based).
  ;
  (action_tile "list_col" "(clist_act $value)")
  (action_tile "edit_col" "(cedit_act $value)")
  (if (= 1 (start_dialog))
    ; User pressed OK
    (if (/= coloridx 0)(command "_.color" coloridx)(command "_.color" "_bylayer")))
  (unload_dialog sc_dcl_id)     ;free DCL from memory
)

; List selections end up here
(defun clist_act (value)
  ; update the edit box
  (set_tile "edit_col" value)
  (setq coloridx (atoi value))
  (color_tile "show_image" coloridx)
)

; Text entry selections end up here
(defun cedit_act (value)
  ; update the list box
  (set_tile "list_col" value)
  (setq coloridx (atoi value))
  (color_tile "show_image" coloridx)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;            General Purpose LISP PDB Functions
;
;
;   The get_ and set_ functions below assume that the tile key 
;   (button or edit box name) is the same as the AutoCAD 
;   variable name.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Get integer variable from AutoCAD and display in dialog

(defun set_tile_int (varname)
  (setq vint (getvar varname))
  (set_tile varname (itoa vint))
)


; Get floating point variable from AutoCAD and display in dialog

(defun set_tile_real (varname)
  (setq vreal (getvar varname))
  (set_tile varname (rtos vreal))
)


; Get integer variable from dialog and set in AutoCAD

(defun get_tile_int (varname)
   (setvar varname (atoi (get_tile varname)))
)

; Get floating point variable from dialog and set in AutoCAD

(defun get_tile_real (varname)
   (setvar varname (distof (get_tile varname)))
)



; Color a tile and show a border around it

(defun color_tile (tile color)
  (setq x (dimx_tile tile))
  (setq y (dimy_tile tile))
  (start_image tile)
  (fill_image 0 0 x y color)
  (tile_rect 0 0 x y 7)
  (end_image)
)

; Draw a rectangle in a tile (assumes start_image has been called)

(defun tile_rect (x1 y1 x2 y2 color)
  (setq x2 (- x2 1))
  (setq y2 (- y2 1))
  (vector_image x1 y1 x2 y1 color)
  (vector_image x2 y1 x2 y2 color)
  (vector_image x2 y2 x1 y2 color)
  (vector_image x1 y2 x1 y1 color)
)

;;;  ----------------------------------------------------------------
;;;
;;;     This file contains a library of layer based routines. See individual
;;;     routines for descriptions.
;;;
;;;  External Functions:
;;;
;;;     INIT_BONUS_ERROR  --> AC_BONUS.LSP   Intializes bonus error routine
;;;     RESTORE_OLD_ERROR --> AC_BONUS.LSP   restores old error routine
;;;
****************************************************************************************************

---------------------------------------------------------------------------------
; Next available MSG number is    22 
; MODULE_ID DDINSERT_LSP_
;;;
;;;    ddinsert.lsp
;;;
;;;    Copyright 1992, 1994, 1996 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;
;;;   DESCRIPTION
;;;
;;;   An AutoLISP implementation of the AutoCAD INSERT command with a dialogue
;;;   interface.  Answers the oft requested feature of being able to select
;;;   at Insert time either an internal or external drawing.
;;;
;;;   The user is presented with a dialogue allowing the selection from nested
;;;   dialogues of either an internal or external block.  Edit fields can be
;;;   used to enter or preset the insertion point, scale, and rotation angle,
;;;   or alternatively, these can be set dynamically as in the INSERT command.
;;;
;;;   UPDATE:  Removed code that presumed a .dwg extension for drawings.  A 
;;;   file extension is defined to be the letters following the final "." in 
;;;   the drawing name.  This extension and period are removed to give the 
;;;   block name.
;;; 
;;;----------------------------------------------------------------------------
;;;   Prefixes in command and keyword strings: 
;;;      "."  specifies the built-in AutoCAD command in case it has been           
;;;           redefined.
;;;      "_"  denotes an AutoCAD command or keyword in the native language
;;;           version, English.
;;;
;;;----------------------------------------------------------------------------
;;;
;;; ===========================================================================
;;; ===================== load-time error checking ============================
;;;

  (defun ai_abort (app msg)
     (defun *error* (s)
        (if old_error (setq *error* old_error))
        (princ)
     )
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       )
     )
     (exit)
  )

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

  (cond
     (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.

     (  (not (findfile "ai_utils.lsp"))                     ; find it
        (ai_abort "DDINSERT"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))      ; load it
        (ai_abort "DDINSERT" "Can't load file AI_UTILS.LSP"))
  )

  ;(if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
  ;    (ai_abort "DDINSERT" nil)         ; a Nil <msg> supresses
  ;)                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================


;;;----------------------------------------------------------------------------
;;;  The main dialogue.
;;;----------------------------------------------------------------------------
(defun c:ddinsert(/  
                     $value           do_insert      old_cmd            x_pt       
                     blk_exists       do_selection   parse_path         x_scale    
                     blk_name         error_msg      pat                y_pt     
                     blk_name1        explode        path_name          y_scale  
                     bl_match         ex_name        path_name_exist1   z_pt    
                     bl_name          error_scale    pat_match          z_scale 
                     check_current    globals        redefine         
                     check_fd         good_value     reset            
                     check_i          ins_name       rotation         
                     check_input      int_blocks     range
                     check_name       ins_var                     
                     check_name_ok    just_name      table_item 
                     cmd              list1                 
                                      list_blocks    table_name       
                     dcl_id           n              use_val          
                     ddinsert_main    name           use_val_tog      
                                      n_name         value            
                                                     what_next        
                  )
                                                                                  
  ;;
  ;; Routine that inserts the selected block.
  ;;
  (defun do_insert()
    (cond
      ((= 1 on_screen)
        (command "_.insert" ins_name)
      )
      ((and (= 0 on_screen) (= 0 explode))
        (command "_.insert" ins_name (list x_pt y_pt z_pt)
                 "_xyz" x_scale y_scale z_scale rotation)
      )
      ((and (= 0 on_screen) (= 1 explode))
        (command "_.insert" ins_name (list x_pt y_pt z_pt)
                 x_scale rotation)
      )
      (T (princ "programming error in do_insert"))
    )
  )  
  ;;
  ;;  Call routine to display nested dialogue.  Set edit box to returned value
  ;;  if not nil.
  ;;
  (defun int_blocks()
    (list_blocks)
    (if blk_name1 
      (progn
        (set_tile "current_name" (setq blk_name (xstrcase blk_name1)))
        (set_tile "path_name" "")
      )
    )
  )
  ;;
  ;; Short hand error tile resetting.
  ;;
  (defun reset()
    (set_tile "error" "")
  )
  ;;
  ;; Check does the block exist, either internally or externally.
  ;;
  (defun check_current()
    (setq blk_name (xstrcase (ai_strtrim (get_tile "current_name"))))     
    (setq path_name (ai_strtrim (get_tile "path_name")))
    (cond
      ;; Length less 32.
      ((not (snvalid blk_name))
        (set_tile "error" "Block name must be less than 32 characters, no spaces.")
      )
      ;; if the block exists in the drawing (necessary for logand).
      ((and (setq xcheck (tblsearch "block" blk_name))
            (not (zerop (logand 52 (cdr (assoc 70 xcheck)))))
       )
        (set_tile "error" "Error - Cannot Insert an Xref.  Use Xref Attach.")
      )
      ;; If the block is undefined give message.
      ((and (= "" path_name)
            (/= "" blk_name)
            (not (member blk_name table_list))
       )
        (set_tile "error" "Block name must be less than 32 characters, no spaces.")
      ) 
      ((and (= "" path_name)
            (= "" blk_name)
       )
        (set_tile "error" "Block name must be less than 32 characters, no spaces.")
      ) 
      ((not (or (member blk_name table_list) ; does name exist as an internal
                 (findfile path_name) ; or external block ??
            )
       )
        (set_tile "error" "Block name must be less than 32 characters, no spaces.")
      )  
      (t)  
    )
  ) 
  ;;
  ;; Check all input via a cond.  If any error exists, display the relevant 
  ;; message. If no errors bring down the dialogue.
  ;;
  (defun check_name_ok(/ ex_found)
    (setq blk_name (xstrcase (ai_strtrim (get_tile "current_name"))))
    (setq path_name (ai_strtrim (get_tile "path_name")))
    ;; Check to see if the path name is valid once here.
    (if (findfile path_name) (setq ex_found 1))

    ;; It's acceptable for the user to type in a drawing name without
    ;; entering a Block name in the Block field.  If there is a valid pathname
    ;; and an empty block name field, use parse path to get the block name 
    ;; from the drawing name.
    (if (and (= 1 ex_found) (= "" blk_name)) (parse_path))

    (setq redefine 1)
    ;; Check once here to see if the block name already exists in the drawing.
    (setq xcheck (tblsearch "block" blk_name))

    (cond 
      ((and (= "" path_name)             ; Internal
            (member blk_name table_list) 
            (= 0 explode)                ; explode off
       )
        (setq ins_name blk_name)
      ) 
      ((and (= "" path_name)             ; Internal
            (member blk_name table_list) 
            (= 1 explode)                ; explode on
       )
        (setq ins_name (strcat "*" blk_name))
      ) 
      ((and (= 1 ex_found)               ; External
            (= explode 1)                ; Explode on
       )
        (setq ins_name (strcat "*" path_name)) 
      )
      ((and (= 1 ex_found)                       ; External
            (check_name blk_name)                ; blk name valid
            (not xcheck)                         ; unique 
        ) 
        (setq ins_name (strcat blk_name "=" path_name))  
      )
      ((and (= 1 ex_found)                                ; External
            xcheck
            (zerop (logand 53 (cdr (assoc 70 xcheck))))   ; Not an Xref
            (blk_exists)                                  ; redefine yes
       )                        
        (setq ins_name (strcat blk_name "=" path_name))  
      )
      (T  
        (cond 
          ((not redefine)
          )
          ((and xcheck
                (not (zerop (logand 53 (cdr (assoc 70 xcheck))))) ; is an Xref
           )
            (set_tile "error" 
                      "Error - Cannot Insert an Xref.  Use Xref Attach."
            ) 
            ;; if the path_name in not "" set focus there on error.
            (if (read path_name)
              (mode_tile "path_name" 2)
              (mode_tile "current_name" 2)
            )
          )
          ((= "" path_name) 
            (set_tile "error" "Block name must be less than 32 characters, no spaces.")
            (mode_tile "current_name" 2)
          )
          ((and (/= "" path_name) 
	        (not (findfile path_name))
		(not (findfile (strcat path_name ".dwg"))))
            (set_tile "error" "Invalid File name.")
            (mode_tile "path_name" 2)
          )
          ((and (/= "" path_name) (findfile path_name))
            (set_tile "error" "Block name must be less than 32 characters, no spaces.")
            (mode_tile "current_name" 2)
          )
          (T (princ "Block Name check error."))
        )
        nil 
      )
    )
  )
  ;; 
  ;;  On OK all input is checked before the dialogue is dismissed.
  ;;
  (defun check_input()
    (if (= 1 explode)
      (progn
        (setq range 6)                 ; non zero & non negative
        (setq error_scale "X scale must be positive & nonzero.")
      )
      (progn
        (setq range 2)                 ; non zero
        (setq error_scale "X scale must be nonzero.")
      )
    )
    (cond
      ((not (check_name_ok)))
      ((and (= 0 on_screen) (bad_xyzxr)))          ; check insert point coords.
      ((and (= 0 on_screen)        ; check scale if explode is off
            (= 0 explode)
            (bad_yz)
      ))
      (t (done_dialog 1))           ; if all is well, bring down the dialogue.
    ) 
  )
  ;; 
  ;; Check some input and set focus on error.  Return nil is nothing bad.
  ;;
  (defun bad_xyzxr()
    (cond 
      ((not (setq x_pt (ai_num (get_tile "x_pt") "Invalid X coordinate." 0)))
        (mode_tile "x_pt" 2)
      )
      ((not (setq y_pt (ai_num (get_tile "y_pt") "Invalid Y coordinate." 0)))
        (mode_tile "y_pt" 2)
      )
      ((not (setq z_pt (ai_num (get_tile "z_pt") "Invalid Z coordinate." 0)))
        (mode_tile "z_pt" 2)
      )
      ((not (setq x_scale (ai_num 
                            (get_tile "x_scale") error_scale range)))
        (mode_tile "x_scale" 2)
      )
      ((not (setq rotation (ai_angle (get_tile "rotation")
                                    "Invalid Rotation angle." 
                           )
            )
        )
        (mode_tile "rotation" 2)
      )
      (t nil)
    )
  )
  ;;
  ;; Check the Y scale and Z scale.  Return nil if bad.
  ;;
  (defun bad_yz()
    (cond
      ((not (setq y_scale (ai_num (get_tile "y_scale") 
                                  "Y scale must be nonzero." 2)))
        (mode_tile "y_scale" 2)
      )
      ((not (setq z_scale (ai_num (get_tile "z_scale") 
                                  "Z scale must be nonzero." 2)))
        (mode_tile "z_scale" 2)
      )
    )
  )
  ;;
  ;; If called with 0, display getfiled for a drawing name. If called with 1
  ;; get the string form the edit box.
  ;;
  (defun check_fd (bit)
    (cond
      ((and (= 0 bit)
            ;; Update here for filenames without dwg extensions...
            (setq ex_name (getfiled "Select Drawing File" last_ddinsert_dir "dwg" 2))
       )
        (setq path_name ex_name) 
        (setq last_ddinsert_dir (car (fnsplitl path_name)))
        (check_fd1)
      )
      ((= 1 bit)
        (setq path_name (ai_strtrim (get_tile "path_name")))
        (setq last_ddinsert_dir "")
        (if (not (= path_name ""))
            (setq last_ddinsert_dir (car (fnsplitl path_name)))
        )
        (check_fd1)
      )
    )
  )

  (defun check_fd1( / tname) 
    (cond 
     ( (findfile path_name)           ; check to see if it exists
        (set_tile "path_name" path_name)
        (parse_path)
        (set_tile "current_name" blk_name)
        (check_current)
     )
     ( (findfile (setq tname (strcat path_name ".dwg")))  ; check to see if it exists
        (set_tile "path_name" tname)
        (parse_path)
        (set_tile "current_name" blk_name)
        (check_current)
     )

     ((and (= "" path_name)              ; OK to have a null pathname if the
           (member blk_name table_list)  ; Block name is valid
      )
     )
     (t (set_tile "error" "Invalid File name."))
    )
  )
  ;;
  ;;  Find dwg name from path name.
  ;;
  (defun parse_path( / a b c)
    (setq a 1)
    (while ( <= a (strlen path_name))
      (if (is_lead_byte(ascii (substr path_name a 1)))
        (progn
          (setq a (1+ a))
        )
        (progn
          (if (member (substr path_name a 1) '("/" "\\" ":"))
            (setq b a)
          )
          (if (member (substr path_name a 1) '("."))
            (setq c a)
          )
        )
      )
      (setq a (1+ a))
    )
    ;; Remove path
    (if b
      (setq blk_name (strcase (substr path_name (1+ b))))
      (setq blk_name (strcase path_name))
    )
    ;; Remove extension (the last period and the letters following it).
    (if (and c blk_name path_name)
      (progn
        ;; calculate c with respect to the blk_name rather than path_name.
        (setq c (- c (- (strlen path_name) (strlen blk_name))))
        (setq blk_name (strcase (substr blk_name 1 (- c 1))))
      )
    )
  )
  ;;
  ;; Enable/Disable for Insertion Point.
  ;;
  (defun on_screen_tog()
    (cond
      ((= 1 on_screen)
        (mode_tile "x_pt" 1)
        (mode_tile "y_pt" 1)
        (mode_tile "z_pt" 1)
        (mode_tile "x_scale" 1)
        (mode_tile "y_scale" 1)
        (mode_tile "z_scale" 1)
        (mode_tile "rotation" 1)
      )
      ((and (= 0 on_screen)(= 0 explode))
        (mode_tile "x_pt" 0)
        (mode_tile "y_pt" 0)
        (mode_tile "z_pt" 0)
        (mode_tile "x_scale" 0)
        (mode_tile "y_scale" 0)
        (mode_tile "z_scale" 0)
        (mode_tile "rotation" 0)
      )
      ((and (= 0 on_screen)(= 1 explode))
        (mode_tile "x_pt" 0)
        (mode_tile "y_pt" 0)
        (mode_tile "z_pt" 0)
        (mode_tile "x_scale" 0)
        (mode_tile "y_scale" 1)
        (mode_tile "z_scale" 1)
        (mode_tile "rotation" 0)
      )
    )
  )
  ;;
  ;; Displays a nested dialogue containing an edit box for wildcards and
  ;; a list box of the defined blocks in the drawing.
  ;;
  (defun list_blocks()    
    (setq bl_match '())
    (if (not (new_dialog "list_blocks" dcl_id)) (exit))
    (if (not pat) (setq pat "*"))
    (set_tile "pattern" pat)
    (pat_match pat)

    (action_tile "bl_match"   "(bl_name)")
    (action_tile "pattern"    "(pat_match (setq pat (xstrcase $value)))")
    (action_tile "selection"  "(do_selection)")
    (action_tile "accept"     "(if (check_i)(done_dialog 1))")
    (action_tile "cancel"     "(setq blk_name1 nil)(done_dialog 0)")

    (start_dialog)
  )
  ;;
  ;; If a name is typed, check to see if  block with that name exists in the
  ;; drawing.
  ;;
  (defun do_selection()
    (set_tile "bl_match" "")
    (setq blk_name1 (xstrcase (get_tile "selection")))
    (check_i)
  )
  ;;
  ;; Display the selected block name in the edit box.
  ;;
  (defun bl_name()  
    (set_tile "error" "")
    (set_tile "selection" (setq blk_name1 (nth (atoi $value) bl_match)))
  )
  ;;
  ;; Confirms that a block with the entered name exists in the drawing.
  ;;
  (defun check_i()
    (if (member blk_name1 table_list)
      (progn 
        (set_tile "error" "") 
        T
      )
      (progn 
        (set_tile "error" "Block name does not exist.")
;;;     (mode_tile "selection" 2)  Allow 'em to re-select from listbox!
        nil
      )
    )
  )
  ;;
  ;; This function displays the block list based on the pattern.
  ;;
  (defun pat_match (pat)
    (setq bl_match '())
    (foreach n table_list
      (if (wcmatch n pat)
        (setq bl_match (cons n bl_match))
      )
    )
    (if (>= (getvar "maxsort") (length bl_match)) ; Alphabetise if greater
      (if bl_match (setq bl_match (acad_strlsort bl_match))) ; than maxsort.
    )
    (start_list "bl_match")
    (mapcar 'add_list bl_match)
    (end_list)
  )
  ;;
  ;;  This function checks the validity of the Block name.  If legitimate, the  
  ;;  Block name is returned, nil otherwise.
  ;;
  (defun check_name(name)
    (if (not (or (not name)
                 (= "" name)        
                 (not (snvalid name))
             )
        )  
      name
    )
  )
  ;;
  ;; Post a message, when focus is changed from new name, stating that a block
  ;; already exists with this name which will be redefined. 
  ;;
  (defun path_name_exist1()
    (if (member n_name table_list)
      (set_tile "error" "A block with this name will be redefined.")
    )
  ) 
  ;;
  ;;  An Alert dialogue, called on OK to get confirmation of redefining block.
  ;;  Return T if redefine and nil if Cancel.
  ;;
  (defun blk_exists()
    (if (not (new_dialog "blk_exists" dcl_id)) (exit))   
    (action_tile "redefine" "(done_dialog 2)")
    (action_tile "cancel" "(done_dialog 0)")
    (if (= (start_dialog) 2)  T (setq redefine nil)) 
  )

  ;;
  ;; Update the Y and Z scale when X scale is changed.
  ;;
  (defun up_xscale(/ x_temp)
    (reset)
    (if (= 1 explode)
      (progn
        (setq range 6)                 ; non zero & non negative
        (setq error_scale "X scale must be positive & nonzero.")
      )
      (progn
        (setq range 2)                 ; non zero
        (setq error_scale "X scale must be nonzero.")
      )
    )
    (if (setq x_temp (ai_num (get_tile "x_scale") error_scale range))
      (progn 
        (set_tile "y_scale" (rtos x_temp))
        (set_tile "z_scale" (rtos (abs x_temp)))
      )
    )
  )
  ;;
  ;; Put up the dialogue.
  ;;
  (defun ddinsert_main()
  
    (if (not (new_dialog "ddinsert" dcl_id)) (exit))


    ;; Find the defined blocks in the drawing.
    (setq table_list (ai_table "block" 14)) ; no anonymous, Xrefs or 
                                            ; Xref dependents.

    ;; Set up some defaults.
    (setq x_pt 0.0)                     (set_tile "x_pt" (rtos x_pt))             
    (setq y_pt 0.0)                     (set_tile "y_pt" (rtos y_pt))             
    (setq z_pt (getvar "elevation"))    (set_tile "z_pt" (rtos z_pt))             
    (setq x_scale 1.0)                  (set_tile "x_scale" (rtos x_scale))          
    (setq y_scale 1.0)                  (set_tile "y_scale" (rtos y_scale))          
    (setq z_scale 1.0)                  (set_tile "z_scale" (rtos z_scale))          
    (setq rotation 0.0)                 (set_tile "rotation" (angtos rotation))       

    ;; initialize our directory memory
    (if (= last_ddinsert_dir nil)
        (setq last_ddinsert_dir "")
    )

    ;; If a default exists for the on screen toggle, use it.  Else set the 
    ;; toggle to 1.
    (if (setq on_screen (cadr (assoc "ddinsert" ai_defaults)))
      (set_tile "on_screen" (itoa on_screen))
      (set_tile "on_screen" (itoa (setq on_screen 1)))
    )
    (on_screen_tog)

    (set_tile "explode" "0") (setq explode 0)
    ;; If the last insert was of a *block (explode on), then insname
    ;; will have a * in front of the block name.  If the blk_name
    ;; exists within the drawing, then put the name in the block field
    ;; and leave the File field empty, else put the path in the file field
    ;; and the block name in the block field.  In both cases, leave explode off.
    (setq ins_var (getvar "insname"))
    (if (= "*" (substr ins_var 1 1))
      (setq path_name (substr ins_var 2))
      (setq path_name ins_var)
    )
    (parse_path)
    (set_tile "current_name" blk_name)
    (if (member blk_name table_list)
      (set_tile "path_name" (setq path_name ""))
      (set_tile "path_name" path_name)
    )  

    (action_tile "int_blocks" "(reset)(int_blocks)") 
    (action_tile "ext_blocks" "(reset)(check_fd 0)") 
    (action_tile "current_name" "(reset)(check_current)")
    (action_tile "path_name" "(reset)(check_fd 1)")
    (action_tile "on_screen" "(reset)(setq on_screen (atoi $value))(on_screen_tog)")
    (setq cmd_coor (strcat "(reset)(ai_num $value \""
                           "Invalid X coordinate."
			   "\" 0)"))
    (action_tile "x_pt" cmd_coor)
    (setq cmd_coor (strcat "(reset)(ai_num $value \""
                           "Invalid Y coordinate."
			   "\" 0)")) 
    (action_tile "y_pt" cmd_coor)
    (setq cmd_coor (strcat "(reset)(ai_num $value \""
                           "Invalid Z coordinate."
			   "\" 0)")) 
    (action_tile "z_pt" cmd_coor)
    (action_tile "x_scale" "(up_xscale)")
    (setq cmd_scale (strcat "(reset)(ai_num $value \""
		     "Y scale must be nonzero."
		     "\" 2)"))
    (action_tile "y_scale" cmd_scale)

    (setq cmd_scale (strcat "(reset)(ai_num $value\""
                     "Z scale must be nonzero." 
                     "\" 2)")) 
    (action_tile "z_scale" cmd_scale)

    (setq cmd_scale (strcat "(reset)(ai_angle $value \"" 
                     "Invalid Rotation angle."  
                     "\")"))  
    (action_tile "rotation" cmd_scale)

    (action_tile "explode" "(setq explode (atoi $value))(on_screen_tog)")
    (action_tile "accept" "(check_input)")
    (action_tile "cancel" "(done_dialog 0)")
    (action_tile "help" "(help \"\" \"ddinsert\")")

    (setq what_next (start_dialog))

    (if (= 1 what_next) 
      (progn
        (do_insert)
        (if (assoc "ddinsert" ai_defaults)
          (setq ai_defaults (subst (list "ddinsert" on_screen) 
                                   (assoc "ddinsert" ai_defaults)
                                   ai_defaults
                            )
          )
          (setq ai_defaults (cons (list "ddinsert" on_screen) ai_defaults))
        )
       )
    )
  )

  ;; Set up error function.
  (setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
  )

  (setvar "cmdecho" 0)

  (cond
     (  (not (ai_notrans)))                       ; transparent not OK
     ;(  (not (ai_acadapp)))                       ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "ddinsert"))))  ; is .DCL file loaded?

     (t (ddinsert_main))                          ; proceed!
  )

  (setq *error* old_error) 
  (setvar "cmdecho" old_cmd)
  (princ)
)
;;;----------------------------------------------------------------------------
(defun c:D100 ()
 (setq ex (tblsearch "dimstyle" "D100"))
  
  (if (= ex nil)
    (progn  
      (setvar "dimexe" 23)
      (setvar "dimexo" 1)
      (setvar "dimtxt" 2)
      (setvar "dimtad" 3)
      (setvar "dimasz" 4)
      (setvar "dimgap" 5) 
      (setvar "dimscale" 6)
      (setvar "dimclrd" 7)
      (setvar "dimclre" 8)
      (setvar "dimclrt" 9)
      (setvar "dimunit" 10)
      (setvar "dimaltd" 11)
      (setvar "DIMDEC" 12)
      (setvar "DIMALTTZ" 13)
      (setvar "DIMALTu" 14)
      (setvar "DIMALTz" 15) 
      (setvar "DIMFIT" 16)
      (setvar "DIMJUST" 17)
      (SETVAR "DIMTOLJ" 18)
      (SETVAR "DIMZIN" 19)  
      (SETVAR "DIMTVP" 20)
      (SETVAR "DIMTSZ" 21)
      (SETVAR "DIMTP" 22)  
    

      (command "dimstyle" "sa" "D100$7")
      (command "dimstyle" "sa" "D100")
    )

  nil

  )
  
)
**********************************************************************
(defun c:D75 ()
 (setq ex (tblsearch "dimstyle" "D75"))
  
  (if (= ex nil)
    (progn  
      (setvar "dimexe" 1)
      (setvar "dimexo" 1)
      (setvar "dimtxt" 1.8)
      (setvar "dimtad" 0)
      (setvar "dimasz" 1.25)
      (setvar "dimgap" 0.75) 
      (setvar "dimscale" 100)
      (setvar "dimclrd" 9)
      (setvar "dimclre" 9)
      (setvar "dimclrt" 3)
      (setvar "dimunit" 1)
      (setvar "dimaltd" 1)
      (setvar "DIMDEC" 0)
      (setvar "DIMALTTZ" 0)
      (setvar "DIMALTu" 2)
      (setvar "DIMALTz" 0) 
      (setvar "DIMFIT" 3)
      (setvar "DIMJUST" 0)
      (SETVAR "DIMTOLJ" 0)
      (SETVAR "DIMZIN" 0)  
      (SETVAR "DIMTVP" 0)
      (SETVAR "DIMTSZ" 1.8)
      (SETVAR "DIMTP" 1)      

      (command "dimstyle" "sa" "D75$7")
      (command "dimstyle" "sa" "D75")
    )

  nil

  )
  
)

*************************************************************************
(defun c:D50 ()
 (setq ex (tblsearch "dimstyle" "D50"))
  
  (if (= ex nil)
    (progn  
      (setvar "dimexe" 1.5)
      (setvar "dimexo" 1.5)
      (setvar "dimtxt" 2.5)
      (setvar "dimtad" 1)
      (setvar "dimasz" 2.0)
      (setvar "dimgap" 0.75) 
      (setvar "dimscale" 50)
      (setvar "dimclrd" 3)
      (setvar "dimclre" 3)
      (setvar "dimclrt" 7)
      (setvar "dimunit" 2)
      (setvar "dimaltd" 2)
      (setvar "DIMDEC" 0)
      (setvar "DIMALTTZ" 0)
      (setvar "DIMALTu" 2)
      (setvar "DIMALTz" 0) 
      (setvar "DIMFIT" 3)
      (setvar "DIMJUST" 0)
      (SETVAR "DIMTOLJ" 1)
      (SETVAR "DIMZIN" 0)  
      (SETVAR "DIMTVP" 0)
      (SETVAR "DIMTSZ" 0)
      (SETVAR "DIMTP" 0)      

      (command "dimstyle" "sa" "D50$7")
      (command "dimstyle" "sa" "D50")
    )

  nil

  )
  
)
*************************************************************************
(defun c:D25 ()
 (setq ex (tblsearch "dimstyle" "D25"))
  
  (if (= ex nil)
    (progn  
      (setvar "dimexe" 1.5)
      (setvar "dimexo" 1.5)
      (setvar "dimtxt" 2.5)
      (setvar "dimtad" 1)
      (setvar "dimasz" 2.0)
      (setvar "dimgap" 0.75) 
      (setvar "dimscale" 25)
      (setvar "dimclrd" 3)
      (setvar "dimclre" 3)
      (setvar "dimclrt" 7)
      (setvar "dimunit" 2)
      (setvar "dimaltd" 2)
      (setvar "DIMDEC" 0)
      (setvar "DIMALTTZ" 0)
      (setvar "DIMALTu" 2)
      (setvar "DIMALTz" 0) 
      (setvar "DIMFIT" 3)
      (setvar "DIMJUST" 0)
      (SETVAR "DIMTOLJ" 1)
      (SETVAR "DIMZIN" 0)  
      (SETVAR "DIMTVP" 0)
      (SETVAR "DIMTSZ" 0)
      (SETVAR "DIMTP" 0)      

      (command "dimstyle" "sa" "D25$7")
      (command "dimstyle" "sa" "D25")
    )

  nil

  )
  
)
***************************************************************************
(defun c:D30 ()
 (setq ex (tblsearch "dimstyle" "D30"))
  
  (if (= ex nil)
    (progn  
      (setvar "dimexe" 1.5)
      (setvar "dimexo" 1.5)
      (setvar "dimtxt" 2.5)
      (setvar "dimtad" 1)
      (setvar "dimasz" 2.0)
      (setvar "dimgap" 0.75) 
      (setvar "dimscale" 30)
      (setvar "dimclrd" 3)
      (setvar "dimclre" 3)
      (setvar "dimclrt" 7)
      (setvar "dimunit" 2)
      (setvar "dimaltd" 2)
      (setvar "DIMDEC" 0)
      (setvar "DIMALTTZ" 0)
      (setvar "DIMALTu" 2)
      (setvar "DIMALTz" 0) 
      (setvar "DIMFIT" 3)
      (setvar "DIMJUST" 0)
      (SETVAR "DIMTOLJ" 1)
      (SETVAR "DIMZIN" 0)  
      (SETVAR "DIMTVP" 0)
      (SETVAR "DIMTSZ" 0)
      (SETVAR "DIMTP" 0)
      (command "dimstyle" "sa" "D30$7")
      (command "dimstyle" "sa" "D30")
    )

  nil

  )
  
)
*************************************************************************
(defun c:rot()(ssget) 
(prompt "\nSELECT OBJECT TO CHANGED ..Program by GANESH AGARE.")
(COMMAND "change" "" "" "romant" "" "" ""))
     


