
                 
(princ "\n*...****BISMILLAH**** LA-EE-LAHA IL-LALLAH MOHAMMADUR RASUL-LALLAH...*")
                      (GRTEXT -1 "BISMILLAH...")
;;;           Copyright (C) 1997-2004 by PERVEZ
;;;                      (For AutoCAD r2004)
;---------------------------------------------------------------------------------------------------------------!!!!
                                mohd altaf9.LSP
;                        Developed by mohd altaf
;---------------------------------------------------------------------------------------------------------------!!!!
**************************************************************************************
;ORDINARY LSP which inserts blocks or dwg....created in 2000

 (DEFUN C:a0()(COMMAND "insert" "a0"))
 (DEFUN C:a1()(COMMAND "insert" "a1"))
 (DEFUN C:a2()(COMMAND "insert" "a2"))
 (DEFUN C:a3()(COMMAND "insert" "a3"))
 (DEFUN C:a4()(COMMAND "insert" "a4"))
 (DEFUN C:gd()(COMMAND "insert" "ground-symbol"))
 (DEFUN C:hr()(COMMAND "insert" "handrail-detail"))
 (DEFUN C:pc()(COMMAND "insert" "precast-cover"))
 (DEFUN C:rb()(COMMAND "insert" "rubblesoling"))
 (DEFUN C:sx()(COMMAND "insert" "section"))
 (DEFUN C:es()(COMMAND "insert" "extra-steel-pipe-opening"))
 (DEFUN C:ty()(COMMAND "insert" "tor"))
 (DEFUN C:sump()(COMMAND "insert" "sumppit-detail"))
 (DEFUN C:fo()(COMMAND "insert" "footing-detail"))
 (DEFUN C:ud()(COMMAND "insert" "up&dn"))
 (DEFUN C:mh()(COMMAND "insert" "man-hole-detail"))
 (DEFUN C:SSB()(COMMAND "insert" "SIMPLY-SUPP-BEAM-DETAIL"))
 (DEFUN C:CSS()(COMMAND "insert" "COL-SCHEDULE"))
 (DEFUN C:BS()(COMMAND "insert" "BEAMSCHEDULE"))
 (DEFUN C:CJ()(COMMAND "insert" "CONSTRUCTION-JOINT"))
 (DEFUN C:GS()(COMMAND "insert" "GRADE-SLAB"))
 (DEFUN C:CB()(COMMAND "insert" "CONTINOUS-BEAM-DETAIL"))
 (DEFUN C:SS()(COMMAND "insert" "SLAB-SCHEDULE"))
 (DEFUN C:BD()(COMMAND "insert" "BEAM-DETAIL"))
 (DEFUN C:SD()(COMMAND "insert" "COLUMNSPLICING"))
 (DEFUN C:CAT()(COMMAND "insert" "CATLADDER"))


;ORDINARY LSP
 *********
(DEFUN C:MEA()(COMMAND "_MEASURE" ""))
(DEFUN C:WB()(COMMAND "WBLOCK" ""))
(DEFUN C:CF()(COMMAND "_CHAMFER" )) 
(DEFUN C:DB()(COMMAND ".DRAWORDER" )) 
(DEFUN C:reco()(COMMAND "recover" )) 
(DEFUN C:DG()(COMMAND "DIM" "GAP"))
(DEFUN C:DF()(COMMAND "DIM" "STYLE"))
(DEFUN C:EH()(COMMAND "_hatchedit"))
(DEFUN C:pp()(COMMAND "_pasteclip")) 
(DEFUN C:drga()(COMMAND "'_ddrmodes"))
(DEFUN C:L0 () (COMMAND "'LUPREC" "0"))
 (DEFUN C:L2 () (COMMAND "'LUPREC" 2))
 (DEFUN C:A00 () (COMMAND "'AUPREC" "0"))
 (DEFUN C:A20 () (COMMAND "'AUPREC" "2"))
 (DEFUN C:OSF () (COMMAND "'OSNAP" "NONE" ))
 (DEFUN C:OP () (COMMAND "'OSNAP" "END,INT,CEN,MID,QUA,PER,NODE,ins,NEAR" ))
 (DEFUN C:RS () (COMMAND "STYLE"  "romans" "romans" "" "" "" "" "" "")) 
 (DEFUN C:RD () (COMMAND "STYLE"  "romand" "romand" "" "" "" "" "" ""))
 (DEFUN C:Ret ()(COMMAND "STYLE"  "romant" "romant" "" "" "" "" "" ""))
 (DEFUN C:Rcv () (COMMAND "STYLE"  "romanCIV" "romanciv" "" "" "" "" "" ""))
 (DEFUN C:Rc () (COMMAND "STYLE"  "romanc" "romanc" "" "" "" "" "" ""))
 (defun c:ds()(setvar "cmdecho" 0)(command "dimscale"))
 (defun c:lts()(command "ltscale"))
 (defun c:a()(command "array"))
 (defun c:ar()(command "arc"))
 (defun c:tR()(command "trim"))
 (defun c:pt()(command "_plot"))
 (defun c:qQ()(command "quit" "n"))
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
 (defun c:dl () (command "dim" "lea" ))
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
 (DEFUN C:ORF   () (COMMAND "_UCSICON" "OFF"))
 (DEFUN C:ORO   () (COMMAND "_UCSICON" "ON"))                   
 (defun c:bf()(command "break" pause "f" pause "@" ""))
 (defun c:eli()(command "erase" "l"))
 (defun c:res()(command "dim1" "res"))
 (defun c:v()(command "view" "r" "bl"))
 (defun c:vw()(command "view" "w"))
 (defun c:rdd()(command "redraw"))
 (defun c:CAL()(command "sh" "calc"))
 (defun c:CON()(command "sh" "CONVERTER"))
 (DEFUN C:cp()(COMMAND "_copyclip"))
 (DEFUN C:CG()(COMMAND "CHPROP"))

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
(DEFUN C:F66()(COMMAND "FILLET" "R" "66" "fillet"))
(DEFUN C:F67()(COMMAND "FILLET" "R" "67" "fillet"))
(DEFUN C:F68()(COMMAND "FILLET" "R" "68" "fillet"))
(DEFUN C:F69()(COMMAND "FILLET" "R" "69" "fillet"))
(DEFUN C:F70()(COMMAND "FILLET" "R" "70" "fillet"))
(DEFUN C:F71()(COMMAND "FILLET" "R" "71" "fillet"))
(DEFUN C:F72()(COMMAND "FILLET" "R" "72" "fillet"))
(DEFUN C:F73()(COMMAND "FILLET" "R" "73" "fillet"))
(DEFUN C:F74()(COMMAND "FILLET" "R" "74" "fillet"))
(DEFUN C:F75()(COMMAND "FILLET" "R" "75" "fillet"))
(DEFUN C:F76()(COMMAND "FILLET" "R" "76" "fillet"))
(DEFUN C:F77()(COMMAND "FILLET" "R" "77" "fillet"))
(DEFUN C:F78()(COMMAND "FILLET" "R" "78" "fillet"))
(DEFUN C:F79()(COMMAND "FILLET" "R" "79" "fillet"))
(DEFUN C:F80()(COMMAND "FILLET" "R" "80" "fillet"))
(DEFUN C:F81()(COMMAND "FILLET" "R" "81" "fillet"))
(DEFUN C:F82()(COMMAND "FILLET" "R" "82" "fillet"))
(DEFUN C:F83()(COMMAND "FILLET" "R" "83" "fillet"))
(DEFUN C:F84()(COMMAND "FILLET" "R" "84" "fillet"))
(DEFUN C:F85()(COMMAND "FILLET" "R" "85" "fillet"))
(DEFUN C:F86()(COMMAND "FILLET" "R" "86" "fillet"))
(DEFUN C:F87()(COMMAND "FILLET" "R" "87" "fillet"))
(DEFUN C:F88()(COMMAND "FILLET" "R" "88" "fillet"))
(DEFUN C:F89()(COMMAND "FILLET" "R" "89" "fillet"))
(DEFUN C:F90()(COMMAND "FILLET" "R" "90" "fillet"))
(DEFUN C:F91()(COMMAND "FILLET" "R" "91" "fillet"))
(DEFUN C:F92()(COMMAND "FILLET" "R" "92" "fillet"))
(DEFUN C:F93()(COMMAND "FILLET" "R" "93" "fillet"))
(DEFUN C:F94()(COMMAND "FILLET" "R" "94" "fillet"))
(DEFUN C:F95()(COMMAND "FILLET" "R" "95" "fillet"))
(DEFUN C:F96()(COMMAND "FILLET" "R" "96" "fillet"))
(DEFUN C:F97()(COMMAND "FILLET" "R" "97" "fillet"))
(DEFUN C:F98()(COMMAND "FILLET" "R" "98" "fillet"))
(DEFUN C:F99()(COMMAND "FILLET" "R" "99" "fillet"))
(DEFUN C:F100()(COMMAND "FILLET" "R" "100" "fillet"))
(DEFUN C:F101()(COMMAND "FILLET" "R" "101" "fillet"))
(DEFUN C:F102()(COMMAND "FILLET" "R" "102" "fillet"))
(DEFUN C:F103()(COMMAND "FILLET" "R" "103" "fillet"))
(DEFUN C:F104()(COMMAND "FILLET" "R" "104" "fillet"))
(DEFUN C:F105()(COMMAND "FILLET" "R" "105" "fillet"))
(DEFUN C:F106()(COMMAND "FILLET" "R" "106" "fillet"))
(DEFUN C:F107()(COMMAND "FILLET" "R" "107" "fillet"))
(DEFUN C:F108()(COMMAND "FILLET" "R" "108" "fillet"))
(DEFUN C:F109()(COMMAND "FILLET" "R" "109" "fillet"))
(DEFUN C:F110()(COMMAND "FILLET" "R" "110" "fillet"))
(DEFUN C:F111()(COMMAND "FILLET" "R" "111" "fillet"))
(DEFUN C:F112()(COMMAND "FILLET" "R" "112" "fillet"))
(DEFUN C:F113()(COMMAND "FILLET" "R" "113" "fillet"))
(DEFUN C:F114()(COMMAND "FILLET" "R" "114" "fillet"))
(DEFUN C:F115()(COMMAND "FILLET" "R" "115" "fillet"))
(DEFUN C:F116()(COMMAND "FILLET" "R" "116" "fillet"))
(DEFUN C:F117()(COMMAND "FILLET" "R" "117" "fillet"))
(DEFUN C:F118()(COMMAND "FILLET" "R" "118" "fillet"))
(DEFUN C:F119()(COMMAND "FILLET" "R" "119" "fillet"))
(DEFUN C:F120()(COMMAND "FILLET" "R" "120" "fillet"))
(DEFUN C:F121()(COMMAND "FILLET" "R" "121" "fillet"))
(DEFUN C:F122()(COMMAND "FILLET" "R" "122" "fillet"))
(DEFUN C:F123()(COMMAND "FILLET" "R" "123" "fillet"))
(DEFUN C:F124()(COMMAND "FILLET" "R" "124" "fillet"))
(DEFUN C:F125()(COMMAND "FILLET" "R" "125" "fillet"))
(DEFUN C:F126()(COMMAND "FILLET" "R" "126" "fillet"))
(DEFUN C:F127()(COMMAND "FILLET" "R" "127" "fillet"))
(DEFUN C:F128()(COMMAND "FILLET" "R" "128" "fillet"))
(DEFUN C:F129()(COMMAND "FILLET" "R" "129" "fillet"))
(DEFUN C:F130()(COMMAND "FILLET" "R" "130" "fillet"))
(DEFUN C:F150()(COMMAND "FILLET" "R" "150" "fillet"))
(DEFUN C:F175()(COMMAND "FILLET" "R" "175" "fillet"))
(DEFUN C:F200()(COMMAND "FILLET" "R" "200" "fillet"))
****************************************************************
;;
;;  save all open drawings
;;
(defun C:sav (/ dwg saveDwg)
  ;;  save drawing
  (defun saveDwg (dwg / titled writeable name)
    (setq titled (= 1 (vlax-variant-value (vla-getvariable dwg "DWGTITLED")))
          writeable (= 1 (vlax-variant-value (vla-getvariable dwg "WRITESTAT")))
          name (if titled
                 (vlax-get dwg "fullname")
                 (vlax-variant-value (vla-getvariable dwg "DWGNAME")) )
    )
    (cond
      ;;  REFEDIT active ??
      ((/= "" (vlax-variant-value (vla-getvariable dwg "REFEDITNAME")))
        (acet-ui-message
          (acet-str-format "%1\nCannot Save while REFEDIT active." name)
          "AutoCAD - SAVEALL"
          Acet:ICONSTOP )
      )
      ;;  simple save if titled and writeable
      ((and titled writeable)
        (vla-save dwg)
      )
      ;;  otherwise ask for name first
      (T
        (if (setq name (ACET-FILE-WRITEDIALOG "Save Drawing As" name "dwg" 1))
          (vla-saveas dwg (vlax-make-variant name)) )
      )
    )
  )

  ;;  quietly
  (acet-error-init '(("CMDECHO" 0)))

  ;;  for each drawing
  (vlax-for dwg (vla-get-documents (vlax-get-acad-object))
    ;;  save if modified
    (if (/= 0 (vlax-variant-value (vla-getvariable dwg "DBMOD")))
      (saveDwg dwg) )
  )

  (acet-error-restore)
  (princ)
)



****************************************************************
(defun c:CH (/ elst no count en old str new enlst )    ; this one changes text
  (prompt "\nSelect Text to change: ")
  (setq elst (ssget ))
  (setq no (sslength elst))
  (setq count 0)
  (repeat no
     (setq en (ssname elst count))
      (command (redraw en 3)) 
      (setq enlst (entget en))
      (setq old (assoc 1 enlst))
      (setq str (getstring old "\nEnter new text: "))
      (setq new old)
      (setq new (cons 1 str))
      (setq enlst (subst new old enlst))
      (entmod enlst)
      (setq count (1+ count))
 )
 (princ "CHANGED ")
 (princ count)
 (princ " TEXT LINES")
 (terpri)
)

(defun dtr(a)
       (* pi (/ a 180.0))
)

**************************************************************** 
 ;MULTIPLE TEXT EDIT.
 (DEFUN C:11()(princ "\n ** Multi Text Change Program by mohd altaf.")
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
 (defun c:22()(princ "\n ** Change Entity Prop. Program by mohd altaf.")
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
 (Defun C:33 () (princ "\n***  program by mohd altaf ....") 
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
(defun C:44 () (princ "\n***  program  by mohd altaf ....")
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
***********************************************************************
(defun c:444 ( / v1 v2 v3 newtx nme oldtx)(princ "\n*** Program by mohd altaf...")
(setvar "cmdecho" 0)
          (prompt "\nPick text to be changed.")
          (setq v1 (ssget '((0 . "TEXT")) ))
          (setq newtx (getstring T "\nEnter new string: "))
          (setq newtx (cons 1 newtx))
          (setq v2 0)
             (if (and v1 newtx)
                 (while (< v2 (sslength v1))
                        (setq nme (ssname v1 v2))
                        (setq oldtx (assoc 1 (entget nme)))
                        (setq v3 (entget nme))
                        (entmod (subst newtx oldtx v3))
                        (entupd nme)
                        (setq v2 (+ v2 1))
                 )
             )
)







*********************************************************************
;GET TOTAL .LISP
 (DEFUN C:55 ()(princ "\n*** Program by mohd altaf...")
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
        (princ "\n*** Program by mohd altaf...")
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
        (princ "\n*** Program by mohd altaf...")
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
***************
CHANGES BLOCK ON GIVEN SCALE


(defun c:RIB ()
  (setq elst (ssget))
  (initget 1)
  (SETQ OB   (GETSTRING "\n GIVE BLOCK NAME TO REPLACE :"))
  (SETQ no   (sslength elst))
  (SETQ  count 0)
  (initget 3 " Y N ")
  (setq  q  (strcase (getkword "\nReplace blocks with old scale ? :<y> ")))
  (if (= q "Y") (osc) (nsc)))


 (defun nsc()
 (setq  x1 (GETREAL "\n NEW SCALE FACTOR:") y1 X1 Z1 X1 )
  (repeat no
  (setq en    (ssname elst count)
   S2    (ENTGET EN)         OLD   (ASSOC 2 S2)          NEW   (CONS 2 OB)
   S2    (SUBST NEW OLD S2)  OLD1  (ASSOC 41 S2) )  

; TINY MODFICATION

   (IF ( < (CDR OLD1) 0 ) (SETQ NEW1 (CONS 41 (- 0 X1 )))   
                          (SETQ   NEW1  (CONS 41 x1))   
   ) ; END IF

; EO MODIFICATION

   (SETQ  OLD2  (ASSOC 42 S2)       NEW2  (CONS 42 y1)          OLD3  (ASSOC 43 S2)
          NEW3  (CONS 43 X1)          S2  (SUBST NEW1 OLD1 S2)  
            S2  (SUBST NEW2 OLD2 S2)  S2  (SUBST NEW3 OLD3 S2))
   (ENTMOD S2) ;                     modify entity
   (setq count (1+ count)) ) )

  (defun osc()
  (repeat no
  (setq en    (ssname elst count)   S2    (ENTGET EN)         
  OLD   (ASSOC 2 S2)  NEW   (CONS 2 OB)  S2    (SUBST NEW OLD S2)) 
   (ENTMOD S2)    (setq count (1+ count))  )  )

*******************************************************************************
(defun C:MODI()
(setvar "cmdecho" 0)
(setq olds(getREAL "\nPlease Enter Old Scale......:"))
(setq news(getREAL "\nPlease Enter New Scale......:"))
(setq scm(ssget))
(setq pole(getpoint "\nPlease Enter Base Point......:"))
(setq txsize (getdist "\nEnter New Text Height.....:"))
(PROMPT "<<<----RUKZA NAHI TO TERE LAVDE LAGENGE.---->>>")
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
            (COMMAND "osnap" "end,mid,per,qua,ins,int,cen,node")

        )
         (SETQ IND(+ 1 IND))
 )
)
************************************************************************************
****************************************************************************
 
; TO GIVE THE DESIRED THICKNESS TO THE SELECTED polyLINES
 (defun c:wd1()
         (prompt "\nSelect the polylines ..Program by mohd altaf.")
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
(defun c:LN()(princ "\n ** Line Program by mohd altaf")
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
(defun c:c1()(princ "\nProgram to convert INCH'S into MM by mohd altaf***")
       (setq p1 (getreal "\nEnter value in Inch...."))
       (setq p2 (* p1 25.4 ))                                 
       (setq p3 (rtos p2 ))
       (princ (strcat "The answer is  " p3  "mm"))(princ))

 (defun c:c2()(princ "\nProgram to convert MM'S into FEET by mohd altaf***")
       (setq p1 (getreal "\nEnter value in MM...."))
       (setq p2 (* p1 0.039370078))                                 
       (setq p3 (rtos p2 ))
       (princ (strcat "The answer is  " p3 "inc"))(princ))

(defun c:c3()(princ "\nProgram to convert INCH'S into FEET by mohd altaf***")
       (setq p1 (getreal "\nEnter value in INCH...."))
       (setq p2 (* p1 0.083333333))                                 
       (setq p3 (rtos p2 ))
       (princ (strcat "The answer is  " p3 "..FEET.."))(princ))

 
---------------------------------------------------------------------
;PUT BRACKET ON TEXT
***********************
(defun c:brc()(setvar "cmdecho" 0)
        (princ "\nAdd bracket to Text, program by mohd altaf")
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
 (defun c:Ul()(princ "\n Program by mohd altaf")
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
(defun c:rUl()(princ "\n Program by mohd altaf")
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
 (princ "\n*** Program by mohd altaf")
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
 (princ "\n*** Program by mohd altaf.")
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
 
 (defun c:UC()(princ "\n*** Program by mohd altaf.")
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
 (defun c:LC()(princ "\n*** Program by mohd altaf.")
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
(defun c:AD()(setq gets (getstring T "\nTYPE TEXT TO ADD  ...."))
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

*********************************************************
; TO CHANGE A TEXT'S HEIGHT WRT ANOTHER TEXT'S HEIGHT
 
 (defun c:HTT()(princ "\n*** Program by mohd altaf.")
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
 

*********************************************************
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
*********************************************************

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
   (setq ts (tblsearch "LAYER" "12"))
   (if (null ts) 
     (prompt "\nCreating new layer - 12. ") 
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
      (if e (redraw e 12))
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
       (command "_.LAYER" "_M" "12" "") 
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
       (command "Change" "l" "" "p" "la" "4" "" "" "")
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
 (defun c:MTC()(princ "\n*** Program by mohd altaf.")
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
(DEFUN C:TP()(princ "\n ** Program for text (TYP)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "(TYP.)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------
(DEFUN C:28()(princ "\n ** Program for text (2-8#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-8#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------
(DEFUN C:26()(princ "\n ** Program for text (2-6#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-6#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------
(DEFUN C:210()(princ "\n ** Program for text (2-10#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-10#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:212()(princ "\n ** Program for text (2-12#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-12#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:216()(princ "\n ** Program for text (2-16#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-16#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------------------------
(DEFUN C:220()(princ "\n ** Program for text (2-20#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-20#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------------
(DEFUN C:225()(princ "\n ** Program for text (2-25#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-25#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------------------------------------
(DEFUN C:232()(princ "\n ** Program for text (2-32#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "2-32#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-----------------------------------------------------------
(DEFUN C:38()(princ "\n ** Program for text (3-8#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-8#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------
(DEFUN C:310()(princ "\n ** Program for text (3-10#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-10#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-----------------------------------------------------------------------------------
(DEFUN C:312()(princ "\n ** Program for text (3-12#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-12#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------------------------------
(DEFUN C:316()(princ "\n ** Program for text (3-16#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-16#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------------------------------
(DEFUN C:320()(princ "\n ** Program for text (3-20#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-20#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
--------------------------------------------------------------------------------------
(DEFUN C:325()(princ "\n ** Program for text (3-25#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-25#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-----------------------------------------------------------------------------------
(DEFUN C:328()(princ "\n ** Program for text (3-28#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-28#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------------------------------
(DEFUN C:332()(princ "\n ** Program for text (3-32#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "3-32#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

------------------------------------------------------------------------------------
(DEFUN C:48()(princ "\n ** Program for text (4-8#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-8#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------------------------------------
(DEFUN C:410()(princ "\n ** Program for text (4-10#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-10#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
---------------------------------------------------------------------------------------
(DEFUN C:412()(princ "\n ** Program for text (4-12#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-12#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------------------------------------
(DEFUN C:416()(princ "\n ** Program for text (4-16#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-16#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------------------------------------
(DEFUN C:420()(princ "\n ** Program for text (4-20#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-20#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-----------------------------------------------------------------
(DEFUN C:425()(princ "\n ** Program for text (4-25#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-25#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------------------------------
(DEFUN C:428()(princ "\n ** Program for text (4-28#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-28#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

--------------------------------------------------------------------------------------
(DEFUN C:432()(princ "\n ** Program for text (4-32#)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "4-32#")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-----------------------------------------------------------------------------------
(DEFUN C:%T()(princ "\n ** Program for text (TYP.LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT TYPICAL SLAB LVL.")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-----------------------------------------------------------------------------------
(DEFUN C:%f()(princ "\n ** Program for text (FOOTING LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT FOOTING LVL. EL.(-1.500)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-----------------------------------------------------------------------------------


(DEFUN C:%P()(princ "\n ** Program for text (PLINTH LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT PLINTH BEAMS LVL. EL.(-0.150)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-----------------------------------------------------------------------------------
(DEFUN C:%1()(princ "\n ** Program for text (1ST LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT 1st SLAB LVL.EL.(+3.000) ")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------------------
(DEFUN C:%2()(princ "\n ** Program for text (2ND LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT 2nd SLAB LVL.EL.(+6.000) ")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
-------------------------------------------------------------
(DEFUN C:%3()(princ "\n ** Program for text (3RD LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT 3rd SLAB LVL. EL.(+9.000)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------------------------
(DEFUN C:%4()(princ "\n ** Program for text (4TH LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT 4th SLAB LVL. EL.(+12.000)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

-----------------------------------------------------------------------
(DEFUN C:%5()(princ "\n ** Program for text (5TH LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT 5th SLAB LVL. EL.(+15.000)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
------------------------------------------------------------------------------
(DEFUN C:%6()(princ "\n ** Program for text (6TH LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT 6th SLAB LVL.EL.(+18.000) ")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------------------------------
(DEFUN C:%7()(princ "\n ** Program for text (6TH LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT 7th SLAB LVL.EL.(+21.000) ")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------------------------------

(DEFUN C:%T()(princ "\n ** Program for text (TERR. LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%ULAYOUT AT TERRACE SLAB LVL.EL.(+24.000) ")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
---------------------------------------------------------------------------------
(DEFUN C:s1()(princ "\n ** Program for text (SECTION MARK)  by mohd altaf.")
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
(DEFUN C:s2()(princ "\n ** Program for text (SECTION MARK)  by mohd altaf.")
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
(DEFUN C:s3()(princ "\n ** Program for text (SECTION MARK)  by mohd altaf.")
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


(DEFUN C:s4()(princ "\n ** Program for text (SECTION MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECTION D-D")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:s5()(princ "\n ** Program for text (SECTION MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECTION E-E")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:s6()(princ "\n ** Program for text (SECTION MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECTION F-F")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:s7()(princ "\n ** Program for text (SECTION MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECTION G-G")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:s8()(princ "\n ** Program for text (SECTION MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECTION H-H")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:s9()(princ "\n ** Program for text (SECTION MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%USECTION J-J")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
----------------------------------------------------------------------------------------
(DEFUN C:s100()(princ "\n ** Program for text (SCALE 1:100)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "(SCALE 1:100)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:S50()(princ "\n ** Program for text (SCALE 1:50)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "(SCALE 1:50)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

(DEFUN C:S75()(princ "\n ** Program for text (SCALE 1:75)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "(SCALE 1:75)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:S40()(princ "\n ** Program for text (SCALE 1:40)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "(SCALE 1:40)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:s30()(princ "\n ** Program for text (SCALE 1:30)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "(SCALE 1:30)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:s25()(princ "\n ** Program for text (SCALE 1:25)  by pmohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "(SCALE 1:25)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:S15()(princ "\n ** Program for text (SCALE 1:15)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "(SCALE 1:15)")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

----------------------------------------------------------------------------------------
(DEFUN C:D1()(princ "\n ** Program for text (DETAIL MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UDETAIL-`1'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )



(DEFUN C:D2()(princ "\n ** Program for text (DETAIL MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UDETAIL-`2'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:D3()(princ "\n ** Program for text (DETAIL MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UDETAIL-`3'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

(DEFUN C:D4()(princ "\n ** Program for text (DETAIL MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UDETAIL-`4'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:D5()(princ "\n ** Program for text (DETAIL MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UDETAIL-`5'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:D6()(princ "\n ** Program for text (DETAIL MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UDETAIL-`6'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )



(DEFUN C:D7()(princ "\n ** Program for text (DETAIL MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UDETAIL-`7'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )


(DEFUN C:D8()(princ "\n ** Program for text (DETAIL MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UDETAIL-`8'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )



(DEFUN C:D9()(princ "\n ** Program for text (DETAIL MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UDETAIL-`9'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
****************************************************************************************
(DEFUN C:V1()(princ "\n ** Program for text (VIEW MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UVIEW-`1'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

(DEFUN C:V2()(princ "\n ** Program for text (VIEW MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UVIEW-`2'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

(DEFUN C:V3()(princ "\n ** Program for text (VIEW MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UVIEW-`3'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

(DEFUN C:V4()(princ "\n ** Program for text (VIEW MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UVIEW-`4'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

(DEFUN C:V5()(princ "\n ** Program for text (VIEW MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UVIEW-`5'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

(DEFUN C:V6()(princ "\n ** Program for text (VIEW MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UVIEW-`6'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

(DEFUN C:V7()(princ "\n ** Program for text (VIEW MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UVIEW-`7'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

(DEFUN C:V8()(princ "\n ** Program for text (VIEW MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UVIEW-`8'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )

(DEFUN C:V9()(princ "\n ** Program for text (VIEW MARK)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "%%UVIEW-`9'")
       (COMMAND "CHANGE" ee1 "" "" "" "" "" "" typ)
       (setq ctr (+ 1 ctr))
      )
   )
 (PRINC "\n * Program Terminated, Invalid Selection ....")
 )
 (PRINC)
 )
*************************************************************************
(DEFUN C:B1()(princ "\n ** Program for text (LAY)  by mohd altaf.")
 (SETQ SS1 (SSGET))
 (IF SS1
   (PROGN
     (SETQ SSL (SSLENGTH SS1))
     (setq ctr 0)
      (WHILE (< CTR SSl)
       (setq ee1 (ssname SS1 ctr))
       (setq typ "B1(230x600)")
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
 (DEFUN C:LL()(princ "\n*** Program by mohd altaf...")
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
(defun c:Cv( / P0 P1 R D MR P2 DD)(Princ "\n!!!..PROGRAMED BY mohd altaf..!!!\n")
(setq lay (getvar "clayer"))
(setq os (getvar "osmode"))
(setvar "cmdecho" 0)
(setvar "osmode" 0)
(if (= (tblsearch "layer" "11") nil) (command "layer" "N" "11" "C" "56" "11" ""))
(COMMAND "LAYER" "S" "11" "")
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
(COMMAND "LAYER" "S" "11" "")
(COMMAND "'OSNAP" "END,INT,CEN,MID,QUA,PER,NODE,ins")
(princ))
	


	  
(defun c:CV2 (/ p0 p1 pe r clay)(Princ "\n!!!..PROGRAMED BY mohd altaf..!!!\n")
    (setq oldecho (getvar "cmdecho")
   clay    (getvar "clayer"))
    (setvar "cmdecho" 0) (setvar "orthomode" 0) (setvar "osmode" 0)
    (command "layer" "s" "11" "")
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
***********************************************************************

(Defun C:CV1 (/
     ARC_DIST   ;;radius of included arc
     INC_ANGLE  ;;included angle in degrees
     LAST_PT    ;;the last point just entered/shown
     START_PT   ;;where the cloud began
     NEXT_PT    ;;where we are going next
     TMP        ;;tempory holder for radius of bulge
     )

   (init_bonus_error
      (List
         (List "cmdecho" 0
               "blipmode" 0
               "osmode" 0
         )
         T     ;flag. True means use undo for error clean up.  
      ) ;list 
   ) ;init_bonus_error



   ;;--------real program starts here!

   (Setq INC_ANGLE 110)
   
   (if (and
         (/= ""  (getcfg "AppData/AC_Bonus/Revcld_Bulge"))
         (/= nil (getcfg "AppData/AC_Bonus/Revcld_Bulge"))
       )
     (setq ARC_DIST (atof (getcfg "AppData/AC_Bonus/Revcld_Bulge")))
     (if (= (getvar "DIMSCALE") 0)
       (setq ARC_DIST 0.375)
       (setq ARC_DIST (* 0.375 (getvar "DIMSCALE")))
     )
   );end if

   (prompt (strcat "\nArc length set at " (rtos ARC_DIST 2 3)))

   (initget "Arc")
   (setq LAST_PT (GetPoint "\nArc length/<Pick cloud starting point>: "))

   (if (= LAST_PT "Arc")
     (progn
       (initget 6)
       (setq TMP (getdist (strcat "\nArc length <" (rtos ARC_DIST 2 3) ">: ")))
       (if TMP 
         (Progn
           (setq ARC_DIST TMP)
                ;R14 method of saving variable values
           (setcfg "AppData/AC_Bonus/Revcld_Bulge" (rtos ARC_DIST)) 
         )
       )
       (setq LAST_PT (getpoint "\nPick cloud start point: "))
     ) ;;end STR "RADIUS" test
   )

   (if LAST_PT (progn  ;;start up the cloud generator...
     (setq START_PT LAST_PT
           SAVED_EN (entlast))
     (Prompt "\nGuide crosshairs along cloud path...")
     (Command
        "_.pline"     ;draw cloud as a polyline on current layer
        LAST_PT
        "_a"         ;specify arc option
        "_a"         ;specify angle option
        INC_ANGLE    ;included angle
     )
   )) ;end IF LAST_PT

   (While LAST_PT  ;;as long as we have a last point value,

      (Setq NEXT_PT (GrRead 1)     ;;real time read
            READTYP (car NEXT_PT)
      )
      (if (or (=  5 READTYP) (= READTYP 3)) ;;read a position or a pick?
         (progn
           (setq NEXT_PT (cadr NEXT_PT))
           (If (or (> (Distance LAST_PT NEXT_PT) ARC_DIST) (= READTYP 3))
             (Progn
               (Command NEXT_PT "_a" INC_ANGLE)
               (Setq LAST_PT NEXT_PT)
             )
           )
           (If (>
               (Distance LAST_PT NEXT_PT)
               (Distance START_PT NEXT_PT)
               )
             (Progn
               (Command START_PT "_cl")
               (Setq LAST_PT Nil)
               (prompt "\nCloud finished.")
             )
           )
         )
         (prompt "\nMove the pointer to draw the cloud")
      );End if
   );End while
   (restore_old_error)
   (Princ)
) ;end cloud.lsp

(Defun c:RCHELP (/)
(prompt "   The Revision Cloud program draws a user specified bulge pline\n")
(prompt "   along the path of the crosshairs. To close the cloud, \n")
(prompt "   simply return to the starting point\n")
(prompt "   The cloud arc length can be specified in the beginning, with keyboard input\n")
(prompt "   by specifying A for Arc, entering a length, or picking two pts.\n")
(textscr)
(princ)
)
---------------------------------------------------------------------
;TO GET LENGHT OF LINE
(defun c:TT ()
(princ "\n*** Program by mohd altaf")
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
(Defun c:99()(princ "\n *** Program by mohd altaf*** ")
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
********************************************************************
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
(defun c:TJ()(princ "\nChange Justification program by mohd altaf...")
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
          (command "osnap" "end,mid,node,per,center,qua,int,ins")

       )
       (prompt "\007\nRequires two lines - try again ")
 ))
 
*********************************************************************
SPECIAL LAYER.LISP
*********************************************************************
;TO MAKE LAYERS IN ONE COMMAND

(DEFUN C:rclay ()
  (LAY-LIST)
  (SETQ REP (LENGTH ARCH_LIST))
  (SETQ COUNT 0)

  (WHILE
    (SETQ LAY-NAME (NTH COUNT ARCH_LIST))
    (IF	(/= LAY-NAME NIL)
      (SETQ LAY-FIND (TBLSEARCH "LAYER" LAY-NAME))
    )
    (IF	(NOT LAY-FIND )
      (PROGN
	(SETQ LAY-LW (NTH (+ COUNT 1) ARCH_LIST))
	(SETQ LAY-CO (NTH (+ COUNT 2) ARCH_LIST))
	(COMMAND "LAYER" "N" LAY-NAME "LW" LAY-LW LAY-NAME "C" LAY-CO LAY-NAME "")

      )
    )

    (SETQ COUNT (+ COUNT 3))
   (SETQ LAY-NAME (NTH COUNT ARCH_LIST))
 (SETQ LAY-FIND NIL
    )
  )

  (PRINC)
  (SETQ COUNT NIL)
)

(PRINC)

(DEFUN LAY-LIST	()
  (SETQ	ARCH_LIST (LIST
		     
                  	"1"  0 1
			"2"  0 2
			"3"  0 3
			"4"  0 4
			"5"  0 7
			"6"  0 6
			"7"  0 8
			"8"  0 21
			"9"  0 9
			"10" 0 11
			"11" 0 56
			"12" 0 137
			"13" 0 5			
			"14" 0 223
			"15" 0 250	
			"16" 0 252
                 	"17" 0 202
                  	"18" 0 32
                 	"19" 0 120
                 	"20" 0 181
                 	"21" 0 84
		      	"22" 0 40
		     	"23" 0 25
			"30" 0 30
			"31" 0 31
			"134" 0 134
			"183" 0 183
			"255" 0 255

		   )
  )
)


(PRINC) (princ "\n Type LAYERS .") (pRINC)

******************************************************************************************


(defun c:K1 () (command "LINETYPE" "LOAD" "CENER,HIDDEN,DOT,DOT2,phantom" "" "")(princ))
(defun c:q1 () (command "layer" "M" "1" "COL" "RED" "" "")(princ))
(defun c:q2 () (command "layer" "M" "2" "COL" "YELLOW" "" "")(princ))
(defun c:q3 () (command "layer" "M" "3" "COL" "GREEN" "" "")(princ))
(defun c:q4 () (command "layer" "M" "4" "COL" "CYAN" "" "")(princ))
(defun c:q5 () (command "layer" "M" "5" "COL" "WHITE" "" "")(princ))
(defun c:q6 () (command "layer" "M" "6" "COL" "MAGENTA" "" "")(princ))
(defun c:q7 () (command "layer" "M" "7" "COL" "8" "" "")(princ))
(defun c:q8 () (command "layer" "M" "8" "COL" "21" "" "")(princ))
(defun c:q9 () (command "layer" "M" "9" "COL" "9" "" "")(princ))
(defun c:q0 () (command "layer" "M" "10" "COL" "11" "" "")(princ))
(defun c:q11 () (command "layer" "M" "11" "COL" "56" "" "")(princ))
(defun c:q12 () (command "layer" "M" "12" "COL" "137" "" "")(princ))
(defun c:q13 () (command "layer" "M" "13" "COL" "5" "" "")(princ))
(defun c:q14 () (command "layer" "M" "14" "COL" "223" "" "")(princ))
(defun c:q15 () (command "layer" "M" "15" "COL" "250" "" "")(princ))
(defun c:q16 () (command "layer" "M" "16" "COL" "252" "" "")(princ))
(defun c:q17 () (command "layer" "M" "17" "COL" "202" "" "")(princ))
(defun c:q18 () (command "layer" "M" "18" "COL" "32" "" "")(princ))
(defun c:q19 () (command "layer" "M" "19" "COL" "120" "" "")(princ))
(defun c:q20 () (command "layer" "M" "20" "COL" "181" "" "")(princ))
(defun c:q21 () (command "layer" "M" "21" "COL" "84" "" "")(princ))
(defun c:q22 () (command "layer" "M" "22" "COL" "40" "" "")(princ))
(defun c:q23 () (command "layer" "M" "23" "COL" "25" "" "")(princ))
(defun c:q30 () (command "layer" "M" "30" "COL" "30" "" "")(princ))
(defun c:q31 () (command "layer" "M" "31" "COL" "31" "" "")(princ))
(defun c:q134 () (command "layer" "M" "134" "COL" "134" "" "")(princ))
(defun c:q183 () (command "layer" "M" "183" "COL" "183" "" "")(princ))
(defun c:q255 () (command "layer" "M" "255" "COL" "255" "" "")(princ))


(defun c:HC()(ssget) 
(prompt "\nSELECT OBJECT TO CHANGED ..Program by mohd altaf.")
(COMMAND "CHPROP" "P" "" "layer" "12" "LType" "CENTER" ""))

(defun c:DD()(ssget) 
(prompt "\nSELECT OBJECT TO CHANGED ..Program by mohd altaf.")
(COMMAND "CHPROP" "P" "" "layer" "10" "col" "bylayer" "LType" "DOT" "")) 

(defun c:Hd()(ssget) 
(prompt "\nSELECT OBJECT TO CHANGED ..Program by mohd altaf.")
(COMMAND "CHPROP" "P" "" "layer" "" "LType" "hidden" ""))
     
 (defun c:CL()(princ "\n ** Change Current Layer Program by mohd altaf.")
 (setvar "cmdecho" 0)
 (setq lo (entsel "\n * Pick The Object For Current Layer: "))
 (setq slo (car lo))
 (setq nlo (entget slo))
 (setq llo (cdr (assoc 8 nlo)))
 (command "'layer" "s" llo "")
 )
 
(DEFUN C:of (/ VA VB)
  (SETQ VA (CDR (ASSOC 8 (ENTGET (CAR
    (ENTSEL " select an object on the layer to be turned off ")
  )))))
  (IF (/= VA (GETVAR "CLAYER"))
    (PROGN (COMMAND "LAYER" "OFF" VA "")
      (PROMPT "LAYER TURNED OFF: ")(EVAL VA))
    (PROGN (SETQ VB (STRCASE (GETSTRING
      (STRCAT "\nREALLY WANT LAYER " VA
        " (THE CURRENT LAYER) OFF? <N>"
      )
     )))
     (IF (= (ASCII VB) 89)
       (COMMAND "LAYER" "OFF" VA "Y" "")
     )
)))

(DEFUN C:LO NIL (COMMAND "LAYER" "ON" "*" "" ))


    
 (DEFUN C:COL()(princ "\n ** Color Change Program by mohd altaf")
 (SETVAR "CMDECHO" 1)
 (setq cl (ssget))
 (command "chprop" cl "" "c" pause "")
 )
 (defun pro ()
 (setq an nil)
 (setq dt nil)
 (setq an (getangle "\n * Enter Angle OR Second Point For Angle: " fp))
 )

(defun c:00()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `11' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "11" "")(princ))

 (defun c:0()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `10' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "10" "")(princ))

 (defun c:1()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `1' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "1" "")(princ))

 (defun c:2()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `2' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "2" "")(princ)) 

 (defun c:3()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `3' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "3" "")(princ))

 (defun c:4()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `4' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "4" "")(princ))

 (defun c:5()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `5' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "5" "")(princ))

 (defun c:6()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `6' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "6" "")(princ))

 (defun c:7()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `7' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "7" "")(princ))

 (defun c:8()  (setvar "cmdecho" 0)
        (princ "\n***  Change objects into `8' layer...")
        (setq select2 (ssget))
        (command "change" select2 "" "P" "LA" "8" "")(princ))

 (defun c:9()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into '9' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "9" "")(princ))

 (defun c:12()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 12 layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "12" "")(princ))

 (defun c:13()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into '13' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "13" "")(princ))

 (defun c:14()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 14TH layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "14" "")(princ))

 (defun c:15()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into `15' layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "15" "")(princ))

 (defun c:16()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 16 layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "16" "")(princ))

 (defun c:17()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 17TH layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "17" "")(princ))

 (defun c:18()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 18TH layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "18" "")(princ))

 (defun c:19()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 19TH layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "19" "")(princ))

 (defun c:20()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 20TH layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "20" "")(princ))

 (defun c:21()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 21TH layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "21" "")(princ))

 (defun c:2`()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 22ND layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "22" "")(princ))

 (defun c:23()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 23RD layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "23" "")(princ))

 (defun c:30()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 30RD layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "30" "")(princ))

 (defun c:31()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 31RD layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "31" "")(princ))

 (defun c:134()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 134RD layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "134" "")(princ))

 (defun c:183()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 183RD layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "183" "")(princ))

 (defun c:255()(setvar "cmdecho" 0)
        (princ "\n***  Change objects into 255RD layer...")
        (setq select1 (ssget))
        (command "change" select1 "" "P" "LA" "255" "")(princ))

  (defun c:cd() (setvar "cmdecho" 0)
        (princ "\n***  Change objects into 3 layer...")
        (setq select3 (ssget))
        (command "change" select3 "" "P" "LA" "3" "")(princ))
 
  (defun c:hid() (setvar "cmdecho" 0)
        (princ "\n***  Change objects into HIDDEN LINE layer...")
        (setq select4 (ssget))
        (command "change" select4 "" "P" "LT" "HIDDEN" "")(princ))
 
 (defun c:HH() (setvar "cmdecho" 0)
        (princ "\n***  Change objects into 3 layer...")
        (setq select3 (ssget))
        (command "change" select3 "" "P" "LA" "3" "")(princ))
 
 
 (defun c:on()(setq layx (getstring "\nEnter Layer to turn ON <*>..."))
       (if (= layx "")
       (progn (setq layx "*")))
       (command "layer" "on" layx ""))
 
 (defun c:off()(setq lay2 (getstring "\nEnter Layer to turn OFF <*>..."))
       (if (= lay2 "")
       (progn (setq lay2 "*")))
       (command "layer" "off" lay2 "" ""))




      
**********************************************************************************
                  DIMENSION UPDATED LISP. 
**********************************************************************************      
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
 
 
 
 (defun c:ms()(princ "\n*** Program by mohd altaf")
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
*****************************************************************
(defun c:D100 ()
 (setq ex (tblsearch "dimstyle" "D100"))
  
  (if (= ex nil)
    (progn  
      (setvar "dimexe" 1.5)
      (setvar "dimexo" 1.5)
      (setvar "dimtxt" 2.5)
      (setvar "dimtad" 1)
      (setvar "dimasz" 2.0)
      (setvar "dimgap" 0.75) 
      (setvar "dimscale" 100)
      (setvar "dimclrd" 1)
      (setvar "dimclre" 1)
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
      (setvar "dimexe" 1.5)
      (setvar "dimexo" 1.5)
      (setvar "dimtxt" 2.5)
      (setvar "dimtad" 1)
      (setvar "dimasz" 2.0)
      (setvar "dimgap" 0.75) 
      (setvar "dimscale" 75)
      (setvar "dimclrd" 1)
      (setvar "dimclre" 1)
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
      (setvar "dimclrd" 1)
      (setvar "dimclre" 1)
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
      (setvar "dimclrd" 1)
      (setvar "dimclre" 1)
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
      (setvar "dimclrd" 1)
      (setvar "dimclre" 1)
      (setvar "dimclrt" 9)
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
***************************************************************************

(defun c:D20 ()
 (setq ex (tblsearch "dimstyle" "D20"))
  
  (if (= ex nil)
    (progn  
      (setvar "dimexe" 1.5)
      (setvar "dimexo" 1.5)
      (setvar "dimtxt" 2.5)
      (setvar "dimtad" 1)
      (setvar "dimasz" 2.0)
      (setvar "dimgap" 0.75) 
      (setvar "dimscale" 30)
      (setvar "dimclrd" 1)
      (setvar "dimclre" 1)
      (setvar "dimclrt" 9)
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
      (command "dimstyle" "sa" "D20$7")
      (command "dimstyle" "sa" "D20")
    )

  nil

  )
  
)

*************************************************************************
(defun c:D15 ()
 (setq ex (tblsearch "dimstyle" "D20"))
  
  (if (= ex nil)
    (progn  
      (setvar "dimexe" 1.5)
      (setvar "dimexo" 1.5)
      (setvar "dimtxt" 2.5)
      (setvar "dimtad" 1)
      (setvar "dimasz" 2.0)
      (setvar "dimgap" 0.75) 
      (setvar "dimscale" 30)
      (setvar "dimclrd" 1)
      (setvar "dimclre" 1)
      (setvar "dimclrt" 9)
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
      (command "dimstyle" "sa" "D15$7")
      (command "dimstyle" "sa" "D15")
    )

  nil

  )
  
)

*************************************************************************
(defun c:D10 ()
 (setq ex (tblsearch "dimstyle" "D20"))
  
  (if (= ex nil)
    (progn  
      (setvar "dimexe" 1.5)
      (setvar "dimexo" 1.5)
      (setvar "dimtxt" 2.5)
      (setvar "dimtad" 1)
      (setvar "dimasz" 2.0)
      (setvar "dimgap" 0.75) 
      (setvar "dimscale" 30)
      (setvar "dimclrd" 1)
      (setvar "dimclre" 1)
      (setvar "dimclrt" 9)
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
      (command "dimstyle" "sa" "D10$7")
      (command "dimstyle" "sa" "D10")
    )

  nil

  )
  
)

*************************************************************************

WEIGHT CALCULATION LISP
*************************************************************************
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

*****************************************************************************

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

*****************************************************************************

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

*****************************************************************************

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

*****************************************************************************
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

*****************************************************************************
 ;TO CALCULATE BASE PLATE,INSERTPLATE,CAP PLATE,ETC
 (defun c:FW ()
 (princ " ** Weight Calculation Program By mohd altaf")
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

*****************************************************************************

;TO CALCULATE BASE PLATE,INSERTPLATE,CAP PLATE,ETC
(defun c:Pw ()
(princ " ** Weight Calculation Program By mohd altaf")
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
 (princ " ** Weight Calculation Program By mohd altaf.")
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
 (DEFUN C:CMm ()(princ "\n*** Copy & Mirror Program by mohd altaf.")
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

(DEFUN C:CR ()(princ "\n*** Copy & Rotate Program by mohd altaf")
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
 
(DEFUN C:CS()(princ "\n*** Copy & Scale Program by mohd altaf.")
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

(DEFUN C:CE()(princ "\n*** Copy & EDIT Program by mohd altaf.")
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

 
 (DEFUN C:MR ()(princ "\n*** Move & Rotate  Program by mohd altaf.")
 (PRINC"\n * Select Entities ...")
 (SETQ SS (SSGET))
 (SETQ BP (GETPOINT "\n * Enter Base Point For Move & Rotate:"))
 (PRINC "\n * Enter Second Point For Move :")
 (COMMAND "MOVE" SS "" BP pause)
 (SETQ RP (GETVAR "LASTPOINT"))
 (princ)
 (COMMAND "Rotate" SS "" Rp)
 )
 
 (DEFUN C:MBR ()(princ "\n*** Multiline Break Program by mohd altaf.")
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
    (prompt "\nMT Complete.") (prin1))
 
 -----------------------------------------------------------
  (defun c:cm()(setq se1 (ssget))(command "copy" se1 "" "m")) 
  ---------------------------------------------------------------PERVEZ.")
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

*****************************************************************************
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
   (setq   propmb  ' ( (100.0    75.0    7.2   4.0   9.0   4.5)
                       (125.0    75.0    7.6   4.4   9.0   4.5)
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
                       (600.0   210.0   20.8  12.0  20.0  10.0) ))
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
*************************************************************************
(defun c:rot()(ssget) 
(prompt "\nSELECT OBJECT TO CHANGED ..Program by mohd altaf.")
(COMMAND "change" "" "" "romant" "" "" ""))

**********************************************************************!!!!!
;RAIL.LSP --> TO DRAW A HAND RAIL
(defun c:rail()
	(setvar "cmdecho" 0)
	(setvar "blipmode" 0)
	(setq s1(getpoint "\nSTART POINT : ")
	      s2(getpoint "\nTO POINT   : ")
	      xx(getreal "\nDIAMETER OF CIRCLE : ")
	)
	(command "layer" "s" "8" "c" "21" "" "")
	(while (/= s2 nil)
	    (setq s3(angle s1 s2)
		  s4(distance s1 s2)
		  s5(fix (/ s4 1100.0))
		  xy(fix (* xx 1.25))
	)
	    (command "line" s1 s2 "")
	    (repeat s5
		  (setq g1(polar s1 s3 1100.0)
			g2(polar g1 s3 xy)
			g3(polar g1 (+ s3 pi) xy)
			s1 g1
		  )
		  (command "circle" g1 "d" xx "break" g2 g3)
	    )
	(setq s1 s2)
	(setq s2(getpoint "\nTO POINT : " s1))
	)
	(setvar "cmdecho" 0)
)
*********************************************************!!!!!!!!!!!!!!!

(defun c:AS3()
(setq s2 (getdist "angle size:- "))
(setq cr1(getdist "bot. clearanc:-"))
(setq cr2(getdist "top. clearanc:-"))
(Progn
            (if (= s2  50 )  (Progn (setq d 28 )(SETQ B 22 )  (SETQ C4 6 )) (setq zd 0))
		(if (= s2  65 )  (Progn (setq d 35) (setq B4 30 ) (SETQ C4 6 ))(setq zd 0))
		(if (= s2  75 )  (Progn (setq d 40) (setq B4 35 ) (SETQ C4 6 ))(setq zd 0))
		(if (= s2  90 )  (Progn (setq d 50) (setq B4 40 ) (SETQ C4 6 ))(setq zd 0))
		(if (= s2  100 ) (Progn (setq d 55) (setq B4 45 ) (SETQ C4 8 ))(setq zd 0))
		(if (= s2  110 ) (Progn (setq d 60) (setq B4 50 ) (SETQ C4 8 ))(setq zd 0))		  
            )


(setq a (getpoint "Pick Bottom of Line "))
(setq b (getpoint "Pick top of Line "))
(setq f (* (/ 180.0 pi ) (angle a b )))
(setq x  (* d (sin  (* f (/ pi 180.0 )))))
(setq y  (* d (cos  (* f (/ pi 180.0 )))))

(setq z (list (+ (car a) x) (- (cadr a) y)))
(setq z1 (list (+ (car b) x) (- (cadr b) y)))
;(command "line" a z "")
;(command "line" b z1 "")

(setq c (- 90.0 (* (/ 180.0 pi) f )))
(setq x1  (* s2 (sin  (* f (/ pi 180.0 )))))
(setq y1  (* s2 (cos  (* f (/ pi 180.0 )))))

 
(setq d1 (list (- (car a )10) (+ (cadr a) cr1)))
 (setq d2 (list (+ (car a )10) (+ (cadr a) cr1)))
 (setq p1 (inters z z1 d1 d2 nil ))


(setq d3 (list (- (car b )10) (- (cadr b) (+ cr2  y1 ))))
 (setq d4 (list (+ (car b )10) (- (cadr b) (+ cr2 y1 ))))
(setq p2 (inters z z1 d3 d4 nil ))



(setq p3 (list (- (car p1) x1) (+ (cadr p1) y1)))
(setq p4 (list (- (car p2) x1) (+ (cadr p2) y1)))

 (command "line" p1 p3 p4 p2 p1 "")

        (setq cc2 (list  (+ (car p1) (car p2) ) (+ (cadr p1) (cadr p2))))
        (SETQ Aa1 (list  (/ (car cc2 ) 2.0 ) (/ (cadr cc2 ) 2.0 )))
              
           (command "offset" c4 P1 a "")
)
*************************************************************!!!!!!!!!!!!!!!!!!!!!!!!!
(defun c:AS1 ()  
     (SETQ z (GETVAR "OSMODE"))
     (command "osmode" "0")
          (SETQ S2(GETDIST "ANGLE SIZE:-"))
          (setq a1 (entsel "PICK THE LINE"))
(Progn
            (if (= s2  50 )  (Progn (setq A 14.5 )(SETQ B 35.5 ) (SETQ C 6 )) (setq zd 0))
            (if (= s2  60 )  (Progn (setq A 16.9) (setq B 43.1 ) (SETQ C 6 ))(setq zd 0))
            (if (= s2  65 )  (Progn (setq A 18.1) (setq B 46.9 ) (SETQ C 6 ))(setq zd 0))
		(if (= s2  75 )  (Progn (setq A 20.6) (setq B 54.4) (SETQ C 6 ))(setq zd 0))
		(if (= s2  90 )  (Progn (setq A 24.2) (setq B 65.8 ) (SETQ C 6 ))(setq zd 0))
		(if (= s2  100 ) (Progn (setq A 26.7) (setq B 73.3 ) (SETQ C 6 ))(setq zd 0))
		(if (= s2  110 ) (Progn (setq A 30) (setq B 80 ) (SETQ C 8 ))(setq zd 0))
            (if (= s2  130 ) (Progn (setq A 35) (setq B 95 ) (SETQ C 8 ))(setq zd 0))
		(if (= s2  150 ) (Progn (setq A 40.8) (setq B 109.2 ) (SETQ C 10 ))(setq zd 0))
            (if (= s2  200 ) (Progn (setq A 53.9) (setq B 146.1 ) (SETQ C 12 ))(setq zd 0))
            )

                  (setq enk (entget (car a1 )))       
                  (setq v (cadr a1))
			(setq x (- (car v) 11.0))
			(setq y (- (cadr v) 11.0))
                        
                  (setq x1 (+ (car v) 11.0 ))
			(setq y1 (+ (cadr v) 11.0))

                  (setq p1 (list x y))
                  (setq p2 (list x1 y1))  

                      (SETQ D (- A C ))
                        (command "offset" A a1 P1 "")
         			(command "offset" B a1 P2 "")
      			(command "offset" D a1 P1 "")     
     					(SETVAR "OSMODE" z)
   )
(defun c:AS2 ()  
     (SETQ z (GETVAR "OSMODE"))
     (command "osmode" "512")
          (SETQ S2(GETDIST "ANGLE SIZE:-"))
          (setq a1 (entsel "PICK THE LINE"))
(Progn
            (if (= s2  50 )  (Progn (setq A 14.5 )(SETQ B 35.5 ) (SETQ C 6 )) (setq zd 0))
            (if (= s2  60 )  (Progn (setq A 16.9) (setq B 43.1 ) (SETQ C 6 ))(setq zd 0))
            (if (= s2  65 )  (Progn (setq A 18.1) (setq B 46.9 ) (SETQ C 6 ))(setq zd 0))
		(if (= s2  75 )  (Progn (setq A 20.6) (setq B 54.4) (SETQ C 6 ))(setq zd 0))
		(if (= s2  90 )  (Progn (setq A 24.2) (setq B 65.8 ) (SETQ C 6 ))(setq zd 0))
		(if (= s2  100 ) (Progn (setq A 26.7) (setq B 73.3 ) (SETQ C 6 ))(setq zd 0))
		(if (= s2  110 ) (Progn (setq A 30) (setq B 80 ) (SETQ C 8 ))(setq zd 0))
            (if (= s2  130 ) (Progn (setq A 35) (setq B 95 ) (SETQ C 8 ))(setq zd 0))
		(if (= s2  150 ) (Progn (setq A 40.8) (setq B 109.2 ) (SETQ C 10 ))(setq zd 0))
            (if (= s2  200 ) (Progn (setq A 53.9) (setq B 146.1 ) (SETQ C 12 ))(setq zd 0))	  
            )

                  (setq enk (entget (car a1 )))       
                  (setq v (cadr a1))
			(setq x (- (car v) 11.0))
			(setq y (- (cadr v) 11.0))
                        
                  (setq x1 (+ (car v) 11.0 ))
			(setq y1 (+ (cadr v) 11.0))

                  (setq p1 (list x y))
                  (setq p2 (list x1 y1))  

                      (SETQ D (- A C ))
                        (command "offset" A a1 P2 "")
         			(command "offset" B a1 P1 "")
      			(command "offset" D a1 P2 "")     
     					(SETVAR "OSMODE" z)
   )


*********************************************************************************************************************
(defun c:PI3 ()  
     (SETQ z (GETVAR "OSMODE"))
     (command "osmode" "512")
          (SETQ S2(GETDIST "PIPE SIZE TYPE 0.25 ,1.5 ...12 "))
          (setq a1 (entsel "PICK THE LINE"))
(Progn
            (if (= s2  0.25 )  (SETQ C 13.7 ) (setq zd 0))
		(if (= s2  0.5 )   (SETQ C 21.3 ) (setq zd 0))
		(if (= s2  0.75 )  (SETQ C 26.7 ) (setq zd 0))

		(if (= s2  1 )     (SETQ C 33.4 ) (setq zd 0))
		(if (= s2  1.25 )  (SETQ C 42.2 ) (setq zd 0))
		(if (= s2  1.5 )   (SETQ C 48.3 ) (setq zd 0))

		(if (= s2  2 )     (SETQ C 60.3 ) (setq zd 0))
		(if (= s2  2.5 )   (SETQ C 73 )   (setq zd 0))
		(if (= s2  3 )     (SETQ C 88.9 ) (setq zd 0))

            (if (= s2  4 )     (SETQ C 114.3 ) (setq zd 0))
		(if (= s2  5 )     (SETQ C 141.3 ) (setq zd 0))
		(if (= s2  6 )     (SETQ C 168.3 ) (setq zd 0))

            (if (= s2  8 )     (SETQ C 219.1 ) (setq zd 0))
		(if (= s2  10 )    (SETQ C 273 )   (setq zd 0))
		(if (= s2  12 )    (SETQ C 323.8 ) (setq zd 0))

  
		  
            )

                  (setq enk (entget (car a1 )))       
                  (setq v (cadr a1))
			(setq x (- (car v) 11.0))
			(setq y (- (cadr v) 11.0))
                        
                  (setq x1 (+ (car v) 11.0 ))
			(setq y1 (+ (cadr v) 11.0))

                  (setq p1 (list x y))
                  (setq p2 (list x1 y1))  

                      (SETQ D (/ C 2.0 ))
                        (command "offset" D a1 P2 "")
         			(command "offset" D a1 P1 "")      			
     					(SETVAR "OSMODE" z )
   )
****************************************************************************************************************
(defun c:PI ()(princ "\n ** OFFSETS PIPE'S DIA IN ELEVATION Program by mohd altaf.**")
  
	(SETQ z (GETVAR "OSMODE"))
    	 (command "osmode" "0")
     
          (SETQ S2(GETDIST "TYPE PIPE SIZE:-"))
          (setq a1 (entsel "PICK THE LINE"))

(Progn
                       (if (= s2  10 )  (Progn (setq A 5)      (setq B 5 )   (setq C 0 ))  (setq D 0 ))
		(if (= s2  15 )  (Progn (setq A 7.5)    (setq B 7.5 ) (setq C 0 ))  (setq D 0 ))
		(if (= s2  20 )  (Progn (setq A 10)      (setq B 10 )   (setq C 0 ))  (setq D 0 ))
		(if (= s2  25 )  (Progn (setq A 12.5)    (setq B 12.5 ) (setq C 0 ))  (setq D 0 ))
		(if (= s2  30 )  (Progn (setq A 15)     (setq B 15)   (setq C 0))  (setq D 0))
		(if (= s2  35 )  (Progn (setq A 17.5)   (setq B 17.5) (setq C 0))  (setq D 0))
		(if (= s2  40 )  (Progn (setq A 20)     (setq B 20)   (setq C 0))  (setq D 0))
		(if (= s2  45 )  (Progn (setq A 22.5)     (setq B 22.5)   (setq C 0))  (setq D 0))
		(if (= s2  50 )  (Progn (setq A 25)     (setq B 25)   (setq C 0))  (setq D 0))
		(if (= s2  55 )  (Progn (setq A 27.5)     (setq B 27.5)   (setq C 0))  (setq D 0))
		(if (= s2  60 )  (Progn (setq A 30)     (setq B 30)   (setq C 0))  (setq D 0))
		(if (= s2  65 )  (Progn (setq A 32.5)     (setq B 32.5)   (setq C 0))  (setq D 0))
		(if (= s2  70 )  (Progn (setq A 35)     (setq B 35)   (setq C 0))  (setq D 0))
		(if (= s2  75 )  (Progn (setq A 37.5)     (setq B 37.5)   (setq C 0))  (setq D 0))
                       (if (= s2  80 )  (Progn (setq A 40)     (setq B 40)   (setq C 0))  (setq D 0))
                       (if (= s2  85 )  (Progn (setq A 42.5)     (setq B 42.5)   (setq C 0))  (setq D 0))
                       (if (= s2  90 )  (Progn (setq A 45)     (setq B 45)   (setq C 0))  (setq D 0))
                       (if (= s2  95 )  (Progn (setq A 47.5)     (setq B 47.5)   (setq C 0))  (setq D 0))
                       (if (= s2  100 )  (Progn (setq A 50)     (setq B 50)   (setq C 0))  (setq D 0))
                       (if (= s2  120 )  (Progn (setq A 60)     (setq B 60)   (setq C 0))  (setq D 0)) 
                       (if (= s2  125 )  (Progn (setq A 62.5)     (setq B 62.5)   (setq C 0))  (setq D 0))
                       (if (= s2  130 )  (Progn (setq A 65)     (setq B 65)   (setq C 0))  (setq D 0))
                       (if (= s2  140 )  (Progn (setq A 70)     (setq B 70)   (setq C 0))  (setq D 0))
                       (if (= s2  150 )  (Progn (setq A 75)     (setq B 75)   (setq C 0))  (setq D 0))
                       (if (= s2  160 )  (Progn (setq A 80)     (setq B 80)   (setq C 0))  (setq D 0)) 
                       (if (= s2  170 )  (Progn (setq A 85)     (setq B 85)   (setq C 0))  (setq D 0))
                       (if (= s2  175 )  (Progn (setq A 87.5)     (setq B 87.5)   (setq C 0))  (setq D 0))
                       (if (= s2  200 )  (Progn (setq A 100)     (setq B 100)   (setq C 0))  (setq D 0))
                       (if (= s2  225 )  (Progn (setq A 112.5)     (setq B 112.5)   (setq C 0))  (setq D 0))
                       (if (= s2  250 )  (Progn (setq A 125)     (setq B 125)   (setq C 0))  (setq D 0)))

   (setq enk (entget (car a1 )))       
                  (setq v (cadr a1))
		      (setq x (+ (car v) 11.0))
		      (setq y (+ (cadr v) 11.0))
                        
                  (setq x1 (- (car v) 11.0 ))
		      (setq y1 (- (cadr v) 11.0))		

                  (setq p1 (list x y))
                  (setq p2 (list x1 y1)) 
 
                  (SETQ E (- A C ))
		  
		 

                        (command "offset" A a1 P1 "")
         		(command "offset" B a1 P2 "")
			(command "offset" E a1 P1 "")
      			(command "offset" E a1 P2 "")
 
					(SETVAR "OSMODE" z))

        (COMMAND "'OSNAP" "END,INT,CEN,MID,QUA,PER,NODE,ins" )
******************************************************************************************
(defun c:MB1 ()(princ "\n ** OFFSETS MB'S FLANGE ELEVATION Program by mohd altaf.")
  
	(SETQ z (GETVAR "OSMODE"))
    	 (command "osmode" "0")
     
          (SETQ S2(GETDIST "ISMB SIZE:-"))
          (setq a1 (entsel "PICK THE LINE"))

(Progn
            (if (= s2  100 )  (Progn (setq A 50)      (setq B 50 )   (setq C 7 ))  (setq D 7 ))
		(if (= s2  125 )  (Progn (setq A 62.5)    (setq B 62.5 ) (setq C 8 ))  (setq D 8 ))
		(if (= s2  150 )  (Progn (setq A 75)      (setq B 75 )   (setq C 8 ))  (setq D 8 ))
		(if (= s2  175 )  (Progn (setq A 87.5)    (setq B 87.5 ) (setq C 9 ))  (setq D 9 ))
		(if (= s2  200 )  (Progn (setq A 100)     (setq B 100)   (setq C 11))  (setq D 11))
		(if (= s2  225 )  (Progn (setq A 112.5)   (setq B 112.5) (setq C 12))  (setq D 12))
		(if (= s2  250 )  (Progn (setq A 125)     (setq B 125)   (setq C 12))  (setq D 12))
		(if (= s2  300 )  (Progn (setq A 150)     (setq B 150)   (setq C 12))  (setq D 12))
		(if (= s2  350 )  (Progn (setq A 175)     (setq B 175)   (setq C 14))  (setq D 14))
		(if (= s2  400 )  (Progn (setq A 200)     (setq B 200)   (setq C 16))  (setq D 16))
		(if (= s2  450 )  (Progn (setq A 225)     (setq B 225)   (setq C 17))  (setq D 17))
		(if (= s2  500 )  (Progn (setq A 250)     (setq B 250)   (setq C 17))  (setq D 17))
		(if (= s2  550 )  (Progn (setq A 275)     (setq B 275)   (setq C 19))  (setq D 19))
		(if (= s2  600 )  (Progn (setq A 300)     (setq B 300)   (setq C 21))  (setq D 21)))
          



                  (setq enk (entget (car a1 )))       
                  (setq v (cadr a1))
		      (setq x (+ (car v) 11.0))
		      (setq y (+ (cadr v) 11.0))
                        
                  (setq x1 (- (car v) 11.0 ))
		      (setq y1 (- (cadr v) 11.0))		

                  (setq p1 (list x y))
                  (setq p2 (list x1 y1)) 
 
                  (SETQ E (- A C ))
		  
		 

                        (command "offset" A a1 P1 "")
         		(command "offset" B a1 P2 "")
			(command "offset" E a1 P1 "")
      			(command "offset" E a1 P2 "")
 
					(SETVAR "OSMODE" z))

        (COMMAND "'OSNAP" "END,INT,CEN,MID,QUA,PER,NODE,ins" )

*****************************************************************************

(defun c:MB2 ()(princ "\n ** OFFSETS MB'S WEB ELEVATION Program by mohd altaf.")
  
	(SETQ z (GETVAR "OSMODE"))
    	 (command "osmode" "0")
     
          (SETQ S2(GETDIST "ISMB SIZE:-"))
          (setq a1 (entsel "PICK THE LINE"))

(Progn
            (if (= s2  100 )  (Progn (setq A 37.5)   (setq B 37.5 ) (setq C 35.5 )) (setq D 35.5 ))
		(if (= s2  125 )  (Progn (setq A 37.5)   (setq B 37.5 ) (setq C 35.3 )) (setq D 35.3 ))
		(if (= s2  150 )  (Progn (setq A 40)     (setq B 40 )   (setq C 37.5 )) (setq D 37.5 ))
		(if (= s2  175 )  (Progn (setq A 45)     (setq B 45 )   (setq C 42 ))   (setq D 42 ))
		(if (= s2  200 )  (Progn (setq A 50)     (setq B 50)    (setq C 47))    (setq D 47))
		(if (= s2  225 )  (Progn (setq A 55)     (setq B 55)    (setq C 51.5))  (setq D 51.5))
		(if (= s2  250 )  (Progn (setq A 62.5)   (setq B 62.5)  (setq C 59))    (setq D 59))
		(if (= s2  300 )  (Progn (setq A 70)     (setq B 70)    (setq C 66))    (setq D 66))
		(if (= s2  350 )  (Progn (setq A 70)     (setq B 70)    (setq C 66))    (setq D 66))
		(if (= s2  400 )  (Progn (setq A 70)     (setq B 70)    (setq C 65.5))  (setq D 65.5))
		(if (= s2  450 )  (Progn (setq A 75)     (setq B 75)    (setq C 70.3))  (setq D 70.3))
		(if (= s2  500 )  (Progn (setq A 90)     (setq B 90)    (setq C 85))    (setq D 85))
		(if (= s2  550 )  (Progn (setq A 95)     (setq B 95)    (setq C 89.5))  (setq D 89.5))
		(if (= s2  600 )  (Progn (setq A 105)    (setq B 105)   (setq C 99))    (setq D 99)))
          



                  (setq enk (entget (car a1 )))       
                  (setq v (cadr a1))
		  (setq x (+ (car v) 11.0))
		  (setq y (+ (cadr v) 11.0))
                        
                  (setq x1 (- (car v) 11.0 ))
		  (setq y1 (- (cadr v) 11.0))		

                  (setq p1 (list x y))
                  (setq p2 (list x1 y1)) 
 
                  (SETQ E (- A C ))
		  
		 

                        (command "offset" A a1 P1 "")
         		(command "offset" B a1 P2 "")
			(command "offset" E a1 P1 "")
      			(command "offset" E a1 P2 "")
 
					(SETVAR "OSMODE" z))

        (COMMAND "'OSNAP" "END,INT,CEN,MID,QUA,PER,NODE,ins" )


----------------------------------------------------------------------------------------------



(defun c:MC1 ()(princ "\n ** OFFSETS MB'S FLANGE ELEVATION Program by mohd altaf.")
  
	(SETQ z (GETVAR "OSMODE"))
    	 (command "osmode" "0")
     
          (SETQ S2(GETDIST "ISMB SIZE:-"))
          (setq a1 (entsel "PICK THE LINE"))

(Progn
            (if (= s2  100 )  (Progn (setq A 50)      (setq B 50 )   (setq C 5.5 ))  (setq D 5.5 ))
		(if (= s2  125 )  (Progn (setq A 62.5)    (setq B 62.5 ) (setq C 5.5 ))  (setq D 5.5 ))
		(if (= s2  150 )  (Progn (setq A 75)      (setq B 75 )   (setq C 5.5 ))  (setq D 5.5 ))
		(if (= s2  175 )  (Progn (setq A 87.5)    (setq B 87.5 ) (setq C 7.0 ))  (setq D 7.0 ))
		(if (= s2  200 )  (Progn (setq A 100)     (setq B 100)   (setq C 8))  (setq D 8))
		(if (= s2  225 )  (Progn (setq A 112.5)   (setq B 112.5) (setq C 9))  (setq D 9))
		(if (= s2  250 )  (Progn (setq A 125)     (setq B 125)   (setq C 10.5))  (setq D 10.5))
		(if (= s2  300 )  (Progn (setq A 150)     (setq B 150)   (setq C 9.5))  (setq D 9.5))
		(if (= s2  350 )  (Progn (setq A 175)     (setq B 175)   (setq C 9))  (setq D 9))
		(if (= s2  400 )  (Progn (setq A 200)     (setq B 200)   (setq C 11))  (setq D 11)))



                  (setq enk (entget (car a1 )))       
                  (setq v (cadr a1))
		      (setq x (+ (car v) 11.0))
		      (setq y (+ (cadr v) 11.0))
                        
                  (setq x1 (- (car v) 11.0 ))
		      (setq y1 (- (cadr v) 11.0))		

                  (setq p1 (list x y))
                  (setq p2 (list x1 y1)) 
 
                  (SETQ E (- A C ))
		  
		 

                  (command "offset" A a1 P1 "")
         		(command "offset" B a1 P2 "")
			(command "offset" E a1 P1 "")
      	      (command "offset" E a1 P2 "")
 
					(SETVAR "OSMODE" z))

        (COMMAND "'OSNAP" "END,INT,CEN,MID,QUA,PER,NODE,ins" )

--------------------------------------------------------------------------------------------

(defun c:MC2 ()(princ "\n ** OFFSETS MB'S WEB ELEVATION Program by mohd altaf.")
  
	(SETQ z (GETVAR "OSMODE"))
    	 (command "osmode" "0")
     
          (SETQ S2(GETDIST "ISMC SIZE:-"))
          (setq a1 (entsel "PICK THE LINE"))

(Progn
            (if (= s2  100 )  (Progn (setq A 28)   (setq B 22) (setq C 5)) (setq D 0))
		(if (= s2  125 )  (Progn (setq A 35)   (setq B 30) (setq C 5.3)) (setq D 0))
		(if (= s2  150 )  (Progn (setq A 40)     (setq B 35 )   (setq C 5.7)) (setq D 0))
		(if (= s2  175 )  (Progn (setq A 40)     (setq B 35 )   (setq C 6 ))   (setq D 0 ))
		(if (= s2  200 )  (Progn (setq A 40)     (setq B 35)    (setq C 6.2))    (setq D 0))
		(if (= s2  225 )  (Progn (setq A 45)     (setq B 35)    (setq C 6.5))  (setq D 0))
		(if (= s2  250 )  (Progn (setq A 45)     (setq B 35)    (setq C 7.2))    (setq D 0))
		(if (= s2  300 )  (Progn (setq A 50)     (setq B 40)    (setq C 7.8))    (setq D 0))
		(if (= s2  350 )  (Progn (setq A 60)     (setq B 40)    (setq C 8.3))    (setq D 0))
		(if (= s2  400 )  (Progn (setq A 60)     (setq B 40)    (setq C 8.8))  (setq D 0)))
          

                  (setq enk (entget (car a1 )))       
                  (setq v (cadr a1))
		  (setq x (+ (car v) 11.0))
		  (setq y (+ (cadr v) 11.0))
                        
                  (setq x1 (- (car v) 11.0 ))
		  (setq y1 (- (cadr v) 11.0))		

                  (setq p1 (list x y))
                  (setq p2 (list x1 y1)) 
 
                  (SETQ E (- A C ))
		  
		 

                        (command "offset" A a1 P1 "")
         		(command "offset" B a1 P2 "")
			(command "offset" E a1 P1 "")

      			(SETVAR "OSMODE" z))

        (COMMAND "'OSNAP" "END,INT,CEN,MID,QUA,PER,NODE,ins" )
**********************************************************************************************************************
(defun Env_save ()
(setq ce (getvar "cmdecho")
      cl (getvar "clayer")
      os (getvar "osmode")
);setq
);defun
;--------------------------------------------------------
(defun Env_res ()
(setvar "cmdecho" ce)
(setvar "clayer" cl)
(setvar "osmode" os)
);defun
;--------------------------------------------------------
(defun *errer* (msg)
(Princ "error: ")
(Princ msg)
(Env_res)
(terpri)
);defun
;----------------------------------------------------
(defun c:ee ()(princ "\n write northing & southing Programed by mohd altaf.")
(Env_save)
(setvar "cmdecho" 0)     
(setvar "osmode" 32)
(setq pt (getpoint "\n Pick Point")
      Et (rtos (car pt) 2)
      Nt (rtos (cadr pt) 2)
      Ep (list (+(car pt)3)(+(cadr pt)1))
      Np (list (+(car pt)3)(+(cadr pt)3))
      Ep (strcat (rtos (car Ep) 2 ) "," (rtos(cadr Ep) 2 ))  
      Np (strcat (rtos (car Np) 2 ) "," (rtos(cadr Np) 2 ))   
      Tx (strcat "E " Et)
      Ty (strcat "N " Nt)
      Ip (strcat Et "," Nt)
);setq
(setvar "osmode" 0)
(command "layer" "m" "Ne" "s" "Ne" "c" "2" "Ne" ""
         "Text" Ep "2.0" "0.0" Ty
         "Text" Np "2.0" "90" Tx
        
);command
;----------------------------------------------------
(princ)
(Env_res)
);End Ne
**************************************************************************************************************
;--------------------------------------------------------	
(defun wp_error ()
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "console break")))
        (princ (strcat "\nError: " msg "\n"))
    );if
      (if old_error (setq *error* old_error))
      (princ)
  )  
  (princ)
);wp_error    
;--------------------------------------------------------
(defun Env_save ()
(setq ce (getvar "cmdecho")
      cl (getvar "clayer")
      os (getvar "osmode")
);setq
);defun
;--------------------------------------------------------
(defun Env_res ()
(setvar "cmdecho" ce)
(setvar "clayer" cl)
(setvar "osmode" os)
);defun
;--------------------------------------------------------
(defun CB (num)
     (cdr(assoc num Lst))
);defun
;--------------------------------------------------------
(defun C:WD ()(princ "\n ...CHANGES PLOYLINE IN DESIRED WIDTH... Programed by mohd altaf.")
   (wp_error)
   (Env_save)
   (setvar "cmdecho" 0) 
   (setvar "osmode" 0)
   (setq SS (ssget))
     (if SS
       (setq ct 0
             sl (sslength ss)
             nw (getreal "\nEnter New Width ") 
       );setq
     );if
  (While (< ct sl)
    (setq ENT(ssname ss ct)     
          LST(entget ent)
    );setq
    (if (or (= (cb 0) "POLYLINE")(= (cb 0) "LWPOLYLINE"))               
               (command "Pedit" ent "w" nw "x")
    );if 
    (if (= (cb 0) "LINE")              
               (command "Pedit" ent "y" "w" nw "x")
    );if       
    (setq ct (+ 1 ct))           
  );while      
(terpri)  
;--------------------------------------------------------
(Env_res)
(princ (strcat "\nPolylines width successfully changed to " (rtos nw 2 3)))
(terpri)
);end wD
