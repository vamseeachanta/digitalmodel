
;--- Macro to convert a selection of text entities to
;--- lower or upper case. Non-text entities will be ignored.

(defun C:CASE (/ x c e ss txt cmde)
  (setq cmde (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  ; List keywords
  (initget "Upper Lower")
  (setq x (getkword "\nChange case to? Lower/<Upper>: "))
  ; Create a selection set of entities
  (setq ss (ssget))
  ; Initialise counter
  (setq c 0)
  (if ss (setq e (ssname ss c)))
  (while e
    (setq e (entget e))
    ; Ensure entity is text
    (if (= (cdr (assoc 0 e)) "TEXT")
        (progn
           (if (= x "Lower")
                 ; Option to convert to lower case
                 (setq txt (strcase (cdr (assoc 1 e)) t))
                 ; Option to convert to upper case
                 (setq txt (strcase (cdr (assoc 1 e))))
           )
           (setq e (subst (cons 1 txt) (assoc 1 e) e))
           (entmod e)
        )
    )
    (setq c (1+ c)) ; Increment counter.
    (setq e (ssname ss c))  ; Obtain next entity.
   )
   (setvar "CMDECHO" cmde)
   (princ)
);**********************************************************
;*                                                        *
;* NAME :        CHCULAY.LSP                              *
;*                                                        *
;* FUNCTION :    ALLOWS YOU TO TRANSFER ENTITIES TO       *
;*               THE CURRENT LAYER.                       *
;*                                                        *
;* WRITTEN  :    D. RENNIE, ACER SCOTLAND                 *
;*               JUNE 1993                                *
;*                                                        *
;**********************************************************
(defun c:HH (/ a1 a2 a3 n index b1 d1 b2)
 (graphscr)
 (prompt "Select entities to be changed") (terpri)
 (setq a1 (ssget))
 (setq a2 (Getvar "CLAYER"))
 (setq n (sslength a1))
 (setq index 0)
 (setq a3 (cons 8 a2))
 (repeat n
   (setq b1 (entget (ssname a1 index)))
   (setq d1 (assoc 8 b1))
   (setq b2 (subst a3 d1 b1))
   (entmod b2)
   (setq index (+ index 1))
 )
(Prompt "\n")
(Prompt "\nEntities are now transferred to layer ")(Princ a2)
(Prompt "\n")
(Princ)
)
;**********************************************************
;*                                                        *
;* NAME :        CHLAYER.LSP                              *
;*                                                        *
;* FUNCTION :    ALLOWS YOU TO TRANSFER ENTITIES TO A     *
;*               DIFFERENT LAYER BY PICKING AN ENTITY     *
;*               ON THE TARGET LAYER.                     *
;*                                                        *
;* WRITTEN  :    FROM "AUTOLISP IN PLAIN ENGLISH"         *
;*               THIRD EDITION                            *
;*                                                        *
;**********************************************************
(defun c:ch (/ a1 a2 n index b1 b2 d1 d2 b3)
 (graphscr)
 (prompt "Select entities to be changed") (terpri)
 (setq a1 (ssget))
 (prompt "Point to entity on target layer") (terpri)
 (setq a2 (entsel))
 (setq n (sslength a1))
 (setq index 0)
 (setq b2 (entget (car a2)))
 (setq d2 (assoc 8 b2))
 (setq d3 (cdr d2))
 (repeat n
   (setq b1 (entget (ssname a1 index)))
   (setq d1 (assoc 8 b1))
   (setq b3 (subst d2 d1 b1))
   (entmod b3)
   (setq index (+ index 1))
 )
(Prompt "\n")
(Prompt "\nEntities are now transferred to layer ")(Princ d3)
(Prompt "\n")
(Prompt "\n")
)
(defun c:MH (/ va)
 (setvar "cmdecho" 0)
 (setq va (cdr (assoc 8 (entget
                         (car (entsel "Pick entity to set layer: "))))))
 (command "layer" "set" va "")
)
**Truy bat:

(defun c:oq () (command "osnap" "qua"))
(defun c:on () (command "osnap" "non"))
(defun c:ono () (command "osnap" "nod"))
(defun c:oi () (command "osnap" "int"))
(defun c:oe () (command "osnap" "end"))
(defun c:oc () (command "osnap" "cen"))
(defun c:om () (command "osnap" "mid"))
(defun c:opp () (command "osnap" "per"))
(defun c:w () (command "blipmode" "off"))

**Zoom:
(defun c:zc () (command "zoom" "1.1x"))
(defun c:zx () (command "zoom" ".8x"))
(defun c:z7 () (command "zoom" ".7x"))
(defun c:z6 () (command "zoom" ".6x"))
(defun c:z5 () (command "zoom" ".5x"))
(defun c:p- () (command "pan" "0,0" "0,1500"))
(defun c:p= () (command "pan" "0,0" "0,-1500"))
(defun c:p[ () (command "pan" "0,0" "1500,0"))
(defun c:p] () (command "pan" "0,0" "-1500,0"))

**Xoa:

(defun c:er () (command "erase" "previous"))
(defun c:ew () (command "erase" "wp"))
(defun c:ea () (command "erase" "all"))
(defun c:e4 () (command "erase" "cp"))
(defun c:e3 () (command "erase" "f"))

**mline:

(defun c:ac () (command "arc" "c"))
(defun c:cpp () (command "circle" "3p"))
(defun c:cpo () (command "circle" "2p"))
(defun c:ml () (command "mline" "j" "z" "s"))
(defun c:p3 () (command "polygon" "3" pause "i"))
(defun c:p4 () (command "polygon" "4" pause "i"))
(defun c:p5 () (command "polygon" "5" pause "i"))
(defun c:p6 () (command "polygon" "6" pause "i"))
(defun c:p8 () (command "polygon" "8" pause "i"))
(defun c:p12 () (command "polygon" "12" pause "i")) 
(defun c:p16 () (command "polygon" "16" pause "i"))
(defun c:po3 () (command "polygon" "3" pause "c"))
(defun c:po4 () (command "polygon" "4" pause "c"))
(defun c:po5 () (command "polygon" "5" pause "c"))
(defun c:po6 () (command "polygon" "6" pause "c"))
(defun c:po8 () (command "polygon" "8" pause "c"))
(defun c:po12 () (command "polygon" "12" pause "c"))
(defun c:poo () (command "polygon"))

**dim-text:
(defun c:SPTEXT ()
    (setvar "cmdecho" 0)
    (setq flag 0)
    (setq gs (ssadd))
    (setq tot nil totent nil)
    (setq cont 0 ccc 0)
    (setq ent nil)
    (while (= nil ent)
        (setq ent (car (entsel "\nChon SPLINE: ")))
        (if (= nil ent)
            (princ "\Khong chon doi tuong.")
        )
    )
    (setq cod (cdr (assoc 70 (entget ent))))
    (setq tipo (cdr (assoc 0 (entget ent))))
 
    (if (or (/= cod 4) (/= tipo "POLYLINE"))
        (progn
           (setq flag 1) 
           (princ "\nPhai chon mot SPLINE.") 
        )
    )
    (if (= flag 0)
        (progn
            (setq cmx (getvar "CMDECHO"))
            (setq blx (getvar "BLIPMODE"))
            (setvar "CMDECHO" 0)
            (setvar "BLIPMODE" 0)
            (gestpl)
            (creapl)
            (instesto)
            (setvar "CMDECHO" cmx)
            (setvar "BLIPMODE" blx)
        )
    )
    (princ)
)
(defun c:te () (command "dtext" "j" "mc"))
(defun c:tel () (command "dtext" "j" "bl"))
(defun c:ter () (command "dtext" "j" "br"))
(defun c:tef () (command "dtext" "j" "f"))
(defun c:ded () (command "dim1" "tedit"))
(defun c:dtr () (command "dim1" "dia"))
(defun c:da () (command "dim1" "ali"))
(defun c:dg () (command "dim1" "ang"))
(defun c:dce () (command "dim1" "cen"))
(defun c:du () (command "dim1" "up"))
(defun c:dn () (command "dim1" "new"))
(defun c:dx () (command "dim1" "hor"))
(defun c:dy () (command "dim1" "ver"))
(defun c:dl () (command "dimlinear"))
(defun c:dc () (command "dimcontinue"))

**Modify:
-------------------------------------------------------------------------
(DEFUN C:LW(/ SSET NET SSL M)
   (PRINC "\nSelect lines :")
   (SETQ SSET (SSGET))
   (IF (/= NIL SSET) (PROGN
   (SETQ SSL (SSLENGTH SSET))
   (INITGET 4)
   (SETQ NET (GETREAL "New width : "))
   (IF (/= NIL NET)
    (WHILE (> SSL 0)
      (SETQ M (SSNAME SSET (SETQ SSL (- SSL 1)) ))
      (IF (= (CDR (ASSOC '0 (ENTGET M))) "LINE") 
          (COMMAND "PEDIT" M "Y" "W" NET "") 
      ) 
    ) 
   )))
   (PRINC)
) 

;; ================ Change width of polylines =========================

(DEFUN C:PW (/ SSET SSL M WID I)
   (PRINC "\nSelect polylines :")
   (SETQ SSET (SSGET))
   (IF (/= NIL SSET) (PROGN
   (SETQ SSL (SSLENGTH SSET))
   (INITGET 4 "")
   (SETQ WID (GETREAL "\nNew width : "))
   (IF (/= WID NIL) 
      (PROGN  		
   	(SETQ I 0)
   	(WHILE (< I SSL)
       	  (SETQ M (ENTGET (SSNAME SSET I)))
          (IF (= (CDR (ASSOC '0 M)) "POLYLINE") 
              (PROGN
               (SETQ M (SUBST (CONS 40 WID) (ASSOC 40 M) M))
               (SETQ M (SUBST (CONS 41 WID) (ASSOC 41 M) M))
	       (ENTMOD M)
              )
          ) 
          (SETQ I (+ I 1))
        )  
      )
   )))  
   (PRINC)
)

;; ================ Change radius of circles ==========================

(DEFUN C:CR (/ SSET SSL M RD I)
   (PRINC "\nSelect circles :")
   (SETQ SSET (SSGET))
   (IF (/= NIL SSET) (PROGN
   (SETQ SSL (SSLENGTH SSET))
   (INITGET 4 "")
   (SETQ RD (GETREAL "\nNew radius : "))
   (IF (/= RD NIL)
      (PROGN  		
   	(SETQ I 0)
   	(WHILE (< I SSL)
       	  (SETQ M (ENTGET (SSNAME SSET I)))
       	  (IF (= (CDR (ASSOC '0 M)) "CIRCLE") 
              (PROGN
	       (SETQ M (SUBST (CONS 40 RD) (ASSOC 40 M) M))
               (ENTMOD M)
              )
          )
          (SETQ I (+ I 1))
        )
      )
   )))
   (PRINC)
)
;;; ----------------------- TRIM NHIEU DOI TUONG -------------------------

(Defun C:Tri ()
  (Setq CVAR (Getvar "CMDECHO"))
  (Setvar "CMDECHO" 0)
  (Prompt "Select cutting edges")
  (Setq CUTEDG (Ssget))
  (Prompt "Select objects to trim")
  (Setq SS (SSget))
  (Setq LEN (SSlength SS))
  
  (Setq I -1)
  (Setq SIDE (getpoint"pick side to trim"))
  (Command "TRIM" CUTEDG "")
  (Repeat LEN
   (Setq I (1+ I))
   (Command (List(SSname SS I) SIDE))
  )
  (Command "")
  (Setvar "CMDECHO" CVAR)
  (Princ "\nTrim complete.")
  (Princ)
)

;;;----------------- EXTEND NHIEU DOI TUONG -----------------------

(Defun C:EET ()
  (Setq CVAR (Getvar "CMDECHO"))
  (Setvar "CMDECHO" 0)
  (Prompt "Chon doi tuong dich cua EXTEND :")
  (Setq CUTEDG (Ssget))
  (Prompt "Chon doi tuong de Extend")
  (Setq SS (SSget))
  (Setq LEN (SSlength SS))
  
  (Setq I -1)
  (Setq SIDE (getpoint"Chon phia de EXTEND"))
  (Command "EXTEND" CUTEDG "")
  (Repeat LEN
   (Setq I (1+ I))
   (Command (List(SSname SS I) SIDE))
  )
  (Command "")
  (Setvar "CMDECHO" CVAR)
  (Princ "Da Extend xong moi ban lam tiep")
  (Princ)
)
(defun c:oo () (command "oops"))
(defun c:d1 () (command "dimlfac" "1" "dim1" "up"))
(defun c:d2 () (command "dimlfac" "2" "dim1" "up"))
(defun c:df4 () (command "dimlfac" "4" "dim1" "up"))
(defun c:df5 () (command "dimlfac" "0.5" "dim1" "up"))
(defun c:df2 () (command "dimlfac" "0.25" "dim1" "up"))
(defun c:df1 () (command "dimlfac" "0.1" "dim1" "up"))
(defun c:mall () (command "move" "all"))
(defun c:mt () (command "mirrtext" "0"))
(defun c:mp () (command "move" "previous"))
(defun c:mk () (command "mirror" "previous"))
(defun c:vr () (command "view" "r"))
(defun c:vs () (command "view" "save"))
(defun c:cxx () (command "chamfer" "p"))
(defun c:cz () (command "chamfer" "d"))
(defun c:fd () (command "fillet" "p"))
(defun c:ff ()(setvar "filletrad" (getdist "\nHay nhap ban kinh fillet: ")) (command "fillet")(princ))
(defun c:f0 ()(setvar "filletrad" 0) (command "fillet")(princ))
(defun c:if () (command "insert" "~"))
(defun c:sva () (command "saveas" "~"))
(defun c:pp () (command "pedit" pause "y" "j"))
(defun c:pw () (command "pedit" pause "w"))
(defun c:pa () (command "pline" pause "a"))
(defun c:tr (/ ss1)
  (setq ss1 (ssget))(command "trim" ss1 "" "f"))
(defun c:ef (/ ss1)
  (setq ss1 (ssget))(command "extend" ss1 "" "f"))
(defun c:cm (/ ss1)
  (setq ss1 (ssget))(command "copy" ss1 "" "m"))
(defun c:aar () (ssget) (command "array" "p" "" "p"))
(defun c:my () (ssget) (command "mirror" "" "y"))
(defun c:aac () (ssget) (command "array" "r" "pause" "1" ""))

(defun c:shnc () (command "shell" "nc"))
-------------------------------------------------------------------
**ucs:
-----------------------------------------------------------------------
(defun c:up () (command "ucs" "previous"))
(defun c:u8 () (command "ucs" "zaxis"))
(defun c:u0 () (command "ucs" "world"))
(defun c:uuu () (command "ucs" "3point"))
(defun c:pl () (command "plan" "world"))
(defun c:45 () (command "snapang" "45"))
(defun c:00 () (command "snapang" "0"))
(defun c:60 () (command "snapang" "60"))
(defun c:30 () (command "snapang" "30"))
(defun c:15 () (command "snapang" "15"))
(defun c:75 () (command "snapang" "75"))

**Do day mat luoi:

(defun c:12 () (command "surftab1" "12" "surftab2" "12"))
(defun c:16 () (command "surftab1" "16" "surftab2" "16"))
(defun c:20 () (command "surftab1" "20" "surftab2" "20"))
(defun c:24 () (command "surftab1" "24" "surftab2" "24"))
(defun c:33 () (command "surftab1" "30" "surftab2" "30"))
(defun c:40 () (command "surftab1" "40" "surftab2" "40"))
(defun c:50 () (command "surftab1" "50" "surftab2" "50"))
(defun c:66 () (command "surftab1" "60" "surftab2" "60"))

**Thay doi tinh chat:

(defun c:c1 () (ssget) (command "change" "p" "" "p" "la" "layer1" ""))
(defun c:c2 () (ssget) (command "change" "p" "" "p" "la" "layer2" ""))
(defun c:c3 () (ssget) (command "change" "p" "" "p" "la" "layer3" ""))
(defun c:c4 () (ssget) (command "change" "p" "" "p" "la" "layer4" ""))
(defun c:c5 () (ssget) (command "change" "p" "" "p" "la" "layer5" ""))
(defun c:c6 () (ssget) (command "change" "p" "" "p" "la" "layer6" ""))
(defun c:c7 () (ssget) (command "change" "p" "" "p" "la" "layer7" ""))
(defun c:c8 () (ssget) (command "change" "p" "" "p" "la" "layer8" ""))
(defun c:c9 () (ssget) (command "change" "p" "" "p" "la" "layer9" ""))
(defun c:c0 () (ssget) (command "change" "p" "" "p" "la" "0" ""))
(defun c:cki () (ssget) (command "change" "p" "" "p" "la" "kinh" ""))
(defun c:ctu () (ssget) (command "change" "p" "" "p" "la" "tuong" ""))
(defun c:cth () (ssget) (command "change" "p" "" "p" "la" "thay" ""))
(defun c:ctr () (ssget) (command "change" "p" "" "p" "la" "truc" ""))
(defun c:co1 () (ssget) (command "change" "p" "" "p" "color" "1" ""))
(defun c:co2 () (ssget) (command "change" "p" "" "p" "color" "2" ""))
(defun c:co3 () (ssget) (command "change" "p" "" "p" "color" "3" ""))
(defun c:co4 () (ssget) (command "change" "p" "" "p" "color" "4" ""))
(defun c:co5 () (ssget) (command "change" "p" "" "p" "color" "5" ""))
(defun c:co6 () (ssget) (command "change" "p" "" "p" "color" "6" ""))
(defun c:co7 () (ssget) (command "change" "p" "" "p" "color" "7" ""))
(defun c:co8 () (ssget) (command "change" "p" "" "p" "color" "8" ""))
(defun c:co9 () (ssget) (command "change" "p" "" "p" "color" "9" ""))
(defun c:co0 () (ssget) (command "change" "p" "" "p" "color" "10" ""))
(defun c:co40 () (ssget) (command "change" "p" "" "p" "color" "40" ""))
(defun c:coo () (ssget) (command "change" "p" "" "p" "color" "bylayer" ""))
---------------------------------------------------------------------------
**Copy hang loat tang dan:
;;;=====Increasing copy=====
(defun c:ccb (/ ang x y ent tg tg1tg2 num_r num_c num_inc dis_r dis_c num top idnum
               dx dy bottom inc tgnum attr attr_ent t_base b_base locat value 
               deci stnum loca1 loca2 tt count inctg inctg1 bpoint mx my nx ny bx by)
  (setq idnum 0)
  (while (/= idnum 1)
    (setq ent (entsel "\nHay lua chon so ma ban muon copy : "))
    (if ent
      (progn
        (setq e (car ent))
        (setq tg (entget e))
        (if (= (cdr (assoc 0 tg)) "TEXT") (setq idnum 1))
      )
      (princ)
    )
  )

  (setq num_inc (getreal "\nHay nhap he so tang giam <1> : "))
  (if (= num_inc nil) (setq num_inc 1))

  (setq bpoint (getpoint "\nChon diem goc de copy : "))
  (setq x (car bpoint))
  (setq y (car(cdr bpoint)))

  (if (and (= (cdr (assoc 72 tg)) 0) (= (cdr (assoc 73 tg)) 0))
    (progn
      (setq bx (car (cdr (assoc 10 tg))))
      (setq by (car (cdr (cdr (assoc 10 tg)))))
    )
    (progn
      (setq bx (car (cdr (assoc 11 tg))))
      (setq by (car (cdr (cdr (assoc 11 tg)))))
    )
  )

  (setq attr (cdr tg))	;attr chua cac thuoc tinh cua Entity nguon
  (setq tg (cdr (assoc 1 tg)))
  (setq inc 0)
  (setq tg1 "")
  (setq t_base "")
  (setq b_base "")
  (setq idnum 0)
  (setq top 0)
  (setq bottom 0)
  (setq stnum "") 
  (setq deci 0) 
  (repeat  (strlen tg)    
    (if  (or (and (> (ascii (setq tgnum (substr tg (- (strlen tg) inc) 1))) 47) 
                    (< (ascii (setq tgnum (substr tg (- (strlen tg) inc) 1))) 58))
                    (= (ascii (setq tgnum (substr tg (- (strlen tg) inc) 1))) 32)
                    (= (ascii (setq tgnum (substr tg (- (strlen tg) inc) 1))) 46))
      (progn  
          (if  (= (ascii (setq tgnum (substr tg (- (strlen tg) inc) 1))) 46) (setq deci inc))      
          (if (= inc 0)
             (progn
               (setq idnum 1)
               (if  (= (ascii (setq tgnum (substr tg (- (strlen tg) inc) 1))) 46) 
                  (setq b_base (strcat "." b_base)))               
             )
          )
          (if (= bottom 1) (progn (setq bottom 0) (setq idnum 1) (setq top 1)))
          (if (and (= idnum 0) (= top 1)) (setq t_base (strcat tgnum t_base)))
          (if (= idnum 1) 
            (progn    
              (if (and (= tgnum "0") (> inc 0)) (setq stnum (strcat stnum "0")) (setq stnum ""))
              (setq tg1 (strcat tgnum tg1))
            )
          )
      )
      (if (= inc 0) 
        (progn
          (setq b_base (strcat tgnum b_base))
          (setq bottom 1)
        )
        (if (= bottom 1)
          (setq b_base (strcat tgnum b_base))
          (progn
            (setq top 1)
            (setq t_base (strcat tgnum t_base))
            (if (= idnum 1) (setq idnum 0))
          )
        )
      )
    )
    (setq inc (+ inc 1))
  )
  (if (= tg1 "") (exit))
  (setq num (atof tg1))
  (setq count 1)

  (while (setq bpoint (getpoint "\nChon diem copy tiep theo : "))
     (setq num (+ num num_inc))
     (setq value (strcat t_base (strcat stnum (rtos num 2 deci)) b_base))
     (setq nx (car bpoint))
     (setq ny (car(cdr bpoint)))
     (setq dx (- nx x))
     (setq dy (- ny y))
     (setq mx (car (getvar "ucsxdir")))
     (setq my (car (cdr (getvar "ucsxdir"))))
     (setq loca1 (+ bx (* mx dx)))
     (setq loca2 (+ by (* my dx)))
     (setq mx (car (getvar "ucsydir")))
     (setq my (car (cdr (getvar "ucsydir"))))
     (setq loca1 (+ loca1 (* mx dy)))
     (setq loca2 (+ loca2 (* my dy)))
     (setq attr_ent (subst (cons 1 value) (assoc 1 attr) attr))
     (if (and (= (cdr (assoc 72 attr_ent)) 0) (= (cdr (assoc 73 attr_ent)) 0))
       (setq attr_ent (subst (list 10 loca1 loca2 0) (assoc 10 attr_ent) attr_ent))
       (setq attr_ent (subst (list 11 loca1 loca2 0) (assoc 11 attr_ent) attr_ent))
     )
     (entmake attr_ent)
     (setq count (+ count 1))
  )	 ;end while
  (princ)
)
(defun c:rtr (/ ss1)
  (setq ss1 (ssget))(command "rotate" ss1 "" pause "r"))
(defun c:+11 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause "1.1"))
(defun c:+12 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause "1.2"))
(defun c:+10 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause "10"))
(defun c:+2 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause "2"))
(defun c:+15 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause "1.5"))
(defun c:+25 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause "2.5"))
(defun c:+4 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause "4"))
(defun c:-01 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause ".1"))
(defun c:-02 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause ".2"))
(defun c:-025 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause ".25"))
(defun c:-05 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause ".5"))
(defun c:-75 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause ".75"))
(defun c:-08 (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause ".8"))
(defun c:sr (/ ss1)
  (setq ss1 (ssget))(command "scale" ss1 "" pause "r"))
*========stretch
(defun c:ste () (command "stretch" "CP"))
(defun c:ru24 () (command "surftab1" "24" "surftab2" "24" "rulesurf"))
(defun c:ru40 () (command "surftab1" "40" "surftab2" "40" "rulesurf"))
(defun C:55() (command "offset" "55"))
(defun C:110 () (command "offset" "110"))
(defun C:220 () (command "offset" "220"))
(defun C:330() (command "offset" "330"))
(defun C:440() (command "offset" "440"))
(defun C:2400() (command "offset" "2400"))
(defun C:2100() (command "offset" "2100"))
(defun C:1500() (command "offset" "1500"))
(defun C:1600() (command "offset" "1600"))
(defun C:3600() (command "offset" "3600"))
(defun C:3400() (command "offset" "3400"))
(defun C:1000() (command "offset" "1000"))
(defun C:2000() (command "offset" "2000"))
(defun C:3000() (command "offset" "3000"))
(defun C:250() (command "offset" "250"))
(defun C:300() (command "offset" "300"))
(defun C:150() (command "offset" "150"))
(defun C:1800() (command "offset" "1800"))
(defun C:bp() (command "BOUNDARY" "p"))
(defun c:du1 () (command "dimlfac" "1" "dim1" "up"))
(defun c:du2 () (command "dimlfac" "2" "dim1" "up"))
(defun c:du5 () (command "dimlfac" ".5" "dim1" "up"))
(defun c:du02 () (command "dimlfac" ".2" "dim1" "up"))
(defun c:du01 () (command "dimlfac" ".1" "dim1" "up"))
(defun c:du25 () (command "dimlfac" ".25" "dim1" "up"))
(defun c:db () (command "dimbaseline"))
(defun c:du1a () (command "dimlfac" "1" "dim1" "up" "all"))
(defun c:du2a () (command "dimlfac" "2" "dim1" "up" "all"))
(defun c:du25a () (command "dimlfac" ".25" "dim1" "up" "all"))
;CHANGES
(defun c:con () (ssget) (command "change" "p" "" "p" "lt" "continuous" ""))
(defun c:hi1 () (ssget) (command "change" "p" "" "p" "lt" "hidden" ""))
(defun c:hi2 () (ssget) (command "change" "p" "" "p" "lt" "hidden2" ""))
(defun c:hx2 () (ssget) (command "change" "p" "" "p" "lt" "hiddenx2" ""))
(defun c:ce1 () (ssget) (command "change" "p" "" "p" "lt" "center" ""))
(defun c:ce2 () (ssget) (command "change" "p" "" "p" "lt" "center2" ""))
(defun c:cx2 () (ssget) (command "change" "p" "" "p" "lt" "centerx2" ""))
(defun c:da1 () (ssget) (command "change" "p" "" "p" "lt" "dashdot" ""))
(defun c:da2 () (ssget) (command "change" "p" "" "p" "lt" "dashdot2" ""))
(defun c:dx2 () (ssget) (command "change" "p" "" "p" "lt" "dashdotx2" ""))
(defun c:zza () (ssget) (command "change" "p" "" "p" "lt" "zigzag" ""))
(defun c:ts ()(setvar "ltscale" (getdist "\n He so: ")) (command "change" "p" "p" "ltsc" "s")(princ))
(defun c:on0 () (command "layer" "on" "layer0"))
(defun c:on1 () (command "layer" "on" "layer1"))
(defun c:on2 () (command "layer" "on" "layer2"))
(defun c:on3 () (command "layer" "on" "layer3"))
(defun c:on4 () (command "layer" "on" "layer4"))
(defun c:on5 () (command "layer" "on" "layer5"))
(defun c:on6 () (command "layer" "on" "layer6"))
(defun c:on7 () (command "layer" "on" "layer7"))
(defun c:on8 () (command "layer" "on" "layer8"))
(defun c:on9 () (command "layer" "on" "layer9"))
(defun c:onth () (command "layer" "on" "thay"))
(defun c:onnt () (command "layer" "on" "noithat"))
(defun c:ontr () (command "layer" "on" "truc"))
(defun c:ontu () (command "layer" "on" "tuong"))

(defun c:of0 () (command "layer" "off" "layer0"))
(defun c:of1 () (command "layer" "off" "layer1"))
(defun c:of2 () (command "layer" "off" "layer2"))
(defun c:of3 () (command "layer" "off" "layer3"))
(defun c:of4 () (command "layer" "off" "layer4"))
(defun c:of5 () (command "layer" "off" "layer5"))
(defun c:of6 () (command "layer" "off" "layer6"))
(defun c:of7 () (command "layer" "off" "layer7"))
(defun c:of8 () (command "layer" "off" "layer8"))
(defun c:of9 () (command "layer" "off" "layer9"))
(defun c:ofth () (command "layer" "off" "thay"))
(defun c:ofnt () (command "layer" "off" "noithat"))
(defun c:oftr () (command "layer" "off" "truc"))
(defun c:oftu () (command "layer" "off" "tuong"))

(defun c:lk0 () (command "layer" "lock" "layer0"))
(defun c:lk1 () (command "layer" "lock" "layer1"))
(defun c:lk2 () (command "layer" "lock" "layer2"))
(defun c:lk3 () (command "layer" "lock" "layer3"))
(defun c:lk4 () (command "layer" "lock" "layer4"))
(defun c:lk5 () (command "layer" "lock" "layer5"))
(defun c:lk6 () (command "layer" "lock" "layer6"))
(defun c:lk7 () (command "layer" "lock" "layer7"))
(defun c:lk8 () (command "layer" "lock" "layer8"))
(defun c:lk9 () (command "layer" "lock" "layer9"))
(defun c:lkth () (command "layer" "lock" "thay"))
(defun c:lknt () (command "layer" "lock" "noithat"))
(defun c:lktr () (command "layer" "lock" "truc"))
(defun c:lktu () (command "layer" "lock" "tuong"))

(defun c:ul0 () (command "layer" "unlock" "layer0"))
(defun c:ul1 () (command "layer" "unlock" "layer1"))
(defun c:ul2 () (command "layer" "unlock" "layer2"))
(defun c:ul3 () (command "layer" "unlock" "layer3"))
(defun c:ul4 () (command "layer" "unlock" "layer4"))
(defun c:ul5 () (command "layer" "unlock" "layer5"))
(defun c:ul6 () (command "layer" "unlock" "layer6"))
(defun c:ul7 () (command "layer" "unlock" "layer7"))
(defun c:ul8 () (command "layer" "unlock" "layer8"))
(defun c:ul9 () (command "layer" "unlock" "layer9"))
(defun c:ulth () (command "layer" "unlock" "thay"))
(defun c:ulnt () (command "layer" "unlock" "noithat"))
(defun c:ultr () (command "layer" "unlock" "truc"))
(defun c:ultu () (command "layer" "unlock" "tuong"))

(defun c:se0 () (command "layer" "set" "layer0"))
(defun c:se1 () (command "layer" "set" "layer1"))
(defun c:se2 () (command "layer" "set" "layer2"))
(defun c:se3 () (command "layer" "set" "layer3"))
(defun c:se4 () (command "layer" "set" "layer4"))
(defun c:se5 () (command "layer" "set" "layer5"))
(defun c:se6 () (command "layer" "set" "layer6"))
(defun c:se7 () (command "layer" "set" "layer7"))
(defun c:se8 () (command "layer" "set" "layer8"))
(defun c:se9 () (command "layer" "set" "layer9"))
(defun c:seth () (command "layer" "set" "thay"))
(defun c:sent () (command "layer" "set" "noithat"))
(defun c:setr () (command "layer" "set" "truc"))
(defun c:setu () (command "layer" "set" "tuong"))

-------------------------------------------------------------
(defun C:Dla (/ olderr ocmd L S) 
  (setq olderr *error* 
        *error* dellerr)
  (setq ocmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq L (strcase (getstring "\nLayer(s) to delete: ")))
  ;; Get all entities on layer(s)
  (setq S (ssget "X" (list (cons 8 L)))) 
  (if S 
    (command "ERASE" S "")            ; Delete 'em!
    (princ "Layer empty or not a valid layer name.")
  ) 
  (setq S nil)                        ; Free selection-set
  (setvar "CMDECHO" ocmd)             ; Restore saved mode
  (setq *error* olderr)               ; Restore old *error* handler
  (princ)
)

;;; ===== Tutorial Utility Functions =====

;;; If you can find the Toolbook Viewer and the Toolbook file, 
;;; start this Toolbook Tutorial

(defun playtbk (book / exe full fbook)
  (setq exe   (findfile "tbook.exe"))

  (setq full (strcat book ".tbk"))
  (if exe
    (setq fbook 
      (cond
        ((findfile full))
        ((findfile book))
        (T nil) 
      )
    )
  )
  (if (and exe fbook)
    (startapp exe fbook)
    (alert "Cannot run tutorial.")
  )
)

(defun acad_helpdlg (helpfile topic)
  (help helpfile topic)
)

(defun *merr* (msg)
  (setq *error* m:err m:err nil)
  (princ)
)

(defun *merrmsg* (msg)
  (princ msg)
  (setq *error* m:err m:err nil)
  (princ)
)
;;; ===== AutoLoad =====
;;  
;;  Native Rx commands cannot be called with the "C:" syntax.  They must 
;;  be called via (command).  Therefore they require their own autoload 
;;  command.

(defun autonativeload (app cmdliste / qapp)
  (setq qapp (strcat "\"" app "\""))
  (setq initstring "\n")
  (mapcar
   '(lambda (cmd / nom_cmd native_cmd)
      (progn
        (setq nom_cmd (strcat "C:" cmd))
        (setq native_cmd (strcat "\"_" cmd "\""))
        (if (not (eval (read nom_cmd)))
            (eval
             (read (strcat
                    "(defun " nom_cmd "()"
                    "(setq m:err *error* *error* *merrmsg*)"
                    "(if (ai_ffile " qapp ")"
                    "(progn (princ initstring)"
                    "(_autoarxload " qapp ") (command " native_cmd "))"
                    "(ai_nofile " qapp "))"
                    "(setq *error* m:err m:err nil))"
                    ))))))
   cmdliste)
  nil
)

(defun _autoqload (quoi app cmdliste / qapp symnam)
  (setq qapp (strcat "\"" app "\""))
  (setq initstring "\nBat dau nap_PHAN HA HUNG...")
  (mapcar
   '(lambda (cmd / nom_cmd)
      (progn
        (setq nom_cmd (strcat "C:" cmd))
        (if (not (eval (read nom_cmd)))
            (eval
             (read (strcat
                    "(defun " nom_cmd "( / rtn)"
                    "(setq m:err *error* *error* *merrmsg*)"
                    "(if (ai_ffile " qapp ")"
                    "(progn (princ initstring)"
                    "(_auto" quoi "load " qapp ") (setq rtn (" nom_cmd ")))"
                    "(ai_nofile " qapp "))"
                    "(setq *error* m:err m:err nil)"
                    "rtn)"
                    ))))))
   cmdliste)
  nil
)

(defun autoload (app cmdliste)
  (_autoqload "" app cmdliste)
)

(defun autoxload (app cmdliste)
  (_autoqload "x" app cmdliste)
)

(defun autoarxload (app cmdliste)
  (_autoqload "arx" app cmdliste)
)

(defun _autoload (app)
; (princ "Auto:(load ") (princ app) (princ ")") (terpri)
  (load app)
)

(defun _autoxload (app)
; (princ "Auto:(xload ") (princ app) (princ ")") (terpri)
  (if (= app "region") (ai_select))
  (xload app)
  (if (= app "region") (ai_amegrey "~"))
)

(defun _autoarxload (app)
; (princ "Auto:(arxload ") (princ app) (princ ")") (terpri)
  (arxload app)
)

(defun ai_ffile (app)
  (or (findfile (strcat app ".lsp"))
      (findfile (strcat app ".exp"))
      (findfile (strcat app ".exe"))
      (findfile (strcat app ".arx"))
      (findfile app)
  )
)

(defun ai_nofile (filename)
  (princ
    (strcat "\nThe file "
            filename
            "(.lsp/.exp/.exe/.arx) was not found in your search path directories."
    )
  )
  (princ "\nCheck the installation of the Support Files and try again.")
  (princ)
)


;;;===== AutoLoad LISP Applications =====

(autoload "appload" '("appload" "appload"))

(autoload "edge"  '("edge"))

(autoload "filter" '("filter " "filter"))

(autoload "3d" '("3d" "3d" "ai_box" "ai_pyramid" "ai_wedge" "ai_dome"
                 "ai_mesh" "ai_sphere" "ai_cone" "ai_torus" "ai_dish")
)

(autoload "ddinsert" '("ddinsert"))

(autoload "ddattdef" '("ddattdef"))

(autoload "ddattext" '("ddattext"))

(autoload "3darray" '("3darray"))

(autoload "ddmodify" '("ddmodify"))

(autoload "ddchprop" '("ddchprop"))

(autoload "ddview" '("ddview"))

(autoload "ddvpoint" '("ddvpoint"))

(autoload "mvsetup" '("mvsetup"))

(autoload "ddosnap" '("ddosnap"))

(autoload "ddptype" '("ddptype"))

(autoload "dducsp" '("dducsp"))

(autoload "ddunits" '("ddunits"))

(autoload "ddgrips" '("ddgrips"))

(autoload "ddselect" '("ddselect"))

(autoload "ddrename" '("ddrename"))

(autoload "ddcolor" '("ddcolor"))

(autoload "xrefclip" '("xrefclip"))

(autoload "attredef" '("attredef"))

(autoload "xplode" '("xp" "xplode"))

(autoload "ame" '("ame"))

;;;===== Autoload platform-specific applications =====

(if (wcmatch (getvar "platform") "*DOS*")
    (autoload "r13new" '("whatsnew" "whatsnew"))
    (autoload "tutorial" '("tutdemo" "tutclear"
				       "tutdemo" 
				       "tutclear"))
)

;;;===== AutoXLoad ADS Applications =====

(autoxload "rasterin" '( "gifin"	"riaspect"	"pcxin"	
			 "riedge"	"rigamut"	"rigrey"
			 "ribackg"	"rithresh"	"tiffin"
			"gifin" "pcxin"
			"riaspect" "ribackg"
			"riedge" "rigamut"
			"rigrey" "rithresh"
			"tiffin")
)

(autoxload "geomcal" '("cal" "cal"))

(autoxload "geom3d" '("mirror3d" "rotate3d" "align"
		      "mirror3d" "rotate3d" 
                                 "align"))

(autoxload "hpmplot" ' ("hpconfig" "hprender" "hpmplot"
			"hpconfig" "hprender" 
                                  "hpmplot"))

;;;===== AutoArxLoad Arx Applications =====

(defun AutoVisionPresent ()
  (setq AutoVisionPresent
    (list '()
      (and
          (getenv "ACAD")
          (wcmatch
            (strcase (getenv "ACAD") T)
            "*avis_sup*"
          )
          (findfile "autovis.arx")
      )
    )
  )
)

(AutoVisionPresent)

(defun autoloadrender (/ filedia cmdecho)
  (if (AutoVisionPresent)
      (progn (autoarxload "autovis" '("render"  "rpref"  "rmat"       "light" 
				      "matlib"  "replay" "saveimg"    "3dsin" 
				      "3dsout"  "vlconv" "rconfig"    "scene"
                                      "stats"   "setuv"  "showmat"    "rfileopt"
                                      "rendscr" "fog"    "background" "lsnew"
                                      "lsedit"  "lslib"
				      "render"
                                      "rpref" 
                                      "rmat"
                                      "light" 
                                      "matlib"
                                      "replay" 
                                      "saveimg"
                                      "3dsin" 
                                      "3dsout"
                                      "vlconv" 
                                      "rconfig"
                                      "scene" 
                                      "stats"
                                      "setuv"
                                      "showmat"
                                      "rfileopt"
                                      "rendscr"
                                      "fog"
                                      "background"
                                      "lsnew"
                                      "lsedit"
                                      "lslib"))
	     (autoload "anim" '("animate" "animation"
				"animate"
				"animation"))

        )
                              
       (autoarxload "render" '("render"  "rpref"   "rmat"    "light"	
                               "matlib"  "replay"  "saveimg" "3dsin"
                               "3dsout"  "vlconv"  "rconfig" "scene"
			       "showmat" "rendscr" "stats"
			      "render"
                              "rpref" 
                              "rmat"
                              "light" 
                              "matlib"
                              "replay" 
                              "saveimg" 
                              "3dsin" 
                              "3dsout"
                              "vlconv" 
                              "rconfig"
                              "scene" 
                              "showmat" 
                              "rendscr" 
                              "stats"))
   )
)
(autoloadrender)

(defun C:RENDERUNLOAD ()
  (if (if (autovisionPresent)
        (arxunload "autovis" nil)
        (arxunload "render" nil)
      )
      (progn
        (autoloadrender)
        (princ "\nRender has been unloaded from memory. ")
      )
      (princ "\nRender is not loaded. ")
  )
  (princ)
)

(defun autoloadase ()
  (autonativeload "ase" '("aseadmin"	"aserows"	"aselinks"
			"aseselect"	"aseexport"	"asesqled"
		    "aseadmin" 
		    "aserows" 
		    "aselinks" 
		    "aseselect" 
		    "aseexport" 
		    "asesqled"))
)
(autoloadase)

(princ)

;;; --------------------------- Main Program ---------------------------------;

(defun C:CCv (/ olderr clay sblip scmde sgrid shl sucsf e cen rad d ts xx) 
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
  (setq ts (tblsearch "LAYER" "CL"))
  (if (null ts) 
    (prompt "\nCreating new layer - CL. ") 
    (progn
      (if (= (logand 1 (cdr (assoc 70 ts))) 1) 
        (progn
          (prompt "\nLayer CL is frozen. ") 
          (initget "Yes No") 
          (setq xx (getkword "\nProceed? <N>: "))
          (if (= xx "Yes") 
            (command "_.LAYER" "_T" "CL" "")
          )
        )
      )
    )
  ) 
  (if (= xx "Yes") 
    (progn
      (while (null e) 
        (setq e (entsel "\nHay chon cung hoac duong tron: "))
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
      (setq d (getdist "\nChokichthuoc/<Extension>: "))
      (if (= d "Length") 
        (progn
          (initget 7) 
          (setq d (getdist cen "\nLength: "))
        ) 
        (setq d (+ rad d))
      ) 
      (setvar "BLIPMODE" 0)
      (setvar "HIGHLIGHT" 0)
      (command "_.LAYER" "_M" "CL" "") 
      (command "_.LINE" (list (car cen) (- (cadr cen) d) (caddr cen)) 
               (list (car cen) (+ (cadr cen) d) (caddr cen)) ""
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

;;; --------------------------------------------------------------------------;
(defun C:DFIN (
/  ps1  ps2  pf1  pf2  pf3  pf4  pf5  pf6
   pf7  pf8  sb   sh   se   ang  old_osmode old_wid)
   (setq old_osmode (getvar "OSMODE"))
   (setq ps1 (getpoint "\nPick first point: "))
   (setq ps2 (getpoint ps1 "\nPick second point: "))
   (setq sb 2.0 sh 4.0 se 2.0)
   (setq ang (angle ps1 ps2))
   (setq pf1 (polar ps1 ang (- 0.0 se)))
   (setq pf2 (polar ps1 ang (/ (- (distance ps1 ps2) sb) 2.0) ))
   (setq pf7 (polar pf2 ang (/ sb 3.0)))
   (setq pf3 (polar pf7 (+ ang (/ pi 2)) (/ sh 2.0)))
   (setq pf8 (polar pf7 ang (/ sb 3.0)))
   (setq pf4 (polar pf8 (- ang (/ pi 2)) (/ sh 2.0)))
   (setq pf5 (polar pf8 ang (/ sb 3.0)))
   (setq pf6 (polar ps2 ang se))

   (setvar "OSMODE" 0)
   (setvar "BLIPMODE" 0)
   (setq old_wid (getvar "PLINEWID"))
   (setvar "PLINEWID" 0)

   (command "PLINE" pf1 pf2 pf3 pf4 pf5 pf6 "")

   (setvar "OSMODE" old_osmode)
   (setvar "BLIPMODE" 1)
   (setvar "PLINEWID" old_wid)
   (princ)
   )
(defun C:DfF (
/  ps1  ps2  pf1  pf2  pf3  pf4  pf5  pf6
   pf7  pf8  sb   sh   se   ang  old_osmode old_wid)
   (setq old_osmode (getvar "OSMODE"))
   (setq ps1 (getpoint "\nPick first point: "))
   (setq ps2 (getpoint ps1 "\nPick second point: "))
   (setq sb 2.5 sh 3.5 se 1.0)
   (setq ang (angle ps1 ps2))
   (setq pf1 (polar ps1 ang (- 0.0 se)))
   (setq pf2 (polar ps1 ang (/ (- (distance ps1 ps2) sb) 2.0) ))
   (setq pf3 (polar pf2 (+ ang (/ pi 2)) (/ sh 2.0)))
   (setq pf5 (polar pf2 ang sb))
   (setq pf4 (polar pf5 (- ang (/ pi 2)) (/ sh 2.0)))
   (setq pf6 (polar ps2 ang se))

   (setvar "OSMODE" 0)
   (setvar "BLIPMODE" 0)
   (setq old_wid (getvar "PLINEWID"))
   (setvar "PLINEWID" 0)

   (command "PLINE" pf1 pf2 pf3 pf4 pf5 pf6 "")

   (setvar "OSMODE" old_osmode)
   (setvar "BLIPMODE" 1)
   (setvar "PLINEWID" old_wid)
   (princ)
(defun C:Gf (/ point p1 p2 oldwid oldosmode)
 (setq point (getpoint "\nInsert point : "))
 (setq p1 (polar point (+ (/ pi 4) pi) 1) )
 (setq p2 (polar point    (/ pi 4)     1) )
 (command "LAYER" "M" "CHI" "")
 (setq oldwid (getvar "PLINEWID"))
 (setq oldosmode (getvar "OSMODE"))
 (setvar "PLINEWID" 0.2)
 (setvar "OSMODE" 0)
 (command "PLINE" p1 point p2 "")
 (setvar "PLINEWID" oldwid)
 (setvar "OSMODE" oldosmode)
 (princ)
)