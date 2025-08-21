
******************************************************************
; -------------------- ISOLATE LAYER FUNCTION --------------------
; Ch‰n 1 ÆËi t≠Óng lµm Layer hi÷n hµnh & tæt T t c∂ c∏c Layer Kh∏c
; ----------------------------------------------------------------
******************************************************************

(Defun C:LK (/ SS CNT LAY LAYLST VAL)

  (if (not (setq SS (ssget "i")))
    (progn
      (prompt "\nMay hay chon 1 doi tuong lam Layer hien hanh va tat het cac Layer khac: ")
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
        (prompt (strcat "\nLayer " (car LAYLST) " Da chuyen sang Layer hien hanh. The nhe!."))
        (prompt (strcat "\n" (itoa (length LAYLST)) " layers thi lam sao ma hien hanh cung 1 Luc duoc ha.... Tao buon qua            Do ngu! May hay chon lai di. "
                        "Layer " LAY " dang la Layer hien hanh day."
                )
        )
      )
    )
  )

  

  (princ)
)

******************************************************************
; --------------------- LAYER LOCK FUNCTION ----------------------
; Kho∏ 1 layer c n ch‰n
; ----------------------------------------------------------------
******************************************************************

(Defun C:LO (/ LAY)

  (setq LAY (entsel "\nChon 1 doi tuong tren Layer May muon Khoa: "))

  (if LAY
    (progn
      (setq LAY (cdr (assoc 8 (entget (car LAY)))))
      (Command "_.-LAYER" "_LOCK" LAY "")
      (prompt (strcat "\nLayer " LAY " Vua duoc Khoa. Xin cam on!."))
    )
  )



  (princ)
)


; -------------------- LAYER UNLOCK FUNCTION ---------------------
; B· kho∏ 1 layer c«n ch‰n
; ----------------------------------------------------------------

(Defun C:LUN (/ LAY)

  (if (not (setq SS (ssget "i")))
    (progn
  (setq LAY (entsel "\nChon 1 doi tuong tren Layer May muon Mo Khoa: "))

      (Command "_.-LAYER" "_UNLOCK" LAY "")
      (prompt (strcat "\nLayer " LAY " Vua Duoc May mo Khoa. Ke ra May cung kha."))
    )
  )

 

  (princ)
)
******************************************************************
; ---------------------- LAYER ON FUNCTION -----------------------
; BÀt t t c∂ c∏c Layer 
; ----------------------------------------------------------------
******************************************************************

(Defun C:ON ()
   (Command "_.-LAYER" "ON" "*" "")
  (prompt "\nOK Roi day! Tat ca cac Layer cua May da bat Len roi.")

  (princ) 
)

; ---------------------- LAYER OFF FUNCTION ----------------------
; Tat LAYER minh chon  LAYOFF
; ----------------------------------------------------------------

(defun C:LF ()
  (layproc "off")
  (princ)
)

; ------------- LAYER PROCESSOR FOR LAYOFF & LAYFRZ --------------
; Main program body for LAYOFF and LAYFRZ. Provides user with
; options for handling nested entities.
; ----------------------------------------------------------------

(defun LAYPROC ( TASK / NOEXIT OPT BLKLST CNT EN PMT ANS LAY NEST BLKLST)



; -------------------- Variable initialization -------------------

  (setq NOEXIT T)

  (setq OPT (getcfg (strcat "AppData/AC_Bonus/Lay" TASK)))    ; get default option setting
  (if (not (or (null OPT) (= OPT ""))) (setq OPT (atoi OPT)))

  (setq CNT 0)                                                ; cycle counter


  (while NOEXIT

    (initget "Options Undo")
    (if (= TASK "off")
      (setq EN (nentsel "\nOptions/Undo/<Kich len doi tuong ma May muon tat Layer do>: "))
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
              (prompt (strcat "\nDo ngu! layer " LAY " La Layer Hien hanh. May muon tat Layer " LAY" Khong? <N>: "))
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
              (prompt (strcat "\nLayer " LAY " Vua duoc May tat. May co muon tiep tuc Khong?"))
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



)


******************************************************************
; --------------------- LAYER MATCH FUNCTION ---------------------
; Thay ÆÊi ÆËi t≠Óng theo Layer c«n ch‰n
; 
; ----------------------------------------------------------------
******************************************************************

(Defun C:lCH (/ SS CNT LOOP LAY ANS)



  (if (not (setq SS (ssget "i")))
    (progn
      (prompt "\nMay hay chon Nhung doi tuong thuoc Layer can thay doi: ")
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
      (setq LAY  (entsel "\nType name/Chon D.tuong de Layers vua chon thay doi thuoc tinh theo Layer do: ")
            LOOP T
      )
      
      (while LOOP
        (cond
          ((not LAY)
            (prompt "\nChang co doi tuong nao ca.")
            (prompt "\nMay co muon chon layer Hien hanh khong? Lau qua roi day <Y> ")
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
            (setq LAY (getstring "\nMe kiep! The thi May Nhap 1 Layer May muon vao: "))
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
            (setq LAY (entsel "\nType name/Tien su May! Chon di chu. Neu khong thi Enter tiep: "))
          )
        )
      ); while LOOP
        

      (if (listp LAY)
        (setq LAY (cdr (assoc 8 (entget (car LAY)))))
      )

      (command "_.change" SS "" "_p" "_la" LAY "")

      (if (= (sslength SS) 1)
        (prompt (strcat "\nOne object changed to layer " LAY ))
        (prompt (strcat "\n" (itoa (sslength SS)) " Doi tuong da thay doi thuoc tinh theo layer " LAY ))
      )
      (if (= LAY (getvar "clayer"))
        (prompt " (the current layer).")
        (prompt ".")
      )
    )
  )



  (princ)
)

;**********************************************************
;*                                                        *
;* NAME :        CHCULAY.LSP                              *
;*                                                        *
;* FUNCTION :    THAY DOI DOI TUONG CHON THEO LAYER       *
;*               HIEN HANH                                *   
;*                                                        *
;* WRITTEN  :    NGUYEN HAI NAM                           *
;*               10-2000                                  *
;*                                                        *
;**********************************************************
(defun c:LQ (/ a1 a2 a3 n index b1 d1 b2)
 (graphscr)
 (prompt "Chon doi tuong de thay doi thuoc tinh theo Layer hien hanh") (terpri)
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
(Prompt "\nNhung doi tuong vua chon da thay doi thuoc tinh theo layer ")(Princ a2)
(Prompt "\n")
(Princ)
)


;**********************************************************
;*                                                        *
;* NAME :        OFFCHLAY.LSP                             *
;*                                                        *
;* FUNCTION :    OFFSET DOI TUONG VA THAY DOI TINH CHAT   *
;*               DOI TUONG THEO LAYER HIEN HANH           *   
;*                                                        *
;* WRITTEN  :    NGUYEN HAI NAM                           *
;*               10-2000                                  *
;*                                                        *
;**********************************************************


(defun c:OL ()
(setq cmd (getvar "cmdecho"))
(setvar "cmdecho" 0)
(setq olderr *error*
          *error* offerr)
(if (/= (numberp dist))
    (setq dist 15)
)
(if (numberp new)
   (setq dist new)
   )
(while
(princ "\nMay co muon OFFSET doi tuong voi 15 don vi khong? CHU Y LAYER HIEN HANH<")
(princ dist)
(princ ">:")
(setq new (getreal))
(if (numberp new)
    (setq dist new)
    )
(setq a (entsel "\nChon doi tuong de thay doi Layer va Offset:"))
(setq s (getpoint "\nOffset ve phia nao                 :"))
(command "offset" dist a s "")
(setq b (getvar "clayer"))
(command "change" "l" "si" "p" "la" b "")
)
)

;**********************************************************
;*                                                        *
;* NAME :        SETLAYER.LSP                             *
;*                                                        *
;* FUNCTION :    THAY DOI LAYER HIEN HANH THEO DOI        *
;*               TUONG CHON                               *   
;*                                                        *
;* WRITTEN  :    NGUYEN HAI NAM                           *
;*               10-2000                                  *
;*                                                        *
;**********************************************************

(defun c:Kl (/ va)
 (setvar "cmdecho" 0)
 (setq va (cdr (assoc 8 (entget
                         (car (entsel "May hay chon doi tuong Layer ma May muon lam hien hanh: "))))))
 (command "layer" "set" va "")
  (princ)
)

;**********************************************************
;*                                                        *
;* NAME :        OF.LSP                                   *
;*                                                        *
;* FUNCTION :    OFF TAT CA LAYER CHI TRU LAYER HIEN HANH *
;*                                                        *
;* WRITTEN  :    NGUYEN HAI NAM                           *
;*               10-2000                                  *
;*                                                        *
;**********************************************************

(defun c:OF ()
  (command "layer" "off" "*" "" "" "")
   (prompt "\nOK Roi day! Tat ca cac Layer cua May da tat chi con Layer hien hanh.")
   (princ)
)

;**********************************************************
;*                                                        *
;* NAME :        LOO.LSP                                  *
;*                                                        *
;* FUNCTION :    TU DONG MO KHOA TAT CA LAYER             *
;*                                                        *
;* WRITTEN  :    NGUYEN HAI NAM                           *
;*               10-2000                                  *
;*                                                        *
;**********************************************************

(defun c:LLo ()
  (command "layer" "U" "*" "")
  (prompt "\nOK Roi day! Tat ca cac Layer cua May da mo khoa.")
   (princ)
)
***************************************************
(defun c:1 ()
  (command "-layer" "s" "1" "" "")
   (prompt "\nOK Roi day! Layer hien hanh cua May la Layer 1.")
 (princ)
)
(defun c:2 ()
  (command "-layer" "s" "2" "" "")
   (prompt "\nOK Roi day! Layer hien hanh cua May la Layer 2.")
 (princ)
)
(defun c:3 ()
  (command "-layer" "s" "3" "" "")
   (prompt "\nOK Roi day! Layer hien hanh cua May la Layer 3.")
 (princ)
)
(defun c:4 ()
  (command "-layer" "s" "4" "" "")
   (prompt "\nOK Roi day! Layer hien hanh cua May la Layer 4.")
 (princ)
)
(defun c:5 ()
  (command "-layer" "s" "5" "" "")
   (prompt "\nOK Roi day! Layer hien hanh cua May la Layer 5.")
 (princ)
)
(defun c:6 ()
  (command "-layer" "s" "6" "" "")
   (prompt "\nOK Roi day! Layer hien hanh cua May la Layer 6.")
 (princ)
)
(defun c:7 ()
  (command "-layer" "s" "7" "" "")
   (prompt "\nOK Roi day! Layer hien hanh cua May la Layer 7.")
 (princ)
)
(defun c:8 ()
  (command "-layer" "s" "8" "" "")
   (prompt "\nOK Roi day! Layer hien hanh cua May la Layer 8.")
 (princ)
)
(defun c:9 ()
  (command "-layer" "s" "9" "" "")
   (prompt "\nOK Roi day! Layer hien hanh cua May la Layer 9.")
 (princ)
)
(defun c:0 ()
  (command "-layer" "s" "0" "" "")
   (prompt "\nOK Roi day! Layer hien hanh cua May la Layer 0.")
 (princ)
)

*********************************
(defun c:11 ()
  (ssget)
  (command "change" "p" "" "P" "LA" "1" "")
  (prompt "\nOK Roi day! Nhung cai doi tuong vua chon da thay doi thuoc tinh theo Layer 1.")
 (princ)
)
(defun c:22 ()
  (ssget)
  (command "CHANGE" "P" "" "p" "la" "2" "")
 (prompt "\nOK Roi day! Nhung cai doi tuong vua chon da thay doi thuoc tinh theo Layer 2.")
 (princ)
)
(defun c:33 ()
  (ssget)
  (command "CHANGE" "P" "" "p" "la" "3" "")
 (prompt "\nOK Roi day! Nhung cai doi tuong vua chon da thay doi thuoc tinh theo Layer 3.")
 (princ)
)
(defun c:44 ()
  (ssget)
  (command "CHANGE" "P" "" "p" "la" "4" "")
 (prompt "\nOK Roi day! Nhung cai doi tuong vua chon da thay doi thuoc tinh theo Layer 4.")
 (princ)
)
(defun c:55 ()
  (ssget)
  (command "CHANGE" "P" "" "p" "la" "5" "")
 (prompt "\nOK Roi day! Nhung cai doi tuong vua chon da thay doi thuoc tinh theo Layer 5.")
 (princ)
)
(defun c:66 ()
  (ssget)
  (command "CHANGE" "P" "" "p" "la" "6" "")
 (prompt "\nOK Roi day! Nhung cai doi tuong vua chon da thay doi thuoc tinh theo Layer 6.")
 (princ)
)
(defun c:77 ()
  (ssget)
  (command "CHANGE" "P" "" "p" "la" "7" "")
 (prompt "\nOK Roi day! Nhung cai doi tuong vua chon da thay doi thuoc tinh theo Layer 7.")
 (princ)
)
(defun c:88 ()
  (ssget)
  (command "CHANGE" "P" "" "p" "la" "8" "")
 (prompt "\nOK Roi day! Nhung cai doi tuong vua chon da thay doi thuoc tinh theo Layer 8.")
 (princ)
)
(defun c:99 ()
  (ssget)
  (command "CHANGE" "P" "" "p" "la" "9" "")
 (prompt "\nOK Roi day! Nhung cai doi tuong vua chon da thay doi thuoc tinh theo Layer 9.")
 (princ)
)
(defun c:00 ()
  (ssget)
  (command "CHANGE" "P" "" "p" "la" "0" "")
 (prompt "\nOK Roi day! Nhung cai doi tuong vua chon da thay doi thuoc tinh theo Layer 0.")
 (princ)
)
**********************************





