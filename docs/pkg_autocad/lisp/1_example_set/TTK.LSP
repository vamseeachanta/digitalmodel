;;;
;;; TK.LSP Version 2.0, Copyright (C) (1997) by DANG XUAN LOI - CONG TY XAY DUNG THANH NAM
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
        (ai_abort "tk"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "tk" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "tk" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================
;;; ===================== Insert Function ============================
(defun C:TK_IN ( / old_cmd old_osmode
                   inspoint path
               )
 (setq  old_cmd (getvar "CMDECHO")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
  )
 (setvar "CMDECHO" 0)
 ;; --------------------------------------------------
 (setq path "*c:\\acadtool\\lisp\\tk")
 (initget 1)
 (setq inspoint (getpoint "\nDiem chen bang: "))
 (command "INSERT" path inspoint "" "" )
 ;; --------------------------------------------------
 (setq *error* old_error)
 (setvar "CMDECHO" old_cmd)
 (princ)
)
;;; ===================== TK Function ============================
(defun C:TK ( / old_cmd old_osmode i  dlist
             l1     l2         l3
             sh     style      dk     dai     sl1     slck     sla    dt     tl    chuvi   nchuvi
             p0   p1    p2    p3    p4    p5    p6    p7    p8    p9
             nhap   thongke    ve    set_osnap  set_style   tk_lib
            )
;==================== Chon kieu thep ==========================
(defun tk_lib ( / app dcl_id dir tk_no in_point explode
		    i   no     tk_next tk_old no_tk   tk_currtile

		    ai_tk_start  ai_tk_set  ai_tk_main  set_explode  set_currtile
		    set_no_tk	 next_tk    pre_tk      grey
		)
(defun set_no_tk ()
 (setq no_tk 12)
 (setq tk_next 1)
 (setq tk_old tk_next)
 (setq tk_no "1")
 (setq tk_currtile "tk_1")
)

(defun grey ()
 (if (= 1 tk_next) 
     (progn
      (mode_tile "previous" 1)
      (if (> no_tk (* tk_next 12)) (mode_tile "next" 0) (mode_tile "next" 1) )
     )
     (progn
      (mode_tile "previous" 0)
      (if (> no_tk (* tk_next 12)) (mode_tile "next" 0) (mode_tile "next" 1) )
     )
 )
)

(defun ai_tk_start ()
  (setq i 0)
  (repeat 12
    (setq i (+ 1 i))
    (setq no (itoa i)) 
    (start_image (strcat "tk_" no))
    (fill_image 0 0
      (dimx_tile (strcat "tk_" no)) (dimy_tile (strcat "tk_" no))
      0
    )
    (slide_image
      0 0
      (dimx_tile (strcat "tk_" no)) (dimy_tile (strcat "tk_" no))
;      (strcat "tk_" (itoa (+ i (* (- tk_next 1) 8) ) )) 
      (strcat "tk_lib(tk_" (itoa (+ i (* (- tk_next 1) 12))) ")") 
    )
    (end_image)
  )
  (grey)
)

(defun next_tk ()
 (if (> no_tk (* 12 tk_next)) 
     (setq tk_next (+ 1 tk_next))
 )
 (ai_tk_start)
; (if (= tk_old tk_next) (mode_tile tk_currtile 4))
)

(defun pre_tk ()
 (if (> tk_next 1) 
     (setq tk_next (- tk_next 1))
 )
 (ai_tk_start)
; (if (= tk_old tk_next) (mode_tile tk_currtile 4))
)

(defun set_currtile ( currtile )
 (setq style (+ currtile (* (- tk_next 1) 12)))
 (setq style (- style 1))
 (cond
  ( (= style 6) (setq style 51))
 )
 (done_dialog)
 (princ style)
)

(defun ai_tk_main ()
  (if (not (new_dialog "tk_lib" dcl_id))
    (exit)
  )
  (ai_tk_start)
  (action_tile "tk_1" "(set_currtile 1)")
  (action_tile "tk_2" "(set_currtile 2)")
  (action_tile "tk_3" "(set_currtile 3)")
  (action_tile "tk_4" "(set_currtile 4)")
  (action_tile "tk_5" "(set_currtile 5)")
  (action_tile "tk_6" "(set_currtile 6)")
  (action_tile "tk_7" "(set_currtile 7)")
  (action_tile "tk_8" "(set_currtile 8)")
  (action_tile "tk_9" "(set_currtile 9)")
  (action_tile "tk_10" "(set_currtile 10)")
  (action_tile "tk_11" "(set_currtile 11)")
  (action_tile "tk_12" "(set_currtile 12)")
  (action_tile "previous" "(pre_tk)")
  (action_tile "next" "(next_tk)")
  (action_tile "cancel" "(done_dialog 1)")
  (if (= (start_dialog) 1) (set_style))
)
;;-------------------------------------------
  (set_no_tk)
  (cond
     (  (not (ai_notrans)))                     ; transparent not OK
     (  (not (ai_acadapp)))                     ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "tk"))))  ; is .DCL file loaded?
     (T (ai_undo_push)
        (ai_tk_main)
        (ai_undo_pop)
     )
  )
  (princ)
)
;==================== Dat bat dinh ==========================
(defun set_osnap ( os )
 (if (= 1 os)
   (progn
     (SETVAR "OSMODE" 177)
     (SETVAR "BLIPMODE" 1)
   )
   (progn
     (SETVAR "OSMODE" 0)
     (SETVAR "BLIPMODE" 0)
   )
 )
)
;------------khoi tao-------------------------------
(defun khoitao ()
 (alert "Xin ch�o m�ng c�c b�n ��n v�i ch��ng tr�nh th�ng k� th�p TTK Version 2.5, (c) 2003 Copyright b�i ��ng Xu�n L�i - C�ng ty x�y d�ng Th�nh Nam 21/199 ���ng Tr��ng Chinh - H� n�i")
 (alert "B�n h�y ch�n m�t �i�m trong b�ng th�ng k� c�t th�p �� b�t ��u nh�p d� li�u.")
  (setq i 0)
 (setq dlist (list 6 8 10 12 14 16 18 20 22 25 28 30 32 36 40) )
)
;;--------------------------------------------
(defun set_style ()
 ;(initget 4)
 (setq style (getint "\nKieu thep <Chon>: "))
 (if ( = style nil) (tk_lib))
 (if (not (member style (list 0 1 2 7 3 6 4 5 51 8 9 10 11) ))
  (progn
   (alert "Ki�u th�p b�n v�a ch�n kh�ng c� trong ch��ng tr�nh n�y, b�n ph�i ch�n s� t� 1 ��n 10 th� ch��ng tr�nh m�i ti�p t�c ho�t ��ng ���c.")
   (set_style)
  )
 )
)
;------------nhap so lieu---------------------------
(defun nhap ()
 ;;--------nhap duong kinh--------------
 (defun nhap_dk ()
  (initget 7)
  (setq dk (getint   "\nDuong kinh (mm): "))
  (if (not (member dk dlist))
   (progn
    (alert "���ng k�nh b�n v�a nh�p kh�ng ��ng, xin m�i b�n nh�p l�i.")
    (nhap_dk)
   )
   (if (and (or (= style 2) (= style 7) (= 51 style))
            (> dk 12))
    (progn
     (alert "���ng k�nh b�n v�a nh�p kh�ng ph� h�p v�i ki�u th�p b�n v�a ch�n, xin m�i b�n nh�p l�i.")
     (nhap_dk)
    )
   )
  )
 )
 ;;--------nhap chieu dai---------------
 (defun nhap_chieu_dai ()
  ;;;--------nhap1-----------
  (defun nhap1 ()
   (initget 7)
   (princ             "Chieu dai (mm):")
   (initget 7)
   (setq l2 (getreal "\n       l  = "))
  )
  ;;;--------nhap2-----------
  (defun nhap2 ()
   (princ             "Chieu dai (mm):")
   (initget 7)
   (setq l1 (getreal "\n       l1 = "))
   (initget 7)
   (setq l2 (getreal "\n       l2 = "))
  )
  ;;;--------nhap3-----------
  (defun nhap3 ()
               (princ "Chieu dai (mm):")
   (initget 7)
   (setq l1 (getreal "\n       l1 = "))
   (initget 7)
   (setq l2 (getreal "\n       l2 = "))
   (initget 7)
   (setq l3 (getreal "\n       l3 = "))
  )
  ;;;--------nhap4-----------
  (defun nhap4 ()
   (initget 7)
   (princ             "Chieu dai (mm):")
   (initget 7)
   (setq l2 (getreal "\n       R  = "))
   (initget 7) 
   (setq l3 (getreal "\n       rt  = "))
  )
  ;;;--------nhap10-----------
  (defun nhap10 ()
               (princ "Chieu dai (mm):")
   (initget 7)
   (setq l1 (getreal "\n       l1 = "))
   (initget 7)
   (setq l2 (getreal "\n       R1 = "))
   (initget 7)
   (setq l3 (getreal "\n       l2 = "))
  )
  ;;;------------------------
  (cond
       ( (or (= 0 style) (= 1 style) (= 2 style)) (nhap1) )
       ( (or (= 4 style) (= 51 style)) (nhap2) )
       ( (or (= 3 style) (= 6 style)(= 5 style)(= 8 style)(= 9 style)) (nhap3) )
       ( (= 7 style) (nhap4) )
       ( (= 10 style) (nhap10) )
  )
 )
 ;;--------nhap so luong----------------
 (defun nhap_sl (/ osl1 oslck)

  (if (null sl1 ) (setq sl1  1))
  (if (null slck) (setq slck 1))
  (setq osl1  sl1)
  (setq oslck slck)

  (initget 6)
  (setq sl1 (getint  (strcat "\nSo luong cho 1 c.k <" (itoa sl1) ">: ")))
  (if (null sl1) (setq sl1 osl1))

  (initget 6)
  (setq slck (getint (strcat "\nSo luong cau kien <" (itoa slck) ">: ")))
  (if (null slck) (setq slck oslck))
 )
 ;;-------------------------------------
 (setq p0 4.5) 				; Chieu cao o thong ke bang 9

 (if (= i 0)
  (progn
   (initget 1)
   (setq p1 (getpoint "\nDiem chen: "))
   (setq i 1)
  )
  (progn
   (setq p1 (list (nth 0 p1) (- (nth 1 p1) (* p0 2)) ))
  )
 )

 (setq p2 (polar p1 0 10)
       p3 (polar p2 0 50)
       p4 (polar p3 0 10)
       p5 (polar p4 0 20)
       p6 (polar p5 0 15)
       p7 (polar p6 0 15)
       p8 (polar p7 0 20)
       p9 (polar p8 0 20)
 )
 (princ               "\n---------------------")
 (setq sh (getstring  "\nSo hieu (0-Thoat): "))
 (if (/= sh "0")
  (progn
   (set_style)
   (nhap_dk)
   (nhap_chieu_dai)
   (nhap_sl)
  )
 )
)
;-------------------------------------------
;------------thong ke---------------------------
(defun thongke ( / dkinh dtich )
 ;;----------tinh chieu dai thanh--------------
 (defun tinh_chieu_dai ()
  ;;;---------tinh 1--------------------------
  (defun tinh1 ()
    (setq dai l2)
  )
  ;;;---------tinh 2--------------------------
  (defun tinh2 ()
   (cond
       ( (= 6 dk)  (setq l1 50) )
       ( (= 8 dk)  (setq l1 60) )
       ( (= 10 dk) (setq l1 80) )
       ( (= 12 dk) (setq l1 100) )
   )
   (setq l3 l1)
   (setq dai (+ l1 (+ l2 l3)) )
  )
  ;;;---------tinh 3--------------------------
  (defun tinh3 ()
   (setq dai (+ l1 (+ l2 l3)) )
  )
  ;;;---------tinh 4--------------------------
  (defun tinh4 ()
   (setq dai (+ l1 l2) )
  )
  ;;;---------tinh 5--------------------------
  (defun tinh5 ()
   (setq dai (* 2 (+ l1 (+ l2 l3))) )
  )
  ;;;---------tinh 51--------------------------
  (defun tinh51 ()
   (cond
       ( (= 6 dk)  (setq l3 50) )
       ( (= 8 dk)  (setq l3 60) )
       ( (= 10 dk) (setq l3 80) )
       ( (= 12 dk) (setq l3 100) )
   )
   (setq dai (* 2 (+ l1 (+ l2 l3))) )
  )
  ;;;---------tinh 7--------------------------
  (defun tinh7 ()
   (setq dai (+ (* (* 2 pi) l2) (* 2 l3)) )
   (setq chuvi (* (* 2 pi) l2))
  )
  ;;;---------tinh 10--------------------------
  (defun tinh10 ()
   (setq dai (+ (* pi l2) (* 2 (+ l1 l3))) )
   (setq nchuvi (* pi l2))
  ) 
  ;;;-----------------------------------------
  (cond
      ( (or (= 0 style) (= 1 style)) (tinh1) )
      ( (= 2 style) (tinh2) )
      ( (or (= 3 style) (= 6 style) (= 8 style) (= 9 style)) (tinh3) )
      ( (= 4 style) (tinh4) )
      ( (= 5 style) (tinh5) )
      ( (= 51 style) (tinh51) )
      ( (= 7 style) (tinh7) )
      ( (= 10 style) (tinh10) )
  )
 )
 ;;------------------------------------------
 (tinh_chieu_dai)
 (setq dkinh (* dk 0.001))
 (setq dtich (* (* pi (* dkinh dkinh)) 0.25) )
 (setq sla (* sl1 slck))
 (setq dt (* (* dai sla) 0.001) )
 (setq tl (* (* 7850 dtich) dt) )
)
;---------------ve thong ke---------------------
(defun ve ()
 ;;---------------text_left---------------------
 (defun left ( l_p txt)
  (command "TEXT" "ML" l_p "" "" txt)
 )
 ;;---------------text_center------------------
 (defun center ( c_p txt)
  (command "TEXT" "MC" c_p "" "" txt)
 )
 ;;---------------text_right-------------------
 (defun right ( r_p txt)
  (command "TEXT" "MR" r_p "" "" txt)
 )
 ;;---------------text0------------------------
 (defun text0 ( / txt_p)
  (setq txt_p (list (+ 5 (car p1)) (+ p0 (cadr p1)) ) )
  (center txt_p sh)
  (setq txt_p (list (+ 5 (car p3)) (+ p0 (cadr p3)) ) )
  (center txt_p (itoa dk))
  (setq txt_p (list (+ 15 (car p4)) (+ p0 (cadr p4)) ) )
  (right txt_p (rtos dai 2 0))
  (setq txt_p (list (+ 10 (car p5)) (+ p0 (cadr p5)) ) )
  (right txt_p (itoa sl1))
  (setq txt_p (list (+ 10 (car p6)) (+ p0 (cadr p6)) ) )
  (right txt_p (itoa sla))
  (setq txt_p (list (+ 15 (car p7)) (+ p0 (cadr p7)) ) )
  (right txt_p (rtos dt 2 1))
  (setq txt_p (list (+ 15 (car p8)) (+ p0 (cadr p8)) ) )
  (right txt_p (rtos tl 2 1))
 )
 ;;---------------text1------------------------
 (defun text1 ( / txt_p)
  (setq txt_p (list (+ 25 (car p2)) (+ p0 (cadr p2)) ) )
  (center txt_p (rtos dai 2 0))
 )
 ;;---------------text2------------------------
 (defun text2 ( / txt_p)
  (setq txt_p (list (+ 9 (car p2)) (+ p0 (cadr p2)) ) )
  (right txt_p (rtos l1 2 0))
  (setq txt_p (list (+ 25 (car p2)) (+ p0 (cadr p2)) ) )
  (center txt_p (rtos l2 2 0))
  (setq txt_p (list (+ 41 (car p2)) (+ p0 (cadr p2)) ) )
  (left txt_p (rtos l3 2 0))
 )
 ;;---------------text21------------------------
 (defun text21 ( / txt_p)
  (setq txt_p (list (+ 14 (car p2)) (+ p0 (cadr p2)) ) )
  (right txt_p (rtos l1 2 0))
  (setq txt_p (list (+ 25 (car p2)) (+ p0 (cadr p2)) ) )
  (center txt_p (rtos l2 2 0))
  (setq txt_p (list (+ 36 (car p2)) (+ p0 (cadr p2)) ) )
  (left txt_p (rtos l3 2 0))
 )
 ;;---------------text4------------------------
 (defun text4 ( / txt_p)
  (setq txt_p (list (+ 9 (car p2)) (+ p0 (cadr p2)) ) )
  (right txt_p (rtos l1 2 0))
  (setq txt_p (list (+ 25 (car p2)) (+ p0 (cadr p2)) ) )
  (center txt_p (rtos l2 2 0))
 )
 ;;---------------text5------------------------
 (defun text5 ( / txt_p)
  (setq txt_p (list (+ 14 (car p2)) (+ p0 (cadr p2)) ) )
  (right txt_p (rtos l1 2 0))
  (setq txt_p (list (+ 25 (car p2)) (+ p0 (cadr p2)) ) )
  (center txt_p (rtos l2 2 0))
  (setq txt_p (list (+ 36 (car p2)) (+ p0 (cadr p2)) ) )
  (left txt_p (rtos l3 2 0))
 )
 ;;---------------text7------------------------
 (defun text7 ( / txt_p)
  (setq txt_p (list (+ 15 (car p2)) (+ p0 (cadr p2)) ) )
  (center txt_p (rtos chuvi 2 0))
  (setq txt_p (list (+ 36 (car p2)) (+ p0 (cadr p2)) ) )
  (left txt_p (rtos l3 2 0))
 )
 ;;---------------text8------------------------
 (defun text8 ( / txt_p)
  (setq txt_p (list (+ 9 (car p2)) (+ p0 (cadr p2)) ) )
  (right txt_p (rtos l1 2 0))
  (setq txt_p (list (+ 30 (car p2)) (+ p0 (cadr p2)) ) )
  (center txt_p (rtos l2 2 0))
  (setq txt_p (list (+ 41 (car p2)) (+ p0 (cadr p2)) ) )
  (left txt_p (rtos l3 2 0))
 )
 ;;---------------text9------------------------
 (defun text9 ( / txt_p)
  (setq txt_p (list (+ 9 (car p2)) (+ p0 (cadr p2)) ) )
  (right txt_p (rtos l1 2 0))
  (setq txt_p (list (+ 26 (car p2)) (+ p0 (cadr p2)) ) )
  (center txt_p (rtos l2 2 0))
  (setq txt_p (list (+ 41 (car p2)) (+ p0 (cadr p2)) ) )
  (left txt_p (rtos l3 2 0))
 )
 ;;---------------text10------------------------
 (defun text10 ( / txt_p)
  (setq txt_p (list (+ 9 (car p2)) (+ p0 (cadr p2)) ) )
  (right txt_p (rtos l1 2 0))
  (setq txt_p (list (+ 25 (car p2)) (+ p0 (cadr p2)) ) )
  (center txt_p (rtos l2 2 0))
  (setq txt_p (list (+ 35 (car p2)) (+ p0 (cadr p2)) ) )
  (left txt_p (rtos l3 2 0))
 )
 ;;
 ;;---------------ve0-------------------------
 (defun ve0 ()
  (setvar "CLAYER" "TEXT")
  (text0)
 )
 ;;---------------ve1-------------------------
 (defun ve1 ( / pt1 pt2)
  (setq pt1 (list (+ 10 (car p2)) (+ 2 (cadr p2)) ) )
  (setq pt2 (polar pt1 0 30) )

  (setvar "CLAYER" "THEP")
  (command "PLINE" pt1 pt2 "")
  (setvar "CLAYER" "TEXT")
  (text1)
  (text0)
 )
 ;;---------------ve2-------------------------
 (defun ve2 ( / pt1 pt2 pt3 pt4 pt5 pt6)
  (setq pt1 (list (+ 10.75 (car p2)) (+ 2 (cadr p2)) ) )
  (setq pt2 (polar pt1 0 28.5) )
  (setq pt3 (polar pt1 (/ pi 2) 1.5) )
  (setq pt4 (polar pt2 (/ pi 2) 1.5) )
  (setq pt5 (polar pt3 0 2.5) )
  (setq pt6 (polar pt4 0 -2.5) )

  (setvar "CLAYER" "THEP")
  (command "PLINE" pt5 pt3 "A" pt1 "L" pt2 "A" pt4 "L" pt6 "")
  (setvar "CLAYER" "TEXT")
  (text2)
  (text0)
 )
 ;;---------------ve7-------------------------
 (defun ve7 ( / pt1 pt2 pt3 pt4 )
  (setq pt1 (list (+ 30 (car p2)) (+ 8.5 (cadr p2)) ) )
  (setq pt2 (polar pt1 (/ (* 3 pi) 2) 4) )
  (setq pt3 (polar pt2 (/ (* 2 pi) 2) 8) )
  (setq pt4 (polar pt2 (/ (* 3 pi) 2) 4) )

  (setvar "CLAYER" "THEP")
  (command "PLINE" pt1 pt2 "A" pt3 pt2 "L" pt4 "")
  (setvar "CLAYER" "TEXT")
  (text7)
  (text0)
 )
 ;;---------------ve8-------------------------
 (defun ve8 ( / pt1 pt2 pt3 pt4 )
  (setq pt1 (list (+ 10.5 (car p2)) (+ 8 (cadr p2)) ) )
  (setq pt2 (polar pt1 (/ (* 15 pi) 8) 18) )
  (setq pt3 (polar pt1 0 12) )
  (setq pt4 (polar pt2 0 12) )

  (setvar "CLAYER" "THEP")
  (command "PLINE" pt1 pt3 pt2 pt4 "")
  (setvar "CLAYER" "TEXT")
  (text8)
  (text0)
 )
 ;;---------------ve9-------------------------
 (defun ve9 ( / pt1 pt2 pt3 pt4 )
  (setq pt1 (list (+ 15 (car p2)) (+ 3.5 (cadr p2)) ) )
  (setq pt2 (list (+ 36 (car p2)) (+ 0.5 (cadr p2)) ) )
  (setq pt3 (polar pt1 (/ (* 5 pi) 2) 5) )
  (setq pt4 (polar pt2 (/ (* 5 pi) 2) 5) )

  (setvar "CLAYER" "THEP")
  (command "PLINE" pt4 "A" pt2 "L" pt3 "A" pt1 "")
  (setvar "CLAYER" "TEXT")
  (text9)
  (text0)
 )
 ;;---------------ve10-------------------------
 (defun ve10 ( / pt1 pt2 pt3 pt4 pt5 pt6 pt7)
  (setq pt1 (list (+ 40 (car p2)) (+ 0.5 (cadr p2)) ) )
  (setq pt2 (polar pt1 (/ (* 2 pi) 2) 10) )
  (setq pt3 (polar pt2 (/ (* 2 pi) 2) 10) )
  (setq pt4 (polar pt2 (/ (* 2 pi) 2) 20) )
  (setq pt5 (list (+ 25 (car p2)) (+ 8.5 (cadr p2)) ) )
  (setq pt6 (list (+ 30 (car p2)) (+ 4.5 (cadr p2)) ) )
  (setq pt7 (list (+ 20 (car p2)) (+ 4.5 (cadr p2)) ) )

  (setvar "CLAYER" "THEP")
  (command "PLINE" pt1 pt2 pt6 "A" "S" pt5 pt7 pt3 "L" pt4 "")
  (setvar "CLAYER" "TEXT")
  (text10)
  (text0)
 )
 ;;---------------ve3-------------------------
 (defun ve3 ( / pt1 pt2 pt3 pt4 )
  (setq pt1 (list (+ 10 (car p2)) (+ 2 (cadr p2)) ) )
  (setq pt2 (polar pt1 0 30) )
  (setq pt3 (polar pt1 (/ pi 2) 5) )
  (setq pt4 (polar pt2 (/ pi 2) 5) )

  (setvar "CLAYER" "THEP")
  (command "PLINE" pt1 pt3 pt4 pt2 "")
  (setvar "CLAYER" "TEXT")
  (text2)
  (text0)
 )
 ;;---------------ve6-------------------------
 (defun ve6 ( / pt1 pt2 pt3 pt4 )
  (setq pt1 (list (+ 10 (car p2)) (+ 2 (cadr p2)) ) )
  (setq pt2 (polar pt1 0 30) )
  (setq pt3 (polar pt1 (/ pi 4) 7) )
  (setq pt4 (polar pt2 (/ (* 3 pi) 4) 7) )

  (setvar "CLAYER" "THEP")
  (command "PLINE" pt1 pt3 pt4 pt2 "")
  (setvar "CLAYER" "TEXT")
  (text2)
  (text0)
 )
 ;;---------------ve4-------------------------
 (defun ve4 ( / pt1 pt3 pt4 )
  (setq pt1 (list (+ 10 (car p2)) (+ 2 (cadr p2)) ) )
  (setq pt3 (polar pt1 (/ pi 2) 5) )
  (setq pt4 (polar pt3 0 30) )

  (setvar "CLAYER" "THEP")
  (command "PLINE" pt1 pt3 pt4 "")
  (setvar "CLAYER" "TEXT")
  (text4)
  (text0)
 )
 ;;---------------ve5-------------------------
 (defun ve5 ( / pt1 pt2 pt3 pt5 pt6)
  (setq pt1 (list (+ 15 (car p2)) (+ 2 (cadr p2)) ) )
  (setq pt2 (polar pt1 0 20) )
  (setq pt3 (polar pt1 (/ pi 2) 5) )
  (setq pt5 (polar pt2 (/ pi 2) 7) )
  (setq pt6 (polar pt3 0 22) )

  (setvar "CLAYER" "THEP")
  (command "PLINE" pt6 pt3 pt1 pt2 pt5 "")
  (setvar "CLAYER" "TEXT")
  (text5)
  (text0)
 )
 ;;------------------------------------------
 (setvar "TEXTSTYLE" "ROMANS")
 (setvar "PLINEWID" 0.35)
 (cond
     ( (= 0 style) (ve0) )
     ( (= 1 style) (ve1) )
     ( (= 2 style) (ve2) )
     ( (= 7 style) (ve7) )
     ( (= 3 style) (ve3) )
     ( (= 6 style) (ve6) )
     ( (= 4 style) (ve4) )
     ( (= 5 style) (ve5) )
     ( (= 51 style) (ve5) )
     ( (= 8 style) (ve8) )
     ( (= 9 style) (ve9) )
     ( (= 10 style) (ve10) )
 )
)
;-------------------------------------------
;;;================ Main function  ============================================
  ;; Set up error function.
  (setq old_cmd (getvar "CMDECHO")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
;        old_osmode (getvar "OSMODE")  ; disable OSNAP for duration
  )

  (setvar "CMDECHO" 0)

  (khoitao)
  (while (/= sh "0")
   (set_osnap 1)
   (nhap)
   (if (/= sh "0")
    (progn
     (set_osnap 0)
     (thongke)
     (ve)
     (set_osnap 1)
    )
   )
  )
  (princ)

;  (setvar "OSMODE" old_osmode)
  (setq *error* old_error)
  (setvar "cmdecho" old_cmd)

  (princ)
)
;;; ================= End main function ==================
(princ "  TK loaded.")
(princ)
