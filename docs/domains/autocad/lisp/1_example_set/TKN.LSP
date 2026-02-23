;;;
;;; TKN.LSP Version 3.1, Copyright (C) 1996-1998 by Pham Hoang Anh, HUCE.
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
        (ai_abort "tkn"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "tkn" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "tkn" nil)         ; a Nil <msg> supresses
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
;;; ===================== Modify Function ============================
(defun C:TK_MOD ( / mode style
                    ssent object data  i ii  att attdata code
                    l1   l2      l3
                    sh   style   dk   dai   sl1   slck   sla  dt    tl
                    nl1   nl2    nl3
                    nsh   ndk    nsl1   nslck
                )
 (defun select ()
  (setq ssent (ssget))
  (if ssent (setq mode (sslength ssent)) )
 )
 ;; --------------------------------------------------
 (defun start ()

  (setq style nil)
  (setq sh "")
  (setq dk "")
  (setq l1 "")
  (setq l2 "")
  (setq l3 "")
  (setq sl1 "")
  (setq slck "")

  (if (= mode 1)
   (progn
    (setq object (ssname ssent 0))
    (setq data (entget object))
    (if (= "INSERT" (cdr (assoc 0 data)) )
     (progn
        (setq style (cdr (assoc 2 (entget object))))
        (setq style (strcase style))
     )
    )
    (if (member style (list "TK_1" "TK_2" "TK_3" "TK_4"
                            "TK_5" "TK_6" "TK_7" "TK_8"
                            "TK_9"
                      ))
     (progn
      (setq ii 1)
      (setq att (entnext object))
      (while ( and att (<= ii 10))
       (setq attdata (entget att))
       (if (= "ATTRIB" (cdr (assoc 0 attdata)) )
        (progn
         (setq code (cdr (assoc 2 attdata)))
         (cond
          ( (= "SH" code)  (setq sh  (cdr (assoc 1 attdata))) )
          ( (= "DK" code)  (setq dk  (cdr (assoc 1 attdata))) )
          ( (= "L1" code)  (setq l1  (cdr (assoc 1 attdata))) )
          ( (= "L2" code)  (setq l2  (cdr (assoc 1 attdata))) )
          ( (= "L3" code)  (setq l3  (cdr (assoc 1 attdata))) )
          ( (= "DAI" code) (setq dai (cdr (assoc 1 attdata))) )
          ( (= "SL1" code) (setq sl1 (cdr (assoc 1 attdata))) )
          ( (= "SLA" code) (setq sla (cdr (assoc 1 attdata))) )
         );cond
         (setq ii (+ ii 1))
        );progn
       );if
       (setq att (entnext att))
      );while

      (setq slck (rtos (/ (atof sla) (atof sl1)) 2 0) )

      (set_tile "sh"  sh)
      (set_tile "dk"  dk)
      (set_tile "l1"  l1)
      (set_tile "l2"  l2)
      (set_tile "l3"  l3)
      (set_tile "sl1"  sl1)
      (set_tile "slck" slck)

      (cond
       ( (or (= style "TK_1") (= style "TK_2") (= style "TK_3")
             (= style "TK_7") (= style "TK_9") )
         (if (= style "TK_1") (set_tile "l2"  dai))
         (mode_tile "l1" 1)
         (mode_tile "l3" 1)
       )
       ( (or (= style "TK_5") (= style "TK_6"))
         (mode_tile "l3" 1)
       )
      )

     );progn
    );if
   );progn
  );if
  (if (> mode 1)
   (progn
    (mode_tile "sh" 1)
    (mode_tile "l1" 1)
    (mode_tile "l2" 1)
    (mode_tile "l3" 1)
   )
  )
 )
 ;; --------------------------------------------------
 (defun clear ()
      (set_tile "sh"  "")
      (set_tile "dk"  "")
      (set_tile "l1"  "")
      (set_tile "l2"  "")
      (set_tile "l3"  "")
      (set_tile "sl1"  "")
      (set_tile "slck" "")
 )
 ;; --------------------------------------------------
 (defun olddata ()
    (if (member style (list "TK_1" "TK_2" "TK_3" "TK_4"
                            "TK_5" "TK_6" "TK_7" "TK_8"
                            "TK_9"
                      ))
     (progn
      (setq ii 1)
      (setq att (entnext object))
      (while ( and att (<= ii 10))
       (setq attdata (entget att))
       (if (= "ATTRIB" (cdr (assoc 0 attdata)) )
        (progn
         (setq code (cdr (assoc 2 attdata)))
         (cond
          ( (= "SH" code)  (setq sh  (cdr (assoc 1 attdata))) )
          ( (= "DK" code)  (setq dk  (cdr (assoc 1 attdata))) )
          ( (= "L1" code)  (setq l1  (cdr (assoc 1 attdata))) )
          ( (= "L2" code)  (setq l2  (cdr (assoc 1 attdata))) )
          ( (= "L3" code)  (setq l3  (cdr (assoc 1 attdata))) )
          ( (= "DAI" code) (setq dai  (cdr (assoc 1 attdata))) )
          ( (= "SL1" code) (setq sl1 (cdr (assoc 1 attdata))) )
          ( (= "SLA" code) (setq sla (cdr (assoc 1 attdata))) )
         );cond
         (setq ii (+ ii 1))
        );progn
       );if
       
       (setq att (entnext att))
      );while

      (if (= style "TK_1") (setq l2  dai))
      (setq slck (rtos (/ (atof sla) (atof sl1)) 2 0) )

      (if nsh   (setq sh   nsh)   (setq  sh sh) )
      (if ndk   (setq dk   ndk)   (setq  dk (distof dk)) )
      (if nl1   (setq l1   nl1)   (setq  l1 (distof l1)) )
      (if nl2   (setq l2   nl2)   (setq  l2 (distof l2)) )
      (if nl3   (setq l3   nl3)   (setq  l3 (distof l3)) )
      (if nsl1  (setq sl1  nsl1)  (setq  sl1 (distof sl1)) )
      (if nslck (setq slck nslck) (setq  slck (distof slck)) )

     );progn
    );if
 )
 ;; --------------------------------------------------
 (defun newdata ()
   (set_tile "error" "")
   (setq nsh nil)
   (setq ndk nil)
   (setq nl1 nil)
   (setq nl2 nil)
   (setq nl3 nil)
   (setq nsl1 nil)
   (setq nslck nil)

   (if (/= (get_tile "sh") "")
       (setq nsh (get_tile "sh"))
   )
   (if (/= (get_tile "dk") "")
      (setq ndk (ai_num (get_tile "dk") "So lieu sai. Vao lai." 6))
   )
   (if (/= (get_tile "l1") "")
      (setq nl1 (ai_num (get_tile "l1") "So lieu sai. Vao lai." 6))
   )
   (if (/= (get_tile "l2") "")
      (setq nl2 (ai_num (get_tile "l2") "So lieu sai. Vao lai." 6))
   )
   (if (/= (get_tile "l3") "")
      (setq nl3 (ai_num (get_tile "l3") "So lieu sai. Vao lai." 6))
   )
   (if (/= (get_tile "sl1") "")
      (setq nsl1 (ai_num (get_tile "sl1") "So lieu sai. Vao lai." 6))
   )
   (if (/= (get_tile "slck") "")
      (setq nslck (ai_num (get_tile "slck") "So lieu sai. Vao lai." 6))
   )

   ; Kiem tra duong kinh
   (if (numberp ndk)
    (if (not (member ndk (list 6 8 10 12 14 16 18 20 22 25 28 30 32 36 40)) )
     (set_tile "error" "Duong kinh khong dung.")
    )
   )

   (if (= (get_tile "error") "") (done_dialog 1))
 )
 ;; --------------------------------------------------
 (defun modify ()
  ; --------------------------------------------------
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
     (cond
       ( (= 6 dk)  (setq l3 50) )
       ( (= 8 dk)  (setq l3 60) )
       ( (= 10 dk) (setq l3 80) )
       ( (= 12 dk) (setq l3 100) )
     )
     (setq dai (* 2 (+ l1 (+ l2 l3))) )
    )
    ;;;---------tinh 8--------------------------
    (defun tinh8 ()
     (cond
       ( (= 6 dk)  (setq l1 50) )
       ( (= 8 dk)  (setq l1 60) )
       ( (= 10 dk) (setq l1 80) )
       ( (= 12 dk) (setq l1 100) )
     )
     (setq l3 l1)
     (setq dai (+ (* pi l2) l1 l3))
    )
    ;;;-----------------------------------------
    (cond
      ( (or (= "TK_1" style) (= "TK_2" style)) (tinh1) )
      ( (or (= "TK_3" style) (= "TK_7" style)) (tinh2) )
      ( (or (= "TK_4" style) (= "TK_8" style)) (tinh3) )
      ( (= "TK_5" style) (tinh4) )
      ( (= "TK_6" style) (tinh5) )
      ( (= "TK_9" style) (tinh8) )
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
  (defun mod ()
   (setq ii 1)
   (setq att (entnext object))
   (while ( and att (<= ii 10))
    (setq attdata (entget att))
    (if (= "ATTRIB" (cdr (assoc 0 attdata)) )
     (progn
      (setq code (cdr (assoc 2 attdata)))
      (cond
       ( (= "SH" code)
         (setq attdata (subst (cons 1 sh) (assoc 1 attdata) attdata))
       )
       ( (= "L1" code)
         (if (and l1 (/= style "TK_9"))
          (setq attdata (subst (cons 1 (rtos l1 2 0)) (assoc 1 attdata) attdata))
          (setq attdata (subst (cons 1 "")            (assoc 1 attdata) attdata))
         )
       )
       ( (= "L2" code)
         (if (/= style "TK_1")
          (setq attdata (subst (cons 1 (rtos l2 2 0)) (assoc 1 attdata) attdata))
          (setq attdata (subst (cons 1 "")            (assoc 1 attdata) attdata))
         )
       )
       ( (= "L3" code)
         (if (and l3 (/= style "TK_9"))
          (setq attdata (subst (cons 1 (rtos l3 2 0)) (assoc 1 attdata) attdata))
          (setq attdata (subst (cons 1 "")            (assoc 1 attdata) attdata))
         )
       )
       ( (= "DK" code)
         (setq attdata (subst (cons 1 (rtos dk 2 0)) (assoc 1 attdata) attdata))
       )
       ( (= "DAI" code)
         (setq attdata (subst (cons 1 (rtos dai 2 0)) (assoc 1 attdata) attdata))
       )
       ( (= "SL1" code)
         (setq attdata (subst (cons 1 (rtos sl1 2 0)) (assoc 1 attdata) attdata))
       )
       ( (= "SLA" code)
         (setq attdata (subst (cons 1 (rtos sla 2 0)) (assoc 1 attdata) attdata))
       )
       ( (= "DT" code)
         (setq attdata (subst (cons 1 (rtos dt 2 1)) (assoc 1 attdata) attdata))
       )
       ( (= "TL" code)
         (setq attdata (subst (cons 1 (rtos tl 2 1)) (assoc 1 attdata) attdata))
       )
      );cond

      ; Cap nhat so lieu rieng le
      (entmod attdata)

      (setq ii (+ ii 1))
     );progn
    );if
    
    (setq att (entnext att))
   );while
   ; Cap nhat toan bo so lieu
   (entmod data)
  )
  ; ---------------------------------------------------
  (setq i 0)
  (repeat mode
   (setq style nil)

   (setq object (ssname ssent i))
   (setq data (entget object))
   (if (= "INSERT" (cdr (assoc 0 data)) )
    (progn
     (setq style (cdr (assoc 2 (entget object))))
     (setq style (strcase style))
    )
   )
   (if (member style (list "TK_1" "TK_2" "TK_3" "TK_4"
                           "TK_5" "TK_6" "TK_7" "TK_8"
                           "TK_9"
                      ))
    (progn
     (olddata)
     (thongke)
     (mod)
    );progn
   );if
   (setq i (+ i 1))
  );repeat
 )
 ;; --------------------------------------------------
 (defun main ()
  (if (not (new_dialog "tk_mod" dcl_id))
    (exit)
  )
  (start)

  (action_tile "nhan" "(newdata)")
  (action_tile "huy" "(clear)")
  (action_tile "thoat" "(done_dialog)")
  (if (= (start_dialog) 1) (modify))
 )
 ;; ==================================================
 (setq  old_cmd (getvar "CMDECHO")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
  )
 (setvar "CMDECHO" 0)
 ;; --------------------------------------------------
 (select)
 (if mode
 (cond
     (  (not (ai_notrans)))                     ; transparent not OK
     (  (not (ai_acadapp)))                     ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "tk"))))  ; is .DCL file loaded?
     (T (ai_undo_push)
        (main)
        (ai_undo_pop)
     )
 )
 )
 ;; --------------------------------------------------
 (setq *error* old_error)
 (setvar "CMDECHO" old_cmd)
 (princ)
)
;;; ===================== TKN Function ============================
(defun C:TKN ( / old_cmd old_osmode i  dlist
             l1     l2         l3
             sh   osh   style   dk  odk   dai   sl1   slck   sla    dt     tl
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
 (setq no_tk 9)
 (setq tk_next 1)
 (setq tk_old tk_next)
 (setq tk_no "1")
 (setq tk_currtile "tk_1")
)

(defun grey (/ j)
 (if (= 1 tk_next)
     (progn
      (mode_tile "previous" 1)
      (if (> no_tk (* tk_next 8)) (mode_tile "next" 0) (mode_tile "next" 1) )
     )
     (progn
      (mode_tile "previous" 0)
      (if (> no_tk (* tk_next 8)) (mode_tile "next" 0) (mode_tile "next" 1) )
     )
 )
)

(defun ai_tk_start ()
  (setq i 0)
  (repeat 8
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
      (strcat "tk_lib(tk_" (itoa (+ i (* (- tk_next 1) 8))) ")")
    )
    (end_image)
  )
  (grey)
)

(defun next_tk ()
 (if (> no_tk (* 8 tk_next))
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
 (setq style (+ currtile (* (- tk_next 1) 8)))
 (setq style (- style 1))
 (cond
  ( (= style 6) (setq style 21))
  ( (= style 7) (setq style 31))
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
     (  (not (setq dcl_id (ai_dcl "tk"))))      ; is .DCL file loaded?
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
 (princ "\nTK Version 3.1, (c) 1996-1998 by Pham Hoang Anh, HUCE.")
 (setq i 0)
 (setq dlist (list 6 8 10 12 14 16 18 20 22 25 28 30 32 36 40) )


)
;;--------------------------------------------
(defun set_style ()
 ;(initget 4)
 (setq style (getint "\nKieu thep <Chon>: "))
 (if ( = style nil) (tk_lib))
 (if (not (member style (list 0 1 2 21 3 31 4 5 8) ))
  (progn
   (princ "\nKieu thep khong dung.")
   (set_style)
  )
 )
)
;------------nhap so lieu---------------------------
(defun nhap ()
 ;;--------nhap duong kinh--------------
 (defun nhap_dk ()
  (if (null dk) (setq dk 0))
  (setq odk dk)

  (initget 6)
  (setq dk (getint   (strcat "\nDuong kinh (mm) <" (itoa dk) ">: ")))
  (if (null dk) (setq dk odk))

  (if (not (member dk dlist))
   (progn
    (princ "\nDuong kinh khong dung.")
    (nhap_dk)
   )
   (if (and (or (= style 2) (= style 21) (= style 5))
            (> dk 12))
    (progn
     (princ "\nDuong kinh khong phu hop.")
     (nhap_dk)
    )
   )
  )
 )
 ;;--------nhap chieu dai---------------
 (defun nhap_chieu_dai ()
  ;;;--------nhap1-----------
  (defun nhap1 ()
   (setq l1 nil)
   (initget 7)
   (cond
    ((/= style 8) (setq l2 (getreal "\n       L  = ")))
    (( = style 8) (setq l2 (getreal "\n       D  = ")))
   )
   (setq l3 nil)
  )
  ;;;--------nhap2-----------
  (defun nhap2 ()
   (initget 7)
   (setq l1 (getreal "\n       L1 = "))
   (initget 7)
   (setq l2 (getreal "\n       L2 = "))
   (setq l3 nil)
  )
  ;;;--------nhap3-----------
  (defun nhap3 ()
   (initget 7)
   (setq l1 (getreal "\n       L1 = "))
   (initget 7)
   (setq l2 (getreal "\n       L2 = "))
   (initget 7)
   (setq l3 (getreal "\n       L3 = "))
  )
  ;;;------------------------
  (princ             "Chieu dai (mm):")
  (cond
       ( (or (= 0 style) (= 1 style) (= 2 style) (= 21 style)
             (= 8 style)) (nhap1) )
       ( (or (= 4 style) (= 5 style)) (nhap2) )
       ( (or (= 3 style) (= 31 style)) (nhap3) )
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
 (if (= osh nil) (setq osh 0))

 (princ               "\n---------------------")
 (setq sh (getstring  (strcat "\nSo hieu (0-Thoat) <" (rtos (+ osh 1) 2 0) ">: ")))
 (if (or (= sh "") (= sh "Enter")) (setq sh (rtos (+ osh 1) 2 0)) )
 
 (if (/= (distof sh) nil) (setq osh (distof sh)) )

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
   (cond
       ( (= 6 dk)  (setq l3 50) )
       ( (= 8 dk)  (setq l3 60) )
       ( (= 10 dk) (setq l3 80) )
       ( (= 12 dk) (setq l3 100) )
   )
   (setq dai (* 2 (+ l1 (+ l2 l3))) )
  )
  ;;;---------tinh 8--------------------------
  (defun tinh8 ()
   (cond
       ( (= 6 dk)  (setq l1 50) )
       ( (= 8 dk)  (setq l1 60) )
       ( (= 10 dk) (setq l1 80) )
       ( (= 12 dk) (setq l1 100) )
   )
   (setq l3 l1)
   (setq dai (+ (* pi l2) l1 l3))
  )
  ;;;-----------------------------------------
  (cond
      ( (or (= 0 style) (= 1 style)) (tinh1) )
      ( (or (= 2 style) (= 21 style)) (tinh2) )
      ( (or (= 3 style) (= 31 style)) (tinh3) )
      ( (= 4 style) (tinh4) )
      ( (= 5 style) (tinh5) )
      ( (= 8 style) (tinh8) )
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
(defun ve (/ name object data att attdata code)

 ;;------------------------------------------
 (cond
     ( (= 0 style)  (setq name "tk_1") )
     ( (= 1 style)  (setq name "tk_2") )
     ( (= 2 style)  (setq name "tk_3") )
     ( (= 3 style)  (setq name "tk_4") )
     ( (= 4 style)  (setq name "tk_5") )
     ( (= 5 style)  (setq name "tk_6") )
     ( (= 21 style) (setq name "tk_7") )
     ( (= 31 style) (setq name "tk_8") )
     ( (= 8 style)  (setq name "tk_9") )
 )
 (command "INSERT" name p1 "" "" "" "" "" "" "" "" "" "" "" "" "")

 ;;------------------------------------------
 (setq object (entlast))
 (setq data (entget object))
 (setq att (entnext object))

 (while att
  (setq attdata (entget att))
  (if (= "ATTRIB" (cdr (assoc 0 attdata)) )
   (progn
    (setq code (cdr (assoc 2 attdata)))
    (cond
     ( (= "SH" code)
       (setq attdata (subst (cons 1 sh) (assoc 1 attdata) attdata))
     )
     ( (= "L1" code)
       (if (and l1 (/= style 8))
        (setq attdata (subst (cons 1 (rtos l1 2 0)) (assoc 1 attdata) attdata))
        (setq attdata (subst (cons 1 "")            (assoc 1 attdata) attdata))
       )
     )
     ( (= "L2" code)
       (if (/= style 0)
        (setq attdata (subst (cons 1 (rtos l2 2 0)) (assoc 1 attdata) attdata))
        (setq attdata (subst (cons 1 "")            (assoc 1 attdata) attdata))
       )
     )
     ( (= "L3" code)
       (if (and l3 (/= style 8))
        (setq attdata (subst (cons 1 (rtos l3 2 0)) (assoc 1 attdata) attdata))
        (setq attdata (subst (cons 1 "")            (assoc 1 attdata) attdata))
       )
     )
     ( (= "DK" code)
       (setq attdata (subst (cons 1 (itoa dk)) (assoc 1 attdata) attdata))
     )
     ( (= "DAI" code)
       (setq attdata (subst (cons 1 (rtos dai 2 0)) (assoc 1 attdata) attdata))
     )
     ( (= "SL1" code)
       (setq attdata (subst (cons 1 (itoa sl1)) (assoc 1 attdata) attdata))
     )
     ( (= "SLA" code)
       (setq attdata (subst (cons 1 (itoa sla)) (assoc 1 attdata) attdata))
     )
     ( (= "DT" code)
       (setq attdata (subst (cons 1 (rtos dt 2 1)) (assoc 1 attdata) attdata))
     )
     ( (= "TL" code)
       (setq attdata (subst (cons 1 (rtos tl 2 1)) (assoc 1 attdata) attdata))
     )
    );cond

    ; Cap nhat so lieu rieng le
    (entmod attdata)

   );progn
  );if

  (setq att (entnext att))
 );while

 ; Cap nhat toan bo so lieu
 (entmod data)
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
(princ "  TKN loaded.")
(princ)
