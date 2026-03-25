;;; AutoLISP Type-file, Modify (C) by Do Manh Hien 44x5 Nam Truc-Nam Dinh
; Khi tinh Momen goi co ke den he so dieu chinh theo 2 phuong vx vy
;;;============================================================================
(defun ai_abort (app msg)
   (defun *error* (s)
      (if old_error (setq *error* old_error))
      (princ)  )
   (if msg (alert (strcat " Application error: "
                            app
                          " \n\n  "
                            msg
                          " \n" ) ) ) 
  (exit))
;;; Check if AI_UTILS is loaded.If not, try to find it,then try to load it.
;;; If it can't be found or can't be loaded, then abort the loading of this 
;;; file immediately, preserving the (autoload) stub function.

  (cond
     (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.
     (  (not (findfile "ai_utils.lsp"))       ; find it
        (ai_abort "SLAB"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))
     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "SLAB" "Can't load file AI_UTILS.LSP"))  )
  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "SLAB" nil))             ; a Nil <msg> supresses

;;;========================= The main routine ================================

(defun C:SLAB ( / 
    lx       ly       h         q        ra         rb      a
    bl       br       bt        bb       bh         bv      bc 
    mx       my       mt        mb       ml         mr
    fx       fy       ft        fb       fl         fr
    reset    draw     stress    start    SLAB_main  ho  
    getdata  solution   bpbp    tinhthep            hienthi 
    )
(defun reset()
    (set_tile "error" "") 
    )
(defun bp( num )  
    (ai_return (* num num))
    )
(defun bpbp( num )  
    (ai_return (* (* num num) (* num num)))
    )
(defun tinhthep (m / aa tg muy fa1)
    (setq aa (/ (* m 10000) (* 100 (* ho ho))))
    (setq tg (- 1 (/ (* 2 aa) rb)))
    (If (> tg 0)  
    (Progn 
        (setq muy (/ (* (- 1 (sqrt tg)) rb) ra))
        (setq fa1 (* muy (* 100 ho)))
        (ai_return fa1)
        )
    (Progn
        (set_tile "error" "The SLAB is too thin. Increase the thickness!")
        (ai_return nil)
        )
     ) 
    )
(defun hienthi( / ff)
    (set_tile "m1" (rtos mx))
    (setq ff (tinhthep (abs mx)))
    (if (/= ff nil) (set_tile "fa1" (rtos ff)) (set_tile "fa1" "")) 

    (set_tile "m2" (rtos my))
    (setq ff (tinhthep (abs my)))
    (if (/= ff nil) (set_tile "fa2" (rtos ff)) (set_tile "fa2" ""))

    (set_tile "ml" (rtos ml))
    (setq ff (tinhthep (abs ml)))
    (if (/= ff nil) (set_tile "fal" (rtos ff)) (set_tile "fal" ""))

    (set_tile "mr" (rtos mr))
    (setq ff (tinhthep (abs mr)))
    (if (/= ff nil) (set_tile "far" (rtos ff)) (set_tile "far" ""))

    (set_tile "mt" (rtos mt))
    (setq ff (tinhthep (abs mt)))
    (if (/= ff nil) (set_tile "fat" (rtos ff)) (set_tile "fat" ""))

    (set_tile "mb" (rtos mb))
    (setq ff (tinhthep (abs mb)))
    (if (/= ff nil) (set_tile "fab" (rtos ff)) (set_tile "fab" ""))
    )
(defun solution( 
  / lam      qx       qy
    xx       xy       vx        vy       ax       ay 
    px       py       pt        pb       pl       pr
    ts1      ts2      ts15      ts25     total
    )

; =================== Kiem tra DK bien cua phuong X
    (cond

; Khong co canh ngam theo phuong X
    ( (= bh 0) 
      (setq px (/ 1.0 8.0))   
      (setq ax (/ 5.0 384.0))
      (setq pl 0)
      (setq pr 0)
      )

; Co 1 canh ngam theo phuong X
    ( (= bh 1) 
      (setq px (/ 9.0 128.0)) 
      (setq ax (/ 1.0 192.0))
      (if (= bl 1) 
      (progn
         (setq pl (/ -1.0 8.0))
         (setq pr 0)
         )
      (progn 
         (setq pl 0)
         (setq pr (/ -1.0 8.0))
      )))

; Co 2 canh ngam theo phuong X
    ( (= bh 2) 
      (setq px (/ 1.0 24.0))  
      (setq ax (/ 1.0 384.0))
      (setq pl (/ -1.0 12.0)) 
      (setq pr (/ -1.0 12.0))
      )
    ( T  (set_tile "error" "Data error") )
    )

;================== Kiem tra DK bien cua phuong Y
    (cond

; Khong co canh ngam theo phuong Y
    ( (= bv 0) 
      (setq py (/ 1.0 8.0)) 
      (setq ay (/ 5.0 384.0))
      (setq pt 0) 
      (setq pb 0)
      )

; Co 1 canh ngam theo phuong Y
    ( (= bv 1) 
      (setq py (/ 9.0 128.0))
      (setq ay (/ 1.0 192.0))
      (if (= bt 1) 
        (progn 
           (setq pt (/ -1.0 8.0))
           (setq pb 0)
           )
        (progn  
           (setq pt 0)
           (setq pb (/ -1.0 8.0))
       ) ) )

; Co 2 canh ngam theo phuong Y
     ( (= bv 2) 
       (setq py (/ 1.0 24.0))
       (setq ay (/ 1.0 384.0))
       (setq pt (/ -1.0 12.0))
       (setq pb (/ -1.0 12.0))
       )
     ( T (set_tile "error" "Data error")) 
     )

     (setq lam (/ ly lx))
     (setq ts1  (/ (bp lam) (+ 1.0 (bpbp lam))))
     (setq ts2  (/ (bp lam) (+ 1.0 (* 2.0 (bpbp lam)))))
     (setq ts15 (/ (bp lam) (+ 1.0 (* 5.0 (bpbp lam)))))
     (setq ts25 (/ (bp lam) (+ 2.0 (* 5.0 (bpbp lam)))))

; =================== Xac dinh so do tinh ban san
     (cond 

; Ban khong co canh ngam 
     ( (= bc 0)
       (setq vx (- 1.0 (/ (* 5.0 ts1) 6.0)))
       (setq vy vx)
       )

; Ban co 1 canh ngam 
     ( (= bc 1)
       (if (= bh 1)
       (progn  
          (setq vx (- 1.0 (/ (* 75.0 ts25) 32.0)))
          (setq vy (- 1.0 (/ (* 5.0 ts25) 3.0)))
          )
       (progn  
          (setq vy (- 1.0 (/ (* 75.0 ts25) 32.0)))
          (setq vx (- 1.0 (/ (* 5.0 ts25) 3.0)))
       ) ) )

; Ban co 2 canh ngam 
     ( (= bc 2)
       (if (= bh 0) 
       (progn  
          (setq vx (- 1.0 (/ (* 5.0 ts15) 6.0)))
          (setq vy (- 1.0 (/ (* 25.0 ts15) 18.0)))
       ) )
       (if (= bh 1)
       (progn  
          (setq vy (- 1.0 (/ (* 15.0 ts1) 32.0)))
          (setq vx vy)
       ) )
       (if (= bh 2)
       (progn  
          (setq vy (- 1.0 (/ (* 5.0 ts15) 6.0)))
          (setq vx (- 1.0 (/ (* 25.0 ts15) 18.0)))
       ) ) )          

; Ban co 3 canh ngam 
     ( (= bc 3)
       (if (= bh 1) 
       (progn  
          (setq vx (- 1.0 (/ (* 15.0 ts2) 32.0)))
          (setq vy (- 1.0 (/ (* 5.0 ts2) 9.0)))
       ) ) 
       (if (= bh 2) 
       (progn  
          (setq vx (- 1.0 (/ (* 5.0 ts2) 9.0)))
          (setq vy (- 1.0 (/ (* 15.0 ts2) 32.0)))
       ) ) )          

; Ban co 4 canh ngam 
     ( (= bc 4)
       (setq vx (- 1.0 (/ (* 5.0 ts1) 18.0)))
       (setq vy vx)
       ) 
     ( T )
     )           

;==================== Tinh toan noi luc
    (setq total (+ (* ax (bpbp lx)) (* ay (bpbp ly))))
    (setq xx (/ (* ay (bpbp ly)) total))             
    (setq xy (/ (* ax (bpbp lx)) total))             
    (setq qx (* (* xx q) (* lx lx)))
    (setq qy (* (* xy q) (* ly ly)))
    (setq mx (* (* px vx) qx))
    (setq my (* (* py vy) qy))
    (setq ml (* (* pl vx) qx))
    (setq mr (* (* pr vx) qx))
    (setq mt (* (* pt vy) qy))
    (setq mb (* (* pb vy) qy))
    (set_tile "error" (strcat "Mx=" (rtos mx) "  My=" (rtos my)))
    (hienthi)
    )
(defun getdata()
    (setq lx (abs lx))
    (setq ly (abs ly))
    (setq h (abs h))
    (setq q (abs q))
    (setq ra (abs ra))
    (setq rb (abs rb))
    (setq a (abs a))
    (setq ho (- h a))
    (setq bh (+ bl br))
    (setq bv (+ bt bb))
    (setq bc (+ bv bh))
    (solution)
    )  
(defun start()
    (set_tile "Left" "0")
    (set_tile "Right" "0")
    (set_tile "Top" "0")
    (set_tile "Bottom" "0")
    (set_tile "ra" "2100")
    (set_tile "rb" "90")
    (set_tile "a" "3")
    )
(defun stress ( )
    (setq lx (ai_num (get_tile "l1") "Must be a numeric value" 0))
    (setq ly (ai_num (get_tile "l2") "Must be a numeric value" 0))
    (setq h  (ai_num (get_tile "h") "Must be a numeric value" 0))
    (setq q  (ai_num (get_tile "q") "Must be a numeric value" 0))
    (setq ra  (ai_num (get_tile "ra") "Must be a numeric value" 0))
    (setq rb  (ai_num (get_tile "rb") "Must be a numeric value" 0))
    (setq a  (ai_num (get_tile "a") "Must be a numeric value" 0))
    (setq bl (atoi (get_tile "left")))
    (setq br (atoi (get_tile "right")))
    (setq bt (atoi (get_tile "top")))
    (setq bb (atoi (get_tile "bottom")))
    (if (= (get_tile "error") "") (getdata))
    )
(defun draw () 
    (set_tile "error" "draw SLAB")
    )
(defun SLAB_main()
    (if (not (new_dialog "SLAB" dcl_id)) (exit))
    (start)
    (action_tile "stress" "(reset) (stress)")
    (action_tile "draw" "(draw)")
    (action_tile "close" "(done_dialog)")
    (start_dialog)
    )
;====================== Set up error function ================================

   (setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
         old_error  *error*            ; save current error function
         *error* ai_error)             ; new error function
   (setvar "cmdecho" 0)
   (cond
   ( (not (ai_transd)))                        ; transparent OK
   ( (not (ai_acadapp)))                       ; ACADAPP.EXP xloaded?
   ( (not (setq dcl_id (ai_dcl "SLAB"))))       ; is .DCL file loaded?
   (t (if (and (/= 1 (logand 1 (getvar "cmdactive")))
               (/= 8 (logand 8 (getvar "cmdactive"))))
               (ai_undo_push) )
      (SLAB_main)                       ; proceed!
      (if (and (/= 1 (logand 1 (getvar "cmdactive")))
               (/= 8 (logand 8 (getvar "cmdactive"))))
               (ai_undo_pop) ) ) )
   (setq *error* old_error) 
   (setvar "cmdecho" old_cmd)
   (princ))
;;;============================================================================
(princ "  SLAB loaded.\nType SLAB to start command.")
(princ)
