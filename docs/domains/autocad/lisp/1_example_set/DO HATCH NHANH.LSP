(defun ai_undo_on ()
  (setq undo_setting (getvar "undoctl"))
  (cond
    ((= 2 (logand undo_setting 2))     ; Undo is one
      (command "_.undo" "_control" "_all" "_.undo" "_auto" "_off")
    )
    ((/= 1 (logand undo_setting 1))    ; Undo is disabled
      (command "_.undo" "_all" "_.undo" "_auto" "_off")
    )
  )
)

;;;f
;;; Return UNDO to the initial setting.  Do not use with new routines as they 
;;; should be designed to operate with any UNDO setting.
;;;
(defun ai_undo_off ()
  (cond 
    ((/= 1 (logand undo_setting 1))
      (command "_.undo" "_control" "_none")
    )
    ((= 2 (logand undo_setting 2))
      (command "_.undo" "_control" "_one")
    )
  )
)

(defun ai_undo_push()
  (setq undo_init (getvar "undoctl"))
  (cond
    ((and (= 1 (logand undo_init 1))   ; enabled
          (/= 2 (logand undo_init 2))  ; not ONE (ie ALL is ON)
          (/= 8 (logand undo_init 8))   ; no GROUP active
     )
      (command "_.undo" "_group")
    )
    (T)
  )  
  ;; If Auto is ON, turn it off.
  (if (= 4 (logand 4 undo_init))
      (command "_.undo" "_auto" "_off")
  )
)

;;;
;;; Add an END to UNDO and return to initial state.
;;;
(defun ai_undo_pop()
  (cond 
    ((and (= 1 (logand undo_init 1))   ; enabled
          (/= 2 (logand undo_init 2))  ; not ONE (ie ALL is ON)
          (/= 8 (logand undo_init 8))   ; no GROUP active
     )
      (command "_.undo" "_end")
    )
    (T)
  )  
  ;; If it has been forced off, turn it back on.
  (if (= 4 (logand undo_init 4))
    (command "_.undo" "_auto" "_on")
  )  
)

;===== AUTO HATCH (hh) ========

(defun mkhatch(v_hatchtp v_scale v_angle data_m / i)
	(command "hatch" v_hatchtp v_scale v_angle)
	(setq i 0)
	(while (< i (length data_m)) (progn
		(command (nth i data_m))
		(setq i (+ i 1))
	))
	(command "")  
)

(defun c:hh(/ data_m check)

  
(defun ah_import(/ p1 p2 old1 ent1 ent2 axa)
  (if (= nil hscale_d) (setq hscale_d 1))  
  (setq old1 (getvar "osmode") check 1)
  (setvar "osmode" 0)
  (setq p1 '(0 0 0) p2 p1)
  (command "line"  p1 p2 "")
  
  (setq data_m '())
  (setq ent1 (entlast) ent2 ent1)
  
  (princ ent1)
  
  (setvar "osmode" old1)
  
  (command "boundary")
  (setq p1 (getpoint))
  (while (not (= nil p1)) (progn
	
	(command p1)
	
	
	(setq p1 (getpoint))
	
  ))
  (command "")
  
  (setq ent1 (entnext ent1))
  
  (princ ent1)
  (if (= nil ent1) (setq check 0) (progn
	(while (not (= nil ent1)) (progn
		(setq data_m (append data_m (list ent1)))
		(setq ent1 (entnext ent1))
	))
  ))
  (command "erase" ent2 "")
  (princ)
)

(defun ah_procced(/ i s1)
  
  (if (= 0 check) (princ "\ninvalid data") (progn
	(initget 1 "D WALL W CONCRETE C GROUND G FLOOR F WC S SAND")
  	(setq s1 (getkword "\nD/Wall/Concrete/Ground/Floor/WC/Sand : "))
	(if (not (= nil s1)) (progn
		(cond
		  ((= "D" (strcase s1)) (mkhatch "solid" (* 1500 hscale_d) 0 data_m))
		  ((= "WC" (strcase s1)) (mkhatch "ansi37" (* 1500 hscale_d) 45 data_m))
		  ((or (= "W" (strcase s1)) (= "WALL" (strcase s1))) (mkhatch "ansi31" (* 500 hscale_d) 0 data_m))
		  ((or (= "C" (strcase s1)) (= "CONCRETE" (strcase s1))) (progn
				(mkhatch "ansi31" (* 200 hscale_d) 0 data_m)
    			(mkhatch "ar-conc" (* 20 hscale_d) 0 data_m)
																	   
		  ))
		  ((or (= "S" (strcase s1)) (= "SAND" (strcase s1))) (progn
				(mkhatch "ar-sand" (* 80 hscale_d) 0 data_m)
    			
																	   
		  ))
		  
		  ((or (= "G" (strcase s1)) (= "GROUND" (strcase s1))) (mkhatch "ansi38" (* 600 hscale_d) 0 data_m))
		  ((or (= "F" (strcase s1)) (= "FLOOR" (strcase s1))) (mkhatch "ar-conc" (* 20 hscale_d) 0 data_m))
		)
    ))						 
	
	(command "erase")
	(setq i 0)
	(while (< i (length data_m)) (progn
		(command (nth i data_m))
		(setq i (+ i 1))
	))
	(command "")
  ))
  
  (princ)
)
  (ai_undo_push)	
  (ah_import)
  
  (ah_procced)
  (ai_undo_pop)
)


(defun c:hscale(/ i)
  (ai_undo_push)
  (if (= nil hscale_d) (setq hscale_d 1))
  (setq i (getreal (strcat (strcat "enter new hatch scale <" (rtos hscale_d 2 5)) "> ")))
  (if (not (= nil i)) (setq hscale_d i))
  (ai_undo_pop)
  (princ)
)
;===== AUTO DRAW DOOR-WINDOW (d1,w1,w2) ========

(defun moveent(ls1 post1 post2 / ls2 ent1 ent2 ent3 i)
    (setq ent1 (nth post1 ls1) ent2 (nth post2 ls1) i 0 ls2 '())
    (while (< i (length ls1)) (progn
        (if (= i post1) (setq ent3 ent2)
            (if (= i post2) (setq ent3 ent1) (setq ent3 (nth i ls1))
            )
        )
        (if (= nil ls2) (setq ls2 (list ent3))
            (setq ls2 (append ls2 (list ent3)))
        )
        (setq i (+ i 1))
    ))
    (setq ls1 ls2)
)

(defun arlst(ls1 / ls2 i j k)
;    (princ ls1)
    (setq i 0 ls2 ls1)
    (if (> (length ls2) 1) (progn
        (while (< i (- (length ls2) 1) ) (progn
            (setq j (+ i 1) k 0)
            (while (and (= 0 k) (< j (length ls2) )) (progn
                (if (< (nth j ls2) (nth i ls2)) (progn
                    ;(setq k 1)
                    (setq ls2 (moveent ls2 i j))
                ))
                (setq j (+ j 1))
            ))
            (setq i (+ i 1))
        ))
    ))
    (setq ls1 ls2)
)

(defun mkline(point1 point2 line1 / line2 i)
    (setq i 0 line2 '())
    (while (< i (length line1)) (progn
        (if (and (not (= 10 (car (nth i line1))) )
                 (not (= 11 (car (nth i line1))))) (progn
                    (if (= nil line2) (setq line2 (list (nth i line1)))
                        (setq line2 (append line2 (list (nth i line1))))
                    )
                 ))
        (setq i (+ i 1))
    ))
    (setq point1 (list 0 (nth 0 point1) (nth 1 point1) (nth 2 point1)))
    (setq point2 (list 0 (nth 0 point2) (nth 1 point2) (nth 2 point2)))
    (setq line2 (append line2 (list (cons 10 (cdr point1)))))
    (setq line2 (append line2 (list (cons 11 (cdr point2)))))
;    (princ line2)
    (entmake line2)
    (princ)
)

(defun drawrec (point1 point2 l1 / point3 point4)
    (setq point3 (list (nth 0 point1) (nth 1 point2) 0))
    (mkline point1 point3 l1) (mkline point2 point3 l1)
    (setq point3 (list (nth 0 point2) (nth 1 point1) 0))
    (mkline point1 point3 l1) (mkline point2 point3 l1)
    
)

(defun drawrt (point1 point2 / point3 point4 x l1)
    (setq l1 (list
        (cons 0 "line")
        (cons 8 (getvar "clayer"))
    ))
    
    (drawrec point1 point2 l1)
    (setq x (abs (- (nth 0 point1) (nth 0 point2))))
    (setq point3 (list (+ (nth 0 point1) (* 0.15 x) ) (nth 1 point1) 0))
    (setq point4 (list (nth 0 point3) (nth 1 point2) 0))
    (mkline point3 point4 l1)

    (setq point3 (list (- (nth 0 point2) (* 0.15 x) ) (nth 1 point1) 0))
    (setq point4 (list (nth 0 point3) (nth 1 point2) 0))
    (mkline point3 point4 l1)

    (setq point3 (list (+ (nth 0 point1) (* 0.15 x) ) (* 0.5 (+(nth 1 point1) (nth 1 point2))) 0))
    (setq point4 (list (- (nth 0 point2) (* 0.15 x) ) (nth 1 point3) 0))
    (mkline point3 point4 l1)

    (princ)
)

(defun drawrt1 (point1 point2 / point3 point4 x l1)
    (setq l1 (list
        (cons 0 "line")
        (cons 8 (getvar "clayer"))
    ))
    
    (drawrec point1 point2 l1)
    (setq x (abs (- (nth 1 point1) (nth 1 point2))))
    (setq point3 (list (nth 0 point1) (+ (nth 1 point1) (* 0.15 x) ) 0))
    (setq point4 (list (nth 0 point2) (nth 1 point3) 0))
    (mkline point3 point4 l1)

    (setq point3 (list (nth 0 point1) (- (nth 1 point2) (* 0.15 x) ) 0))
    (setq point4 (list (nth 0 point2) (nth 1 point3) 0))
    (mkline point3 point4 l1)

    (setq point3 (list (* 0.5 (+ (nth 0 point1) (nth 0 point2))) (+ (nth 1 point1) (* 0.15 x) ) 0))    (setq point4 (list (nth 0 point3) (- (nth 1 point2) (* 0.15 x) ) 0))
    (mkline point3 point4 l1)

    (princ)
)

(defun drawrt2(pt1 pt2 l1 / pt3 pt4 i dy1 dy2 qt)
    (setq pt3 (list (nth 0 pt1) (nth 1 pt2) 0))
    (mkline pt1 pt3 l1)
    (setq pt3 (list (nth 0 pt2) (nth 1 pt1) 0))
    (mkline pt2 pt3 l1)
    (if (< 150 (- (nth 1 pt2) (nth 1 pt1))) (setq dy1 40)
        (setq dy1 (* 0.3 (- (nth 1 pt2) (nth 1 pt1)))) )
    (setq qt (fix (/ (- (- (nth 1 pt2) (nth 1 pt1)) dy1) (+ 650 dy1))))
    (if (= 0 qt) (setq dy2 (- (- (nth 1 pt2) (nth 1 pt1)) (* 2 dy1))) 
    (setq dy2 (/ (- (- (nth 1 pt2) (nth 1 pt1)) (* (+ qt 1) dy1)) qt))
    )
    (if (= 0 qt) (setq qt 1))
    (setq i 0)
    (while (< i qt) (progn
        (setq pt3 (list (nth 0 pt1) (+ (nth 1 pt1) (+ dy1 (* (+ dy1 dy2) i))) 0))
        (setq pt4 (list (nth 0 pt2) (nth 1 pt3) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (nth 0 pt1) (+ (nth 1 pt3) dy2) 0))
        (setq pt4 (list (nth 0 pt2) (nth 1 pt3) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (+ (nth 0 pt1) (* 0.38 (- (nth 0 pt2) (nth 0 pt1)))) (nth 1 pt3) 0))
        (setq pt4 (list (nth 0 pt3) (- (nth 1 pt3) dy2) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (- (nth 0 pt2) (* 0.38 (- (nth 0 pt2) (nth 0 pt1)))) (nth 1 pt3) 0))
        (setq pt4 (list (nth 0 pt3) (- (nth 1 pt3) dy2) 0))
        (mkline pt3 pt4 l1)

        (setq i (+ i 1))
    ))
;    (princ qt)
    (princ)
)

(defun drawrt3(pt1 pt2 l1 / pt3 pt4 i dy1 dy2 qt)
    (setq pt3 (list (nth 0 pt2) (nth 1 pt1) 0))
    (mkline pt1 pt3 l1)
    (setq pt3 (list (nth 0 pt1) (nth 1 pt2) 0))
    (mkline pt2 pt3 l1)
    (if (< 150 (- (nth 0 pt2) (nth 0 pt1))) (setq dy1 60)
        (setq dy1 (* 0.3 (- (nth 0 pt2) (nth 0 pt1)))) )
    (setq qt (fix (/ (- (- (nth 0 pt2) (nth 0 pt1)) dy1) (+ 650 dy1))))
    (if (= 0 qt) (setq dy2 (- (- (nth 0 pt2) (nth 0 pt1)) (* 2 dy1))) 
    (setq dy2 (/ (- (- (nth 0 pt2) (nth 0 pt1)) (* (+ qt 1) dy1)) qt))
    )
    (if (= 0 qt) (setq qt 1))
    (setq i 0)
    (while (< i qt) (progn
        (setq pt3 (list (+ (nth 0 pt1) (+ dy1 (* (+ dy1 dy2) i))) (nth 1 pt1) 0))
        (setq pt4 (list (nth 0 pt3) (nth 1 pt2) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (+ (nth 0 pt3) dy2) (nth 1 pt1) 0))
        (setq pt4 (list (nth 0 pt3) (nth 1 pt2) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (nth 0 pt3) (+ (nth 1 pt1) (* 0.38 (- (nth 1 pt2) (nth 1 pt1)))) 0))
        (setq pt4 (list (- (nth 0 pt3) dy2) (nth 1 pt3) 0))
        (mkline pt3 pt4 l1)

        (setq pt3 (list (nth 0 pt3) (- (nth 1 pt2) (* 0.38 (- (nth 1 pt2) (nth 1 pt1)))) 0))
        (setq pt4 (list (- (nth 0 pt3) dy2) (nth 1 pt3) 0))
        (mkline pt3 pt4 l1)

        (setq i (+ i 1))
    ))
;    (princ qt)
    (princ)
)





(defun c:d1(/ data_m l1 l2 p1 p2 check)

(defun dw_import(/ p3 p4 p5 p6)
    (setq data_m (ssget))
    (setq p1 (getpoint "\nfirst point :") p2 (getpoint "\nsecond point :"))
    (setq l1 nil l2 nil check 1)
    (if (not (= nil data_m)) (progn
        (setq l1 (entget (ssname data_m 0)))
        (setq l2 (entget (ssname data_m 1)))
        (if (or (= nil l1) (not (= "LINE" (cdr (assoc 0 l1))))) (setq check 0))
        (if (or (= nil l2) (not (= "LINE" (cdr (assoc 0 l2))))) (setq check 0))
        (if (not (= 0 (-(sslength data_m) 2))) (setq check 0))
        (if (= 1 check) (progn
            (setq p3 (cdr (assoc 10 l1))) (setq p3 (list (nth 0 p3) (nth 1 p3)))
            (setq p4 (cdr (assoc 11 l1))) (setq p4 (list (nth 0 p4) (nth 1 p4)))
            (setq p5 (cdr (assoc 10 l2))) (setq p5 (list (nth 0 p5) (nth 1 p5)))
            (setq p6 (cdr (assoc 11 l2))) (setq p6 (list (nth 0 p6) (nth 1 p6)))
            (if (not (= nil (inters p3 p4 p5 p6 nil))) (setq check 0))
        ))
    ) (setq check 0))
    (princ)
)

(defun dw_procced()

(defun mkv(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom)

    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 
    (if (> (abs (- (nth 1 p1) (nth 1 p3)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p1) (nth 1 p4)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p5)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p6)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (= 0 check) (princ "\ninvalid data") (progn
        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p3) (nth 1 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p3) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p5) (nth 1 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p5) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p5) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 0 p3) (nth 1 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 2 ls1) 0))
        (mkline p7 p8 l1)

        (setq getom (getvar "osmode"))
        (setvar "osmode" 0)
        (if (< (nth 1 p1) (nth 1 p2)) (progn
            (setq ls2 (arlst (list (nth 0 p1) (nth 0 p3) (nth 0 p5) )))
            ;(princ ls2)
            (if (= (nth 0 p1) (nth 0 ls2)) (progn
                (setq p7 (list (nth 1 ls2) (nth 1 ls1) 0))

                (setq p8 (list (- (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) ))) )
                                  (+ (nth 1 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                ;(princ p8)
                (drawrt p8 p7)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p7) (nth 1 p8) 0))
                (setq p9 (list (nth 1 ls2) (nth 2 ls1) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p9 p8)
                (setvar "cmdecho" 1)
            )(progn
                (setq ls2 (arlst (list (nth 0 p3) (nth 0 p5) )))
                (setq p7 (list (nth 1 ls2) (nth 1 ls1) 0))
                (setq p8 (list (+ (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) ))) )
                                  (+ (nth 1 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                (drawrt p7 p8)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p7) (nth 1 p8) 0))
                (setq p9 (list (nth 1 ls2) (nth 2 ls1) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p8 p9)
                (setvar "cmdecho" 1)
            ))
        ) (progn
            (setq ls2 (arlst (list (nth 0 p1) (nth 0 p3) (nth 0 p5) )))
            (if (= (nth 0 p1) (nth 0 ls2)) (progn
                (setq p7 (list (nth 1 ls2) (nth 2 ls1) 0))

                (setq p8 (list (- (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) ))) )
                                  (- (nth 2 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                ;(princ p8)
                (drawrt p8 p7)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p7) (nth 1 p8) 0))
                (setq p9 (list (nth 1 ls2) (nth 1 ls1) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p8 p9)
                (setvar "cmdecho" 1)
            )(progn
                (setq ls2 (arlst (list (nth 0 p3) (nth 0 p5) )))
                (setq p7 (list (nth 1 ls2) (nth 2 ls1) 0))
                (setq p8 (list (+ (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) ))) )
                                  (- (nth 2 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                (drawrt p7 p8)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p7) (nth 1 p8) 0))
                (setq p9 (list (nth 1 ls2) (nth 1 ls1) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p9 p8)
                (setvar "cmdecho" 1)
            ))

        ))
        (setvar "osmode" getom)
        (setvar "cmdecho" 0)
        (command "erase" data_m "")
        (setvar "cmdecho" 1)
    ))
    (princ)
)



(defun mkh(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom)
    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 

    (if (> (abs (- (nth 0 p1) (nth 0 p3)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p1) (nth 0 p4)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p5)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p6)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))

    (if (= 0 check) (princ "\ninvalid data") (progn
        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p3) (nth 0 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p5) (nth 0 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 1 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 2 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

        (setq getom (getvar "osmode"))
        (setvar "osmode" 0)
        (if (< (nth 0 p1) (nth 0 p2)) (progn
            (setq ls2 (arlst (list (nth 1 p1) (nth 1 p3) (nth 1 p5) )))
            ;(princ ls2)
            (if (= (nth 1 p1) (nth 0 ls2)) (progn
                (setq p7 (list (nth 1 ls1) (nth 1 ls2) 0))

                (setq p8 (list (+ (nth 1 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) ))))
                                (- (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                ;(princ p8)
                (drawrt1 p8 p7)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p8) (nth 1 p7) 0))
                (setq p9 (list (nth 2 ls1) (nth 1 ls2) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p8 p9)
                (setvar "cmdecho" 1)
            )(progn
                (setq ls2 (arlst (list (nth 1 p3) (nth 1 p5) )))
                (setq p7 (list (nth 1 ls1) (nth 1 ls2) 0))

                (setq p8 (list (+ (nth 1 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) ))))
                                (+ (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
;                (princ p7)
;                (princ p8)
                (drawrt1 p7 p8)    
                (setq p9 p7) 
                (setq p7 (list (nth 0 p8) (nth 1 p7) 0))
                (setq p9 (list (nth 2 ls1) (nth 1 ls2) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p9 p8)
                (setvar "cmdecho" 1)
            ))
        ) (progn
            (setq ls2 (arlst (list (nth 1 p1) (nth 1 p3) (nth 1 p5) )))
            ;(princ ls2)
            (if (= (nth 1 p1) (nth 0 ls2)) (progn
                (setq p7 (list (nth 2 ls1) (nth 1 ls2) 0))

                (setq p8 (list (- (nth 2 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) ))))
                                (- (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
                ;(princ p8)
                (drawrt1 p8 p7)
                (setq p9 p7) 
                (setq p7 (list (nth 0 p8) (nth 1 p7) 0))
                (setq p9 (list (nth 1 ls1) (nth 1 ls2) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p9 p8)
                (setvar "cmdecho" 1)
            )(progn
                (setq ls2 (arlst (list (nth 1 p3) (nth 1 p5) )))
                (setq p7 (list (nth 2 ls1) (nth 1 ls2) 0))

                (setq p8 (list (- (nth 2 ls1) (* 0.05 (abs (- (nth 1 ls1) (nth 2 ls1) ))))
                                (+ (nth 1 ls2) (* 0.95 (abs (- (nth 1 ls1) (nth 2 ls1) )))) 0))
;                (princ p7)
;                (princ p8)
                (drawrt1 p7 p8)    
                (setq p9 p7) 
                (setq p7 (list (nth 0 p8) (nth 1 p7) 0))
                (setq p9 (list (nth 1 ls1) (nth 1 ls2) 0))
                (setvar "cmdecho" 0)
                (command "arc" "c" p7 p8 p9)
                (setvar "cmdecho" 1)
                
            ))

        ))
        (setvar "osmode" getom)
        (setvar "cmdecho" 0)
        (command "erase" data_m "")
        (setvar "cmdecho" 1)
    ))



    (princ) 
)
    (setvar "cmdecho" 0) (command "undo" "mark") (setvar "cmdecho" 1)
    (if (= 0 check) (princ "\ninvalid data") (progn
        (if (< (abs (- (nth 0 (cdr (assoc 10 l1)))
                       (nth 0 (cdr (assoc 11 l1))) )) 0.00001) (mkv))
        (if (< (abs (- (nth 1 (cdr (assoc 10 l1)))
                       (nth 1 (cdr (assoc 11 l1))) )) 0.00001) (mkh))
        

    ))
    
    (princ)
)

    (dw_import)
  	(ai_undo_push)
    (dw_procced)
  	(ai_undo_pop)
    
)


(defun c:w1(/ data_m l1 l2 p1 p2 check)

(defun wd_import(/ p3 p4 p5 p6)
    (setq data_m (ssget))
    (setq p1 (getpoint "\nfirst point :") p2 (getpoint "\nsecond point :"))
    (setq l1 nil l2 nil check 1)
    (if (not (= nil data_m)) (progn
        (setq l1 (entget (ssname data_m 0)))
        (setq l2 (entget (ssname data_m 1)))
        (if (or (= nil l1) (not (= "LINE" (cdr (assoc 0 l1))))) (setq check 0))
        (if (or (= nil l2) (not (= "LINE" (cdr (assoc 0 l2))))) (setq check 0))
        (if (not (= 0 (-(sslength data_m) 2))) (setq check 0))
        (if (= 1 check) (progn
            (setq p3 (cdr (assoc 10 l1))) (setq p3 (list (nth 0 p3) (nth 1 p3)))
            (setq p4 (cdr (assoc 11 l1))) (setq p4 (list (nth 0 p4) (nth 1 p4)))
            (setq p5 (cdr (assoc 10 l2))) (setq p5 (list (nth 0 p5) (nth 1 p5)))
            (setq p6 (cdr (assoc 11 l2))) (setq p6 (list (nth 0 p6) (nth 1 p6)))
            (if (not (= nil (inters p3 p4 p5 p6 nil))) (setq check 0))
        ))
    ) (setq check 0))
    (princ)
)

(defun wd_procced()

(defun mkv(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom ll1)

    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 
    (if (> (abs (- (nth 1 p1) (nth 1 p3)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p1) (nth 1 p4)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p5)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p6)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (= 0 check) (princ "\ninvalid data") (progn
        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p3) (nth 1 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p3) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p5) (nth 1 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p5) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p5) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 0 p3) (nth 1 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 2 ls1) 0))
        (mkline p7 p8 l1)

        (setq getom (getvar "osmode"))
        (setvar "osmode" 0)

        (setq ls2 (arlst (list (nth 0 p3) (nth 0 p5))))
        (setq p7 (list (nth 0 ls2) (nth 1 ls1) 0))
        (setq p8 (list (nth 1 ls2) (nth 2 ls1) 0))

        (setq ll1 (list
            (cons 0 "line")
            (cons 8 (getvar "clayer"))
        ))
        
        (drawrt2 p7 p8 ll1)

        (setvar "osmode" getom)
        (command "erase" data_m "")

    ))
    (princ)
)

(defun mkh(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom ll1)

    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 

    (if (> (abs (- (nth 0 p1) (nth 0 p3)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p1) (nth 0 p4)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p5)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p6)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))

    (if (= 0 check) (princ "\ninvalid data") (progn
        
        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p3) (nth 0 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p5) (nth 0 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 1 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 2 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

        (setq getom (getvar "osmode"))
        (setvar "osmode" 0)

        (setq ls2 (arlst (list (nth 1 p3) (nth 1 p5))))
        (setq p7 (list (nth 1 ls1) (nth 0 ls2) 0))
        (setq p8 (list (nth 2 ls1) (nth 1 ls2) 0))

        (setq ll1 (list
            (cons 0 "line")
            (cons 8 (getvar "clayer"))
        ))

        (drawrt3 p7 p8 ll1)

        (setvar "osmode" getom)
        (command "erase" data_m "")


    ))
    (princ)
)

    (setvar "cmdecho" 0) (command "undo" "mark") (setvar "cmdecho" 1)
    (if (= 0 check) (princ "\ninvalid data") (progn
        (if (< (abs (- (nth 0 (cdr (assoc 10 l1)))
                       (nth 0 (cdr (assoc 11 l1))) )) 0.00001) (mkv))
        (if (< (abs (- (nth 1 (cdr (assoc 10 l1)))
                       (nth 1 (cdr (assoc 11 l1))) )) 0.00001) (mkh))
        

    ))
    
    (princ)
    
)
    (wd_import)
  	(ai_undo_push)
    (wd_procced)
  	(ai_undo_pop)	
)


(defun c:w2(/ data_m l1 l2 p1 p2 check)

(defun wd_import(/ p3 p4 p5 p6)
    (setq data_m (ssget))
    (setq p1 (getpoint "\nfirst point :") p2 (getpoint "\nsecond point :"))
    (setq l1 nil l2 nil check 1)
    (if (not (= nil data_m)) (progn
        (setq l1 (entget (ssname data_m 0)))
        (setq l2 (entget (ssname data_m 1)))
        (if (or (= nil l1) (not (= "LINE" (cdr (assoc 0 l1))))) (setq check 0))
        (if (or (= nil l2) (not (= "LINE" (cdr (assoc 0 l2))))) (setq check 0))
        (if (not (= 0 (-(sslength data_m) 2))) (setq check 0))
        (if (= 1 check) (progn
            (setq p3 (cdr (assoc 10 l1))) (setq p3 (list (nth 0 p3) (nth 1 p3)))
            (setq p4 (cdr (assoc 11 l1))) (setq p4 (list (nth 0 p4) (nth 1 p4)))
            (setq p5 (cdr (assoc 10 l2))) (setq p5 (list (nth 0 p5) (nth 1 p5)))
            (setq p6 (cdr (assoc 11 l2))) (setq p6 (list (nth 0 p6) (nth 1 p6)))
            (if (not (= nil (inters p3 p4 p5 p6 nil))) (setq check 0))
        ))
    ) (setq check 0))
    (princ)
)

(defun wd_procced()

(defun mkv(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom ll1)

    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 
    (if (> (abs (- (nth 1 p1) (nth 1 p3)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p1) (nth 1 p4)))
           (abs (- (nth 1 p3) (nth 1 p4))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p5)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (> (abs (- (nth 1 p2) (nth 1 p6)))
           (abs (- (nth 1 p5) (nth 1 p6))) ) (setq check 0))
    (if (= 0 check) (princ "\ninvalid data") (progn
        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p3) (nth 1 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p3) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p3) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 1 p1) (nth 1 p2) (nth 1 p5) (nth 1 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 p5) (nth 0 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p5) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 3 ls1) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 0 p3) (nth 1 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 1 ls1) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 0 p3) (nth 2 ls1) 0))
        (setq p8 (list (nth 0 p5) (nth 2 ls1) 0))
        (mkline p7 p8 l1)

        (setq ls2 (arlst (list (nth 0 p3) (nth 0 p5))))
        
        (setq ll1 (list
            (cons 0 "line")
            (cons 8 (getvar "clayer"))
        ))
		
		(if (< (nth 0 p1) (nth 0 ls2)) (progn
        	(setq p7 (list (nth 0 ls2) (nth 1 ls1) 0))
			(setq p8 (list (nth 1 ls2) (nth 2 ls1) 0))
			(setq p7 (list (* (+ (nth 0 p7) (nth 0 p8)) 0.5) (nth 1 p7) 0))
        	(drawrt2 p7 p8 ll1)

			(setq p7 (list (- (nth 0 ls2) 70) (- (nth 1 ls1) 100) 0))
			(setq p8 (list (- (nth 0 ls2) 70) (+ (nth 2 ls1) 100) 0))
			(mkline p7 p8 ll1)

			(setq p9 (list (+ (nth 0 p7) 70) (nth 1 p7) 0))
			(mkline p7 p9 ll1)

			(setq p9 (list (+ (nth 0 p8) 70) (nth 1 p8) 0))
			(mkline p8 p9 ll1)
	    )(progn
		   	(setq p7 (list (nth 0 ls2) (nth 1 ls1) 0))
			(setq p8 (list (nth 1 ls2) (nth 2 ls1) 0))
			(setq p8 (list (* (+ (nth 0 p7) (nth 0 p8)) 0.5) (nth 1 p8) 0))
        	(drawrt2 p7 p8 ll1)

			(setq p7 (list (+ (nth 1 ls2) 70) (- (nth 1 ls1) 100) 0))
			(setq p8 (list (+ (nth 1 ls2) 70) (+ (nth 2 ls1) 100) 0))
			(mkline p7 p8 ll1)

			(setq p9 (list (- (nth 0 p7) 70) (nth 1 p7) 0))
			(mkline p7 p9 ll1)

			(setq p9 (list (- (nth 0 p8) 70) (nth 1 p8) 0))
			(mkline p8 p9 ll1)
		))

        
        (command "erase" data_m "")

    ))
    (princ)
)

(defun mkh(/ p3 p4 p5 p6 p7 p8 p9 ls1 ls2 getom ll1)

    (setq p3 (cdr (assoc 10 l1))) 
    (setq p4 (cdr (assoc 11 l1))) 
    (setq p5 (cdr (assoc 10 l2))) 
    (setq p6 (cdr (assoc 11 l2))) 

    (if (> (abs (- (nth 0 p1) (nth 0 p3)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p1) (nth 0 p4)))
           (abs (- (nth 0 p3) (nth 0 p4))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p5)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))
    (if (> (abs (- (nth 0 p2) (nth 0 p6)))
           (abs (- (nth 0 p5) (nth 0 p6))) ) (setq check 0))

    (if (= 0 check) (princ "\ninvalid data") (progn
        
        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p3) (nth 0 p4) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p3) 0))
        (mkline p7 p8 l1)

        (setq ls1 (arlst (list (nth 0 p1) (nth 0 p2) (nth 0 p5) (nth 0 p6) )))
;        (princ ls1)
        (setq p7 (list (nth 0 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p5) 0))
        (setq p8 (list (nth 3 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

        (setq p7 (list (nth 1 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 1 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)
        (setq p7 (list (nth 2 ls1) (nth 1 p3) 0))
        (setq p8 (list (nth 2 ls1) (nth 1 p5) 0))
        (mkline p7 p8 l1)

		(setq getom (getvar "osmode"))
        (setvar "osmode" 0)
		
		(setq ll1 (list
	            (cons 0 "line")
	            (cons 8 (getvar "clayer"))
	        ))
		(setq ls2 (arlst (list (nth 1 p3) (nth 1 p5))))
		;(princ ls2)
		
		(if (> (nth 1 p1) (nth 1 ls2)) (progn

        	(setq p7 (list (nth 1 ls1) (nth 0 ls2) 0))
        	(setq p8 (list (nth 2 ls1) (nth 1 ls2) 0))
			(setq p8 (list (nth 2 ls1) (* (+ (nth 1 p7) (nth 1 p8)) 0.5) 0))
		
        	(drawrt3 p7 p8 ll1)

			(setq p7 (list (- (nth 1 ls1) 100) (+ (nth 1 ls2) 70) 0))
			(setq p8 (list (+ (nth 2 ls1) 100) (+ (nth 1 ls2) 70) 0))
			(mkline p7 p8 ll1)

			(setq p9 (list (nth 0 p7) (- (nth 1 p7) 70) 0))
			(mkline p7 p9 ll1)

			(setq p9 (list (nth 0 p8) (- (nth 1 p8) 70) 0))
			(mkline p8 p9 ll1)
		)(progn
		  	(setq p7 (list (nth 1 ls1) (nth 0 ls2) 0))
        	(setq p8 (list (nth 2 ls1) (nth 1 ls2) 0))
			(setq p7 (list (nth 1 ls1) (* (+ (nth 1 p7) (nth 1 p8)) 0.5) 0))
			
			(drawrt3 p7 p8 ll1)
			

			(setq p7 (list (- (nth 1 ls1) 100) (- (nth 0 ls2) 70) 0))
			(setq p8 (list (+ (nth 2 ls1) 100) (- (nth 0 ls2) 70) 0))
			(mkline p7 p8 ll1)

			(setq p9 (list (nth 0 p7) (+ (nth 1 p7) 70) 0))
			(mkline p7 p9 ll1)

			(setq p9 (list (nth 0 p8) (+ (nth 1 p8) 70) 0))
			(mkline p8 p9 ll1)
			
			
		))
		
		(setvar "osmode" getom)
	
        (command "erase" data_m "")


    ))
    (princ)
)

    (setvar "cmdecho" 0) (command "undo" "mark") (setvar "cmdecho" 1)
    (if (= 0 check) (princ "\ninvalid data") (progn
        (if (< (abs (- (nth 0 (cdr (assoc 10 l1)))
                       (nth 0 (cdr (assoc 11 l1))) )) 0.00001) (mkv))
        (if (< (abs (- (nth 1 (cdr (assoc 10 l1)))
                       (nth 1 (cdr (assoc 11 l1))) )) 0.00001) (mkh))
        

    ))
    
    (princ)
    
)
    (wd_import)
    (wd_procced)
)

;======= AUTO DIM (h1,v1,h2,v2) =======

(defun c:h1(/ data_m ls1 p1 p2)

(defun import(/ i ent p3 p4)

(defun putnum(/ j k l ls3)
;    (princ p3)
    (if (= nil ls1) (setq ls1 (list (nth 0 p3))) (progn
        (setq ls3 '() j 0 k 0)
        (while (and (< j (length ls1)) (= k 0)) (progn
            (if (< (nth 0 p3) (nth j ls1)) (setq k 1) (progn
                (if (= nil ls3) (setq ls3 (list (nth j ls1)) )
                    (setq ls3 (append ls3 (list (nth j ls1)))))
                (setq j (+ j 1))
;                (princ *x*)
            ))
        ))
;        (princ j)
        (if (= nil ls3) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 0 p3) (nth j ls1)))) )
            (if (or (= nil k) (> k 0.0001))
            (setq ls3 (list (nth 0 p3)))
;            (princ "a")
        )) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 0 p3) (nth j ls1)))) )
            (if (> j 0) 
            (setq l (abs (- (nth 0 p3) (nth (- j 1) ls1)))) ) 
;            (princ k) (princ l)
            (if (and (or (= nil k) (> k 0.0001)) (or (= nil l) (> l 0.0001))) 
            (setq ls3 (append ls3 (list (nth 0 p3)))) )
;            (princ "b")
        ))
;        (princ ls3)
        (while (< j (length ls1)) (progn
            (setq ls3 (append ls3 (list (nth j ls1))))
            (setq j (+ j 1))
;            (princ ls3)
        ))
        (setq ls1 ls3)
    ))
;    (princ ls1)
    (princ)
)

    (setq data_m (ssget) ls1 '())
    (setq p1 (getpoint "\nfirst point") p2 (getpoint "\nsecond point"))

    (setq i 0)
    (while (< i (sslength data_m)) (progn
        (setq ent (entget (ssname data_m i)))
        ;(princ ent)
        (if (= "LINE" (cdr (assoc 0 ent))) (progn
            (setq p3 (cdr (assoc 10 ent))) (putnum)
            (setq p3 (cdr (assoc 11 ent))) (putnum)
            ;(princ p3)
            ;(princ p4)
            ;(if (and (>  0.00001 (- (nth 0 p3) (nth 0 p4)))
            ;         (< -0.00001 (- (nth 0 p3) (nth 0 p4)))) (putnum))
        ))
        (setq i (+ i 1))
    ))
    (princ)
)

(defun procced(/ p3 p4 p5 i omd)
  	(setq omd (getvar "osmode"))
  	(setvar "osmode" 0)
  	(setvar "cmdecho" 0)
    (if (> (length ls1) 1) (progn
        (setq i 0)
        (command "dim")
        (while (< i (- (length ls1) 1)) (progn
            (setq p3 (list (nth i ls1) (nth 1 p1) (nth 2 p1) ))
            (setq p4 (list (nth (+ i 1) ls1) (nth 1 p1) (nth 2 p1)))
            (setq p5 (list (nth 0 p1) (nth 1 p2) (nth 2 p1)) )
            (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 0.0001)
            (command "hor" p3 p4 p5 ""))
            (setq i (+ i 1))

        ))
        (command "exit")
    ))
  	(setvar "osmode" omd)
  	(setvar "cmdecho" 1)
    (princ)
)

    (import)
  	(ai_undo_push)
    (procced)
    (ai_undo_pop)
  	(princ)
)

(defun c:v1(/ data_m ls1 p1 p2 omd)

(defun import(/ i ent p3 p4)

(defun putnum(/ j k l ls3)
;    (princ p3)
    (if (= nil ls1) (setq ls1 (list (nth 1 p3))) (progn
        (setq ls3 '() j 0 k 0)
        (while (and (< j (length ls1)) (= k 0)) (progn
            (if (< (nth 1 p3) (nth j ls1)) (setq k 1) (progn
                (if (= nil ls3) (setq ls3 (list (nth j ls1)) )
                    (setq ls3 (append ls3 (list (nth j ls1)))))
                (setq j (+ j 1))
;                (princ *x*)
            ))
        ))
;        (princ j)
        (if (= nil ls3) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 1 p3) (nth j ls1)))) )
            (if (or (= nil k) (> k 0.0001))
            (setq ls3 (list (nth 1 p3)))
;            (princ "a")
        )) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 1 p3) (nth j ls1)))) )
            (if (> j 0) 
            (setq l (abs (- (nth 1 p3) (nth (- j 1) ls1)))) ) 
;            (princ k) (princ l)
            (if (and (or (= nil k) (> k 0.0001)) (or (= nil l) (> l 0.0001))) 
            (setq ls3 (append ls3 (list (nth 1 p3)))) )
;            (princ "b")
        ))
;        (princ ls3)
        (while (< j (length ls1)) (progn
            (setq ls3 (append ls3 (list (nth j ls1))))
            (setq j (+ j 1))
;            (princ ls3)
        ))
        (setq ls1 ls3)
    ))
;    (princ ls1)
    (princ)
)

    (setq data_m (ssget) ls1 '())
    (setq p1 (getpoint "\nfirst point") p2 (getpoint "\nsecond point"))

    (setq i 0)
    (while (< i (sslength data_m)) (progn
        (setq ent (entget (ssname data_m i)))
        ;(princ ent)
        (if (= "LINE" (cdr (assoc 0 ent))) (progn
            (setq p3 (cdr (assoc 10 ent))) (putnum)
            (setq p3 (cdr (assoc 11 ent))) (putnum)
            ;(princ p3)
            ;(princ p4)
            ;(if (and (>  0.00001 (- (nth 1 p3) (nth 1 p4)))
            ;         (< -0.00001 (- (nth 1 p3) (nth 1 p4)))) (putnum))
        ))
        (setq i (+ i 1))
    ))
    (princ)
)

(defun procced(/ p3 p4 p5 i)
    (if (> (length ls1) 1) (progn
        (setq i 0)
        (command "dim")
        (while (< i (- (length ls1) 1)) (progn
            (setq p3 (list (nth 0 p1) (nth i ls1) 0 ))
            (setq p4 (list (nth 0 p1) (nth (+ i 1) ls1) 0 ))
            (setq p5 (list (nth 0 p2) (nth 1 p2) (nth 2 p1)) )
            (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 0.0001)
            (command "ver" p3 p4 p5 ""))
            (setq i (+ i 1))

        ))
        (command "exit")
    ))
    (princ)
)

    (import)
  	(ai_undo_push)
    (setq omd (getvar "osmode"))
    (setvar "osmode" 0)
    (procced)
    (setvar "osmode" omd)
  	(ai_undo_pop)
    (princ)
)




(defun c:h2(/ data_m ls1 p1 omd)

(defun import(/ i ent p3 p4)

(defun putnum(/ j k l ls3)
;    (princ p3)
    (if (= nil ls1) (setq ls1 (list (nth 0 p3))) (progn
        (setq ls3 '() j 0 k 0)
        (while (and (< j (length ls1)) (= k 0)) (progn
            (if (< (nth 0 p3) (nth j ls1)) (setq k 1) (progn
                (if (= nil ls3) (setq ls3 (list (nth j ls1)) )
                    (setq ls3 (append ls3 (list (nth j ls1)))))
                (setq j (+ j 1))
;                (princ *x*)
            ))
        ))
;        (princ j)
        (if (= nil ls3) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 0 p3) (nth j ls1)))) )
            (if (or (= nil k) (> k 0.0001))
            (setq ls3 (list (nth 0 p3)))
;            (princ "a")
        )) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 0 p3) (nth j ls1)))) )
            (if (> j 0) 
            (setq l (abs (- (nth 0 p3) (nth (- j 1) ls1)))) ) 
;            (princ k) (princ l)
            (if (and (or (= nil k) (> k 0.0001)) (or (= nil l) (> l 0.0001))) 
            (setq ls3 (append ls3 (list (nth 0 p3)))) )
;            (princ "b")
        ))
;        (princ ls3)
        (while (< j (length ls1)) (progn
            (setq ls3 (append ls3 (list (nth j ls1))))
            (setq j (+ j 1))
;            (princ ls3)
        ))
        (setq ls1 ls3)
    ))
;    (princ ls1)
    (princ)
)

    (setq data_m (ssget) ls1 '())
    (setq p1 (getpoint "\nPick point"))

    (setq i 0)
    (while (< i (sslength data_m)) (progn
        (setq ent (entget (ssname data_m i)))
        ;(princ ent)
        (if (= "LINE" (cdr (assoc 0 ent))) (progn
            (setq p3 (cdr (assoc 10 ent))) (putnum)
            (setq p3 (cdr (assoc 11 ent))) (putnum) 
            ;(if (and (>  0.00001 (- (nth 0 p3) (nth 0 p4)))
            ;         (< -0.00001 (- (nth 0 p3) (nth 0 p4)))) (putnum))
        ))
        (setq i (+ i 1))
    ))
    (princ)
)

(defun procced(/ s1)

(defun putnumber(/ i j p2)
;    (princ ls1)
    (if (= nil startnb) (setq startnb 1))
    (setq i (getint "\nEnter first number : "))
    (if (not (= nil i)) (setq startnb i))
    (setq i 0 j 0)
    (while (< i (- (length ls1) 1)) (progn
        (setq p2 (list (nth i ls1) (nth 1 p1) 0))
        (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 1) (progn
            (command "circle" p2 300)
            (command "text" "j" "mc" p2 300 0 (itoa (+ j startnb)))
			(setq j (+ j 1))
        ))
        (setq i (+ i 1))
    ))
    (if (< i (length ls1)) (progn
        (setq p2 (list (nth i ls1) (nth 1 p1) 0))
        (command "circle" p2 300)
        (command "text" "j" "mc" p2 300 0 (itoa (+ j startnb)))

    ))
    (princ)
)

(defun putchar(/ i j)
;    (princ ls1)
    (if (= nil startnb) (setq startnb 0))
    (setq i "asd")
    (while (or (= nil i) (or (= " " i) (< 1 (strlen i)) ) ) 
        (setq i (getstring "\nEnter charater : ")))
    (if (not (= nil i)) (setq startnb (ascii (strcase i))))
    (setq i 0 j 0)
    (while (< i (- (length ls1) 1)) (progn
        (setq p2 (list (nth i ls1) (nth 1 p1) 0))
        (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 1) (progn
            (command "circle" p2 300)
            (command "text" "j" "mc" p2 300 0 (chr (+ j startnb)))
			(setq j (+ j 1))
        ))
        (setq i (+ i 1))
    ))
    (if (< i (length ls1)) (progn
        (setq p2 (list (nth i ls1) (nth 1 p1) 0))
        (command "circle" p2 300)
        (command "text" "j" "mc" p2 300 0 (chr (+ j startnb)))
    ))
    (princ)
)

    (if (> (length ls1) 0) (progn
        (initget 1 "Number Charater N C n c")
        (setq s1 (getkword "\nNumber/Charater : "))
        (if (not (= nil s1)) (progn
            (if (or (= (strcase s1) "NUMBER") (= (strcase s1) "N") ) 
            (putnumber) (putchar))
        ))
    ))
)

    (import)
    
    (setq omd (getvar "osmode"))
    (setvar "osmode" 0)
  	(ai_undo_push)
    (procced)
  	(ai_undo_pop)
    (setvar "osmode" omd)
    
    (princ)
)



(defun c:v2(/ data_m ls1 p1 omd)

(defun import(/ i ent p3 p4)

(defun putnum(/ j k l ls3)
;    (princ p3)
    (if (= nil ls1) (setq ls1 (list (nth 1 p3))) (progn
        (setq ls3 '() j 0 k 0)
        (while (and (< j (length ls1)) (= k 0)) (progn
            (if (< (nth 1 p3) (nth j ls1)) (setq k 1) (progn
                (if (= nil ls3) (setq ls3 (list (nth j ls1)) )
                    (setq ls3 (append ls3 (list (nth j ls1)))))
                (setq j (+ j 1))
;                (princ *x*)
            ))
        ))
;        (princ j)
        (if (= nil ls3) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 1 p3) (nth j ls1)))) )
            (if (or (= nil k) (> k 0.0001))
            (setq ls3 (list (nth 1 p3)))
;            (princ "a")
        )) (progn
            (setq k nil l nil)
            (if (< j (length ls1)) 
            (setq k (abs (- (nth 1 p3) (nth j ls1)))) )
            (if (> j 0) 
            (setq l (abs (- (nth 1 p3) (nth (- j 1) ls1)))) ) 
;            (princ k) (princ l)
            (if (and (or (= nil k) (> k 0.0001)) (or (= nil l) (> l 0.0001))) 
            (setq ls3 (append ls3 (list (nth 1 p3)))) )
;            (princ "b")
        ))
;        (princ ls3)
        (while (< j (length ls1)) (progn
            (setq ls3 (append ls3 (list (nth j ls1))))
            (setq j (+ j 1))
;            (princ ls3)
        ))
        (setq ls1 ls3)
    ))
;    (princ ls1)
    (princ)
)

    (setq data_m (ssget) ls1 '())
    (setq p1 (getpoint "\nPick point"))

    (setq i 0)
    (while (< i (sslength data_m)) (progn
        (setq ent (entget (ssname data_m i)))
        ;(princ ent)
        (if (= "LINE" (cdr (assoc 0 ent))) (progn
            (setq p3 (cdr (assoc 10 ent))) (putnum)
            (setq p3 (cdr (assoc 11 ent))) (putnum)
            ;(if (and (>  0.00001 (- (nth 1 p3) (nth 1 p4)))
            ;         (< -0.00001 (- (nth 1 p3) (nth 1 p4)))) (putnum))
        ))
        (setq i (+ i 1))
    ))
    (princ)
)

(defun procced(/ s1)

(defun putnumber(/ i j p2)
;    (princ ls1)
    (if (= nil startnb) (setq startnb 1))
    (setq i (getint "\nEnter first number : "))
    (if (not (= nil i)) (setq startnb i))
    (setq i 0 j 0)
    (while (< i (- (length ls1) 1)) (progn
        (setq p2 (list (nth 0 p1) (nth i ls1) 0))
        (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 1) (progn
            (command "circle" p2 300)
            (command "text" "j" "mc" p2 300 0 (itoa (+ j startnb)))
			(setq j (+ j 1))
        ))
        (setq i (+ i 1))
    ))
    (if (< i (length ls1)) (progn
        (setq p2 (list (nth 0 p1) (nth i ls1) 0))
        (command "circle" p2 300)
        (command "text" "j" "mc" p2 300 0 (itoa (+ j startnb)))

    ))
    (princ)
)

(defun putchar(/ i j)
;    (princ ls1)
    (if (= nil startnb) (setq startnb 0))
    (setq i "asd")
    (while (or (= nil i) (or (= " " i) (< 1 (strlen i)) ) ) 
        (setq i (getstring "\nEnter charater : ")))
    (if (not (= nil i)) (setq startnb (ascii (strcase i))))
    (setq i 0 j 0)
    (while (< i (- (length ls1) 1)) (progn
        (setq p2 (list (nth 0 p1) (nth i ls1) 0))
        (if (> (abs (- (nth i ls1) (nth (+ i 1) ls1))) 1) (progn
            (command "circle" p2 300)
            (command "text" "j" "mc" p2 300 0 (chr (+ j startnb)))
			(setq j (+ j 1))
        ))
        (setq i (+ i 1))
    ))
    (if (< i (length ls1)) (progn
        (setq p2 (list (nth 0 p1) (nth i ls1) 0))
        (command "circle" p2 300)
        (command "text" "j" "mc" p2 300 0 (chr (+ j startnb)))

    ))
    (princ)
)

    (if (> (length ls1) 0) (progn
        (initget 1 "Number Charater N C n c")
        (setq s1 (getkword "\nNumber/Charater : "))
        (if (not (= nil s1)) (progn
            (if (or (= (strcase s1) "NUMBER") (= (strcase s1) "N") ) 
            (putnumber) (putchar))
        ))
    ))
)

    (import)
  	(ai_undo_push)
    (setq omd (getvar "osmode"))
    (setvar "osmode" 0)
    (procced)
    (setvar "osmode" omd)
  	(ai_undo_pop)
    (princ)
)


;====== OO (oo) =======

(defun c:oo(/ data_m)

(defun import_data(/ i)
    (setq data_m (ssget))
    (if (= nil distan_m) (setq distan_m 110.0))
    (princ "Distance (")
    (princ distan_m)
    (princ "):")
    (setq i (getreal ))
    (if (not (= nil i)) (setq distan_m i))
)

(defun process(/ ent check)

(defun p_check()
    (setq check 0)
    (if (= "LINE" (cdr (assoc 0 ent))) (setq check 1))
    (princ)
)

(defun p_d_offset(/ p1 p2 p3 p4)

(defun makeline(/ e2 e5)
;    (princ ent)
;    (setq e5 nil)
;    (setq e5 (cdr (assoc 5 ent)))
;    (princ e5)
;    (if (= nil e5) (setq e5 ))

    (setq la (list (cons 0 "LINE")
        (cons 5 (cdr (assoc 5 ent)) )
        (cons 8 (cdr (assoc 8 ent)) )
        (cons 10 p3)
        (cons 11 p4)
    ))
;    (princ la)
    (entmake la)
    (princ)
)

    (setq p1 (cdr (assoc 10 ent)) p2 (cdr (assoc 11 ent)) )
    (if (not (= p1 p2)) (progn
        (if (< (abs (- (nth 0 p1) (nth 0 p2))) 0.000001) (progn
            (setq p3 (list (+ (nth 0 p1) distan_m) (nth 1 p1) (nth 2 p1) ) )
            (setq p4 (list (+ (nth 0 p2) distan_m) (nth 1 p2) (nth 2 p2) ) )
            (makeline)
            (setq p3 (list (- (nth 0 p1) distan_m) (nth 1 p1) (nth 2 p1) ) )
            (setq p4 (list (- (nth 0 p2) distan_m) (nth 1 p2) (nth 2 p2) ) )
            (makeline)
        ))
        (if (< (abs (- (nth 1 p1) (nth 1 p2))) 0.000001) (progn
            (setq p3 (list (nth 0 p1) (+ (nth 1 p1) distan_m) (nth 2 p1) ) )
            (setq p4 (list (nth 0 p2) (+ (nth 1 p2) distan_m) (nth 2 p2) ) )
            (makeline)
            (setq p3 (list (nth 0 p1) (- (nth 1 p1) distan_m) (nth 2 p1) ) )
            (setq p4 (list (nth 0 p2) (- (nth 1 p2) distan_m) (nth 2 p2) ) )
            (makeline)
        ))

    ))
    (princ)
)

    (if (not (= nil data_m)) (progn
        (setq i 0)
        (while (< i (sslength data_m)) (progn
            (setq ent (entget (ssname data_m i)))
            (p_check)
            (if (= 1 check) (p_d_offset))
            (setq i (+ i 1))
        ))
    ))
    (princ)
)
    (import_data)
  	(ai_undo_push)
    (process)
  	(ai_undo_pop)
    (princ)
)

