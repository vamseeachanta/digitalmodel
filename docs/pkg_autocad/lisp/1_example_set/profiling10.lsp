           ;;;;;;��� �������� ���� �� ����� �������� �� ���� ��������;;;;;;;;;;
(defun c:pro ()
;;;;;;;;;;;;; n.g drawing   ;;;;;;;;;;;;;;;;;;;;;;
          (command "osnap" "off" ) 
 ;;;���� ���� ����� ������������� ���� ����� ������� ����� ������ ����� ���� ��������;;        
            (command "layer" "m" "ng" "c" "1" "" "l" "acad_iso03w100" "" "") 
            (command "layer" "m" "txt" "c" "4" "" "")
            (command "layer" "m" "pg" "c" "3" "" "")            
            (command "layer" "m" "grid" "c" "7" "" "")
            (command "layer" "m" "FGRID" "c" "2" "" "l" "acad_iso07w100" "" "")  
 ;;; ���� �������� �� ��� ���� ������ ��� ���� ������ �������;;;;
            (setq X1 (getdist "\nPlease Enter The First Station <>:"))    
            (setq y1 (getdist "\nPlease Enter The First Elevation <>:"))
            (setq STATION (getdist "\nPlease Enter The Stationing Distance <>:"))
            (setq yy (* (fix y1) 10))
            (setq pp (list x1 yy))
            (setq p1 (list x1 (* y1 10)))    
            (setq p2 (polar pp (/ (* -1 pi) 2) 100.0))  
            (setq originx (car p2))    
            (setq originy (cadr p2))
            (setq bordery (/ originy 50))
            (setq bordery (fix bordery))
            (setq bordery (* bordery 50))
            (if (< bordery originy) (setq bordery (+ bordery 50)))
            (setq p3 (polar pp (/ (* -1 pi) 2) 140.0)) 
            (setq p4 (polar pp (/ (* -1 pi) 2) 180.0)) 
            (setq p5 (polar pp (/ (* -1 pi) 2) 220.0)) 
            (setq p6 (polar pp (/ (* +1 pi) 2) 100.0)) 
            (setq p7 (polar p5 (* -1 pi ) 100.0)) 
            (setq p8 (polar p4 (* -1 pi) 100.0)) 
            (setq p9 (polar p3 (* -1 pi) 100.0)) 
            (setq p10 (polar p2 (* -1 pi) 100.0)) 
           ;;;;;;;;;;;��� ���� ������� �� ����� ������ ;;;;;;
            (command "layer" "set" "grid" "")        
            (command "line" p6 p5 "" )
            (command "rectangle" p7 p2 )
            (command "line" p8 p4 "")
	    (command "line" p9 p3 "")  
            (setq p11 (polar p9 (/ (* +1 pi) 4) 10.0)) 
            (setq p12 (polar p8 (/ (* +1 pi) 4) 10.0)) 
            (setq p13 (polar p7 (/ (* +1 pi) 4) 10.0)) 
            (command "layer" "set" "txt" "")
            (command "text" "j" "ml" p11 5.0 0.0 "Grade Elev." "") 
            (command "text" "j" "ml" p12 5.0 0.0 "Ground Elev." "") 
            (command "text" "j" "ml" p13 5.0 0.0 "STATION" "") 
            (setq p22 (polar p4 (/(* +1 pi)3) 6.0)) 
            (setq p23 (polar p5 (/(* +1 pi)3) 6.0)) 
            (command "text" "j" "ml" p22 5.0 90.0 (rtos y1 2 3)) 
            (setq st x1 )
            (setq inst (/ st 1000))
            (setq inst (fix inst))
            (setq frst (- st (* inst 1000)))
            (setq frst (RTOS frst 2 0))
            (setq le (strlen frst))            
            (if (< le 3) (setq frst (strcat "0" frst)))
            (setq ST (strcat (rtos inst 2 0) "+"  frst ))
            (command "text" "j" "ml" p23 5.0 90.0 st) 
         ;   (command "text" "j" "ml" p23 5.0 90.0 (rtos x1 2 0)) 
            (command "zoom" "e")
         ;;;;;;����� ����� �������� ��� ������;;;;;;;;;
            (while  (< bordery (+ originy 200))
                    (setq p44 (list x1  bordery))
                    (setq p44 (polar p44 (* -1 pi) 20))
                    (command "text" "m" p44 5.0 0.0 (rtos (/ bordery 10) 2 3))
                    (setq bordery (+ bordery 50))
             )
            (command "redraw")
            (command "layer" "set" "ng" "")
       :::���� �� ����� ������ ����� �������� ;;;;;;;;;
        (while  (setq y1 (getdist "\Please Enter The Next N.G Elevation <>:"))    
            (setq x1 (+ x1 STATION))   
            (setq y (* y1 10))
            (setq pt (list x1 y))  
            (setq p14 (polar p6 0.0 STATION))  
            (setq p15 (polar p2 0.0 STATION)) 
            (setq p16 (polar p3 0.0 STATION)) 
            (setq p17 (polar p4 0.0 STATION))
            (setq p18 (polar p5 0.0 STATION))
            (SETQ L1 P6P14) 
    ;;;;;;;��� �� ����� �������� ;;;;;;;;;;;;;;;;;
            (command "layer" "set" "ng" "")
            (command "line" p1 pt "")  
            (COMMAND "change" "l" "" "p" "s" 0.5 "")           
    ;;;;;;;��� ���� ������ ;;;;;;;;;;;;;;;;;
            (command "layer" "set" "grid" "")    
            (command "line" p14 p18 "")            
            (command "line" p2 p15 "")
            (command "line" p3 p16 "")
            (command "line" p4 p17 "")
            (command "line" p5 p18 "")
            (command "line" p6 p14 "")
            (setq p12 (polar p17 (/ (* -5 pi) 4) 5.5)) 
            (setq p13 (polar p18 (/ (* -5 pi) 4) 5.5))          
  ;;;;;;;��� ���� ������ ������� ;;;;;;;;;;;;;;;;; 
            (command "layer" "set" "FGRID" "")
            (setq p66 (polar p6 (/ (* -1 pi)2) 10.0))
	    (setq p144 (polar p14 (/ (* -1 pi)2) 10.0))
            (command "line" p66 p144 "")
           ; (COMMAND "change" "l" "" "p" "s" 0.7 "")      
            (COMMAND "array" "l" "" "r" 20 1 -10)
       ;;;;;;;����� ������� ������ ����� ��������  ;;;;;;;;;;;;;;;;;    
            (command "layer" "set" "txt" "")
            (command "text" "j" "ml" p12 5.0 90.0 (rtos y1 2 3)) 
            (setq st x1 )
            (setq inst (/ st 1000))
            (setq inst (fix inst))
            (setq frst (- st (* inst 1000)))
            (setq frst (RTOS frst 2 0))
            (setq le (strlen frst))            
            (if (< le 3) (setq frst (strcat "0" frst)))
            (setq ST (strcat (rtos inst 2 0) "+"  frst ))
            (command "text" "j" "ml" p13 5.0 90.0 st) 
    ;;;;;;;����� ��� ������ ������ ������� ���� �� ������ �������  ;;;;;;;;;;;;;;;;;     
          (setq p1 pt) 
          (setq p6 p14)
          (setq p2 p15)
          (setq p3 p16) 
          (setq p4 p17)
          (setq p5 p18)
          (command "zoom" "e")
      )
    (command "zoom" "e")    
    (command "redraw")
    (load "d:/lisp/desining10")
 
)
