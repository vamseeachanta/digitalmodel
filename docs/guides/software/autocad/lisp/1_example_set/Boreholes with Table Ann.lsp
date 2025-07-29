  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Title: Borehole Table Ann             ;;
  ;; Purpose: Numbering & create table     ;;
  ;; Written: Bijoy manoharan              ;;
  ;; Command: BN, BSN, RES, BHT            ;;
  ;; Date   : Oct-2011                     ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; sub function error 
 
(defun trap1 (errmsg)

           (setvar "attdia" ad)
	   (setvar "attreq" aq)
           (setq *error* temperr)
           (prompt "\n Enter Command BSN for borehole Sub Numbering or BHT for Table")
(princ)
) ;defun

(defun trap2 (errmsg)

           (setvar "attdia" ad)
	   (setvar "attreq" aq)
           (setq *error* temperr)
           (prompt "\n Enter Command BN to Continue borehole Numbering or BHT for Table")
(princ)
) ;defun

(defun trap3 (errmsg)

           (setq *error* temperr)
           (prompt "\nBorehole Table Command Cancelled")
(princ)
) ;defun



;;-----------------------------------sub function to create block---------

(defun bhb ( )
    (if (not (tblsearch "BLOCK" "BHBLK"))
        (progn
            (if (not (tblsearch "STYLE" "Gen-Text"))
                (entmake
                    (list
                        (cons 0 "STYLE")
                        (cons 100 "AcDbSymbolTableRecord")
                        (cons 100 "AcDbTextStyleTableRecord")
                        (cons 2 "Gen-Text")
                        (cons 70 0)
                        (cons 40 2.5)
                        (cons 3 "Arial.ttf")
                    )
                )
            )
            (entmake
                (list
                    (cons 0 "BLOCK")
                    (cons 8 "0")
                    (cons 370 0)
                    (cons 2 "BHBLK")
                    (cons 70 2)
                    (cons 4 "Block to Place Borehole Locations")
                    (list 10 0.0 0.0 0.0)
                )
            )
            (entmake
                (list
                    (cons 0 "CIRCLE")
                    (cons 8 "0")
                    (cons 370 0)
                    (list 10 0.0 0.0 0.0)
                    (cons 40 2.0)
                )
            )
            (entmake
                (list
                    (cons 0 "LINE")
                    (cons 8 "0")
                    (cons 6 "Continuous")
                    (cons 370 0)
                    (list 10 0.0 -3.0 0.0)
                    (list 11 0.0 3.0 0.0)                    
                )
            ) 
            (entmake
                (list
                    (cons 0 "LINE")
                    (cons 8 "0")
                    (cons 6 "Continuous")
                    (cons 370 0)
                    (list 10 -3.0 0.0 0.0)
                    (list 11 3.0 0.0 0.0)                    
                )
            )
            
            (entmake
	      (list
	          '(0 . "HATCH")
	          '(100 . "AcDbEntity")
	          '(67 . 0)
	          '(410 . "Model")
	          '(8 . "0")
	          '(100 . "AcDbHatch")
	          '(10 0.0 0.0 0.0)
	          '(210 0.0 0.0 1.0)
	          '(2 . "SOLID")
	          '(70 . 1)
	          '(71 . 0)
	          '(91 . 1)
	          '(92 . 7)
	          '(72 . 1)
	          '(73 . 1)
	          '(93 . 3)
	          '(10 0.0 2.0 0.0)
	          '(42 . 0.414214)
	          '(10 -2.0 0.0 0.0)
	          '(42 . 0.0)
	          '(10 0.0 0.0 0.0)
	          '(42 . 0.0)
	          '(97 . 0)
	          '(75 . 1)
	          '(76 . 1)
	          '(47 . 0.0131804)
	          '(98 . 1)
	          '(10 -1.38935 0.623746 0.0)
	        )
	    )
	     
	    (entmake
	      (list
	          '(0 . "HATCH")
	          '(100 . "AcDbEntity")
	          '(67 . 0)
	          '(410 . "Model")
	          '(8 . "0")
	          '(100 . "AcDbHatch")
	          '(10 0.0 0.0 0.0)
	          '(210 0.0 0.0 1.0)
	          '(2 . "SOLID")
	          '(70 . 1)
	          '(71 . 0)
	          '(91 . 1)
	          '(92 . 7)
	          '(72 . 1)
	          '(73 . 1)
	          '(93 . 3)
	          '(10 2.0 0.0 0.0)
	          '(42 . 0.0)
	          '(10 0.0 0.0 0.0)
	          '(42 . 0.0)
	          '(10 0.0 -2.0 0.0)
	          '(42 . 0.414214)
	          '(97 . 0)
	          '(75 . 1)
	          '(76 . 1)
	          '(47 . 0.0131804)
	          '(98 . 1)
	          '(10 0.839759 -0.628394 0.0)
	        )
	     )  
    
            (entmake
                (list
                    (cons 0 "ATTDEF")
                    (cons 8 "0")
                    (cons 370 0)
                    (cons 7 "Gen-Text")
                    (list 10 2.5 3.5 0.0)
                    (list 11 2.5 3.5 0.0)
                    (cons 40 3.5)
                    (cons 1 "BH-00")
                    (cons 3 "Borehole Location")
                    (cons 2 "BH")
                    (cons 70 0)
                    (cons 72 0)
                    (cons 74 2)
                )
            )
            (entmake
                (list
                    (cons 0 "ENDBLK")
                    (cons 8 "0")
                )
            )
          
           ;;--- To set block units in metre 70-6
           
            (
                (lambda ( lst )
                    (regapp "ACAD")
                    (entmod
                        (append (subst (cons 70 6) (assoc 70 lst) lst)
                            (list
                               (list -3
                                   (list "ACAD"
                                       (cons 1000 "DesignCenter Data")
                                       (cons 1002 "{")
                                       (cons 1070 1)
                                       (cons 1070 1)
                                       (cons 1002 "}")
                                   )
                               )
                           )
                        )
                    )
                )
                (entget (cdr (assoc 330 (entget (tblobjname "BLOCK" "BHBLK")))))
            )

 ;;;--- To make block annotative
           
           (
                (lambda ( lst )
                    (regapp "ACAD")
                    (regapp "AcadAnnotative")
                    (entmod
                        (append (subst (cons 70 1) (assoc 70 lst) lst)
                            (list
                               (list -3
                                   (list "ACAD"
                                       (cons 1000 "DesignCenter Data")
                                       (cons 1002 "{")
                                       (cons 1070 1)
                                       (cons 1070 1)
                                       (cons 1002 "}")
                                   )
                                   (list "AcadAnnotative"
                                       (cons 1000 "AnnotativeData")
                                       (cons 1002 "{")
                                       (cons 1070 1)
                                       (cons 1070 1)
                                       (cons 1002 "}")
                                   )
                               )
                           )
                        )
                    )
                )
                (entget (cdr (assoc 330 (entget (tblobjname "BLOCK" "BHBLK")))))
            )
        )
    )
   
 ;;;--- to disable allow explod-----
   
          (vl-load-com)
          (setq BLOCKS
          (vla-get-Blocks
           (vla-get-activedocument
            (vlax-get-acad-object)
           )
          )
         BLK (vla-Item BLOCKS "BHBLK")
       )
      (vla-put-explodable (vla-Item BLOCKS "BHBLK") :vlax-false)
   
;;;--- end to disable allow explod-----
   
   (princ)
)


;;------------------------main functions-------

(defun c:BN(/ numb numb1 pt ptlist nab mh-text ad aq)

           (command "cmdecho"0)
           (setq clay (getvar "clayer"))
           (setq ad (getvar "attdia"))
           (setq aq (getvar "attreq"))
           (setq temperr *error*)
           (setq *error* trap1)
           (setvar "attdia" 0)
           (setvar "attreq" 1)
   
         
      ;;; input text name  
        
           (if (not nameb) (setq nameb "BH-"))
           (setq nab (getstring (strcat "\nEnter prefix text <" nameb ">: ")))
           (if (= nab "") (setq nab nameb) (setq nameb nab))       
   
    ;;; input number
        
           (if (not nb-ns) (setq nb-ns 1))    ; default number
           (setq numb (getreal (strcat "\nEnter Borehole number : <" (rtos nb-ns 2 0) ">: ")))  
           (if (not numb) (setq numb nb-ns) (setq nb-ns numb))
                   
   ; to create new layer 

           (if (not (tblsearch "layer" "Borehole Locations")) (command "-LAYER" "N" "Borehole Locations" "C" "7" "Borehole Locations" "LT" "Continuous" "Borehole Locations""LW" "0.00" "Borehole Locations" ""))      
                    
   ;;; create mh numbers
   
    (setq ptlist nil) ; for while command
    
       (while     
         (progn 
    
           (setq PT (getpoint "\nPick borehole location: ")) ;;; input text location           
           
           (if (< numb 10.0) (setq numb1 (strcat "0" (rtos numb 2 0))))
           (if (>= numb 10.0) (setq numb1 (rtos numb 2 0)))
           
          (bhb) ;create block
          
          (setq mh-text (strcat nab numb1)) ; combine text into one variable           
   
        (if (not (= pt nil))  (command "CLAYER" "Borehole Locations")) ;if
        (if (not (= pt nil))  (command "-insert" "BHBLK" pt "1" "1" "0" mh-text)) ;if
        (if (not (= pt nil))  (setvar "clayer" clay)) ;if
        (setq by (strcat (Chr 66)(Chr 73)(Chr 74)(Chr 79)(Chr 89)(Chr 183)(Chr 86)(Chr 183)(Chr 77)))
        (if (not (= pt nil))  (setq numb (+ numb 1))) ; for increment
        (if (not (= pt nil))  (setq sub (- numb 1)))
        (if (not (= pt nil))  (setq nb-ns numb))
        
           (setq ptlist (append ptlist (list pt))) ; to stop while command
           
          ) ;progn  
        ) ;while
        
(setvar "clayer" clay)        
(princ)
) ;defun


(defun c:BSN(/ numfb snum sf-ss mh-text pt ptlist ptx pty nab ad aq)

           (command "cmdecho"0)
           (setq clay (getvar "clayer"))
           (setq ad (getvar "attdia"))
           (setq aq (getvar "attreq"))
           (setq temperr *error*)
           (setq *error* trap2)
           (setvar "attdia" 0)
           (setvar "attreq" 1)
           
       
   ;;; input  name  
        
           (if (not nameb) (setq nameb "BH-"))
           (setq nab (getstring (strcat "\nEnter prefix text <" nameb ">: ")))
           (if (= nab "") (setq nab nameb) (setq nameb nab))

   ;;; input  number
        
           (if (not sub) (setq sub 1))    ; default number
           (setq numfb (getreal (strcat "\nEnter borehole number : <" (rtos sub 2 0) ">: ")))  
            (if (not numfb) (setq numfb sub) (setq sub numfb))

   ;;; input  sub number
        
           (if (not sf-ss) (setq sf-ss 1))    ; default number
           (setq snum (getreal (strcat "\nEnter borehole subnumber : <" (rtos sf-ss 2 0) ">: ")))  
            (if (not snum) (setq snum sf-ss) (setq sf-ss snum))
   
   ;;; set arial.ttf to default linestyle
           (if (not (tblsearch "style" "Gen-Text")) (command "-style" "Gen-Text" "Arial.ttf" 2.5 "1" 0 "n" "n"))
           
   ; to create new layer 

           (if (not (tblsearch "layer" "Borehole Locations"))
                    (command "-LAYER" "N" "Borehole Locations" "C" "7" "Borehole Locations" "LT" "Continuous" "Borehole Locations""LW" "0.00" "Borehole Locations" ""))      
                    
                    
   ;;; create NO numbers
   
    (setq ptlist nil) ; for while command
    
       (while     
         (progn 
    
           (setq PT (getpoint "\nPick borehole location: ")) ;;; input text location
           
           (if (< numfb 10.0) (setq numfb1 (strcat "0" (rtos numfb 2 0))))
           (if (>= numfb 10.0) (setq numfb1 (rtos numfb 2 0)))

           (if (< snum 10.0) (setq snum1 (strcat "0" (rtos snum 2 0))))
           (if (>= snum 10.0) (setq snum1 (rtos snum 2 0)))

           (bhb) ;create block
           
           (setq mh-text (strcat nab numfb1 "-" snum1)) ; combine text into one variable
           
           (if (not (= pt nil))(command "CLAYER" "Borehole Locations"))
           (if (not (= pt nil))  (command "-insert" "BHBLK" pt "1" "1" "0" mh-text)) ;if
           (if (not (= pt nil))(setvar "clayer" clay))
           (if (not (= pt nil))(setq snum (+ snum 1))) ; for increment
           (if (not (= pt nil))(setq nb-ns (+ numfb 1)))
           
           (setq ptlist (append ptlist (list pt))) ; to stop while command
            
          ) ;progn  
        ) ;while       
        
(princ)
) ;defun


(defun c:RES ()

   (setq nameb "")
   (prompt "\nPrefix Text Variable Reseted")
   
(princ)
) ;defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;---------- sub function for Table----------

(defun BHTable (/ dpth)        
        
	(setq LEN (length CORDS))
	(setq CORDS (acad_strlsort CORDS))			;;;sorts list into order
	(setq CNT 0)
	(if (= (getvar "tilemode") 1) (setvar "tilemode" 0))
	(command "pspace")
	
   ;;; input  depth 
        
        (if (not dpthf) (setq dpthf "25m"))
        (setq dpth (getstring (strcat "\nEnter borehole depth <" dpthf ">: ")))
        (if (= dpth "") (setq dpth dpthf) (setq dpthf dpth))	
	
 ;;; pick strating point 
 
	(setq SP (getpoint "\nPick start point for table"))
	
        (setq ht 2.5) ;; text hieght
        
        (command "-style" "Gen-Text" "Arial.ttf" 2.5 "1" 0 "n" "n")
        (if (not (tblsearch "layer" "Borehole Table")) 
        (command "-LAYER" "N" "Borehole Table" "C" "7" "Borehole Table" "LT" "Continuous" "Borehole Table""LW" "0.00" "Borehole Table" ""))
		
	(if (/= SP nil)						;;;checks for null input
	  (progn
	    (setq TXTX (car SP))				;;;gets x coord of text start point
	    (setq fx txtx)                                      ;;; set first x value
	   
	    (setq TXTY (cadr SP))				;;;gets y coord
	    (setq fy TXTY)
	    
	    (setq encw 25.00)  ; borehole Column width
            (setq nocw 20.00)  ; number Column width            
            
            (setq ten (/ encw 2))
            (setq tno (+ (/ nocw 2) encw))
	  
     ;; place borehole text
	    (entmake 
	      (list 
	        (cons 0 "text") 
	        (cons 1 "BOREHOLE DETAILS") 
	        (cons 7 "Gen-Text") 
	        (cons 8 "Borehole Table")
	        (cons 10 (list TXTX (+ TXTY (/ ht 2) (* ht 2)))) 
	        (cons 11 (list TXTX (+ TXTY (/ ht 2) (* ht 2)))) 
	        (cons 40 3.0) 
	        (cons 50 0.0) 
	        (cons 72 4)
	      )
	    )
     
	    (entmake 
	      (list 
	        (cons 0 "text") 
	        (cons 1 "NO") 
	        (cons 7 "Gen-Text") 
	        (cons 8 "Borehole Table")
	        (cons 10 (list (- TXTX tno) TXTY)) 
	        (cons 11 (list (- TXTX tno) TXTY)) 
	        (cons 40 ht) 
	        (cons 50 0.0) 
	        (cons 72 4)
	      )
	    )
	        
	    (entmake 
	      (list 
	        (cons 0 "text") 
	        (cons 1 "EASTING") 
	        (cons 7 "Gen-Text") 
	        (cons 8 "Borehole Table")
	        (cons 10 (list (- TXTX ten) TXTY)) 
	        (cons 11 (list (- TXTX ten) TXTY)) 
	        (cons 40 ht) 
	        (cons 50 0.0) 
	        (cons 72 4)
	      )
	    )  
	        
	    (entmake 
	      (list 
	        (cons 0 "text") 
	        (cons 1 "NORTHING") 
	        (cons 7 "Gen-Text") 
	        (cons 8 "Borehole Table")
	        (cons 10 (list (+ TXTX ten) TXTY)) 
	        (cons 11 (list (+ TXTX ten) TXTY)) 
	        (cons 40 ht) 
	        (cons 50 0.0) 
	        (cons 72 4)
	      )
	    ) 
	    
	    (entmake 
	      (list 
	        (cons 0 "text") 
	        (cons 1 "DEPTH") 
	        (cons 7 "Gen-Text") 
	        (cons 8 "Borehole Table")
	        (cons 10 (list (+ TXTX tno) TXTY)) 
	        (cons 11 (list (+ TXTX tno) TXTY)) 
	        (cons 40 ht) 
	        (cons 50 0.0) 
	        (cons 72 4)
	      )
	    )
     
     ;; place borehole horizontal table lines
	    (entmake 
	      (list 
	        (cons 0 "line") 
	        (cons 8 "Borehole Table") 
	        (cons 10 (list (- TXTX (+ encw nocw)) (+ TXTY ht)))
	        (cons 11 (list (+ TXTX nocw encw) (+ TXTY ht)))
	      )
	    )
	     
	    (entmake 
	      (list 
	        (cons 0 "line") 
	        (cons 8 "Borehole Table") 
	        (cons 10 (list (- TXTX (+ encw nocw)) (- TXTY ht)))
	        (cons 11 (list (+ TXTX nocw encw) (- TXTY ht)))
	      )
	    )
	  
	  (repeat LEN
		(setq TXTY (- TXTY (* 2 HT)))			;;;set new y coord for text
		
		(setq SP (list TXTX TXTY))			;;;creates code start point
		(setq CORD (nth CNT CORDS))			;;;gets coord from list
		(setq COLEN (strlen CORD))			;
		(setq COM 1 GAP 1)	
				
		(while (/= COLEN COM)						;
			(setq COM1 (substr CORD COM 1))				;finds ',' in strings for
			(if (and (= COM1 ",") (= GAP 1)) (setq S1 COM GAP 2))	;spliting string
			(if (and (= COM1 ",") (= GAP 2)) (setq S2 COM))		;
			(setq COM (+ COM 1))				;
		) ;while
		
		(setq CODE (substr CORD 1 (- S1 1)))		;;;strips of code
		(setq SON (substr CORD (+ S1 1) (- S2 S1 1)))	;;;strips of north
		(setq SOE (substr CORD (+ S2 1) (- COLEN S2)))	;;;strips of east
		
	        (entmake 
	          (list 
	            (cons 0 "text") 
	            (cons 1 code) 
	            (cons 7 "Gen-Text") 
	            (cons 8 "Borehole Table")
	            (cons 10 (list (- TXTX tno) TXTY))
	            (cons 11 (list (- TXTX tno) TXTY)) 
	            (cons 40 ht) 
	            (cons 50 0.0) (cons 72 4)
	          )
	        )
	        
	        (entmake 
	          (list 
	            (cons 0 "text") 
	            (cons 1 soe) 
	            (cons 7 "Gen-Text") 
	            (cons 8 "Borehole Table")
	            (cons 10 (list (- TXTX ten) TXTY)) 
	            (cons 11 (list (- TXTX ten) TXTY)) 
	            (cons 40 ht) 
	            (cons 50 0.0) 
	            (cons 72 4)
	          )
	        )
	  	
	  	(entmake 
	  	  (list 
	  	    (cons 0 "text") 
	  	    (cons 1 son) (cons 7 "Gen-Text") 
	  	    (cons 8 "Borehole Table")
	  	    (cons 10 (list (+ TXTX ten) TXTY)) 
	  	    (cons 11 (list (+ TXTX ten) TXTY)) 
	  	    (cons 40 ht) 
	  	    (cons 50 0.0) 
	  	    (cons 72 4)
	  	  )
	  	)
	  	
	        (entmake 
	          (list 
	            (cons 0 "text") 
	            (cons 1 dpth) 
	            (cons 7 "Gen-Text") 
	            (cons 8 "Borehole Table")
	            (cons 10 (list (+ TXTX tno) TXTY))
	            (cons 11 (list (+ TXTX tno) TXTY)) 
	            (cons 40 ht) 
	            (cons 50 0.0) (cons 72 4)
	          )
	        )	  	
	  	  	  
                (entmake 
                  (list 
                    (cons 0 "line") 
                    (cons 8 "Borehole Table") 
                    (cons 10 (list (- TXTX (+ encw nocw)) (- TXTY ht)))
                    (cons 11 (list (+ TXTX nocw encw) (- TXTY ht)))
                  )
                ) ;; horizontal lines
		
		(setq hl (entlast)) ; set hl as last horizontal line		
	
		(setq CNT (+ CNT 1))
		
	    ) ;repeat
	    
                (setq ly (caddr (assoc 10 (entget hl)))) ;set last y value
                
      ;; place borehole vertical table lines
               (entmake 
                  (list 
                    (cons 0 "line") 
                    (cons 8 "Borehole Table") 
                    (cons 10 (list (- TXTX encw) (+ fy ht))) 
                    (cons 11 (list (- TXTX encw) ly))
                  )
               )
               
               (entmake 
                  (list 
                    (cons 0 "line") 
                    (cons 8 "Borehole Table") 
                    (cons 10 (list TXTX (+ fy ht))) 
                    (cons 11 (list TXTX ly))
                  )
               )               
               
               (entmake 
                  (list 
                    (cons 0 "line") 
                    (cons 8 "Borehole Table") 
                    (cons 10 (list (+ TXTX encw) (+ fy ht))) 
                    (cons 11 (list (+ TXTX encw) ly))
                  )
               )
	       
	       (entmake
	          (list
	            (cons 0 "LWPOLYLINE")
	            (cons 100 "AcDbEntity")
	            (cons 100 "AcDbPolyline")
	            (cons 8 "Borehole Table")
	            (cons 90 4)
	            (cons 70 1)
	            (cons 10 (list (- fx (+ encw nocw)) (+ fy (* ht 4))))
	            (cons 10 (list (+ fx (+ nocw encw)) (+ fy (* ht 4))))
	            (cons 10 (list (+ fx (+ nocw encw)) ly))
	            (cons 10 (list (- fx (+ encw nocw)) ly))
	          )
               ) ; inner rectangle
	
	       (entmake
	          (list
	            (cons 0 "LWPOLYLINE")
	            (cons 100 "AcDbEntity")
	            (cons 100 "AcDbPolyline")
	            (cons 8 "Borehole Table")
	            (cons 90 4)
	            (cons 70 1)
	            (cons 10 (list (- fx (+ encw nocw 1)) (+ fy (* ht 4) 1)))
	            (cons 10 (list (+ fx (+ nocw encw 1)) (+ fy (* ht 4) 1)))
	            (cons 10 (list (+ fx (+ nocw encw 1)) (- ly 1)))
	            (cons 10 (list (- fx (+ encw nocw 1)) (- ly 1)))
	          )
               ) ; outer rectangle	
	
	 (command "erase" hl "")
	
	  ) ; progn
	) ;if 
	(command "redraw")
	(princ)
	
) ; defun


;;-------------Main function to make List of points-----

(defun c:BHT (/ txtx txty len cord cords cnt sp ht code son soe sox soy so1 encw nocw ten tno lat hl ly fx fy)

	(setvar "cmdecho" 0)
	
	(setq temperr *error*)
        (setq *error* trap3)
        
	(setq CORDS nil LEN nil CNT 0)	;;resets coord list to nil
	(princ (strcat "\n "))
	
	(initget 1 "All Select")	 
	(setq sel (strcase (getkword "\Select individual borehole points or Select All (S or A): ")))
	(if (= sel "SELECT") (setq SS (ssget '((2 . "BHBLK")))) (setq SS (ssget "X" '((2 . "BHBLK")))))
	  
	(command "UCS" "WORLD")
	
	(while (/= SS nil)					;;;checks for nil selection
	  (setq LEN (sslength SS))
	    (repeat LEN
		(setq SO0 (ssname SS CNT))
		(setq CORD (cdr (assoc '10 (entget SO0))))	;;;gets coords of point
		(setq SOX (rtos (car CORD) 2 3))		;;;strips off X coord
		(setq SOY (rtos (cadr CORD) 2 3))		;;;strips off Y coord
		(setq SO1 (entnext SO0))			;;;gets attribute entity
		(setq CODE (cdr (assoc '1 (entget SO1))))	;;;strips off point code from attribute
		(setq CORD (strcat CODE "," SOY "," SOX))	;;;creates string of code,y,x
		(setq CORDL (list CORD))			;;;converts into list
		(if (= CORDS nil) (setq CORDS CORDL) (setq CORDS (append CORDL CORDS)))	;;;starts new list or adds to old
		(setq CNT (+ CNT 1))
	    )
	  (setq SS nil)						;;;finishes loop
	) ;while
	
	(command "UCS" "P")
	
	(if (/= (length CORDS) 0) (BHTable))
	
	(setq *error* temperr)
	(prompt "\n Borehole Table is Placed\n © Bijoy Manoharan 2011 www.cadlispandtips.com")
	(princ)
) ;defun


;;------------- end Main function --------------------

(alert "--------------------- Boreholes with Table (Annotative)-----------------------

\n Commands                                           
\n    Command   BN   ( For Increment Borehole Number )
\n    Command   BSN  ( For Increment Borehole Sub Number )
\n    Command   RES  ( To Reset Prefix Text Variable )
\n    Command   BHT  ( To Place Borehole Table )
\n Steps
\n 1. Enter Prefix Text
\n 2. Enter Starting Number
\n 3. Pick Text Location
\n 4. After Placing Borehole Locations run Command BHT to place table   
\n 5. Type A to select all Boreholes
\n 6. Type S to select individual Boreholes
\n 7. Pick a point to place Borehole Table.

\nBijoy Manoharan\nOct 2011\nwww.cadlispandtips.com")
