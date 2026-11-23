    

  
               
                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                      ;;                                           ;;
                      ;; Title  : Import coordinate                ;;
                      ;; Purpose: Import coordinate & create table ;;
                      ;; Written: Bijoy Manoharan                  ;;
                      ;; Command: IMPO, CRT                        ;;
                      ;; Date   : May-2012                         ;;
                      ;;                                           ;;
                      ;; Website: www.cadlispandtips.com           ;;
                      ;;                                           ;; 
                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; ----------------------------------sub function error--------------------------------
 
(defun trap1 (errmsg)

           (setvar "attdia" ad)
	   (setvar "attreq" aq)
           (setq *error* temperr)
           ;(command "UCS" "P")
           (prompt "\n *** File not Found or Incorrect Format ***\n © Bijoy.v.m 2012 www.cadlispandtips.com")
(princ)
) ;defun

;;-----------------------------------sub function to create block-----------------------


;;;--- create block function start -----

(defun crb ( )
    
    (if (not (tblsearch "BLOCK" "CRBLK"))
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
                    (cons 2 "CRBLK")
                    (cons 70 2)
                    (cons 4 "Block to Place Coordinate Points")
                    (list 10 0.0 0.0 0.0)
                )
            )
            (entmake
                (list
                    (cons 0 "CIRCLE")
                    (cons 8 "0")
                    (cons 370 0)
                    (list 10 0.0 0.0 0.0)
                    (cons 40 1.25)
                )
            )
            (entmake
                (list
                    (cons 0 "ATTDEF")
                    (cons 8 "0")
                    (cons 370 0)
                    (cons 7 "Gen-Text")
                    (list 10 3.0 2.5 0.0)
                    (list 11 3.0 2.5 0.0)
                    (cons 40 2.5)
                    (cons 1 "00")
                    (cons 3 "Coordinate Point")
                    (cons 2 "00")
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
            
   ;;;--- To set block units in metre 70-6
	              
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
	                   (entget (cdr (assoc 330 (entget (tblobjname "BLOCK" "CRBLK")))))
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
                (entget (cdr (assoc 330 (entget (tblobjname "BLOCK" "CRBLK")))))
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
         BLK (vla-Item BLOCKS "CRBLK")
       )
      (vla-put-explodable (vla-Item BLOCKS "CRBLK") :vlax-false)
   
;;;--- end to disable allow explod-----
   
   (princ)
)

;;;--- create function block end -----




;; ----------------------------------Sub function--------------------------------

(DEFUN IMPPS (/ TXTF)
  (SETQ TXTF (GETVAR "dwgname"))
  (IF (WCMATCH (STRCASE TXTF) "*`.DWG")
      (SETQ
      TXTF
      (STRCAT (GETVAR "dwgprefix") TXTF)
      TXTF
      (SUBSTR TXTF 1 (- (STRLEN TXTF) 4))
    ) 
  )
  TXTF
)


;; ----------------------------------Main function--------------------------------

(defun C:IMPO (/ CRDS ENCR ENNO ENNO1 CRPT DATA)

           (command "cmdecho"0)
           (setq clay (getvar "clayer"))
           (setq ad (getvar "attdia"))
           (setq aq (getvar "attreq"))
           (setq temperr *error*)
           (setq *error* trap1)
           (setvar "attdia" 0)
           (setvar "attreq" 1)


   (SETQ Data (GETFILED "Select Text File to Import" (IMPPS) "" 0))

   (SETQ CRDS (OPEN Data "r"))


   (setq ENCR 0)
   (command "UCS" "WORLD")
  
 (while (/= ENCR nil)
      (setq ENCR (read-line CRDS)) ;Read ENCR from input file
      
     
 (if (/= ENCR nil)
         (progn
            (crb) ;create block            
            (setq ENCR (strcat "(" ENCR ")")) ;Correct format
            (setq ENCR (read ENCR))           ;Convert to list
            (setq ENNO (nth 0 ENCR))          ;Get the CRPT number

       (cond 
             ((= (type ENNO) 'STR)       
              (setq ENNO1 ENNO)
             )

             ((and (< ENNO 10.0)(= (type ENNO) 'INT))       
                   (setq ENNO1 (strcat "0" (rtos ENNO 2 0)))
             )

             ((and (>= ENNO 10.0)(= (type ENNO) 'INT))       
                   (setq ENNO1 (rtos ENNO 2 0))
             )
        
      
        ) ;cond
            
            (setq CRPT
               (list                  
                  (nth 1 ENCR) ;Get easting
                  (nth 2 ENCR) ;Get northing
               )
            )

 ;create new layer 

           (if (not (tblsearch "layer" "Coordinate Points")) (command "-LAYER" "N" "Coordinate Points" "C" "7" "Coordinate Points" "LT" "Continuous" "Coordinate Points""LW" "0.00" "Coordinate Points" ""))      
           (command "CLAYER" "Coordinate Points")                      
           (command "-insert" "CRBLK" "_NON" CRPT "1" "1" "0" ENNO1)           
        )
      )
   )
   
   (command "UCS" "P")   
   (setvar "clayer" clay)
   (close CRDS)
   (prompt "\n Zoom Extend to View All Points & Enter Command CRT to Create Table....")

) ;defun

;;---------- sub function for Table----------

(defun CRTable ()        
        
	(setq LEN (length CORDS))
	(setq CORDS (acad_strlsort CORDS))			;;;sorts list into order
	(setq CNT 0)
	(if (= (getvar "tilemode") 1) (setvar "tilemode" 0))
	(command "pspace")
	
	(setq SP (getpoint "\nPick start point for table"))
	
        (setq ht 2.5) ;; text hieght
        
        (command "-style" "Gen-Text" "Arial.ttf" 2.5 "1" 0 "n" "n")
        (if (not (tblsearch "layer" "Coordinate Table")) 
        (command "-LAYER" "N" "Coordinate Table" "C" "7" "Coordinate Table" "LT" "Continuous" "Coordinate Table""LW" "0.00" "Coordinate Table" ""))
		
	(if (/= SP nil)						;;;checks for null input
	  (progn
	    (setq TXTX (car SP))				;;;gets x coord of text start point
	    (setq fx txtx)                                      ;;; set first x value
	   
	    (setq TXTY (cadr SP))				;;;gets y coord
	    (setq fy TXTY)
	    
	    (setq encw 25.00)  ; easting & northing Column width
            (setq nocw 20.00)  ; number Column width            
            
            (setq ten (/ encw 2))
            (setq tno (+ (/ nocw 2) ten))
	  
     ;; place easting & northing text
	    (entmake 
	      (list 
	        (cons 0 "text") 
	        (cons 1 "COORDINATES") 
	        (cons 7 "Gen-Text") 
	        (cons 8 "Coordinate Table")
	        (cons 10 (list (+ TXTX 2.5) (+ TXTY (/ ht 2) (* ht 2)))) 
	        (cons 11 (list (+ TXTX 2.5) (+ TXTY (/ ht 2) (* ht 2)))) 
	        (cons 40 3.0) 
	        (cons 50 0.0) 
	        (cons 72 4)
	      )
	    )
     
	    (entmake 
	      (list 
	        (cons 0 "text") 
	        (cons 1 "POINTS") 
	        (cons 7 "Gen-Text") 
	        (cons 8 "Coordinate Table")
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
	        (cons 8 "Coordinate Table")
	        (cons 10 (list TXTX TXTY)) 
	        (cons 11 (list TXTX TXTY)) 
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
	        (cons 8 "Coordinate Table")
	        (cons 10 (list (+ TXTX encw) TXTY)) 
	        (cons 11 (list (+ TXTX encw) TXTY)) 
	        (cons 40 ht) 
	        (cons 50 0.0) 
	        (cons 72 4)
	      )
	    )      
     
     ;; place easting & northing horizontal table lines
	    (entmake 
	      (list 
	        (cons 0 "line") 
	        (cons 8 "Coordinate Table") 
	        (cons 10 (list (- TXTX (+ ten nocw)) (+ TXTY ht)))
	        (cons 11 (list (+ TXTX ten encw) (+ TXTY ht)))
	      )
	    )
	     
	    (entmake 
	      (list 
	        (cons 0 "line") 
	        (cons 8 "Coordinate Table") 
	        (cons 10 (list (- TXTX (+ ten nocw)) (- TXTY ht)))
	        (cons 11 (list (+ TXTX ten encw) (- TXTY ht)))
	      )
	    )
	  
	  (repeat LEN
		(setq TXTY (- TXTY (* 2 HT)))			;;;set new y coord for text
		
		(setq SP (list TXTX TXTY))			;;;creates code start point
		(setq CORD (nth CNT CORDS))			;;;gets coord from list
		(setq COLEN (strlen CORD))		
		(setq COM 1 GAP 1)	
				
		(while (/= COLEN COM)						
			(setq COM1 (substr CORD COM 1))				
			(if (and (= COM1 ",") (= GAP 1)) (setq S1 COM GAP 2))	
			(if (and (= COM1 ",") (= GAP 2)) (setq S2 COM))		
			(setq COM (+ COM 1))				
		) ;while
		
		(setq CODE (substr CORD 1 (- S1 1)))		;;;strips of code
		(setq SON (substr CORD (+ S1 1) (- S2 S1 1)))	;;;strips of north
		(setq SOE (substr CORD (+ S2 1) (- COLEN S2)))	;;;strips of east
		
	        (entmake 
	          (list 
	            (cons 0 "text") 
	            (cons 1 code) 
	            (cons 7 "Gen-Text") 
	            (cons 8 "Coordinate Table")
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
	            (cons 8 "Coordinate Table")
	            (cons 10 (list TXTX TXTY)) 
	            (cons 11 (list TXTX TXTY)) 
	            (cons 40 ht) 
	            (cons 50 0.0) 
	            (cons 72 4)
	          )
	        )
	  	
	  	(entmake 
	  	  (list 
	  	    (cons 0 "text") 
	  	    (cons 1 son) (cons 7 "Gen-Text") 
	  	    (cons 8 "Coordinate Table")
	  	    (cons 10 (list (+ TXTX encw) TXTY)) 
	  	    (cons 11 (list (+ TXTX encw) TXTY)) 
	  	    (cons 40 ht) 
	  	    (cons 50 0.0) 
	  	    (cons 72 4)
	  	  )
	  	)
  	  
                (entmake 
                  (list 
                    (cons 0 "line") 
                    (cons 8 "Coordinate Table") 
                    (cons 10 (list (- TXTX (+ ten nocw)) (- TXTY ht)))
                    (cons 11 (list (+ TXTX ten encw) (- TXTY ht)))
                  )
                ) ;; horizontal lines
		
		(setq hl (entlast)) ; set hl as last horizontal line		
	
		(setq CNT (+ CNT 1))
		
	    ) ;repeat
	    
                (setq ly (caddr (assoc 10 (entget hl)))) ;set last y value
                
      ;; place easting & northing vertical table lines
               (entmake 
                  (list 
                    (cons 0 "line") 
                    (cons 8 "Coordinate Table") 
                    (cons 10 (list (- fx ten) (+ fy ht))) 
                    (cons 11 (list (- fx ten) ly))
                  )
               )
               
               (entmake 
                  (list 
                    (cons 0 "line") 
                    (cons 8 "Coordinate Table") 
                    (cons 10 (list (+ fx ten) (+ fy ht))) 
                    (cons 11 (list (+ fx ten) ly))
                  )
               )
	       
	       (entmake
	          (list
	            (cons 0 "LWPOLYLINE")
	            (cons 100 "AcDbEntity")
	            (cons 100 "AcDbPolyline")
	            (cons 8 "Coordinate Table")
	            (cons 90 4)
	            (cons 70 1)
	            (cons 10 (list (- fx (+ ten nocw)) (+ fy (* ht 4))))
	            (cons 10 (list (+ fx (+ ten encw)) (+ fy (* ht 4))))
	            (cons 10 (list (+ fx (+ ten encw)) ly))
	            (cons 10 (list (- fx (+ ten nocw)) ly))
	          )
               ) ; inner rectangle
	
	       (entmake
	          (list
	            (cons 0 "LWPOLYLINE")
	            (cons 100 "AcDbEntity")
	            (cons 100 "AcDbPolyline")
	            (cons 8 "Coordinate Table")
	            (cons 90 4)
	            (cons 70 1)
	            (cons 10 (list (- fx (+ ten nocw 1)) (+ fy (* ht 4) 1)))
	            (cons 10 (list (+ fx (+ ten encw 1)) (+ fy (* ht 4) 1)))
	            (cons 10 (list (+ fx (+ ten encw 1)) (- ly 1)))
	            (cons 10 (list (- fx (+ ten nocw 1)) (- ly 1)))
	          )
               ) ; outer rectangle	
	
	 (command "erase" hl "")
	
	  ) ; progn
	) ;if 
	(command "redraw")
	(princ)
	
) ; defun


;;-------------Main function to make List of points-----

(defun c:CRT (/ txtx txty len cord cords cnt sp ht code son soe sox soy so1 encw nocw ten tno lat hl ly fx fy)

	(setvar "cmdecho" 0)
	
	(setq temperr *error*)
        (setq *error* trap3)
        
	(setq CORDS nil LEN nil CNT 0)	;;resets coord list to nil
	(princ (strcat "\n "))
	
	(initget 1 "All Select")	 
	(setq sel (strcase (getkword "\Select individual coordinate points or Select All (S or A): ")))
	(if (= sel "SELECT") (setq SS (ssget '((2 . "crblk")))) (setq SS (ssget "X" '((2 . "crblk")))))
	  
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
	  (setq SS nil)						
	) ;while
	
	(command "UCS" "P")
	
	(if (/= (length CORDS) 0) (CRTable))
	
	(setq *error* temperr)
	(prompt "\n Coordinate Table is Placed\n © Bijoy.v.m 2012 www.cadlispandtips.com")
	(princ)
) ;defun


;;------------- end Main function --------------------

(alert "------------------- Import Coordinates & Export to Table (Annotative) ----------------------

\n Commands                                           
\n    Command :  IMPO ( For Import Coordinate Data Points )
\n    Command :  CRT  ( To Create Coordinate Table )
\n Steps
\n 1. Select Coordinate Data TXT File
\n 2. After Importing Coordinate Points run Command CRT to place table   
\n 3. Type A to select all coordinate points
\n 4. Type S to select individual coordinate points
\n 5. Pick a point to place Coordinate Table.
\n Note
\n TXT File Format : Number  Easting  Northing
\n eg.    100   00000.000  00000.000


\nBijoy.v.m\nMay 2012\nwww.cadlispandtips.com")

(princ "\nImport coordinate Lisp | © Bijoy manoharan 2012 www.cadlispandtips.com")
(princ "\nLisp Command:IMPO (to Import Coordinates Points)   ")
(princ)
