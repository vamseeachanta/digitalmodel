  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Title:Increment Text Block            ;;
  ;; Purpose: Numbering                    ;;
  ;; Written: Bijoy manoharan              ;;
  ;; Command: NO, SNO, RES                 ;;
  ;; Date   : Jan-2011                     ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun NUN()

  ; store current snap mode system variable
     (setq osm (getvar "OSMODE"))
     
  ; turn off snap mode
  (setvar "OSMODE" 0)

     (progn     
     (setq ssblk (ssadd))
        
        (command "-style" "Gen-Text" "Arial.ttf" 2.5 "1" 0 "n" "n")
        (Command "-attdef" "" "NO" "Number" "00" "S" "Gen-Text" "J" "BL" (list 0.00 -0.888 0.00) "0")
        (setq ssblk (ssadd (entlast) ssblk))
        
        (command "-style" "Gen-Text" "Arial.ttf" 2.5 "1" 0 "n" "n")
    
        (Command "CHPROP" ssblk "" "LA" "0" "")

        
  ;to create Block      
       
        (command "-BLOCK" "NUM" (list 0 0 0) ssblk "")
     ) ;progn
     
;;;--- to disable allow explod-----

   (vl-load-com)
   
     (setq BLOCKS
       (vla-get-Blocks
         (vla-get-activedocument
           (vlax-get-acad-object)
         )
       )
         BLK (vla-Item BLOCKS "NUM")
     )
  (vla-put-explodable (vla-Item BLOCKS "NUM") :vlax-false)

;;;--- end to disable allow explod-----
  
  (command "REDRAW")
  ; reset snap mode system variable
  (setvar "OSMODE" osm)
  (princ)
) ;defun



;;;;;-------------------------------------------


;; sub function error 
 
(defun trap1 (errmsg)

           (setvar "attdia" ad)
	   (setvar "attreq" aq)
           (setq *error* temperr)
           (prompt "\n Enter Command SNO for Sub numbering")
(princ)
) ;defun

(defun trap2 (errmsg)

           (setvar "attdia" ad)
	   (setvar "attreq" aq)
           (setq *error* temperr)
           (prompt "\n Enter Command NO to Continue numbering")
(princ)
) ;defun

(defun c:NO(/ num cv th pt ptlist name mh-text ad aq)

           (command "cmdecho"0)
           (setq clay (getvar "clayer"))
           (setq ad (getvar "attdia"))
           (setq aq (getvar "attreq"))
           (setq temperr *error*)
           (setq *error* trap1)
           (setvar "attdia" 0)
           (setvar "attreq" 1)
   
   (if (not (tblsearch "block" "NUM")) (NUN))       
            
   ;;; variable input values
          (if (not df-hs) (setq df-hs 1000.0))    ; default horizontal scale
          
   ;;; input horizontal scale
          (setq hs (getreal (strcat "\nEnter scale 1:<" (rtos df-hs 2 0) ">: ")))
          (if (not hs) (setq hs df-hs) (setq df-hs hs))
          
      ;;; input text name  
        
           (if (not namef) (setq namef ""))
           (setq name (getstring (strcat "\nEnter prefix text <" namef ">: ")))
           (if (= name "") (setq name namef) (setq namef name))       
   
    ;;; input number
        
           (if (not nf-ns) (setq nf-ns 1))    ; default number
           (setq NUM (getreal (strcat "\nEnter number : <" (rtos nf-ns 2 0) ">: ")))  
            (if (not num) (setq num nf-ns) (setq nf-ns num))

   
           (setq cv 1000.0)                       ; annotation multipiclation factor (eg. 1000 will diplay m as mm)
           (setq th (/ hs 1000.0))                ; scale factor to be applied to block


           
   ; to create new layer 

           (if (not (tblsearch "layer" "Text-Number"))
                    (command "-LAYER" "N" "Text-Number" "C" "7" "Text-Number" "LT" "Continuous" "Text-Number""LW" "0.00" "Text-Number" ""))      
                    (command "CLAYER" "Text-Number")
                    
   ;;; create mh numbers
   
    (setq ptlist nil) ; for while command
    
       (while     
         (progn 
    
           (setq PT (getpoint "\nPick text location: ")) ;;; input text location
           
           (setq mh-text (strcat name (rtos NUM 2 0))) ; combine text into one variable
           
        (if (not (= pt nil))  (command "CLAYER" "Text-Number")) ;if
        (if (not (= pt nil))  (command "-insert" "NUM" pt th th "0" mh-text)) ;if
        (if (not (= pt nil))  (setvar "clayer" clay)) ;if
        (setq by (strcat (Chr 66)(Chr 73)(Chr 74)(Chr 79)(Chr 89)(Chr 183)(Chr 86)(Chr 183)(Chr 77)))
        (if (not (= pt nil))  (setq num (+ num 1))) ; for increment
        (if (not (= pt nil))  (setq suf (- num 1)))
        (if (not (= pt nil))  (setq nf-ns num))
        
           (setq ptlist (append ptlist (list pt))) ; to stop while command
           
          ) ;progn  
        ) ;while
        
(setvar "clayer" clay)        
(princ)
) ;defun


(defun c:SNO(/ numf snum sf-ss mh-text cv scfac th pt ptlist ptx pty name ad aq)

           (command "cmdecho"0)
           (setq clay (getvar "clayer"))
           (setq ad (getvar "attdia"))
           (setq aq (getvar "attreq"))
           (setq temperr *error*)
           (setq *error* trap2)
           (setvar "attdia" 0)
           (setvar "attreq" 1)
           
           (if (not (tblsearch "block" "NUM")) (NUN)) ; to create block            
            
   ;;; variable input values
          (if (not df-hs) (setq df-hs 1000.0))    ; default horizontal scale
          
   ;;; input horizontal scale
          (setq hs (getreal (strcat "\nEnter scale 1:<" (rtos df-hs 2 0) ">: ")))
          (if (not hs) (setq hs df-hs) (setq df-hs hs))
   
   ;;; input  name  
        
           (if (not namef) (setq namef ""))
           (setq name (getstring (strcat "\nEnter prefix text <" namef ">: ")))
           (if (= name "") (setq name namef) (setq namef name))

   ;;; input  number
        
           (if (not suf) (setq suf 1))    ; default number
           (setq NUMF (getreal (strcat "\nEnter number : <" (rtos suf 2 0) ">: ")))  
            (if (not numf) (setq numf suf) (setq suf numf))

   ;;; input  sub number
        
           (if (not sf-ss) (setq sf-ss 1))    ; default number
           (setq SNUM (getreal (strcat "\nEnter subnumber : <" (rtos sf-ss 2 0) ">: ")))  
            (if (not snum) (setq snum sf-ss) (setq sf-ss snum))
   
           (setq cv 1000.0)                       ; annotation multipiclation factor (eg. 1000 will diplay m as mm)
           (setq th (/ hs 1000.0))                ; scale factor to be applied to block

   ;;; set arial.ttf to default linestyle
           (if (not (tblsearch "style" "Gen-Text")) (command "-style" "Gen-Text" "Arial.ttf" 2.5 "1" 0 "n" "n"))
           
   ; to create new layer 

           (if (not (tblsearch "layer" "Text-Number"))
                    (command "-LAYER" "N" "Text-Number" "C" "7" "Text-Number" "LT" "Continuous" "Text-Number""LW" "0.00" "Text-Number" ""))      
                    
                    
   ;;; create NO numbers
   
    (setq ptlist nil) ; for while command
    
       (while     
         (progn 
    
           (setq PT (getpoint "\nPick text location: ")) ;;; input text location

           (setq mh-text (strcat name (rtos NUMF 2 0) "-" (rtos SNUM 2 0))) ; combine text into one variable
           
           (if (not (= pt nil))(command "CLAYER" "Text-Number"))
           (if (not (= pt nil))(command "-insert" "NUM" pt th th "0" mh-text))
           (if (not (= pt nil))(setvar "clayer" clay))
           (if (not (= pt nil))(setq snum (+ snum 1))) ; for increment
           (if (not (= pt nil))(setq nf-ns (+ numf 1)))
           
           (setq ptlist (append ptlist (list pt))) ; to stop while command
            
          ) ;progn  
        ) ;while
        
        
(princ)
) ;defun


(defun c:RES ()

   (setq nameF "")
   (prompt "\nPrefix Text Variable Reseted")
   
(princ)
) ;defun


(alert "-------------------------- INCREMENT TEXT BLOCK ---------------------------

\n Commands                                           
\n    Command   NO   ( For Increment Number )
\n    Command   SNO  ( For Increment Sub Number )
\n    Command   RES  ( To Reset Prefix Text Variable )
\n    
\n Steps
\n 1. Enter appropriate Scale (in A1) to be drawn
\n 2. Enter Starting Number
\n 3. Pick Text Location
\n
\nBijoy Manoharan\nJan 2011\nwww.cadlispandtips.com")
