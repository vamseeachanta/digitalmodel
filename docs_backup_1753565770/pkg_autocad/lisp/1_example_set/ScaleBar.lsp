;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Title    : Scale bar                    
;; Purpose  : To Place Scale Bar 
;; Written  : Bijoy manoharan                
;; Web page : www.cadlispandtips.com 
;; Command  : sb & sbs                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------sub function error------------------------
 
 (defun trap1 (errmsg)
    (setvar "clayer" clay)
    (setvar "dimzin" di)
    (command "undo" "end")
    (setq *error* temperr)
    (prompt "\n© Bijoy manoharan 2011 www.cadlispandtips.com")
    (princ)
   ) ;defun

;;---------------------Create Blocks---------------------------

(defun SBC(/ osm ad aq ssblk x y z ) 

  (setq osm (getvar "OSMODE"))  
  (setq ad (getvar "attdia"))
  (setq aq (getvar "attreq"))
  
  (setvar "OSMODE" 0)
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  
  (if (not (tblsearch "block" "Scale-B"))
      (progn     
      (setq ssblk (ssadd))

; Set Dimensions 
      (setq X 100) ; set horizontal dimension
      (setq Y 2)   ; set Vertical dimension
      
; Draw Function      
      (setq Z 0)
      
; Rectangle      
      (command "RECTANG" (list z z z)(list x y z))
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list z (* y 0.5) z)(list x (* y 0.5) z) "") 
         (setq ssblk (ssadd (entlast) ssblk))
         
; Soild Hatches          
      (command "SOLID" (list z z z)(list z (* y 0.5) z)(list (* x 0.02) z z)(list (* x 0.02) (* y 0.5) z) "")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.02) (* y 0.5) z)(list (* x 0.02) y z)(list (* x 0.04) (* y 0.5) z)(list (* x 0.04) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))  
      (command "SOLID" (list (* x 0.04) (* y 0.5) z)(list (* x 0.04) z z)(list (* x 0.06) (* y 0.5) z)(list (* x 0.06) z z)"")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (command "SOLID" (list (* x 0.06) (* y 0.5) z)(list (* x 0.06) y z)(list (* x 0.08) (* y 0.5) z)(list (* x 0.08) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.08) (* y 0.5) z)(list (* x 0.08) z z)(list (* x 0.1) (* y 0.5) z)(list (* x 0.1) z z)"")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (command "SOLID" (list (* x 0.1) (* y 0.5) z)(list (* x 0.1) y z)(list (* x 0.15) (* y 0.5) z)(list (* x 0.15) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.15) (* y 0.5) z)(list (* x 0.15) z z)(list (* x 0.2) (* y 0.5) z)(list (* x 0.2) z z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.2) (* y 0.5) z)(list (* x 0.2) y z)(list (* x 0.3) (* y 0.5) z)(list (* x 0.3) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.3) (* y 0.5) z)(list (* x 0.3) z z)(list (* x 0.4) (* y 0.5) z)(list (* x 0.4) z z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.4) (* y 0.5) z)(list (* x 0.4) y z)(list (* x 0.6) (* y 0.5) z)(list (* x 0.6) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.6) (* y 0.5) z)(list (* x 0.6) z z)(list (* x 0.8) (* y 0.5) z)(list (* x 0.8) z z)"")
         (setq ssblk (ssadd (entlast) ssblk))  
      (command "SOLID" (list (* x 0.8) (* y 0.5) z)(list (* x 0.8) y z)(list x (* y 0.5) z)(list x y z)"")
         (setq ssblk (ssadd (entlast) ssblk))
         
; Lines     
      (command "LINE" (list (* x 0.02) y z)(list (* x 0.02) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.04) y z)(list (* x 0.04) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.06) y z)(list (* x 0.06) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.08) y z)(list (* x 0.08) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.1) y z)(list (* x 0.1) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.15) y z)(list (* x 0.15) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.2) y z)(list (* x 0.2) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.3) y z)(list (* x 0.3) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.4) y z)(list (* x 0.4) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.5) y z)(list (* x 0.5) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.6) y z)(list (* x 0.6) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.7) y z)(list (* x 0.7) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.8) y z)(list (* x 0.8) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.9) y z)(list (* x 0.9) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))

 ; Attribute texts 
      (if (not (tblsearch "style" "Gen-Text")) (command "-style" "Gen-Text" "Arial.ttf" 2.5 "1" 0 "n" "n"))
      
      (Command "-attdef" "" "1:1000" "Scale" "1:1000" "S" "Gen-Text" "J" "TL" (list z (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "L" "L" "100m" "S" "Gen-Text" "J" "C" (list x (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
      (Command "-attdef" "" "0" "L x 0.0" "0" "S" "Gen-Text" "J" "C" (list z (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "1" "L x 0.1" "10" "S" "Gen-Text" "J" "C" (list (* x 0.1) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "2" "L x 0.2" "20" "S" "Gen-Text" "J" "C" (list (* x 0.2) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "3" "L x 0.3" "30" "S" "Gen-Text" "J" "C" (list (* x 0.3) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))  
      (Command "-attdef" "" "4" "L x 0.4" "40" "S" "Gen-Text" "J" "C" (list (* x 0.4) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
      (Command "-attdef" "" "5" "L x 0.6" "60" "S" "Gen-Text" "J" "C" (list (* x 0.6) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "6" "L x 0.8" "80" "S" "Gen-Text" "J" "C" (list (* x 0.8) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))    
      
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "CHPROP" ssblk "" "LA" "0" "")
      (Command "CHPROP" ssblk "" "LW" "0.13" "")
      (Command "CHPROP" ssblk "" "C" "7" "")
      (Command "CHPROP" ssblk "" "LT" "Continuous" "")
      
      (command "-BLOCK" "Scale-B" (list (* x 0.5) z z) ssblk "") ; create block     
    
      ) ;progn
  ) ;if
  
  ;;;--- to disable allow explod-----
  
  (vl-load-com)
  (setq BLOCKS
    (vla-get-Blocks
      (vla-get-activedocument
        (vlax-get-acad-object)
      )
    )
    BLK (vla-Item BLOCKS "Scale-B")
  )
  (vla-put-explodable (vla-Item BLOCKS "Scale-B") :vlax-false)
  
;;;--- end to disable allow explod-----

  (setvar "OSMODE" osm) 
  (setvar "attdia" ad) 
  (setvar "attreq" aq)
  
  (princ)
) ;defun

;---------------------------------Sub function Scale-B end--------------------------------

(defun CSBS(/ osm ad aq ssblk x y z )

  (setq osm (getvar "OSMODE"))
  (setq ad (getvar "attdia"))
  (setq aq (getvar "attreq"))
  
  (setvar "OSMODE" 0)
  (setvar "dimzin" 8)
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  

  (if (not (tblsearch "block" "Scale-S"))
      (progn     
      (setq ssblk (ssadd))

; Set Dimensions 

      (setq X 100) ; set horizontal dimension
      (setq Y 2)   ; set Vertical dimension
      
; Draw Function      
      (setq Z 0)
      
; Rectangle      
      (command "RECTANG" (list z z z)(list (* x 0.5) y z))
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list z (* y 0.5) z)(list (* x 0.5) (* y 0.5) z) "") 
         (setq ssblk (ssadd (entlast) ssblk))
         
; Soild Hatches          
      (command "SOLID" (list z z z)(list z (* y 0.5) z)(list (* x 0.02) z z)(list (* x 0.02) (* y 0.5) z) "")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.02) (* y 0.5) z)(list (* x 0.02) y z)(list (* x 0.04) (* y 0.5) z)(list (* x 0.04) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))  
      (command "SOLID" (list (* x 0.04) (* y 0.5) z)(list (* x 0.04) z z)(list (* x 0.06) (* y 0.5) z)(list (* x 0.06) z z)"")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (command "SOLID" (list (* x 0.06) (* y 0.5) z)(list (* x 0.06) y z)(list (* x 0.08) (* y 0.5) z)(list (* x 0.08) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.08) (* y 0.5) z)(list (* x 0.08) z z)(list (* x 0.1) (* y 0.5) z)(list (* x 0.1) z z)"")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (command "SOLID" (list (* x 0.1) (* y 0.5) z)(list (* x 0.1) y z)(list (* x 0.15) (* y 0.5) z)(list (* x 0.15) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.15) (* y 0.5) z)(list (* x 0.15) z z)(list (* x 0.2) (* y 0.5) z)(list (* x 0.2) z z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.2) (* y 0.5) z)(list (* x 0.2) y z)(list (* x 0.3) (* y 0.5) z)(list (* x 0.3) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.3) (* y 0.5) z)(list (* x 0.3) z z)(list (* x 0.4) (* y 0.5) z)(list (* x 0.4) z z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.4) (* y 0.5) z)(list (* x 0.4) y z)(list (* x 0.5) (* y 0.5) z)(list (* x 0.5) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))
        
; Lines     
      (command "LINE" (list (* x 0.02) y z)(list (* x 0.02) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.04) y z)(list (* x 0.04) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.06) y z)(list (* x 0.06) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.08) y z)(list (* x 0.08) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.1) y z)(list (* x 0.1) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.15) y z)(list (* x 0.15) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.2) y z)(list (* x 0.2) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.3) y z)(list (* x 0.3) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.4) y z)(list (* x 0.4) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))

 ; Attribute texts  
      (if (not (tblsearch "style" "Gen-Text")) (command "-style" "Gen-Text" "Arial.ttf" 2.5 "1" 0 "n" "n"))
      
      (Command "-attdef" "" "1:1000" "Scale" "1:1000" "S" "Gen-Text" "J" "TL" (list z (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "L" "L" "50m" "S" "Gen-Text" "J" "C" (list (* x 0.5) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
      (Command "-attdef" "" "0" "L x 0.0" "0" "S" "Gen-Text" "J" "C" (list z (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "1" "L x 0.2" "10" "S" "Gen-Text" "J" "C" (list (* x 0.1) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "2" "L x 0.4" "20" "S" "Gen-Text" "J" "C" (list (* x 0.2) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "3" "L x 0.6" "30" "S" "Gen-Text" "J" "C" (list (* x 0.3) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))  
      (Command "-attdef" "" "4" "L x 0.8" "40" "S" "Gen-Text" "J" "C" (list (* x 0.4) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "CHPROP" ssblk "" "LA" "0" "")
      (Command "CHPROP" ssblk "" "LW" "0.13" "")
      (Command "CHPROP" ssblk "" "C" "7" "")
      (Command "CHPROP" ssblk "" "LT" "Continuous" "")
      
      (command "-BLOCK" "Scale-S" (list (* x 0.25) z z) ssblk "") ; create block
     
    
      ) ;progn
  ) ;if
  
  ;;;--- to disable allow explod-----
  
  (vl-load-com)
  (setq BLOCKS
    (vla-get-Blocks
      (vla-get-activedocument
        (vlax-get-acad-object)
      )
    )
    BLK (vla-Item BLOCKS "Scale-S")
  )
  (vla-put-explodable (vla-Item BLOCKS "Scale-S") :vlax-false)
  
;;;--- end to disable allow explod-----

  (setvar "OSMODE" osm)
  (setvar "attdia" ad) 
  (setvar "attreq" aq)
  
  (princ)
) ;defun

;-----------------------------Sub function Scale-S end-------------------------------

(defun CHVS(/ osm ad aq ssblk x y z )

  (setq osm (getvar "OSMODE"))
  (setq ad (getvar "attdia"))
  (setq aq (getvar "attreq"))
  
  (setvar "OSMODE" 0)
  (setvar "dimzin" 8)
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  

  (if (not (tblsearch "block" "Scale-HV"))
      (progn     
      (setq ssblk (ssadd))

; Set Dimensions 

      (setq X 100) ; set horizontal dimension
      (setq Y 2)   ; set Vertical dimension
      
; Draw Function      
      (setq Z 0)
      
; Rectangle      
      (command "RECTANG" (list z z z)(list x y z))
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list z (* y 0.5) z)(list x (* y 0.5) z) "") 
         (setq ssblk (ssadd (entlast) ssblk))
         
; Soild Hatches          
      (command "SOLID" (list z z z)(list z (* y 0.5) z)(list (* x 0.02) z z)(list (* x 0.02) (* y 0.5) z) "")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.02) (* y 0.5) z)(list (* x 0.02) y z)(list (* x 0.04) (* y 0.5) z)(list (* x 0.04) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))  
      (command "SOLID" (list (* x 0.04) (* y 0.5) z)(list (* x 0.04) z z)(list (* x 0.06) (* y 0.5) z)(list (* x 0.06) z z)"")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (command "SOLID" (list (* x 0.06) (* y 0.5) z)(list (* x 0.06) y z)(list (* x 0.08) (* y 0.5) z)(list (* x 0.08) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.08) (* y 0.5) z)(list (* x 0.08) z z)(list (* x 0.1) (* y 0.5) z)(list (* x 0.1) z z)"")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (command "SOLID" (list (* x 0.1) (* y 0.5) z)(list (* x 0.1) y z)(list (* x 0.15) (* y 0.5) z)(list (* x 0.15) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.15) (* y 0.5) z)(list (* x 0.15) z z)(list (* x 0.2) (* y 0.5) z)(list (* x 0.2) z z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.2) (* y 0.5) z)(list (* x 0.2) y z)(list (* x 0.3) (* y 0.5) z)(list (* x 0.3) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.3) (* y 0.5) z)(list (* x 0.3) z z)(list (* x 0.4) (* y 0.5) z)(list (* x 0.4) z z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.4) (* y 0.5) z)(list (* x 0.4) y z)(list (* x 0.6) (* y 0.5) z)(list (* x 0.6) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.6) (* y 0.5) z)(list (* x 0.6) z z)(list (* x 0.8) (* y 0.5) z)(list (* x 0.8) z z)"")
         (setq ssblk (ssadd (entlast) ssblk))  
      (command "SOLID" (list (* x 0.8) (* y 0.5) z)(list (* x 0.8) y z)(list x (* y 0.5) z)(list x y z)"")
         (setq ssblk (ssadd (entlast) ssblk))
         
; Lines     
      (command "LINE" (list (* x 0.02) y z)(list (* x 0.02) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.04) y z)(list (* x 0.04) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.06) y z)(list (* x 0.06) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.08) y z)(list (* x 0.08) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.1) y z)(list (* x 0.1) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.15) y z)(list (* x 0.15) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.2) y z)(list (* x 0.2) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.3) y z)(list (* x 0.3) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.4) y z)(list (* x 0.4) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.5) y z)(list (* x 0.5) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.6) y z)(list (* x 0.6) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.7) y z)(list (* x 0.7) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.8) y z)(list (* x 0.8) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.9) y z)(list (* x 0.9) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))

 ; Attribute texts  
      (if (not (tblsearch "style" "Gen-Text")) (command "-style" "Gen-Text" "Arial.ttf" 2.5 "1" 0 "n" "n"))
      
      (Command "-attdef" "" "1:1000" "Horizontal Scale" "(1:1000 H)" "S" "Gen-Text" "J" "BR" (list (- z 2) (+ y 0.26) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "HL" "HL" "100m" "S" "Gen-Text" "J" "C" (list x (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
      (Command "-attdef" "" "0" "HL x 0.0" "0" "S" "Gen-Text" "J" "C" (list z (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "1" "HL x 0.1" "10" "S" "Gen-Text" "J" "C" (list (* x 0.1) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "2" "HL x 0.2" "20" "S" "Gen-Text" "J" "C" (list (* x 0.2) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "3" "HL x 0.3" "30" "S" "Gen-Text" "J" "C" (list (* x 0.3) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))  
      (Command "-attdef" "" "4" "HL x 0.4" "40" "S" "Gen-Text" "J" "C" (list (* x 0.4) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
      (Command "-attdef" "" "5" "HL x 0.6" "60" "S" "Gen-Text" "J" "C" (list (* x 0.6) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "6" "HL x 0.8" "80" "S" "Gen-Text" "J" "C" (list (* x 0.8) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))  
         
      (Command "-attdef" "" "1:100" "Vertical Scale" "(1:100 V)" "S" "Gen-Text" "J" "TR" (list (- z 2) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "VL" "VL" "10m" "S" "Gen-Text" "J" "TC" (list x (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
      (Command "-attdef" "" "10" "VL x 0.0" "0" "S" "Gen-Text" "J" "TC" (list z (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "11" "VL x 0.1" "1" "S" "Gen-Text" "J" "TC" (list (* x 0.1) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "12" "VL x 0.2" "2" "S" "Gen-Text" "J" "TC" (list (* x 0.2) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "13" "VL x 0.3" "3" "S" "Gen-Text" "J" "TC" (list (* x 0.3) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))  
      (Command "-attdef" "" "14" "VL x 0.4" "4" "S" "Gen-Text" "J" "TC" (list (* x 0.4) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
      (Command "-attdef" "" "15" "VL x 0.6" "6" "S" "Gen-Text" "J" "TC" (list (* x 0.6) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "16" "VL x 0.8" "8" "S" "Gen-Text" "J" "TC" (list (* x 0.8) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))        
      
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "CHPROP" ssblk "" "LA" "0" "")
      (Command "CHPROP" ssblk "" "LW" "0.13" "")
      (Command "CHPROP" ssblk "" "C" "7" "")
      (Command "CHPROP" ssblk "" "LT" "Continuous" "")
      
     (command "-BLOCK" "Scale-HV" (list (* x 0.5) z z) ssblk "") ; create block
     
    
      ) ;progn
  ) ;if
  
  ;;;--- to disable allow explod-----
  
  (vl-load-com)
  (setq BLOCKS
    (vla-get-Blocks
      (vla-get-activedocument
        (vlax-get-acad-object)
      )
    )
    BLK (vla-Item BLOCKS "Scale-HV")
  )
  (vla-put-explodable (vla-Item BLOCKS "Scale-HV") :vlax-false)
  
;;;--- end to disable allow explod-----

  (setvar "OSMODE" osm)
  (setvar "attdia" ad) 
  (setvar "attreq" aq)
  
  (princ)
) ;defun

;-------------------------Sub function Scale-HV end--------------------------------

(defun CHVSS(/ osm ad aq ssblk x y z )

  (setq osm (getvar "OSMODE"))
  (setq ad (getvar "attdia"))
  (setq aq (getvar "attreq"))
  
  (setvar "OSMODE" 0)
  (setvar "dimzin" 8)
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  

  (if (not (tblsearch "block" "Scale-HV-S"))
      (progn     
      (setq ssblk (ssadd))

; Set Dimensions 

      (setq X 100) ; set horizontal dimension
      (setq Y 2)   ; set Vertical dimension
      
; Draw Function      
      (setq Z 0)
      
; Rectangle      
      (command "RECTANG" (list z z z)(list (* x 0.5) y z))
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list z (* y 0.5) z)(list (* x 0.5) (* y 0.5) z) "") 
         (setq ssblk (ssadd (entlast) ssblk))
         
; Soild Hatches          
      (command "SOLID" (list z z z)(list z (* y 0.5) z)(list (* x 0.02) z z)(list (* x 0.02) (* y 0.5) z) "")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.02) (* y 0.5) z)(list (* x 0.02) y z)(list (* x 0.04) (* y 0.5) z)(list (* x 0.04) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))  
      (command "SOLID" (list (* x 0.04) (* y 0.5) z)(list (* x 0.04) z z)(list (* x 0.06) (* y 0.5) z)(list (* x 0.06) z z)"")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (command "SOLID" (list (* x 0.06) (* y 0.5) z)(list (* x 0.06) y z)(list (* x 0.08) (* y 0.5) z)(list (* x 0.08) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.08) (* y 0.5) z)(list (* x 0.08) z z)(list (* x 0.1) (* y 0.5) z)(list (* x 0.1) z z)"")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (command "SOLID" (list (* x 0.1) (* y 0.5) z)(list (* x 0.1) y z)(list (* x 0.15) (* y 0.5) z)(list (* x 0.15) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.15) (* y 0.5) z)(list (* x 0.15) z z)(list (* x 0.2) (* y 0.5) z)(list (* x 0.2) z z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.2) (* y 0.5) z)(list (* x 0.2) y z)(list (* x 0.3) (* y 0.5) z)(list (* x 0.3) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))   
      (command "SOLID" (list (* x 0.3) (* y 0.5) z)(list (* x 0.3) z z)(list (* x 0.4) (* y 0.5) z)(list (* x 0.4) z z)"")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "SOLID" (list (* x 0.4) (* y 0.5) z)(list (* x 0.4) y z)(list (* x 0.5) (* y 0.5) z)(list (* x 0.5) y z)"")
         (setq ssblk (ssadd (entlast) ssblk))
        
; Lines     
      (command "LINE" (list (* x 0.02) y z)(list (* x 0.02) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.04) y z)(list (* x 0.04) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.06) y z)(list (* x 0.06) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.08) y z)(list (* x 0.08) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.1) y z)(list (* x 0.1) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.15) y z)(list (* x 0.15) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.2) y z)(list (* x 0.2) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.3) y z)(list (* x 0.3) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))
      (command "LINE" (list (* x 0.4) y z)(list (* x 0.4) z z) "")
         (setq ssblk (ssadd (entlast) ssblk))

 ; Attribute texts  
      (if (not (tblsearch "style" "Gen-Text")) (command "-style" "Gen-Text" "Arial.ttf" 2.5 "1" 0 "n" "n"))
      
      (Command "-attdef" "" "1:1000" "Horizontal Scale" "(1:1000 H)" "S" "Gen-Text" "J" "BR" (list (- z 2) (+ y 0.26) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "HL" "HL" "50m" "S" "Gen-Text" "J" "C" (list (* x 0.5) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
      (Command "-attdef" "" "0" "HL x 0.0" "0" "S" "Gen-Text" "J" "C" (list z (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "1" "HL x 0.2" "10" "S" "Gen-Text" "J" "C" (list (* x 0.1) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "2" "HL x 0.4" "20" "S" "Gen-Text" "J" "C" (list (* x 0.2) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "3" "HL x 0.6" "30" "S" "Gen-Text" "J" "C" (list (* x 0.3) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))  
      (Command "-attdef" "" "4" "HL x 0.8" "40" "S" "Gen-Text" "J" "C" (list (* x 0.4) (+ y 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
  
         
      (Command "-attdef" "" "1:100" "Vertical Scale" "(1:100 V)" "S" "Gen-Text" "J" "TR" (list (- z 2) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "VL" "VL" "5m" "S" "Gen-Text" "J" "TC" (list (* x 0.5) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
      (Command "-attdef" "" "10" "VL x 0.0" "0" "S" "Gen-Text" "J" "TC" (list z (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "-attdef" "" "11" "VL x 0.2" "1" "S" "Gen-Text" "J" "TC" (list (* x 0.1) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "12" "VL x 0.4" "2" "S" "Gen-Text" "J" "TC" (list (* x 0.2) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk)) 
      (Command "-attdef" "" "13" "VL x 0.6" "3" "S" "Gen-Text" "J" "TC" (list (* x 0.3) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))  
      (Command "-attdef" "" "14" "VL x 0.8" "4" "S" "Gen-Text" "J" "TC" (list (* x 0.4) (- z 1) z) "0")
         (setq ssblk (ssadd (entlast) ssblk))   
        
      
         (setq ssblk (ssadd (entlast) ssblk))
      (Command "CHPROP" ssblk "" "LA" "0" "")
      (Command "CHPROP" ssblk "" "LW" "0.13" "")
      (Command "CHPROP" ssblk "" "C" "7" "")
      (Command "CHPROP" ssblk "" "LT" "Continuous" "")
      
     (command "-BLOCK" "Scale-HV-S" (list (* x 0.25) z z) ssblk "") ; create block    
    
      ) ;progn
  ) ;if
  
  ;;;--- to disable allow explod-----
  
  (vl-load-com)
  (setq BLOCKS
    (vla-get-Blocks
      (vla-get-activedocument
        (vlax-get-acad-object)
      )
    )
    BLK (vla-Item BLOCKS "Scale-HV-S")
  )
  (vla-put-explodable (vla-Item BLOCKS "Scale-HV-S") :vlax-false)
  
;;;--- end to disable allow explod-----

  (setvar "OSMODE" osm)
  (setvar "attdia" ad) 
  (setvar "attreq" aq)
  
  (princ)
) ;defun

;--------------------------Sub function Scale-HV-S end----------------

;-------------------------------Main Function-------------------------

(defun SB1(/ ht L Lt a b c d e f g pt ptlist)
  
   (setq L (rtos (* hsa 0.1) 2 1))
   (setq a 0)
   (setq b (rtos (* hsa 0.01)2 1))
   (setq c (rtos (* hsa 0.02)2 1))
   (setq d (rtos (* hsa 0.03)2 1))
   (setq e (rtos (* hsa 0.04)2 1))
   (setq f (rtos (* hsa 0.06)2 1))
   (setq g (rtos (* hsa 0.08)2 1))
   
   (setq ht (strcat "1:" (rtos hsa)))
   (setq Lt (strcat L "m"))
   
   (setq ptlist nil) ; for while command
       
      (while     
        (progn
          (setq PT (getpoint "\nPick Scale Bar location: ")) ;;; input location           
	  
	  (if (not (tblsearch "layer" "Scale bar"))
	      (command "-LAYER" "N" "Scale bar" "C" "7" "Scale bar" "LT" "Continuous" "Scale bar""LW" "0.00" "Scale bar" ""))      
              (command "CLAYER" "Scale bar")               
              (SBC)              
          (if (not (= pt nil))  (command "-insert" "Scale-B" pt "1" "1" "0" ht Lt a b c d e f g))
          (setq ptlist (append ptlist (list pt))) ; to stop while command           
        ) ;progn  
      ) ;while       
  
 (princ)
) ;defun

;-------------------------------Main Function SB1 end-------------------------

(defun SBS1(/ ht L Lt a b c d e pt ptlist)

   (setq L (rtos (* hsa 0.05) 2 1))
   (setq a 0)
   (setq b (rtos (* hsa 0.01)2 1))
   (setq c (rtos (* hsa 0.02)2 1))
   (setq d (rtos (* hsa 0.03)2 1))
   (setq e (rtos (* hsa 0.04)2 1))
   
   (setq ht (strcat "1:" (rtos hsa)))
   (setq Lt (strcat L "m"))
   
   (setq ptlist nil) ; for while command
       
      (while     
        (progn
          (setq PT (getpoint "\nPick Scale Bar location: ")) ;;; input location           
	  
	  (if (not (tblsearch "layer" "Scale bar"))
	      (command "-LAYER" "N" "Scale bar" "C" "7" "Scale bar" "LT" "Continuous" "Scale bar""LW" "0.00" "Scale bar" ""))      
              (command "CLAYER" "Scale bar")               
              (CSBS)              
          (if (not (= pt nil))  (command "-insert" "Scale-S" pt "1" "1" "0" ht Lt a b c d e))
          (setq ptlist (append ptlist (list pt))) ; to stop while command           
        ) ;progn  
      ) ;while       
  
 (princ)
) ;defun

;-------------------------------Main Function SBS1 end-------------------------

(defun HVS1(/ hht hl hlt a b c d e f g vht vl vlth i j k l m n pt ptlist)

   (setq HL (rtos (* hsa 0.1) 2 1))
   (setq a 0)
   (setq b (rtos (* hsa 0.01)2 1))
   (setq c (rtos (* hsa 0.02)2 1))
   (setq d (rtos (* hsa 0.03)2 1))
   (setq e (rtos (* hsa 0.04)2 1))
   (setq f (rtos (* hsa 0.06)2 1))
   (setq g (rtos (* hsa 0.08)2 1))
   
   (setq hht (strcat "(1:" (rtos hsa) " H)"))
   (setq HLt (strcat HL "m"))
   
   (setq VL (rtos (* vsa 0.1) 2 1))
   (setq h 0)
   (setq i (rtos (* vsa 0.01)2 1))
   (setq j (rtos (* vsa 0.02)2 1))
   (setq k (rtos (* vsa 0.03)2 1))
   (setq l (rtos (* vsa 0.04)2 1))
   (setq m (rtos (* vsa 0.06)2 1))
   (setq n (rtos (* vsa 0.08)2 1))
      
      (setq vht (strcat "(1:" (rtos vsa) " V)"))
   (setq VLt (strcat VL "m"))
   
   (setq ptlist nil) ; for while command
       
      (while     
        (progn
          (setq PT (getpoint "\nPick Scale Bar location: ")) ;;; input location           
	  
	  (if (not (tblsearch "layer" "Scale bar"))
	      (command "-LAYER" "N" "Scale bar" "C" "7" "Scale bar" "LT" "Continuous" "Scale bar""LW" "0.00" "Scale bar" ""))      
              (command "CLAYER" "Scale bar")               
              (CHVS)              
          (if (not (= pt nil))  (command "-insert" "Scale-HV" pt "1" "1" "0" hht hlt a b c d e f g vht vlt h i j k l m n))
          (setq ptlist (append ptlist (list pt))) ; to stop while command           
        ) ;progn  
      ) ;while       
  
 (princ)
) ;defun

;-------------------------------Main Function HVS1 end-------------------------

(defun HVSS1(/ hht hl hlt a b c d e f g vht vl vlth i j k l m n pt ptlist)
   
   (setq HL (rtos (* hsa 0.05) 2 1))
   (setq a 0)
   (setq b (rtos (* hsa 0.01)2 1))
   (setq c (rtos (* hsa 0.02)2 1))
   (setq d (rtos (* hsa 0.03)2 1))
   (setq e (rtos (* hsa 0.04)2 1))
   
   (setq hht (strcat "(1:" (rtos hsa) " H)"))
   (setq HLt (strcat HL "m"))
   
   (setq VL (rtos (* vsa 0.05) 2 1))
   (setq f 0)
   (setq g (rtos (* vsa 0.01)2 1))
   (setq h (rtos (* vsa 0.02)2 1))
   (setq i (rtos (* vsa 0.03)2 1))
   (setq j (rtos (* vsa 0.04)2 1))
      
   (setq vht (strcat "(1:" (rtos vsa) " V)"))
   (setq VLt (strcat VL "m"))
   
   (setq ptlist nil) ; for while command
       
      (while     
        (progn
          (setq PT (getpoint "\nPick Scale Bar location: ")) ;;; input location           
	  
	  (if (not (tblsearch "layer" "Scale bar"))
	      (command "-LAYER" "N" "Scale bar" "C" "7" "Scale bar" "LT" "Continuous" "Scale bar""LW" "0.00" "Scale bar" ""))      
              (command "CLAYER" "Scale bar")               
              (CHVSS)              
          (if (not (= pt nil))  (command "-insert" "Scale-HV-S" pt "1" "1" "0" hht hlt a b c d e vht vlt f g h i j))
          (setq ptlist (append ptlist (list pt))) ; to stop while command           
        ) ;progn  
      ) ;while 
      
 (princ)
) ;defun

;-------------------------------Main Function HVSS1 end-------------------------

(defun C:SBB(/ clay di df-hs hs hsa df-vs vs vsa)

     ; store current system variables  
      (setq clay (getvar "clayer"))
      (setq di (getvar "dimzin"))
      (setq temperr *error*)
      (setq *error* trap1)
   
     ; set system variables
      (setvar "cmdecho" 0)
      (command "undo" "group")
      (setvar "dimzin" 8)
      (setvar "blipmode" 0)
      (setvar "tilemode" 0)
      (command "_.pspace")

  ; variable input values
   (if (not df-hs) (setq df-hs 1000.0))    ; default horizontal scale             
       (setq hs (getreal (strcat "\nEnter Horizontal Scale 1:<" (rtos df-hs 2 0) ">: ")))
   (if (not hs) (setq hs df-hs) (setq df-hs hs)) 
   
   (setq hsa (fix hs))
   
   (if (not df-vs) (setq df-vs 0))    ; default horizontal Scale             
       (setq vs (getreal (strcat "\nEnter Vertical Scale 1:<" (rtos df-vs 2 0) ">: ")))
   (if (not vs) (setq vs df-vs) (setq df-vs vs))  
   
   (setq vsa (fix vs))

   (if(= vs 0) (SB1) (HVS1))   
 
 (princ)
) ;defun

(defun C:SB(/ clay di df-hs hs hsa df-vs vs vsa)

  ; store current system variables  
      (setq clay (getvar "clayer"))
      (setq di (getvar "dimzin"))
      (setq temperr *error*)
      (setq *error* trap1)
   
  ; set system variables
      (setvar "cmdecho" 0)
      (command "undo" "group")
      (setvar "dimzin" 8)
      (setvar "blipmode" 0)
      (setvar "tilemode" 0)
      (setvar "tilemode" 0)
      (command "_.pspace")

  ; variable input values
   (if (not df-hs) (setq df-hs 1000.0))    ; default horizontal Scale             
       (setq hs (getreal (strcat "\nEnter Horizontal Scale 1:<" (rtos df-hs 2 0) ">: ")))
   (if (not hs) (setq hs df-hs) (setq df-hs hs)) 
   
   (setq hsa (fix hs))
   
   (if (not df-vs) (setq df-vs 0))    ; default horizontal Scale             
       (setq vs (getreal (strcat "\nEnter Vertical Scale 1:<" (rtos df-vs 2 0) ">: ")))
   (if (not vs) (setq vs df-vs) (setq df-vs vs))  
   
   (setq vsa (fix vs))

   (if(= vs 0) (SBS1) (HVSS1))  
 
 (princ)
) ;defun


(princ "\nScale Bar Lisp | © Bijoy manoharan 2011 | www.cadlispandtips.com |")
(princ "\nLisp Commands:SB(Half length Scale Bar), SBB(Full Length Scale Bar)")
(princ)

;---------------------------------------End------------------------------------
