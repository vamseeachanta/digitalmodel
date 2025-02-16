
;--------------------------------------------Main Function-1-----------------------------------
(defun c:NEWS(/ p na  clay )

    (progn
; store current system variable
              (setq clay (getvar "clayer"))
  ; turn off snap mode
       (command "cmdecho"0)
       
  ;;; variable input values
       (if (not df-hs) (setq df-hs 1000.0))    ; default scale
           (setq hs (getreal (strcat "\nEnter Drawing scale 1:<" (rtos df-hs 2 0) ">: ")))
       (if (not hs) (setq hs df-hs) (setq df-hs hs))
       
       (setq th (/ hs 1000.0))                ; scale factor to be applied to block

       
 ; create layer 
     
   (if (not (tblsearch "layer" "br_north"))
       (command "-LAYER" "N" "br_north" "C" "7" "br_north" "LT" "Continuous" "br_north""LW" "0.00" "br_north" ""))      
           
       (command (nrth)); go to sub function to create Block       
      
       (command "UCS" "WORLD")
       
       (setq p (getpoint "\nSpecify insertion point to insert North Block: "))
       
       (command "-insert" "north" p "xyz" th th "1" "0")
       (setq aa (entlast))
       (command "CHPROP" aa "" "LA" "br_north" "")
       (command "UCS" "P")
       (setq by (strcat (Chr 66)(Chr 73)(Chr 74)(Chr 79)(Chr 89)(Chr 183)(Chr 86)(Chr 183)(Chr 77)))

       (command "Wipeout" "f" "off")

; reset snap mode system variable

       (setvar "clayer" clay)
 
       (PROMPT "\n © Bijoy manoharan 2010 www.cadlispandtips.com")
    );porgn
    
  
(princ)
)

(princ "\nNorth direction Block Lisp | © Bijoy manoharan 2010 | www.cadlispandtips.com |")
(princ "\nLisp Commands:NEWS   ")
(princ)

;--------------------------------------------Sub Function-----------------------------------
; sub function to create block

(defun NRTH(/ ssblk ew e ea osm)

(setq osm (getvar "OSMODE"))
          (setvar "OSMODE" 0)

; to create block North

 (if (not (tblsearch "block" "North"))
   
   (progn
     (setq ssblk (ssadd))
     
 ; create object
     
       (command "polygon" "50"(list 0 0 0)"I" "10")
       (setq ew (entlast))
       (command "wipeout" "p" ew "Y")
       (setq ssblk (ssadd (entlast) ssblk))
       (command "ERASE" ew "") 
       
       (command "_Line" (list 0 13 0)(list 0 -13 0)"")
       (setq ssblk (ssadd (entlast) ssblk))
       
       (command "_Line" (list -13 0 0)(list 13 0 0)"")
       (setq ssblk (ssadd (entlast) ssblk))
       
       (command "Circle" (list 0 0 0)"10")
       (setq ssblk (ssadd (entlast) ssblk))
       
       (command "PLINE" (list -3.0 1.0 0.0)(list -1.5 1.0 0.0)(list -1.5 4.5 0.0)(list 1.5 1.0 0.0)(list 3.0 1.0 0.0)(list 3.0 6.0 0.0)(list 1.5 6.0 0.0)(list 1.5 2.5 0.0)(list -1.5 6.0 0.0)(list -3.0 6.0 0.0)(list -3.0 1.0 0.0)"c")
       (setq e (entlast))
       (command "-HATCH" "p" "s" "s" e "" "")
       (setq ssblk (ssadd (entlast) ssblk))
       (command "ERASE" e "")
       
       (command "PLINE" (list -3.2 -5.8 0.0)(list 3.2 -5.8 0.0)(list 3.2 -3.8 0.0)(list 6.5 -3.8 0.0)(list 0.0 0.7 0.0)(list -6.5 -3.8 0.0)(list -3.2 -3.8 0.0)(list -3.2 -5.8 0.0)"c")
       (setq ea (entlast))
       (command "-HATCH" "p" "s" "s" ea "" "")
       (setq ssblk (ssadd (entlast) ssblk))
       (command "ERASE" ea "")
       
 ; convert to block
     
      (Command "CHPROP" ssblk "" "LA" "0" "")
      (Command "CHPROP" ssblk "" "LW" "0" "")
      (command "-BLOCK" "North" (list 0 0 0) ssblk "")
   );progn
 );if
 
 ;;;--- to disable allow explod-----
 
    (vl-load-com)
    
      (setq BLOCKS
        (vla-get-Blocks
          (vla-get-activedocument
            (vlax-get-acad-object)
          )
        )
          BLK (vla-Item BLOCKS "North")
      )
   (vla-put-explodable (vla-Item BLOCKS "North") :vlax-false)
 
;;;--- end to disable allow explod-----
        
       (command "REDRAW")
       (setvar "OSMODE" osm)
        
 
):defun 

;--------------------------------------------------------------------------end--------------------------------------------