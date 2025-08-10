;
; insert holes top and side, solid and hidden
; Created by Allan Wise, Professional Draftsperson
; Email: allanyz@bigpond.com
;
; Installation
; 1. Copy all the files into an AutoCAD support Directory
; 2. Appload hole_insert.lsp
; 3. Type HOLE_INSERT on the command line and follow the prompts
;
; Created: 
; 2 Dec 2003 YZ
; 
;       
; COMPANION FILES:
;	hole_side_hidden.dwg
;	hole_side_solid.dwg
;	hole_top_Hidden.dwg
;	hole_top_solid.dwg

(defun c:hole_insert ( / TYP SIZE IP LENGTH)

  (princ "\nHoles: <T>op solid, <S>ide solid, <TO>p hidden, <SI>de Hidden")
  ;set input to no zero and no negative
  (initget 6 "S T SI TO")	
  (setq TYP(getkword "\nEnter view <T>: "))
  ;check for default
  (if (= TYP nil) (setq TYP "T"))		
  (setq TYP(strcase TYP))		;change input to caps

(if (= TYP "T")
 (progn
   (setq IP (getpoint "\nInsert point: "))
   (setq SIZE (getreal "\nEnter size: "))   
   (command "-insert" "hole_top_solid.dwg" IP SIZE SIZE "")
  )
)

(if (= TYP "S")
 (progn
   (setq IP (getpoint "\nInsert point: "))
   (setq SIZE (getreal "\nEnter size: "))
   (setq LENGTH (getreal "\nEnter length: "))   
   (command "-insert" "hole_side_solid.dwg" IP SIZE LENGTH "")
  )
)

(if (= TYP "TO")
 (progn
   (setq IP (getpoint "\nInsert point: "))
   (setq SIZE (getreal "\nEnter size: "))   
   (command "-insert" "hole_top_Hidden.dwg" IP SIZE SIZE "")
  )
)

(if (= TYP "SI")
 (progn
   (setq IP (getpoint "\nInsert point: "))
   (setq SIZE (getreal "\nEnter size: "))
   (setq LENGTH (getreal "\nEnter length: "))   
   (command "-insert" "hole_side_hidden.dwg" IP SIZE LENGTH "")
  )
)

(princ)
)