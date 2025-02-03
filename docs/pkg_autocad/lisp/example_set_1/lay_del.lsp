;------------------------------------------------------------------;
; LAY_DEL.LSP 							   ;
;------------------------------------------------------------------;
; Deletes all of the entities that belong to the layer of 	   ;
; selected entity						   ;
;								   ;
; (c) Copyright 2007 by Taliasoft Taliasoft.com			   ;
; Prepared by Orhan Toker					   ;
; 								   ;
; Designed to be used for educational purposes			   ;
; 								   ;
; www.dailyautocad.com						   ;
;------------------------------------------------------------------;


; Define the function that will be called from the command line

(defun c:ld(/ oldCmdecho)

  ; Turn off the command echo
  (setq oldCmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ; Mark undo start
  (command "_.UNDO" "_mark")
  
  ; Call main fuction
  (lay_del)

  ; restore command echo value
  (setvar "CMDECHO" oldCmdecho)
  (princ)
  )

; Main function

(defun lay_del ( / sec el ln msg answer selection)

  ; Prompt user for select entity
  (princ "\nSelect an entity that you want to erase it's LAYER:")
  (setq sec (entsel)
	el (entget (car sec)) ; get entity list
	ln (cdr (assoc 8 el)) ; LAYER of selected entity
	)
  ; Separate the 8 DXF code, which gives LAYER information from
  ; the entity list by using assoc

  ; Now prompt user for comfirmation

  ; Prepare warning message
  ; By using strcat, we combine our message with 'ln' variable  
  ; in which the name of entity that we selected is stored
  
  (setq msg (strcat "All objects in " ln " LAYER will be deleted!"
  "\nDo you want to continue? [Yes] / [No] :"))

  (initget "Yes No") ; Set keywords
  (setq answer (getkword msg))

  ; Evaluate the answer, if no go to (exit_prg)
  ; Else go on
  (if (= answer "No") (exit_prg) (princ))

  ; Now select the entities to be deleted
  ; We are passing dotted pair (8 . "CHOSEN_LAYER") to
  ; ssget function as filter value
  
  (setq selection (ssget "X" (list (cons 8 ln))))

  ; now we can erase entities which are stored as previous 
  ; selection set using command function
  (command "_.ERASE" "_p" "")

  ; exit
  (exit_prg)
  
  )

(defun exit_prg()
  (princ "\nPlease UNDO BACK if you want to undelete!")
  (exit)
  (princ)
  )