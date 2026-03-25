;;;---------------------------------------------------------------------------;
;;;
;;;    XLIST.LSP
;;;    
;;;    Copyright (C) 1997 by Autodesk, Inc.
;;;		by Paul Vine
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;.
;;;  
;;;---------------------------------------------------------------------------;
;;;  DESCRIPTION
;;;
;;;   XLIST
;;;   This routine lists certain properties of objects that are nested in
;;;   a block or xref.  
;;;   
;;;   (c:xlist) and (c:-xlist) call (XLIST) with a boolean kUseDialog turned on 
;;;	to use the dialog display or off for listing at the command line, respectively.
;;;
;;;   (XLIST)
;;;   This is the main function called from the command line.  This function
;;;   prompts the user to pick an object, determines if the selection is valid,
;;;   and if it is, calls GetList().  After calling this function, it calls (DisplayDialog) 
;;;   if kUseDialog is true or (DisplayList) if it is false.
;;;
;;;   (GetList)
;;;   This function take the object handle as an argument and parses out the assoc 
;;;   codes we are interested in, makes adjustments for the optional codes, converts 
;;;   color numbers 1 through 8 back to their color names.  It calls a "vertex" a polyline
;;;   and an "insert" a block.  
;;;
;;;   (DisplayDialog)
;;;   It loads XLIST.DCL and sets the keys to the results from GetList() and 
;;;   invokes the appropriate dialog to display the results.  I have defined three 
;;;   different dialogs depending on the type of object: a dialog to display blocks, 
;;;   one for text and one for everything else.
;;;
;;;   (DisplayList)
;;;   Invokes the text screen and displays the results in list format.
;;;
;;;---------------------------------------------------------------------------;

(defun xlist_err ( /  ) 
	(setq *error* old_err)	
	(command "_.undo" "_end")
	(if old_cmd (setvar "cmdecho" old_cmd))
	(princ)
	;(princ "xlist_err was called.")
); exit quietly

(defun GetList ( / iNest eList   )		
	
	(setq iNest (length (last ePick)))

;The next if statement handles block within blocks. iNest = 1 means no nesting. Since (nentsel) goes all the
;way to the root AutoCAD object we have to traverse back up to the top level of the nesting to get a block name.
	(if (= iNest 1) 
		(setq eList (entget (car ePick)))	;then pull the list from the standard nentsel call.
		(setq eList (entget (nth (- iNest 2) (last ePick))))	;else last last our way back up to the top block definition
	);end if


;Pull out the assoc codes.
	(setq	sLayer (cdr (assoc 8 eList))
              	sObjectType (cdr (assoc 0 eList))
	    	sLineType (cdr (assoc 6 eList))			; This is optional, we check for it later.
               	sColor (cdr (assoc 62 eList))
		sBlockname ""
		sStyleName ""
     	); end setq


;Check for no linetype override, in which case it is bylayer.
    	(if (= sLineType nil) (setq sLineType "Bylayer"))		;Tidy up the optional DXF codes for linetype
    	

;If the object is a vertex, call a vertex a polyline
    	(if (= "VERTEX" sObjectType) (setq sObjectType "POLYLINE"))	

;If the object is a block, call an insert a block and find out the block name
	(if (= "INSERT" sObjectType) 
		(progn
			(setq 	sObjectType "BLOCK"
				 sBlockname (cdr (assoc 2 eList))
			)	
		);end progn
	);end if 

;If the object is text or mtext, find out the style name
	(if (or (= "TEXT" sObjectType) (= "MTEXT" sObjectType)) 
		(setq sStyleName (cdr (assoc 7 eList)))	
	);end if 

; Sort out the colors and assign names to the first 8 plus bylayer and byblock
    	(cond ( (= nil sColor) (setq sColor "Bylayer"))
        	( (= 0 sColor) (setq sColor "Byblock")) 
          	( (= 1 sColor) (setq sColor "Red"))
          	( (= 2 sColor) (setq sColor "Yellow"))
		( (= 3 sColor) (setq sColor "Green"))
          	( (= 4 sColor) (setq sColor "Cyan"))
          	( (= 5 sColor) (setq sColor "Blue"))
         	( (= 6 sColor) (setq sColor "Magenta"))
          	( (= 7 sColor) (setq sColor "White"))
          	( (= 256 sColor) (setq sColor "Bylayer"))
          	(t (setq sColor (itoa sColor)))
    	);end cond

;(princ (strcat sLayer sColor sObjectType sLinetype sThickness sStylename sBlockname))	; for debugging purposes

); End GetList


;This fucntion displays the results in LIST form...
(defun DisplayList (  / )
	(textscr)
	(cond 
		((= "BLOCK" sObjectType) 
			(princ (strcat 	"\n\tObject:\t" sObjectType
			 	"\n\tBlock name:\t" sBlockname )
			);end princ
		);end this condition
		((or (= "TEXT" sObjectType) (= "MTEXT" sObjectType))
			(princ (strcat 	"\n\tObject:\t" sObjectType
			 	"\n\tStyle name:\t" sStylename)
			);end princ
		);end this condition
		( T   (princ (strcat 	"\n\tObject:\t" sObjectType));end princ
		);end this condition		
	); end cond

	(princ (strcat "\n\tLayer:\t\t" sLayer
			"\n\tColor:\t\t" sColor
			"\n\tLinetype:\t" sLinetype )
	);end princ
);end DisplayList



;This function displays the results in dialog form...

;Findfile for the dialog in case it isn't in the bonus/lisp/ directory
(defun DisplayDialog ( /  sAcad sAcadPath sDlgNameAndPath dcl_id  )
	
	(setq sAcad (findfile "acad.exe"))
	(setq sAcadPath (substr sAcad 1 (- (strlen sAcad) 8) ))

	(if (< (setq dcl_id (load_dialog (strcat sAcadPath "bonus/cadtools/xlist.dcl") ) ) 0)
	(progn
		(if (not (setq sDlgNameAndPath (findfile "xlist.dcl")))
		(progn
			(alert (strcat "Can't locate dialog definition file XLIST.DCL."
			"\nCheck your support directories."))
			(exit)
		);end progn
		(setq dcl_id (load_dialog sDlgNameAndPath))
		);end if
	);end progn
	);end if


;Load the dialog.  If the object is a block, load the block dialog; if it is a text entity, load the text dialog.
	(cond 
		((= "BLOCK" sObjectType) (if (not (new_dialog "xlistblock" dcl_id)) (EXIT)))
  		((or (= "TEXT" sObjectType) (= "MTEXT" sObjectType))(if (not (new_dialog "xlisttext" dcl_id)) (EXIT)))
		( T (if (not (new_dialog "xlist" dcl_id)) (EXIT) ))
	); end cond
      	(set_tile "sLayer" (strcase sLayer T))
      	(set_tile "sColor" sColor) 
      	(set_tile "sObjectType"  sObjectType )
      	(set_tile "sLineType" sLineType )
	(set_tile "sBlockname" sBlockname)
	(set_tile "sStyleName" sStyleName)

;If we can't starts the dialog, then bail.
      	(if 	(= (start_dialog) nil) 	(exit));
	(unload_dialog dcl_id);
	;(princ)

); end DisplayDialog



(defun XLIST (  kUseDialog /   sLayer sObjectType sLineType sColor sBlockname sStyleName ePick )
;capture existing settings 
  	(setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
     		old_err *error*
		*error* xlist_err
  	)

	(setvar "cmdecho" 0)
	(command "_.undo" "_be")

;The next while loop checks for null or invalid selection.
    	(while (or 
		(not (setq ePick (nentsel "\nSelect nested xref or block object to list: "))) 
		(< (length ePick) 3)
		);end or
		(progn 	(princ "\nObject was invalid or was not selected."))
	);end while

;Extract the information...	
    	(GetList)

;If we are calling from "xlist" use the dialog, else display at command line... 
	(if kUseDialog (DisplayDialog) (DisplayList))
	(setq *error* old_err)
	(command "_.undo" "_end")	
	(setvar "cmdecho" old_cmd)
	; princ "normal exit called.")
	;(princ)	
)

(defun c:-xlist ( / kUseDialog )
	(setq kUseDialog F)
	(xlist kUseDialog)
)
(defun c:xlist ( / kUseDialog )
	(setq kUseDialog T)
	(xlist kUseDialog)
)

(princ "\nXLIST.LSP loaded. Enter \"xlist\" or \"-xlist\" to use.")
(princ);silent load...
