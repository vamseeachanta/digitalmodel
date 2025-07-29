;***    TEXTOUT.LSP
;***    Written by Don J. Buschert (c) 1996
;
;    Email:    don.buschert@sait.ab.ca
;        buschert@spots.ab.ca
;    AutoCAD Page:    http://www.spots.ab.ca/~buschert/autocad/main.htm 
;
;    Disclaimer:
;    Permission to use, copy, modify, and distribute this software 
;    for any purpose and without fee is hereby granted, provided
;    that the above copyright notice appears in all copies and 
;    that both that copyright notice and the limited warranty and 
;    restricted rights notice below appear in all supporting 
;    documentation.
;
;    THIS PROGRAM IS PROVIDED "AS IS" AND WITH ALL FAULTS.  THE AUTHOR
;    SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR
;    FITNESS FOR A PARTICULAR USE.  THE AUTHOR ALSO DOES NOT WARRANT THAT
;    THE OPERATION OF THE PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.
;
;    Version 1.1    06/20/97
;    Revised the (REMOVE_ELEM) function thanks to Reini Urban pointing out a
;    serious flaw in the original by C. Bertha.  Apparently if there were multiple
;    elements, the old version would add the element instead of removing it.
;
;    Version 1.0 May 15, 1995
;    Original program.
;
;    Outputs selected text to an ASCII file.  Sorts the text by the
;    Y value, so you can select by window or crossing, and the lines
;    in the ASCII file will be correct.  You must select text with a 
;    rotation angle of 0.  Failing to do so may yield unpredictable
;    results.
;
;    Warning!  There is a bug in the program.  If you select text objects with y
;    values that are equal, TX2MTX will ignore them entirely.  Just be aware of
;    the text objects your are selecting.  I'll get around to fixing this
;    eventually...
;
(princ "\nInitial load, please wait...")
;
;***    Function TEXTOUT_SORT
;This functions sorts the text object selection set and returns it
;in order with the highest Y values at the beginning...
(defun TEXTOUT_SORT ( / )
  ;create dotted pair list of text Y insertions points and string
  (setq tout_indx 0);set the index
  (repeat (sslength tout_sst2);repeat for each text object
    (setq tout_enam (ssname tout_sst2 tout_indx));get object name
    (setq tout_strg (cdr (assoc '1 (entget tout_enam))));get string
    (setq tout_yval (caddr (assoc '10 (entget tout_enam))));get y value
    (setq tout_dtpr (cons tout_yval tout_strg));create dotted pair
    (if tout_dtpr_list ;if dotted pair list exists,
      (setq tout_dtpr_list (cons tout_dtpr tout_dtpr_list));then append
      (setq tout_dtpr_list (list tout_dtpr));else create it
    )
    ;increase index
    (setq tout_indx (1+ tout_indx));set the index    
  )

  ;create list of y values
  (foreach n tout_dtpr_list ;for each item in the dotted pair list...
    (setq tout_yval (car n));get the y value
    (if tout_yval_list ;if the y value list exists,
      (setq tout_yval_list (cons tout_yval tout_yval_list));add
      (setq tout_yval_list (list tout_yval));else create
    ) 
  )

  ;sort the y value list
  (while (> (length tout_yval_list) 1);as long as more than 1 element...
    (setq tout_yval (apply 'MIN tout_yval_list));get the min Y value
    (if tout_yval_slis ;if the Y value sorted list exists,
      (setq tout_yval_slis (cons tout_yval tout_yval_slis));add
      (setq tout_yval_slis (list tout_yval));else create
    )
    ;remove the y value from the y value list
    (setq tout_yval_list (remove_elem tout_yval tout_yval_list))

  )

  ;add the last element to the sorted y value list
  (if tout_yval_slis ;if the Y value sorted list exists,
    (setq tout_yval_slis (cons (car tout_yval_list) tout_yval_slis));add
    (setq tout_yval_slis (list (car tout_yval_list)));else create
  )

  ;create a string list of the text values based on the sorted Y value
  ;list
  (foreach n tout_yval_slis ;for each element in the sorted Y value list...
    (setq tout_text (cdr (assoc n tout_dtpr_list)));get the text string
    ;if text string list exists
    (if tout_text_list
      (setq tout_text_list (cons tout_text tout_text_list));add to list
      (setq tout_text_list (list tout_text));else create it.
    )
  )

  ;reverse the order of the text list
  (setq tout_text_list (reverse tout_text_list))
 
)

;***    Function TEXTOUT
;Controls and executes program
(defun C:TEXTOUT ( / 
   tout_coun        ;counter
   tout_filo        ;open'ed file
   tout_flag        ;flag.
   tout_fnam        ;ASCII file name
   tout_dtpr        ;dotted pair
   tout_dtpr_list     ;dotted pair list
   tout_enam        ;entity name
   tout_indx        ;index
   tout_sst1        ;initial selection set
   tout_sst2        ;text only selection set
   tout_strg        ;string
   tout_text        ;text string
   tout_text_list    ;text string list
   tout_yval        ;text Y value
   tout_yval_list    ;Y value list
   tout_yval_slis    ;Sorted Y value list
   
               )

;Define error routine for this command
  (defun textout_error (s)
    (if (/= s "Function cancelled.");if ^c occurs...
      (princ (strcat "\nError: " s))
    )
    (if olderr (setq *error* olderr))
    (princ)
  )
  (setq olderr *error*)
  (setq *error* textout_error)
  
  ;select text
  (setq tout_sst1 (ssget))
  ;if objects selected...
  (if tout_sst1
    (progn
      ;create list of text objects only
      (setq tout_coun 0);set counter
      (setq tout_sst2 (ssadd))
      (repeat (sslength tout_sst1)
        ;get ename
        (setq tout_enam (ssname tout_sst1 tout_coun))
        ;test to see if entity is a text object
        (if (eq (cdr (assoc '0 (entget tout_enam))) "TEXT")
          ;check to see if text has rotation angle of 0
          (if (<= (cdr (assoc '50 (entget tout_enam))) 1.0)
            (setq tout_sst2 (ssadd tout_enam tout_sst2));add to selection
          )
        )
        (setq tout_coun (1+ tout_coun))
      )
      (if tout_sst2
        (princ
          (strcat "\n" (itoa (sslength tout_sst2))
                  " text objects found, with horizontal rotation..."
          )
        )
      )
      ;if tout_sst2 is empty, then clear it...
      (if (eq (sslength tout_sst2) 0)
        (setq tout_sst2 nil)
      )
    )
  )

  ;if there were text objects selected, then
  ;get a name for the ascii file
  (if tout_sst2
    (setq tout_fnam (getfiled "TEXTOUT - ASCII Filename" "" "txt" 1))   
  )

  ;if a filename was selected
  (if tout_fnam
    (progn
      ;sort the selected text objects by Y value...
      (TEXTOUT_SORT)
      ;open the file for writing
      (setq tout_filo (open tout_fnam "w"))
      ;for each selected text object, write to the file
      (if tout_text_list
        (foreach n tout_text_list
          (write-line n tout_filo)        
        )
      )
        
      ;close the file 
      (close tout_filo )
      
      ;delete the seleted text
      (if (not tout_flag)(setq tout_flag "Yes" ))
      (initget "No Yes" )
      (setq tout_flag
        (getkword (strcat "\nErase selected text? Y/N <" tout_flag ">: "))
      )
      (if (not tout_flag)
        (setq tout_flag "Yes")
      )
      (if (eq tout_flag "Yes") 
        (command ".ERASE" tout_sst2 "")
      )
    ) 
  )

  (setq *error* olderr)
  (princ)
 
)
;Support functions
;***    REMOVE_ELEM
;This function removes an element from a list and return the list without
;the element.  This program was originally known as serge:remove, written
;by Serge Volkov, the winner of the great lisp puzzle contest in 
;comp.cad.autocad 11.Oct.96.  Reini Urban calls it "elegant and fast".
;
(defun REMOVE_ELEM (what from)
  (apply 'append (subst nil (list what) (mapcar 'list from)))
)
(princ) 