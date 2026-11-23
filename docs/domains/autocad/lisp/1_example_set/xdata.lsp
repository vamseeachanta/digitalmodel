; Next available MSG number is    68 
; MODULE_ID XDATA_LSP_
;;;---------------------------------------------------------------------------;
;;;
;;;    XDATA.LSP
;;;    
;;;    Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994 by Autodesk, Inc.
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
;;;   XDATA
;;;
;;;   Program that attaches extended data types to a selected entity.  
;;;
;;;   After selecting an entity and entering an application name for the 
;;;   extended data, the following types of extended data are prompted for:
;;;
;;;    1)  An ASCII string up to 255 bytes long (group code 1000).
;;;    2)  A layer name (group code 1003).
;;;    3)  An entity handle (group code 1005).
;;;    4)  3 real numbers (group code 1010).
;;;    5)  A 3D World space position (group code 1011).
;;;    6)  A 3D World space displacement (group code 1012).
;;;    7)  A 3D World space direction (group code 1013).
;;;    8)  A real number (group code 1040).
;;;    9)  A distance (group code 1041).
;;;   10)  A scale factor (group code 1042).
;;;   11)  A 16-bit integer (group code 1070).
;;;   12)  A 32-bit signed long integer (group code 1071).
;;;
;;;   Numbers 5, 6, 7, 9 and 10 are "transformable" data types, and
;;;   are either moved, scaled, rotated or mirrored along with the parent
;;;   entity, or possibly some combination of these, depending on the
;;;   group code and the nature of the operation on the parent entity.
;;;
;;;   Binary data chunks (group code 1004) are not supported. 
;;;
;;;
;;;   XDLIST
;;;
;;;   Program that lists the Xdata associated with an application for the
;;;   selected entity.
;;; 
;;;   For a complete description of extended data types see the "AutoCAD 
;;;   Reference Manual."
;;;
;;;---------------------------------------------------------------------------;


;;;---------------------------------------------------------------------------;
;;; Internal error handling.
;;;---------------------------------------------------------------------------;

(defun xdataerr(s)
  (if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
  )
  (setq *error* olderr) 
  (if ename (redraw ename 4))         ; de-highlight entity
  (princ)
)

;;;---------------------------------------------------------------------------;
;;; Get user input.
;;;---------------------------------------------------------------------------;

(defun getinput (/ cont esel)

  ;; Forces selection of an entity and sets ename to the name of the  
  ;; selected entity.
 
  (while
    (not (setq esel (entsel)))
  )
            
  (if (= which 1)                     ; If XDATA() is happening...
    (progn
      (setq ename (car esel))         ; Get entity info...
      (redraw ename 3)                ; ...highlight entity
      (setq elist (entget ename (list "*"))) ; ...including xdata for all 
                                      ; registered applications.    
            
      ;; Force the entry of a registered application name (group code 1001).
   
      (setq cont T)
      (while cont 
        (setq rname (xstrcase (getstring "\nApplication name: ")))
        (if (/= rname "") 
          (setq cont nil)
        )
      )
    )
  )
  (if (= which 2)                     ; If XDPRINT() is happening...
    (progn                           
      (setq ename (car esel))         ; Get entity info
      (redraw ename 3)                ; ...highlight entity  
      (setq rname (xstrcase (getstring "\nApplication name <*>: ")))
      (if (= rname "")                ; If null input, get all.
        (setq rname "*")
      )                   
      (setq elist (entget ename (list rname))) 
    ) 
  )
)
 
;;;---------------------------------------------------------------------------;
;;; Get user values for extended entity data and build XD_LIST.
;;;---------------------------------------------------------------------------;

(defun getdata (/ xd_type)

  (setq xflag 0)

  ;; Check whether the selected entity has some extended data already.

  (if (assoc -3 elist)
    (progn
      (setq size_old (xdsize (assoc -3 elist)))
      (princ "\nObject has ") 
      (princ size_old )
      (princ " bytes of Xdata - new Xdata will be appended.\n")
    )
  )

  (setq xd_list (list '(1002 . "}"))) ; Initialize list of xdata for this app.

  (setq xd_type T)                    ; Initialize loop terminator.

  (while (not (or (eq xd_type "EXit") (eq xd_type "Xit") (eq xd_type nil)))
    (setq hand (getvar "handles"))
    (initget                          ; Initialize keyword list.
      (strcat "STring LAyer 3Real Position DISPlacement Handle"
            " DIRection Real DISTance SCale"
            " Integer LOng EXit Xit"

      )
    )     
    
    (setq xd_type (getkword (strcat   ; Prompt user to select keyword.
       "\n3Real/DIR/DISP/DIST/Hand/Int/LAyer/LOng/Pos/Real/SCale/STr/<eXit>: "))
    )

    ;; Add sub-list to xdata list.

    (cond
      ((eq xd_type "3Real")
        (if (/= (setq input (getpoint "\n3 real numbers: ")) nil)  
           (setq xd_list (cons (cons 1010 input) xd_list))
        )    
      )
      ((eq xd_type "DIRection")
        (if (/= (setq input (getpoint "\n3D World space direction: ")) nil)
          (setq xd_list (cons (cons 1013 input) xd_list))
        )      
      )
      ((eq xd_type "DISPlacement")
        (if (/= (setq input (getpoint "\n3D World space displacement: ")) nil)  
          (setq xd_list (cons (cons 1012 input) xd_list))
        )
      )
      ((eq xd_type "DISTance")
        (if (/= (setq input (getdist "\nDistance: ")) nil) 
          (setq xd_list (cons (cons 1041 input) xd_list))
        )
      )
      ((eq xd_type "Handle")
        (if (or ( = (setq hand (getstring "\nDatabase handle: ")) "0")
                (handent hand) 
            )
          (setq xd_list (cons (cons 1005 hand) xd_list))
          (if (/= hand "") 
            (princ "\nInvalid handle - handle must exist or have a 0 value.")
          )
        )         
      )
      ;; Values entered greater than 32767 cause AutoLISP to issue an
      ;; error message stating "Value must be between -32767 and 32767. "
      ;; Values less than 0 are trapped out by the (initget).  Though the 
      ;; message may be confusing, the values are always correct.  This is
      ;; an AutoLISP limitation.
      ((eq xd_type "Integer")
        (initget 4)
        (if (/= (setq input (getint "\n16-bit integer: ")) nil)
          (setq xd_list (cons (cons 1070 input) xd_list))
        )
      )  
      ((eq xd_type "LAyer")
        (setq input (getstring "\nLayer name: "))
        (if (tblsearch "layer" input)
          (setq xd_list (cons (cons 1003 input) xd_list))
          (if (/= input "")
            (princ "\nInvalid layer name - layer must exist.")
          )
        ) 
      )
      ((eq xd_type "LOng")
        (if (/= (setq input (getint "\n32-bit signed long integer: ")) nil)
          (setq xd_list (cons (cons 1071 input) xd_list))
        )
      )
      ((eq xd_type "Position")
        (if (/= (setq input (getpoint "\n3D World space position: ")) nil) 
          (setq xd_list (cons (cons 1011 input) xd_list))
        )    
      )
      ((eq xd_type "Real")
        (if (/= (setq input (getreal "\nReal number: ")) nil) 
          (setq xd_list (cons (cons 1040 input) xd_list))
        ) 
      )
      ((eq xd_type "SCale")
        (if (/= (setq input (getreal "\nScale factor: ")) nil)
          (setq xd_list (cons (cons 1042 input) xd_list))
        )
      )
      ((eq xd_type "STring")
        (setq xd_list (cons (cons 1000 (getstring T 
          "\nASCII string: ")) xd_list))
      )
      (t)
    )
  )

  ;; Was any xdata entered besides a registered application name ??

  (setq xflag (length xd_list))

  ;; Append opening brace to front of xdata list.

  (setq xd_list (cons '(1002 . "{") xd_list))

  ;; Append application name to front of xdata list.

  (setq xd_list (cons rname xd_list))

  ;; Append -3 group code to front of list containing xdata list.

  (setq xd_list (list -3 xd_list))

  ;; Find the total size of the new xdata. 

  (setq size_new (xdsize xd_list))
)


;-----------------------------------------------------------------------------;
; XDATA
;-----------------------------------------------------------------------------;

(defun c:xdata (/ all elist ename old olderr new rname size_new xd_list
                  xd_list1 xd_list2 xd_list3 xd_ent regflag hand xflag
                  size_old which)

  

  (setq olderr *error*                ; Use special error handling function.
        *error* xdataerr)
 
  (setq which 1)                      ; Flag for (getinput)  

  (setq regflag 0)                    ; Regapp flag.

  (getinput)                          ; Prompt for user input

  (redraw ename 4)                    ; De-highlight entity 

    

  (if (regapp rname)                  ; Register the application name.
    (princ (strcat "\n" rname " new application.\n"))
    (princ (strcat "\nApplication " rname " already registered.\n"))
  )

  ;; Prompt for user values for xdata and build xdata list XD_LIST.

  (getdata)

  ;; The extended data list is now added to the entity data.  This is a
  ;; little more involved if the entity already has extended data.  A check
  ;; of available Xdata space must be made too.

  (if (< size_new (xdroom ename))     ; If there is room for more...
    (progn     
      (if (assoc -3 elist)            ; and contains xdata already...
        (progn                                            
          (setq xd_list (cdr xd_list)) ; New xdata.
          (setq xd_ent (cdr (assoc -3 elist))) ; Old xdata.
          ;; Find old xdata with same regapp
          (if (setq old (cddr (assoc rname xd_ent))) 
            (progn                                    
              (setq regflag 1)              
              (setq new (cdr (reverse (cddr (assoc rname xd_list)))))
              (setq all (append new old)) ; Join old and new xdata with 
                                      ; same application name.
              (setq xd_list1 (cons (cons 1002 "{") all)) ; Add open curly
              (setq xd_list2 (cons rname xd_list1)) ; Add regapp
             
              ;; Substitute back into existing xdata list.
             
              (setq xd_list3 (subst xd_list2 (assoc rname xd_ent) 
                                             (assoc -3 elist))) 
           )
            (progn                    ; This is a new regapp...
              (setq xd_list (append xd_ent xd_list)) ; Joins xdata.
              (setq xd_list3 (cons -3 xd_list))
            )
          )
          (setq elist (subst xd_list3 (assoc -3 elist) elist)) ; Joins entity
        )  
        (setq elist (cons xd_list elist)) ; No xdata yet.
      )
      
    )
    (princ (strcat "\nInsufficient Xdata space available on object"
                   "- no new Xdata appended.")
    )
  )

  ;; Finally update the entity in the database to contain the new xdata.

  (if (entmod elist)     
    (if (and (= 1 regflag) (<= xflag 1))   ; old application name     
      (princ "\nNo xdata appended.")  
      (princ "\nNew xdata appended.") 
    )
  )

  (setq *error* olderr)               ; Reset the error function.
  (redraw ename 4)                    ; Dehighlight entity.

  (prin1)
)

;;;---------------------------------------------------------------------------;
;;;  XDLIST
;;;---------------------------------------------------------------------------;

(defun C:XDLIST (/ linecount xd_list app_list app_sub_list xd_code
                   xd_data rname elist ename)

  (setq olderr *error*                ; Redefine error handler.
        *error* xdataerr)

  (setq which 2)                      ; Flag for (getinput)
 
  (getinput)                          ; Get user input. 

  (redraw ename 4)                    ; De-highlight entity.

  ;; See if there's any xdata in the selected entity associated with the
  ;; application name.

  (if (not (setq xd_list (assoc -3 elist)))
    (progn  
      (princ "\nNo Xdata associated with Application Name(s).")
    )
    (setq xd_list (cdr xd_list))      ; Strip -3 from xd_list
  )

  (setq linecount 0)                  ; # of lines printed

  (while xd_list                      ; There's any xdata left...
    (setq app_list (car xd_list))           
    (textscr)
    (princ "\n\n* Registered Application Name: ")
    (princ (car app_list))
    (setq app_list (cdr app_list))    ; Strip app name
    (while app_list
      (setq app_sub_list (car app_list))  ; Get sub list
      (setq xd_code (car app_sub_list))   ; Get group code
      (setq xd_data (cdr app_sub_list))   ; Get data

      ;; Conditions for all group codes.
      ;; Prints 'em all except binary chunks.
      (cond
        ((= 1000 xd_code)
          (princ "\n* Code 1000, ASCII string: ")
          (princ xd_data)
        )
        ((= 1001 xd_code)
          (princ "\n* Code 1001, Registered application name: ")
          (princ xd_data)
        )
        ((= 1002 xd_code)
          (princ "\n* Code 1002, Starting or ending brace: ")
          (princ xd_data)
        )
        ((= 1003 xd_code)
          (princ "\n* Code 1003, Layer name: ")
         (princ xd_data)
        )
        ((= 1004 xd_code)
          (princ "\n* Code 1004, Binary data not printed.")
        )
        ((= 1005 xd_code)
          (princ "\n* Code 1005, Database handle: ")
          (princ xd_data)
        )
        ((= 1010 xd_code)
          (princ "\n* Code 1010, 3 real numbers: ")
          (princ (strcat "("
                 (rtos (car xd_data)) " " 
                 (rtos (cadr xd_data)) " "
                 (rtos (caddr xd_data)) ")"))
        )
        ((= 1011 xd_code)
          (princ "\n* Code 1011, 3D World space position: ")
          (princ (strcat "("
                 (rtos (car xd_data)) " "
                 (rtos (cadr xd_data)) " "
                 (rtos (caddr xd_data)) ")"))
        )
        ((= 1012 xd_code)
          (princ "\n* Code 1012, 3D World space displacement: ")
          (princ (strcat "("
                 (rtos (car xd_data)) " "
                 (rtos (cadr xd_data)) " "
                 (rtos (caddr xd_data)) ")"))
        )
        ((= 1013 xd_code)
          (princ "\n* Code 1013, 3D World space direction: ")
          (princ (strcat "("
                 (rtos (car xd_data)) " "
                 (rtos (cadr xd_data)) " "
                 (rtos (caddr xd_data)) ")"))
        )
        ((= 1040 xd_code)
          (princ "\n* Code 1040, Real number: ")
          (princ (rtos xd_data))
        )
        ((= 1041 xd_code)
          (princ "\n* Code 1041, Distance: ")
          (princ (rtos xd_data))
        )
        ((= 1042 xd_code)
          (princ "\n* Code 1042, Scale factor: ")
          (princ (rtos xd_data))
        )
        ((= 1070 xd_code)
          (princ "\n* Code 1070, 16-bit integer: ")
          (princ xd_data)
        )
        ((= 1071 xd_code)
          (princ "\n* Code 1071, 32-bit signed long integer: ")
          (princ (rtos xd_data 2 0))
        )
        (t 
          (princ "\n* Unknown xdata code: ") 
          (princ xd_code)
          (princ " *")
        )
      )
      (setq app_list (cdr app_list))
      (setq linecount (1+ linecount))
      (if (>= linecount 20)           ; Pause at 20 lines printed.
        (progn
          (getstring "\n-More-")
          (setq linecount 0)
        )
      )
    )  
  (setq xd_list (cdr xd_list))        ; Get next xdata list.
)  
  

  (princ "\n\nObject has ")                     
  (princ (xdroom ename))              ; Figure how much room is left.
  (princ " bytes of Xdata space available.")
 
  (setq *error* olderr)               ; Reset the error function.
  (prin1)                             ; Quiet exit.

)
;;;---------------------------------------------------------------------------;
(princ "\nC:XDATA loaded.  Enter XDATA and XDLIST to define and list data. ")
(princ)
