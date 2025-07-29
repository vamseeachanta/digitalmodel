;;;--- CREATOR.lsp - Create autolisp programs.                                     
;;;                                                                                
;;;--- Version 1.1  1/3/2012                                                         
;;;                                                                                
;;;                                                                                
;;;--- Creator for AutoCAD Versions 2004, 2005, 2006, 2007 and 2008                
;;;    Tested with AutoCAD 2007, 2008, 2010, & 2012                                           
;;;                                                                                
;;;                                                                                
;;;--- Copyright 2007-2012 by JefferyPSanders.com                                       
;;;    All rights reserved.                                                        
;;;                                                                                
;;;                                                                                
;;;--- This program is the updated version of MAKE_LSP                             
;;;                                                                                
;;;                                                                                
;;;--- Please send all comments/suggestions/complaints to jps@jefferypsanders.com  
;;;                                                                                

(vl-load-com)

(defun C:CREATOR()



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;   C R E A T O R    D I A L O G    B O X    F U N C T I O N S     ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;;--- Function to save the CREATOR dialog box settings
  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun saveVars()
   
    ;;;--- Save selection type
    (setq selSing(atoi(get_tile "selsing")))
    (setq selMult(atoi(get_tile "selmult")))
    (setq selGlob(atoi(get_tile "selglob")))

    ;;;--- Set up a list to hold the entity types selected
    (setq entityTypes(list))

    ;;;--- Save entity types
    (if(= (get_tile "togarc") "1")(setq entityTypes(append entityTypes(list "ARC"))))
    (if(= (get_tile "togatt") "1")(setq entityTypes(append entityTypes(list "ATTRIBUTE"))))
    (if(= (get_tile "togcir") "1")(setq entityTypes(append entityTypes(list "CIRCLE"))))
    (if(= (get_tile "togell") "1")(setq entityTypes(append entityTypes(list "ELLIPSE"))))
    (if(= (get_tile "togimg") "1")(setq entityTypes(append entityTypes(list "IMAGE"))))
    (if(= (get_tile "togins") "1")(setq entityTypes(append entityTypes(list "INSERT"))))
    (if(= (get_tile "toglin") "1")(setq entityTypes(append entityTypes(list "LINE"))))
    (if(= (get_tile "toglwp") "1")(setq entityTypes(append entityTypes(list "LWPOLYLINE"))))
    (if(= (get_tile "togmli") "1")(setq entityTypes(append entityTypes(list "MLINE"))))
    (if(= (get_tile "togmtx") "1")(setq entityTypes(append entityTypes(list "MTEXT"))))
    (if(= (get_tile "togpnt") "1")(setq entityTypes(append entityTypes(list "POINT"))))
    (if(= (get_tile "togpol") "1")(setq entityTypes(append entityTypes(list "POLYLINE"))))
    (if(= (get_tile "togsol") "1")(setq entityTypes(append entityTypes(list "SOLID"))))
    (if(= (get_tile "togtxt") "1")(setq entityTypes(append entityTypes(list "TEXT"))))
    (if(= (get_tile "togtrc") "1")(setq entityTypes(append entityTypes(list "TRACE"))))
    (if(= (get_tile "togxli") "1")(setq entityTypes(append entityTypes(list "XLINE"))))

    ;;;--- Build a filter list
    (setq filterList(list))
    (if(= (get_tile "ftogatt") "1")(setq filterList(append filterList (list (list "AttVal" (get_tile "ftogattval"))))))
    (if(= (get_tile "ftogblk") "1")(setq filterList(append filterList (list (list "Block"  (get_tile "ftogblkval"))))))
    (if(= (get_tile "ftogclr") "1")(setq filterList(append filterList (list (list "Color"  (get_tile "ftogclrval"))))))
    (if(= (get_tile "ftoglay") "1")(setq filterList(append filterList (list (list "Layer"  (get_tile "ftoglayval"))))))
    (if(= (get_tile "ftoglty") "1")(setq filterList(append filterList (list (list "LType"  (get_tile "ftogltyval"))))))
    (if(= (get_tile "ftogaxi") "1")(setq filterList(append filterList (list (list "MAxis"  (get_tile "ftogaxival"))))))
    (if(= (get_tile "ftograd") "1")(setq filterList(append filterList (list (list "Radius" (get_tile "ftogradval"))))))
    (if(= (get_tile "ftogtag") "1")(setq filterList(append filterList (list (list "Tag"    (get_tile "ftogtagval"))))))
    (if(= (get_tile "ftogtxt") "1")(setq filterList(append filterList (list (list "Value"  (get_tile "ftogtxtval"))))))
    (if(= (get_tile "ftogsty") "1")(setq filterList(append filterList (list (list "Style"  (get_tile "ftogstyval"))))))
  )






  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                    
  ;;;--- Function to turn filter toggles on and off based on selection   
  ;;;                                                                    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun chkFilterToggle()

    ;;;--- If multiple selection or global is chosen...
    (if(= (get_tile "selsing") "0")
      (progn

        ;;;--- Set up variables to hold filter matches
        (setq fAttrVal 0
              fBlkName 0
              fClrNumb 0
              fLayName 0
              fMajAxis 0
              fCRadius 0
              fLineTyp 0
              fTagName 0
              fTextVal 0
              fTextSty 0
        )

        ;;;--- Set up a counter to count the selected entities
        (setq selEntCntr 0)

        (if(= (get_tile "togarc") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
            (setq fLineTyp(+ fLineTyp 1))
            (setq fCRadius(+ fCRadius 1))
          )
        )
        (if(= (get_tile "togatt") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fAttrVal(+ fAttrVal 1))
            (setq fTagName(+ fTagName 1))
            (setq fTextSty(+ fTextSty 1))
          )
        )
        (if(= (get_tile "togcir") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
            (setq fLineTyp(+ fLineTyp 1))
            (setq fCRadius(+ fCRadius 1))
          )
        )
        (if(= (get_tile "togell") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
            (setq fLineTyp(+ fLineTyp 1))
            (setq fMajAxis(+ fMajAxis 1))
          )
        )
        (if(= (get_tile "togimg") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
          )
        )
        (if(= (get_tile "togins") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fBlkName(+ fBlkName 1))
            (setq fClrnumb(+ fClrNumb 1))
          )
        )
        (if(= (get_tile "toglin") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
            (setq fLineTyp(+ fLineTyp 1))
          )
        )
        (if(= (get_tile "toglwp") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
            (setq fLineTyp(+ fLineTyp 1))
          )
        )
        (if(= (get_tile "togmli") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
            (setq fLineTyp(+ fLineTyp 1))
          )
        )
        (if(= (get_tile "togmtx") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
            (setq fTextVal(+ fTextVal 1))
            (setq fTextSty(+ fTextSty 1))            
          )
        )
        (if(= (get_tile "togpnt") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
          )
        )
        (if(= (get_tile "togpol") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
            (setq fLineTyp(+ fLineTyp 1))
          )
        )
        (if(= (get_tile "togsol") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
          )
        )
        (if(= (get_tile "togtxt") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
            (setq fTextVal(+ fTextVal 1))
            (setq fTextSty(+ fTextSty 1))            
          )
        )
        (if(= (get_tile "togtrc") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
          )
        )
        (if(= (get_tile "togxli") "1")
          (progn
            (setq selEntCntr(+ selEntCntr 1))
            (setq fLayName(+ fLayName 1))
            (setq fClrnumb(+ fClrNumb 1))
            (setq fLineTyp(+ fLineTyp 1))
          )
        )

        ;;;--- If no entities are selected...
        (if (= selEntCntr 0)
          (progn
            
            ;;;--- Set the entities selected varaible high, so all filters are disabled
            (setq selEntCntr 100)

            ;;;--- Disable the NEXT button
            (mode_tile "accept" 1)
          )
     
          ;;;--- Else, entity types were selected, so enable the NEXT button
          (mode_tile "accept" 0)
        )
          


        ;;;--- Enable or disable filter toggles based on matching filters
        (if(>= fAttrVal selEntCntr)(mode_tile "ftogatt" 0)(progn(mode_tile "ftogatt" 1)(set_tile "ftogatt" "0")))
        (if(>= fBlkName selEntCntr)(mode_tile "ftogblk" 0)(progn(mode_tile "ftogblk" 1)(set_tile "ftogblk" "0")))
        (if(>= fClrNumb selEntCntr)(mode_tile "ftogclr" 0)(progn(mode_tile "ftogclr" 1)(set_tile "ftogclr" "0")))
        (if(>= fLayName selEntCntr)(mode_tile "ftoglay" 0)(progn(mode_tile "ftoglay" 1)(set_tile "ftoglay" "0")))
        (if(>= fLineTyp selEntCntr)(mode_tile "ftoglty" 0)(progn(mode_tile "ftoglty" 1)(set_tile "ftoglty" "0")))
        (if(>= fMajAxis selEntCntr)(mode_tile "ftogaxi" 0)(progn(mode_tile "ftogaxi" 1)(set_tile "ftogaxi" "0")))
        (if(>= fCradius selEntCntr)(mode_tile "ftograd" 0)(progn(mode_tile "ftograd" 1)(set_tile "ftograd" "0")))
        (if(>= fTagName selEntCntr)(mode_tile "ftogtag" 0)(progn(mode_tile "ftogtag" 1)(set_tile "ftogtag" "0")))
        (if(>= fTextVal selEntCntr)(mode_tile "ftogtxt" 0)(progn(mode_tile "ftogtxt" 1)(set_tile "ftogtxt" "0")))
        (if(>= fTextSty selEntCntr)(mode_tile "ftogsty" 0)(progn(mode_tile "ftogsty" 1)(set_tile "ftogsty" "0")))

        ;;;--- If the tiles were disabled, uncheck them
        (if(= (get_tile "ftogatt") 1)(set_tile "ftogatt" "0"))
        (if(= (get_tile "ftogblk") 1)(set_tile "ftogblk" "0"))
        (if(= (get_tile "ftogclr") 1)(set_tile "ftogclr" "0"))
        (if(= (get_tile "ftoglay") 1)(set_tile "ftoglay" "0"))
        (if(= (get_tile "ftoglty") 1)(set_tile "ftoglty" "0"))
        (if(= (get_tile "ftogaxi") 1)(set_tile "ftogaxi" "0"))
        (if(= (get_tile "ftograd") 1)(set_tile "ftograd" "0"))
        (if(= (get_tile "ftogtag") 1)(set_tile "ftogtag" "0"))
        (if(= (get_tile "ftogtxt") 1)(set_tile "ftogtxt" "0"))
        (if(= (get_tile "ftogsty") 1)(set_tile "ftogsty" "0"))
      )
    
      ;;;--- Else, single selection is selected, disable and uncheck filter toggles
      (progn
        (set_tile "ftogatt" "0")
        (set_tile "ftogblk" "0")
        (set_tile "ftogclr" "0")
        (set_tile "ftoglay" "0")
        (set_tile "ftoglty" "0")
        (set_tile "ftogaxi" "0")
        (set_tile "ftograd" "0")
        (set_tile "ftogtag" "0")
        (set_tile "ftogtxt" "0")
        (set_tile "ftogsty" "0")
        
        (mode_tile "ftogatt" 1)
        (mode_tile "ftogblk" 1)
        (mode_tile "ftogclr" 1)
        (mode_tile "ftoglay" 1)
        (mode_tile "ftoglty" 1)
        (mode_tile "ftogaxi" 1)
        (mode_tile "ftograd" 1)
        (mode_tile "ftogtag" 1)
        (mode_tile "ftogtxt" 1)
        (mode_tile "ftogsty" 1)

        ;;;--- See if any entities are selected
        (if
          (or
            (= (get_tile "togarc") "1")
            (= (get_tile "togatt") "1")
            (= (get_tile "togcir") "1")
            (= (get_tile "togell") "1")
            (= (get_tile "togimg") "1")
            (= (get_tile "togins") "1")
            (= (get_tile "toglin") "1")
            (= (get_tile "toglwp") "1")
            (= (get_tile "togmli") "1")
            (= (get_tile "togmtx") "1")
            (= (get_tile "togpnt") "1")
            (= (get_tile "togpol") "1")
            (= (get_tile "togsol") "1")
            (= (get_tile "togtxt") "1")
            (= (get_tile "togtrc") "1")
            (= (get_tile "togxli") "1")
          )

          ;;;--- Enable the NEXT button
          (mode_tile "accept" 0)

          ;;;--- Else, disable the NEXT button
          (mode_tile "accept" 1)
        )
      )
    )
  )

  (defun chkXtraToggle(a)
    (cond
      ; Attribute Value
      ((= a 1) (if(= (get_tile "ftogatt") "1")(set_tile "ftogattval" (getFilterData "Attribute Value"))))
      ; Block Name
      ((= a 2) (if(= (get_tile "ftogblk") "1")(set_tile "ftogblkval" (getFilterData "Name of Block"))))
      ; Color
      ((= a 3) (if(= (get_tile "ftogclr") "1")(set_tile "ftogclrval" (getFilterData "Color Number"))))
      ; Layer Name
      ((= a 4) (if(= (get_tile "ftoglay") "1")(set_tile "ftoglayval" (getFilterData "Layer Name"))))
      ; Linetype
      ((= a 5) (if(= (get_tile "ftoglty") "1")(set_tile "ftogltyval" (getFilterData "Linetype"))))      
      ; Major Axis
      ((= a 6) (if(= (get_tile "ftogaxi") "1")(set_tile "ftogaxival" (getFilterData "Length of Major Axis"))))      
      ; Radius
      ((= a 7) (if(= (get_tile "ftograd") "1")(set_tile "ftogradval" (getFilterData "Radius"))))      
      ; Attribute Tag
      ((= a 8) (if(= (get_tile "ftogtag") "1")(set_tile "ftogtagval" (getFilterData "Attribute Tag"))))      
      ; Text Value
      ((= a 9) (if(= (get_tile "ftogtxt") "1")(set_tile "ftogtxtval" (getFilterData "New Value"))))
      ; Text Style
      ((= a 10)(if(= (get_tile "ftogsty") "1")(set_tile "ftogstyval" (getFilterData "Text Style"))))      
    )
    
    (if(= (get_tile "ftogatt") "0")(set_tile "ftogattval" ""))
    (if(= (get_tile "ftogblk") "0")(set_tile "ftogblkval" ""))
    (if(= (get_tile "ftogclr") "0")(set_tile "ftogclrval" ""))
    (if(= (get_tile "ftoglay") "0")(set_tile "ftoglayval" ""))
    (if(= (get_tile "ftoglty") "0")(set_tile "ftogltyval" ""))
    (if(= (get_tile "ftogaxi") "0")(set_tile "ftogaxival" ""))
    (if(= (get_tile "ftograd") "0")(set_tile "ftogradval" ""))
    (if(= (get_tile "ftogtag") "0")(set_tile "ftogtagval" ""))
    (if(= (get_tile "ftogtxt") "0")(set_tile "ftogtxtval" ""))
    (if(= (get_tile "ftogsty") "0")(set_tile "ftogstyval" ""))
  )



  
  ;;;--- Save the dialog box value from the "extra filters" function
  
  (defun saveFilterValue()
    (setq filterValue(get_tile "filtervalue"))
  )

  

  ;;;--- Get the data associated with the "extra filter" functions

  (defun getFilterData(a)

    (setq returnValue "")
    
    ;;;--- Try to load the ACTIONARRAY dialog inside the DCL file
    (if (not (new_dialog "GETFILTERDATA" dcl_id))
      (progn
        (setq alertStr "The GETFILTERDATA dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )
    
    (set_tile "filterprompt" a)

    (if
      (or
        (= a "Attribute Value")
        (= a "Length of Major Axis")
        (= a "Radius")
        (= a "Attribute Tag")
        (= a "New Value")
      )
      (mode_tile "getdatafromdia" 1)
    )
    
    (action_tile "getdatafromdia" "(getDataFromDialog a)")
    (action_tile "accept8" "(saveFilterValue)(done_dialog 47)")
    (action_tile "cancel8" "(done_dialog 48)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 47)
      (setq returnValue filterValue)
    )
    returnValue
  )


  (defun getDataFromDialog(a)
    (setq changeValue nil)
    (cond
      ((= a "Name of Block"       )(getDwgData "BLKNAME"))      
      ((= a "Color Number"        )(getDwgData "COLOR"))
      ((= a "Layer Name"          )(getDwgData "LAYER"))
      ((= a "Linetype"            )(getDwgData "LINETYPE"))
      ((= a "Text Style"          )(getDwgData "STYLE"))
    )
    (if changeValue
      (set_tile "filtervalue" changeValue)
    )
  )

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                         E N D    O F                             ;;;
  ;;;                                                                  ;;;
  ;;;    C R E A T O R    D I A L O G    B O X    F U N C T I O N S    ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;        C H A N G E    D I A L O G    F U N C T I O N S           ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;--- Function to save the selections from the change dialog box

  (defun saveChange()
    (setq selectedChangeIndex(get_tile "changelist"))
    (setq selectedChange(nth (atoi selectedChangeIndex) changeList))
    (if(= (get_tile "askwhen1") "1")
      (setq askNow "All")
      (setq askNow "Each")
    )
    ;;;--- If a permanent value is selected...there is no reason to prompt for each
    (if(= selectedChangeIndex "1")
      (setq askNow "All")
    )
    (setq changeValue(get_tile "changevalue"))
  )


      
  ;;;--- Function to save the selection from the tables dialog box

  (defun saveTable()
    (setq selectedTableIndex(get_tile "tablelist"))
    (setq selectedTable(nth (atoi selectedTableIndex) tableList))
    (if selectedTable (setq changeValue selectedTable))
  )




  ;;;--- Function to get data from the drawing

  (defun getDwgData(typeOfChange)
    (cond
      (
        (= typeOfChange "ANGLE") 
        (setq changeValue(angtos(getangle "\n New Angle: ")))
      )
      (
        (= typeOfChange "BLKNAME")
        (progn

          ;;;--- Try to load the TABLES dialog inside the DCL file
          (if (not (new_dialog "TABLES" dcl_id))
            (progn
              (setq alertStr "The TABLES dialog could not be loaded.")
              (setq alertStr
                (strcat 
                  alertStr
                  "\n\nMake sure the DCL file is in a directory located"
                  "\ninside the autocad search path. For more information"
                  "\non this, please see this web page:"
                  "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
                )
              )
              (alert alertStr)
              (exit)
            )
          )

          ;;;--- Build a list to hold the block names
          (setq tableList(list))

          ;;;--- Get the first block name in the drawing
          (if(setq tblN(tblnext "BLOCK" T))
            (progn
 
              ;;;--- Save the block name in the list if it does not begin with an "*"
              (if(/= "*" (substr (cdr(assoc 2 rblN)) 1 1))
                (setq tableList(append tableList (list (cdr (assoc 2 tblN)))))
              )

              ;;;--- Loop through all of the remaining block names
              (while(setq tblN(tblnext "BLOCK"))
                (if(/= "*" (substr (cdr(assoc 2 rblN)) 1 1))
                  (setq tableList(append tableList (list (cdr(assoc 2 tblN)))))
                )
              )
            )
          )

          ;;;--- Display the table list in the change dialog box
          (start_list "tablelist" 3)
          (mapcar 'add_list tableList)
          (end_list)
          
          ;;;;--- If an action event occurs, do this function
          (action_tile "accept4"    "(saveTable)(done_dialog)")
          (action_tile "cancel4"    "(done_dialog)")   

          ;;;--- Display the dialog box
          (setq dDiag4(start_dialog))     
        )
      )      
      (
        (= typeOfChange "COLOR")
        (progn
          (setq changeValue(acad_colordlg 1 nil))
          (if changeValue(setq changeValue(itoa changeValue)))
        )
      )
      (
        (= typeOfChange "ELEVATION")
        (setq changeValue(rtos(getreal "\n New Elevation: ")))
      )
      (
        (= typeOfChange "HEIGHT")
        (setq changeValue(rtos(getdist "\n New Height: ")))
      )
      (
        (= typeOfChange "LAYER")
        (progn

          ;;;--- Try to load the TABLES dialog inside the DCL file
          (if (not (new_dialog "TABLES" dcl_id))
            (progn
              (setq alertStr "The TABLES dialog could not be loaded.")
              (setq alertStr
                (strcat 
                  alertStr
                  "\n\nMake sure the DCL file is in a directory located"
                  "\ninside the autocad search path. For more information"
                  "\non this, please see this web page:"
                  "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
                )
              )
              (alert alertStr)
              (exit)
            )
          )


          ;;;--- Build a list to hold the layer names
          (setq tableList(list))

          ;;;--- Get the first layer name in the drawing
          (if(setq tblN(tblnext "LAYER" T))
            (progn
 
              ;;;--- Save the layer name in the list
              (setq tableList(append tableList (list (cdr (assoc 2 tblN)))))

              ;;;--- Loop through all of the remaining layers
              (while(setq tblN(tblnext "LAYER"))
                (setq tableList(append tableList (list (cdr(assoc 2 tblN)))))
              )
            )
          )

          ;;;--- Display the table list in the change dialog box
          (start_list "tablelist" 3)
          (mapcar 'add_list tableList)
          (end_list)
          
          ;;;;--- If an action event occurs, do this function
          (action_tile "accept4"    "(saveTable)(done_dialog)")
          (action_tile "cancel4"    "(done_dialog)")   

          ;;;--- Display the dialog box
          (setq dDiag4(start_dialog))     
        )
      )
      (
        (= typeOfChange "LINETYPE")
        (progn

          ;;;--- Try to load the TABLES dialog inside the DCL file
          (if (not (new_dialog "TABLES" dcl_id))
            (progn
              (setq alertStr "The TABLES dialog could not be loaded.")
              (setq alertStr
                (strcat 
                  alertStr
                  "\n\nMake sure the DCL file is in a directory located"
                  "\ninside the autocad search path. For more information"
                  "\non this, please see this web page:"
                  "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
                )
              )
              (alert alertStr)
              (exit)
            )
          )

          ;;;--- Build a list to hold the linetype names
          (setq tableList(list))

          ;;;--- Get the first linetype name in the drawing
          (if(setq tblN(tblnext "LTYPE" T))
            (progn
 
              ;;;--- Save the linetype name in the list
              (setq tableList(append tableList (list (cdr (assoc 2 tblN)))))

              ;;;--- Loop through all of the remaining linetypes
              (while(setq tblN(tblnext "LTYPE"))
                (setq tableList(append tableList (list (cdr(assoc 2 tblN)))))
              )
            )
          )

          ;;;--- Display the table list in the change dialog box
          (start_list "tablelist" 3)
          (mapcar 'add_list tableList)
          (end_list)
          
          ;;;;--- If an action event occurs, do this function
          (action_tile "accept4"    "(saveTable)(done_dialog)")
          (action_tile "cancel4"    "(done_dialog)")   

          ;;;--- Display the dialog box
          (setq dDiag4(start_dialog))     
        )
      )
      (
        (= typeOfChange "RADIUS")
        (setq changeValue(rtos(getdist "\n New Radius: ")))
      )
      (
        (= typeOfChange "STYLE")
        (progn

          ;;;--- Try to load the TABLES dialog inside the DCL file
          (if (not (new_dialog "TABLES" dcl_id))
            (progn
              (setq alertStr "The TABLES dialog could not be loaded.")
              (setq alertStr
                (strcat 
                  alertStr
                  "\n\nMake sure the DCL file is in a directory located"
                  "\ninside the autocad search path. For more information"
                  "\non this, please see this web page:"
                  "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
                )
              )
              (alert alertStr)
              (exit)
            )
          )


          ;;;--- Build a list to hold the style names
          (setq tableList(list))

          ;;;--- Get the first style name in the drawing
          (if(setq tblN(tblnext "STYLE" T))
            (progn
 
              ;;;--- Save the style name in the list
              (setq tableList(append tableList (list (cdr (assoc 2 tblN)))))

              ;;;--- Loop through all of the remaining styles
              (while(setq tblN(tblnext "STYLE"))
                (setq tableList(append tableList (list (cdr(assoc 2 tblN)))))
              )
            )
          )

          ;;;--- Display the table list in the change dialog box
          (start_list "tablelist" 3)
          (mapcar 'add_list tableList)
          (end_list)
          
          ;;;;--- If an action event occurs, do this function
          (action_tile "accept4"    "(saveTable)(done_dialog)")
          (action_tile "cancel4"    "(done_dialog)")   

          ;;;--- Display the dialog box
          (setq dDiag4(start_dialog))     
        )
      )
      (
        (= typeOfChange "VALUE") 
        (setq changeValue(getstring T "\n New Value: "))
      )
    )
  )


  ;;;--- Function to check for selected item in change list
  (defun changedList()

    (saveChange)
    (cond
      (
        (or(= selectedChangeIndex "")(= selectedChangeIndex nil))
        (progn
          (set_tile "changelist" "")
          (mode_tile "changevalue" 1)
          (mode_tile "getfromdwg" 1)
          (mode_tile "askwhen1" 1)
          (mode_tile "askwhen2" 1)
          (mode_tile "accept3" 1)
        )
      )
      (
        (= selectedChangeIndex "0")
        (progn
          (set_tile "changelist" selectedChangeIndex)
          (mode_tile "changevalue" 1)
          (mode_tile "getfromdwg" 1)
          (mode_tile "askwhen1" 0)
          (mode_tile "askwhen2" 0)
          (mode_tile "accept3" 0)
        )
      )
      (
        (= selectedChangeIndex "1")
        (progn
          (set_tile "changelist" selectedChangeIndex)
          (mode_tile "changevalue" 0)
          (mode_tile "getfromdwg" 0)
          (mode_tile "askwhen1" 1)
          (mode_tile "askwhen2" 1)
          (mode_tile "accept3" 0)
        )
      )	
    )
  )    


  ;;;--- Function to display the change dialog 

  (defun getChangesDialog(typeOfChange)

    (setq dDiag 5)
 
    (setq selectedChangeIndex nil)
    (setq changeValue nil)

    (while (> dDiag 4)

      ;;;--- Try to load the CHANGE dialog inside the DCL file
      (if (not (new_dialog "CHANGE" dcl_id))
        (progn
          (setq alertStr "The CHANGE dialog could not be loaded.")
          (setq alertStr
            (strcat 
              alertStr
              "\n\nMake sure the DCL file is in a directory located"
              "\ninside the autocad search path. For more information"
              "\non this, please see this web page:"
              "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
            )
          )
          (alert alertStr)
          (exit)
        )
      )

      ;;;--- Set up the list to display
      (setq changeList
        (list
          "Let the user type in a value during program execution"
          "Select a permanent value now"
        )
      )

      ;;;--- Display the change list in the change dialog box
      (start_list "changelist" 3)
      (mapcar 'add_list changeList)
      (end_list)

      

      ;;;--- Get defaults if they exist
      (if changeValue(set_tile "changevalue" changeValue))

      (cond
	(
	  (or(= selectedChangeIndex "")(= selectedChangeIndex nil))
          (progn
            (set_tile "changelist" "")
            (mode_tile "changevalue" 1)
            (mode_tile "getfromdwg" 1)
            (mode_tile "askwhen1" 1)
            (mode_tile "askwhen2" 1)
            (mode_tile "accept3" 1)
          )
	)
	(
	  (= selectedChangeIndex "0")
          (progn
            (set_tile "changelist" selectedChangeIndex)
            (mode_tile "changevalue" 1)
            (mode_tile "getfromdwg" 1)
            (mode_tile "askwhen1" 0)
            (mode_tile "askwhen2" 0)
            (mode_tile "accept3" 0)
          )
	)
	(
	  (= selectedChangeIndex "1")
          (progn
            (set_tile "changelist" selectedChangeIndex)
            (mode_tile "changevalue" 0)
            (mode_tile "getfromdwg" 0)
            (mode_tile "askwhen1" 1)
            (mode_tile "askwhen2" 1)
            (mode_tile "accept3" 0)
          )
	)	
      )

      (set_tile "textvalue1" (strcat "CHANGE-------------> " typeOfChange))

      ;;;;--- If an action event occurs, do this function
      (action_tile "changelist" "(changedList)")
      (action_tile "getfromdwg" "(done_dialog 5)")
      (action_tile "accept3"    "(saveChange)(done_dialog 4)")
      (action_tile "cancel3"    "(done_dialog 3)")

      ;;;--- Display the dialog box
      (setq dDiag(start_dialog))

      ;;;--- If the user pressed the OKAY button...
      (if(= dDiag 4)
        (progn

          (if(= changeValue "")
            (setq changeValue (strcat "PROMPT " askNow))
          )

          (if
            (and
              (/= (substr changeValue (strlen changeValue)) "l")
              (/= (substr changeValue (strlen changeValue)) "h")
            )
            (setq changeValue(strcat changeValue " " askNow))
          )

          ;;;--- Save the action in the action list             
          (setq actionList
            (append 
              actionList 
              (list 
                 (strcat typeOfChange " " changeValue)
              )
            )
          )

        )
      )

      (if(= dDiag 5)
        (getDwgData typeOfChange)
      )
    )

  )



  ;;;--- Function to save the selections from the change value dialog box

  (defun saveChangeValue()
    (setq selectedChangeIndex(get_tile "changelist"))
    (setq selectedChange(nth (atoi selectedChangeIndex) changeList))
    (if(= (get_tile "askwhen1") "1")
      (setq askNow "All")
      (setq askNow "Each")
    )
    ;;;--- If a permanent value is selected...there is no reason to prompt for each
    (if(= selectedChangeIndex "1")
      (setq askNow "All")
    )
    (setq changeValue "")
    (cond
      ((= (get_tile "chvalue1") "1")(setq chVal1 ""                   chVal2 (get_tile "chval1t") typeOfChange "VAL_REPLACE"))
      ((= (get_tile "chvalue2") "1")(setq chVal1 (get_tile "chval2f") chVal2 (get_tile "chval2t") typeOfChange "VAL_FIRST"))
      ((= (get_tile "chvalue3") "1")(setq chVal1 (get_tile "chval3f") chVal2 (get_tile "chval3t") typeOfChange "VAL_LAST"))
      ((= (get_tile "chvalue4") "1")(setq chVal1 (get_tile "chval4f") chVal2 (get_tile "chval4t") typeOfChange "VAL_EVERY"))
      ((= (get_tile "chvalue5") "1")(setq chVal1 (get_tile "chval5f") chVal2 "" typeOfChange "VAL_INC_PREFX"))
      ((= (get_tile "chvalue6") "1")(setq chVal1 (get_tile "chval6f") chVal2 "" typeOfChange "VAL_INC_SUFFX"))
    )
  )  

  

  ;;;--- Function to display the change dialog for text value

  (defun getChangesValueDialog()

    (setq dDiag 5)
 
    (setq selectedChangeIndex nil)
    (setq changeValue nil)

    (while (> dDiag 4)

      ;;;--- Try to load the VALUE dialog inside the DCL file
      (if (not (new_dialog "VALUE" dcl_id))
        (progn
          (setq alertStr "The VALUE dialog could not be loaded.")
          (setq alertStr
            (strcat 
              alertStr
              "\n\nMake sure the DCL file is in a directory located"
              "\ninside the autocad search path. For more information"
              "\non this, please see this web page:"
              "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
            )
          )
          (alert alertStr)
          (exit)
        )
      )

      ;;;--- Set up the list to display
      (setq changeList
        (list
          "Let the user type in a value during program execution"
          "Select a permanent value now"
        )
      )

      ;;;--- Display the change list in the change dialog box
      (start_list "changelist" 3)
      (mapcar 'add_list changeList)
      (end_list)

      

      ;;;--- Get defaults if they exist
      (if changeValue(set_tile "changevalue" changeValue))

      (defun selectedChangeFunc()
        (setq selectedChangeIndex(get_tile "changelist"))
        (cond
          (
            (or(= selectedChangeIndex "")(= selectedChangeIndex nil))
            (progn
              (set_tile "changelist" "")
              (mode_tile "askwhen1" 1)
              (mode_tile "askwhen2" 1)
              (mode_tile "accept3" 1)
              (mode_tile "chvalue1" 1)(mode_tile "chval1f" 1)(mode_tile "chval1t" 1)
              (mode_tile "chvalue2" 1)(mode_tile "chval2f" 1)(mode_tile "chval2t" 1)
              (mode_tile "chvalue3" 1)(mode_tile "chval3f" 1)(mode_tile "chval3t" 1)
              (mode_tile "chvalue4" 1)(mode_tile "chval4f" 1)(mode_tile "chval4t" 1)
              (mode_tile "chvalue5" 1)(mode_tile "chval5f" 1)
              (mode_tile "chvalue6" 1)(mode_tile "chval6f" 1)            
            )
	  )
	  (
	    (= selectedChangeIndex "0")
            (progn
              (set_tile "changelist" selectedChangeIndex)
              (mode_tile "askwhen1" 0)
              (mode_tile "askwhen2" 0)
              (mode_tile "accept3" 0)
              (mode_tile "chvalue1" 0)(mode_tile "chvalue2" 0)(mode_tile "chvalue3" 0)
              (mode_tile "chvalue4" 0)(mode_tile "chvalue5" 0)(mode_tile "chvalue6" 0)
              (mode_tile "chval1f" 1)(mode_tile "chval1t" 1)
              (mode_tile "chval2f" 1)(mode_tile "chval2t" 1)
              (mode_tile "chval3f" 1)(mode_tile "chval3t" 1)
              (mode_tile "chval4f" 1)(mode_tile "chval4t" 1)
              (mode_tile "chval5f" 1)
              (mode_tile "chval6f" 1)
            )
	  )
	  (
            (= selectedChangeIndex "1")
            (progn
              (set_tile "changelist" selectedChangeIndex)
              (mode_tile "askwhen1" 1)
              (mode_tile "askwhen2" 1)
              (mode_tile "accept3" 0)
              (mode_tile "chvalue1" 0)(mode_tile "chvalue2" 0)(mode_tile "chvalue3" 0)
              (mode_tile "chvalue4" 0)(mode_tile "chvalue5" 0)(mode_tile "chvalue6" 0)
              (if(= (get_tile "chvalue1") "1")(changeValueSelected 1))
              (if(= (get_tile "chvalue2") "1")(changeValueSelected 2))
              (if(= (get_tile "chvalue3") "1")(changeValueSelected 3))
              (if(= (get_tile "chvalue4") "1")(changeValueSelected 4))
              (if(= (get_tile "chvalue5") "1")(changeValueSelected 5))
              (if(= (get_tile "chvalue6") "1")(changeValueSelected 6))            
            )
	  )	
        )
      )

      (defun changeValueSelected(a)
        (mode_tile "chval1f" 1)(mode_tile "chval1t" 1)
        (mode_tile "chval2f" 1)(mode_tile "chval2t" 1)
        (mode_tile "chval3f" 1)(mode_tile "chval3t" 1)
        (mode_tile "chval4f" 1)(mode_tile "chval4t" 1)
        (mode_tile "chval5f" 1)
        (mode_tile "chval6f" 1)
        (cond
          ((= a 1)(progn (mode_tile "chval1f" 1)(mode_tile "chval1t" 0)))
          ((= a 2)(progn (mode_tile "chval2f" 0)(mode_tile "chval2t" 0)))
          ((= a 3)(progn (mode_tile "chval3f" 0)(mode_tile "chval3t" 0)))
          ((= a 4)(progn (mode_tile "chval4f" 0)(mode_tile "chval4t" 0)))
          ((= a 5)(progn (mode_tile "chval5f" 0)(mode_tile "chval5t" 0)))
          ((= a 6)(progn (mode_tile "chval6f" 0)(mode_tile "chval6t" 0)))          
        )
        (if(= (get_tile "changelist") "0")
          (progn
            (mode_tile "chval1f" 1)(mode_tile "chval1t" 1)
            (mode_tile "chval2f" 1)(mode_tile "chval2t" 1)
            (mode_tile "chval3f" 1)(mode_tile "chval3t" 1)
            (mode_tile "chval4f" 1)(mode_tile "chval4t" 1)
            (mode_tile "chval5f" 1)
            (mode_tile "chval6f" 1)
          )
        )
      )

      (selectedChangeFunc)
      
      (mode_tile "chval1f" 1)(mode_tile "chval1t" 1)
      (mode_tile "chval2f" 1)(mode_tile "chval2t" 1)
      (mode_tile "chval3f" 1)(mode_tile "chval3t" 1)
      (mode_tile "chval4f" 1)(mode_tile "chval4t" 1)
      (mode_tile "chval5f" 1)(mode_tile "trash1"  1)
      (mode_tile "chval6f" 1)(mode_tile "trash2"  1)

      ;;;;--- If an action event occurs, do this function
      (action_tile "chvalue1"   "(changeValueSelected 1)")   ;change value
      (action_tile "chvalue2"   "(changeValueSelected 2)")   ;change first occurence
      (action_tile "chvalue3"   "(changeValueSelected 3)")   ;change last  occurence
      (action_tile "chvalue4"   "(changeValueSelected 4)")   ;change every occurence
      (action_tile "chvalue5"   "(changeValueSelected 5)")   ;increment prefix
      (action_tile "chvalue6"   "(changeValueSelected 6)")   ;increment suffix      
      (action_tile "changelist" "(selectedChangeFunc)")
      (action_tile "accept3"    "(saveChangeValue)(done_dialog 4)")
      (action_tile "cancel3"    "(done_dialog 3)")

      ;;;--- Display the dialog box
      (setq dDiag(start_dialog))

      ;;;--- If the user pressed the OKAY button...
      (if(= dDiag 4)
        (progn
          (if(= selectedChangeIndex "0")
            (setq changeValue "PROMPT")
            (setq changeValue (strcat chVal1 " " chVal2))
          )
          (setq changeValue(strcat changeValue " " askNow))


          ;;;--- Save the action in the action list             
          (setq actionList
            (append 
              actionList 
              (list 
                 (strcat typeOfChange " " changeValue)
              )
            )
          )
        )
      )
    )
  )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                       E N D    O F                               ;;;
  ;;;                                                                  ;;;
  ;;;        C H A N G E    D I A L O G    F U N C T I O N S           ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;     L I S T    M A N I P U L A T I O N    F U N C T I O N S      ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  (defun actionMoveUp()
    (setq actionIndex(atoi (get_tile "actionlist")))
    (setq oldActionList actionList)
    (setq actionList (list))
    (setq cnt 0)
    (foreach a oldActionList
      (if(/= cnt actionIndex)
        (setq actionList(append actionList (list a)))
        (progn
          (setq tmp(last actionList))
          (setq actionList(reverse(cdr(reverse actionList))))
          (setq actionList(append actionList (list a)))
          (setq actionList(append actionList (list tmp)))
        )
      )
      (setq cnt(+ cnt 1))
    )

    ;;;--- If actions exist in the actions list, display them in the dialog box
    (displayActions)
    (set_tile "actionlist" (itoa (- actionIndex 1)))
    (actionListAction)
  )



  
  (defun actionMoveDown()
    (setq actionIndex(atoi (get_tile "actionlist")))
    (setq oldActionList actionList)
    (setq actionList (list))
    (setq cnt 0)
    (while (< cnt (length oldActionList))
      (if(= cnt actionIndex)
        (progn
          (setq actionList(append actionList (list (nth (+ cnt 1) oldActionList))))
          (setq actionList(append actionList (list (nth cnt oldActionList))))
          (setq cnt(+ cnt 1))
        )
        (setq actionList(append actionList (list (nth cnt oldActionList))))
      )
      (setq cnt(+ cnt 1))
    )

    ;;;--- If actions exist in the actions list, display them in the dialog box
    (displayActions)
    (set_tile "actionlist" (itoa (+ actionIndex 1)))
    (actionListAction)

  )




  (defun actionRemove()
    (setq actionIndex(atoi (get_tile "actionlist")))
    (setq oldActionList actionList)
    (setq actionList (list))
    (if(> (length oldActionList) 0)
      (progn
        (setq cnt 0)
        (foreach a oldActionList
          (if(/= cnt actionIndex)
            (setq actionList(append actionList (list a)))
          )
          (setq cnt(+ cnt 1))
        )
      )
    )

    ;;;--- If actions exist in the actions list, display them in the dialog box
    (displayActions)

    (if(> actionIndex 0)
      (set_tile "actionlist" (itoa (- actionIndex 1)))
      (if (> (length actionList) 0)
        (set_tile "actionlist" "0")
      )
    )
    (actionListAction)
  )



  (defun actionListAction()
    (mode_tile "moveup" 1)
    (mode_tile "movedn" 1)
    (mode_tile "remove" 1)

    (setq actionIndex(atoi(get_tile "actionlist")))

    (if(> (- (length actionList) 1) actionIndex) (mode_tile "movedn" 0))
    (if(> actionIndex 0)(mode_tile "moveup" 0))

    (if(> (length actionList) 0)
      (progn
        (mode_tile "remove" 0)
        (mode_tile "accept" 0)
      )
      (mode_tile "accept" 1)
    )
  )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                         E N D    O F                             ;;;
  ;;;                                                                  ;;;
  ;;;     L I S T    M A N I P U L A T I O N    F U N C T I O N S      ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;               A U T O C A D    F U N C T I O N S                 ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                  ;;;
  ;;;              A C T I O N    A D D                ;;;
  ;;;                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defun actionAdd()

    ;;;--- Try to load the ACTIONADD dialog inside the DCL file
    (if (not (new_dialog "ACTIONADD" dcl_id))
      (progn
        (setq alertStr "The ACTIONADD dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )

    ;;;--- If a block is included in the entity types, ask for the tag to find value of attribute
    (if(member "INSERT" entityTypes)
      (mode_tile "actionaddtag" 0)
      (mode_tile "actionaddtag" 1)
    )

    ;;;--- Set a default tag name
    (setq tagName "nil")

    

    ;;;--- Enable/Disable tiles
    (mode_tile "addvalu" 1)
    (mode_tile "addarea" 1)
    (mode_tile "addleng" 1)
    (mode_tile "addradi" 1)
    (mode_tile "adddiam" 1)
    (setq addValu 0)
    (setq addArea 0)
    (setq addLeng 0)
    (setq addRadi 0)
    (setq addDiam 0)
    (foreach a entityTypes
      (cond
        ((= a "ARC")(setq addLeng(+ addLeng 1)addRadi(+ addRadi 1)))
        ((= a "ATTRIBUTE")(setq addValu(+ addValu 1)))
        ((= a "CIRCLE")(setq addArea(+ addArea 1) addRadi(+ addRadi 1) addDiam(+ addDiam 1)))
        ((= a "ELLIPSE")(setq addArea(+ addArea 1) addLeng(+ addLeng 1)))
        ((= a "INSERT")(setq addValu(+ addValu 1)))
        ((= a "LINE")(setq addLeng(+ addLeng 1)))
        ((= a "LWPOLYLINE")(setq addLeng(+ addLeng 1) addArea(+ addArea 1)))
        ((= a "MTEXT")(setq addValu(+ addValu 1)))
        ((= a "POLYLINE")(setq addLeng(+ addLeng 1) addArea(+ addArea 1)))
        ((= a "TEXT")(setq addValu(+ addValu 1)))
      )
    )
    (setq areaFlag nil)
    (if(= addValu (length entityTypes))(mode_tile "addvalu" 0))
    (if(= addArea (length entityTypes))(progn (mode_tile "addarea" 0)(setq areaFlag T)))
    (if(= addLeng (length entityTypes))(mode_tile "addleng" 0))
    (if(= addRadi (length entityTypes))(mode_tile "addradi" 0))
    (if(= addDiam (length entityTypes))(mode_tile "adddiam" 0))

    ;;;--- Build a units list
    (setq drawingUnitsList
      (list
        "One unit equals 1 inch"
        "One unit equals 1 foot"
        "One unit equals 1 millimeter"
        "One unit equals 1 centimeter"
        "One unit equals 1 decimeter"        
        "One unit equals 1 meter"
        "One unit equals 1 mile"
      )
    )
    ;;;--- Build another units list
    (setq drawingDisplayList
      (list
        "Display in feet"
        "Display in inches"
        "Display in millimeters"
        "Display in centimeters"
        "Display in decimeters"        
        "Display in meters"
        "Display in acres"
        "Display in miles"
      )
    )
    ;;;--- Display the table list in the change dialog box with INCH as default
    (start_list "drawingutype" 3)
    (mapcar 'add_list drawingUnitsList)
    (end_list)
    (setq unit "INCH")
    (setq unitType "0")
    (set_tile "drawingutype" unitType)
    
    ;;;--- Display the table list in the change dialog box with INCH as default
    (start_list "drawingdtype" 3)
    (mapcar 'add_list drawingDisplayList)
    (end_list)
    (setq unitd "INCH")
    (setq unitdType "0")
    (set_tile "drawingdtype" unitdType)
    (setq displayAs 1)

    (if(not areaFlag)
      (progn
        (mode_tile "drawingutype" 1)
        (mode_tile "drawingdtype" 1)
      )
      (progn
        (mode_tile "drawingutype" 0)
        (mode_tile "drawingdtype" 0)
      )      
    )

    ;;;;--- If an action event occurs, do this function
    (action_tile "actionaddtag" "(setq tagName $value)")
    (action_tile "actionaddcom" "(setq displayAs 1)")
    (action_tile "actionaddbox" "(setq displayAs 2)")
    (action_tile "actionaddtxt" "(setq displayAs 3)")
    (action_tile "addvalu" "(setq addValu $value)")
    (action_tile "addarea" "(setq addArea $value)")
    (action_tile "addleng" "(setq addLeng $value)")
    (action_tile "addradi" "(setq addRadi $value)")
    (action_tile "adddiam" "(setq addDiam $value)")
    (action_tile "drawingutype""(setq unitType $value)")
    (action_tile "drawingdtype""(setq unitdType $value)")
    (action_tile "accept4"    "(done_dialog 40)")
    (action_tile "cancel4"    "(done_dialog 41)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 40)
      (progn
	(cond
	  ((= displayAs 1)(setq displayAs "COMMANDLINE"))
	  ((= displayAs 2)(setq displayAs "ALERTBOX"))
 	  ((= displayAs 3)(setq displayAs "INSERTTXT"))
	)
        (setq addList(list))
        (cond
          ((= addValu "1")(setq addList(append addList(list "VALUE"))))
          ((= addArea "1")(setq addList(append addList(list "AREA"))))
          ((= addLeng "1")(setq addList(append addList(list "LENGTH"))))
          ((= addRadi "1")(setq addList(append addList(list "RADIUS"))))
          ((= addDiam "1")(setq addList(append addList(list "DIAMETER"))))
        )
        (cond
          ((= unitType "0")(setq unit "INCH"))
          ((= unitType "1")(setq unit "FEET"))
          ((= unitType "2")(setq unit "MILLIMETER"))
          ((= unitType "3")(setq unit "CENTIMETER"))
          ((= unitType "4")(setq unit "DECIMETER"))
          ((= unitType "5")(setq unit "METER"))
          ((= unitType "6")(setq unit "MILE"))
        )
        (cond
          ((= unitdType "0")(setq unitd "INCH"))
          ((= unitdType "1")(setq unitd "FEET"))
          ((= unitdType "2")(setq unitd "MILLIMETER"))
          ((= unitdType "3")(setq unitd "CENTIMETER"))
          ((= unitdType "4")(setq unitd "DECIMETER"))
          ((= unitdType "5")(setq unitd "METER"))
          ((= unitdType "6")(setq unitd "ACRE"))
          ((= unitdType "7")(setq unitd "MILE"))
        )
        (foreach a addList
          (if (not areaFlag)(setq unit "nil" unitd "nil"))
          (setq changeValue(strcat "ADD " a " " unit " " unitd " " displayAs " " tagName))
          (setq actionList(append actionList(list changeValue)))
        )  
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                  ;;;
  ;;;            A C T I O N    A R R A Y              ;;;
  ;;;                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun actionArray()

    ;;;--- Try to load the ACTIONARRAY dialog inside the DCL file
    (if (not (new_dialog "ACTIONARRAY" dcl_id))
      (progn
        (setq alertStr "The ACTIONARRAY dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )

    (defun actionArrayRec()
      (if(= (get_tile "actionarrayrec") "1")
        (progn
          (mode_tile "actionarraycol" 0)
          (mode_tile "actionarrayrow" 0)
          (mode_tile "actionarrayxsp" 0)
          (mode_tile "actionarrayysp" 0)
          (mode_tile "actionarraycpx" 1)
          (mode_tile "actionarraycpy" 1)
          (mode_tile "actionarraynum" 1)
          (mode_tile "actionarrayyes" 1)
          (mode_tile "actionarraynil" 1)
        )
        (progn
          (mode_tile "actionarraycol" 1)
          (mode_tile "actionarrayrow" 1)
          (mode_tile "actionarrayxsp" 1)
          (mode_tile "actionarrayysp" 1)
          (mode_tile "actionarraycpx" 0)
          (mode_tile "actionarraycpy" 0)
          (mode_tile "actionarraynum" 0)
          (mode_tile "actionarrayyes" 0)
          (mode_tile "actionarraynil" 0)
        )
      )
      (actionArrayPrm)
    )


    (defun actionArrayPrm()
      (if(= (get_tile "actionarrayprm") "1")
        (progn
          (if(= (get_tile "actionarrayrec") "1")
            (progn
              (mode_tile "actionarraycol" 0)
              (mode_tile "actionarrayrow" 0)
              (mode_tile "actionarrayxsp" 0)
              (mode_tile "actionarrayysp" 0)
              (mode_tile "actionarraycpx" 1)
              (mode_tile "actionarraycpy" 1)
              (mode_tile "actionarraynum" 1)
              (mode_tile "actionarrayyes" 1)
              (mode_tile "actionarraynil" 1)
              (set_tile "actionarrayall" "1")
            )
            (progn
              (mode_tile "actionarraycol" 1)
              (mode_tile "actionarrayrow" 1)
              (mode_tile "actionarrayxsp" 1)
              (mode_tile "actionarrayysp" 1)
              (mode_tile "actionarraycpx" 0)
              (mode_tile "actionarraycpy" 0)
              (mode_tile "actionarraynum" 0)
              (mode_tile "actionarrayyes" 0)
              (mode_tile "actionarraynil" 0)
            )
          )
        )
        (progn
          (mode_tile "actionarraycol" 1)
          (mode_tile "actionarrayrow" 1)
          (mode_tile "actionarrayxsp" 1)
          (mode_tile "actionarrayysp" 1)
          (mode_tile "actionarraycpx" 1)
          (mode_tile "actionarraycpy" 1)
          (mode_tile "actionarraynum" 1)
          (mode_tile "actionarrayyes" 1)
          (mode_tile "actionarraynil" 1)
        )
      )
    )
    (defun actionArrayAll()
      (if(= (get_tile "actionarrayprm") "1")
        (progn
          (set_tile "actionarrayall" "1")
          (set_tile "actionarrayeac" "0")
        )
      )
    )
    
    (defun saveActionArrayVars()
      (if(= (get_tile "actionarrayrec") "1")
        (setq acType "Rectangular")
	(setq acType "Polar")
      )
      (if(= (get_tile "actionarrayprm") "1")
	(setq comType "Permanent")
	(setq comType "Ask")
      )
      (if(= (get_tile "actionarrayeac") "1")
	(setq askNow "Each")
	(setq askNow "All")
      )
      
      (setq xQty(atoi(get_tile "actionarraycol")))
      (setq yQty(atoi(get_tile "actionarrayrow")))
      (setq xDis(distof(get_tile "actionarrayxsp")))
      (setq yDis(distof(get_tile "actionarrayysp")))
      (setq cenPtx(atof(get_tile "actionarraycpx")))
      (setq cenPty(atof(get_tile "actionarraycpy")))
      (setq cenPt(strcat (rtos cenPtx 2 4) " "(rtos cenPty 2 4)))
      (setq numIt(atoi(get_tile "actionarraynum")))
      (if(= (get_tile "actionarrayyes") "1")
        (setq ans "Yes")
        (setq ans "No")
      )
    )

    ;;;--- Set defaults
    (mode_tile "actionarraycol" 0)
    (mode_tile "actionarrayrow" 0)
    (mode_tile "actionarrayxsp" 0)
    (mode_tile "actionarrayysp" 0)
    (mode_tile "actionarraycpx" 1)
    (mode_tile "actionarraycpy" 1)
    (mode_tile "actionarraynum" 1)
    (mode_tile "actionarrayyes" 1)
    (mode_tile "actionarraynil" 1)
    (mode_tile "actionarrayeac" 0)
    (mode_tile "actionarrayall" 0)      
      
    ;;;;--- If an action event occurs, do this function
    (action_tile "actionarrayrec" "(actionArrayRec)")
    (action_tile "actionarraypol" "(actionArrayRec)")
    (action_tile "actionarrayprm" "(actionArrayPrm)")
    (action_tile "actionarrayask" "(actionArrayPrm)")
    (action_tile "actionarrayeac" "(actionArrayAll)")
    (action_tile "actionarrayall" "(actionArrayAll)")
    (action_tile "accept4"    "(saveActionArrayVars)(done_dialog 40)")
    (action_tile "cancel4"    "(done_dialog 41)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 40)
      (progn

        (cond
          (
            (and (= acType "Rectangular")(= comType "Permanent"))
            (setq changeValue(strcat "ARRAY RECT " (itoa xQty) " " (itoa yQty) " " (rtos xDis) " " (rtos yDis)))
          )
          (
            (and(= acType "Polar")(= comType "Permanent"))
            (setq changeValue(strcat  "ARRAY POLAR " cenPt " " (itoa numIt) " " ans))          
          )
          ( 
            T
            (if (= acType "Polar")
              (setq changeValue (strcat "ARRAY POLAR PROMPT " askNow))
              (setq changeValue (strcat "ARRAY RECT PROMPT " askNow))
            )  
	  )  
        )
        (setq actionList(append actionList(list changeValue)))
      )
    )  
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                  ;;;
  ;;;             A C T I O N    C O P Y               ;;;
  ;;;                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun actionCopy()

    ;;;--- Try to load the ACTIONCOPY dialog inside the DCL file
    (if (not (new_dialog "ACTIONCOPY" dcl_id))
      (progn
        (setq alertStr "The ACTIONCOPY dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )

    (defun saveActionCopyVars()
      (setq fromPt "")
      (if(= (get_tile "actioncopyprm") "1")
	(setq comType "Permanent")
        (setq comType "Ask")
      )
      (if(= (get_tile "actioncopyeac") "1")
	(setq askNow "Each")
	(setq askNow "All")
      )
      (if(and(= (get_tile "actioncopyask") "1")(= (get_tile "actioncopyent") "1"))
	(setq fromPt "FromEntity")
      )
      (if(and(= (get_tile "actioncopyprm") "1")(= (get_tile "actioncopysel") "1"))
        (progn
	  (setq fromPt(strcat(get_tile "actioncopyfpx") "," (get_tile "actioncopyfpy") "," (get_tile "actioncopyfpz")))
          (setq toPt(strcat(get_tile "actioncopytpx") "," (get_tile "actioncopytpy") "," (get_tile "actioncopytpz")))
        )
      )
      (if(= fromPt "")(setq fromPt "PROMPT"))
    )
    
    (defun actionCopyChk()
      (if(= (get_tile "actioncopyprm") "1")
        (progn
          (mode_tile "actioncopyfpx" 0)
          (mode_tile "actioncopyfpy" 0)
          (mode_tile "actioncopyfpz" 0)
          (mode_tile "actioncopytpx" 0)
          (mode_tile "actioncopytpy" 0)
          (mode_tile "actioncopytpz" 0)
          (set_tile "actioncopyall" "1")
          (set_tile "actioncopyeac" "0")
          (set_tile "actioncopysel" "1")
          (set_tile "actioncopyent" "0")
          (mode_tile "actioncopyeac" 1)
          (mode_tile "actioncopyent" 1)       
        )
        ;;;--- Else Ask when the program runs
        (progn
          (mode_tile "actioncopyfpx" 1)
          (mode_tile "actioncopyfpy" 1)
          (mode_tile "actioncopyfpz" 1)
          (mode_tile "actioncopytpx" 1)
          (mode_tile "actioncopytpy" 1)
          (mode_tile "actioncopytpz" 1)
          (mode_tile "actioncopyeac" 0)
          (mode_tile "actioncopyall" 0)
          (mode_tile "actioncopyent" 0)
          (mode_tile "actioncopyall" 0)
          (mode_tile "actioncopysel" 0)
          (if(= (get_tile "actioncopyall") "1")
            (progn
              (set_tile "actioncopysel" "1")
              (mode_tile "actioncopyent" 1)
            )
            (progn
              (mode_tile "actioncopysel" 0)
              (mode_tile "actioncopyent" 0)
            )
          )
        )
      )
    )
    (actionCopyChk)
    
    ;;;;--- If an action event occurs, do this function
    (action_tile "actioncopyprm" "(actionCopyChk)")
    (action_tile "actioncopyask" "(actionCopyChk)")
    (action_tile "actioncopyeac" "(actionCopyChk)")
    (action_tile "actioncopyall" "(actionCopyChk)")
    (action_tile "actioncopyent" "(actionCopyChk)")
    (action_tile "actioncopysel" "(actionCopyChk)")
    (action_tile "accept4"    "(saveActionCopyVars)(done_dialog 40)")
    (action_tile "cancel4"    "(done_dialog 41)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 40)
      (progn
        
        (cond
          (
            (= comType "Permanent")
            (setq changeValue (strcat "COPY " fromPt " " toPt))
          )
          (
            (= comType "Ask")
            (progn
              (if(= fromPt "PROMPT")
                (setq changeValue (strcat "COPY PROMPT " askNow))
                (setq changeValue (strcat "COPY FromEntity " askNow))
              )
            )  
          )
        )
        (setq actionList(append actionList(list changeValue)))
      )
    )
  )



  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                  ;;;
  ;;;             A C T I O N    C O U N T             ;;;
  ;;;                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun actionCount()

    ;;;--- Try to load the ACTIONCOUNT dialog inside the DCL file
    (if (not (new_dialog "ACTIONCOUNT" dcl_id))
      (progn
        (setq alertStr "The ACTIONCOUNT dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )

    (setq displayAs 1)
    
    ;;;;--- If an action event occurs, do this function
    (action_tile "actioncountcom" "(setq displayAs 1)")
    (action_tile "actioncountbox" "(setq displayAs 2)")
    (action_tile "actioncounttxt" "(setq displayAs 3)")
    (action_tile "accept4"    "(done_dialog 40)")
    (action_tile "cancel4"    "(done_dialog 41)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 40)
      (progn
	(cond
	  ((= displayAs 1)(setq displayAs "COMMANDLINE"))
	  ((= displayAs 2)(setq displayAs "ALERTBOX"))
 	  ((= displayAs 3)(setq displayAs "INSERTTXT"))
	)  
        (setq changeValue(strcat "COUNT "  displayAs " All"))
        (setq actionList(append actionList(list changeValue)))
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                  ;;;
  ;;;            A C T I O N    D E L E T E            ;;;
  ;;;                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun actionDelete()

    ;;;--- Try to load the ACTIONDELETE dialog inside the DCL file
    (if (not (new_dialog "ACTIONDELETE" dcl_id))
      (progn
        (setq alertStr "The ACTIONDELETE dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )

    (defun saveActionDeleteVars()
      (if(= (get_tile "actiondeleteeac") "1")
	(setq askNow "Each")
	(setq askNow "All")
      )
    )
    

    ;;;;--- If an action event occurs, do this function
    (action_tile "accept4"    "(saveActionDeleteVars)(done_dialog 40)")
    (action_tile "cancel4"    "(done_dialog 41)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 40)
      (setq actionList(append actionList(list (strcat "DELETE " askNow))))
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                  ;;;
  ;;;           A C T I O N    M I R R O R             ;;;
  ;;;                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun actionMirror()

    ;;;--- Try to load the ACTIONMIRROR dialog inside the DCL file
    (if (not (new_dialog "ACTIONMIRROR" dcl_id))
      (progn
        (setq alertStr "The ACTIONMIRROR dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )

    (defun saveActionMirrorVars()
      (if(= (get_tile "actionmirrorprm") "1")
	(setq comType "Permanent")
	(setq comType "Ask")
      )
      (if(= (get_tile "actionmirroreac") "1")
	(setq askNow "Each")
	(setq askNow "All")
      )
      (if(= (get_tile "actionmirrorent") "1")
	(setq fromPt "FromEntity")
	(setq fromPt(strcat(get_tile "actionmirrorfpx") "," (get_tile "actionmirrorfpy")))
      )
      (setq toPt(strcat(get_tile "actionmirrortpx") "," (get_tile "actionmirrortpy")))             
    )
    
    (defun actionMirrorPrm()
      (mode_tile "actionmirrorfpx" 0)
      (mode_tile "actionmirrorfpy" 0)
      (mode_tile "actionmirrortpx" 0)
      (mode_tile "actionmirrortpy" 0)
    )
    (defun actionMirrorAsk()
      (mode_tile "actionmirrorfpx" 1)
      (mode_tile "actionmirrorfpy" 1)
      (mode_tile "actionmirrortpx" 1)
      (mode_tile "actionmirrortpy" 1)
    )
    (actionMirrorPrm)
    
    ;;;;--- If an action event occurs, do this function
    (action_tile "actionmirrorprm" "(actionMirrorPrm)")
    (action_tile "actionmirrorask" "(actionMirrorAsk)")
    (action_tile "accept4"    "(saveActionMirrorVars)(done_dialog 40)")
    (action_tile "cancel4"    "(done_dialog 41)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 40)
      (progn
        (cond
          ((= comType "Permanent")(setq changeValue (strcat "MIRROR " fromPt " " toPt " " askNow)))
          ((= comType "Ask")(setq changeValue (strcat "MIRROR PROMPT " askNow)))
        )
        (setq actionList(append actionList(list changeValue)))
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                  ;;;
  ;;;             A C T I O N    M O V E               ;;;
  ;;;                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun actionMove()

    ;;;--- Try to load the ACTIONMOVE dialog inside the DCL file
    (if (not (new_dialog "ACTIONMOVE" dcl_id))
      (progn
        (setq alertStr "The ACTIONMOVE dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )

    (defun saveActionMoveVars()
      (setq fromPt "")
      (if(= (get_tile "actionmoveprm") "1")
	(setq comType "Permanent")
        (setq comType "Ask")
      )
      (if(= (get_tile "actionmoveeac") "1")
	(setq askNow "Each")
	(setq askNow "All")
      )
      (if(and(= (get_tile "actionmoveask") "1")(= (get_tile "actionmoveent") "1"))
	(setq fromPt "FromEntity")
      )
      (if(and(= (get_tile "actionmoveprm") "1")(= (get_tile "actionmovesel") "1"))
        (progn
	  (setq fromPt(strcat(get_tile "actionmovefpx") "," (get_tile "actionmovefpy") "," (get_tile "actionmovefpz")))
          (setq toPt(strcat(get_tile "actionmovetpx") "," (get_tile "actionmovetpy") "," (get_tile "actionmovetpz")))
        )
      )
      (if(= fromPt "")(setq fromPt "PROMPT"))
    )

    (defun actionMoveChk()
      (if(= (get_tile "actionmoveprm") "1")
        (progn
          (mode_tile "actionmovefpx" 0)
          (mode_tile "actionmovefpy" 0)
          (mode_tile "actionmovefpz" 0)
          (mode_tile "actionmovetpx" 0)
          (mode_tile "actionmovetpy" 0)
          (mode_tile "actionmovetpz" 0)
          (set_tile "actionmoveall" "1")
          (set_tile "actionmoveeac" "0")
          (set_tile "actionmovesel" "1")
          (set_tile "actionmoveent" "0")
          (mode_tile "actionmoveeac" 1)
          (mode_tile "actionmoveent" 1)       
        )
        ;;;--- Else Ask when the program runs
        (progn
          (mode_tile "actionmovefpx" 1)
          (mode_tile "actionmovefpy" 1)
          (mode_tile "actionmovefpz" 1)
          (mode_tile "actionmovetpx" 1)
          (mode_tile "actionmovetpy" 1)
          (mode_tile "actionmovetpz" 1)
          (mode_tile "actionmoveeac" 0)
          (mode_tile "actionmoveall" 0)
          (mode_tile "actionmoveent" 0)
          (mode_tile "actionmoveall" 0)
          (mode_tile "actionmovesel" 0)
          (if(= (get_tile "actionmoveall") "1")
            (progn
              (set_tile "actionmovesel" "1")
              (mode_tile "actionmoveent" 1)
            )
            (progn
              (mode_tile "actionmovesel" 0)
              (mode_tile "actionmoveent" 0)
            )
          )
        )
      )
    )

    
    (actionMoveChk)

    ;;;;--- If an action event occurs, do this function
    (action_tile "actionmoveprm" "(actionMoveChk)")
    (action_tile "actionmoveask" "(actionMoveChk)")
    (action_tile "actionmoveeac" "(actionMoveChk)")
    (action_tile "actionmoveall" "(actionMoveChk)")
    (action_tile "actionmoveent" "(actionMoveChk)")
    (action_tile "actionmovesel" "(actionMoveChk)")    
    (action_tile "accept4"    "(saveActionMoveVars)(done_dialog 40)")
    (action_tile "cancel4"    "(done_dialog 41)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 40)
      (progn
        (cond
          (
            (= comType "Permanent")
            (setq changeValue (strcat "MOVE " fromPt " " toPt))
          )
          (
            (= comType "Ask")
            (progn
              (if(= fromPt "PROMPT")
                (setq changeValue (strcat "MOVE PROMPT " askNow))
                (setq changeValue (strcat "MOVE FromEntity " askNow))
              )
            )  
          )
        )
        (setq actionList(append actionList(list changeValue)))
      )
    )
  )
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                  ;;;
  ;;;            A C T I O N    O F F S E T            ;;;
  ;;;                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun actionOffset()

   ;;;--- Try to load the ACTIONOFFSET dialog inside the DCL file
    (if (not (new_dialog "ACTIONOFFSET" dcl_id))
      (progn
        (setq alertStr "The ACTIONOFFSET dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )

    (defun saveActionOffsetVars()
      
      (setq offsetDis "" offsetAng "" offsetNum "")
      
      (if(= (get_tile "actionoffsetprm") "1")
	(setq comType "Permanent")
	(setq comType "Ask") 
      )
      (if(= (get_tile "actionoffseteac") "1")
	(setq askNow "Each")
	(setq askNow "All")
      )
      (if(= (get_tile "actionoffsetprm") "1")
        (progn
          (setq offsetAng(angtos(angtof(get_tile "actionoffsetang"))))
          (setq offsetDis(rtos(distof(get_tile "actionoffsetdis"))))
          (setq offsetNum(get_tile "actionoffsetnum"))
        )
      )
    )
    
    (defun actionOffsetChk()
      (if(= (get_tile "actionoffsetprm") "1")
        (progn
          (set_tile  "actionoffsetall" "1")
          (mode_tile "actionoffseteac" 1)
          (mode_tile "actionoffsetang" 0)
          (mode_tile "actionoffsetdis" 0)
          (mode_tile "actionoffsetnum" 0)
        )
        (progn
          (mode_tile "actionoffseteac" 0)
          (mode_tile "actionoffsetall" 0)
          (mode_tile "actionoffsetang" 1)
          (mode_tile "actionoffsetdis" 1)
          (mode_tile "actionoffsetnum" 1)
        )
      )
    )

    (actionOffsetChk)
    
    ;;;;--- If an action event occurs, do this function
    (action_tile "actionoffsetprm" "(actionOffsetChk)")
    (action_tile "actionoffsetask" "(actionOffsetChk)")
    (action_tile "actionoffseteac" "(actionOffsetChk)")
    (action_tile "actionoffsetall" "(actionOffsetChk)")    
    (action_tile "accept4"    "(saveActionOffsetVars)(done_dialog 40)")
    (action_tile "cancel4"    "(done_dialog 41)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 40)
      (progn
        (cond
          ((= comType "Permanent")(setq changeValue (strcat "OFFSET " offsetAng " " offsetDis " " offsetNum " " askNow)))
          ((= comType "Ask")(setq changeValue (strcat "OFFSET PROMPT " askNow)))
        )
        (setq actionList(append actionList(list changeValue)))
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                  ;;;
  ;;;            A C T I O N    R O T A T E            ;;;
  ;;;                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun actionRotate()

    ;;;--- Try to load the ACTIONROTATE dialog inside the DCL file
    (if (not (new_dialog "ACTIONROTATE" dcl_id))
      (progn
        (setq alertStr "The ACTIONROTATE dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )

    (defun saveActionRotateVars()
      (setq fromPt "")
      (setq rotAng "")
      (if(= (get_tile "actionrotprm") "1")
        (progn
	  (setq comType "Permanent")
          (setq fromPt(strcat(get_tile "actionrotfpx") "," (get_tile "actionrotfpy") "," (get_tile "actionrotfpz")))
          (setq rotAng(get_tile "actionrotang"))
        )
        (progn
          (setq comType "Ask")
          (setq fromPt "PROMPT")
          (if(= (get_tile "actionrotent") "1")
	    (setq fromPt "FromEntity")
          )
        )
      )
      (if(= (get_tile "actionroteac") "1")
	(setq askNow "Each")
	(setq askNow "All")
      )
    )

    (defun actionRotateChk()
      (if(= (get_tile "actionrotprm") "1")
        (progn
          (mode_tile "actionrotfpx" 0)
          (mode_tile "actionrotfpy" 0)
          (mode_tile "actionrotfpz" 0)
          (mode_tile "actionrotang" 0)
          (set_tile  "actionrotall" "1")
          (set_tile  "actionroteac" "0")
          (set_tile  "actionrotsel" "1")
          (set_tile  "actionrotent" "0")
          (mode_tile "actionroteac" 1)
          (mode_tile "actionrotent" 1)       
        )
        ;;;--- Else Ask when the program runs
        (progn
          (mode_tile "actionrotfpx" 1)
          (mode_tile "actionrotfpy" 1)
          (mode_tile "actionrotfpz" 1)
          (mode_tile "actionrotang" 1)
          (mode_tile "actionroteac" 0)
          (mode_tile "actionrotall" 0)
          (mode_tile "actionrotent" 0)
          (mode_tile "actionrotall" 0)
          (mode_tile "actionrotsel" 0)
          (if(= (get_tile "actionrotall") "1")
            (progn
              (set_tile "actionrotsel" "1")
              (mode_tile "actionrotent" 1)
            )
            (progn
              (mode_tile "actionrotsel" 0)
              (mode_tile "actionrotent" 0)
            )
          )
        )
      )
    )

    
    (actionRotateChk)

    ;;;;--- If an action event occurs, do this function
    (action_tile "actionrotprm" "(actionRotateChk)")
    (action_tile "actionrotask" "(actionRotateChk)")
    (action_tile "actionroteac" "(actionRotateChk)")
    (action_tile "actionrotall" "(actionRotateChk)")
    (action_tile "actionrotent" "(actionRotateChk)")
    (action_tile "actionrotsel" "(actionRotateChk)")    
    (action_tile "accept4"    "(saveActionRotateVars)(done_dialog 40)")
    (action_tile "cancel4"    "(done_dialog 41)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 40)
      (progn
        (cond
          (
            (= comType "Permanent")
            (setq changeValue (strcat "ROTATE " fromPt " " rotAng))
          )
          (
            (= comType "Ask")
            (progn
              (if(= fromPt "PROMPT")
                (setq changeValue (strcat "ROTATE PROMPT " askNow))
                (setq changeValue (strcat "ROTATE FromEntity " askNow))
              )
            )  
          )
        )
        (setq actionList(append actionList(list changeValue)))
      )
    )
  )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                  ;;;
  ;;;             A C T I O N    S C A L E             ;;;
  ;;;                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun actionScale()

    ;;;--- Try to load the ACTIONSCALE dialog inside the DCL file
    (if (not (new_dialog "ACTIONSCALE" dcl_id))
      (progn
        (setq alertStr "The ACTIONSCALE dialog could not be loaded.")
        (setq alertStr
          (strcat 
            alertStr
            "\n\nMake sure the DCL file is in a directory located"
            "\ninside the autocad search path. For more information"
            "\non this, please see this web page:"
            "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
          )
        )
        (alert alertStr)
        (exit)
      )
    )

    (defun saveActionScaleVars()
      (setq fromPt "")
      (setq scFact "")
      (if(= (get_tile "actionscaleprm") "1")
        (progn
	  (setq comType "Permanent")
          (setq fromPt(strcat(get_tile "actionscalefpx") "," (get_tile "actionscalefpy") "," (get_tile "actionscalefpz")))
          (setq scFact(get_tile "actionscalefac"))
        )
        (progn
          (setq comType "Ask")
          (setq fromPt "PROMPT")
          (if(= (get_tile "actionscaleent") "1")
	    (setq fromPt "FromEntity")
          )
        )
      )
      (if(= (get_tile "actionscaleeac") "1")
	(setq askNow "Each")
	(setq askNow "All")
      )
    )

    (defun actionScaleChk()
      (if(= (get_tile "actionscaleprm") "1")
        (progn
          (mode_tile "actionscalefpx" 0)
          (mode_tile "actionscalefpy" 0)
          (mode_tile "actionscalefpz" 0)
          (mode_tile "actionscalefac" 0)
          (set_tile  "actionscaleall" "1")
          (set_tile  "actionscaleeac" "0")
          (set_tile  "actionscalesel" "1")
          (set_tile  "actionscaleent" "0")
          (mode_tile "actionscaleeac" 1)
          (mode_tile "actionscaleent" 1)       
        )
        ;;;--- Else Ask when the program runs
        (progn
          (mode_tile "actionscalefpx" 1)
          (mode_tile "actionscalefpy" 1)
          (mode_tile "actionscalefpz" 1)
          (mode_tile "actionscalefac" 1)
          (mode_tile "actionscaleeac" 0)
          (mode_tile "actionscaleall" 0)
          (mode_tile "actionscaleent" 0)
          (mode_tile "actionscaleall" 0)
          (mode_tile "actionscalesel" 0)
          (if(= (get_tile "actionscaleall") "1")
            (progn
              (set_tile "actionscalesel" "1")
              (mode_tile "actionscaleent" 1)
            )
            (progn
              (mode_tile "actionscalesel" 0)
              (mode_tile "actionscaleent" 0)
            )
          )
        )
      )
    )

    
    (actionScaleChk)

    ;;;;--- If an action event occurs, do this function
    (action_tile "actionscaleprm" "(actionScaleChk)")
    (action_tile "actionscaleask" "(actionScaleChk)")
    (action_tile "actionscaleeac" "(actionScaleChk)")
    (action_tile "actionscaleall" "(actionScaleChk)")
    (action_tile "actionscaleent" "(actionScaleChk)")
    (action_tile "actionscalesel" "(actionScaleChk)")    
    (action_tile "accept4"    "(saveActionScaleVars)(done_dialog 40)")
    (action_tile "cancel4"    "(done_dialog 41)")

    ;;;--- Display the dialog box
    (setq dDiag(start_dialog))

    ;;;--- If the user pressed the OKAY button...
    (if(= dDiag 40)
      (progn
        (cond
          (
            (= comType "Permanent")
            (setq changeValue (strcat "SCALE " fromPt " " scFact))
          )
          (
            (= comType "Ask")
            (progn
              (if(= fromPt "PROMPT")
                (setq changeValue (strcat "SCALE PROMPT " askNow))
                (setq changeValue (strcat "SCALE FromEntity " askNow))
              )
            )  
          )
        )
        (setq actionList(append actionList(list changeValue)))
      )
    )
  )

  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                        E N D    O F                              ;;;
  ;;;                                                                  ;;;
  ;;;              A U T O C A D    F U N C T I O N S                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;    A C T I O N    D I A L O G    B O X    F U N C T I O N S      ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  


  ;;;--- Determine which action buttons should be enabled
  (defun setActionButtons()

    ;;;--- Set up variables to hold action type matches
    (setq actChAng 0
          actChClr 0
          actChElv 0
          actChHgt 0
          actChLay 0
          actChLty 0
          actChRad 0
          actChSty 0
          actChVal 0
          actAdd   0
          actArr   0
          actCop   0
          actCnt   0
          actDel   0
          actMir   0
          actMov   0
          actOff   0
          actRot   0
          actScl   0
    )

    ;;;--- Get a count of the selected entities
    (setq selEntCntr (length entityTypes))

    (if(member "ARC" entityTypes)
      (progn

        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actChLty(+ actChLty 1))
        (setq actChRad(+ actChRad 1))
        (setq actAdd(+ actAdd 1))
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "ATTRIBUTE" entityTypes)
      (progn
        (setq actChAng(+ actChAng 1))
        (setq actChElv(+ actChElv 1))
        (setq actChHgt(+ actChHgt 1))
        (setq actChLay(+ actChLay 1))
        (setq actChSty(+ actChSty 1))
        (setq actChVal(+ actChVal 1))
        (setq actAdd(+ actAdd 1))
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))        
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "CIRCLE" entityTypes)
      (progn
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actChLty(+ actChLty 1))
        (setq actChRad(+ actChRad 1))
        (setq actAdd(+ actAdd 1))        
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "ELLIPSE" entityTypes)
      (progn
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actChLty(+ actChLty 1))
        (setq actAdd(+ actAdd 1))        
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "IMAGE" entityTypes)
      (progn
        (setq actChAng(+ actChAng 1))
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))        
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "INSERT" entityTypes)
      (progn
        (setq actChAng(+ actChAng 1))
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actAdd(+ actAdd 1))
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))        
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "LINE" entityTypes)
      (progn
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actChLty(+ actChLty 1))
        (setq actAdd(+ actAdd 1))        
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "LWPOLYLINE" entityTypes)
      (progn
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actChLty(+ actChLty 1))
        (setq actAdd(+ actAdd 1))        
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "MLINE" entityTypes)
      (progn
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actChLty(+ actChLty 1))
        (setq actAdd(+ actAdd 1))        
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "MTEXT" entityTypes)
      (progn
        (setq actChAng(+ actChAng 1))
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChHgt(+ actChHgt 1))
        (setq actChLay(+ actChLay 1))
        (setq actChSty(+ actChSty 1))
        (setq actChVal(+ actChVal 1))
        (setq actAdd(+ actAdd 1))
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))        
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "POINT" entityTypes)
      (progn
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))        
      )
    )
    (if(member "POLYLINE" entityTypes)
      (progn
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actChLty(+ actChLty 1))
        (setq actAdd(+ actAdd 1))        
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "SOLID" entityTypes)
      (progn
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))        
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "TEXT" entityTypes)
      (progn
        (setq actChAng(+ actChAng 1))
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChHgt(+ actChHgt 1))
        (setq actChLay(+ actChLay 1))
        (setq actChSty(+ actChSty 1))
        (setq actChVal(+ actChVal 1))
        (setq actAdd(+ actAdd 1))
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))        
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "TRACE" entityTypes)
      (progn
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actArr(+ actArr 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMir(+ actMir 1)) 
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))        
        (setq actRot(+ actRot 1))
        (setq actScl(+ actScl 1))
      )
    )
    (if(member "XLINE" entityTypes)
      (progn
        (setq actChClr(+ actChClr 1))
        (setq actChElv(+ actChElv 1))
        (setq actChLay(+ actChLay 1))
        (setq actChLty(+ actChLty 1))
        (setq actCop(+ actCop 1))
        (setq actCnt(+ actCnt 1))
        (setq actDel(+ actDel 1))
        (setq actMov(+ actMov 1))
        (setq actOff(+ actOff 1))
        (setq actRot(+ actRot 1))
      )
    )

    ;;;--- Enable or disable action buttons based on entity selection
    (if(>= actChAng selEntCntr)(mode_tile "actchang" 0)(mode_tile "actchang" 1))
    (if(>= actChClr selEntCntr)(mode_tile "actchclr" 0)(mode_tile "actchclr" 1))
    (if(>= actChElv selEntCntr)(mode_tile "actchelv" 0)(mode_tile "actchelv" 1))
    (if(>= actChHgt selEntCntr)(mode_tile "actchhgt" 0)(mode_tile "actchhgt" 1))
    (if(>= actChLay selEntCntr)(mode_tile "actchlay" 0)(mode_tile "actchlay" 1))
    (if(>= actChLty selEntCntr)(mode_tile "actchlty" 0)(mode_tile "actchlty" 1))
    (if(>= actChRad selEntCntr)(mode_tile "actchrad" 0)(mode_tile "actchrad" 1))
    (if(>= actChSty selEntCntr)(mode_tile "actchsty" 0)(mode_tile "actchsty" 1))
    (if(>= actChVal selEntCntr)(mode_tile "actchval" 0)(mode_tile "actchval" 1))
    (if(>= actAdd selEntCntr)  (mode_tile "actadd" 0)  (mode_tile "actadd" 1))
    (if(>= actArr selEntCntr)  (mode_tile "actarr" 0)  (mode_tile "actarr" 1))
    (if(>= actCop selEntCntr)  (mode_tile "actcop" 0)  (mode_tile "actcop" 1))
    (if(>= actCnt selEntCntr)  (mode_tile "actcnt" 0)  (mode_tile "actcnt" 1))
    (if(>= actDel selEntCntr)  (mode_tile "actdel" 0)  (mode_tile "actdel" 1))
    (if(>= actMir selEntCntr)  (mode_tile "actmir" 0)  (mode_tile "actmir" 1))
    (if(>= actMov selEntCntr)  (mode_tile "actmov" 0)  (mode_tile "actmov" 1))
    (if(>= actOff selEntCntr)  (mode_tile "actoff" 0)  (mode_tile "actoff" 1))
    (if(>= actRot selEntCntr)  (mode_tile "actrot" 0)  (mode_tile "actrot" 1))
    (if(>= actScl selEntCntr)  (mode_tile "actscl" 0)  (mode_tile "actscl" 1))

  )



  ;;;--- Function to display the actions in the action list

  (defun displayActions()

    ;;;--- Display them in the action dialog box
    (start_list "actionlist" 3)
    (mapcar 'add_list actionList)
    (end_list)
  )




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                          E N D    O F                            ;;;
  ;;;    A C T I O N    D I A L O G    B O X    F U N C T I O N S      ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







  


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                 M I S C.   F U N C T I O N S                     ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                          ;;;
  ;;;           F U N C T I O N  -  P a r s e S t r I n t o L i s t            ;;;
  ;;;                                                                          ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;;--- ParseStrIntoList > Convert a 2d or 3d point string into a point list
  ;;;
  ;;;        Parameters:
  ;;;
  ;;;           a - String consisting of [x,y] or [x,y,z] 
  ;;;
  ;;;
 
  (defun parseStrIntoList(a / ch cnt ptlist)
    (setq ch "" ptList(list) cnt 0)
    (while(< cnt (strlen a))
      (setq cnt(+ cnt 1))
      (setq ch(substr a cnt 1))
      (if(= ch ",")
        (progn
          (setq ptList(append ptList(list (substr a 1 (- cnt 1)))))
          (setq a(substr a (+ cnt 1)))
          (setq cnt 0)
        )
      )
    )
    (if(> (strlen a) 0)
      (setq ptList(append ptList(list (substr a 1))))
    )
    ptList
  )



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                          ;;;
  ;;;               F U N C T I O N  -  P a r s e A c t i o n                  ;;;
  ;;;                                                                          ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;;--- Function to parse the action string into a list of items
  ;;;
  ;;;    Example:
  ;;;      (parseAction "ANGLE 45.6,67.8,34.5 PROMPT PROMPT")
  ;;;      Returns ("ANGLE" "45.6,67.8,34.5" "PROMPT" "PROMPT") 

  (defun parseAction(str)
    (setq action(list))
    (setq start 1 cntr 1 ch "")
    (while(<= cntr (strlen str))
      (setq ch(substr str cntr 1))
      (if(= ch " ")
        (progn
          (setq action(append action (list(substr str start (- cntr 1)))))
          (setq str(substr str (+ cntr 1)))
          (setq start 1 cntr 0)
          (while(= " " (substr str 1 1))(setq str(substr str 2)))
        )
      )
      (setq cntr(+ cntr 1))
    )
    (if(and (/= str "")(/= (substr str 1 1) " "))
      (setq action(append action (list str)))
    )
    action
  )
  








  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                   W R I T E    F U N C T I O N S                 ;;;
  ;;;                                                                  ;;;
  ;;;                 FUNCTIONS TO WRITE TO AUTOLISP FILE              ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (defun writeFunctionADDVAL()
    (setq addvalFlag nil)
    (princ "\n\n\n" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;                     F U N C T I O N  -  G E T V A L                      ;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;--- GetVal > Function to get the value from text, mtext, attdef, or a block's attribute" fil)
    (princ "\n  ;;;" fil) 
    (princ "\n  ;;;        Parameters:" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;                en - Entity name" fil)
    (princ "\n  ;;;           tagName - Name of an attribute tag to retrieve it's value" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;--- Set up global variable to hold the total" fil)
    (princ "\n  (setq addValTotal 0)" fil)
    (princ "\n " fil)
    (princ "\n  (defun getVal(en tagName / val enlist entType blkType group66)" fil)
    (princ (strcat "\n    (setq val " (chr 34) "0" (chr 34) ")" )fil)
    (princ "\n    (setq enlist(entget en))" fil)
    (princ "\n    (setq entType(cdr(assoc 0 enlist)))" fil)
    (princ "\n    (cond" fil)
    (princ "\n      (" fil)
    (princ "\n        (or" fil)
    (princ (strcat "\n           (= entType " (chr 34) "TEXT" (chr 34) ")") fil)
    (princ (strcat "\n           (= entType " (chr 34) "ATTDEF" (chr 34) ")" ) fil)
    (princ (strcat "\n           (= entType " (chr 34) "MTEXT" (chr 34) ")" ) fil)
    (princ "\n        )" fil)
    (princ "\n        (setq val(cdr(assoc 1 enlist)))" fil)
    (princ "\n      )" fil)
    (princ "\n      (" fil)
    (princ (strcat "\n        (= entType " (chr 34) "INSERT" (chr 34) ")")fil)
    (princ "\n        (progn" fil)
    (princ "\n          (if(cdr(assoc 66 enlist))" fil)
    (princ "\n            (progn" fil)
    (princ "\n              (setq en(entnext en))" fil)
    (princ "\n              (setq enlist(entget en))" fil)
    (princ "\n              (setq blkType (cdr(assoc 0 enlist)))" fil)
    (princ "\n              (setq group66(cdr(assoc 66 enlist)))" fil)
    (princ (strcat "\n              (while(or (= blkType " (chr 34) "ATTRIB" (chr 34) ")(= group66 1))") fil)
    (princ "\n                (setq blkType (cdr(assoc 0 enlist)))" fil)
    (princ (strcat "\n                (if(= blkType " (chr 34) "ATTRIB" (chr 34) ")") fil)
    (princ "\n                  (progn" fil)
    (princ "\n                    (if(= tagName (cdr(assoc 2 enlist)))" fil)
    (princ "\n                      (setq val(cdr(assoc 1 enlist)))" fil)
    (princ "\n                    )" fil)
    (princ "\n                  )" fil)
    (princ "\n                )" fil)
    (princ "\n                (setq en(entnext en))" fil)
    (princ "\n                (setq enlist(entget en))" fil)
    (princ "\n                (setq blkType (cdr(assoc 0 enlist)))" fil)
    (princ "\n                (setq group66(cdr(assoc 66 enlist)))" fil)
    (princ "\n              )" fil)
    (princ "\n            )" fil)
    (princ "\n          )" fil)
    (princ "\n        )" fil)
    (princ "\n      )" fil)
    (princ "\n    )" fil)
    (princ "\n    (atof val)" fil)
    (princ "\n  )\n\n" fil)
  )
  (defun writeFunctionADDARE()
    (setq addAREFlag nil)
    (princ "\n\n\n" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;                    F U N C T I O N  -  G E T A R E A                     ;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;" fil)    
    (princ "\n  ;;;--- GetArea > Returns the area of an object" fil)
    (princ "\n  ;;; " fil)
    (princ "\n  ;;;        Parameters: " fil)
    (princ "\n  ;;; " fil)       
    (princ "\n  ;;;            en - Entity Name" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;--- Set up global variable to hold the total" fil)
    (princ "\n  (setq addAreTotal 0)" fil)
    (princ "\n " fil)
    (princ "\n  (defun getArea(en / areaT)" fil)
    (princ (strcat "\n    (setvar " (chr 34) "cmdecho" (chr 34) " 0)") fil)
    (princ (strcat "\n    (setq areaT " (chr 34) "0" (chr 34)")" )fil)
    (princ (strcat "\n    (command " (chr 34) "area" (chr 34) " " (chr 34) "Object" (chr 34) "  en)")fil)
    (princ (strcat "\n    (setq areaT(getvar " (chr 34) "area" (chr 34) "))") fil)
    (princ (strcat "\n    (setvar " (chr 34) "cmdecho" (chr 34) " 1)") fil)
    (princ "\n    areaT" fil)
    (princ "\n  )\n\n" fil)
  )
  (defun writeFunctionADDLEN()
    (setq addLenFlag nil)
    (princ "\n\n\n" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;                     F U N C T I O N  -  G E T L E N                      ;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;" fil)    
    (princ "\n  ;;;--- GETLEN > Get the length of an entity" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;       Parameters:" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;          en - Entity Name" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;--- Set up global variable to hold the total" fil)
    (princ "\n  (setq addLenTotal 0)" fil)
    (princ "\n  " fil)
    (princ "\n  (defun getLen(en / enlist entType len cpt rad p1 p2 circum perc)" fil)
    (princ "\n    (setq enlist(entget en))" fil)
    (princ "\n    (setq entType(cdr(assoc 0 enlist)))" fil)
    (princ "\n    (cond" fil)
    (princ "\n      (" fil)
    (princ "\n        (or" fil)
    (princ (strcat "\n          (= entType " (chr 34) "LWPOLYLINE" (chr 34) ")") fil)
    (princ (strcat "\n          (= entType " (chr 34) "ELLIPSE" (chr 34) ")") fil)
    (princ (strcat "\n          (= entType " (chr 34) "CIRCLE" (chr 34) ")") fil)
    (princ (strcat "\n          (= entType " (chr 34) "POLYLINE" (chr 34) ")") fil)
    (princ "\n        )" fil)
    (princ "\n        (progn" fil)
    (princ (strcat "\n          (command " (chr 34) "area" (chr 34) " " (chr 34) "Object" (chr 34) " en)") fil)
    (princ (strcat "\n          (setq len(getvar " (chr 34) "perimeter" (chr 34) "))") fil)
    (princ "\n        )" fil)
    (princ "\n      )" fil)
    (princ "\n      ( " fil)
    (princ (strcat "\n        (= entType " (chr 34) "LINE" (chr 34) ")")fil)
    (princ         "\n          (setq len(distance(cdr(assoc 10 enlist))(cdr(assoc 11 enlist))))" fil)
    (princ "\n      )" fil)
    (princ "\n      (" fil)
    (princ (strcat "\n        (= entType " (chr 34) "ARC" (chr 34) ")") fil)
    (princ "\n        (progn" fil)
    (princ "\n          (setq cpt(cdr(assoc 10 enlist)))" fil)
    (princ "\n          (setq rad(cdr(assoc 40 enlist)))" fil)
    (princ "\n          (setq p1(polar cpt (cdr(assoc 50 enlist)) rad))" fil)
    (princ "\n          (setq p2(polar cpt (cdr(assoc 51 enlist)) rad))" fil)
    (princ "\n          (setq circum(* pi (* 2.0 rad)))" fil)
    (princ (strcat "\n          (if (not c:cal)(arxload " (chr 34) "geomcal" (chr 34) "))") fil)
    (princ (strcat "\n          (setq myAng (* pi (/ (cal " (chr 34) "ang(cpt,p1,p2)" (chr 34) ") 180.0)))") fil)
    (princ "\n          (setq perc(/ myAng (* pi 2.0)))" fil)
    (princ "\n          (setq len(* perc circum))" fil)
    (princ "\n        )" fil)
    (princ "\n      )" fil)
    (princ "\n    )" fil)
    (princ "\n    len" fil)
    (princ "\n  )\n\n" fil)
  )
  (defun writeFunctionADDRAD()
    (setq addRadFlag nil)
    (princ "\n\n\n" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;                     F U N C T I O N  -  G E T R A D                      ;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;--- GETRAD > Get the radius of a circle or arc" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;        Parameters:" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;            en - Entity Name" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;--- Set up global variable to hold the total" fil)
    (princ "\n  (setq addRadTotal 0)" fil)
    (princ "\n  " fil)
    (princ "\n  (defun getRad(en / rad enlist entType)" fil)
    (princ "\n    (setq rad 0.0)" fil)
    (princ "\n    (setq enlist(entget en))" fil)
    (princ "\n    (setq entType(cdr(assoc 0 enlist)))" fil)
    (princ (strcat "\n    (if(or(= entType " (chr 34) "CIRCLE" (chr 34) ")(= entType " (chr 34) "ARC" (chr 34) "))") fil)
    (princ "\n      (setq rad(cdr(assoc 40 enlist)))" fil)
    (princ "\n    )" fil)
    (princ "\n    rad" fil)
    (princ "\n  )\n\n" fil)
  )
  (defun writeFunctionADDDIA()
    (setq addDiaFlag nil)
    (princ "\n\n\n" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;                     F U N C T I O N  -  G E T V A L                      ;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;" fil)    
    (princ "\n  ;;;--- GETDIA > Get the diameter of a circle" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;        Parameters:" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;           en - Entity Name" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;--- Set up global variable to hold the total" fil)
    (princ "\n  (setq addDiaTotal 0)" fil)
    (princ "\n " fil)    
    (princ "\n  (defun getDia(en / dia enlist entType)" fil)
    (princ "\n    (setq dia 0.0)" fil)
    (princ "\n    (setq enlist(entget en))" fil)
    (princ "\n    (setq entType(cdr(assoc 0 enlist)))" fil)
    (princ (strcat "\n    (if(= entType " (chr 34) "CIRCLE" (chr 34) ")") fil)
    (princ "\n      (setq dia(* 2.0 (cdr(assoc 40 enlist))))" fil)
    (princ "\n    )" fil)
    (princ "\n    dia" fil)
    (princ "\n  )\n\n" fil)
  )
  (defun writeFunctionADDCIR()
    (setq addCirFlag nil)
    (princ "\n\n\n" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;                     F U N C T I O N  -  G E T C I R                      ;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;" fil)    
    (princ "\n  ;;;--- GETCIR > Get the circumference of a circle" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;         Parameters:" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;            en - Entity Name" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;--- Set up global variable to hold the total" fil)
    (princ "\n  (setq addCirTotal 0)" fil)
    (princ "\n " fil)
    (princ "\n  (defun getCir(en / cir enlist entType)" fil)
    (princ "\n    (setq cir 0.0)" fil)
    (princ "\n    (setq enlist(entget en))" fil)
    (princ "\n    (setq entType(cdr(assoc 0 enlist)))" fil)
    (princ (strcat "\n    (if(= entType " (chr 34) "CIRCLE" (chr 34) ")") fil)
    (princ "\n      (setq cir(* pi (* 2.0 (cdr(assoc 40 enlist)))))" fil)
    (princ "\n    )" fil)
    (princ "\n    cir" fil)
    (princ "\n  )\n\n" fil)    
  )
  (defun writeFunctionADDPER()
    (setq addPerFlag nil)
    (princ "\n\n\n" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;                     F U N C T I O N  -  G E T P E R                      ;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;" fil)    
    (princ "\n  ;;;--- GETPER > Get the perimeter of an ellipse" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;        Parameters:" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;           en - Entity Name" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;--- Set up global variable to hold the total" fil)
    (princ "\n  (setq addPerTotal 0)" fil)
    (princ "\n " fil)
    (princ "\n  (defun getPer(en / per)" fil)
    (princ "\n    (setq per 0.0)" fil)
    (princ (strcat "\n    (command " (chr 34) "area" (chr 34) " " (chr 34) "Object" (chr 34) "  en)")fil)
    (princ (strcat "\n    (setq per(getvar " (chr 34) "perimeter" (chr 34) "))") fil)
    (princ "\n    per" fil)
    (princ "\n  )\n\n" fil)    
  )
  (defun writeFunctionDeleteEach()
    (setq deleteFlag nil)
    (princ "\n\n\n" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;                 F U N C T I O N  -  D e l e t e E n t i t y              ;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;" fil)    
    (princ "\n  ;;;--- deleteEntity > Prompt for deletion of an entity" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;        Parameters:" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;           en - Entity Name" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  " fil)
    (princ "\n  (defun deleteEntity(en / lt pt ans)" fil)
    (princ (strcat "\n    (setq lt(getvar " (chr 34) "dimscale" (chr 34) "))") fil)
    (princ "\n    (if(setq pt(cdr(assoc 10 (entget en))))" fil)
    (princ "\n      (progn" fil)
    (princ "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
    (princ "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
    (princ "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
    (princ "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
    (princ "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
    (princ "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
    (princ "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
    (princ (strcat "\n        (initget 1 " (chr 34) "Yes No" (chr 34) ")") fil)
    (princ (strcat "\n        (setq ans(getkword " (chr 34) "\\n Delete this entity? Yes/No : " (chr 34) "))") fil)
    (princ (strcat "\n        (if(= ans " (chr 34) "Yes" (chr 34) ")") fil)
    (princ         "\n          (entdel en)" fil)
    (princ         "\n        )" fil)
    (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
    (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
    (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
    (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
    (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
    (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
    (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
    (princ         "\n      )" fil)
    (princ         "\n    )" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun writeFunctionCount(selectType)
    (setq countFlag nil)
    (princ "\n\n\n" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;              F U N C T I O N  -  C o u n t E n t i t i e s               ;;;" fil)
    (princ "\n  ;;;                                                                          ;;;" fil)
    (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ "\n  ;;;" fil)    
    (princ "\n  ;;;--- countEntities > Count entities" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;        Parameters:" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;           eset       - Selection set of entities" fil)
    (princ "\n  ;;;           selectType -  1=Global selection, anything else means a selection set." fil)
    (princ "\n  ;;;" fil)
    (princ "\n  ;;;" fil)
    (princ "\n  " fil)
    (princ "\n  (defun countEntities(eset selectType / totals cntr en enlist entType eList)" fil)
    (princ (strcat "\n    (setq totals " (chr 34)(chr 34) " eList (list))") fil)
    (princ "\n    (setq cntr 0)" fil)
    (princ "\n    (while(< cntr (sslength eset))" fil)
    (princ "\n      (setq en(ssname eset cntr))" fil)
    (princ "\n      (setq enlist(entget en))" fil)
    (princ "\n      (setq entType(cdr(assoc 0 enlist)))" fil)
    (princ "\n      (if(not(member entType eList))" fil)
    (princ "\n        (setq eList(append eList (list entType)))" fil)
    (princ "\n      )" fil)
    (princ "\n      (setq cntr(+ cntr 1))" fil)
    (princ "\n    )" fil)
    (princ "\n    (if(= selectType 1)" fil)
    (princ "\n      (progn" fil)
    (princ "\n        (foreach a eList" fil)
    (princ (strcat "\n          (setq eset(ssget " (chr 34) "X" (chr 34) " (list(cons 0 a))))") fil)
    (princ (strcat "\n          (setq totals(strcat totals " (chr 34) "\\n" (chr 34) " a " (chr 34)" - "(chr 34) " (itoa (sslength eset))))") fil)
    (princ "\n        )" fil)
    (princ "\n      )" fil)
    (princ "\n      (progn" fil)
    (princ "\n        (foreach a eList" fil)
    (princ "\n          (setq cntr 0 tcnt 0)" fil)
    (princ "\n          (while(< cntr (sslength eset))" fil)
    (princ "\n            (setq en(ssname eset cntr))" fil)
    (princ "\n            (setq enlist(entget en))" fil)
    (princ "\n            (setq entType(cdr(assoc 0 enlist)))" fil)
    (princ "\n            (if(= a entType)(setq tcnt(+ tcnt 1)))" fil)
    (princ "\n            (setq cntr(+ cntr 1))" fil)
    (princ "\n          )" fil)
    (princ (strcat "\n          (setq totals(strcat totals " (chr 34)"\\n" (chr 34) " a " (chr 34) " - "(chr 34) "(itoa tcnt)))" ) fil)
    (princ "\n        )" fil)
    (princ "\n      )" fil)
    (princ "\n    )" fil)
    (princ "\n    totals" fil)
    (princ "\n  )\n\n" fil)
  )
  (defun FilterAttValuX()
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  F i l t e r A t t V a l u X              ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- FilterAttValuX > Filter a selection set for matching attribute value" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           eset - Selection set of entities" fil)
    (princ         "\n  ;;;           valu - Value of an attribute [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Selection set with only entities that have matching attribute values." fil)    
    (princ         "\n  ;;;" fil)
    (princ         "\n  " fil)    
    (princ         "\n  (defun FilterAttValuX(eset valu)" fil)
    (princ         "\n    (setq cntr 0 newSet(ssadd))" fil)
    (princ         "\n    (while(< cntr (sslength eset))" fil)
    (princ         "\n      (setq en(ssname eset cntr))" fil)
    (princ         "\n      (setq en2 en)" fil)
    (princ         "\n      (setq enlist(entget en))" fil)
    (princ (strcat "\n      (if(= " (chr 34) "INSERT" (chr 34) " (cdr(assoc 0 enlist)))") fil)
    (princ         "\n        (progn" fil)
    (princ         "\n          (if(cdr(assoc 66 enlist))" fil)
    (princ         "\n            (progn" fil)
    (princ         "\n              (setq en(entnext en))" fil)
    (princ         "\n              (setq enlist(entget en))" fil)
    (princ         "\n              (setq blkType (cdr(assoc 0 enlist)))" fil)
    (princ         "\n              (setq group66(cdr(assoc 66 enlist)))" fil)
    (princ (strcat "\n              (while(or (= blkType " (chr 34) "ATTRIB" (chr 34) ")(= group66 1))") fil)
    (princ         "\n                (setq blkType (cdr(assoc 0 enlist)))" fil)
    (princ (strcat "\n                (if(= blkType " (chr 34) "ATTRIB" (chr 34) ")" ) fil)
    (princ         "\n                  (progn" fil)
    (princ         "\n                    (if(= (strcase valu) (strcase(cdr(assoc 1 enlist))))" fil)
    (princ         "\n                      (ssadd en2 newSet)" fil)
    (princ         "\n                    )" fil)
    (princ         "\n                  )" fil)
    (princ         "\n                )" fil)
    (princ         "\n                (setq en(entnext en))" fil)
    (princ         "\n                (setq enlist(entget en))" fil)
    (princ         "\n                (setq blkType (cdr(assoc 0 enlist)))" fil)
    (princ         "\n                (setq group66(cdr(assoc 66 enlist)))" fil)
    (princ         "\n              )" fil)
    (princ         "\n            )" fil)
    (princ         "\n          )" fil)
    (princ         "\n        )" fil)
    (princ         "\n      )" fil)
    (princ         "\n      (setq cntr(+ cntr 1))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    newSet" fil)
    (princ         "\n  )\n\n" fil)
  )   
  (defun FilterBlkNameX()
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  F i l t e r B l k N a m e X              ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- FilterBlkNameX > Filter a selection set for matching block name" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           eset - Selection set of entities" fil)
    (princ         "\n  ;;;           valu - Block name [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;       Returns: Selection set with only entities that have matching block name." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  " fil)        
    (princ         "\n  (defun FilterBlkNameX(eset valu)" fil)
    (princ         "\n    (setq cntr 0 newSet(ssadd))" fil)
    (princ         "\n    (while(< cntr (sslength eset))" fil)
    (princ         "\n      (setq en(ssname eset cntr))" fil)
    (princ         "\n      (setq enlist(entget en))" fil)
    (princ (strcat "\n      (if(= " (chr 34) "INSERT" (chr 34) " (cdr(assoc 0 enlist)))") fil)
    (princ         "\n        (progn" fil)
    (princ         "\n          (if(= (strcase valu)(strcase (cdr(assoc 2 enlist))))" fil)
    (princ         "\n            (ssadd en newSet)" fil)
    (princ         "\n          )" fil)
    (princ         "\n        )" fil)
    (princ         "\n      )" fil)
    (princ         "\n      (setq cntr(+ cntr 1))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    newSet" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun FilterClrNumbX()
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  F i l t e r C l r N u m b X              ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- FilterClrNumbX > Filter a selection set for matching color" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           eset - Selection set of entities" fil)
    (princ         "\n  ;;;           valu - Color Number [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Selection set with only entities that have matching color." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  "    fil)            
    (princ         "\n  (defun FilterClrNumbX(eset valu)" fil)
    (princ         "\n    (setq valu(atoi valu) layerList(list))" fil)
    (princ (strcat "\n    (setq tbl(tblnext " (chr 34) "LAYER" (chr 34) " T))") fil)
    (princ         "\n    (if(= valu (cdr(assoc 62 tbl)))" fil)
    (princ         "\n      (setq layerList(append layerList (list (cdr(assoc 2 tbl)))))" fil)
    (princ         "\n    )" fil)
    (princ (strcat "\n    (while(setq tbl(tblnext " (chr 34) "LAYER" (chr 34) "))") fil)
    (princ         "\n      (if(= valu (cdr(assoc 62 tbl)))" fil)
    (princ         "\n        (setq layerList(append layerList (list (cdr(assoc 2 tbl)))))" fil)
    (princ         "\n      )" fil)
    (princ         "\n    )" fil)
    (princ         "\n    (setq cntr 0 newSet(ssadd))" fil)
    (princ         "\n    (while(< cntr (sslength eset))" fil)
    (princ         "\n      (setq en(ssname eset cntr))" fil)
    (princ         "\n      (setq enlist(entget en))" fil)
    (princ         "\n      (if (assoc 62 enlist)" fil)
    (princ         "\n        (if(= valu (cdr(assoc 62 enlist)))" fil)
    (princ         "\n          (ssadd en newSet)" fil)
    (princ         "\n        )" fil)
    (princ         "\n        (if(member (cdr(assoc 8 enlist)) layerList)" fil)
    (princ         "\n          (ssadd en newSet)" fil)
    (princ         "\n        )" fil)
    (princ         "\n      )" fil)
    (princ         "\n      (setq cntr(+ cntr 1))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    newSet" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun FilterLayNameX()
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  F i l t e r L a y N a m e X              ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;--- FilterLayNameX > Filter a selection set for matching layer name" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           eset - Selection set of entities" fil)
    (princ         "\n  ;;;           valu - Layer name [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Selection set with only entities that have matching layer name." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  " fil)            
    (princ         "\n  (defun FilterLayNameX(eset valu)" fil)
    (princ         "\n    (setq cntr 0 newSet(ssadd))" fil)
    (princ         "\n    (while(< cntr (sslength eset))" fil)
    (princ         "\n      (setq en(ssname eset cntr))" fil)
    (princ         "\n      (setq enlist(entget en))" fil)
    (princ         "\n      (if(= valu (cdr(assoc 8 enlist)))" fil)
    (princ         "\n        (ssadd en newSet)" fil)
    (princ         "\n      )" fil)
    (princ         "\n      (setq cntr(+ cntr 1))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    newSet" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun FilterLtyNameX()
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  F i l t e r L t y N a m e X              ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;--- FilterLtyNameX > Filter a selection set for matching linetype" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           eset - Selection set of entities" fil)
    (princ         "\n  ;;;           valu - Linetype Name [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Selection set with only entities that have matching linetype." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  "    fil)            
    (princ         "\n  (defun FilterLtyNameX(eset valu)" fil)
    (princ         "\n    (setq valu(atoi valu) layerList(list))" fil)
    (princ (strcat "\n    (setq tbl(tblnext " (chr 34) "LTYPE" (chr 34) " T))") fil)
    (princ         "\n    (if(= valu (cdr(assoc 6 tbl)))" fil)
    (princ         "\n      (setq layerList(append layerList (list (cdr(assoc 2 tbl)))))" fil)
    (princ         "\n    )" fil)
    (princ (strcat "\n    (while(setq tbl(tblnext " (chr 34) "LTYPE" (chr 34) "))") fil)
    (princ         "\n      (if(= valu (cdr(assoc 6 tbl)))" fil)
    (princ         "\n        (setq layerList(append layerList (list (cdr(assoc 2 tbl)))))" fil)
    (princ         "\n      )" fil)
    (princ         "\n    )" fil)
    (princ         "\n    (setq cntr 0 newSet(ssadd))" fil)
    (princ         "\n    (while(< cntr (sslength eset))" fil)
    (princ         "\n      (setq en(ssname eset cntr))" fil)
    (princ         "\n      (setq enlist(entget en))" fil)
    (princ         "\n      (if (assoc 6 enlist)" fil)
    (princ         "\n        (if(= valu (cdr(assoc 6 enlist)))" fil)
    (princ         "\n          (ssadd en newSet)" fil)
    (princ         "\n        )" fil)
    (princ         "\n        (if(member (cdr(assoc 8 enlist)) layerList)" fil)
    (princ         "\n          (ssadd en newSet)" fil)
    (princ         "\n        )" fil)
    (princ         "\n      )" fil)
    (princ         "\n      (setq cntr(+ cntr 1))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    newSet" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun FilterMaxAxisX()
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  F i l t e r M a x A x i s X              ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;--- FilterMaxAxisX > Filter a selection set for matching Major Axis" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           eset - Selection set of entities" fil)
    (princ         "\n  ;;;           valu - Lenth of Major Axis [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Selection set with only ellipses that have matching major axis." fil)    
    (princ         "\n  ;;;" fil)
    (princ         "\n  " fil)        
    (princ         "\n  (defun FilterMaxAxisX(eset valu)" fil)
    (princ         "\n    (setq cntr 0 newSet(ssadd) valu(distof valu))" fil)
    (princ         "\n    (while(< cntr (sslength eset))" fil)
    (princ         "\n      (setq en(ssname eset cntr))" fil)
    (princ         "\n      (setq enlist(entget en))" fil)
    (princ         "\n      (setq maxAxis(* 2.0 (distance (list 0.0 0.0)(cdr(assoc 11 enlist)))))" fil)
    (princ         "\n      (if(equal valu maxAxis 0.0001)" fil)
    (princ         "\n        (ssadd en newSet)" fil)
    (princ         "\n      )" fil)
    (princ         "\n      (setq cntr(+ cntr 1))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    newSet" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun FilterRadValuX()
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  F i l t e r R a d V a l u X              ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;--- FilterRadValuX > Filter a selection set for a matching radius" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           eset - Selection set of entities" fil)
    (princ         "\n  ;;;           valu - Radius [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Selection set with only entities that have a matching radius." fil)    
    (princ         "\n  ;;;" fil)
    (princ         "\n  " fil)        
    (princ         "\n  (defun FilterRadValuX(eset valu)" fil)
    (princ         "\n    (setq cntr 0 newSet(ssadd))" fil)
    (princ         "\n    (setq valu(distof valu))" fil)
    (princ         "\n    (while(< cntr (sslength eset))" fil)
    (princ         "\n      (setq en(ssname eset cntr))" fil)
    (princ         "\n      (setq enlist(entget en))" fil)
    (princ         "\n      (if(equal valu (cdr(assoc 40 enlist)) 0.0001)" fil)
    (princ         "\n        (ssadd en newSet)" fil)
    (princ         "\n      )" fil)
    (princ         "\n      (setq cntr(+ cntr 1))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    newSet" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun FilterTagNameX()
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  F i l t e r T a g N a m e X              ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- FilterTagNameX > Filter a selection set for matching attribute tag" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           eset - Selection set of entities" fil)
    (princ         "\n  ;;;           valu - Attribute Tag Name [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Selection set with only entities that have matching attribute tags." fil)    
    (princ         "\n  ;;;" fil)
    (princ         "\n  " fil)    
    (princ         "\n  (defun FilterTagNameX(eset valu)" fil)
    (princ         "\n    (setq cntr 0 newSet(ssadd))" fil)
    (princ         "\n    (while(< cntr (sslength eset))" fil)
    (princ         "\n      (setq en(ssname eset cntr))" fil)
    (princ         "\n      (setq en2 en)" fil)
    (princ         "\n      (setq enlist(entget en))" fil)
    (princ (strcat "\n      (if(= " (chr 34) "INSERT" (chr 34) " (cdr(assoc 0 enlist)))") fil)
    (princ         "\n        (progn" fil)
    (princ         "\n          (if(cdr(assoc 66 enlist))" fil)
    (princ         "\n            (progn" fil)
    (princ         "\n              (setq en(entnext en))" fil)
    (princ         "\n              (setq enlist(entget en))" fil)
    (princ         "\n              (setq blkType (cdr(assoc 0 enlist)))" fil)
    (princ         "\n              (setq group66(cdr(assoc 66 enlist)))" fil)
    (princ (strcat "\n              (while(or (= blkType " (chr 34) "ATTRIB" (chr 34) ")(= group66 1))") fil)
    (princ         "\n                (setq blkType (cdr(assoc 0 enlist)))" fil)
    (princ (strcat "\n                (if(= blkType " (chr 34) "ATTRIB" (chr 34) ")" ) fil)
    (princ         "\n                  (progn" fil)
    (princ         "\n                    (if(= (strcase valu) (strcase(cdr(assoc 2 enlist))))" fil)
    (princ         "\n                      (ssadd en2 newSet)" fil)
    (princ         "\n                    )" fil)
    (princ         "\n                  )" fil)
    (princ         "\n                )" fil)
    (princ         "\n                (setq en(entnext en))" fil)
    (princ         "\n                (setq enlist(entget en))" fil)
    (princ         "\n                (setq blkType (cdr(assoc 0 enlist)))" fil)
    (princ         "\n                (setq group66(cdr(assoc 66 enlist)))" fil)
    (princ         "\n              )" fil)
    (princ         "\n            )" fil)
    (princ         "\n          )" fil)
    (princ         "\n        )" fil)
    (princ         "\n      )" fil)
    (princ         "\n      (setq cntr(+ cntr 1))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    newSet" fil)
    (princ         "\n  )\n\n" fil)
  )   
  (defun FilterTxtValuX()
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  F i l t e r R a d V a l u X              ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- FilterTxtValuX > Filter a selection set for matching text value" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           eset - Selection set of entities" fil)
    (princ         "\n  ;;;           valu - Text Value [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Selection set with only entities that have matching text values." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  " fil)            
    (princ         "\n  (defun FilterTxtValuX(eset valu)" fil)
    (princ         "\n    (setq cntr 0 newSet(ssadd))" fil)
    (princ         "\n    (while(< cntr (sslength eset))" fil)
    (princ         "\n      (setq en(ssname eset cntr))" fil)
    (princ         "\n      (setq enlist(entget en))" fil)
    (princ         "\n      (if(= (strcase valu) (strcase (cdr(assoc 1 enlist))))" fil)
    (princ         "\n        (ssadd en newSet)" fil)
    (princ         "\n      )" fil)
    (princ         "\n      (setq cntr(+ cntr 1))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    newSet" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun FilterStyNameX()
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  F i l t e r S t y N a m e X              ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- FilterStyNameX > Filter a selection set for matching text style" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           eset - Selection set of entities" fil)
    (princ         "\n  ;;;           valu - Text Style [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Selection set with only entities that have matching text styles." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  "    fil)            
    (princ         "\n  (defun FilterStyNameX(eset valu)" fil)
    (princ         "\n    (setq cntr 0 newSet(ssadd))" fil)
    (princ         "\n    (while(< cntr (sslength eset))" fil)
    (princ         "\n      (setq en(ssname eset cntr))" fil)
    (princ         "\n      (setq enlist(entget en))" fil)
    (princ         "\n      (if(= (strcase valu) (strcase(cdr(assoc 7 enlist))))" fil)
    (princ         "\n        (ssadd en newSet)" fil)
    (princ         "\n      )" fil)
    (princ         "\n      (setq cntr(+ cntr 1))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    newSet" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun writeValFirst()
    (setq valFirstFlag nil)
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  ReplaceFirstOccurrence                   ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- ReplaceFirstOccurrence > Replace the first occurrence of a string value" fil)
    (princ         "\n  ;;;                             inside a string" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           enlist - Entity dxf group code list" fil)
    (princ         "\n  ;;;           oldStr - Text String to search for     [string]" fil)
    (princ         "\n  ;;;           newStr - Text String to replace oldStr [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Entity dxf group code list with the new value." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  "    fil)
    (princ         "\n  (defun replaceFirstOccurrence(enlist oldStr newStr)" fil)
    (princ         "\n    (setq str(cdr(assoc 1 enlist)))" fil)
    (princ (strcat "\n    (setq ch " (chr 34) (chr 34) " cnt 0)") fil)
    (princ         "\n    (while(and(< cnt (- (strlen str)(strlen oldStr))) (/= ch oldStr))" fil)
    (princ         "\n      (setq cnt(+ cnt 1))" fil)
    (princ         "\n      (setq ch(substr str cnt (strlen oldStr)))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    (if(= ch oldStr)" fil)
    (princ         "\n      (progn" fil)
    (princ         "\n        (setq 1stPart(substr str 1 (- cnt 1)))" fil)
    (princ         "\n        (setq 1stPart(strcat 1stPart newStr))" fil)
    (princ         "\n        (setq 1stPart(strcat 1stPart (substr str (+ cnt (strlen oldStr)))))" fil)
    (princ         "\n        (setq enlist(subst (cons 1 1stPart)(assoc 1 enlist)enlist))" fil)
    (princ         "\n      )" fil)
    (princ         "\n    )" fil)
    (princ         "\n    enlist" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun writeValLast()
    (setq valLastFlag nil)    
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  ReplaceLastOccurrence                    ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- ReplaceLastOccurrence > Replace the last occurrence of a string value" fil)
    (princ         "\n  ;;;                            inside a string" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           enlist - Entity dxf group code list" fil)
    (princ         "\n  ;;;           oldStr - Text String to search for     [string]" fil)
    (princ         "\n  ;;;           newStr - Text String to replace oldStr [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Entity dxf group code list with the new value." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  "    fil)    
    (princ         "\n  (defun replaceLastOccurrence(enlist oldStr newStr)" fil)
    (princ         "\n    (setq str(cdr(assoc 1 enlist)))" fil)
    (princ (strcat "\n    (setq ch "(chr 34)(chr 34)" cnt 0 index nil)") fil)
    (princ         "\n    (while (< cnt (strlen str))" fil)
    (princ         "\n      (setq cnt(+ cnt 1))" fil)
    (princ         "\n      (setq ch(substr str cnt (strlen oldStr)))" fil)
    (princ         "\n      (if(= ch oldStr)(setq index cnt))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    (if index" fil)
    (princ         "\n      (progn" fil)
    (princ         "\n        (setq 1stPart(substr str 1 (- index 1)))" fil)
    (princ         "\n        (setq 1stPart(strcat 1stPart newStr))" fil)
    (princ         "\n        (setq 1stPart(strcat 1stPart (substr str (+ index (strlen oldStr)))))" fil)
    (princ         "\n        (setq enlist(subst(cons 1 1stPart)(assoc 1 enlist)enlist))" fil)
    (princ         "\n      )" fil)
    (princ         "\n    )" fil)
    (princ         "\n    enlist" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun writeValEvery()
    (setq valEveryFlag nil)    
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  ReplaceEveryOccurrence                   ;;;" fil)
    (princ         "\n  ;;;                                                                          ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- ReplaceEveryOccurrence > Replace every occurrence of a string value" fil)
    (princ         "\n  ;;;                             inside a string" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           enlist - Entity dxf group code list" fil)
    (princ         "\n  ;;;           oldStr - Text String to search for     [string]" fil)
    (princ         "\n  ;;;           newStr - Text String to replace oldStr [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Entity dxf group code list with the new value." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  "    fil)    
    (princ         "\n  (defun replaceEveryOccurrence(enlist oldStr newStr)" fil)
    (princ         "\n    (setq str(cdr(assoc 1 enlist)))" fil)
    (princ (strcat "\n    (setq ch "(chr 34)(chr 34) " cnt 0 flag nil)") fil)
    (princ         "\n    (while (< cnt (strlen str))" fil)
    (princ         "\n      (setq cnt(+ cnt 1))" fil)
    (princ         "\n      (setq ch(substr str cnt (strlen oldStr)))" fil)
    (princ         "\n      (if(= ch oldStr)" fil)
    (princ         "\n        (progn" fil)
    (princ         "\n          (setq 1stPart(substr str 1 (- cnt 1)))" fil)
    (princ         "\n          (setq 1stPart(strcat 1stPart newStr))" fil)
    (princ         "\n          (setq str(strcat 1stPart (substr str (+ cnt (strlen oldStr)))))" fil)
    (princ         "\n          (setq flag T)" fil)
    (princ         "\n        )" fil)          
    (princ         "\n      )" fil)
    (princ         "\n    )" fil)
    (princ         "\n    (if flag" fil)
    (princ         "\n      (setq enlist(subst (cons 1 str)(assoc 1 enlist)enlist))" fil)
    (princ         "\n    )" fil)
    (princ         "\n    enlist" fil)
    (princ         "\n  )\n\n" fil)
  )
  (defun writeValPrefix()
    (setq valPrefixFlag nil)    
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                 ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  prefixValue                     ;;;" fil)
    (princ         "\n  ;;;                                                                 ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- prefixValue > Increment the first part of a string" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           enlist - Entity dxf group code list" fil)
    (princ         "\n  ;;;           incStr - Increment    [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Entity dxf group code list with the new value." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  "    fil)        
    (princ         "\n  (defun prefixValue(enlist incStr)" fil)
    (princ         "\n    (setq numList" fil)
    (princ         "\n      (list" fil)
    (princ (strcat "\n        " (chr 34) "0" (chr 34) " " (chr 34) "1" (chr 34) " " (chr 34) "2" (chr 34)) fil)
    (princ (strcat "\n        " (chr 34) "3" (chr 34) " " (chr 34) "4" (chr 34) " " (chr 34) "5" (chr 34)) fil)
    (princ (strcat "\n        " (chr 34) "6" (chr 34) " " (chr 34) "7" (chr 34) " " (chr 34) "8" (chr 34)) fil)
    (princ (strcat "\n        " (chr 34) "9" (chr 34) " " (chr 34) "0" (chr 34) " " (chr 34) "." (chr 34)) fil)
    (princ         "\n      )" fil)
    (princ         "\n    )" fil)
    (princ         "\n    (setq oldStr(cdr(assoc 1 enlist)))" fil)
    (princ         "\n    (setq numPfx(atof oldStr))" fil)
    (princ         "\n    (setq cnt 0 decimalIndex 0 index 0 decimalPlaces 0)" fil)
    (princ         "\n    (if(member (substr oldStr 1 1) numList)" fil)
    (princ         "\n      (progn" fil)    
    (princ         "\n        (while(< cnt (strlen oldStr))" fil)
    (princ         "\n          (setq cnt(+ cnt 1))" fil)
    (princ         "\n          (setq ch(substr oldStr cnt 1))" fil)
    (princ         "\n          (if(member ch numList)" fil)
    (princ         "\n            (progn" fil)
    (princ         "\n              (setq index cnt)" fil)
    (princ (strcat "\n              (if(= " (chr 34) "." (chr 34) " ch)(setq decimalIndex cnt))") fil)
    (princ         "\n            )  " fil)
    (princ         "\n            (setq cnt(+ cnt (strlen oldStr)))" fil)
    (princ         "\n          )" fil)
    (princ         "\n        )" fil)
    (princ         "\n        (if(> index 0)" fil)
    (princ         "\n          (progn" fil)
    (princ         "\n            (if(> decimalIndex 0)" fil)
    (princ         "\n              (setq decimalPlaces (- index decimalIndex))" fil)
    (princ         "\n            )  " fil)
    (princ         "\n            (setq numPfx(+ numPfx (atof incStr)))" fil)
    (princ         "\n            (setq numPfx(rtos numPfx 2 decimalPlaces))" fil)
    (princ         "\n            (while(< (strlen numPfx) index)" fil)
    (princ (strcat "\n              (setq numPfx(strcat "(chr 34) "0"(chr 34) " numPfx))") fil)
    (princ         "\n            )" fil)
    (princ         "\n            (setq newStr(strcat numPfx (substr oldStr (+ index 1))))" fil)
    (princ         "\n            (setq enlist(subst(cons 1 newStr)(assoc 1 enlist)enlist))" fil)
    (princ         "\n          )" fil)
    (princ         "\n        )" fil)
    (princ         "\n      )" fil)
    (princ         "\n    )" fil)
    (princ         "\n    enlist" fil)
    (princ         "\n  )\n\n" fil)
  )  
  (defun writeValSuffix()
    (setq valSuffixFlag nil)        
    (princ "\n\n\n" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;                                                                 ;;;" fil)
    (princ         "\n  ;;;             F U N C T I O N  -  suffixValue                     ;;;" fil)
    (princ         "\n  ;;;                                                                 ;;;" fil)
    (princ         "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
    (princ         "\n  ;;;" fil)    
    (princ         "\n  ;;;--- suffixValue > Increment the last part of a string" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Parameters:" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;           enlist - Entity dxf group code list" fil)
    (princ         "\n  ;;;           incStr - Increment    [string]" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        Returns: Entity dxf group code list with the new value." fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  ;;;        N O T E ..... REQUIRES: prefixValue function to run!" fil)
    (princ         "\n  ;;;" fil)
    (princ         "\n  " fil)    
    (princ         "\n  (defun suffixValue(enlist incStr)" fil)
    (princ         "\n    (setq numList" fil)
    (princ         "\n      (list" fil)
    (princ (strcat "\n        " (chr 34) "0" (chr 34) " " (chr 34) "1" (chr 34) " " (chr 34) "2" (chr 34)) fil)
    (princ (strcat "\n        " (chr 34) "3" (chr 34) " " (chr 34) "4" (chr 34) " " (chr 34) "5" (chr 34)) fil)
    (princ (strcat "\n        " (chr 34) "6" (chr 34) " " (chr 34) "7" (chr 34) " " (chr 34) "8" (chr 34)) fil)
    (princ (strcat "\n        " (chr 34) "9" (chr 34) " " (chr 34) "0" (chr 34) " " (chr 34) "." (chr 34)) fil)
    (princ         "\n      )" fil)
    (princ         "\n    )" fil)
    (princ         "\n    (setq oldStr(cdr(assoc 1 enlist)))" fil)
    (princ         "\n    (setq cnt(strlen oldStr))" fil)
    (princ         "\n    (if(member (substr oldStr (strlen oldStr) 1) numList)" fil)
    (princ         "\n      (progn" fil)
    (princ         "\n        (while(> cnt 0)" fil)
    (princ         "\n          (setq ch(substr oldStr cnt 1))" fil)
    (princ         "\n          (if(not (member ch numList))" fil)
    (princ         "\n            (setq stNum (+ cnt 1) cnt 0)" fil)
    (princ         "\n          )" fil)
    (princ         "\n          (setq cnt(- cnt 1))" fil)
    (princ         "\n        )" fil)
    (princ         "\n        (setq 1stPart(substr oldStr 1 (- stNum 1)))" fil)
    (princ         "\n        (setq numStr(substr oldStr stNum))" fil)
    (princ         "\n        (setq tmplist(subst(cons 1 numStr)(assoc 1 enlist)enlist))" fil)
    (princ         "\n        (setq tmplist(prefixValue tmplist incStr))" fil)
    (princ         "\n        (setq newStr(cdr(assoc 1 tmplist)))" fil)
    (princ         "\n        (setq newStr(strcat 1stPart newStr))" fil)
    (princ         "\n        (setq enlist(subst(cons 1 newStr)(assoc 1 enlist)enlist))" fil)
    (princ         "\n      )" fil)
    (princ         "\n    )" fil)
    (princ         "\n    enlist" fil)
    (princ         "\n  )\n\n" fil)
  )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;            E N D    O F    W R I T E    F U N C T I O N S        ;;;
  ;;;                 FUNCTIONS TO WRITE TO AUTOLISP FILE              ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




  




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                                 ;;;
  ;;;                                                                                 ;;;
  ;;;                                                                                 ;;;
  ;;;                                                                                 ;;;
  ;;;      W r i t e   t h e   c o d e   f o r   e n t i t y   s e l e c t i o n      ;;;
  ;;;                                                                                 ;;;
  ;;;                                                                                 ;;;
  ;;;                                                                                 ;;;
  ;;;                                                                                 ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun  writeSelectionCode()

    ;;;--- Decide which entity selection method was selected...
    (cond

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;                                                ;;;
       ;;;--- Write the code for single selection         ;;;
       ;;;                                                ;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (
        (= selSing 1)
        (progn
          (princ         "\n" fil)
          (princ (strcat "\n  ;;;--- Let the user select an entity") fil)
          (princ (strcat "\n  (setq ent(entsel " (chr 34) "\\n Select object: " (chr 34) "))") fil)
          (princ         "\n  " fil)
          (princ         "\n  ;;;--- If the point selected contains an entity..." fil)
          (princ         "\n  (if(and ent (setq en(car ent)))" fil)
          (princ         "\n    (progn" fil)
          (princ         "\n    " fil)
          (princ         "\n      ;;;--- Create an empty selection set" fil)
          (princ         "\n      (setq eset(ssadd))" fil)
          (princ         "\n      " fil)
          (princ         "\n      ;;;--- Add the entity to the empty selection set..." fil)
          (princ         "\n      ;;;    [ This is not necessary but is for consistency only ]" fil)
          (princ         "\n      (ssadd en eset)" fil)
          (princ         "\n    )" fil)
          (princ         "\n  )" fil)
          (princ         "\n  " fil)
          (princ         "\n  ;;;--- Make sure entity selected matches the entity types selected" fil)
          (princ         "\n  (if (and eset (> (sslength eset) 0))" fil)
          (princ         "\n    (progn" fil)
          (princ         "\n      (setq enlist(entget en))" fil)
          (princ         "\n      (setq enType(cdr(assoc 0 enlist)))" fil)
          (princ         "\n      (if" fil)
          (princ         "\n        (not" fil)
          (princ         "\n          (member " fil)
          (princ         "\n            enType" fil)
          (princ         "\n            (list " fil)
          (princ         "\n" fil)
          (foreach a entityTypes
            (princ(strcat  "              " (chr 34) a (chr 34)"\n") fil)
          )
          (princ         "            )" fil)
          (princ         "\n          )" fil)
          (princ         "\n        )" fil)
          (princ         "\n        (ssdel en eset)" fil)
          (princ         "\n      )" fil)
          (princ         "\n    )" fil)
          (princ         "\n  )" fil)
        )
      )
       
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                                  ;;;
      ;;;--- Write the code for multiple selection         ;;;
      ;;;                                                  ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (
        (= selMult 1)
        (progn
          
          ;;;--- If the entityType list contains more than one item...
          (if(> (length entityTypes) 1)
            (progn
              (princ          "\n" fil)
              (princ          "\n  ;;;--- Get a selection set" fil)
              (princ          "\n  (setq eset" fil)
              (princ          "\n    (ssget" fil)                                  
              (princ          "\n      (list" fil)
              (princ (strcat  "\n        (cons -4 " (chr 34) "<OR" (chr 34) ")" ) fil)
              (foreach e entityTypes
                (princ(strcat "\n          (cons 0 " (chr 34) e (chr 34) ")" ) fil)
              )
              (princ (strcat  "\n        (cons -4 " (chr 34) "OR>" (chr 34) ")" ) fil)
              (princ          "\n      )" fil)   ;'
              (princ          "\n    )" fil)     ;ssget
              (princ          "\n  )" fil)       ;setq
            )
            
            ;;;--- Else, the entityType list contains one item...
            (progn
              (princ         "\n  ;;;--- Get a selection set" fil)                                  
              (princ (strcat "\n  (setq eset(ssget (list(cons 0 " (chr 34) (car entityTypes)(chr 34) "))))" ) fil)
            )
          )
        )
      )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                                  ;;;
      ;;;--- Write the code for global selection           ;;;
      ;;;                                                  ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (
        (= selGlob 1)
        (progn  

          ;;;--- If the entityType list contains more than one item...
          (if(> (length entityTypes) 1)
            (progn
              (princ          "\n" fil)
              (princ          "\n  ;;;--- Get a global selection set" fil)
              (princ          "\n  (setq eset" fil)
              (princ (strcat  "\n    (ssget" (chr 34) "X" (chr 34) " ") fil)                                  
              (princ          "\n      (list " fil)
              (princ (strcat  "\n        (cons -4 " (chr 34) "<OR" (chr 34) ")" ) fil)
              (foreach e entityTypes
                (princ(strcat "\n          (cons 0 " (chr 34) e (chr 34) ")" ) fil))
              (princ (strcat  "\n        (cons -4 " (chr 34) "OR>" (chr 34) ")" ) fil)
              (princ          "\n      )" fil)  ;'
              (princ          "\n    )" fil)     ;ssget
              (princ          "\n  )" fil)       ;setq
            )
                                     
            ;;;--- Else, the entityType list contains one item...
            (progn
              (princ         "\n" fil)
              (princ         "\n  ;;;--- Get a global selection set" fil)
              (princ (strcat "\n  (setq eset(ssget " (chr 34) "X" (chr 34) " " "(list(cons 0 " (chr 34) (car entityTypes)(chr 34) "))))" ) fil)
            )
          )
        ) 
      )
    )
  )









  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                    W R I T E   P R O M P T S                     ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;                                              
  ;;;   Parameters:                                
  ;;;                                              
  ;;;        spcr - Alignment string  (Ex. "\n   " 
  ;;;                                              
  
  (defun writePrompts(spcr)
    
    (foreach a actionList
      (setq actions(parseAction a))
      (setq actionType(car actions))
      
      (if(and (member "All" actions) writePromptAllFlag (member "PROMPT" actions))
        (progn
          (princ "\n " fil)
          (princ (strcat spcr ";;;---Prompt the user for information") fil)
          (setq writePromptAllFlag  nil)
        )
      )            
      
      (cond
         
        ;;; Properties
        
        (
          (= actionType "ANGLE")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat spcr "(setq ang(getangle " (chr 34) "\\n Angle: " (chr 34) "))") fil)
          )
        )
        (
          (= actionType "COLOR")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat  spcr "(setq clr(acad_colordlg 1 nil))") fil)
          )
        )
        (
          (= actionType "ELEVATION")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat spcr "(setq elv(getdist " (chr 34) "\\n Elevation: " (chr 34) "))") fil)
          )
        )
        (
          (= actionType "HEIGHT")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat spcr "(setq hgt(getdist " (chr 34) "\\n Height: " (chr 34) "))") fil)
          )
        )
        (
          (= actionType "LAYER")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat spcr "(setq lay(getstring " (chr 34) "\\n Layer: " (chr 34) "))") fil)
          )
        )
        (
          (= actionType "LINETYPE")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat spcr "(setq lty(getstring " (chr 34) "\\n Linetype: " (chr 34) "))") fil)
          )
        )
        (
          (= actionType "RADIUS")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat spcr "(setq rad(getdist " (chr 34) "\\n Radius: " (chr 34) "))") fil)
          )
        )
        (
          (= actionType "STYLE")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat spcr "(setq sty(getstring " (chr 34) "\\n Style: " (chr 34) "))") fil)
          )
        )  
        (
          (= actionType "VAL_REPLACE")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat spcr "(setq valNew(getstring T " (chr 34) "\\n Replace Text Value With: " (chr 34) "))") fil)
          )
        )
        (
          (= actionType "VAL_FIRST")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (progn
              (princ (strcat spcr "(setq valFirstOld(getstring T " (chr 34) "\\n Replace First Occurence Of: " (chr 34) "))") fil)
              (princ (strcat spcr "(setq valFirstNew(getstring T " (chr 34) "\\n Replace First Occurence With: " (chr 34) "))") fil)
            )  
          )
        )
        (
          (= actionType "VAL_LAST")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (progn
              (princ (strcat spcr "(setq valLastOld(getstring T " (chr 34) "\\n Replace Last Occurence Of: " (chr 34) "))") fil)
              (princ (strcat spcr "(setq valLastNew(getstring T " (chr 34) "\\n Replace Last Occurence With: " (chr 34) "))") fil)
            )  
          )
        )
        (
          (= actionType "VAL_EVERY")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (progn
              (princ (strcat spcr "(setq valEveryOld(getstring T " (chr 34) "\\n Replace Every Occurence Of: " (chr 34) "))") fil)
              (princ (strcat spcr "(setq valEveryNew(getstring T " (chr 34) "\\n Replace Every Ocurrence With: " (chr 34) "))") fil)
            )  
          )
        )        
        (
          (= actionType "VAL_INC_PREFX")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat spcr "(setq valPrefxInc(getstring " (chr 34) "\\n Increment Prefix With: " (chr 34) "))") fil)
          )
        )
        (
          (= actionType "VAL_INC_SUFFX")
          (if (and (= "PROMPT" (cadr actions))(member "All" actions))
            (princ (strcat spcr "(setq valSuffxInc(getstring " (chr 34) "\\n Increment Suffix With: " (chr 34) "))") fil)
          )
        )
                                
        ;;;--- Commands
                                
        (
          (= actionType "ARRAY")
          (progn
            (if (= "RECT" (cadr actions))
              (progn
                (if(and (= "PROMPT" (caddr actions))(member "All" actions))
                  (progn
                    (princ(strcat spcr "(setq xQty(getint " (chr 34) "\\n Number of Columns [X Axis]: " (chr 34) "))") fil)
                    (princ(strcat spcr "(setq yQty(getint " (chr 34) "\\n Number of Columns [Y Axis]: " (chr 34) "))") fil)
                    (princ(strcat spcr "(setq xDis(getdist " (chr 34) "\\n X Spacing: " (chr 34) "))") fil)
                    (princ(strcat spcr "(setq yDis(getdist " (chr 34) "\\n Y Spacing: " (chr 34) "))") fil)
                  )
                )
              )
            )
            (if (= "POLAR" (cadr actions))
              (progn
                (if(and (= "PROMPT" (caddr actions))(member "All" actions))
                  (progn
                    (princ (strcat spcr "(setq cenPt(getpoint " (chr 34) "\\n Center Point: " (chr 34) "))" ) fil)
                    (princ (strcat spcr "(setq cenPtX(car cenPt))") fil)
                    (princ (strcat spcr "(setq cenPtY(cadr cenPt))") fil)
                    (princ (strcat spcr "(setq numIt(itoa(getint " (chr 34) "\\n Number of items: " (chr 34) ")))" ) fil)
                    (princ (strcat spcr "(initget 1 " (chr 34) "Yes No" (chr 34) ")") fil)
                    (princ (strcat spcr "(setq polarans(getkword " (chr 34) "\\n Rotate items as they are copied?  Y/N :" (chr 34) "))" ) fil)
                  )
                )
              )
            )
          )
        )
        (
          (= actionType "COPY")
          (progn
            (if (and (= "PROMPT" (cadr actions))(member "All" actions))
              (progn
                (princ (strcat spcr "(setq cpyf(getpoint " (chr 34) "\\n Base point: " (chr 34) "))") fil)
                (princ (strcat spcr "(setq cpyt(getpoint cpyf " (chr 34) "\\n Displacement point: " (chr 34) "))") fil)                                    
              )
            )
          )
        )
        (
          (= actionType "MIRROR")
          (progn
            (if (and (= "PROMPT" (cadr actions))(member "All" actions))
              (progn
                (princ (strcat spcr "(setq mirf(getpoint " (chr 34) "\\n Specify first point of mirror line: " (chr 34) "))") fil)
                (princ (strcat spcr "(setq mirt(getpoint mirf " (chr 34) "\\n Specify second point of mirror line: " (chr 34) "))") fil)
              )
            )
          )
        )
        (
          (= actionType "MOVE")
          (progn
            (if (and (= "PROMPT" (cadr actions))(member "All" actions))
              (progn
                (princ (strcat spcr "(setq movf(getpoint " (chr 34) "\\n Base point: " (chr 34) "))") fil)
                (princ (strcat spcr "(setq movt(getpoint movf " (chr 34) "\\n Displacement point: " (chr 34) "))") fil)                                    
              )
            )
          )
        )
        (
          (= actionType "OFFSET")
          (progn
            (if (and (= "PROMPT" (cadr actions))(member "All" actions))
              (progn
                (princ (strcat spcr "(setq offsetAng(getangle " (chr 34) "\\n Offset Angle: " (chr 34) "))") fil)                                    
                (princ (strcat spcr "(setq offsetDis(getdist " (chr 34) "\\n Offset Distance: " (chr 34) "))") fil)
                (princ (strcat spcr "(setq offsetNum(getint " (chr 34) "\\n Number of Offsets: " (chr 34) "))") fil)                                    
              )
            )
          )
        )
        (
          (= actionType "ROTATE")
          (progn
            (if (and (= "PROMPT" (cadr actions))(member "All" actions))
              (progn
                (princ (strcat spcr "(setq rotf(getpoint " (chr 34) "\\n Base point: " (chr 34) "))") fil)
                (princ (strcat spcr "(setq rota(angtos(getangle rotf " (chr 34) "\\n Angle: " (chr 34) ")0 6))") fil)                                    
              )
            )
          )
        )
        (
          (= actionType "SCALE")
          (progn
            (if (and (= "PROMPT" (cadr actions))(member "All" actions))
              (progn
                (princ (strcat spcr "(setq scaf(getpoint " (chr 34) "\\n Base point: " (chr 34) "))") fil)
                (princ (strcat spcr "(setq scfc(getreal " (chr 34) "\\n Scale Factor: " (chr 34) "))") fil)                                    
              )
            )
          )
        )
      )
    )
    (princ "\n" fil)
  )




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                W R I T E   A C T I O N    C O D E                ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (defun writeActionCode()

    (princ "\n  (if (and eset (> (sslength eset) 0))" fil)
    (princ "\n    (progn" fil)
    
    (foreach a actionList                              
      (setq actions(parseAction a))
      (setq actionType(car actions))
      (cond
        (
          (= actionType "ANGLE")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;          CHANGE ANGLE FOR EACH ENTITY         ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter and save the dimscale" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar "(chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Prompt for the new angle" fil)
                (princ (strcat "\n        (if(setq ang(getangle " (chr 34) "\\n Angle: " (chr 34) "))") fil)
                (princ         "\n          (progn" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Substitute the angle dxf group code" fil)
                (princ         "\n            (setq enlist(subst(cons 50 ang)(assoc 50 enlist)enlist))" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the autocad database" fil)
                (princ         "\n            (entmod enlist)" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n            (entupd en)" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n      )" fil)
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;          CHANGE ANGLE FOR ALL ENTITIES        ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ         "\n      (setq cntr 0)" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Set the rotation angle" fil)
                    (princ(strcat "\n        (setq ang " (angtos(angtof(cadr actions))3 6) ")") fil)
                  )
                )  
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Substitute the angle dxf group code" fil)
                (princ         "\n        (setq enlist(subst(cons 50 ang)(assoc 50 enlist)enlist))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the autocad database" fil)
                (princ         "\n        (entmod enlist)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n        (entupd en)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )"  fil)
              ) 
            )
          )
        )
        (
          (= actionType "COLOR")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;          CHANGE COLOR FOR EACH ENTITY         ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ (strcat "\n      (setq cntr 0 lt(getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)                
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Prompt for the new color" fil)
                (princ         "\n        (if(setq clr(acad_colordlg 1 nil))" fil)
                (princ         "\n          " fil)
                (princ         "\n          ;;;--- Change the color" fil)                
                (princ (strcat "\n          (command " (chr 34) "change" (chr 34) " en " (chr 34)(chr 34) " " (chr 34) "Properties" (chr 34)" "(chr 34) "Color" (chr 34) " clr " (chr 34) (chr 34)")") fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;          CHANGE COLOR FOR ALL ENTITIES        ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n      " fil)
                    (princ        "\n      ;;;--- Set the color" fil)
                    (princ(strcat "\n      (setq clr " (cadr actions) ")") fil)
                  )
                )  
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Change the color" fil)
                (princ (strcat    "\n      (command " (chr 34) "change" (chr 34) " eset " (chr 34)(chr 34)" "(chr 34) "Properties" (chr 34)" "(chr 34) "Color" (chr 34) " clr " (chr 34) (chr 34) ")") fil)
              ) 
            )
          )
        )
        (
          (= actionType "ELEVATION")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;        CHANGE ELEVATION FOR EACH ENTITY       ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter and save the dimscale" fil)
                (princ (strcat "\n      (setq cntr 0 lt(getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name" fil)
                (princ         "\n        (setq en(ssname eset cntr)) " fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en)) " fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ (strcat "\n        (if(setq elv(getdist " (chr 34) "\\n Elevation: " (chr 34) "))") fil)
                (princ         "\n          (progn" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Move all entities to an extreme z coordinate to fool autocad" fil)
                (princ (strcat "\n            (command " (chr 34) "move" (chr 34) " en " (chr 34)(chr 34) " (list 0 0 0)(list 0 0 1e99))" ) fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Move all entities from an extreme z coord to a zero z coord" fil)
                (princ         "\n            ;;;    This trick 'always' resets all elevations to zero " fil)                
                (princ (strcat "\n            (command " (chr 34) "move" (chr 34) " en " (chr 34)(chr 34) " (list 0 0 1e99)(list 0 0 0))" ) fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Now, move the entities to the desired elevation" fil)
                (princ (strcat "\n            (command " (chr 34) "move" (chr 34) " en " (chr 34)(chr 34) " ") fil)
                (princ         "\n              (list 0.0 0.0 0.0) " fil)
                (princ         "\n              (list 0.0 0.0 elv)" fil)
                (princ         "\n            )" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)
              )
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;       CHANGE ELEVATION FOR ALL ENTITIES       ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n      " fil)
                    (princ        "\n      ;;;--- Set the elevation" fil)
                    (princ(strcat "\n      (setq elv " (cadr actions) ")") fil)
                  )
                )
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Move all entities to an extreme z coordinate to fool autocad" fil)
                (princ (strcat "\n      (command " (chr 34) "move" (chr 34) " eset " (chr 34)(chr 34) " (list 0 0 0)(list 0 0 1e99))" ) fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Move all entities from an extreme z coord to a zero z coord" fil)
                (princ         "\n      ;;;    This trick 'always' resets all elevations to zero " fil)
                (princ (strcat "\n      (command " (chr 34) "move" (chr 34) " eset " (chr 34)(chr 34) " (list 0 0 1e99)(list 0 0 0))" ) fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Now, move the entities to the desired elevation" fil)
                (princ (strcat "\n      (command " (chr 34) "move" (chr 34) " eset " (chr 34)(chr 34) " ") fil)
                (princ         "\n        (list 0.0 0.0 0.0) " fil)
                (princ         "\n        (list 0.0 0.0 elv)" fil)
                (princ         "\n      )" fil)
              )  
            )
          )
        )  
        (
          (= actionType "HEIGHT")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;          CHANGE HEIGHT FOR EACH ENTITY        ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)                
                (princ         "\n        " fil)                
                (princ         "\n        ;;;--- Prompt for the new height" fil)
                (princ (strcat "\n        (if(setq hgt(getdist " (chr 34) "\\n New Height: " (chr 34) "))") fil)
                (princ         "\n          (progn" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Substitute the height dxf group code" fil)
                (princ         "\n            (setq enlist(subst(cons 40 hgt)(assoc 40 enlist)enlist))" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the autocad database" fil)
                (princ         "\n            (entmod enlist)" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n            (entupd en)" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;         CHANGE HEIGHT FOR ALL ENTITIES        ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ         "\n      (setq cntr 0)" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Set the text height" fil)
                    (princ(strcat "\n        (setq hgt " (rtos(distof(cadr actions))2 6) ")") fil)
                  )
                )  
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Substitute the height dxf group code" fil)
                (princ         "\n        (setq enlist(subst(cons 40 hgt)(assoc 40 enlist)enlist))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the autocad database" fil)
                (princ         "\n        (entmod enlist)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n        (entupd en)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)
              ) 
            )
          )
        )
        (
          (= actionType "LAYER")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;          CHANGE LAYER FOR EACH ENTITY         ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)                
                (princ         "\n        ;;;--- Prompt for the new layer" fil)
                (princ (strcat "\n        (if(/= " (chr 34)(chr 34) " (setq lay(getstring " (chr 34) "\\n New Layer: " (chr 34) ")))") fil)
                (princ         "\n        " fil)
                (princ         "\n          ;;;--- Change the layer" fil)
                (princ (strcat "\n          (command " (chr 34) "change" (chr 34) " en " (chr 34)(chr 34) " " (chr 34) "Properties" (chr 34)" "(chr 34) "LAyer" (chr 34) " lay " (chr 34)(chr 34)")") fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;          CHANGE LAYER FOR ALL ENTITIES        ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      " fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n      " fil)
                    (princ        "\n      ;;;--- Set the layer" fil)
                    (princ(strcat "\n      (setq lay " (chr 34) (cadr actions) (chr 34) ")") fil)
                  )
                )  
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Change the layer" fil)
                (princ (strcat    "\n      (command " (chr 34) "change" (chr 34) " eset " (chr 34)(chr 34)" "(chr 34) "Properties" (chr 34)" "(chr 34) "LAyer" (chr 34) " lay " (chr 34) (chr 34) ")") fil)
              ) 
            )
          )
        )
        (
          (= actionType "LINETYPE")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;        CHANGE LINETYPE FOR EACH ENTITY        ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)                
                (princ         "\n        " fil)                
                (princ         "\n        ;;;--- Prompt for the new linetype" fil)
                (princ (strcat "\n        (if(/= " (chr 34)(chr 34)" (setq lty(getstring " (chr 34) "\\n New Linetype: " (chr 34) ")))") fil)
                (princ         "\n          " fil)
                (princ         "\n          ;;;--- Change the linetype" fil)
                (princ (strcat "\n          (command " (chr 34) "change" (chr 34) " en " (chr 34)(chr 34) " " (chr 34) "Properties" (chr 34)" "(chr 34) "LType" (chr 34) " lty " (chr 34)(chr 34)")") fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;       CHANGE LINETYPE FOR ALL ENTITIES        ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n      " fil)
                    (princ        "\n      ;;;--- Set the linetype" fil)
                    (princ(strcat "\n      (setq lty " (chr 34) (cadr actions) (chr 34) ")") fil)
                  )
                )  
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Change the linetype" fil)
                (princ (strcat    "\n      (command " (chr 34) "change" (chr 34) " eset " (chr 34)(chr 34) (chr 34) "Properties" (chr 34)" "(chr 34) "LType" (chr 34) " lty "(chr 34) (chr 34)")") fil)
              ) 
            )
          )
        )
        (
          (= actionType "RADIUS")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;          CHANGE RADIUS FOR EACH ENTITY        ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)                
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)                                
                (princ         "\n        ;;;--- Prompt for the new radius" fil)
                (princ (strcat "\n        (if(setq rad(getdist " (chr 34) "\\n New Radius: " (chr 34) "))") fil)
                (princ         "\n          (progn" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Substitute the radius dxf group code" fil)
                (princ         "\n            (setq enlist(subst(cons 40 rad)(assoc 40 enlist)enlist))" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the autocad database" fil)
                (princ         "\n            (entmod enlist)" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n            (entupd en)" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;        CHANGE RADIUS FOR ALL ENTITIES         ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ         "\n      (setq cntr 0)" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Set the radius" fil)
                    (princ(strcat "\n        (setq rad " (rtos(distof(cadr actions))2 6) ")") fil)
                  )
                )  
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Substitute the radius dxf group code" fil)
                (princ         "\n        (setq enlist(subst(cons 40 rad)(assoc 40 enlist)enlist))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the autocad database" fil)
                (princ         "\n        (entmod enlist)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n        (entupd en)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)

              )
            )  
          )
        )
        (
          (= actionType "STYLE")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;         CHANGE STYLE FOR EACH ENTITY          ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Prompt for the new style" fil)
                (princ (strcat "\n        (if(/= " (chr 34)(chr 34)" (setq sty(getstring " (chr 34) "\\n New Style: " (chr 34) ")))") fil)
                (princ         "\n          (progn " fil)
                (princ         "\n            " fil)                
                (princ         "\n            ;;;--- Substitute the style dxf group code" fil)
                (princ         "\n            (setq enlist(subst(cons 7 sty)(assoc 7 enlist)enlist))" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the autocad database" fil)
                (princ         "\n            (entmod enlist)" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n            (entupd en)" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)                
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;          CHANGE STYLE FOR ALL ENTITIES        ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ         "\n      (setq cntr 0)" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Set the style" fil)
                    (princ(strcat "\n        (setq sty " (chr 34)(cadr actions)(chr 34) ")") fil)
                  )
                )  
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Substitute the style dxf group code" fil)
                (princ         "\n        (setq enlist(subst(cons 40 sty)(assoc 40 enlist)enlist))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the autocad database" fil)
                (princ         "\n        (entmod enlist)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n        (entupd en)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)
              )
            )  
          )
        )      
        (
          (= actionType "VAL_REPLACE")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;         REPLACE VALUE FOR EACH ENTITY         ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Prompt for the new value" fil)
                (princ (strcat "\n        (if(/= "(chr 34) (chr 34)" (setq valNew(getstring T " (chr 34) "\\n New Value: " (chr 34) ")))") fil)
                (princ         "\n          (progn" fil)
                (princ         "\n            " fil)                
                (princ         "\n            ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n            (setq enlist(subst(cons 1 valNew)(assoc 1 enlist)enlist))" fil)
                (princ         "\n             " fil)
                (princ         "\n            ;;;--- Update the autocad database" fil)
                (princ         "\n            (entmod enlist)" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n            (entupd en)" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)                
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;         REPLACE VALUE FOR ALL ENTITIES        ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ         "\n      (setq cntr 0)" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Set the value" fil)
                    (princ(strcat "\n        (setq valNew " (chr 34)(cadr actions)(chr 34) ")") fil)
                  )
                )  
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n        (setq enlist(subst(cons 1 valNew)(assoc 1 enlist)enlist))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the autocad database" fil)
                (princ         "\n        (entmod enlist)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n        (entupd en)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)
              )
            )  
          )
        )
        (
          (= actionType "VAL_FIRST")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;; REPLACE FIRST OCCURENCE OF VALUE FOR EACH ENTITY ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Display the old value" fil)
                (princ (strcat "\n        (princ " (chr 34) "\\n Old Value = " (chr 34) " (cdr(assoc 1 enlist)))")fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Prompt for the old value" fil)
                (princ (strcat "\n        (if(/= "(chr 34) (chr 34)" (setq valFirstOld(getstring T " (chr 34) "\\n Replace First Occurence Of: " (chr 34) ")))") fil)
                (princ         "\n          (progn" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Prompt for the new value" fil)
                (princ (strcat "\n            (setq valFirstNew(getstring T " (chr 34) "\\n Replace First Occurence With: " (chr 34) "))") fil)
                (princ         "\n            " fil)      
                (princ         "\n            ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n            (setq enlist(replaceFirstOccurrence enlist valFirstOld valFirstNew))" fil)
                (princ         "\n             " fil)
                (princ         "\n            ;;;--- Update the autocad database" fil)
                (princ         "\n            (entmod enlist)" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n            (entupd en)" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)                
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;; REPLACE FIRST OCCURENCE OF VALUE FOR ALL ENTITIES ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ         "\n      (setq cntr 0)" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Get the old value" fil)
                    (princ(strcat "\n        (setq valFirstOld " (chr 34)(cadr actions)(chr 34) ")") fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Get the new value" fil)
                    (princ(strcat "\n        (setq valFirstNew " (chr 34)(caddr actions)(chr 34) ")") fil)                    
                  )
                )  
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n        (setq enlist(replaceFirstOccurrence enlist valFirstOld valFirstNew))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the autocad database" fil)
                (princ         "\n        (entmod enlist)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n        (entupd en)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)
              )
            )  
          )
        )
        (
          (= actionType "VAL_LAST")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;; REPLACE LAST OCCURENCE OF VALUE FOR EACH ENTITY  ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Display the old value" fil)
                (princ (strcat "\n        (princ " (chr 34) "\\n Old Value = " (chr 34) " (cdr(assoc 1 enlist)))")fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Prompt for the old value" fil)
                (princ (strcat "\n        (if(/= "(chr 34) (chr 34)" (setq valLastOld(getstring T " (chr 34) "\\n Replace Last Occurence Of: " (chr 34) ")))") fil)
                (princ         "\n          (progn" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Prompt for the new value" fil)
                (princ (strcat "\n            (setq valLastNew(getstring T " (chr 34) "\\n Replace Last Occurence With: " (chr 34) "))") fil)
                (princ         "\n            " fil)      
                (princ         "\n            ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n            (setq enlist(replaceLastOccurrence enlist valLastOld valLastNew))" fil)                
                (princ         "\n            " fil)                
                (princ         "\n            ;;;--- Update the autocad database" fil)
                (princ         "\n            (entmod enlist)" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n            (entupd en)" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)                
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;  REPLACE LAST OCCURENCE OF VALUE FOR ALL ENTITIES ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ         "\n      (setq cntr 0)" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Get the old value" fil)
                    (princ(strcat "\n        (setq valLastOld " (chr 34)(cadr actions)(chr 34) ")") fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Get the new value" fil)
                    (princ(strcat "\n        (setq valLastNew " (chr 34)(caddr actions)(chr 34) ")") fil)                    
                  )
                )  
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n        (setq enlist(replaceLastOccurrence enlist valLastOld valLastNew))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the autocad database" fil)
                (princ         "\n        (entmod enlist)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n        (entupd en)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)
              )
            )  
          )
        )
        (
          (= actionType "VAL_EVERY")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;; REPLACE EVERY OCCURRENCE OF VALUE FOR EACH ENTITY ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Display the old value" fil)
                (princ (strcat "\n        (princ " (chr 34) "\\n Old Value = " (chr 34) " (cdr(assoc 1 enlist)))")fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Prompt for the old value" fil)
                (princ (strcat "\n        (if(/= "(chr 34) (chr 34)" (setq valEveryOld(getstring T " (chr 34) "\\n Replace Every Occurence Of: " (chr 34) ")))") fil)
                (princ         "\n          (progn" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Prompt for the new value" fil)
                (princ (strcat "\n            (setq valEveryNew(getstring T " (chr 34) "\\n Replace Every Occurence With: " (chr 34) "))") fil)
                (princ         "\n            " fil)      
                (princ         "\n            ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n            (setq enlist(replaceEveryOccurrence enlist valEveryOld valEveryNew))" fil)                
                (princ         "\n            " fil)                                
                (princ         "\n            ;;;--- Update the autocad database" fil)
                (princ         "\n            (entmod enlist)" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n            (entupd en)" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)                
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;; REPLACE EVERY OCCURENCE OF VALUE FOR ALL ENTITIES ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ         "\n      (setq cntr 0)" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Get the old value" fil)
                    (princ(strcat "\n        (setq valEveryOld " (chr 34)(cadr actions)(chr 34) ")") fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Get the new value" fil)
                    (princ(strcat "\n        (setq valEveryNew " (chr 34)(caddr actions)(chr 34) ")") fil)                    
                  )
                )  
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n        (setq enlist(replaceEveryOccurrence enlist valEveryOld valEveryNew))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the autocad database" fil)
                (princ         "\n        (entmod enlist)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n        (entupd en)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)
              )
            )  
          )
        )
        (
          (= actionType "VAL_INC_PREFX")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;   INCREMENT THE PREFIX OF VALUE FOR EACH ENTITY  ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Prompt for the increment" fil)
                (princ (strcat "\n        (if(/= "(chr 34) (chr 34)" (setq valPrefxInc(getstring T " (chr 34) "\\n Prefix Increment: " (chr 34) ")))") fil)
                (princ         "\n          (progn" fil)
                (princ         "\n            " fil)                
                (princ         "\n            ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n            (setq enlist(prefixValue enlist valPrefxInc))" fil)
                (princ         "\n             " fil)
                (princ         "\n            ;;;--- Update the autocad database" fil)
                (princ         "\n            (entmod enlist)" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n            (entupd en)" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)                
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;  INCREMENT THE PREFIX OF VALUE FOR ALL ENTITIES  ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ         "\n      (setq cntr 0)" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Set the value" fil)
                    (princ(strcat "\n        (setq valPrefxInc " (chr 34)(cadr actions)(chr 34) ")") fil)
                  )
                )  
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n        (setq enlist(prefixValue enlist valPrefxInc))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the autocad database" fil)
                (princ         "\n        (entmod enlist)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n        (entupd en)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)
              )
            )  
          )
        )
        (
          (= actionType "VAL_INC_SUFFX")
          (progn
            (if(member "Each" actions)
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;   INCREMENT THE SUFFIX OF VALUE FOR EACH ENTITY  ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                (princ (strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ         "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Prompt for the increment" fil)
                (princ (strcat "\n        (if(/= "(chr 34) (chr 34)" (setq valSuffxInc(getstring T " (chr 34) "\\n Suffix Increment: " (chr 34) ")))") fil)
                (princ         "\n          (progn" fil)
                (princ         "\n            " fil)                
                (princ         "\n            ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n            (setq enlist(suffixValue enlist valSuffxInc))" fil)
                (princ         "\n             " fil)
                (princ         "\n            ;;;--- Update the autocad database" fil)
                (princ         "\n            (entmod enlist)" fil)
                (princ         "\n            " fil)
                (princ         "\n            ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n            (entupd en)" fil)
                (princ         "\n          )" fil)
                (princ         "\n        )" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ         "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)                
                (princ         "\n      )" fil)                
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ         "\n      " fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      ;;;  INCREMENT THE SUFFIX OF VALUE FOR ALL ENTITIES  ;;;" fil)
                (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Set up a counter" fil)
                (princ         "\n      (setq cntr 0)" fil)
                (princ         "\n      " fil)
                (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)                
                (princ         "\n      (while(< cntr (sslength eset))" fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Set the value" fil)
                    (princ(strcat "\n        (setq valSuffxInc " (chr 34)(cadr actions)(chr 34) ")") fil)
                  )
                )  
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                (princ         "\n        (setq en(ssname eset cntr))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ         "\n        (setq enlist(entget en))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Substitute the value dxf group code" fil)
                (princ         "\n        (setq enlist(suffixValue enlist valSuffxInc))" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the autocad database" fil)
                (princ         "\n        (entmod enlist)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Update the entity on the graphics screen" fil)
                (princ         "\n        (entupd en)" fil)
                (princ         "\n        " fil)
                (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ         "\n        (setq cntr(+ cntr 1))" fil)
                (princ         "\n      )" fil)
              )
            )  
          )
        )
        (
          (= actionType "ADD")
          (progn
            (cond
              (
                (and (= (cadr actions) "VALUE")(= addCntr nil))
                (progn
                  (setq addCntr T)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                  (princ         "\n      ;;;                   ADD - VALUE                 ;;;" fil)
                  (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                  (princ         "\n      " fil)                  
                  (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                  (princ         "\n      (setq cntr 0)" fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                  (princ         "\n      (while(< cntr (sslength eset))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                  (princ         "\n        (setq en(ssname eset cntr))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Send the entity to the GETVAL function" fil)
                  (princ (strcat "\n        (setq val(getVal en " (last actions) "))") fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- If a value was found, add it to the running total" fil)
                  (princ         "\n        (if val(setq addValTotal(+ addValTotal val)))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                  (princ         "\n        (setq cntr(+ cntr 1))" fil)
                  (princ         "\n      )" fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Build the display string" fil)
                  (princ (strcat "\n      (setq displayString(strcat " (chr 34) "Total = " (chr 34) "(rtos addValTotal 2 2)))") fil)
                )
              )
              (
                (and (= (cadr actions) "AREA")(= areaCntr nil))
                (progn
                  (setq areaCntr T)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                  (princ         "\n      ;;;                   ADD - AREA                  ;;;" fil)
                  (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                  
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                  (princ         "\n      (setq cntr 0)" fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                  (princ         "\n      (while(< cntr (sslength eset))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                  (princ         "\n        (setq en(ssname eset cntr))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Send the entity to the GETAREA function" fil)
                  (princ (strcat "\n        (setq val(getArea en))") fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- If an area was found, add it to the running total" fil)
                  (princ         "\n        (if val(setq addAreTotal(+ addAreTotal val)))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                  (princ         "\n        (setq cntr(+ cntr 1))" fil)
                  (princ         "\n      )" fil)
                  (setq unitType (nth 2 actions))
                  (setq unitdType(nth 3 actions))
                  (princ         "\n      " fil)
                  (cond
                    ((= unitType "FEET")      (princ "\n      ;;;--- Convert Feet to Sq. Inches        \n      (setq addAreTotal(* addAreTotal 144.0))" fil))
                    ((= unitType "MILLIMETER")(princ "\n      ;;;--- Convert Millimeters to Sq. Inches \n      (setq addAreTotal(* addAreTotal 0.00155))" fil))
                    ((= unitType "CENTIMETER")(princ "\n      ;;;--- Convert Centimeters to Sq. Inches \n      (setq addAreTotal(* addAreTotal 0.155))" fil))
                    ((= unitType "DECIMETER") (princ "\n      ;;;--- Convert Decimeters to Sq. Inches  \n      (setq addAreTotal(/ addAreTotal 15.5))" fil))
                    ((= unitType "METER")     (princ "\n      ;;;--- Convert Meters to Sq. Inches      \n      (setq addAreTotal(/ addAreTotal 1550.0031))" fil))
                    ((= unitType "MILE")      (princ "\n      ;;;--- Convert Miles to Sq. Inches       \n      (setq addAreTotal(* addAreTotal 4014489600.0))" fil))
                  )
                  (princ         "\n      " fil)
                  (cond
                    ((= unitdType "FEET")      (princ "\n      ;;;--- Convert to square Feet           \n      (setq addAreTotal(/ addAreTotal 144.0))" fil))
                    ((= unitdType "MILLIMETER")(princ "\n      ;;;--- Convert to square Millimeters    \n      (setq addAreTotal(* addAreTotal 645.16))" fil))
                    ((= unitdType "CENTIMETER")(princ "\n      ;;;--- Convert to square Centimeters    \n      (setq addAreTotal(* addAreTotal 6.4516))" fil))
                    ((= unitdType "DECIMETER") (princ "\n      ;;;--- Convert to square Decimeters     \n      (setq addAreTotal(/ addAreTotal 0.254))" fil))
                    ((= unitdType "METER")     (princ "\n      ;;;--- Convert to square Meters         \n      (setq addAreTotal(* addAreTotal 0.00064516))" fil))
                    ((= unitdType "ACRE")      (princ "\n      ;;;--- Convert to Acres                 \n      (setq addAreTotal(/ addAreTotal 155001.0242))" fil))
                    ((= unitdType "MILE")      (princ "\n      ;;;--- Convert to square Miles          \n      (setq addAreTotal(/ addAreTotal 4014489600.0))" fil))
                  )
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Build the display string" fil)
                  (princ (strcat "\n      (setq displayString (strcat (rtos addAreTotal 2 2) " (chr 34) " SQUARE "  unitdType "S"(chr 34) "))") fil)
                )
              )
              (
                (and (= (cadr actions) "LENGTH")(= lenCntr nil))
                (progn
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                  (princ         "\n      ;;;                  ADD - LENGTH                 ;;;" fil)
                  (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                  (princ         "\n      (setq cntr 0)" fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                  (princ         "\n      (while(< cntr (sslength eset))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                  (princ         "\n        (setq en(ssname eset cntr))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Send the entity to the GETLEN function" fil)
                  (princ         "\n        (setq val(getLen en))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- If a length was found, add it to the running total" fil)
                  (princ         "\n        (if val(setq addLenTotal(+ addLenTotal val)))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                  (princ         "\n        (setq cntr(+ cntr 1))" fil)
                  (princ         "\n      )" fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Build the display string" fil)
                  (princ (strcat "\n      (setq displayString(strcat " (chr 34) "Total = " (chr 34) "(rtos addLenTotal 2 2)))") fil)
                  (setq lenCntr T)
                  
                )
              )
              (
                (and (= (cadr actions) "RADIUS")(= radCntr nil))
                (progn
                  (setq radCntr T)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                  (princ         "\n      ;;;                  ADD - RADIUS                 ;;;" fil)
                  (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                  
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                  (princ         "\n      (setq cntr 0)" fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                  (princ         "\n      (while(< cntr (sslength eset))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                  (princ         "\n        (setq en(ssname eset cntr))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Send the entity to the GETRAD function" fil)
                  (princ         "\n        (setq val(getRad en))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- If a radius was found, add it to the running total" fil)
                  (princ         "\n        (if val(setq addRadTotal(+ addRadTotal val)))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                  (princ         "\n        (setq cntr(+ cntr 1))" fil)
                  (princ         "\n      )" fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Build the display string" fil)
                  (princ (strcat "\n      (setq displayString(strcat " (chr 34) "Total = " (chr 34) "(rtos addRadTotal 2 2)))") fil)                  
                )
              )
              (
                (and (= (cadr actions) "DIAMETER")(= diaCntr nil))
                (progn
                  (setq diaCntr T)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                  (princ         "\n      ;;;                  ADD - DIAMETER               ;;;" fil)
                  (princ         "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                  
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Set up a counter to start at zero" fil)
                  (princ         "\n      (setq cntr 0)" fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Cycle through each entity in the selection set" fil)
                  (princ         "\n      (while(< cntr (sslength eset))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Get the nth entity name from the selection set" fil)
                  (princ         "\n        (setq en(ssname eset cntr))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Send the entity to the GETDIA function" fil)
                  (princ         "\n        (setq val(getDia en))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- If a diameter was found, add it to the running total" fil)
                  (princ         "\n        (if val(setq addDiaTotal(+ addDiaTotal val)))" fil)
                  (princ         "\n      " fil)
                  (princ         "\n        ;;;--- Increment the counter to get the next entity" fil)
                  (princ         "\n        (setq cntr(+ cntr 1))" fil)
                  (princ         "\n      )" fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Build the display string" fil)
                  (princ (strcat "\n      (setq displayString(strcat " (chr 34) "Total = " (chr 34) "(rtos addDiaTotal 2 2)))") fil)                  
                )
              )
            )
            (cond
              (
                 (= "COMMANDLINE" (nth 1 (reverse actions)))
                 (progn
                   (princ         "\n      " fil)
                   (princ         "\n      ;;;--- Display the results on the command line" fil)
                   (princ         "\n      (princ displayString)" fil)
                 )  
              ) 
              ( 
                (= "ALERTBOX" (nth 1 (reverse actions)))
                (progn
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Display the results in an alert box" fil)
                  (princ         "\n      (alert displayString)" fil)
                )  
              )  
              (
                (= "INSERTTXT" (nth 1 (reverse actions)))
                (progn
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Get the insertion point for the text" fil)               
                  (princ (strcat "\n      (setq pt(getpoint " (chr 34) "\\n Text Insertion Point: " (chr 34) "))" ) fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Print the results to the drawing screen" fil)
                  (princ (strcat "\n      (command " (chr 34) "mtext" (chr 34) " pt (polar pt (* pi 1.75) (* (getvar " (chr 34) "DIMSCALE" (chr 34) ") 12.0)) displayString " (chr 34)(chr 34) ")" ) fil)
                )
              )
            )
          )
        )
        (
          (= actionType "ARRAY")
          (progn
            (if(member "Each" actions)
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;         COMMAND ARRAY FOR EACH ENTITY         ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (if(= "RECT" (cadr actions))
                  (progn
                    (princ        "\n      ;;;--- Set up a counter" fil)
                    (princ(strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                    (princ        "\n      " fil)
                    (princ        "\n      ;;;--- Cycle through each entity in the selection set" fil)
                    (princ        "\n      (while(< cntr (sslength eset))" fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Get the entity name from the selection set" fil)
                    (princ        "\n        (setq en(ssname eset cntr))" fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Get the dxf group codes from the entity" fil)
                    (princ        "\n        (setq enlist(entget en))" fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                    (princ        "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)                    
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Prompt the user for rectangular array parameters" fil)
                    (princ        "\n        (if" fil)
                    (princ        "\n          (and" fil)
                    (princ(strcat "\n            (setq xQty(getint " (chr 34) "\\n Number of Columns [X Axis]: " (chr 34) "))") fil)
                    (princ(strcat "\n            (setq yQty(getint " (chr 34) "\\n Number of Columns [Y Axis]: " (chr 34) "))") fil)
                    (princ(strcat "\n            (setq xDis(getdist " (chr 34) "\\n X Spacing: " (chr 34) "))") fil)
                    (princ(strcat "\n            (setq yDis(getdist " (chr 34) "\\n Y Spacing: " (chr 34) "))") fil)
                    (princ        "\n          )" fil)
                    (princ        "\n          " fil)
                    (princ        "\n          ;;;--- Use the rectangular array command" fil)
                    (princ(strcat "\n          (command " (chr 34) "-ARRAY" (chr 34) " en " (chr 34)(chr 34)) fil)
                    (princ(strcat "\n            " (chr 34) "Rect" (chr 34) " xQty yQty xDis yDis" )fil)
                    (princ        "\n          )" fil)
                    (princ        "\n        )" fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Increment the counter to get the next entity" fil)
                    (princ        "\n        (setq cntr(+ cntr 1))" fil)
                    (princ        "\n      )" fil)
                  )
                  ;;;--- Else it is Polar
                  (progn
                    (princ        "\n      " fil)
                    (princ        "\n      ;;;--- Set up a counter" fil)
                    (princ(strcat "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                    (princ        "\n      " fil)
                    (princ        "\n      ;;;--- Cycle through each entity in the selection set" fil)
                    (princ        "\n      (while(< cntr (sslength eset))" fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Get the entity name from the selection set" fil)
                    (princ        "\n        (setq en(ssname eset cntr))" fil)                    
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Get the dxf group codes from the entity" fil)
                    (princ        "\n        (setq enlist(entget en))" fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                    (princ        "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)                    
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Prompt the user for polar array parameters" fil)
                    (princ        "\n        (if" fil)
                    (princ        "\n          (and" fil)
                    (princ(strcat "\n            (setq cenPt(getpoint " (chr 34) "\\n Center Point: " (chr 34) "))" ) fil)
                    (princ(strcat "\n            (setq numIt(itoa(getint " (chr 34) "\\n Number of items: " (chr 34) ")))" ) fil)
                    (princ        "\n            (progn" fil)
                    (princ(strcat "\n              (initget 1 " (chr 34) "Yes No" (chr 34) ")") fil)
                    (princ(strcat "\n              (setq polarans(getkword " (chr 34) "\\n Rotate items as they are copied?  Y/N :" (chr 34) "))" ) fil)
                    (princ        "\n            )" fil)
                    (princ        "\n          )" fil)
                    (princ        "\n          " fil)
                    (princ        "\n          ;;;--- Use the polar array command" fil)
                    (princ(strcat "\n          (command " (chr 34) "-ARRAY" (chr 34) " en " (chr 34)(chr 34)) fil)
                    (princ(strcat "\n            " (chr 34)"Polar"(chr 34) " cenPt numIt " (chr 34)(chr 34) " polarAns" )fil)
                    (princ        "\n          )" fil)
                    (princ        "\n        )" fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                    (princ        "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                    (princ        "\n        " fil)
                    (princ        "\n        ;;;--- Increment the counter to get the next entity" fil)
                    (princ        "\n        (setq cntr(+ cntr 1))" fil)
                    (princ        "\n      )" fil)                    
                  )
                )
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ               "\n      " fil)
                (princ               "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ               "\n      ;;;       COMMAND ARRAY - FOR ALL ENTITIES        ;;;" fil)
                (princ               "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ               "\n      " fil)
                (if(= "RECT" (cadr actions))
                  (progn
                    (if(not(member "PROMPT" actions))
                      (progn
                        (princ        "\n      ;;;--- Set the parameters" fil)
                        (princ(strcat "\n      (setq xQty " (nth 2 actions) ")") fil)
                        (princ(strcat "\n      (setq yQty " (nth 3 actions) ")") fil)
                        (princ(strcat "\n      (setq xDis " (rtos(distof(nth 4 actions))2 4) ")") fil)
                        (princ(strcat "\n      (setq yDis " (rtos(distof(nth 5 actions))2 4) ")") fil)
                      )
                    )
                    (princ            "\n      " fil)
                    (princ            "\n      ;;;--- Use the rectangular array command" fil)
                    (princ(strcat     "\n      (command " (chr 34) "-ARRAY" (chr 34) " eset " (chr 34)(chr 34)) fil)
                    (princ(strcat     "\n        " (chr 34) "Rect" (chr 34) " xQty yQty xDis yDis" )fil)
                    (princ            "\n      )" fil)                        
                  )
                )
                (if(= "POLAR" (cadr actions))
                  (progn
                    (if(not(member "PROMPT" actions))
                      (progn
                        (princ        "\n      ;;;--- Set the parameters" fil)
                        (princ(strcat "\n      (setq cenPtx " (nth 2 actions) ")") fil)
                        (princ(strcat "\n      (setq cenPty " (nth 3 actions) ")") fil)
                        (princ(strcat "\n      (setq numIt " (nth 4 actions) ")") fil)
                        (princ(strcat "\n      (setq polarans " (chr 34)(nth 5 actions)(chr 34) ")") fil)
                      )
                    )
                    (princ        "\n      " fil)
                    (princ        "\n      ;;;--- Use the polar array command" fil)
                    (princ(strcat "\n      (command " (chr 34) "-ARRAY" (chr 34) " eset " (chr 34)(chr 34)) fil)
                    (princ(strcat "\n        " (chr 34)"Polar"(chr 34) " (list cenPtx cenPty) numIt " (chr 34)(chr 34)" polarAns" )fil)
                    (princ        "\n      )" fil)
                  )
                )             
              )        
            )
          )
        )
        (
          (= actionType "COPY")
          (progn
            (if(member "Each" actions)
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;          COMMAND COPY FOR EACH ENTITY         ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Set up a counter" fil)
                (princ (strcat    "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ            "\n      (while(< cntr (sslength eset))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the entity name from the selection set" fil)
                (princ            "\n        (setq en(ssname eset cntr))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ            "\n        (setq enlist(entget en))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ            "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the point to copy from" fil)
                (if(= (cadr actions) "FromEntity")
                  (princ          "\n        (setq cpyf(cdr(assoc 10 enlist)))" fil)
                  (princ (strcat  "\n        (setq cpyf(getpoint " (chr 34) "\\n Base point: " (chr 34) "))") fil)
                )
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the point to copy to" fil)
                (princ (strcat    "\n        (setq cpyt(getpoint cpyf " (chr 34) "\\n Displacement point: " (chr 34) "))") fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- If both points are valid" fil)
                (princ            "\n        (if(and cpyf cpyt)" fil)
                (princ            "\n          " fil)
                (princ            "\n          ;;;--- Use the copy command" fil)
                (princ (strcat    "\n          (command " (chr 34) "COPY" (chr 34) " en " (chr 34)(chr 34)" cpyf cpyt)") fil)
                (princ            "\n        )" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ            "\n        (setq cntr(+ cntr 1))" fil)
                (princ            "\n      )" fil)                 
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;         COMMAND COPY - FOR ALL ENTITIES       ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (setq pta(parseStrIntoList(cadr actions)))
                    (princ(strcat "\n      (setq cpyf(list " (rtos (atof(car pta)) 2 4)" "(rtos(atof(cadr pta)) 2 4) " "(rtos(atof(caddr pta)) 2 4) "))") fil)
                    (setq pta(parseStrIntoList(caddr actions)))
                    (princ(strcat "\n      (setq cpyt(list " (rtos (atof(car pta)) 2 4)" "(rtos(atof(cadr pta)) 2 4) " "(rtos(atof(caddr pta)) 2 4) "))") fil)                    
                  )
                )
                (princ            "\n      ;;;--- Use the copy command" fil)
                (princ (strcat    "\n      (command " (chr 34) "COPY" (chr 34) " eset " (chr 34)(chr 34)" cpyf cpyt)") fil)
                (princ            "\n      " fil)
              )
            )
          )
        )
        (
          (= actionType "COUNT")
          (progn
            (princ "\n      ;;;--- Get the totals" fil)
            (if(= selGlob 1)
              (princ "\n      (setq totals(countEntities eset 1))" fil)
              (princ "\n      (setq totals(countEntities eset 0))" fil)
            )
            (cond
              (
                 (= "COMMANDLINE" (cadr actions))
                 (progn
                   (princ         "\n      " fil)
                   (princ         "\n      ;;;--- Display the results on the command line" fil)
                   (princ         "\n      (princ totals)" fil)
                 )  
              ) 
              ( 
                (= "ALERTBOX" (cadr actions))
                (progn
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Display the results in an alert box" fil)
                  (princ         "\n      (alert totals)" fil)
                )  
              )  
              (
                (= "INSERTTXT" (cadr actions))
                (progn
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Get the insertion point for the text" fil)               
                  (princ (strcat "\n      (setq pt(getpoint " (chr 34) "\\n Text Insertion Point: " (chr 34) "))" ) fil)
                  (princ         "\n      " fil)
                  (princ         "\n      ;;;--- Print the results to the drawing screen" fil)
                  (princ (strcat "\n      (command " (chr 34) "mtext" (chr 34) " pt (polar pt (* pi 1.75) (* (getvar " (chr 34) "DIMSCALE" (chr 34) ") 12.0)) totals " (chr 34)(chr 34) ")" ) fil)
                )
              )
            )
          )
        )
        (
          (= actionType "DELETE")
          (progn
            (if(member "Each" actions)
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;          COMMAND DELETE FOR EACH ENTITY       ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Set up a counter" fil)
                (princ            "\n      (setq cntr 0)" fil)
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ            "\n      (while(< cntr (sslength eset))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the entity name from the selection set" fil)
                (princ            "\n        (setq en(ssname eset cntr))" fil)
                (princ            "\n        " fil)
                (princ            "\n        (deleteEntity en)" fil) 
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ            "\n        (setq cntr(+ cntr 1))" fil)
                (princ            "\n      )" fil)                 
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;       COMMAND DELETE - FOR ALL ENTITIES       ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Count the entities to delete" fil)
                (princ            "\n      (setq cnt(sslength eset))" fil)
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Use the erase command" fil)
                (princ (strcat    "\n      (command " (chr 34) "ERASE" (chr 34) " eset " (chr 34)(chr 34)")") fil)
                (princ            "\n      " fil)
                (princ (strcat    "\n      (alert (strcat " (chr 34) "Erased " (chr 34)" (itoa cnt) "(chr 34)" Entities." (chr 34) "))" ) fil)
                (princ            "\n      " fil)
              )
            )
          )
        )
        (
          (= actionType "MIRROR")
          (progn
            (if(member "Each" actions)
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;        COMMAND MIRROR FOR EACH ENTITY         ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Set up a counter" fil)
                (princ (strcat    "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ            "\n      (while(< cntr (sslength eset))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the entity name from the selection set" fil)
                (princ            "\n        (setq en(ssname eset cntr))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ            "\n        (setq enlist(entget en))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ            "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n        ;;;--- Set the mirror line points" fil)
                    (setq pt(parseStrIntoList (cadr actions)))
                    (princ(strcat "\n        (setq mirf(list " (rtos(atof(car pt)) 2 4) " " (rtos(atof(cadr pt)) 2 4)" ))") fil)
                    (setq pt(parseStrIntoList (caddr actions)))
                    (princ(strcat "\n        (setq mirt(list " (rtos(atof(car pt)) 2 4) " " (rtos(atof(cadr pt)) 2 4)" ))") fil)                    
                  )
                  (progn
                    (princ        "\n        ;;;--- Prompt user for mirror command parameters" fil)
                    (princ(strcat "\n        (setq mirf(getpoint " (chr 34) "\\n Specify first point of mirror line: " (chr 34) "))") fil)
                    (princ(strcat "\n        (setq mirt(getpoint mirf " (chr 34) "\\n Specify second point of mirror line: " (chr 34) "))") fil)
                  )
                )
                (princ            "\n        (if (and mirf mirt)" fil)
                (princ            "\n          ;;;--- Use the mirror command" fil)
                (princ (strcat    "\n          (command " (chr 34) "MIRROR" (chr 34) " en " (chr 34)(chr 34)" mirf mirt " (chr 34) "No" (chr 34) ")") fil)
                (princ            "\n        )" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ            "\n        (setq cntr(+ cntr 1))" fil)
                (princ            "\n      )" fil)                 
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;        COMMAND MIRROR - FOR ALL ENTITIES      ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n      ;;;--- Set the mirror parameters" fil)
                    (princ(strcat "\n      (setq mirf " (cadr  actions) ")") fil)
                    (princ(strcat "\n      (setq mirt " (caddr actions) ")") fil)                               
                  )
                )
                (princ            "\n      ;;;--- Use the mirror command" fil)
                (princ (strcat    "\n      (command " (chr 34) "MIRROR" (chr 34) " eset " (chr 34)(chr 34)" mirf mirt " (chr 34) "No" (chr 34)")") fil)
              )
            )
          )
        )
        (
          (= actionType "MOVE")
          (progn
            (if(member "Each" actions)
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;          COMMAND MOVE FOR EACH ENTITY         ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Set up a counter" fil)
                (princ (strcat    "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ            "\n      (while(< cntr (sslength eset))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the entity name from the selection set" fil)
                (princ            "\n        (setq en(ssname eset cntr))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ            "\n        (setq enlist(entget en))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ            "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the point to move from" fil)
                (if(= (cadr actions) "FromEntity")
                  (princ          "\n        (setq movf(cdr(assoc 10 enlist)))" fil)
                  (princ (strcat  "\n        (setq movf(getpoint " (chr 34) "\\n Base point: " (chr 34) "))") fil)
                )
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the point to move to" fil)
                (princ (strcat    "\n        (setq movt(getpoint movf " (chr 34) "\\n Displacement point: " (chr 34) "))") fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- If both points are valid" fil)
                (princ            "\n        (if(and movf movt)" fil)
                (princ            "\n          " fil)
                (princ            "\n          ;;;--- Use the move command" fil)
                (princ (strcat    "\n          (command " (chr 34) "MOVE" (chr 34) " en " (chr 34)(chr 34)" movf movt)") fil)
                (princ            "\n        )" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ            "\n        (setq cntr(+ cntr 1))" fil)
                (princ            "\n      )" fil)                 
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;         COMMAND MOVE - FOR ALL ENTITIES       ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (setq pta(parseStrIntoList(cadr actions)))
                    (princ(strcat "\n      (setq movf(list " (rtos (atof(car pta)) 2 4)" "(rtos(atof(cadr pta)) 2 4) " "(rtos(atof(caddr pta)) 2 4) "))") fil)
                    (setq pta(parseStrIntoList(caddr actions)))
                    (princ(strcat "\n      (setq movt(list " (rtos (atof(car pta)) 2 4)" "(rtos(atof(cadr pta)) 2 4) " "(rtos(atof(caddr pta)) 2 4) "))") fil)                    
                  )
                )
                (princ            "\n      ;;;--- Use the move command" fil)
                (princ (strcat    "\n      (command " (chr 34) "MOVE" (chr 34) " eset " (chr 34)(chr 34)" movf movt)") fil)
                (princ            "\n      " fil)
              )
            )
          )
        )
        (
          (= actionType "OFFSET")
          (progn
            (if(member "Each" actions)
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;          COMMAND OFFSET FOR EACH ENTITY       ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Set up a counter" fil)
                (princ (strcat    "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ            "\n      (while(< cntr (sslength eset))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the entity name from the selection set" fil)
                (princ            "\n        (setq en(ssname eset cntr))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ            "\n        (setq enlist(entget en))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ            "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Prompt user for offset command parameters" fil)
                (princ            "\n        (if" fil)
                (princ            "\n          (and " fil)
                (princ (strcat    "\n            (setq offsetAng(getangle " (chr 34) "\\n Offset Angle: " (chr 34) "))") fil)                                    
                (princ (strcat    "\n            (setq offsetDis(getdist " (chr 34) "\\n Offset Distance: " (chr 34) "))") fil)
                (princ (strcat    "\n            (setq offsetNum(getint " (chr 34) "\\n Number of Offsets: " (chr 34) "))") fil)
                (princ            "\n          )" fil)
                (princ            "\n          (progn" fil)
                (princ            "\n            " fil)
                (princ            "\n            ;;;--- Build a list at 0,0" fil)
                (princ            "\n            (setq pt (list 0 0))" fil)
                (princ            "\n            " fil)
                (princ            "\n            ;;;--- Build an offset accumulator" fil)
                (princ            "\n            (setq offsetAccum offsetDis)" fil)                
                (princ            "\n            " fil)
                (princ            "\n            ;;;--- Repeat for every offset" fil)
                (princ            "\n            (repeat offsetNum"  fil)
                (princ            "\n              " fil)
                (princ            "\n              ;;;--- Use the copy command to accomplish an angular offset" fil)
                (princ (strcat    "\n              (command " (chr 34) "COPY" (chr 34) " en "(chr 34)(chr 34)) fil)
                (princ            "\n                pt (polar pt offsetAng offsetAccum)" fil)
                (princ            "\n              )" fil)
                (princ            "\n              " fil)
                (princ            "\n              ;;;--- Increment the accumulator" fil)
                (princ            "\n              (setq offsetAccum(+ offsetAccum offsetDis))" fil)                
                (princ            "\n            )" fil)
                (princ            "\n          )" fil)
                (princ            "\n        )" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ            "\n        (setq cntr(+ cntr 1))" fil)
                (princ            "\n      )" fil)                 
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;        COMMAND OFFSET - FOR ALL ENTITIES      ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n      ;;;--- Set the offset parameters" fil)
                    (princ(strcat "\n      (setq offsetAng " (rtos(angtof(cadr  actions))2 6) ")") fil)
                    (princ(strcat "\n      (setq offsetDis " (rtos(distof(caddr actions))2 6) ")") fil)
                    (princ(strcat "\n      (setq offsetNum " (cadddr actions) ")") fil)                    
                  )
                )
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Build a list at 0,0" fil)
                (princ            "\n      (setq pt (list 0 0))" fil)
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Build an offset accumulator" fil)
                (princ            "\n      (setq offsetAccum offsetDis)" fil)                
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Repeat for every offset" fil)
                (princ            "\n      (repeat offsetNum" fil)
                (princ            "\n      " fil)
                (princ            "\n        ;;;--- Use the copy command to accomplish an angular offset" fil)
                (princ (strcat    "\n        (command " (chr 34) "COPY" (chr 34) " eset "(chr 34)(chr 34)) fil)
                (princ            "\n          pt (polar pt offsetAng offsetAccum)" fil)
                (princ            "\n        )" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Increment the accumulator" fil)
                (princ            "\n        (setq offsetAccum(+ offsetAccum offsetDis))" fil)
                (princ            "\n      )" fil)
              )
            )
          )
        )
        (
          (= actionType "ROTATE")
          (progn
            (if(member "Each" actions)
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;        COMMAND ROTATE FOR EACH ENTITY         ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Set up a counter" fil)
                (princ (strcat    "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ            "\n      (while(< cntr (sslength eset))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the entity name from the selection set" fil)
                (princ            "\n        (setq en(ssname eset cntr))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ            "\n        (setq enlist(entget en))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ            "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the base point" fil)
                (if(= (cadr actions) "FromEntity")
                  (princ          "\n        (setq rotf(cdr(assoc 10 enlist)))" fil)
                  (princ (strcat  "\n        (setq rotf(getpoint " (chr 34) "\\n Base point: " (chr 34) "))") fil)
                )
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the rotation angle" fil)
                (princ (strcat    "\n        (setq rota(angtos(getangle rotf " (chr 34) "\\n Angle: " (chr 34) ")0 6))") fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- If both parameters are valid" fil)
                (princ            "\n        (if(and rotf rota)" fil)
                (princ            "\n          " fil)                
                (princ            "\n          ;;;--- Use the rotate command" fil)
                (princ (strcat    "\n          (command " (chr 34) "ROTATE" (chr 34) " en " (chr 34)(chr 34)" rotf rota)") fil)
                (princ            "\n        )" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ            "\n        (setq cntr(+ cntr 1))" fil)
                (princ            "\n      )" fil)                 
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;       COMMAND ROTATE - FOR ALL ENTITIES       ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n      ;;;--- Set the rotate parameters" fil)
                    (setq pt(parseStrIntoList (cadr actions)))
                    (princ(strcat "\n      (setq rotf(list " (rtos(atof(car pt))2 6)" " (rtos(atof(cadr pt))2 6) " " (rtos (atof(caddr pt))2 6) "))") fil)
                    (princ(strcat "\n      (setq rota " (angtos(angtof(caddr actions))0 6) ")") fil)                               
                  )
                )
                (princ            "\n      ;;;--- Use the rotate command" fil)
                (princ (strcat    "\n      (command " (chr 34) "ROTATE" (chr 34) " eset " (chr 34)(chr 34)" rotf rota)") fil)
              )
            )
          )
        )
        (
          (= actionType "SCALE")
          (progn
            (if(member "Each" actions)
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;         COMMAND SCALE FOR EACH ENTITY         ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Set up a counter" fil)
                (princ (strcat    "\n      (setq cntr 0 lt (getvar " (chr 34) "dimscale" (chr 34) "))") fil)
                (princ            "\n      " fil)
                (princ            "\n      ;;;--- Cycle through each entity in the selection set" fil)
                (princ            "\n      (while(< cntr (sslength eset))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the entity name from the selection set" fil)
                (princ            "\n        (setq en(ssname eset cntr))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the dxf group codes from the entity" fil)
                (princ            "\n        (setq enlist(entget en))" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Draw an arrow pointing to the current entity" fil)
                (princ            "\n        (setq pt(cdr(assoc 10 enlist)))" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the base point" fil)
                (if(= (cadr actions) "FromEntity")
                  (princ          "\n        (setq scaf(cdr(assoc 10 enlist)))" fil)
                  (princ (strcat  "\n        (setq scaf(getpoint " (chr 34) "\\n Base point: " (chr 34) "))") fil)
                )
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Get the scale factor" fil)
                (princ (strcat    "\n        (setq scfc(getreal " (chr 34) "\\n Scale factor: " (chr 34) "))") fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- If both parameters are valid" fil)
                (princ            "\n        (if(and scaf scfc)" fil)
                (princ            "\n          " fil)                                
                (princ            "\n          ;;;--- Use the scale command" fil)
                (princ (strcat    "\n          (command " (chr 34) "SCALE" (chr 34) " en " (chr 34)(chr 34)" scaf scfc)") fil)
                (princ            "\n        )" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Erase the arrow pointing to the current entity" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.9671 (* lt 1.2208))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 2.3562 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.5000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 5.4978 (* lt 2.0000))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 0.7854 (* lt 0.4502))) -1)" fil)
                (princ            "\n        (grdraw pt (setq pt(polar pt 4.8869 (* lt 1.2208))) -1)" fil)
                (princ            "\n        " fil)
                (princ            "\n        ;;;--- Increment the counter to get the next entity" fil)
                (princ            "\n        (setq cntr(+ cntr 1))" fil)
                (princ            "\n      )" fil)                 
              )
              ;;;--- Else actions contain "All" 
              (progn
                (princ            "\n      " fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                (princ            "\n      ;;;        COMMAND SCALE - FOR ALL ENTITIES       ;;;" fil)
                (princ            "\n      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)                
                (princ            "\n      " fil)
                (if(not(member "PROMPT" actions))
                  (progn
                    (princ        "\n      ;;;--- Set the scale parameters" fil)
                    (setq pt(parseStrIntoList (cadr actions)))
                    (princ(strcat "\n      (setq scaf(list " (rtos(atof(car pt))2 6)" " (rtos(atof(cadr pt))2 6) " " (rtos (atof(caddr pt))2 6) "))") fil)
                    (princ(strcat "\n      (setq scfc " (rtos(atof(caddr actions))2 6) ")") fil)                               
                  )
                )
                (princ            "\n      ;;;--- Use the scale command" fil)
                (princ (strcat    "\n      (command " (chr 34) "SCALE" (chr 34) " eset " (chr 34)(chr 34)" scaf scfc)") fil)
              )
            )
          )
        )
      )
    )
    (princ "\n    )" fil)   ;close progn
    (princ "\n  )" fil)     ;if eset
  )


  


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                                               ;;;
  ;;;                                                                                               ;;;
  ;;;                                                                                               ;;;
  ;;;                                                                                               ;;;
  ;;;                                                                                               ;;;
  ;;;                 ;;;          ;;;            ;;;            ;;;    ;;;      ;;;                ;;;
  ;;;                 ;;;;        ;;;;           ;;;;;           ;;;    ;;;;     ;;;                ;;;
  ;;;                 ;;;;;      ;;;;;          ;;; ;;;          ;;;    ;;;;;    ;;;                ;;;
  ;;;                 ;;;;;;    ;;;;;;         ;;;   ;;;         ;;;    ;;;;;;   ;;;                ;;;
  ;;;                 ;;;  ;;; ;;; ;;;        ;;;     ;;;        ;;;    ;;; ;;;  ;;;                ;;;
  ;;;                 ;;;   ;;;;;  ;;;       ;;;;;;;;;;;;;       ;;;    ;;;  ;;; ;;;                ;;;
  ;;;                 ;;;    ;;;   ;;;      ;;;;;;;;;;;;;;;      ;;;    ;;;   ;;;;;;                ;;;
  ;;;                 ;;;          ;;;     ;;;           ;;;     ;;;    ;;;    ;;;;;                ;;;
  ;;;                 ;;;          ;;;    ;;;             ;;;    ;;;    ;;;     ;;;;                ;;;
  ;;;                 ;;;          ;;;   ;;;               ;;;   ;;;    ;;;      ;;;                ;;;
  ;;;                                                                                               ;;;
  ;;;                                                                                               ;;;
  ;;;                                                                                               ;;;
  ;;;                                                                                               ;;;
  ;;;                                                                                               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                            S T E P  1                            ;;;
  ;;;                                                                  ;;;
  ;;;                 GET FILENAME FOR AUTOLISP PROGRAM                ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                          
  ;;;--- Get the file name of the autolisp program to create.  
  ;;;                                                          
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  ;;;--- Get a temporary name for the autolisp file
  (setq tmpName(strcase(vl-filename-base(vl-filename-mktemp (getvar "loginname") nil ".lsp"))))
    
  ;;;--- Get the file name
  (setq filName(getfiled "Creator Ver. 1.1 > Step 1 - Name of Autolisp Program to Create" tmpName "lsp" 1))





  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;                            S T E P  2                            ;;;
  ;;;                                                                  ;;;
  ;;;                        ENTITY SELECTION                          ;;;
  ;;;                                                                  ;;;
  ;;;                                                                  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;---  If a valid file name was entered...
  (if filName
    (progn

      ;;;--- Load the dialog box from file
      (setq dcl_id (load_dialog "CREATOR.dcl"))

      ;;;--- Try to load the CREATOR dialog inside the DCL file
      (if (not (new_dialog "CREATOR" dcl_id))
        (progn
          (setq alertStr "The CREATOR.DCL file could not be loaded.")
          (setq alertStr
            (strcat 
              alertStr
              "\n\nMake sure the DCL file is in a directory located"
              "\ninside the autocad search path. For more information"
              "\non this, please see this web page:"
              "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
            )
          )
          (alert alertStr)
          (exit)
        )
      )

      (setq selSing 1)
      (setq selMult 0)
      (setq selGlob 0)

      ;;;--- Disable future entity toggles
      (mode_tile "togf01" 1)
      (mode_tile "togf02" 1)
      (mode_tile "togf03" 1)
      (mode_tile "togf04" 1)

      ;;;--- Disable all filter toggles
      (mode_tile "ftogatt" 1)
      (mode_tile "ftogblk" 1)
      (mode_tile "ftogclr" 1)
      (mode_tile "ftoglay" 1)
      (mode_tile "ftoglty" 1)
      (mode_tile "ftogaxi" 1)
      (mode_tile "ftograd" 1)
      (mode_tile "ftogtag" 1)
      (mode_tile "ftogtxt" 1)
      (mode_tile "ftogsty" 1)
      
      ;;;--- Disable future filter toggles
      (mode_tile "ftogf01" 1)

      ;;;--- Disable the NEXT button
      (mode_tile "accept" 1)

      ;;;--- If an action event occurs, do this function
      (action_tile "selsing" "(chkFilterToggle)")
      (action_tile "selmult" "(chkFilterToggle)")
      (action_tile "selglob" "(chkFilterToggle)")
      (action_tile "togarc"  "(chkFilterToggle)")
      (action_tile "togatt"  "(chkFilterToggle)")
      (action_tile "togcir"  "(chkFilterToggle)")
      (action_tile "togell"  "(chkFilterToggle)")
      (action_tile "togimg"  "(chkFilterToggle)")
      (action_tile "togins"  "(chkFilterToggle)")
      (action_tile "toglin"  "(chkFilterToggle)")
      (action_tile "toglwp"  "(chkFilterToggle)")
      (action_tile "togmli"  "(chkFilterToggle)")
      (action_tile "togmtx"  "(chkFilterToggle)")
      (action_tile "togpnt"  "(chkFilterToggle)")
      (action_tile "togpol"  "(chkFilterToggle)")
      (action_tile "togsol"  "(chkFilterToggle)")
      (action_tile "togtxt"  "(chkFilterToggle)")
      (action_tile "togtrc"  "(chkFilterToggle)")
      (action_tile "togxli"  "(chkFilterToggle)")
      (action_tile "togf01"  "(chkFilterToggle)")
      (action_tile "togf02"  "(chkFilterToggle)")
      (action_tile "togf03"  "(chkFilterToggle)")
      (action_tile "togf04"  "(chkFilterToggle)")
      (action_tile "ftogatt" "(chkXtraToggle 1)")
      (action_tile "ftogblk" "(chkXtraToggle 2)")
      (action_tile "ftogclr" "(chkXtraToggle 3)")
      (action_tile "ftoglay" "(chkXtraToggle 4)")
      (action_tile "ftoglty" "(chkXtraToggle 5)")
      (action_tile "ftogaxi" "(chkXtraToggle 6)")
      (action_tile "ftograd" "(chkXtraToggle 7)")
      (action_tile "ftogtag" "(chkXtraToggle 8)")
      (action_tile "ftogtxt" "(chkXtraToggle 9)")
      (action_tile "ftogsty" "(chkXtraToggle 10)")
      (action_tile "accept"  "(saveVars)(done_dialog 8)")
      (action_tile "cancel"  "(done_dialog 7)")

      ;;;--- Display the dialog box
      (setq doneDiag(start_dialog))

      (if(= doneDiag 8)
        (progn

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;                                                                  ;;;
          ;;;                                                                  ;;;
          ;;;                            S T E P  3                            ;;;
          ;;;                                                                  ;;;
          ;;;      CHECK THE FILTER LIST AND REMOVE NONCOMPLIANT ENTITIES      ;;;
          ;;;                                                                  ;;;
          ;;;                                                                  ;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          (setq writeFilterAttValu nil)
          (setq writeFilterBlkName nil)
          (setq writeFilterClrNumb nil)
          (setq writeFilterLayName nil)
          (setq writeFilterLtyName nil)
          (setq writeFilterMaxAxis nil)
          (setq writeFilterRadValu nil)
          (setq writeFilterTagName nil)
          (setq writeFilterTxtValu nil)
          (setq writeFilterStyName nil)
          
          (foreach a filterList
            (cond
              ((= (car a) "AttVal") (setq writeFilterAttValu (cadr a)))
              ((= (car a) "Block")  (setq writeFilterBlkName (cadr a)))
              ((= (car a) "Color")  (setq writeFilterClrNumb (cadr a)))
              ((= (car a) "Layer")  (setq writeFilterLayName (cadr a)))
              ((= (car a) "LType")  (setq writeFilterLTYname (cadr a)))
              ((= (car a) "MAxis")  (setq writeFilterMaxAxis (cadr a)))
              ((= (car a) "Radius") (setq writeFilterRadValu (cadr a)))
              ((= (car a) "Tag")    (setq writeFilterTagName (cadr a)))
              ((= (car a) "Value")  (setq writeFilterTxtValu (cadr a)))
              ((= (car a) "Style")  (setq writeFilterStyName (cadr a)))
            )                                                               ;cond
          )                                                                 ;foreach  
                  

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;                                                                  ;;;
          ;;;                                                                  ;;;
          ;;;                            S T E P  4                            ;;;
          ;;;                                                                  ;;;
          ;;;                        GET ACTIONS TO TAKE                       ;;;
          ;;;                                                                  ;;;
          ;;;                                                                  ;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


          ;;;--- Set up a list to hold the actions
          (setq actionList(list))

          (setq doneDiag 9)

          ;;;--- Loop until the user presses CANCEL or NEXT
          (while(> doneDiag 7)

            ;;;--- Try to load the ACTIONS dialog inside the DCL file
            (if (not (new_dialog "ACTIONS" dcl_id))
              (progn
                (setq alertStr "The ACTIONS dialog could not be loaded.")
                (setq alertStr
                  (strcat 
                    alertStr
                    "\n\nMake sure the DCL file is in a directory located"
                    "\ninside the autocad search path. For more information"
                    "\non this, please see this web page:"
                    "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
                  )
                )
                (alert alertStr)
                (exit)
              )
            )

            ;;;--- If actions exist in the actions list, display them in the dialog box
            (displayActions)

            ;;;--- Disable the NEXT button if actions do not exist
            (mode_tile "accept" 1)
            (if(> (length actionList) 0)
              (mode_tile "accept" 0)
            )

            (mode_tile "moveup" 1)
            (mode_tile "movedn" 1)
            (mode_tile "remove" 1)

            (if(= selSing "1")
              (progn
                (mode_tile "actadd" 1)
                (mode_tile "actcnt" 1)
              )
            )

            (mode_tile "actchfut" 1)

            ;;;--- Disable the buttons based on entity types selected
            (setActionButtons)

 
            ;;;--- If an action event occurs, do this function...
            (action_tile "moveup"   "(actionMoveUp)")
            (action_tile "movedn"   "(actionMoveDown)")
            (action_tile "remove"   "(actionRemove)")
            (action_tile "actionlist" "(actionListAction)")
            (action_tile "actchang" "(done_dialog 10)")
            (action_tile "actchclr" "(done_dialog 11)")
            (action_tile "actchelv" "(done_dialog 12)")
            (action_tile "actchhgt" "(done_dialog 13)")
            (action_tile "actchlay" "(done_dialog 14)")
            (action_tile "actchlty" "(done_dialog 15)")
            (action_tile "actchrad" "(done_dialog 16)")
            (action_tile "actchsty" "(done_dialog 17)")
            (action_tile "actchfut" "(done_dialog 18)")
            (action_tile "actchval" "(done_dialog 19)")
            (action_tile "actadd"   "(done_dialog 20)")
            (action_tile "actarr"   "(done_dialog 21)")
            (action_tile "actcop"   "(done_dialog 22)")
            (action_tile "actcnt"   "(done_dialog 23)")
            (action_tile "actdel"   "(done_dialog 24)")
            (action_tile "actmir"   "(done_dialog 25)")
            (action_tile "actmov"   "(done_dialog 26)")
            (action_tile "actoff"   "(done_dialog 27)")
            (action_tile "actrot"   "(done_dialog 28)")
            (action_tile "actscl"   "(done_dialog 29)")
            (action_tile "accept"   "(done_dialog 7)")
            (action_tile "cancel"   "(done_dialog 6)")

            ;;;--- Display the dialog box
            (setq doneDiag(start_dialog))


            ;;;--- If the user pressed the NEXT button
            (if(= doneDiag 7)
              (progn

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;;;                                                                  ;;;
                ;;;                                                                  ;;;
                ;;;                            S T E P  5                            ;;;
                ;;;                                                                  ;;;
                ;;;                        CREATE THE PROGRAM                        ;;;
                ;;;                                                                  ;;;
                ;;;                                                                  ;;;
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                ;;;--- Try to load the CREATE dialog inside the DCL file
                (if (not (new_dialog "CREATE" dcl_id))
                  (progn
                    (setq alertStr "The CREATE dialog could not be loaded.")
                    (setq alertStr
                      (strcat 
                        alertStr
                        "\n\nMake sure the DCL file is in a directory located"
                        "\ninside the autocad search path. For more information"
                        "\non this, please see this web page:"
                        "\n\nhttp:\\www.jefferypsanders.com/autolisp_nodcl.html"
                      )
                    )
                    (alert alertStr)
                    (exit)
                  )
                )

                ;;;--- Set up the display string in the dialog box
                (setq str(strcat "Ready to create " filName))
                (set_tile "textvalue1" str)
                (set_tile "textvalue2"  "Press Next to continue or press cancel to exit.")
 
                ;;;--- If an action event occurs, do this function...
                (action_tile "accept5"   "(done_dialog 10)")
                (action_tile "cancel5"   "(done_dialog 11)")

                ;;;--- Display the dialog box
                (setq dDiag5(start_dialog))

                (if(= dDiag5 10)
                  (progn

                    (setq addvalFlag    T addAREFlag    T addLenFlag  T addRadFlag   T
                          addDiaFlag    T addCirFlag    T copyFlag    T deleteFlag   T
                          countFlag     T valFirstFlag  T valLastFlag T valEveryFlag T
                          valPrefixFlag T valSuffixFlag T
                    )
                    (setq addCntr nil areaCntr nil lenCntr nil radCntr nil diaCntr nil)
                    (setq writePromptAllFlag T)
                    (setq writePromptEachFlag T)
                         
                    (if(setq fil(open filName "w"))
                      (progn

                        ;;;--- Build a description string for the program [ this could get long ]
                        (setq gList(list) aList(list) cList(list))
                        (foreach a actionlist
                          (cond
                            ((= "ANGL" (substr a 1 4))(setq gList(append gList(list "Angle"))))
                            ((= "COLO" (substr a 1 4))(setq gList(append gList(list "Color"))))
                            ((= "ELEV" (substr a 1 4))(setq gList(append gList(list "Elevation"))))
                            ((= "HEIG" (substr a 1 4))(setq gList(append gList(list "Height"))))
                            ((= "LAYE" (substr a 1 4))(setq gList(append gList(list "Layer"))))
                            ((= "LINE" (substr a 1 4))(setq gList(append gList(list "Linetype"))))
                            ((= "RADI" (substr a 1 4))(setq gList(append gList(list "Radius"))))
                            ((= "STYL" (substr a 1 4))(setq gList(append gList(list "Style"))))
                            ((= "VAL_" (substr a 1 4))(setq gList(append gList(list "Value"))))
                            (
                              (= "ADD" (substr a 1 3))
                              (progn
                                (cond
                                  ((= "VALU" (substr a 5 4))(setq aList(append aList(list "Value"))))
                                  ((= "LENG" (substr a 5 4))(setq aList(append aList(list "Length"))))
                                  ((= "RADI" (substr a 5 4))(setq aList(append aList(list "Radius"))))
                                  ((= "DIAM" (substr a 5 4))(setq aList(append aList(list "Diameter"))))
                                  ((= "AREA" (substr a 5 4))(setq aList(append aList(list "Area"))))
                                )  
                              )
                            )
                            ((= "ARRA" (substr a 1 4))(setq cList(append cList(list "Array"))))
                            ((= "COPY" (substr a 1 4))(setq cList(append cList(list "Copy"))))
                            ((= "COUN" (substr a 1 4))(setq cList(append cList(list "Count"))))
                            ((= "DELE" (substr a 1 4))(setq cList(append cList(list "Delete"))))
                            ((= "MIRR" (substr a 1 4))(setq cList(append cList(list "Mirror"))))
                            ((= "MOVE" (substr a 1 4))(setq cList(append cList(list "Move"))))
                            ((= "OFFS" (substr a 1 4))(setq cList(append cList(list "Offset"))))
                            ((= "ROTA" (substr a 1 4))(setq cList(append cList(list "Rotate"))))
                            ((= "SCAL" (substr a 1 4))(setq cList(append cList(list "Scale"))))
                          )  
                        )
                        (setq dStr "")
                        (if gList
                          (progn
                            (setq dStr(strcat dStr "Change "))
                            (foreach a gList(setq dStr(strcat dStr a ", ")))
                            (setq dStr(substr dStr 1 (-(strlen dStr)2)))
                          )
                        )
                        (if aList
                          (progn
                            (if gList
                              (setq dStr(strcat dStr " + Add "))
                              (setq dStr(strcat dStr " Add "))
                            )
                            (foreach a aList(setq dStr(strcat dStr a ", ")))
                            (setq dStr(substr dStr 1 (-(strlen dStr)2)))
                          )
                        )
                        (if cList
                          (progn
                            (if (or gList aList)
                              (setq dStr(strcat dStr " + Use Command "))
                              (setq dStr(strcat dStr " Use Command "))
                            )
                            (foreach a cList(setq dStr(strcat dStr a ", ")))
                            (setq dStr(substr dStr 1 (-(strlen dStr)2)))
                          )
                        )

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                                   ;;;
                        ;;;--- Write the program header                                       ;;;
                        ;;;                                                                   ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                        (princ 
                          (strcat 
                            ";;;--- "  
                            (vl-filename-base  filname) ".lsp - "
                            dStr
                          )
                          fil
                        )
                        (repeat 2 (princ "\n;;;" fil))
                        (princ "\n;;;--- This program was created by CREATOR.lsp  [ Get it free at JefferyPSanders.com ]" fil)
                        (princ "\n;;;\n;;;\n;;;\n;;; \n  " fil)


                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                                      ;;;
                        ;;;--- Build a list to hold all of the variables used in the program     ;;;
                        ;;;                                                                      ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                        (setq varList(list))
                        (foreach a actionlist
                          (setq actions(parseAction a))
                          (setq actionType(car actions))
                          (if(= actionType "ADD")(setq actionType(strcat actionType (cadr actions))))
                          (if(= actionsType "ARRAY")(setq actionType(strcat actionType (cadr actions))))
                          
                          (if(= actionType "ANGLE"        )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "ang"))))
                          (if(= actionType "COLOR"        )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "clr"))))
                          (if(= actionType "ELEVATION"    )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "elv"))))
                          (if(= actionType "HEIGHT"       )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "hgt"))))
                          (if(= actionType "LAYER"        )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "lay"))))
                          (if(= actionType "LINETYPE"     )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "lty"))))
                          (if(= actionType "RADIUS"       )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "rad"))))
                          (if(= actionType "STYLE"        )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "sty"))))
                          (if(= actionType "VAL_REPLACE"  )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "valNew"))))
                          (if(= actionType "VAL_FIRST"    )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "valFirstOld" "valFirstNew"))))
                          (if(= actionType "VAL_LAST"     )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "valLastOld"  "valLastNew"))))
                          (if(= actionType "VAL_EVERY"    )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "valEveryOld" "valEveryNew"))))
                          (if(= actionType "VAL_INC_PREFX")(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "valPrefxInc"))))
                          (if(= actionType "VAL_INC_SUFFX")(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "valSuffxInc"))))
                          (if(= actionType "ADDVALUE"     )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "val" "addValTotal" "displayString"))))
                          (if(= actionType "ADDAREA"      )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "val" "addAreTotal" "displayString"))))                          
                          (if(= actionType "ADDLENGTH"    )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "val" "addLenTotal" "displayString"))))
                          (if(= actionType "ADDRADIUS"    )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "val" "addRadTotal" "displayString"))))
                          (if(= actionType "ADDDIAMETER"  )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "val" "addDiaTotal" "displayString"))))
                          (if(= actionType "ARRAYPOLAR"   )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "elv" "lt" "cenPt" "PolarAns" "numIt"))))
                          (if(= actionType "ARRAYRECT"    )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "elv" "lt" "xQty" "yQty" "xDis" "yDis" "cenPtx" "cenPty"))))
                          (if(= actionType "COPY"         )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "cpyf" "cpyt" "pta"))))
                          (if(= actionType "COUNT"        )(setq varList(append varList (list "eset" "pt" "totals"))))
                          (if(= actionType "DELETE"       )(setq varList(append varList (list "cntr" "eset" "en" "cnt"))))
                          (if(= actionType "MIRROR"       )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "mirf" "mirt"))))
                          (if(= actionType "MOVE"         )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "pta" "movf" "movt"))))
                          (if(= actionType "OFFSET"       )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "offsetAng" "offsetDis" "offsetNum" "offsetAccum"))))
                          (if(= actionType "ROTATE"       )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "rotf" "rota"))))
                          (if(= actionType "SCALE"        )(setq varList(append varList (list "cntr" "eset" "en" "enlist" "pt" "scaf" "scfc"))))
                        )
                        (if varList
                          (progn
                            ;;;--- Remove duplicates
                            (setq newList(list))
                            (foreach a varList
                              (if(not(member a newList))(setq newList(append newList (list a))))
                            )
                            (setq varList nil)
                            (setq varStr "")
                            (foreach a newList
                              (setq varStr(strcat varStr " " a))
                            )
                            (setq newList nil)
                          )
                        )

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                                   ;;;
                        ;;;--- Write the opening defun with local variables                   ;;;
                        ;;;                                                                   ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                        (princ (strcat "\n(defun C:" (vl-filename-base  filname) "(/" varStr ")") fil)


                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                                   ;;;
                        ;;;                                                                   ;;;
                        ;;;--- Write the autolisp sub-functions and variables if required     ;;;
                        ;;;                                                                   ;;;
                        ;;;                                                                   ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                        (foreach a actionList
                          (cond
                            ( (and addvalFlag    (= (substr a 1 7)  "ADD VAL"        )) (writeFunctionADDVAL))
                            ( (and addAreFlag    (= (substr a 1 7)  "ADD ARE"        )) (writeFunctionADDARE))
                            ( (and addLenFlag    (= (substr a 1 7)  "ADD LEN"        )) (writeFunctionADDLEN))
                            ( (and addRadFlag    (= (substr a 1 7)  "ADD RAD"        )) (writeFunctionADDRAD))
                            ( (and addDiaFlag    (= (substr a 1 7)  "ADD DIA"        )) (writeFunctionADDDIA))
                            ( (and deleteFlag    (= (substr a 1 11) "DELETE Each"    )) (writeFunctionDeleteEach))
                            ( (and countFlag     (= (substr a 1 5)  "COUNT"          )) (writeFunctionCount selGlob))
                            ( (and valFirstFlag  (= (substr a 1 9)  "VAL_FIRST"      )) (writeValFirst))
                            ( (and valLastFlag   (= (substr a 1 8)  "VAL_LAST"       )) (writeValLast))
                            ( (and valEveryFlag  (= (substr a 1 9)  "VAL_EVERY"      )) (writeValEvery))
                            ( (and valPrefixFlag (= (substr a 1 11) "VAL_INC_PRE"    )) (writeValPrefix))
                            ( (and valSuffixFlag (= (substr a 1 11) "VAL_INC_SUF"    )) (progn(writeValPrefix)(writeValSuffix)))
                          )
                        )
                        
                        (if writeFilterAttValu (FilterAttValuX))
                        (if writeFilterBlkName (FilterBlkNameX))
                        (if writeFilterClrNumb (FilterClrNumbX))
                        (if writeFilterLayName (FilterLayNameX))
                        (if writeFilterLTYname (FilterLTYnameX))
                        (if writeFilterMaxAxis (FilterMaxAxisX)) 
                        (if writeFilterRadValu (FilterRadValuX)) 
                        (if writeFilterTagName (FilterTagNameX)) 
                        (if writeFilterTxtValu (FilterTxtValuX)) 
                        (if writeFilterStyName (FilterStyNameX))



                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                                  ;;;
                        ;;;--- Create the main app note                                      ;;;
                        ;;;                                                                  ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        
                        (princ "\n  " fil)
                        (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                        (princ "\n  ;;;                                             ;;;" fil)
                        (princ "\n  ;;;             Main Application                ;;;" fil)
                        (princ "\n  ;;;                                             ;;;" fil)
                        (princ "\n  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" fil)
                        (princ "\n  " fil)

                        
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                                  ;;;
                        ;;;--- Write the command echo off statement                          ;;;
                        ;;;                                                                  ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        
                        (princ "\n" fil)
                        (princ "\n  ;;;--- Turn the command echo off" fil)
                        (princ (strcat "\n  (setvar " (chr 34) "cmdecho" (chr 34) " 0)") fil)
                        (princ "\n  " fil)



                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                                   ;;;
                        ;;;--- Write the prompts if ALL entities are to be modified           ;;;
                        ;;;                                                                   ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                        (writePrompts "\n  ")

                           

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                   ;;;
                        ;;;--- Write the entity selection code                ;;;
                        ;;;                                                   ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                        (writeSelectioncode)


                        
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                   ;;;
                        ;;;--- Write the entity filter code                   ;;;
                        ;;;                                                   ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       

                        ;;;--- Write the filter code
                        (if writeFilterAttValu (princ (strcat "\n  ;;;--- Filter for Attribute Value\n  (if(and eset(> (sslength eset) 0))(setq eset(FilterAttValuX eset " (chr 34) writeFilterAttValu (chr 34) ")))") fil))
                        (if writeFilterBlkName (princ (strcat "\n  ;;;--- Filter for Block Name     \n  (if(and eset(> (sslength eset) 0))(setq eset(FilterBlkNameX eset " (chr 34) writeFilterBlkName (chr 34) ")))") fil))
                        (if writeFilterClrNumb (princ (strcat "\n  ;;;--- Filter for Color Number   \n  (if(and eset(> (sslength eset) 0))(setq eset(FilterClrNumbX eset " (chr 34) writeFilterClrNumb (chr 34) ")))") fil))
                        (if writeFilterLayName (princ (strcat "\n  ;;;--- Filter for Layer Name     \n  (if(and eset(> (sslength eset) 0))(setq eset(FilterLayNameX eset " (chr 34) writeFilterLayName (chr 34) ")))") fil))
                        (if writeFilterLTYname (princ (strcat "\n  ;;;--- Filter for Linetype Name  \n  (if(and eset(> (sslength eset) 0))(setq eset(FilterLTYnameX eset " (chr 34) writeFilterLTYname (chr 34) ")))") fil))
                        (if writeFilterMaxAxis (princ (strcat "\n  ;;;--- Filter for Max Axis       \n  (if(and eset(> (sslength eset) 0))(setq eset(FilterMaxAxisX eset " (chr 34) writeFilterMaxAxis (chr 34) ")))") fil))
                        (if writeFilterRadValu (princ (strcat "\n  ;;;--- Filter for Radius         \n  (if(and eset(> (sslength eset) 0))(setq eset(FilterRadValuX eset " (chr 34) writeFilterRadValu (chr 34) ")))") fil))
                        (if writeFilterTagName (princ (strcat "\n  ;;;--- Filter for Attribute Tag  \n  (if(and eset(> (sslength eset) 0))(setq eset(FilterTagNameX eset " (chr 34) writeFilterTagName (chr 34) ")))") fil))
                        (if writeFilterTxtValu (princ (strcat "\n  ;;;--- Filter for Text Value     \n  (if(and eset(> (sslength eset) 0))(setq eset(FilterTxtValuX eset " (chr 34) writeFilterTxtValu (chr 34) ")))") fil))
                        (if writeFilterStyName (princ (strcat "\n  ;;;--- Filter for Text Style     \n  (if(and eset(> (sslength eset) 0))(setq eset(FilterStyNameX eset " (chr 34) writeFilterStyName (chr 34) ")))") fil))
                     
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                                 ;;;
                        ;;;--- Write the code for the actions                               ;;;
                        ;;;                                                                 ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                        (writeActionCode)

                       
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;                                                  ;;;
                        ;;;--- Write the closing statements                  ;;;
                        ;;;                                                  ;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                        (princ "\n" fil)
                        (princ "\n  ;;;--- Reset the command echo" fil)
                        (princ (strcat "\n  (setvar " (chr 34) "cmdecho" (chr 34) " 1)") fil)
                        (princ "\n" fil)
                        (princ "\n  ;;;--- Suppress the last echo for a clean exit" fil)
                        (princ "\n  (princ)" fil)
                        (princ "\n)" fil)
                        (princ (strcat "\n(princ "(chr 34)"\\n Type " (strcase(vl-filename-base filname)) " to run the program." (chr 34)")") fil)
                        (princ "\n(princ)" fil)
                        
                        (close fil)
                        (alert (strcat "\n Program > " filName  " was created successfully."))
                      )
                      (alert "Program was not created. An error occured.\nCould not open file to write.")
                    )									;if file was opened
                  )
                )	     								;if (= dDiag5 10)
              )
            )										;if (= donediag 7)

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;;                                                                  ;;;
            ;;;                           E N D    O F                           ;;;
            ;;;                            S T E P  4                            ;;;
            ;;;                                                                  ;;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
            (cond
              ((= doneDiag 10) (getChangesDialog "ANGLE"))
              ((= doneDiag 11) (getChangesDialog "COLOR"))
              ((= doneDiag 12) (getChangesDialog "ELEVATION"))
              ((= doneDiag 13) (getChangesDialog "HEIGHT"))
              ((= doneDiag 14) (getChangesDialog "LAYER"))
              ((= doneDiag 15) (getChangesDialog "LINETYPE"))
              ((= doneDiag 16) (getChangesDialog "RADIUS"))
              ((= doneDiag 17) (getChangesDialog "STYLE"))
              ((= doneDiag 19) (getChangesValueDialog))
              ((= doneDiag 20)(actionAdd))
              ((= doneDiag 21)(actionArray))
              ((= doneDiag 22)(actionCopy))
              ((= doneDiag 23)(actionCount))
              ((= doneDiag 24)(actionDelete))
              ((= doneDiag 25)(actionMirror))
              ((= doneDiag 26)(actionMove))
              ((= doneDiag 27)(actionOffset))
              ((= doneDiag 28)(actionRotate))
              ((= doneDiag 29)(actionScale))
            )
          ) 										;while (> donediag 7)
        )
      )	 										;if (= donediag 8)
    )
  )   											;if filName
  (princ)
)
