;;;--- IMPORTXYZ.lsp - Import coords from a file.
;;;
;;;
;;;--- Copyright 2005 by JefferyPSanders.com
;;;    All rights reserved.
;;;
;;;--- Revisions
;;;
;;;    3/2/06 - Solved problem with blank Excel lines.


(defun C:IMPORTXYZ()

  (setq exitMessage "\n IMPORTXYZ.lsp Complete. \n ")




  ;;;--- Function to put the values of a range of cells in a list
  ;;;
  ;;;--- Parameters:
  ;;;
  ;;;      stRow = starting row number as integer
  ;;;      stCol = starting column number as integer
  ;;;      LsRow = last row number as integer
  ;;;      LsCol = last column number as integer

  (defun GetRangeCells(stRow stCol LsRow LsCol / cellList copyCol)

    ;;;--- Make a copy of the first column
    (setq copyCol stCol)

    ;;;--- Build an empty list to hold the cell's addresses and values
    (setq cellList(list))

    ;;;--- Save the column
    (setq tmpCol stCol)

    ;;;--- Set up a cell counter and a flag
    (setq cellCnt 0 oldstRow nil)
  
    ;;;--- Loop while we are inside the range
    (while (<= stRow LsRow )

      ;;;--- Increment the cell counter
      (setq cellCnt(+ cellCnt 1))

      ;;;--- Add the address and value to the cell list
      (setq cellList
        (append cellList
                
          ;;;--- Build a list containing the cell's row and value
          (list
            (cons

              ;;;--- Add the row
              stRow
                  
              ;;;--- Add the value of the cell
              (vlax-variant-value
                (JXCL-get-value
                  (vlax-variant-value
                    (JXCL-get-item
                      (JXCL-get-cells (JXCL-get-ActiveSheet myApp))
                      (vlax-make-variant stRow)
                      (vlax-make-variant stCol)
                    )
                  )   
                )
              )
            )
          )
        )        
      )

      ;;;--- Inform the user of progress
      (if(> cellCnt 99)
        (progn
          (princ "\n Currently retrieving cells in Row ")
          (princ (strcat (itoa stRow) " of " (itoa LsRow)))
          (setq cellCnt 0)
        )
      )

      ;;;--- Increment the column
      (setq stCol(+ stCol 1))

      ;;;--- Make sure the column stays in the range
      (if(> stCol LsCol)
        (setq stCol copyCol stRow(+ stRow 1))
      )
    ) 

    ;;;--- Return the list
    cellList
  )  



  ;;;---------------------------------------------------------------------------------------



  ;;;--- Function to get the selected worksheet name from the dialog box
  (defun saveVars3()

    ;;;--- Get the index of the selected sheet
    (setq sheetIndex(atoi(get_tile "sheetlist")))

    ;;;--- Use the index to find the name of the sheet
    (setq sheetName(nth sheetIndex sheetList))
  ) 



  ;;;---------------------------------------------------------------------------------------



  ;;;--- Function to convert a column number to an excel column letter
  ;;;
  ;;;--- Parameters:
  ;;;
  ;;;       a = Column number as integer
  ;;;
  ;;;--- Returns:
  ;;;
  ;;;       Column name as in Excel  Ex. "A" or "AB"
  ;;;
  ;;;
  ;;;--- Limitations
  ;;;
  ;;;      Works from 1 to 702

  (defun N2C(a)
    (if(< a 27)
      (setq column (chr (+ a 64)))
      (setq column
        (strcat
          (if(= 91 (+ 64(fix(/ a 26.001))))
             "Z"
             (chr(+ 64(fix(/ a 26.001))))
          )  
          (if(= 64 (+ 64(- a(* 26(fix(/ a 26))))))
            "Z"
            (chr(+ 64(- a(* 26(fix(/ a 26))))))
          )
        )
      )
    )  
    column
  )


  ;;;---------------------------------------------------------------------------------------

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;;--- Function to get the data from an Excel Sheet
  ;;;
  ;;;    Parameter - fileName = Excel Spread sheet name including path if necessary
  ;;;

  (defun XL_GET (fileName / myApp sysDrive dataList myWBooks myWBook sht shtCnt mySheets
                            sheetList cnt shtName ddiag mySht mySheet myRange myAddress
                            myCells 1stRow 1stCol LstRow LstCol dataList)

    ;;;--- Get the system drive
    (setq sysDrive (getenv "systemdrive"))

    ;;;--- If the excel object library is not found...load it
    (if (null Library)
      (progn

        ;;;--- Find out which version we should use
        (setq Library
          (cond
            ((findfile (strcat sysDrive "\\Program Files\\Microsoft Office\\Office\\Excel8.olb")))
            ((findfile (strcat sysDrive "\\Program Files\\Microsoft Office\\Office\\Excel9.olb")))
            ((findfile (strcat sysDrive "\\Program Files\\Microsoft Office\\Office\\Excel10.olb")))
            ((findfile (strcat sysDrive "\\Program Files\\Microsoft Office\\Office\\Excel.exe")))
            ((findfile (strcat sysDrive "\\Program Files\\Microsoft Office\\Office10\\Excel.exe")))
            ((findfile (strcat sysDrive "\\Program Files\\Microsoft Office\\Office11\\Excel.exe"))) 
            ((findfile (strcat sysDrive "\\Program Files\\Microsoft Office\\Office11\\XL5EN32.OLB")))
          )
        )

        ;;;--- If the library was found...
        (if Library
          (progn

            ;;;--- Strip off 
            (setq LibVer (substr (vl-filename-base Library) 6))
            (cond
              ((= LibVer "8")(princ "\n Opening Excel Version 8..."))
              ((= LibVer "9")(princ "\n Opening Excel Version 9..."))
              ((= LibVer "1")(princ "\n Opening Excel Version 10..."))
              ((= LibVer "") (princ "\n Opening Excel Version 2000+..."))
              ((= LibVer "3")(princ "\n Opening Excel Version 2003..."))
  
            )
            (vlax-import-type-library
              :tlb-filename Library
              :methods-prefix "JXCL-"
              :properties-prefix "JXCL-"
              :constants-prefix "JXCL-"
            )
            (princ " Done.")
          )
          (alert "Excel Object Library was not found!\n\nSee the XL HELP on my web site.")
        )
      )
    )

    ;;;--- If an excel application is not loaded, proceed...
  
    (if (null myApp)
      (progn

        ;;;--- If Excel...
        (if(setq myapp(vlax-get-or-create-object "Excel.Application"))
          (progn

            ;;;--- Open the workbook
            (vlax-invoke-method (vlax-get-property myapp 'WorkBooks) 'Open fileName)
  
            ;;;--- Set it to invisible mode
            (vla-put-visible myApp 0)

            ;;;--- Get the workbooks object
            (setq myWBooks(vlax-get myApp "Workbooks"))

            ;;;--- Open the excel file
            (setq myWBook(vla-open myWBooks fileName))

            ;;;;--- Get the sheets object
            (setq mySheets(vlax-get myWBook "Sheets"))

            ;;;--- Get a list of the sheet names
            (princ "\n Getting Sheet Names from Excel...")
            (setq shtCnt(vla-get-count mySheets))
            (setq sheetList(list))
            (setq cnt 1)
            (while(<= cnt shtCnt)
              (setq sht(JXCL-get-item mySheets cnt))
              (setq shtName(vla-get-name sht))
              (setq sheetList(append sheetList(list shtName)))
              (setq cnt(+ cnt 1))
            )
            (princ " Done.")
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;;--- Let the user select a sheet  ;;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

              
            ;;;--- See if it is already loaded
            (if (not (new_dialog "XL_GET" dcl_id))
              (progn
                (alert "Could not find the IMPORTXYZ.DCL file!")
                (exit)
              )
            )

            ;;;--- Add the layer names to the dialog box
            (start_list "sheetlist" 3)
            (mapcar 'add_list sheetList)
            (end_list)

            ;;;--- Set the first sheet as default
            (set_tile "sheetlist" "0")

            ;;;--- If an action event occurs, do this function
            (action_tile "cancel" "(setq ddiag1 1)(done_dialog)")
            (action_tile "accept" "(setq ddiag1 2)(saveVars3)(done_dialog)") 

            ;;;--- Display the dialog box
            (start_dialog)

            ;;;--- If the "Okay" button was pressed
            (if (= ddiag1 2)
              (progn

                ;;;--- Get the selected worksheet
                (setq mySht(vlax-get-property mySheets 'Item sheetName))

                ;;;--- Make the selected worksheet active
                (vlax-invoke-method mysht "Activate")

                ;;;--- Get the used range of the worksheet
                (setq myRange(vlax-get-property mySht 'UsedRange))

                ;;;--- Get the cells from the range
                (setq myCells(vlax-get-property myRange 'Cells))

                ;;;--- Get the addresses of the top left and bottom right cells
                (setq myAddress(JXCL-get-address myRange myCells 2 3 1))

                ;;;--- Strip off the workbook and worksheet names
                (setq myAddress(substr myAddress (+ 3 (vl-string-search "!" myAddress))))

                ;;;--- Get the starting row number of the range
                (setq 1stRow(fix(atof myAddress)))

                ;;;--- Get the starting column number of the range
                (setq 1stCol(fix(atof (substr myAddress (+ (strlen (itoa 1stRow)) 2)))))

                ;;;--- Strip off the first row and column from the address string
                (setq myAddress(substr myAddress (+ 3 (vl-string-search ":" myAddress))))

                ;;;--- Get the ending row number of the range
                (setq LstRow(fix(atof myAddress)))

                ;;;--- Get the ending column number of the range
                (setq LstCol(fix(atof (substr myAddress (+ (strlen (itoa LstRow)) 2)))))

                ;;;--- Get the data by row into a list
                (princ "\n Getting Excel Data...")

                ;;;--- Get each cells value and store it in a list
                (setq dataList (getRangeCells 1stRow 1stCol LstRow LstCol))

                (princ " Done.")
              
                (princ "\n Exiting EXCEL...")
                (princ)

                ;;;--- Shut Excel down
                (cond
                  (
                    (not(vlax-object-released-p myApp))
                    (vlax-invoke-method myApp 'QUIT)
                    (vlax-release-object myApp)
                  )
                )

                (princ " Done.")
              )
            )                             
          )
          (alert "Could not start Excel application!")
        )                                 
      )
      (alert "Could not Find the Excel Object Library!\nSee the XL HELP section on my Web site.")
    )

    ;;;--- Return the list of data from Excel
    dataList
  ) 




  ;;;-----------------------------------------------------------------------------------------------------




  ;;;--- Function to change an attributes value
  
  (defun repAttVal(en tagName newVal)

    ;;;--- Get the DXF group codes of the entity
    (setq enlist(entget en))

    ;;;--- Get the name of the block
    (setq blkName(cdr(assoc 2 enlist)))

    ;;;--- Check to see if the block's attribute flag is there
    (if(cdr(assoc 66 enlist))
      (progn
         
        ;;;--- Get the entity name
        (setq en(entnext en))

        ;;;--- Get the entity dxf group codes
        (setq enlist(entget en))

        ;;;--- Get the type of block
        (setq blkType (cdr(assoc 0 enlist)))

        ;;;--- If group 66 then there are attributes nested inside this block
        (setq group66(cdr(assoc 66 enlist)))

        ;;;--- Loop while the type is an attribute or a nested attribute exist
        (while(or (= blkType "ATTRIB")(= group66 1))
           
          ;;;--- Get the block type 
          (setq blkType (cdr(assoc 0 enlist)))

          ;;;--- Get the block name         
          (setq entName (cdr(assoc 2 enlist)))

          ;;;--- Check to see if this is the first attribute
          (if(= blkType "ATTRIB")
            (progn

              ;;;--- Get the attribute tag
              (setq attTag(cdr(assoc 2 enlist)))

              ;;;--- Get the value of the attribute
              (setq attVal(cdr(assoc 1 enlist))) 

              ;;;--- If this tag matches our search tag name
              (if(= (strcase tagName)(strcase attTag))
                (progn

                  ;;;--- Replace the attribute's value
                  (setq enlist(subst (cons 1 newVal)(assoc 1 enlist)enlist))
                  (entmod enlist)
                  (entupd en)
                )
              )
            )
          )
          ;;;--- Get the next sub-entity or nested entity as you will
          (setq en(entnext en))

          ;;;--- Get the dxf group codes of the next sub-entity
          (setq enlist(entget en))

          ;;;--- Get the block type of the next sub-entity
          (setq blkType (cdr(assoc 0 enlist)))

          ;;;--- See if the dxf group code 66 exist.  if so, there are more nested attributes
          (setq group66(cdr(assoc 66 enlist)))

        )
      )
    )
  )




  ;;;---------------------------------------------------------------------------------------





  ;;;--- Function to save the variables from the dialog box

  (defun saveVars()
    (princ "\n Saving variables from dialog box...")
    (setq fileName(get_tile "filename"))
    (setq decPlaces(nth (atoi(get_tile "decplaces")) decList))
    (setq dNode(get_tile "dnode"))
    (setq dCirc(get_tile "dcirc"))
    (setq dLine(get_tile "dline"))
    (setq dBloc(get_tile "dbloc"))
    (setq dAttr(get_tile "dattr"))
    (setq dNote(get_tile "dnote"))
    (setq layerIndex(atoi(get_tile "layers")))
    (setq layerName(nth layerIndex layerList))
    (setq cDia(distof(get_tile "cdia")))
    (setq blkIndex(atoi(get_tile "blocks")))
    (if blkList(setq blkName(nth blkIndex blkList)))
    (setq xtag(get_tile "xtag"))
    (setq ytag(get_tile "ytag"))
    (setq ztag(get_tile "ztag"))
    (princ " Done.")
  )




  ;;;---------------------------------------------------------------------------------------



  (defun xyz_help()

    (setq helpList(list))
    (setq helpList
      (list 
        "ImportXYZ - Help"
        " "
        "   Select a file and press the GETXYZ COORDS button.  The first 50"
        "   coordinates will show up in the list box for a sanity check on"
        "   the results.  Next, select the type of action you want to perform."
        "   When you are ready, press the OKAY button." 
        " "
        " FILENAME :"
        " "
        "   The name of the file to open.  Can be an ascii text file that is delimited"
        "   by spaces, commas, tabs, or number of characters.   Can also be an EXCEL"
        "   file.  Press the BROWSE FOR FILENAME button to select from a directory."
        " "
        " "
        " DECIMAL PLACES TO DISPLAY :"
        " "
        "   This will effect the decimal places shown in the list box only.  This will"
        "   NOT alter the actual point in AutoCAD."
        " "
        " "
        " GETXYZ COORDS : "
        " "
        "   Press this button to open the file and import the coordinates.  The"
        "   first 50 results will be shown in the list box for a sanity check."
        " "
        " "
        " DRAW A NODE :"
        "   This will draw an autocad node entity [point] on each coordinate."
        " "
        " "
        " DRAW A CIRCLE :"
        "   This will draw an autocad circle entity on each coordinate."
        " "
        " "
        " DRAW LINES :"
        "   This will draw a line from coordinate to coordinate in order."
        " "
        " "
        " INSERT A BLOCK :"
        "   This will insert a block entity on every coordinate with the option"
        "   to select the block from a directory."
        " "
        " "
        " EDIT ATTRIBUTE :"
        "   This will insert a block entity on every coordinate with the options"
        "   to select the block from a directory and input the tag names to edit."
        "   This will also change the value of the attribute to match the xyz coords."
        "   Try it using the included example block IMPORTXYZATT.dwg."
        " "
        " "
        " INSERT EXCEL NOTE :"
        "   This function will write the data from the first column in excel"
        "   on each coordinate with a leader.  ( If each row contains four items,"
        "   the program assumes the first column of data is a note. )  The program"
        "   will use the default text style and height."
        " "
        " ---------------------------------------------------------------------------"
        " " 
        " If you still need help, please email jps@jefferypsanders.com with any"
        " questions."
        " "
        " "
      )
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;;--- We need to load a dialog box to show the help information
    ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;--- See if the definition is already loaded
    (if (not (new_dialog "XYZ_HELP" dcl_id))
      (progn 
        (alert "The IMPORTXYZ.dcl file could not be found.")
        (exit)
      )
    )    

    ;;;--- Add the layer list to the dialog box drop down list
    (start_list "helplist")
    (mapcar 'add_list helpList)
    (end_list)

    ;;;--- Set up an action event
    (action_tile "cancel" "(done_dialog)")

    ;;;--- Display the dialog box
    (start_dialog)
  )


  ;;;---------------------------------------------------------------------------------------



  ;;;--- Function to get a file name to extract xyz coords from

  (defun getFileName()

    ;;;--- Clear the data from the dialog list-box
    (start_list "xyz")
    (add_list "")
    (end_list)

    ;;;--- Clear the main data list
    (setq dataList(list))

    ;;;--- Get a new file name
    (if(setq filN(getfiled "Select File to Import From" "" "*" 16))

      ;;;--- Display the filename in the dialog box
      (set_tile "filename" filN)
    )
  )



  ;;;---------------------------------------------------------------------------------------



  ;;;--- Function to save the setting from the secondary dialog box

  (defun saveVars2()
    (setq charNum nil)
    (cond
      ((= (get_tile "type7") "1")(setq delimType 7))
      ((= (get_tile "type8") "1")(setq delimType 8))
      ((= (get_tile "type9") "1")(progn(setq charNum(atoi(get_tile "charnum")))(setq delimType 9)))
    )

    ;;;--- Return the delimiter type
    delimType
  )



  ;;;---------------------------------------------------------------------------------------



  ;;;--- Function to enable or disable the character number edit box

  (defun modeCharNum(a)
    (if(= a 1)
      (mode_tile "charnum" 1)
      (mode_tile "charnum" 0)
    )
  )      



  ;;;---------------------------------------------------------------------------------------

  


  ;;;--- Function to determine which type of delimiter to use

  (defun findDelimiter()

    ;;;--- Start off with no delimiter
    (setq delimType nil)

    ;;;--- Open the file name to read a line of data out for testing
    (if(setq fil(open fileName "r"))
      (progn

        ;;;--- Read the line of data
        (if(setq dataLine(read-line fil))
          (progn

            ;;;--- Test the data against the delimiter patterns
            (cond
              ((wcmatch dataLine (cadr pat1))(setq delimType (car pat1))) ;type 4
              ((wcmatch dataLine (cadr pat2))(setq delimType (car pat2))) ;type 3
              ((wcmatch dataLine (cadr pat3))(setq delimType (car pat3))) ;type 2
              ((wcmatch dataLine (cadr pat4))(setq delimType (car pat4))) ;type 1
              ((wcmatch dataLine (cadr pat5))(setq delimType (car pat5))) ;type 6
              ((wcmatch dataLine (cadr pat6))(setq delimType (car pat6))) ;type 5             
              (T (setq delimType 0))
            )
          )
        )

        ;;;--- Close the file
        (close fil)
      )
    )        


    ;;;--- If the delimiter was set to zero, then it is either type 7 8 or 9 
    (if(= delimType 0)
      (progn

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;
        ;;;--- We need to load a dialog box with selections for delimiter types
        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;;--- See if the definition is already loaded
        (if (not (new_dialog "FINDDELIM" dcl_id))
          (progn 
            (alert "The IMPORTXYZ.dcl file could not be found.")
            (exit)
          )
        )    

        ;;;--- Disable the number of characters text box
        (mode_tile "charnum" 1)

        ;;;--- Set up some action events
        (action_tile "type7" "(modeCharNum 1)")
        (action_tile "type8" "(modeCharNum 1)")
        (action_tile "type9" "(modeCharNum 2)")
        (action_tile "cancel" "(setq delimType nil)(done_dialog)")
        (action_tile "accept" "(saveVars2)(done_dialog)") 

        ;;;--- Display the dialog box
        (start_dialog)

      )
    )

    ;;;--- Return the delimiter type

    delimType
  )
 


  ;;;---------------------------------------------------------------------------------------



  ;;;--- Function to get the x y and or z coord from a string of data based on
  ;;;    the type of delimiter to be used
  ;;;
  ;;;    Parameters:
  ;;;
  ;;;       data = a data string
  ;;;      typeD = the delimiter type 1 through 9
  ;;;  
  ;;;         type 1 = x space y
  ;;;         type 2 = x space y space z
  ;;;         type 3 = x comma y
  ;;;         type 4 = x comma y comma z
  ;;;         type 5 = x tab y
  ;;;         type 6 = x tab y tab z
  ;;;         type 7 = x enter y enter
  ;;;         type 8 = x enter y enter z
  ;;;         type 9 = x = # of chars  y = # of chars  z = rest of characters

  (defun getxyzList(data typeD / x y z)

    (cond

      ;;;--- Delimiter type = x space y
      (
        (= typeD 1)
        (progn
          (setq x (atof data))
          (setq y (atof (substr data (+ 2 (vl-string-search " " data)))))
          (setq z 0.0)
        )
      )
      ;;;--- Delimiter type = x space y space z
      (
        (= typeD 2)
        (progn
          (setq x (atof data))
          (setq y (atof (substr data (+ 2 (vl-string-search " " data)))))
          (setq z (atof (substr data (+ 2 (vl-string-position 32 data 0 T)))))
        )
      )
      ;;;--- Delimiter type = x comma y
      (
        (= typeD 3)
        (progn
          (setq x (atof data))
          (setq y (atof (substr data (+ 2 (vl-string-search "," data)))))
          (setq z 0.0)
        )
      )
      ;;;--- Delimiter type = x comma y comma z
      (
        (= typeD 4)
        (progn
          (setq x (atof data))
          (setq y (atof (substr data (+ 2 (vl-string-search "," data)))))
          (setq z (atof (substr data (+ 2 (vl-string-position 44 data 0 T)))))
        )
      )
      ;;;--- Delimiter type = x tab y
      (
        (= typeD 5)
        (progn
          (setq x (atof data))
          (setq y (atof (substr data (+ 2 (vl-string-search (chr 9) data)))))
          (setq z 0.0)
        )
      )
      ;;;--- Delimiter type = x tab y tab z
      (
        (= typeD 6)
        (progn
          (setq x (atof data))
          (setq y (atof (substr data (+ 2 (vl-string-search (chr 9) data)))))
          (setq z (atof (substr data (+ 2 (vl-string-position 9 data 0 T)))))
        )
      )
      ;;;--- Delimiter type = x enter y
      (
        (= typeD 7)
        (progn
          (setq x (atof data))
          (setq y (atof (substr data (+ 2 (vl-string-search " " data)))))
          (setq z 0.0)
        )
      )
      ;;;--- Delimiter type = x enter y enter z
      (
        (= typeD 8)
        (progn
          (setq x (atof data))
          (setq y (atof (substr data (+ 2 (vl-string-search " " data)))))
          (setq z (atof (substr data (+ 2 (vl-string-position 32 data 0 T)))))
        )
      )
      ;;;--- Delimiter type = # of characters
      (
        (= typeD 9)
        (progn
          (setq x (atof (substr data 1 charNum)))
          (setq y (atof (substr data (+ charNum 1) charNum)))
          (setq z (atof (substr data (+ charNum charNum 2) charNum)))
        )
      )
    )

    ;;;--- Return a list of real numbers

    (list x y z)
  )





  ;;;---------------------------------------------------------------------------------------




  ;;;--- Function to get the data from a file  
  ;;;
  ;;;    The type of files can be:
  ;;;
  ;;;    An ascii file delimited with a space, comma, or tab.
  ;;;    An ascii file delimited by the enter key or new line character.
  ;;;    An EXCEL spread sheet.

  (defun getData()

    ;;;--- If the file name in the dialog text box does not exist...
    (if(= "" (setq fileName(get_tile "filename")))

      ;;;--- Alert the user to enter a file name
      (alert "Enter a file name first!")

      ;;;--- Else the file name exist...
      (progn

        ;;;--- Figure out what type of file it is by it's extension
        (setq fileType(strcase (substr fileName (- (+ 1(strlen fileName)) 3))))

        ;;;--- Inform the user of action...
        (princ (strcat "\n Opening " fileType " file..."))

        ;;;--- If the file is not an EXCEL file...
        (if(/= fileType "XLS")
          (progn

            (princ "\n Determining delimiter...")

            ;;;--- Go find the type of delimeter to use on the ascii file
            (if(setq delimType(findDelimiter))
              (progn

                (princ " Done.")

                (princ "\n Counting line numbers...")
                ;;;--- Open the file to read and count the number of lines
                ;;;    We will need this for the progress bar
                (if(setq filt(open fileName "r"))
                  (progn

                    ;;;--- Set up a line counter
                    (setq linecnt 0)

                    ;;;--- While a next line exist, count the lines inside the file
                    (while (read-line filt)(setq linecnt(+ linecnt 1)))

                    ;;;--- Close the file 
                    (close filt)

                    (princ " Done.")

                    ;;;--- Open the file again for reading
                    (setq fil(open fileName "r"))

                    ;;;--- Set the main data list to nil and a line counter to zero
                    (setq dataList(list) thisLine 0)

                    (princ "\n Extracting coords...")

                    ;;;--- Read the file as long as there are lines to read
                    (while (setq dataLine(read-line fil)) 

                      ;;;--- Increment the line counter
                      (setq thisLine(+ thisLine 1))

                      ;;;--- Update the progress bar with a percentage complete
                      (updateProgressBar (/ (float thisLine) lineCnt))

                      ;;;--- If the delimiter type is 7  [ x enter y enter ]...
                      (if(= delimType 7)

                        ;;;--- If the next line is available to read to get the y coord
                        (if(setq a(read-line fil))
     
                          ;;;--- Add the y coord to the data line
                          (setq dataLine(strcat dataLine " " a))

                          ;;;--- Else the file has ended and there is no y coord to be read
                          (progn

                            ;;;--- Let the user know you are going to substitute in a fake y coord
                            ;;;    instead of crashing the program
                            (alert "The data file does not have a finishing Y-Coordinate.\nAdding -1 for Y-Coord!")

                            ;;;--- Add the fake y coord to the data line
                            (setq dataLine(strcat dataline " -1.0"))
                          )
                        )
                      )

                      ;;;--- If the delimiter is type 8 [ x enter y enter z enter ]...
                      (if(= delimType 8)

                        ;;;--- If the next line is available to read to get the y coord...
                        (if(setq a(read-line fil))

                          ;;;--- If the next line is available to read to get the z coord... 
                          (if(setq b(read-line fil))                    

                            ;;;--- Add the y and z coord to the data line   
                            (setq dataLine(strcat dataLine " " a " " b))

                            ;;;--- Else, the z coord was not available. Let the user know ...
                            (progn

                              ;;;--- Alert the user we are going to substitute in a fake z coord 
                              ;;;    instead of crashing the program
                              (alert "The data file does not have a finishing Y-Coordinate.\nAdding -1 for Y-Coord!")   

                              ;;;--- Add the z coord to the data line
                              (setq dataLine(strcat dataLine " " a " -1.0"))
                            )
                          )

                          ;;;--- Else there was not y coord available to read...
                          (progn

                            ;;;--- Alert the user we are going to substitute in a fake y & z coord 
                            ;;;    instead of crashing the program                   
                            (alert "The data file does not have a finishing Y or Z Coordinate.\nAdding -1 for Y & Z Coord!")   

                            ;;;--- Add the fake y and z coord to the data line
                            (setq dataLine(strcat dataLine " " "-1.0 -1.0"))
                          )
                        )
                      )       


                      ;;;--- Send the data line to the function to create a point list from a string
                      ;;;    based on the type of delimiter required to read the string
                      (setq xyzList(getxyzList dataLine delimType))

                      ;;;--- Add the xyz point list to the main data list
                      (setq dataList(append dataList (list xyzList)))
                    )

                    (princ " Done.")

                    ;;;--- We are finally finished reading the file, update the progress bar
                    (updateProgressBar 100)

                    ;;;--- Close the file 
                    (close fil)

                    (princ (strcat"\n Closed " fileType " file."))

                    ;;;--- Display the first 50 lines of data in the dialog box
                    (displayXYZ)

                  )

                  ;;;--- Else we could not open the file for reading
                  (alert "Could not open file!\n\nMake sure it is not opened or protected!")       
                )
              )
            ) 
          )

          ;;;--- Else, this must be an EXCEL file
          (progn

            ;;;--- Send the EXCEL file name to the XL_GET routine to extract the xyz data
            (if(setq XList(XL_GET fileName)) 
              (progn

                ;;;--- Clear the main data list
                (setq dataList(list))     

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;;;
                ;;;--- Cycle through the data returned from the EXCEL file
                ;;;
                ;;;    Do this carefully.  The excel file could contain a lot of data
                ;;;    so we do not want to have two list containing the data.  
                ;;;
                ;;;    XList contains the data retrieved from EXCEL.
                ;;;      Data in the form of:
                ;;;        ( (rowNum xString) (rowNum yString) )
                ;;;                         or
                ;;;        ( (rowNum xString) (rowNum yString) (rowNum zString) )
                ;;;
                ;;;    dataList will contain the data converted to a point list.
                ;;;      Data in the form of:
                ;;;         (x y z)  - z will be substituted in if necessary as 0.0
                ;;;
                ;;;    I need to convert the data from XList [ row xyzString ] into a
                ;;;    point list containing the x y and z coords [ "x y z" ] for
                ;;;    the dataList.  
                ;;;
                ;;;    So I will take the first row,
                ;;;    whether it contains two items or three and make one point list
                ;;;    out of it containing an x y and z coord (possibly fake z coord),
                ;;;    then add it to the datalist.  At the same time I will remove the
                ;;;    row from the XList.  This way there will not be two list of 
                ;;;    data that are possibly enormous in size.
                ;;;
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 

                ;;;--- While there are items left in the XList              
                (while XList

                  ;;;--- Get the first item in the XList  [ row xString ]
                  (setq a(car XList))

                  ;;;--- Get the row number
                  (setq row(car a))

                  ;;;--- Start building a list of strings containing the x coord
                  (setq data(list (cdr a)))

                  ;;;--- Remove the x coord from the XList
                  (setq XList(cdr XList))

                  ;;;--- While the row number matches something in the XList...
                  (while (assoc row XList)

                    ;;;--- Get the y and z coords and add them to the list
                    (setq data(append data (list(cdr(car XList)))))

                    ;;;--- If you add something to the data list, remove it from the XList
                    (setq XList(cdr XList))
                  )

                  ;;;--- If a z coord was not found...
                  (if(= (length data) 2)

                    ;;;--- Add a fake z coord in
                    (setq data(append data (list 0.0)))
                  )

                  ;;;--- Add the xyz list to the main data list
                  (setq dataList(append dataList (list data)))
                )

                ;;;--- Display the first 50 results in the dialog box
                (displayXYZ)
              )
            )
          )  
        )
      )
    )
  )




  ;;;---------------------------------------------------------------------------------------



  ;;;--- Function to display the x y z coords in the dialog box

  (defun displayXYZ()

    ;;;--- Inform the user
    (princ "\n Displaying data...")

    ;;;--- Set a flag to ignore note data
    (setq moreData nil)

    ;;;--- Get the number of decimal places the user has chosen
    (setq decPlaces(atoi(nth (atoi(get_tile "decplaces")) decList)))

    ;;;--- If there is valid data...
    (if(> (length dataList) 0)
      (progn      

        ;;;--- Remove all rows with nil in them
        (setq tmpData(list))
        (foreach a dataList
          (if(not(member nil a))
            (setq tmpData(append tmpData(list a)))
          )
        )
        (setq dataList tmpData tmpData nil)

        
        ;;;--- Check to see if we have more data in the list than x y & z, a note perhaps
        (if(> (length (car dataList)) 3)
          (setq moreData T)
        )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;
        ;;;--- Use the first 500 items to find the longest integer to display
        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;;--- Set the longest integer variable up
        (setq longestInt 0)

        ;;;--- Set up a counter so we don't exceed 500 values  [ time issue ]
        (setq cnt 0)

        ;;;--- While we do not exceed the 500th item...
        (while(< cnt 500)

          ;;;--- And there are items left in the dataList
          (if(> (length dataList) cnt)
            (progn

              ;;;--- Get the nth item from the dataList
              (setq a(nth cnt dataList))

              (if moreData
                (progn
                  ;;;--- Get the integer part of the x y and z coords as a string
                  (setq x(itoa(fix(nth 1 a))))
                  (setq y(itoa(fix(nth 2 a))))
                  (setq z(itoa(fix(nth 3 a))))
                )
                (progn
                  ;;;--- Get the integer part of the x y and z coords as a string
                  (setq x(itoa(fix(nth 0 a))))
                  (setq y(itoa(fix(nth 1 a))))
                  (setq z(itoa(fix(nth 2 a))))
                )
              )

              ;;;--- Check the "integer" string lengths for a longer one
              (if(> (strlen x) longestInt)(setq longestInt(strlen x)))
              (if(> (strlen y) longestInt)(setq longestInt(strlen y)))
              (if(> (strlen z) longestInt)(setq longestInt(strlen z)))
            )
          )

          ;;;--- Increment the counter to get the next item
          (setq cnt(+ cnt 1))
        )


        ;;;--- Set up a string to display as a header in the dialog list box
        (setq x "X" y "Y" z "Z")
        (while(< (strlen x) (+ longestInt 2 decPlaces))(setq x(strcat x " ")))
        (while(< (strlen y) (+ longestInt 2 decPlaces))(setq y(strcat y " ")))
        (while(< (strlen z) (+ longestInt 2 decPlaces))(setq z(strcat z " ")))

        ;;;--- Add the header to the dialog list box       
        (start_list "xyz")
        (add_list (strcat X Y Z))
        (end_list)

        ;;;--- Start the routine to add the first 50 xyz coords to the list box
        (start_list "xyz" 2)

        ;;;--- Set up a counter to make sure we do not display more than 50 [time issue]
        (setq cnt 0)

        ;;;--- While there are items to display and we have not exceeded 50
        (while(and (> (length dataList) cnt)(< cnt 50))

          ;;;--- Get the nth item from the data list
          (setq a(nth cnt dataList))

          (if moreData
            (progn
              (setq x(rtos (nth 1 a) 2 decPlaces))
              (setq y(rtos (nth 2 a) 2 decPlaces))
              (setq z(rtos (nth 3 a) 2 decPlaces))
            )
            (progn
              ;;;--- Get the xy and z coords as strings
              (setq x(rtos (nth 0 a) 2 decPlaces))
              (setq y(rtos (nth 1 a) 2 decPlaces))
              (setq z(rtos (nth 2 a) 2 decPlaces))
            )
          )

          ;;;--- Pad the xy & z if necessary for display purposes
          (while(< (strlen x) (+ longestInt 1 decPlaces))(setq x(strcat " " x)))
          (while(< (strlen y) (+ longestInt 1 decPlaces))(setq y(strcat " " y)))
          (while(< (strlen z) (+ longestInt 1 decPlaces))(setq z(strcat " " z)))

          ;;;--- Add the xyz coord to the dialog list box
          (add_list (strcat x " " y " " z))

          ;;;--- Increment the counter to get the next point
          (setq cnt(+ cnt 1))
        )

        ;;;--- Finalize the list box
        (end_list)

        ;;;--- Enable tiles
        (mode_tile "dnode" 0)
        (mode_tile "dcirc" 0)
        (mode_tile "dline" 0)
        (mode_tile "dbloc" 0)
        (mode_tile "dattr" 0)
        (mode_tile "dnote" 0)
        (mode_tile "layers" 0)
        (mode_tile "blocks" 1)
        (mode_tile "getblkname" 1)
        (mode_tile "cdia" 1)
        (mode_tile "xtag" 1)
        (mode_tile "ytag" 1)
        (mode_tile "ztag" 1)        
      )
    )
    (princ " Done.")
  )

  


  ;;;---------------------------------------------------------------------------------------


  ;;;--- Functions to enable and disable tiles as necessary

  (defun modeDnode()
    (mode_tile "layers" 0)
    (mode_tile "cdia" 1)
    (mode_tile "blocks" 1)
    (mode_tile "getblkname" 1)
    (mode_tile "xtag" 1)
    (mode_tile "ytag" 1)
    (mode_tile "ztag" 1)
  )

  (defun modeDcirc()
    (mode_tile "layers" 0)
    (mode_tile "cdia" 0)
    (mode_tile "blocks" 1)
    (mode_tile "getblkname" 1)
    (mode_tile "xtag" 1)
    (mode_tile "ytag" 1)
    (mode_tile "ztag" 1)
  )

  (defun modeDline()
    (mode_tile "layers" 0)
    (mode_tile "cdia" 1)
    (mode_tile "blocks" 1)
    (mode_tile "getblkname" 1)
    (mode_tile "xtag" 1)
    (mode_tile "ytag" 1)
    (mode_tile "ztag" 1)
  )

  (defun modeDbloc()
    (mode_tile "layers" 1)
    (mode_tile "cdia" 1)
    (mode_tile "blocks" 0)
    (mode_tile "getblkname" 0)
    (mode_tile "xtag" 1)
    (mode_tile "ytag" 1)
    (mode_tile "ztag" 1)
  )

  (defun modeDattr()
    (mode_tile "layers" 1)
    (mode_tile "cdia" 1)
    (mode_tile "blocks" 0)
    (mode_tile "getblkname" 0)
    (mode_tile "xtag" 0)
    (mode_tile "ytag" 0)
    (mode_tile "ztag" 0)
  )

  (defun modeDnote()
    (mode_tile "layers" 0)
    (mode_tile "cdia" 1)
    (mode_tile "blocks" 1)
    (mode_tile "getblkname" 1)
    (mode_tile "xtag" 1)
    (mode_tile "ytag" 1)
    (mode_tile "ztag" 1)
  )



  ;;;---------------------------------------------------------------------------------------



  ;;;--- Function to update the progress bar 
  ;;;
  ;;;    Parameter a = percentage complete   [ a = 0.50 = Fifty percent complete ]

  (defun upDateProgressBar(a)
    (setq width (dimx_tile "progbar") height (dimy_tile "progbar"))
    (start_image "progbar")
    (fill_image 0 0 width height 250)     ;background color = 250 = black
    (setq x (fix(* width a)))             ;fill the used area [ width * percentage used ]
    (fill_image 0 0 x height 1)           ;foreground color = 1 = red
    (end_image)
  )




  ;;;---------------------------------------------------------------------------------------




  (defun getAttTags(en)

    ;;;--- Set up a list to hold the tag names
    (setq attList(list))

    ;;;--- Get the DXF group codes of the entity
    (setq enlist(entget en))

    ;;;--- Get the name of the block
    (setq blkName(cdr(assoc 2 enlist)))

    ;;;--- Check to see if the block's attribute flag is there
    (if(cdr(assoc 66 enlist))
      (progn
         
        ;;;--- Get the entity name
        (setq en(entnext en))

        ;;;--- Get the entity dxf group codes
        (setq enlist(entget en))

        ;;;--- Get the type of block
        (setq blkType (cdr(assoc 0 enlist)))

        ;;;--- If group 66 then there are attributes nested inside this block
        (setq group66(cdr(assoc 66 enlist)))

        ;;;--- Loop while the type is an attribute or a nested attribute exist
        (while(or (= blkType "ATTRIB")(= group66 1))
           
          ;;;--- Get the block type 
          (setq blkType (cdr(assoc 0 enlist)))

          ;;;--- Get the block name         
          (setq entName (cdr(assoc 2 enlist)))

          ;;;--- Check to see if this is the first attribute
          (if(= blkType "ATTRIB")
            (progn

              ;;;--- Get the attribute tag
              (setq attTag(cdr(assoc 2 enlist)))

              ;;;--- Add the tag to the tag list
              (setq attList(append attList (list attTag)))

            )
          )
          ;;;--- Get the next sub-entity or nested entity as you will
          (setq en(entnext en))

          ;;;--- Get the dxf group codes of the next sub-entity
          (setq enlist(entget en))

          ;;;--- Get the block type of the next sub-entity
          (setq blkType (cdr(assoc 0 enlist)))

          ;;;--- See if the dxf group code 66 exist.  if so, there are more nested attributes
          (setq group66(cdr(assoc 66 enlist)))

        )
      )
    )
    
    ;;;--- Return the tag list
    attList
  )





  ;;;---------------------------------------------------------------------------------------

  



  ;;;--- Function to get a block name from file and it's attribute tags

  (defun getBlkFromFile()

    ;;;--- If the user selects a block...
    (if(setq blk(getfiled "Select Block" "" "dwg" 16))
      (progn

        ;;;--- See if the block name already exist
        (if(not(member (vl-filename-base blk) blkList))
          (progn

            ;;;--- Add the block name to the block list
            (setq blkList(append blkList (list blk)))

            ;;;--- Add the block name to the block popup list in the dialog box
            (start_list "blocks")
            (mapcar 'add_list blkList)
            (end_list)

            ;;;--- Set the currently selected block to the item added
            (set_tile "blocks" (itoa (- (length blkList) 1)))

          )
        
          ;;;--- Else the block exist so, select it in the dialog block popup list
          (progn

            (setq a(member (vl-filename-base blk) blkList))
            (setq blkIndex (- (length blkList) (length a)))
            (set_tile "blocks" (itoa blkIndex))

          )
        )
      ) 
    )
  )



  ;;;---------------------------------------------------------------------------------------





  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                ;;;
  ;;;  888       888          888           8888888      8888   888  ;;;
  ;;;  8888     8888         88888            888        88888  888  ;;;
  ;;;  88888   88888        888 888           888        888888 888  ;;;
  ;;;  888888 888888       888   888          888        888 888888  ;;;
  ;;;  888 88888 888      88888888888         888        888  88888  ;;;
  ;;;  888  888  888     888       888      8888888      888   8888  ;;;
  ;;;                                                                ;;;
  ;;;                                                                ;;;
  ;;;           888            888888888        888888888            ;;;
  ;;;          88888           888   888        888   888            ;;;
  ;;;         888 888          888   888        888   888            ;;;
  ;;;        888   888         888888888        888888888            ;;;
  ;;;       88888888888        888              888                  ;;;
  ;;;      888       888       888              888                  ;;;
  ;;;                                                                ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;--- Turn the command echo off
  (setvar "cmdecho" 0)

  ;;;--- Set the main list to nil
  (setq dataList (list))

  ;;;--- Build match patterns for delimiter checking
  (setq pat1(list 4 "*`,*`,*"))
  (setq pat2(list 3 "*`,*"))
  (setq pat3(list 2 "* * *"))
  (setq pat4(list 1 "* *"))
  (setq pat5(list 6 "*\t*\t*"))
  (setq pat6(list 5 "*\t*"))

  ;;;--- Turn the note flag to off [ no notes attached to data ]
  (setq moreData nil)

  ;;;--- Build a list to hold the layer names
  (setq layerList(list))

  ;;;--- Get the first layer name in the drawing
  (if(setq layN(tblnext "LAYER" T))
    (progn
 
      ;;;--- Save the layer name in the list
      (setq layerList(append layerList (list (cdr (assoc 2 layN)))))

      ;;;--- Loop through all of the remaining layers
      (while(setq layN(tblnext "LAYER"))
        (setq layerList(append layerList (list (cdr(assoc 2 layN)))))
      )
    )
  )

  ;;;--- Build a list to hold the block names
  (setq blkList(list))

  ;;;--- Get the first block name in the drawing
  (if(setq blkN(tblnext "BLOCK" T))
    (progn
 
      ;;;--- Save the block name in the list
      (setq blkList(append blkList (list (cdr (assoc 2 blkN)))))

      ;;;--- Loop through all of the remaining blocks
      (while(setq blkN(tblnext "BLOCK"))
        (setq blkList(append blkList (list (cdr(assoc 2 blkN)))))
      )
    )
  )


  ;;;--- Build a list of valid decimal places
  (setq decList(list "0" "1" "2" "3" "4" "5" "6" "7" "8"))
    
  ;;;--- Load the dialog box file
  (setq dcl_id (load_dialog "IMPORTXYZ.dcl"))

  ;;;--- See if the definition is already loaded
  (if (not (new_dialog "IMPORTXYZ" dcl_id))
    (progn 
      (alert "The IMPORTXYZ.dcl file could not be found.")
      (exit)
    )
  )

  ;;;--- Add the layer list to the dialog box drop down list
  (start_list "layers")
  (mapcar 'add_list layerList)
  (end_list)

  ;;;--- Add the block list to the dialog box drop down list
  (start_list "blocks")
  (mapcar 'add_list blkList)
  (end_list)

  ;;;--- Add the decimal places list to the dialog box drop down list
  (start_list "decplaces")
  (mapcar 'add_list decList)
  (end_list)

  (setq width (dimx_tile "progbar") height (dimy_tile "progbar"))
  (start_image "progbar")
  (fill_image 0 0 width height 250)  ;250 = AutoCAD black
  (end_image)

  ;;;--- Disable tiles
  (mode_tile "dnode" 1)
  (mode_tile "dcirc" 1)
  (mode_tile "dline" 1)
  (mode_tile "dbloc" 1)
  (mode_tile "dattr" 1)
  (mode_tile "dnote" 1)
  (mode_tile "layers" 1)
  (mode_tile "cdia" 1)
  (mode_tile "blocks" 1)
  (mode_tile "getblkname" 1)
  (mode_tile "xtag" 1)
  (mode_tile "ytag" 1)
  (mode_tile "ztag" 1)

  ;;;--- If an action event occurs, do this function
  (action_tile "help" "(xyz_help)")
  (action_tile "dnode" "(modeDnode)")
  (action_tile "dcirc" "(modeDcirc)")
  (action_tile "dline" "(modeDline)")
  (action_tile "dbloc" "(modeDbloc)")
  (action_tile "dattr" "(modeDattr)")
  (action_tile "dnote" "(modeDnote)")
  (action_tile "getblkname" "(getBlkFromFile)")
  (action_tile "getfile" "(getFileName)")
  (action_tile "getdata" "(getData)")
  (action_tile "decplaces" "(displayXYZ)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "accept" "(setq ddiag 2)(saveVars)(done_dialog)") 

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- Unload the dialog box from memory, it is not need any longer
  (unload_dialog dcl_id)

  ;;;--- If the cancel button was pressed - display message
  (if (= ddiag 1)
    (setq exitMessage "\n IMPORTXYZ Cancelled. \n ")
  )

  ;;;--- If the "Create" button was pressed
  (if (= ddiag 2)
    (progn

      ;;;--- Save the current snap settings
      (setq oldSnap(getvar "osmode"))

      ;;;--- Turn the osnaps off
      (setvar "osmode" 0)

      ;;;--- Save the current layer
      (setq oldLay(getvar "clayer"))

      ;;;--- Set the layer
      (setvar "clayer" layerName)            


      ;;;--- Run a series of test based on the item chosen in the dialog box       
      (cond

        ;;;--- If the user wanted to draw a node on each point
        ( 
          (= dNode "1")
          (progn

            ;;;--- Display a message on the command line
            (princ "\n Placing nodes...")

            ;;;--- Cycle through each point in the list
            (foreach a dataList

              ;;;--- If the point is prefixed with a note 
              (if moreData

                ;;;--- Draw a node on the point
                (command "point" (cdr a))

                ;;;--- Else, draw a node on the point
                (command "point" a)
              )
            )

            (princ " Done.")
          
          )
        )

        ;;;--- If the user wanted to draw a circle on each point
        (
          (= dCirc "1")
          (progn

            ;;;--- Display a message on the command line
            (princ "\n Placing circles...")

            ;;;--- Cycle through each point in the list                     
            (foreach a dataList

              ;;;--- If the point is prefixed with a note 
              (if moreData

                ;;;--- Draw a node on the point
                (command "circle" (cdr a) "D" cDia)


                ;;;--- Else, draw the circle
                (command "circle" a "D" cDia)
              )
            )

            (princ " Done.")
                         
          )
        )

        ;;;--- If the user wanted to draw lines to each point
        (
          (= dLine "1")
          (progn


            ;;;--- Display a message on the command line
            (princ "\n Drawing lines...")

            ;;;--- If the point is prefixed with a note 
            (if moreData

              ;;;--- Start the line command on the first point
              (command "line" (cdr(car dataList)))

              ;;;--- Else, start the line command on the first point like this
              (command "line" (car dataList))
            )

         
            ;;;--- Cycle through each point in the list except the first [ already used ]
            (foreach a (cdr dataList)

              ;;;--- If the point is prefixed with a note 
              (if moreData

                ;;;--- Send the point to the line command
                (command (cdr a))

                ;;;--- Else, send the point to the line command like this
                (command a)
              )
            )

            ;;;--- End the line command
            (command)

            (princ " Done.")
                      
          )
        )

        ;;;--- If the user wanted to insert a block on each point
        (
          (= dBloc "1")
          (progn

            ;;;--- Set up a counter to keep the user informed
            (setq cnt 0)

            ;;;--- Cycle through each point in the list
            (foreach a dataList

              ;;;--- Increment the counter
              (setq cnt(+ cnt 1))

              ;;;--- Inform the user of progress
              (princ "\n Inserting block # ")(princ cnt)

              ;;;--- If the point is prefixed with a note 
              (if moreData

                ;;;--- Insert the block using the dimscale and rotation of zero
                (command "-insert" blkName (cdr a) (getvar "dimscale") "" 0)

                ;;;--- Else, insert the block using the dimscale and rotation of zero
                (command "-insert" blkName a (getvar "dimscale") "" 0)
              )
            )

            (princ "\n Finished inserting ")(princ cnt)(princ " blocks.")

          )
        )

        ;;;--- If the user wanted to insert a block on each point and upate attributes in the block 
        (
          (= dAttr "1")
          (progn
         
            ;;;--- Save the current state of the attribute request system variable       
            (setq oldAttReq(getvar "attreq"))

            ;;;--- Turn the attribute request off
            (setvar "attreq" 0)

            ;;;--- Set up a counter to keep the user informed
            (setq cnt 0)


            ;;;--- Cycle through each point in the list
            (foreach a dataList

              ;;;--- Increment the counter
              (setq cnt(+ cnt 1))

              ;;;--- Inform the user of progress
              (princ "\n Inserting block # ")(princ cnt)

              ;;;--- If the point is prefixed with a note 
              (if moreData

                ;;;--- Insert the block using the dimscale and rotation of zero
                (command "-insert" blkName (cdr a) (getvar "dimscale") "" 0)

                ;;;--- Else, insert the block using the dimscale and rotation of zero
                (command "-insert" blkName a (getvar "dimscale") "" 0)
              )

              ;;;--- Get the entity name of the last inserted block
              (setq en(entlast))

              ;;;--- If the point is prefixed with a note 
              (if moreData
                (progn
        
                  ;;;--- Replace the x y and z tags of the attribute
                  (repAttVal en xTag (rtos(nth 1 a)))
                  (repAttVal en yTag (rtos(nth 2 a)))
                  (repAttVal en zTag (rtos(nth 3 a)))
                )

                ;;;--- Else,               
                (progn

                  ;;;--- Replace the x y and z tags of the attribute
                  (repAttVal en xTag (rtos(nth 0 a)))
                  (repAttVal en yTag (rtos(nth 1 a)))
                  (repAttVal en zTag (rtos(nth 2 a)))
                )
              )
            )

            (princ "\n Finished inserting ")(princ cnt)(princ " blocks and updating their attributes.")

            ;;;--- Reset the attribute request to previous state
            (setvar "attreq" oldAttReq)
          )
        )

        ;;;--- If the user wanted to insert a note from excel on each point 
        (
          (= dNote "1")
          (progn

            ;;;--- Display a message on the command line
            (princ "\n Placing notes...")

            ;;;--- Cycle through each point in the list
            (foreach a dataList

              ;;;--- If the point is prefixed with a note 
              (if moreData

                ;;;--- Draw a node on the point
                (command "dim1" "Lea" (cdr a) (polar (cdr a) (* pi 0.25) (* 4.0(getvar "textsize"))) "" (car a))

                ;;;--- Else, draw a node on the point
                (command "point" a)
              )
            )

            (princ " Done.")
          )
        )
         
      )

      ;;;--- Reset the osnaps to previous state
      (setvar "osmode" oldSnap)

      ;;;--- Reset the current layer to previous state
      (setvar "clayer" oldLay) 
    )
  )

  ;;;--- Reset the command echo 
  (setvar "cmdecho" 1)

  ;;;--- Display a message
  (princ exitMessage)

  ;;;--- Suppress the last echo for a clean exit
  (princ)
)

