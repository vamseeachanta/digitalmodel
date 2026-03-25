;;;--- CAD2FILE - Send the entity information from CAD to a file.

;;;--- Copyright 2002-2003 by JefferyPSanders.com.  All rights reserved.
;;;  
;;;    This program was created to extract data from entities and send
;;;    them to a file.  The file type can be of an excel type file, a
;;;    comma delimited text file, space delimited text file, or a tab
;;;    delimited text file.

;;;    The types of entities to be selected are ARC, ATTRIB, CIRCLE,
;;;    ELLIPSE, IMAGE, INSERT (Block), LINE, LWPOLYLINE, MLINE,
;;;    MTEXT, POINT, POLYLINE, SOLID, TEXT, TRACE, and XLINE.

;;;    Revised on 5/19/03 to fix the strange MTEXT control characters
;;;    similar to this: "{\\fArial Narrow|b0|i1|c0|p34;Text Here}"

;;;    Revised on 6/24/03 to remove the MTEXT control characters such
;;;    as "\\P" "\\L" "\\O" in a function called stripIt.

;;;    Revised 12/22/03 
;;;    By adding these options:
;;;    ARC        - Added length option
;;;    CIRCLE     - Added perimeter(circumference) and area options
;;;    ELLIPSE    - Added perimeter and area options
;;;    LINE       - Added length option
;;;    LWPOLYLINE - Added length and area options
;;;    MLINE      - Added length option
;;;    POLYLINE   - Added length and area options

;;;    Revised 8/18/04
;;;    Added Handles

;;;    Revised 9/26/07
;;;    Added the ability to select objects with a window.

;;;    Revised 9/26/07
;;;    Added the ability to start the program with C2F or CAD2FILE

;;;    Revised 9/28/07
;;;    Revised bug in windowedSelection function.
;;;    Revised the window selection versus global selection functions.
;;;    Revised version to 3.0

;;;    Revised 9/12/13
;;;    Added ability for multi-language



;;;--- Function to strip commas from a string

(defun stripCommas(a)
  (setq cnt 1)
  (while (< cnt (strlen a))
     (setq chr(substr a cnt 1))
     (if(= chr ",")
       (setq a
         (strcat
           (substr a 1 (- cnt 1))
           (substr a (+ cnt 1))
         )
       )
       (setq cnt(+ cnt 1))
     )
  )
  a
)




;;;--- Function to return an arc length

(defun getArcLen(rad sAng eAng)
  (if (< sAng eAng)
    (setq angl (abs (- sAng eAng)))
    (setq angl (- (* 2 pi) (abs (- sAng eAng))))
  )
  (rtos(setq len (* angl rad))2 4)
)





;;;--- Function to return a windowed selection set

(defun windowedSelection(entType)
  (setq selPt1(getpoint "\n First corner of window: "))
  (setq selPt2(getcorner selPt1 "\n Second corner of window: "))
  (setq eset(ssget "W" selPt1 selPt2 (list(cons 0 entType))))
)





;;;--- Functions to get entity types

(defun getArcData(/ eset hdrStr en enlist entLyr entPt entRad entDia entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "ARC"))
    (setq eset(ssget "X" (list (cons 0 "ARC"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog5  "1")(setq hdrStr (strcat hdrStr "Center X" spcr "Center Y" spcr "Center Z" spcr)))
      (if(= tog10 "1")(setq hdrStr (strcat hdrStr "LineType" spcr)))
      (if(= tog11 "1")(setq hdrStr (strcat hdrStr "Radius" spcr)))
      (if(= tog12 "1")(setq hdrStr (strcat hdrStr "Diameter" spcr)))     
      (if(= tog18 "1")(setq hdrStr (strcat hdrStr "Length" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (setq entRad(rtos(cdr(assoc 40 enlist))2 4))
        (setq entDia(rtos(* 2 (cdr(assoc 40 enlist)))2 4))
        (setq entLen(getArcLen (cdr(assoc 40 enlist))(cdr(assoc 50 enlist))(cdr(assoc 51 enlist))))
        (if(not (setq entHan(cdr(assoc 5 enlist))))
          (setq entHan "OFF")
        )
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))
                         
        (setq dStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog5  "1")(setq dStr (strcat dStr entPtSTr spcr)))
        (if(= tog10 "1")(setq dStr (strcat dStr entLty spcr)))
        (if(= tog11 "1")(setq dStr (strcat dStr entRad spcr)))
        (if(= tog12 "1")(setq dStr (strcat dStr entDia spcr))) 
        (if(= tog18 "1")(setq dStr (strcat dStr entLen spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getAttData(/ eset hdrStr dataList blkCntr en enlist blkType entName entPoint entRot 
                    entX entY entZ entLay attTag attVal entSty  entClr dStr group66)
  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "INSERT"))
    (setq eset(ssget "X" (list (cons 0 "INSERT"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog1  "1")(setq hdrStr (strcat hdrStr "Name" spcr)))
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog4  "1")(setq hdrStr (strcat hdrStr "Insertion X" spcr "Insertion Y" spcr "Insertion Z" spcr)))
      (if(= tog7  "1")(setq hdrStr (strcat hdrStr "Tag" spcr)))
      (if(= tog8  "1")(setq hdrStr (strcat hdrStr "Text Value" spcr)))
      (if(= tog9  "1")(setq hdrStr (strcat hdrStr "Style" spcr)))
      (if(= tog16 "1")(setq hdrStr (strcat hdrStr "Rotation" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up some counters
      (setq blkCntr 0 cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< blkCntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset blkCntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))

        ;;;--- Get the block name         
        (setq entName (cdr(assoc 2 enlist)))
                      
        ;;;--- Check to see if the block's attribute flag is set
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
                          
              ;;;--- Check to see if this is an attribute or a block
              (if(= blkType "ATTRIB")
                (progn
                        
                  ;;;--- Get the insertion point
                  (setq entPoint(cdr(assoc 10 enlist)))
                       
                  ;;;--- Get the X, Y, and Z coordinates of the insertion point
                  (setq entX (rtos (car entPoint)   2 4))
                  (setq entY (rtos (cadr entPoint)  2 4))
                  (setq entZ (rtos (caddr entPoint) 2 4))
                       
                  ;;;--- Save the layer
                  (setq entLay(cdr(assoc 8 enlist)))
                       
                  ;;;--- Save the name of the attribute
                  (setq attTag(cdr(assoc 2 enlist)))
                      
                  ;;;--- Get the value of the attribute
                  (setq attVal(cdr(assoc 1 enlist))) 

                  ;;;--- If the comma delimited format is being used, strip the commas out of the text value
                  (if(= spcr ",")
                      (setq attVal(stripcommas attVal))
                  )

                  ;;;--- Get the style of the attribute
                  (setq entSty(cdr(assoc 7 enlist))) 

                  ;;;--- Get the rotation of the attribute
                  (if(assoc 50 enlist)
                     (setq entRot(angtos(cdr(assoc 50 enlist))0 4)) 
                     (setq entRot "0.0000")
                  )

                  ;;;--- Get the handle
                  (if(not (setq entHan(cdr(assoc 5 enlist))))
                    (setq entHan "OFF")
                  )
        
                  ;;;--- Get the color of the attribute       
                  (if(cdr(assoc 62 enlist))(setq  entClr(cdr(assoc 62 enlist)))(setq  entClr "BYLAYER"))
                  (if(= 'INT (type  entClr))(setq  entClr (itoa  entClr)))
                      
                  ;;;--- Build a data string
                  (setq dStr "")
                  (if(= tog1  "1")(setq dStr (strcat dStr entName spcr)))
                  (if(= tog2  "1")(setq dStr (strcat dStr entLay spcr)))
                  (if(= tog3  "1")(setq dStr (strcat dStr  entClr spcr)))
                  (if(= tog4  "1")(setq dStr (strcat dStr entX spcr entY spcr entZ spcr)))
                  (if(= tog7  "1")(setq dStr (strcat dStr attTag spcr)))
                  (if(= tog8  "1")(setq dStr (strcat dStr attVal spcr)))
                  (if(= tog9  "1")(setq dStr (strcat dStr entSty spcr)))
                  (if(= tog16 "1")(setq dStr (strcat dStr entRot spcr)))
                  (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
                  (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
                  (setq cntr (+ cntr 1))

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
        )
        (setq blkCntr (+ blkCntr 1))
      )
    )                    
  )
  dataList
)
(defun getCirData(/ eset hdrStr en enlist entLyr entPt entRad entDia entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "CIRCLE"))
    (setq eset(ssget "X" (list (cons 0 "CIRCLE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog5  "1")(setq hdrStr (strcat hdrStr "Center X" spcr "Center Y" spcr "Center Z" spcr)))
      (if(= tog10 "1")(setq hdrStr (strcat hdrStr "LineType" spcr)))
      (if(= tog11 "1")(setq hdrStr (strcat hdrStr "Radius" spcr)))
      (if(= tog12 "1")(setq hdrStr (strcat hdrStr "Diameter" spcr)))
      (if(= tog17 "1")(setq hdrStr (strcat hdrStr "Area" spcr)))
      (if(= tog18 "1")(setq hdrStr (strcat hdrStr "Perimeter" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (setq entRad(rtos(cdr(assoc 40 enlist))2 4))
        (setq entDia(rtos(* 2 (cdr(assoc 40 enlist)))2 4))
        (setq entLen(rtos(* pi (* 2.0 (cdr(assoc 40 enlist))))2 4))
        (setq entAre(rtos(* pi (expt (cdr(assoc 40 enlist)) 2))2 4))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
          (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        (setq dStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog5  "1")(setq dStr (strcat dStr entPtSTr spcr)))
        (if(= tog10 "1")(setq dStr (strcat dStr entLty spcr)))
        (if(= tog11 "1")(setq dStr (strcat dStr entRad spcr)))
        (if(= tog12 "1")(setq dStr (strcat dStr entDia spcr))) 
        (if(= tog17 "1")(setq dStr (strcat dStr entAre spcr)))
        (if(= tog18 "1")(setq dStr (strcat dStr entLen spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getEllData(/ eset hdrStr en enlist entLyr entPt entRad entDia entLty entClr dstr 
                    entRot maAxis miAxis)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "ELLIPSE"))
    (setq eset(ssget "X" (list (cons 0 "ELLIPSE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog5  "1")(setq hdrStr (strcat hdrStr "Center X" spcr "Center Y" spcr "Center Z" spcr)))
      (if(= tog10 "1")(setq hdrStr (strcat hdrStr "LineType" spcr)))
      (if(= tog14 "1")(setq hdrStr (strcat hdrStr "Major Axis" spcr)))
      (if(= tog15 "1")(setq hdrStr (strcat hdrStr "Minor Axis" spcr)))                
      (if(= tog16 "1")(setq hdrStr (strcat hdrStr "Rotation" spcr)))                
      (if(= tog17 "1")(setq hdrStr (strcat hdrStr "Area" spcr)))
      (if(= tog18 "1")(setq hdrStr (strcat hdrStr "Perimeter" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (setq maAxis(distance (list 0 0) (cdr(assoc 11 enlist))))
        (setq miAxis(rtos(* maAxis (cdr(assoc 40 enlist)))2 4))
        (setq maAxis(rtos maAxis 2 4))
        (command "_area" "Object" en)
        (setq entAre(rtos (getvar "area") 2 4))
        (setq entLen(rtos (getvar "perimeter")2 4))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
          (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the rotation of the attribute
        (setq entRot(angtos(angle (list 0 0)(cdr(assoc 11 enlist)))0 4)) 

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )


        (setq dStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog5  "1")(setq dStr (strcat dStr entPtSTr spcr)))
        (if(= tog10 "1")(setq dStr (strcat dStr entLty spcr)))
        (if(= tog14 "1")(setq dStr (strcat dStr maAxis spcr)))
        (if(= tog15 "1")(setq dStr (strcat dStr miAxis spcr))) 
        (if(= tog16 "1")(setq dStr (strcat dStr entRot spcr))) 
        (if(= tog17 "1")(setq dStr (strcat dStr entAre spcr))) 
        (if(= tog18 "1")(setq dStr (strcat dStr entLen spcr))) 
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr))) 
        (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)

(defun getImgData(/ eset hdrStr en enlist entLyr entPt entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "IMAGE"))
    (setq eset(ssget "X" (list (cons 0 "IMAGE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog4  "1")(setq hdrStr (strcat hdrStr "Insertion X" spcr "Insertion Y" spcr "Insertion Z" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
          (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )
                         
        (setq dStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog4  "1")(setq dStr (strcat dStr entPtSTr spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getInsData(/ eset en enlist hdrStr dataList entName entPoint entX entY entZ entLay entRot dStr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "INSERT"))
    (setq eset(ssget "X" (list (cons 0 "INSERT"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog1  "1")(setq hdrStr (strcat hdrStr "Name" spcr)))
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog4  "1")(setq hdrStr (strcat hdrStr "Insertion X" spcr "Insertion Y" spcr "Insertion Z" spcr)))
      (if(= tog16 "1")(setq hdrStr (strcat hdrStr "Rotation" spcr)))      
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))      
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up some counters
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                      
        ;;;--- Get the block name         
        (setq entName(cdr(assoc 2 enlist)))
                       
        ;;;--- Get the insertion point
        (setq entPoint(cdr(assoc 10 enlist)))
                       
        ;;;--- Get the X, Y, and Z coordinates of the insertion point
        (setq entX (rtos (car   entPoint) 2 4))
        (setq entY (rtos (cadr  entPoint) 2 4))
        (setq entZ (rtos (caddr entPoint) 2 4))
                       
        ;;;--- Save the layer
        (setq entLay(cdr(assoc 8 enlist)))
                       
        ;;;--- Get the rotation of the entity
        (if(assoc 50 enlist)
           (setq entRot(angtos(cdr(assoc 50 enlist))0 4)) 
           (setq entRot "0.0000")
        )
        
        ;;;--- Get the color of the entity       
        (if(cdr(assoc 62 enlist))(setq entClr(cdr(assoc 62 enlist)))(setq entClr "BYLAYER"))
        (if(= 'INT (type entClr))(setq entClr(itoa  entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )
                      
        ;;;--- Build a data string
        (setq dStr "")
        (if(= tog1  "1")(setq dStr (strcat dStr entName spcr)))
        (if(= tog2  "1")(setq dStr (strcat dStr entLay spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog4  "1")(setq dStr (strcat dStr entX spcr entY spcr entZ spcr)))
        (if(= tog16 "1")(setq dStr (strcat dStr entRot spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getLinData(/ eset en enlist hdrStr entLyr entPt entPtStr entEPt entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "LINE"))
    (setq eset(ssget "X" (list (cons 0 "LINE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog5  "1")(setq hdrStr (strcat hdrStr "Start X" spcr "Start Y" spcr "Start Z" spcr)))
      (if(= tog6  "1")(setq hdrStr (strcat hdrStr "End X" spcr "End Y" spcr "End Z" spcr)))
      (if(= tog10 "1")(setq hdrStr (strcat hdrStr "LineType" spcr)))
      (if(= tog16 "1")(setq hdrStr (strcat hdrStr "Rotation" spcr)))
      (if(= tog18 "1")(setq hdrStr (strcat hdrStr "Length" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (setq entEPt(cdr(assoc 11 enlist)))
        (setq entEPtStr(strcat (rtos(car entEPt)2 4)spcr(rtos(cadr entEPt)2 4)spcr(rtos(caddr entEPt)2 4)))
        (setq entRot(angtos (angle entPt entEPt)0 4))
        (setq entLen(rtos (distance entPt entEPt) 2 4))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )
                         
        (setq dStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog5  "1")(setq dStr (strcat dStr entPtSTr spcr)))
        (if(= tog6  "1")(setq dStr (strcat dStr entEPtSTr spcr)))
        (if(= tog10 "1")(setq dStr (strcat dStr entLty spcr)))
        (if(= tog16 "1")(setq dStr (strcat dStr entRot spcr)))
        (if(= tog18 "1")(setq dStr (strcat dStr entLen spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getLwpData(/ eset en enlist hdrStr dstr vStr entPt entPtStr entEPt entEPtStr
                    entLyr ptList entLty entClr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "LWPOLYLINE"))
    (setq eset(ssget "X" (list (cons 0 "LWPOLYLINE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog5  "1")(setq hdrStr (strcat hdrStr "Start X" spcr "Start Y" spcr)))
      (if(= tog6  "1")(setq hdrStr (strcat hdrStr "End X" spcr "End Y" spcr)))
      (if(= tog10 "1")(setq hdrStr (strcat hdrStr "LineType" spcr)))
      (if(= tog17 "1")(setq hdrStr (strcat hdrStr "Area" spcr)))
      (if(= tog18 "1")(setq hdrStr (strcat hdrStr "Length" spcr)))
      (if(= tog13 "1")(setq hdrStr (strcat hdrStr "Vertex X" spcr "Vertex Y" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                      
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))

        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))

        ;;;--- Get the points in a list
        (setq ptList(list))
        (foreach a enlist(if(= (car a) 10)(setq ptList(append ptList (list (cdr a))))))

        (setq entPt(car ptList))

        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)))

        (setq entEPt(car(reverse ptList)))

        (setq entEPtStr(strcat (rtos(car entEPt)2 4)spcr(rtos(cadr entEPt)2 4)))

        (command "_area" "Object" en)

        (setq entAre(rtos (getvar "area") 2 4))

        (setq entLen(rtos (getvar "perimeter") 2 4))

        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )

        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        (setq dStr "" vStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr) vStr (strcat vStr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr) vStr (strcat vStr spcr)))
        (if(= tog5  "1")(setq dStr (strcat dStr entPtSTr spcr) vStr (strcat vStr spcr spcr)))
        (if(= tog6  "1")(setq dStr (strcat dStr entEPtSTr spcr) vStr (strcat vStr spcr spcr)))
        (if(= tog10 "1")(setq dStr (strcat dStr entLty spcr) vStr (strcat vStr spcr)))
        (if(= tog17 "1")(setq dStr (strcat dStr entAre spcr) vStr (strcat vStr spcr)))
        (if(= tog18 "1")(setq dStr (strcat dStr entLen spcr) vStr (strcat vStr spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))

        (if(= tog13 "1")
           (progn
              (foreach a ptList
                (progn
                  (setq nStr(strcat "\n" vStr (rtos(car a)2 4)spcr(rtos(cadr a)2 4)))
                  (setq dataList(append dataList(list nStr)))
                )
              )
           )
        )  

        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getMliData(/ eset en enlist hdrStr dstr vStr entPt entPtStr entEPt entEPtStr
                    entLyr ptList entLty entClr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "MLINE"))
    (setq eset(ssget "X" (list (cons 0 "MLINE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog9  "1")(setq hdrStr (strcat hdrStr "Style" spcr)))
      (if(= tog5  "1")(setq hdrStr (strcat hdrStr "Start X" spcr "Start Y" spcr "Start Z" spcr)))
      (if(= tog6  "1")(setq hdrStr (strcat hdrStr "End X" spcr "End Y" spcr "Start Z" spcr)))
      (if(= tog10 "1")(setq hdrStr (strcat hdrStr "LineType" spcr)))
      (if(= tog11 "1")(setq hdrStr (strcat hdrStr "Width" spcr)))
      (if(= tog18 "1")(setq hdrStr (strcat hdrStr "Length" spcr)))
      (if(= tog13 "1")(setq hdrStr (strcat hdrStr "Vertex X" spcr "Vertex Y" spcr "Vertex Z" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))

        ;;;--- Get the data from the group codes               
        (setq entSty(cdr(assoc 2 enlist)))

        ;;;--- Get the data from the group codes               
        (setq entWth(rtos(cdr(assoc 40 enlist))2 4))

        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))

        ;;;--- Get the points in a list
        (setq ptList(list))
        (foreach a enlist(if(= (car a) 11)(setq ptList(append ptList (list (cdr a))))))
        ;;;--- Get the length
        (setq entLen 0)
        (setq spt(car ptList))
        (foreach a (cdr ptList)
          (setq entLen(+ entLen (distance spt a)))
          (setq spt a)
        )
        (setq entLen(rtos entLen 2 4))
        (setq entEPt(car(reverse ptList)))
        (setq entEPtStr(strcat (rtos(car entEPt)2 4)spcr(rtos(cadr entEPt)2 4)spcr(rtos(caddr entEPt)2 4)))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )
                         
        (setq dStr "" vStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr) vStr (strcat vStr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr) vStr (strcat vStr spcr)))
        (if(= tog9  "1")(setq dStr (strcat dStr entSty spcr) vStr (strcat vStr spcr)))
        (if(= tog5  "1")(setq dStr (strcat dStr entPtSTr spcr) vStr (strcat vStr spcr spcr spcr)))
        (if(= tog6  "1")(setq dStr (strcat dStr entEPtSTr spcr) vStr (strcat vStr spcr spcr spcr)))
        (if(= tog10 "1")(setq dStr (strcat dStr entLty spcr) vStr (strcat vStr spcr)))
        (if(= tog11 "1")(setq dStr (strcat dStr entWth spcr) vStr (strcat vStr spcr)))
        (if(= tog18 "1")(setq dStr (strcat dStr entLen) vStr (strcat vStr spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))
        (if(= tog13 "1")
           (progn
              (foreach a ptList
                (progn
                  (setq nStr(strcat "\n" vStr (rtos(car a)2 4)spcr(rtos(cadr a)2 4)spcr(rtos(caddr a)2 4)))
                  (setq dataList(append dataList(list nStr)))
                )
              )
           )
        )  
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;   function to break up an mtext entity into individual strings in a list  ;;;;;;;;;;;;;
(defun mtx(mtxtVal)
  ;;;--- Strip trailing spaces
  (while(= (substr mtxtVal (strlen mtxtVal) 1) " ")
    (setq mtxtVal(substr mtxtVal 1 (-(strlen mtxtVal)1)))
  )
  (if(= (substr mtxtVal 1 2) "{\\")
    (progn
      (setq mtxtVal(substr mtxtVal 1 (-(strlen mtxtVal)1)))
      (setq mcnt 1)
      (while(/= (substr mtxtVal mcnt 1) ";")
        (setq mcnt(+ mcnt 1))
      )
      (setq mtxtVal(substr mtxtVal (+ mcnt 1)))
    )
  )    
  (setq mtxtLines(list))
  (setq mstpt 1 mcnt 1)
  (setq msrf "\\P")
  (while (< mcnt (+(-(strlen mtxtVal) (strlen msrf))1))
     (setq mtstStr(substr mtxtVal mcnt (strlen msrf)))
     (if(= mtstStr msrf)
       (progn
         (setq mtxtLines(append mtxtLines (list (substr mtxtVal mstpt (- mcnt mstpt)))))
         (setq mstpt (+ mcnt 2))
         (setq mcnt(+ mcnt 1))
       )
     )
     (setq mcnt(+ mcnt 1))
  )
  (if(< mstpt (strlen mtxtVal))
     (setq mtxtLines(append mtxtLines (list (substr mtxtVal mstpt))))
  )
  mtxtLines
)
(defun stripIt(sTxt / stripList sa sCntr sChk)
  (setq stripList (list "\\O" "\\o" "\\L" "\\l" "\\~" "\\A" "\\P"))
  (foreach sa stripList
    (setq sCntr 1)  
    (while(< sCntr (strlen sTxt))
      (setq sChk(substr sTxt sCntr 2))
      (if(= sChk sa)
        (progn
          (if(= sCntr 1)
            (setq sTxt (substr sTxt 3))
            (setq sTxt (strcat (substr sTxt 1 (- sCntr 1)) (substr sTxt (+ sCntr 2))))
          )
        )
      )
      (setq sCntr(+ sCntr 1))
    )
  )
  sTxt
)
(defun getMtxData(/ eset en enlist hdrStr entLyr entSty bigStr bigList
                    entPt entPtStr entVal entHgt entWth entRot dStr vStr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "MTEXT"))
    (setq eset(ssget "X" (list (cons 0 "MTEXT"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog4  "1")(setq hdrStr (strcat hdrStr "Insertion X" spcr "Insertion Y" spcr "Insertion Z" spcr)))
      (if(= tog8  "1")(setq hdrStr (strcat hdrStr "Text Value" spcr)))
      (if(= tog9  "1")(setq hdrStr (strcat hdrStr "Style" spcr)))
      (if(= tog11 "1")(setq hdrStr (strcat hdrStr "Height" spcr)))
      (if(= tog12 "1")(setq hdrStr (strcat hdrStr "Width" spcr)))
      (if(= tog16 "1")(setq hdrStr (strcat hdrStr "Rotation" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))

        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))

        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entSty(cdr(assoc 7 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (if(assoc 3 enlist)
           (progn
              (setq bigStr "")
              (foreach a enlist(if(= (car a) 3)(setq bigStr(strcat bigStr (cdr a))))) 
              (setq bigStr(strcat bigStr (cdr(assoc 1 enlist))))
           )
           (setq bigStr(cdr(assoc 1 enlist)))
        )

        ;;;--- If the comma delimited format is being used, strip the commas out of the text value
        (if(= spcr ",")
            (setq bigStr(stripcommas bigStr))
        )         

        ;;;--- Get rid of the font control data inside brackets {}
        (setq bigList(mtx bigStr))
        (setq entVal "")
        (foreach a bigList
           (setq entVal(strcat entVal " " a))
        )
        ;;;--- Get rid of the control characters
        (setq entVal(stripIt entVal))
        ;;;--- Strip prefixed spaces
        (while (= (substr entVal 1 1) " ")(setq entVal(substr entVal 2)))
        ;;;--- Strip suffixed spaces
        (while (= (substr entVal (strlen entVal) 1) " ")(setq entVal(substr entVal 1 (-(strlen entVal)1))))        
       
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (if(cdr(assoc 62 enlist))(setq entClr(cdr(assoc 62 enlist)))(setq entClr "BYLAYER"))
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))
        (setq entHgt(rtos(cdr(assoc 43 enlist))2 4))
        (setq entWth(rtos(cdr(assoc 42 enlist))2 4))                         
        (setq entRot(angtos (cdr(assoc 50 enlist)) 0 4))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        (setq dStr "" vStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr) vStr (strcat vStr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr) vStr (strcat vStr spcr)))
        (if(= tog4  "1")(setq dStr (strcat dStr entPtSTr spcr) vStr (strcat vStr spcr spcr)))
        (if(= tog8  "1")(setq dStr (strcat dStr entVal spcr) vStr (strcat vStr spcr)))
        (if(= tog9  "1")(setq dStr (strcat dStr entSty spcr)))
        (if(= tog11 "1")(setq dStr (strcat dStr entHgt spcr)))
        (if(= tog12 "1")(setq dStr (strcat dStr entWth spcr))) 
        (if(= tog16 "1")(setq dStr (strcat dStr entRot spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getPoiData(/ eset en enlist hdrStr entLyr entPt entPtStr entClr entRot dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "POINT"))
    (setq eset(ssget "X" (list (cons 0 "POINT"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog4  "1")(setq hdrStr (strcat hdrStr "Insertion X" spcr "Insertion Y" spcr "Insertion Z" spcr)))
      (if(= tog16 "1")(setq hdrStr (strcat hdrStr "Rotation" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (if(cdr(assoc 62 enlist))(setq entClr(cdr(assoc 62 enlist)))(setq entClr "BYLAYER"))
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))
        (if(cdr(assoc 50 enlist))(setq entRot(rtos(cdr(assoc 50 enlist))2 4))(setq entRot "0.0000"))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )
                         
        (setq dStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog4  "1")(setq dStr (strcat dStr entPtSTr spcr)))
        (if(= tog16 "1")(setq dStr (strcat dStr entRot spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getPolData(/ eset en enlist hdrStr dstr vStr entPt entPtStr entEPt entEPtStr cntr
                    entLyr ptList entLty entClr en2 enlist2)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "POLYLINE"))
    (setq eset(ssget "X" (list (cons 0 "POLYLINE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog5  "1")(setq hdrStr (strcat hdrStr "Start X" spcr "Start Y" spcr "Start Z" spcr)))
      (if(= tog6  "1")(setq hdrStr (strcat hdrStr "End X" spcr "End Y" spcr "End Z" spcr)))
      (if(= tog10 "1")(setq hdrStr (strcat hdrStr "LineType" spcr)))
      (if(= tog17 "1")(setq hdrStr (strcat hdrStr "Area" spcr)))
      (if(= tog18 "1")(setq hdrStr (strcat hdrStr "Length" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (if(= tog13 "1")(setq hdrStr (strcat hdrStr "Vertex X" spcr "Vertex Y" spcr "Vertex Z" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))

        ;;;--- Get the points in a list
        (setq ptList(list))

        ;;;--- Get the sub entities name  
        (setq en2(entnext en)) 
                 
        ;;;--- Get the dxf group codes of the subentity
        (setq enlist2(entget en2))
                
        ;;;--- While the polyline has a next vertice
        (while (not (equal (cdr(assoc 0 (entget(entnext en2))))"SEQEND"))
                    
           ;;;--- Get the next subentity 
           (setq en2(entnext en2))
                    
           ;;;--- Get its dxf group codes
           (setq enlist2(entget en2))
                      
           ;;;--- Check to make sure it is not a spline reference point 
           (if(/= 16 (cdr(assoc 70 enlist2)))
                     
              ;;;--- It is a vertex, save the point in a list [ptlist]
              (setq ptList(append ptList (list (cdr(assoc 10 enlist2)))))
                     
           )
        )   

        (setq entPt(car ptList))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (setq entEPt(car(reverse ptList)))
        (setq entEPtStr(strcat (rtos(car entEPt)2 4)spcr(rtos(cadr entEPt)2 4)spcr(rtos(caddr entEPt)2 4)))
        (command "_area" "Object" en)
        (setq entAre(rtos (getvar "area") 2 4))
        (setq entLen(rtos (getvar "perimeter") 2 4))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )
                         
        (setq dStr "" vStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr) vStr (strcat vStr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr) vStr (strcat vStr spcr)))
        (if(= tog5  "1")(setq dStr (strcat dStr entPtSTr spcr) vStr (strcat vStr spcr spcr spcr)))
        (if(= tog6  "1")(setq dStr (strcat dStr entEPtSTr spcr) vStr (strcat vStr spcr spcr spcr)))
        (if(= tog10 "1")(setq dStr (strcat dStr entLty spcr) vStr (strcat vStr spcr)))
        (if(= tog17 "1")(setq dStr (strcat dStr entAre spcr) vStr (strcat vStr spcr)))
        (if(= tog18 "1")(setq dStr (strcat dStr entLen spcr) vStr (strcat vStr spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))
        (if(= tog13 "1")
           (progn
              (foreach a ptList
                (progn
                  (setq nStr(strcat "\n" vStr (rtos(car a)2 4)spcr(rtos(cadr a)2 4)spcr(rtos(caddr a)2 4)))
                  (setq dataList(append dataList(list nStr)))
                )
              )
           )
        )  
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getSolData(/ eset en enlist hdrStr entLyr entPt entPtStr entEPt entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "SOLID"))
    (setq eset(ssget "X" (list (cons 0 "SOLID"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog10 "1")(setq hdrStr (strcat hdrStr "Line Type" spcr)))
      (if(= tog5  "1")(setq hdrStr (strcat hdrStr "1st X" spcr "1st Y" spcr "1st Z" spcr)))
      (setq c
        (strcat
                  "2nd X" spcr "2nd Y" spcr "2nd Z" spcr
                  "3rd X" spcr "3rd Y" spcr "3rd Z" spcr
                  "4th X" spcr "4th Y" spcr "4th Z" spcr
        )                                       
      )  
      (if(= tog13 "1")(setq hdrStr (strcat hdrStr c)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (setq entPt11(cdr(assoc 11 enlist)))
        (setq entPtStr11(strcat (rtos(car entPt11)2 4)spcr(rtos(cadr entPt11)2 4)spcr(rtos(caddr entPt11)2 4)))
        (setq entPt12(cdr(assoc 12 enlist)))
        (setq entPtStr12(strcat (rtos(car entPt12)2 4)spcr(rtos(cadr entPt12)2 4)spcr(rtos(caddr entPt12)2 4)))
        (setq entPt13(cdr(assoc 13 enlist)))
        (setq entPtStr13(strcat (rtos(car entPt13)2 4)spcr(rtos(cadr entPt13)2 4)spcr(rtos(caddr entPt13)2 4)))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )
                         
        (setq dStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog10 "1")(setq dStr (strcat dStr entLty spcr)))
        (if(= tog5  "1")(setq dStr (strcat dStr entPtStr spcr)))
        (if(= tog13  "1")(setq dStr (strcat dStr entPtStr11 spcr entPtStr12 spcr entPtStr13 spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getTxtData(/ eset en enlist hdrStr cntr entLyr entSty bigStr bigList
                    entPt entPtStr entVal entHgt entWth entRot dStr vStr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "TEXT"))
    (setq eset(ssget "X" (list (cons 0 "TEXT"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog5  "1")(setq hdrStr (strcat hdrStr "Start X" spcr "Start Y" spcr "Start Z"spcr)))
      (if(= tog8  "1")(setq hdrStr (strcat hdrStr "Text Value" spcr)))
      (if(= tog9  "1")(setq hdrStr (strcat hdrStr "Style" spcr)))
      (if(= tog11 "1")(setq hdrStr (strcat hdrStr "Height" spcr)))
      (if(= tog16 "1")(setq hdrStr (strcat hdrStr "Rotation" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entSty(cdr(assoc 7 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (setq entVal(cdr(assoc 1 enlist)))

        ;;;--- If the comma delimited format is being used, strip the commas out of the text value
        (if(= spcr ",")
            (setq entVal(stripcommas entVal))
        )

        (if(cdr(assoc 62 enlist))(setq entClr(cdr(assoc 62 enlist)))(setq entClr "BYLAYER"))
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))
        (setq entHgt(rtos(cdr(assoc 40 enlist))2 4))
        (setq entRot(angtos(cdr(assoc 50 enlist))0 4))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        (setq dStr "" vStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog5  "1")(setq dStr (strcat dStr entPtSTr spcr)))
        (if(= tog8  "1")(setq dStr (strcat dStr entVal spcr)))
        (if(= tog9  "1")(setq dStr (strcat dStr entSty spcr)))
        (if(= tog11 "1")(setq dStr (strcat dStr entHgt spcr)))
        (if(= tog16 "1")(setq dStr (strcat dStr entRot spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))

        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getTraData(/ eset en enlist hdrStr entLyr entPt entPtStr entEPt entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "TRACE"))
    (setq eset(ssget "X" (list (cons 0 "TRACE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog10 "1")(setq hdrStr (strcat hdrStr "Line Type" spcr)))
      (if(= tog5  "1")(setq hdrStr (strcat hdrStr "1st X" spcr "1st Y" spcr "1st Z" spcr)))
      (setq c
        (strcat
                  "2nd X" spcr "2nd Y" spcr "2nd Z" spcr
                  "3rd X" spcr "3rd Y" spcr "3rd Z" spcr
                  "4th X" spcr "4th Y" spcr "4th Z" spcr
        )                                       
      )  
      (if(= tog13 "1")(setq hdrStr (strcat hdrStr c)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (setq entPt11(cdr(assoc 11 enlist)))
        (setq entPtStr11(strcat (rtos(car entPt11)2 4)spcr(rtos(cadr entPt11)2 4)spcr(rtos(caddr entPt11)2 4)))
        (setq entPt12(cdr(assoc 12 enlist)))
        (setq entPtStr12(strcat (rtos(car entPt12)2 4)spcr(rtos(cadr entPt12)2 4)spcr(rtos(caddr entPt12)2 4)))
        (setq entPt13(cdr(assoc 13 enlist)))
        (setq entPtStr13(strcat (rtos(car entPt13)2 4)spcr(rtos(cadr entPt13)2 4)spcr(rtos(caddr entPt13)2 4)))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )
                         
        (setq dStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog10 "1")(setq dStr (strcat dStr entLty spcr)))
        (if(= tog5  "1")(setq dStr (strcat dStr entPtStr spcr)))
        (if(= tog13  "1")(setq dStr (strcat dStr entPtStr11 spcr entPtStr12 spcr entPtStr13 spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun getXliData(/ eset en enlist hdrStr entLyr entPt entPtStr entEPt entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "XLINE"))
    (setq eset(ssget "X" (list (cons 0 "XLINE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr "")
      (if(= tog2  "1")(setq hdrStr (strcat hdrStr "Layer" spcr)))
      (if(= tog3  "1")(setq hdrStr (strcat hdrStr "Color" spcr)))
      (if(= tog4  "1")(setq hdrStr (strcat hdrStr "Insertion X" spcr "Insertion Y" spcr "Insertion Z" spcr)))
      (if(= tog10 "1")(setq hdrStr (strcat hdrStr "LineType" spcr)))
      (if(= tog16 "1")(setq hdrStr (strcat hdrStr "Rotation" spcr)))
      (if(= tog19 "1")(setq hdrStr (strcat hdrStr "Handle" spcr)))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(strcat (rtos(car entPt)2 4)spcr(rtos(cadr entPt)2 4)spcr(rtos(caddr entPt)2 4)))
        (setq entRot(angtos (angle (list 0 0)(cdr(assoc 11 enlist)))0 4))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )
                         
        (setq dStr "")
        (if(= tog2  "1")(setq dStr (strcat dStr entLyr spcr)))
        (if(= tog3  "1")(setq dStr (strcat dStr entClr spcr)))
        (if(= tog4  "1")(setq dStr (strcat dStr entPtSTr spcr)))
        (if(= tog10 "1")(setq dStr (strcat dStr entLty spcr)))
        (if(= tog16 "1")(setq dStr (strcat dStr entRot spcr)))
        (if(= tog19 "1")(setq dStr (strcat dStr entHan spcr)))
        (setq dataList(append dataList(list (strcat "\n" dStr))))                                                               
        (setq cntr (+ cntr 1))
      )
    )                    
  )
  dataList
)
(defun setAble()
  (setq idx(atoi(get_tile "entityType")))
  (cond
     ( (= idx 0)  ;;; ARC
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius
           (mode_tile "tog12" 0)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 1)  ;;; ATTRIB
       (progn
           (mode_tile "tog1"  0)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  0)      ;;; Tag
           (mode_tile "tog8"  0)      ;;; Text Value
           (mode_tile "tog9"  0)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 2)  ;;; CIRCLE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius
           (mode_tile "tog12" 0)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 0)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 3)  ;;; ELLIPSE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 0)      ;;; Major Axis
           (mode_tile "tog15" 0)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 0)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 4)  ;;; IMAGE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 5)  ;;; INSERT
       (progn
           (mode_tile "tog1"  0)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 6)  ;;; LINE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  0)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 7)  ;;; LWPOLYLINE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  0)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 0)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 0)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 8)  ;;; MLINE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  0)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  0)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius/Width
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 0)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 9)  ;;; MTEXT
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  0)      ;;; Text Value
           (mode_tile "tog9"  0)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius/Height
           (mode_tile "tog12" 0)      ;;; Diameter/Width
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 10)  ;;; POINT
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 11)  ;;; POLYLINE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  0)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 0)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 0)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 12)  ;;; SOLID
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 0)      ;;; Vertex/Control Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 13)  ;;; TEXT
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  0)      ;;; Text Value
           (mode_tile "tog9"  0)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius/Height
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 14)  ;;; TRACE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  0)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 0)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
     ( (= idx 15)  ;;; XLINE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
       )
     )
  )
)
(defun saveVars()
  (setq entType(atoi(get_tile "entityType")))
  (setq filType(atoi(get_tile "fileType")))
  (setq tog1 (get_tile "tog1"))
  (setq tog2 (get_tile "tog2"))
  (setq tog3 (get_tile "tog3"))
  (setq tog4 (get_tile "tog4"))
  (setq tog5 (get_tile "tog5"))
  (setq tog6 (get_tile "tog6"))
  (setq tog7 (get_tile "tog7"))
  (setq tog8 (get_tile "tog8"))
  (setq tog9 (get_tile "tog9"))
  (setq tog10(get_tile "tog10"))
  (setq tog11(get_tile "tog11"))
  (setq tog12(get_tile "tog12"))
  (setq tog13(get_tile "tog13"))
  (setq tog14(get_tile "tog14"))
  (setq tog15(get_tile "tog15"))
  (setq tog16(get_tile "tog16"))
  (setq tog17(get_tile "tog17"))
  (setq tog18(get_tile "tog18"))
  (setq tog19(get_tile "tog19"))
  (setq tog20(get_tile "tog20"))
)

(defun C2FMAIN();(/ idx entityType fileType cntr dataList)

 (setvar "cmdecho" 0)

 (setq entityType(list))
 (setq entityType
   (list
       "ARC" "ATTRIB" "CIRCLE" "ELLIPSE" "IMAGE" "INSERT (BLOCK)" "LINE" "LWPOLYLINE"
       "MLINE" "MTEXT" "POINT" "POLYLINE" "SOLID" "TEXT" "TRACE" "XLINE"
   )
 )

 (setq fileType(list))
 (setq fileType
   (list "1 - Text File Space Delimited" 
         "2 - Text File Comma Delimited"
         "3 - Text File Tab Delimited"
         "4 - Excel CSV File"
   )
 )

 ;;;--- Put up the dialog box
 (setq dcl_id (load_dialog "CAD2FILE.dcl"))

 ;;;--- See if it is already loaded
 (if (not (new_dialog "CAD2FILE" dcl_id) ) (exit))

 ;;;--- Add the list to the dialog box
 (start_list "entityType" 3)
 (mapcar 'add_list entityType)
 (end_list)

 ;;;--- Add the list to the dialog box
 (start_list "fileType" 3)
 (mapcar 'add_list fileType)
 (end_list)
 
 (set_tile "fileType" "3")
 (set_tile "entityType" "0")
 (setAble)

 ;;;--- If an action event occurs, do this function
 (action_tile "entityType" "(setAble)")
 (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
 (action_tile "accept" "(setq ddiag 2)(saveVars)(done_dialog)") 

 ;;;--- Display the dialog box
 (start_dialog)

 ;;;--- If the cancel button was pressed - display message
 (if (= ddiag 1)
   (princ "\n \n ...CAD2FILE Cancelled. \n ")
 )

 ;;;--- If the "Create" button was pressed
 (if (= ddiag 2)
   (progn
         
     ;;;--- Get the file type that was selected
     (cond
       ((= filType 0)(setq filType "txt" spcr " "))
       ((= filType 1)(setq filType "txt" spcr ","))
       ((= filType 2)(setq filType "txt" spcr (chr 9)))
       ((= filType 3)(setq filType "csv" spcr ","))        
     )
        
     ;;;--- Check to see what type of entity it is 
     (cond
       ((= idx 0)
         (progn
           (if(setq dataList(getArcData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1))
                 (progn 
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " ARCs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No ARCs found!")
           )    
       ) )                                                                           ;;; end of arc
       ((= idx 1)
         (progn
           (if(setq dataList(getAttData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1)) 
                 (progn
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " ATTRIBUTEs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No ATTRIBUTEs found!")
           )                       
       ) )                                                                           ;;;end of attribute
       ((= idx 2)
         (progn
           (if(setq dataList(getCirData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1))
                 (progn 
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " CIRCLEs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No CIRCLEs found!")
           )                       
       ) )                                                                           ;;;end of circle
       ((= idx 3)
         (progn
           (if(setq dataList(getEllData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1)) 
                 (progn  
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " ELLIPSEs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No ELLIPSEs found!")
           )                       
       ) )                                                                           ;;;end of ellipse
       ((= idx 4)
         (progn
           (if(setq dataList(getImgData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1))
                 (progn 
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " IMAGEs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No IMAGEs found!")
           )                       
       ) )                                                                           ;;;end of image
       ((= idx 5)
         (progn
           (if(setq dataList(getInsData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1))
                 (progn 
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " BLOCKs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             ) 
             (alert "No BLOCKs found!")
           )                       
       ) )                                                                           ;;;end of insert
       ((= idx 6)
         (progn
           (if(setq dataList(getLinData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1))
                 (progn 
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " LINEs to " filName "!"))
                    )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No LINEs found!")
           )                       
       ) )                                                                           ;;;end of line
       ((= idx 7)
         (progn
           (if(setq dataList(getLwpData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1))
                 (progn 
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " LWPOLYLINEs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No LWPOLYLINEs found!")
           )                       
       ) )                                                                           ;;;end of lwpolyline
       ((= idx 8)
         (progn
           (if(setq dataList(getMliData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1)) 
                 (progn
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " MLINEs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No MLINEs found!")
           )                       
       ) )                                                                           ;;;end of mline
       ((= idx 9)
         (progn
           (if(setq dataList(getMtxData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1))
                 (progn 
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " MTEXT entities to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No MTEXT entities found!")
           )                       
       ) )                                                                           ;;;end of mtext
       ((= idx 10)
         (progn
           (if(setq dataList(getPoiData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1)) 
                 (progn
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " POINTs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No POINTs found!")
           )                       
       ) )                                                                           ;;;end of point
       ((= idx 11)
         (progn
           (if(setq dataList(getPolData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1))
                 (progn 
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " POLYLINEs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No POLYLINEs found!")
           )                       
       ) )                                                                           ;;;end of polyline
       ((= idx 12)
         (progn
           (if(setq dataList(getSolData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1))
                 (progn 
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " SOLIDs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No SOLIDs found!")
           )                       
       ) )                                                                           ;;;end of solid
       ((= idx 13)
         (progn
           (if(setq dataList(getTxtData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1)) 
                 (progn
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " TEXT entities to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No TEXT entities found!")
           )                       
       ) )                                                                           ;;; end of text
       ((= idx 14)
         (progn
           (if(setq dataList(getTraData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1))
                 (progn 
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " TRACEs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No TRACEs found!")
           )                       
       ) )                                                                           ;;; end of trace
       ((= idx 15)
         (progn
           (if(setq dataList(getXliData))
             (progn
               ;;;--- If the user has a valid file name
               (if(setq filName(getfiled "Select File Name" "" filType 1)) 
                 (progn
                   ;;;--- Open the file to write
                   (if(setq fil(open filName "w"))
                     (progn
                       (foreach a dataList(princ a fil))
                       (close fil)
                       (alert (strcat "Sent " (itoa cntr) " XLINEs to " filName "!"))
                     )
                     (alert "Error - Could not open File. \nMake sure the file is not opened by another application.")
                   )
                 )
               )
             )
             (alert "No XLINEs found!")
           )                       
         )
       )                                                                             ;;; end of xline
     )                                                                               ;;; end condition 
   )
 )
 (setvar "cmdecho" 1)
 (princ)
)
(defun C:C2F()
  (c2fMain)
)
(defun C:CAD2FILE()
  (c2fMain)
)
(princ "CAD2File loaded!\n\nType C2F or CAD2FILE to start the program.")(princ)