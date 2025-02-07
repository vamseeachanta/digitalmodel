;;-----------------------=={ Burst }==------------------------;;
;;                                                            ;;
;;  Performs in much the same way as the Express Tools' Burst ;;
;;  command, but doesn't display invisible attributes.        ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:iBurst ( / *error* _StartUndo _EndUndo doc blocks bLay bCol bLin def ss )
  (vl-load-com)
  ;; © Lee Mac 2010

  (defun *error* ( msg )
    (if doc (_EndUndo doc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  (setq doc    (vla-get-ActiveDocument (vlax-get-acad-object))
        blocks (vla-get-Blocks doc)
  )    

  (if (ssget "_:L" '((0 . "INSERT")))
    (progn (_StartUndo doc)
      
      (vlax-for obj (setq ss (vla-get-ActiveSelectionSet doc))

        (mapcar 'set '(bLay bCol bLin)
          (mapcar '(lambda ( p ) (vlax-get-property obj p)) '(Layer Color Linetype))
        )

        (if (or (not (vlax-property-available-p (setq def (LM:Itemp blocks (LM:GetBlockName obj))) 'Explodable))
                (eq :vlax-true (vla-get-Explodable def)))
          (progn
            
            (if (eq :vlax-true (vla-get-HasAttributes obj))
              
              (foreach att (vlax-invoke obj 'GetAttributes)

                (if (eq "0" (vla-get-layer att))
                  (vla-put-layer att bLay)
                )
                (if (eq acByBlock (vla-get-color att))
                  (vla-put-color att bCol)
                )
                (if (eq "BYBLOCK" (vla-get-linetype att))
                  (vla-put-linetype att bLin)
                )
                
                (if (eq :vlax-false (vla-get-Invisible att))
                  (
                    (if (and (vlax-property-available-p att 'MTextAttribute)
                             (eq :vlax-true (vla-get-MTextAttribute att)))

                      LM:MAtt2MText LM:Att2Text
                    )
                    (entget (vlax-vla-object->ename att))
                  )
                )
              )
            )
   
            (foreach eobj (vlax-invoke obj 'Explode)
              (if (eq "AcDbAttributeDefinition" (vla-get-ObjectName eobj))
                (vla-delete eobj)
                (progn
                  (if (eq "0" (vla-get-layer eobj))
                    (vla-put-layer eobj bLay)
                  )
                  (if (eq acByBlock (vla-get-color eobj))
                    (vla-put-color eobj bCol)
                  )
                  (if (eq "BYBLOCK" (vla-get-linetype eobj))
                    (vla-put-linetype eobj bLin)
                  )
                )
              )
            )
            (vla-delete obj)
          )
        )
      )
      (vla-Delete ss) (_EndUndo doc)
    )
  )
  (princ)            
)

;;-------------------=={ Get Block Name }==-------------------;;
;;                                                            ;;
;;  Retrieves the Block Name as per the Block Definition      ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  obj - VLA Block Reference Object                          ;;
;;------------------------------------------------------------;;
;;  Returns:  Block Name                                      ;;
;;------------------------------------------------------------;;

(defun LM:GetBlockName ( obj )
  ;; © Lee Mac 2010
  (vlax-get-property obj
    (if (vlax-property-available-p obj 'EffectiveName) 'EffectiveName 'Name)
  )
)
                                                 
;;-----------------------=={ Itemp }==------------------------;;
;;                                                            ;;
;;  Retrieves the item with index 'item' if present in the    ;;
;;  specified collection, else nil                            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  coll - the VLA Collection Object                          ;;
;;  item - the index of the item to be retrieved              ;;
;;------------------------------------------------------------;;
;;  Returns:  the VLA Object at the specified index, else nil ;;
;;------------------------------------------------------------;;

(defun LM:Itemp ( coll item )
  ;; © Lee Mac 2010
  (if
    (not
      (vl-catch-all-error-p
        (setq item
          (vl-catch-all-apply
            (function vla-item) (list coll item)
          )
        )
      )
    )
    item
  )
)

;;-------------------=={ Remove Pairs }==---------------------;;
;;                                                            ;;
;;  Removes items from an association list whose key appears  ;;
;;  in the supplied integer list.                             ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  pairs - list of integers specifying pairs to remove       ;;
;;  lst   - list from which pairs are to be removed           ;;
;;------------------------------------------------------------;;
;;  Returns:  List with the specified pairs removed           ;;
;;------------------------------------------------------------;;

(defun LM:RemovePairs ( pairs lst )
  ;; © Lee Mac 2010
  (vl-remove-if '(lambda ( pair ) (vl-position (car pair) pairs)) lst)
) 

;;-----------------=={ Remove First Pairs }==-----------------;;
;;                                                            ;;
;;  Removes the first instance of an item from an association ;;
;;  list whose key appears in the supplied integer list.      ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  pairs - list of integers specifying pairs to remove       ;;
;;  lst   - list from which pairs are to be removed           ;;
;;------------------------------------------------------------;;
;;  Returns:  List with the specified pairs removed           ;;
;;------------------------------------------------------------;;

(defun LM:RemoveFirstPairs ( pairs lst )
  ;; © Lee Mac 2010
  (if lst
    (if pairs
      (if (vl-position (caar lst) pairs)
        (LM:RemoveFirstPairs (vl-remove (caar lst) pairs) (cdr lst))
        (cons (car lst) (LM:RemoveFirstPairs pairs (cdr lst)))
      )
      lst
    )
  )
)

;;------------------=={ Attribute to Text }==-----------------;;
;;                                                            ;;
;;  Creates a Text entity from a single-line attribute entity ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  elist - Attribute Entity DXF List                         ;;
;;------------------------------------------------------------;;
;;  Returns:  Entity Name of resultant Text, else nil         ;;
;;------------------------------------------------------------;;
  
(defun LM:Att2Text ( elist )
  ;; © Lee Mac 2010
  (
    (lambda ( dxf74 )    
      (entmakex
        (append '( (0 . "TEXT") )
          (LM:RemovePairs '(0 100 2 74 70 280)
            (subst (cons 73 dxf74) (assoc 74 elist) elist)
          )
        )
      )
    )
    (cdr (assoc 74 elist))
  )
)

;;--------------=={ MText Attribute to MText }==--------------;;
;;                                                            ;;
;;  Creates an MText entity from a multiline attribute entity ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  elist - MText Attribute Entity DXF List                   ;;
;;------------------------------------------------------------;;
;;  Returns:  Entity Name of resultant MText, else nil        ;;
;;------------------------------------------------------------;;

(defun LM:MAtt2MText ( elist )
  ;; © Lee Mac 2010
  (entmakex
    (append '( (0 . "MTEXT") (100 . "AcDbEntity") (100 . "AcDbMText") )
      (LM:RemoveFirstPairs '(40 1 50 41 7 71 72 71 72 73 10 11 11 210)
        (LM:RemovePairs '(102 330 360 0 100 101 2 42 43 51 74 70 280) elist)
      )
    )
  )
)