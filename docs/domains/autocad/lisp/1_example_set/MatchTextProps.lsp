;;---------------=={ Match Text Properties }==----------------;;
;;                                                            ;;
;;  Prompts for a selection of Text, MText, Attribute, or     ;;
;;  Attribute Definition object to use as property source,    ;;
;;  then proceed to match those properties listed for similar ;;
;;  objects selected thereafter.                              ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:MTP nil (c:MatchTextProps))

(defun c:MatchTextProps ( / *error* _StartUndo _EndUndo _GetTextInsertion _PutTextInsertion Props doc entity object ss )
  (vl-load-com)
  ;; © Lee Mac 2010

  (setq Props
   '(
     Alignment
     AttachmentPoint
     BackgroundFill
     Backward
     DrawingDirection
     Height
     Layer
     LineSpacingDistance
     LineSpacingFactor
     LineSpacingStyle
     Linetype
     LinetypeScale
     Lineweight
     ObliqueAngle
     Rotation
     ScaleFactor
     StyleName
    ; TextString
     Thickness
     UpsideDown
     Width
    )
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (defun *error* ( msg )
    (if doc (_EndUndo doc)) (if mutt (setvar 'NOMUTT mutt))
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

  (defun _GetTextInsertion ( object )
    (vlax-get-property object
      (if
        (or
          (eq "AcDbMText" (vla-get-ObjectName object))
          (vl-position (vla-get-Alignment object)
            (list acAlignmentLeft acAlignmentFit acAlignmentAligned)
          )
        )
        'InsertionPoint
        'TextAlignmentPoint
      )
    )
  )

  (defun _PutTextInsertion ( object point )
    (vlax-put-property object
      (if
        (or
          (eq "AcDbMText" (vla-get-ObjectName object))
          (vl-position (vla-get-Alignment object)
            (list acAlignmentLeft acAlignmentFit acAlignmentAligned)
          )
        )
        'InsertionPoint
        'TextAlignmentPoint
      )
      point
    )
  )

  (if
    (and
      (setq entity
        (LM:Selectif
          (lambda ( x )
            (wcmatch (cdr (assoc 0 (entget x))) "TEXT,MTEXT,ATTRIB,ATTDEF")
          )
          nentsel "\nSelect Source Object: "
        )
      )
      (progn
        (setq mutt (getvar 'NOMUTT))
        (setvar 'NOMUTT 1)
        
        (princ (strcat "\nSelect Destination " (cdr (assoc 0 (entget entity))) " objects: "))
        (setq object (vlax-ename->vla-object entity)
          ss
           (ssget "_:L"
             (list
               (assoc 0 (entget entity))
             )
           )
        )
        (setvar 'NOMUTT mutt) ss
      )
    )
    (
      (lambda ( i values / entity obj )

        (_StartUndo doc)
        
        (while (setq entity (ssname ss (setq i (1+ i))))
          (setq obj (vlax-ename->vla-object entity))

          (mapcar
            (function
              (lambda ( prop value )
                (if
                  (vl-catch-all-error-p
                    (vl-catch-all-apply
                      (function
                        (lambda nil
                          (if (and (vlax-property-available-p obj prop t) value)
                            (if (vl-position prop '(Alignment AttachmentPoint))
                              (
                                (lambda ( insertion )
                                  (vlax-put-property obj prop value)
                                  (_PutTextInsertion obj insertion)
                                )
                                (_GetTextInsertion obj)
                              )
                              (vlax-put-property obj prop value)
                            )
                          )
                        )
                      )
                    )
                  )
                  (princ (strcat "\n** Error Applying Property: " Prop " **"))
                )
              )
            )
            Props Values
          )
        )

        (_EndUndo doc)
      )
      -1
      (mapcar
        (function
          (lambda ( prop )
            (if (vlax-property-available-p object prop)
              (vlax-get-property object prop)
            )
          )
        )
        Props
      )
    )
  )
  (princ)
)

;;---------------------=={ Select if }==----------------------;;
;;                                                            ;;
;;  Continuous selection prompts until the predicate function ;;
;;  foo is validated                                          ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  foo - optional predicate function taking ename argument   ;;
;;  fun - selection function to invoke                        ;;
;;  str - prompt string                                       ;;
;;------------------------------------------------------------;;
;;  Returns:  selected entity ename if successful, else nil   ;;
;;------------------------------------------------------------;;

(defun LM:Selectif ( foo fun str / e )
  ;; © Lee Mac 2010
  (while
    (progn (setq e (car (fun str)))      
      (cond
        ( (eq 'ENAME (type e))

          (if (and foo (not (foo e)))
            (princ "\n** Invalid Object Selected **")
          )
        )
      )
    )
  )
  e
)