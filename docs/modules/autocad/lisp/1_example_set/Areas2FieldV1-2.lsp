;;-------------------=={ Areas 2 Field }==--------------------;;
;;                                                            ;;
;;  Creates an MText object containing a Field Expression     ;;
;;  referencing the area, or sum of areas, of one or more     ;;
;;  selected objects.                                         ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.2    -    26-05-2013                            ;;
;;------------------------------------------------------------;;

(defun c:a2f ( / *error* fmt inc ins lst sel str )

    (setq fmt "%lu6%qf1") ;; Field Formatting

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if
        (and
            (setq sel (ssget '((0 . "ARC,CIRCLE,ELLIPSE,HATCH,*POLYLINE,REGION,SPLINE"))))
            (setq ins (getpoint "\nPick Point for Field: "))
        )
        (progn
            (if (= 1 (sslength sel))
                (setq str
                    (strcat
                        "%<\\AcObjProp Object(%<\\_ObjId "
                        (LM:ObjectID (vlax-ename->vla-object (ssname sel 0)))
                        ">%).Area \\f \"" fmt "\">%"
                    )
                )
                (progn
                    (repeat (setq inc (sslength sel))
                        (setq lst
                            (vl-list*
                                "%<\\AcObjProp Object(%<\\_ObjId "
                                (LM:ObjectID (vlax-ename->vla-object (ssname sel (setq inc (1- inc)))))
                                ">%).Area>%" " + "
                                lst
                            )
                        )
                    )
                    (setq str
                        (strcat
                            "%<\\AcExpr "
                            (apply 'strcat (reverse (cdr (reverse lst))))
                            " \\f \"" fmt "\">%"
                        )
                    )
                )
            )
            (LM:startundo (LM:acdoc))
            (vla-addmtext
                (vlax-get-property (LM:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                (vlax-3D-point (trans ins 1 0))
                0.0
                str
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems
 
(defun LM:ObjectID ( obj )
    (eval
        (list 'defun 'LM:ObjectID '( obj )
            (if
                (and
                    (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
                    (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                )
                (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:ObjectID obj)
)
 
;; Start Undo  -  Lee Mac
;; Opens an Undo Group.
 
(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)
 
;; End Undo  -  Lee Mac
;; Closes an Undo Group.
 
(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)
 
;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object
 
(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)
 
(vl-load-com) (princ)
 
;;------------------------------------------------------------;;
;;                        End of File                         ;;
;;------------------------------------------------------------;;