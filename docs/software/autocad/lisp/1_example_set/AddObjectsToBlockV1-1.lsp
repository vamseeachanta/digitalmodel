;;----------------=={ Add Objects to Block }==----------------;;
;;                                                            ;;
;;  Adds all objects in the provided SelectionSet to the      ;;
;;  definition of the specified block.                        ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  doc   - Document Object in which block resides.           ;;
;;  block - Entity name of reference insert                   ;;
;;  ss    - SelectionSet of objects to add to definition      ;;
;;------------------------------------------------------------;;

(defun LM:AddObjectstoBlock ( doc block ss / lst mat )
  
  (setq lst (LM:ss->vla ss)
        mat (LM:Ref->Def block)
        mat (vlax-tmatrix (append (mapcar 'append (car mat) (mapcar 'list (cadr mat))) '((0. 0. 0. 1.))))
  )
  (foreach obj lst (vla-transformby obj mat))

  (vla-CopyObjects doc (LM:SafearrayVariant vlax-vbobject lst)
    (vla-item (vla-get-Blocks doc) (cdr (assoc 2 (entget block))))
  )
  (foreach obj lst (vla-delete obj))
  (vla-regen doc acAllViewports)
)

;;-----------------=={ Remove From Block }==------------------;;
;;                                                            ;;
;;  Removes an Entity from a Block Definition                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ent - Entity name of Object to Delete from Block [ENAME]  ;;
;;------------------------------------------------------------;;

(defun LM:RemovefromBlock ( doc ent )
  (vla-delete (vlax-ename->vla-object ent))
  (vla-regen doc acAllViewports)
  (princ)
)

;;------------------=={ Safearray Variant }==-----------------;;
;;                                                            ;;
;;  Creates a populated Safearray Variant of a specified      ;;
;;  data type                                                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  datatype - variant type enum (eg vlax-vbDouble)           ;;
;;  data     - list of static type data                       ;;
;;------------------------------------------------------------;;
;;  Returns:  VLA Variant Object of type specified            ;;
;;------------------------------------------------------------;;
                         
(defun LM:SafearrayVariant ( datatype data )
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray datatype (cons 0 (1- (length data)))) data
    )    
  )
)

;;------------=={ SelectionSet -> VLA Objects }==-------------;;
;;                                                            ;;
;;  Converts a SelectionSet to a list of VLA Objects          ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ss - Valid SelectionSet (Pickset)                         ;;
;;------------------------------------------------------------;;
;;  Returns:  List of VLA Objects, else nil                   ;;
;;------------------------------------------------------------;;

(defun LM:ss->vla ( ss / i l )
  (if ss
    (repeat (setq i (sslength ss))
      (setq l (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) l))
    )
  )
)

;;---------------=={ Block Ref -> Block Def }==---------------;;
;;                                                            ;;
;;  Returns the Transformation Matrix and Translation Vector  ;;
;;  for transforming Block Reference Geometry to the Block    ;;
;;  Definiton.                                                ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  e - Block Reference Entity                                ;;
;;------------------------------------------------------------;;
;;  Returns:  List of 3x3 Transformation Matrix, Vector       ;;
;;------------------------------------------------------------;;

(defun LM:Ref->Def ( e / _dxf a l n )

  (defun _dxf ( x l ) (cdr (assoc x l)))

  (setq l (entget e) a (- (_dxf 50 l)) n (_dxf 210 l))
  (
    (lambda ( m )
      (list m
        (mapcar '- (_dxf 10 (tblsearch "BLOCK" (_dxf 2 l)))
          (mxv m
            (trans (_dxf 10 l) n 0)
          )
        )
      )
    )
    (mxm
      (list
        (list (/ 1. (_dxf 41 l)) 0. 0.)
        (list 0. (/ 1. (_dxf 42 l)) 0.)
        (list 0. 0. (/ 1. (_dxf 43 l)))
      )
      (mxm
        (list
          (list (cos a) (sin (- a)) 0.)
          (list (sin a) (cos a)     0.)
          (list    0.        0.     1.)
        )
        (mapcar '(lambda ( e ) (trans e n 0 t))
         '(
            (1. 0. 0.)
            (0. 1. 0.)
            (0. 0. 1.)
          )
        )
      )
    )
  )
)

;; Matrix x Vector  -  Vladimir Nesterovsky
(defun mxv ( m v )
  (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
(defun mxm ( m q )
  (mapcar (function (lambda ( r ) (mxv (trp q) r))) m)
)

;; Matrix Transpose  -  Doug Wilson
(defun trp ( m )
  (apply 'mapcar (cons 'list m))
)

;;---------------------=={ Select if }==----------------------;;
;;                                                            ;;
;;  Provides continuous selection prompts until either a      ;;
;;  predicate function is validated or a keyword is supplied. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  msg  - prompt string                                      ;;
;;  pred - optional predicate function [selection list arg]   ;;
;;  func - selection function to invoke                       ;;
;;  keyw - optional initget argument list                     ;;
;;------------------------------------------------------------;;
;;  Returns:  Entity selection list, keyword, or nil          ;;
;;------------------------------------------------------------;;

(defun LM:SelectIf ( msg pred func keyw / sel ) (setq pred (eval pred))  
  (while
    (progn (setvar 'ERRNO 0) (if keyw (apply 'initget keyw)) (setq sel (func msg))
      (cond
        ( (= 7 (getvar 'ERRNO))

          (princ "\nMissed, Try again.")
        )
        ( (eq 'STR (type sel))

          nil
        )
        ( (vl-consp sel)

          (if (and pred (not (pred sel)))
            (princ "\nInvalid Object Selected.")
          )
        )
      )
    )
  )
  sel
)

;-------------------------------------------------------------;
;                   -- Test Functions --                      ;
;-------------------------------------------------------------;

(defun c:Add2Block ( / *error* _StartUndo _EndUndo acdoc ss e )

  (defun *error* ( msg )
    (if acdoc (_EndUndo acdoc))
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
  
  (setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (if
    (and (setq ss (ssget "_:L"))
      (setq e
        (LM:SelectIf "\nSelect Block to Add Objects to: "
         '(lambda ( x ) (eq "INSERT" (cdr (assoc 0 (entget (car x)))))) entsel nil
        )
      )
    )
    (progn
      (_StartUndo acdoc) (LM:AddObjectstoBlock acdoc (car e) ss) (_EndUndo acdoc)
    )
  )
  (princ)
)

;-------------------------------------------------------------;

(defun c:Remove ( / *error* _StartUndo _EndUndo acdoc e )

  (defun *error* ( msg )
    (if acdoc (_EndUndo acdoc))
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
  
  (setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (while (setq e (car (nentsel "\nSelect Object to Remove: ")))
    (_StartUndo acdoc) (LM:RemovefromBlock acdoc e) (_EndUndo acdoc)
  )
  (princ)
)

(vl-load-com) (princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;