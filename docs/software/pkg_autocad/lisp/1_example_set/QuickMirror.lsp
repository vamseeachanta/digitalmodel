;;---------------------=={ Quick Mirror }==-------------------;;
;;                                                            ;;
;;  Provides functionality to mirror a selection of objects   ;;
;;  or a single object without the need to select two points  ;;
;;  defining a mirror axis.                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  single - if T, mirrors selected object using derivative   ;;
;;           at the selected edge to determine mirror axis,   ;;
;;           else prompts for selection set and object to use ;;
;;           as mirror axis.                                  ;;
;;  delete - if T, selected object or selection set is        ;;
;;           deleted following the mirror operation.          ;;
;;------------------------------------------------------------;;
;;  Returns:  Null.                                           ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    15-02-2011                            ;;
;;                                                            ;;
;;  First Release.                                            ;;
;;------------------------------------------------------------;;

;;------------------------------------------------------------;;
;;                      Program Shortcuts                     ;;
;;------------------------------------------------------------;;

;; Mirror SelectionSet about selected object
(defun c:QM   nil (QuickMirror nil nil))

;; Mirror SelectionSet about selected object, delete SelectionSet
(defun c:QMD  nil (QuickMirror nil   t))

;; Mirror Single Object about Selection Point
(defun c:QMO  nil (QuickMirror   t nil))

;; Mirror Single Object about Selection Point, delete Original Object
(defun c:QMOD nil (QuickMirror   t   t))

;;------------------------------------------------------------;;
;;                   Quick Mirror Subfunction                 ;;
;;------------------------------------------------------------;;

(defun QuickMirror ( single delete / *error* _StartUndo _EndUndo doc ss sel p1 p2 i o ) (vl-load-com)
  ;; © Lee Mac 2011

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

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (if (or single (setq ss (ssget "_:L")))
    (while
      (progn (setvar 'ERRNO 0) (setq sel (entsel "\nSelect Mirror Object: "))
        (cond
          (
            (=  7 (getvar 'ERRNO)) (princ "\n** Missed, Try Again **")
          )
          (
            (and sel
              (not
                (vl-catch-all-error-p
                  (setq p1
                    (vl-catch-all-apply 'vlax-curve-getClosestPointto
                      (list (car sel) (trans (cadr sel) 1 0))
                    )
                  )
                )
              )
            )

            (setq p2
              (polar p1
                (angle '(0. 0. 0.)
                  (vlax-curve-getFirstDeriv (car sel)
                    (vlax-curve-getParamatPoint (car sel) p1)
                  )
                )
                1.
              )
            )

            (setq p1 (vlax-3D-point p1) p2 (vlax-3D-point p2))

            (_StartUndo doc)
            (if ss
              (repeat (setq i (sslength ss))
                (vla-mirror (setq o (vlax-ename->vla-object (ssname ss (setq i (1- i))))) p1 p2)
                (if delete  (vla-delete o))
              )
              (progn
                (vla-mirror (setq o (vlax-ename->vla-object (car sel))) p1 p2)
                (if delete  (vla-delete o))
              )
            )
            (_EndUndo doc)
          )
        )
      )
    )
  )
  (princ)
)

;;------------------------------------------------------------;;
;;                          End of File                       ;;
;;------------------------------------------------------------;;
