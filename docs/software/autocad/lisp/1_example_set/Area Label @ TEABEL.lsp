;;---------------------=={ Area Label }==---------------------;;
;;                                                            ;;
;;  Allows the user to label picked areas or objects and      ;;
;;  either display the area in an ACAD Table (if available),  ;;
;;  optionally using fields to link area numbers and objects; ;;
;;  or write it to file.                                      ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.9    -    29-10-2011                            ;;
;;------------------------------------------------------------;;

(defun c:AT nil (AreaLabel   t))  ;; Areas to Table
(defun c:AF nil (AreaLabel nil))  ;; Areas to File

;;------------------------------------------------------------;;

(defun AreaLabel ( flag / *error* _startundo _endundo _centroid _text _open _select _getobjectid _isannotative
                          acdoc acspc ap ar as cf cm el fd fl fo n of om p1 pf pt sf st t1 t2 tb th ts tx ucsxang ucszdir )

  ;;------------------------------------------------------------;;
  ;;                         Adjustments                        ;;
  ;;------------------------------------------------------------;;

  (setq h1 "Area Table"  ;; Heading
        t1 "Number"      ;; Number Title
        t2 "Area"        ;; Area Title
        pf ""            ;; Number Prefix (optional, "" if none)
        sf ""            ;; Number Suffix (optional, "" if none)
        ap ""            ;; Area Prefix (optional, "" if none)
        as ""            ;; Area Suffix (optional, "" if none)
        cf 1.0           ;; Area Conversion Factor (e.g. 1e-6 = mm2->m2)
        fd t             ;; Use fields to link numbers/objects to table (t=yes, nil=no)
        fo "%lu6%qf1"    ;; Area field formatting
  )

  ;;------------------------------------------------------------;;

  (defun *error* ( msg )
    (if cm (setvar 'CMDECHO cm))
    (if el (progn (entdel el) (setq el nil)))
    (if acdoc (_EndUndo acdoc))
    (if (and of (eq 'FILE (type of))) (close of))
    (if (and Shell (not (vlax-object-released-p Shell))) (vlax-release-object Shell))
    (if (null (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
        (princ (strcat "\n--> Error: " msg))
    )
    (princ)
  )

  ;;------------------------------------------------------------;;

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  ;;------------------------------------------------------------;;

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  ;;------------------------------------------------------------;;

  (defun _centroid ( space objs / reg cen )
    (setq reg (car (vlax-invoke space 'addregion objs))
          cen (vlax-get reg 'centroid)
    )
    (vla-delete reg) (trans cen 1 0)
  )

  ;;------------------------------------------------------------;;

  (defun _text ( space point string height rotation / text )
    (setq text (vla-addtext space string (vlax-3D-point point) height))
    (vla-put-alignment text acalignmentmiddlecenter)
    (vla-put-textalignmentpoint text (vlax-3D-point point))
    (vla-put-rotation text rotation)
    text
  )

  ;;------------------------------------------------------------;;

  (defun _Open ( target / Shell result )
    (if (setq Shell (vla-getInterfaceObject (vlax-get-acad-object) "Shell.Application"))
      (progn
        (setq result
          (and (or (eq 'INT (type target)) (setq target (findfile target)))
            (not
              (vl-catch-all-error-p
                (vl-catch-all-apply 'vlax-invoke (list Shell 'Open target))
              )
            )
          )
        )
        (vlax-release-object Shell)
      )
    )
    result
  )

  ;;------------------------------------------------------------;;

  (defun _Select ( msg pred func init / e ) (setq pred (eval pred)) 
    (while
      (progn (setvar 'ERRNO 0) (apply 'initget init) (setq e (func msg))
        (cond
          ( (= 7 (getvar 'ERRNO))
            (princ "\nMissed, try again.")
          )
          ( (eq 'STR (type e))
            nil
          )            
          ( (vl-consp e)
            (if (and pred (not (pred (setq e (car e)))))
              (princ "\nInvalid Object Selected.")
            )
          )
        )
      )
    )
    e
  )

  ;;------------------------------------------------------------;;

  (defun _GetObjectID ( doc obj )
    (if (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
      (vlax-invoke-method (vla-get-Utility doc) 'GetObjectIdString obj :vlax-false)
      (itoa (vla-get-Objectid obj))
    )
  )

  ;;------------------------------------------------------------;;

  (defun _isAnnotative ( style / object annotx )
    (and
      (setq object (tblobjname "STYLE" style))
      (setq annotx (cadr (assoc -3 (entget object '("AcadAnnotative")))))
      (= 1 (cdr (assoc 1070 (reverse annotx))))
    )
  )
  
  ;;------------------------------------------------------------;;

  (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
        acspc (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'Paperspace 'Modelspace))

        ucszdir (trans '(0. 0. 1.) 1 0 t)
        ucsxang (angle '(0. 0. 0.) (trans (getvar 'UCSXDIR) 0 ucszdir))
  )
  (_StartUndo acdoc)
  (setq cm (getvar 'CMDECHO))
  (setvar 'CMDECHO 0)
  (setq om (eq "1" (cond ((getenv "LMAC_AreaLabel")) ((setenv "LMAC_AreaLabel" "0")))))

  (setq ts
    (/ (getvar 'TEXTSIZE)
      (if (_isAnnotative (getvar 'TEXTSTYLE))
        (cond ( (getvar 'CANNOSCALEVALUE) ) ( 1.0 )) 1.0
      )
    )
  )

  (cond
    ( (not (vlax-method-applicable-p acspc 'addtable))

      (princ "\n--> Table Objects not Available in this Version.")
    )
    ( (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar 'CLAYER))))))

      (princ "\n--> Current Layer Locked.")
    )
    ( (not
        (setq *al:num
          (cond
            (
              (getint
                (strcat "\nSpecify Starting Number <"
                  (itoa (setq *al:num (1+ (cond ( *al:num ) ( 0 ))))) ">: "
                )
              )
            )
            ( *al:num )
          )
        )
      )
    )
    ( flag

      (setq th
        (* 2.
          (if
            (zerop
              (setq th
                (vla-gettextheight
                  (setq st
                    (vla-item
                      (vla-item
                        (vla-get-dictionaries acdoc) "ACAD_TABLESTYLE"
                      )
                      (getvar 'CTABLESTYLE)
                    )
                  )
                  acdatarow
                )
              )
            )
            ts
            (/ th
              (if (_isAnnotative (vla-gettextstyle st acdatarow))
                (cond ( (getvar 'CANNOSCALEVALUE) ) ( 1.0 )) 1.0
              )
            )
          )
        )
      )

      (if
        (cond
          (
            (progn (initget "Add")
              (vl-consp (setq pt (getpoint "\nPick Point for Table <Add to Existing>: ")))
            )
            (setq tb
              (vla-addtable acspc
                (vlax-3D-point (trans pt 1 0)) 2 2 th (* 0.8 th (max (strlen t1) (strlen t2)))
              )
            )
            (vla-put-direction tb (vlax-3D-point (getvar 'UCSXDIR)))
            (vla-settext tb 0 0 h1)
            (vla-settext tb 1 0 t1)
            (vla-settext tb 1 1 t2)
            
            (while
              (progn
                (if om
                  (setq p1
                    (_Select (strcat "\nSelect Object [Pick] <Exit>: ")
                     '(lambda ( x )
                        (and
                          (vlax-property-available-p (vlax-ename->vla-object x) 'area)
                          (not (eq "HATCH" (cdr (assoc 0 (entget x)))))
                          (or (eq "REGION" (cdr (assoc 0 (entget x)))) (vlax-curve-isclosed x))
                        )
                      )
                      entsel '("Pick")
                    )
                  )
                  (progn (initget "Object") (setq p1 (getpoint "\nPick Area [Object] <Exit>: ")))
                )
                (cond
                  ( (null p1)

                    (vla-delete tb)
                  )
                  ( (eq "Pick" p1)

                    (setq om nil) t
                  )
                  ( (eq "Object" p1)

                    (setq om t)
                  )
                  ( (eq 'ENAME (type p1))

                    (setq tx
                      (cons
                        (_text acspc
                          (_centroid acspc (list (setq p1 (vlax-ename->vla-object p1))))
                          (strcat pf (itoa *al:num) sf)
                          ts
                          ucsxang
                        )
                        tx
                      )
                    )
                    (vla-insertrows tb (setq n 2) th 1)
                    (vla-settext tb n 1
                      (if fd
                        (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                          (_GetObjectID acdoc p1) ">%).Area \\f \"" fo "\">%"
                        )
                        (strcat ap (rtos (* cf (vla-get-area p1)) 2) as)
                      )
                    )
                    (vla-settext tb n 0
                      (if fd
                        (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                          (_GetObjectID acdoc (car tx)) ">%).TextString>%"
                        )
                        (strcat pf (itoa *al:num) sf)
                      )
                    )
                    nil
                  )                      
                  ( (vl-consp p1)

                    (setq el (entlast))
                    (vl-cmdf "_.-boundary" "_A" "_I" "_N" "" "_O" "_P" "" "_non" p1 "")

                    (if (not (equal el (setq el (entlast))))
                      (progn
                        (setq tx
                          (cons
                            (_text acspc
                              (_centroid acspc (list (vlax-ename->vla-object el)))
                              (strcat pf (itoa *al:num) sf)
                              ts
                              ucsxang
                            )
                            tx
                          )
                        )
                        (vla-insertrows tb (setq n 2) th 1)
                        (vla-settext tb n 1 (strcat ap (rtos (* cf (vlax-curve-getarea el)) 2) as))
                        (vla-settext tb n 0
                          (if fd
                            (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                              (_GetObjectID acdoc (car tx)) ">%).TextString>%"
                            )
                            (strcat pf (itoa *al:num) sf)
                          )
                        )
                        (redraw el 3)
                        nil
                      )
                      (vla-delete tb)
                    )
                  )
                )
              )
            )
            (not (vlax-erased-p tb))
          )
          (
            (and
              (setq tb
                (_Select "\nSelect Table to Add to: "
                 '(lambda ( x ) (eq "ACAD_TABLE" (cdr (assoc 0 (entget x))))) entsel nil
                )
              )
              (< 1 (vla-get-columns (setq tb (vlax-ename->vla-object tb))))
            )
            (setq n (1- (vla-get-rows tb)) *al:num (1- *al:num))
          )
        )
        (progn
          (while
            (if om
              (setq p1
                (_Select (strcat "\nSelect Object [" (if tx "Undo/" "") "Pick] <Exit>: ")
                 '(lambda ( x )
                    (and
                      (vlax-property-available-p (vlax-ename->vla-object x) 'area)
                      (not (eq "HATCH" (cdr (assoc 0 (entget x)))))
                      (or (eq "REGION" (cdr (assoc 0 (entget x)))) (vlax-curve-isclosed x))
                    )
                  )
                  entsel (list (if tx "Undo Pick" "Pick"))
                )
              )
              (progn (initget (if tx "Undo Object" "Object"))
                (setq p1 (getpoint (strcat "\nPick Area [" (if tx "Undo/" "") "Object] <Exit>: ")))
              )
            )
            (cond
              ( (and tx (eq "Undo" p1))

                (if el (progn (entdel el) (setq el nil)))
                (vla-deleterows tb n 1)
                (vla-delete (car tx))
                (setq n (1- n) tx (cdr tx) *al:num (1- *al:num))
              )
              ( (eq "Undo" p1)

                (princ "\n--> Nothing to Undo.")
              )
              ( (eq "Object" p1)

                (if el (progn (entdel el) (setq el nil)))
                (setq om t)
              )
              ( (eq "Pick" p1)

                (setq om nil)
              )
              ( (and om (eq 'ENAME (type p1)))

                (setq tx
                  (cons
                    (_text acspc
                      (_centroid acspc (list (setq p1 (vlax-ename->vla-object p1))))
                      (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
                      ts
                      ucsxang
                    )
                    tx
                  )
                )
                (vla-insertrows tb (setq n (1+ n)) th 1)
                (vla-settext tb n 1
                  (if fd
                    (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                      (_GetObjectID acdoc p1) ">%).Area \\f \"" fo "\">%"
                    )
                    (strcat ap (rtos (* cf (vla-get-area p1)) 2) as)
                  )
                )
                (vla-settext tb n 0
                  (if fd
                    (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                      (_GetObjectID acdoc (car tx)) ">%).TextString>%"
                    )
                    (strcat pf (itoa *al:num) sf)
                  )
                )
              )               
              ( (vl-consp p1)      

                (if el (progn (entdel el) (setq el nil)))
                (setq el (entlast))
                (vl-cmdf "_.-boundary" "_A" "_I" "_N" "" "_O" "_P" "" "_non" p1 "")

                (if (not (equal el (setq el (entlast))))
                  (progn
                    (setq tx
                      (cons
                        (_text acspc
                          (_centroid acspc (list (vlax-ename->vla-object el)))
                          (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
                          ts
                          ucsxang
                        )
                        tx
                      )
                    )
                    (vla-insertrows tb (setq n (1+ n)) th 1)
                    (vla-settext tb n 1 (strcat ap (rtos (* cf (vlax-curve-getarea el)) 2) as))
                    (vla-settext tb n 0
                      (if fd
                        (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                          (_GetObjectID acdoc (car tx)) ">%).TextString>%"
                        )
                        (strcat pf (itoa *al:num) sf)
                      )
                    )
                    (redraw el 3)
                  )
                  (princ "\n--> Error Retrieving Area.")
                )
              )
            )
          )
          (if el (progn (entdel el) (setq el nil)))
        )
      )
    )
    (
      (and
        (setq fl (getfiled "Create Output File" (cond ( *file* ) ( "" )) "txt;csv;xls" 1))
        (setq of (open fl "w"))
      )
      (setq *file*  (vl-filename-directory fl)
            de      (cdr (assoc (strcase (vl-filename-extension fl) t) '((".txt" . "\t") (".csv" . ",") (".xls" . "\t"))))
            *al:num (1- *al:num)
      )
      (write-line h1 of)
      (write-line (strcat t1 de t2) of)

      (while
        (if om
          (setq p1
            (_Select (strcat "\nSelect Object [Pick] <Exit>: ")
             '(lambda ( x )
                (and
                  (vlax-property-available-p (vlax-ename->vla-object x) 'area)
                  (not (eq "HATCH" (cdr (assoc 0 (entget x)))))
                  (or (eq "REGION" (cdr (assoc 0 (entget x)))) (vlax-curve-isclosed x))
                )
              )
              entsel '("Pick")
            )
          )
          (progn (initget "Object") (setq p1 (getpoint (strcat "\nPick Area [Object] <Exit>: "))))
        )
        (cond
          ( (eq "Object" p1)

            (if el (progn (entdel el) (setq el nil)))
            (setq om t)
          )
          ( (eq "Pick" p1)

            (setq om nil)
          )
          ( (eq 'ENAME (type p1))

            (_text acspc
              (_centroid acspc (list (setq p1 (vlax-ename->vla-object p1))))
              (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
              ts
              ucsxang
            )           
            (write-line (strcat pf (itoa *al:num) sf de ap (rtos (* cf (vla-get-area p1)) 2) as) of)
          )
          ( (vl-consp p1)
        
            (if el (progn (entdel el) (setq el nil)))
            (setq el (entlast))
            (vl-cmdf "_.-boundary" "_A" "_I" "_N" "" "_O" "_P" "" "_non" p1 "")

            (if (not (equal el (setq el (entlast))))
              (progn
                (_text acspc
                  (_centroid acspc (list (vlax-ename->vla-object el)))
                  (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
                  ts
                  ucsxang
                )
                (write-line (strcat pf (itoa *al:num) sf de ap (rtos (* cf (vlax-curve-getarea el)) 2) as) of)
                (redraw el 3)
              )
              (princ "\n--> Error Retrieving Area.")
            )
          )
        )
      )
      (if el (progn (entdel el) (setq el nil)))
      (setq of (close of))
      (_Open (findfile fl))
    )      
  )
  (setenv "LMAC_AreaLabel" (if om "1" "0"))
  (setvar 'CMDECHO cm)
  (_EndUndo acdoc)
  (princ)
)

;;------------------------------------------------------------;;

(vl-load-com)
(princ)
(princ "\n:: AreaLabel.lsp | Version 1.9 | © Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Commands: \"AT\" for ACAD Table, \"AF\" for File ::")
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;
