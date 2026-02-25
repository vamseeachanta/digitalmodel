;;; AreaText.LSP ver 3.0
;;; Command name is AT
;;; Select a polyline and where to place the text
;;; Sample result: 2888.89 SQ. FT.
;;; As this is a FIELD it is updated based on the FIELDEVAL
;;; or the settings found in the OPTIONS dialog box

;;; By Jimmy Bergmark
;;; Copyright (C) 2007-2010 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2007-09-05 - First release
;;; 2009-08-02 - Updated to work in both modelspace and paperspace
;;; 2010-10-29 - Updated to work also on 64-bit AutoCAD

;;; Uses TEXTSIZE for the text height

(defun Get-ObjectIDx64 (obj / util)
  (setq util (vla-get-Utility (vla-get-activedocument (vlax-get-acad-object))))
  (if (= (type obj) 'ENAME)(setq obj (vlax-ename->vla-object obj)))
  (if (= (type obj) 'VLA-OBJECT)
    (if (> (vl-string-search "x64" (getvar "platform")) 0)
      (vlax-invoke-method util "GetObjectIdString" obj :vlax-False)
      (rtos (vla-get-objectid obj) 2 0)
    )
  )
)

(defun c:AT (/ entObject entObjectID InsertionPoint ad)
  (vl-load-com)
  (setq entObject (vlax-ename->vla-object(car (entsel)))
        entObjectID (Get-ObjectIDx64 entObject)
        InsertionPoint (vlax-3D-Point (getpoint "Select point: "))
        ad (vla-get-ActiveDocument (vlax-get-acad-object))
  )
  (vla-addMText (if (= 1 (vla-get-activespace ad))
    (vla-get-modelspace ad)
    (if (= (vla-get-mspace ad) :vlax-true)
      (vla-get-modelspace ad)
      (vla-get-paperspace ad)
    )
  )
  InsertionPoint 0.0 (strcat
  "%<\\AcObjProp Object(%<\\_ObjId "
  entObjectID
  ">%).Area \\f \"%pr2%lu2%ct4%qf1 SQ. FT.\">%"
  ))
)