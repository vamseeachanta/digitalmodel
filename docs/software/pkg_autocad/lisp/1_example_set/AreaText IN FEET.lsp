;;; AreaText.LSP ver 2.0
;;; Select a polyline and where to place the text
;;; Sample result: 2888.89 SQ. FT.

;;; By Jimmy Bergmark
;;; Copyright (C) 2007-2009 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2007-09-05 - First release
;;; 2009-08-02 - Updated to work in both modelspace and paperspace

;;; Uses TEXTSIZE for the text height
(defun c:AT (/ entObject entObjectID InsertionPoint ad)
  (vl-load-com)
  (setq	entObject   (vlax-ename->vla-object(car (entsel)))
	entObjectID (vla-get-objectid entObject)
	InsertionPoint    (vlax-3D-Point (getpoint "Select point: "))
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
		(rtos entObjectID 2 0)
		">%).Area \\f \"%pr2%lu2%ct4%qf1 SQ. FT.\">%"
	      ))
)