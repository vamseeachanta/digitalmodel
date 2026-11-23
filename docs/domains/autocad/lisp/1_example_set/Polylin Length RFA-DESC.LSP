(defun C:FA  (/ acsp adoc cpt elist en ent fld lead_obj lpt mtx oid osm)
  (vl-load-com)
(setq DESC (getstring 1"\n Enter description of the entity: "))
  (or adoc
      (setq adoc
	     (vla-get-activedocument
	       (vlax-get-acad-object)
	       )
	    )
      )
  (if (and
	(= (getvar "tilemode") 0)
	(= (getvar "cvport") 1)
	)
    (setq acsp (vla-get-paperspace adoc))
    (setq acsp (vla-get-modelspace adoc))
    )
 (setq osm (getvar "osmode"))
  (setvar "osmode" 0)

  (while
    (setq ent (entsel "\nSelect pline or hit Enter to exit"))
     (setq en (car ent))
     (if (wcmatch (cdr (assoc 0 (setq elist (entget en)))) "*POLYLINE")
       (progn
	 (setq cpt (trans (cadr ent)1 0)
	       lpt (trans (getpoint cpt "\nPick the ending point of leader:") 1 0)
	       )

	 (setq oID (vla-get-objectid (vlax-ename->vla-object en)))
	 (setq fld
		(strcat
		 (strcat "RFA:ED-"DESC"_Len:" "%<\\AcObjProp Object(%<\\_ObjId "
			  (itoa oID)
			  ">%).Length \\f \"%lu2%pr3\">%"))
	       )
	 (setq mtx (vlax-invoke
		     acsp 'AddMText lpt 0.0 fld)
	       )
	 (vlax-put mtx
		   'AttachmentPoint
		   (cond ((> (car cpt) (car lpt))
			  6
			  )
			 ((< (car cpt) (car lpt))
			  4
			  )
			 (T 4)
			 )
		   )
	 (vlax-put mtx 'Height (getvar "textsize"))
	 (setq lead_obj	(vlax-invoke
			  acsp
			  'Addleader
			  (apply 'append (list cpt lpt))
			  mtx
			  acLineWithArrow
			  )
	       )
	 (vlax-put lead_obj 'VerticalTextPosition 0);1
	 )
       )
     )
  (setvar "osmode" osm)
  (princ)
  )
(princ "\n Start command with FA ...")
(princ)