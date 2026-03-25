;;CADALYST 09/03 AutoLISP Solutions
;;; PLINE-3D-2D.LSP - a program to convert
;;; 3D polylines to 2D
;;; Program by Tony Hotchkiss

(defun pline-3d-2d ()
  (vl-load-com)
  (setq	*thisdrawing* (vla-get-activedocument
			(vlax-get-acad-object)
		      ) ;_ end of vla-get-activedocument
	*modelspace*  (vla-get-ModelSpace *thisdrawing*)
  ) ;_ end of setq
  (setq	3d-pl-list
	 (get-3D-pline)
  ) ;_ end of setq
  (if 3d-pl-list
    (progn
      (setq vert-array-list (make-list 3d-pl-list))
      (setq n (- 1))
      (repeat (length vert-array-list)
	(setq vert-array (nth (setq n (1+ n)) vert-array-list))
	(setq lyr (vlax-get-property (nth n 3d-pl-list) 'Layer))
	(setq obj (vla-AddPolyline *modelspace* vert-array))
	(vlax-put-property obj 'Layer lyr)
      ) ;_ end of repeat
      (foreach obj 3d-pl-list (vla-delete obj))
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of pline-3d-2d

(defun get-3D-pline ()
  (setq	pl3dobj-list nil
	obj	     nil
	3d	     "AcDb3dPolyline"
  ) ;_ end of setq
  (setq selsets (vla-get-selectionsets *thisdrawing*))
  (setq ss1 (vlax-make-variant "ss1"))
  (if (= (vla-get-count selsets) 0)
    (setq ssobj (vla-add selsets ss1))
  ) ;_ end of if
  (vla-clear ssobj)
  (setq Filterdata (vlax-make-variant "POLYLINE"))
  (setq no-ent 1)
  (while no-ent
    (vla-Selectonscreen ssobj)
    (if	(> (vla-get-count ssobj) 0)
      (progn
	(setq no-ent nil)
	(setq i (- 1))
	(repeat	(vla-get-count ssobj)
	  (setq
	    obj	(vla-item ssobj
			  (vlax-make-variant (setq i (1+ i)))
		) ;_ end of vla-item
	  ) ;_ end of setq
	  (cond
	    ((= (vlax-get-property obj "ObjectName") 3d)
	     (setq pl3dobj-list
		    (append pl3dobj-list (list obj))
	     ) ;_ end of setq
	    )
	  ) ;_ end-of cond
	) ;_ end of repeat
      ) ;_ end of progn
      (prompt "\nNo entities selected, try again.")
    ) ;_ end of if
    (if	(and (= nil no-ent) (= nil pl3dobj-list))
      (progn
	(setq no-ent 1)
	(prompt "\nNo 3D-polylines selected.")
	(quit)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of while  
  (vla-delete (vla-item selsets 0))
  pl3dobj-list
) ;_ end of get-3D-pline


(defun get-3D-pline-old ()
  (setq no-ent 1)
  (setq	filter '((-4 . "<AND")
		 (0 . "POLYLINE")
		 (70 . 8)
		 (-4 . "AND>")
		)
  ) ;_ end of setq
  (while no-ent
    (setq ss	       (ssget filter)
	  k	       (- 1)
	  pl3dobj-list nil
	  obj	       nil
	  3d	       "AcDb3dPolyline"
    ) ;_ end-of setq
    (if	ss
      (progn
	(setq no-ent nil)
	(repeat	(sslength ss)
	  (setq	ent (ssname ss (setq k (1+ k)))
		obj (vlax-ename->vla-object ent)
	  ) ;_ end-of setq
	  (cond
	    ((= (vlax-get-property obj "ObjectName") 3d)
	     (setq pl3dobj-list
		    (append pl3dobj-list (list obj))
	     ) ;_ end of setq
	    )
	  ) ;_ end-of cond
	) ;_ end-of repeat
      ) ;_ end-of progn
      (prompt "\nNo 3D-polylines selected, try again.")
    ) ;_ end-of if
  ) ;_ end-of while
  pl3dobj-list
) ;_ end of get-3D-pline-old

(defun make-list (p-list)
  (setq	i (- 1)
	vlist nil
	calist nil
  ) ;_ end of setq
  (repeat (length p-list)
    (setq obj	 (nth (setq i (1+ i)) p-list)
	  coords (vlax-get-property obj "coordinates")
	  ca	 (vlax-variant-value coords)
    ) ;_ end-of setq
    (setq calist (append calist (list ca)))
  ) ;_ end-of repeat
) ;_ end-of make-list

(defun c:pl32 ()
  (pline-3d-2d)
  (princ)
) ;_ end of pl32

(prompt "Enter PL32 to start: ")
