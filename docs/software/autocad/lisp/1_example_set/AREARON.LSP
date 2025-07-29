;;; CADALYST 07/08  www.cadalyst.com/code 
;;; Tip 2292: AreaRon.lsp	Area of Closed Polylines	(c) 2008 Ronald Maneja

;;; PRODUCES TEXT CONTAINING AREA OF  SELECTED CLOSED POLYLINES
;;; AND PUTS THEM IN AREARON LAYER
;;; CREATED BY RON MANEJA 31JAN08
;;; USER INPUTS: SCALE, POLYLINE SELECTION
;;;



(defun C:AREARON (/
		  allx
		  ally
		  areaobj
		  counter
		  ctr
		  el
		  entity-name
		  entnamevla
		  mysset
		  pt
		  tst
		  vertex
		  x
		  y
		 )
  (vl-load-com)
  (COMMAND "_.UNDO" "BE")
  (set_var)
  (if (tblsearch "Layer" "AREARON")
    (command "._layer"
	     "_thaw"
	     "AREARON"
	     "_on"
	     "AREARON"
	     "_unlock"
	     "AREARON"
	     "_set"
	     "AREARON"
	     ""
    ) ;_ closes command
    (command "._layer"
	     "_make"
	     "AREARON"
	     "_color"
	     1
	     "AREARON"
	     ""
    ) ;_ closes command
  )
  (if (null sch)
    (setq sch 1.0)
  )
  (initget 6)
  (setq	temp (getreal (strcat "\nENTER SCALE <"
			      (rtos sch 2 2)
			      ">: "
		      )
	     )
  )
  (if temp
    (setq sch temp)
    (setq temp sch)
  )

  (if (null precision)
    (setq precision 1)
  )
  (initget 6)
  (setq	prec_temp (getint (strcat "\nHOW MANY DECIMAL PLACES?: <"
				  (rtos precision 2 2)
				  ">: "
			  )
		  )
  )
  (if prec_temp
    (setq precision prec_temp)
    (setq prec_temp precision)
  )
  (prompt "\nSELECT CLOSED POLYLINES:> ")
  (setq
    mysset  (ssget '((0 . "POLYLINE,LWPOLYLINE") (-4 . "&") (70 . 1)))
    counter 0
  )
  (if mysset
    (progn
      (while (< counter (sslength mysset))
	(setq allx	  0
	      ally	  0
	      ctr	  0
	      tst	  1
	      entity-name (ssname mysset counter)
	      EL	  (entget entity-name)
	      entnamevla  (vlax-ename->vla-object entity-name)
	      areaobj	  (vla-get-area entnamevla)
	)
	(while (assoc 10 el)
	  (setq	vertex (cdr (assoc 10 el))
		ctr    (+ ctr 1)
		x      (car vertex)
		y      (cadr vertex)
		allx   (+ allx x)
		ally   (+ ally y)
		EL     (cdr (member (assoc 10 el) el))
	  )
	)
	(setq x	 (/ allx ctr)
	      y	 (/ ally ctr)
	      pt (list x y)
	)
	(command "text" "j" "mc"
		 pt
		 (* sch 2.5)
		 "0"
		 (rtos areaobj 2 precision)
	)
	(setq counter (+ counter 1))
      )
    )
    (alert "\nNO CLOSED POLYLINES/LWPOLYLINES IN YOUR SELECTION"
    )
  )
  (reset_var)
  (princ)
(COMMAND "_.UNDO" "END")
)
(princ)


(defun set_var ()
  (setq oldlayer (getvar "clayer"))
  (setq oldsnap (getvar "osmode"))
  (setq temperr *error*)
  (setq *error* traperror)
  (setvar "osmode" 0)
  (princ)
)


(defun traperror (errmsg)
  (command nil nil nil)
  (if (not (member errmsg '("console break" "Function Cancelled"))
      )
    (princ (strcat "\nError: " errmsg))
  )
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldsnap)
  (princ "\nError Resetting Enviroment ")
  (setq *error* temperr)
  (princ)
)



(defun reset_var ()
  (setq *error* temperr)
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldsnap)
  (princ)
)





