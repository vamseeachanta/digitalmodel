;;; PageSetup.LSP
;;; Miscellaneous routines related to Page Setup
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2000-04-05 - First release
;;; Tested on AutoCAD 2000

;;; (listPageSetups <AcadDocument>)
;;; (listPageSetups (vla-get-activedocument (vlax-get-acad-object)))
(defun listPageSetups (doc / pc)
  (vlax-for pc (vla-get-plotconfigurations doc)
    (princ (strcat (vla-get-name pc) "\n"))
  )
  (princ)
)

;;; (allPageSetups <AcadDocument>)
;;; (allPageSetups (vla-get-activedocument (vlax-get-acad-object)))
(defun allPageSetups (doc / aps pc)
  (vlax-for pc (vla-get-plotconfigurations doc)
    (setq aps (cons (vla-get-name pc) aps))
  )
  (reverse aps)
)

;;; (allPageSetupsAndModelType <AcadDocument>)
;;; (allPageSetupsAndModelType (vla-get-activedocument (vlax-get-acad-object)))
(defun allPageSetupsAndModelType (doc / aps pc)
  (vlax-for pc (vla-get-plotconfigurations doc)
    (setq aps (cons (cons (vla-get-name pc)
			  (if (= (vla-get-ModelType pc) :vlax-true)
			    "Model"
			    "Layout"
			  )
		    )
		    aps
	      )
    )
  )
  (reverse aps)
)

;;; (allPageSetupsOfModelType <AcadDocument>)
;;; (allPageSetupsOfModelType (vla-get-activedocument (vlax-get-acad-object)))
(defun allPageSetupsOfModelType (doc / aps)
  (vlax-for pc (vla-get-plotconfigurations doc)
    (if (= (vla-get-ModelType pc) :vlax-true)
      (setq aps (cons (vla-get-name pc) aps))
    )
  )
  (vl-sort aps '<)
)

;;; (allPageSetupsOfLayoutType <AcadDocument>)
;;; (allPageSetupsOfLayoutType (vla-get-activedocument (vlax-get-acad-object)))
(defun allPageSetupsOfLayoutType (doc / aps)
  (vlax-for pc (vla-get-plotconfigurations doc)
    (if (= (vla-get-ModelType pc) :vlax-false)
      (setq aps (cons (vla-get-name pc) aps))
    )
  )
  (vl-sort aps '<)
)

;;; (deleteAllPageSetups <AcadDocument>)
;;; (deleteAllPageSetups (vla-get-activedocument (vlax-get-acad-object)))
(defun deleteAllPageSetups (doc)
  (vlax-for pc (vla-get-plotconfigurations doc)
    (vla-delete pc)
  )
)

;;; (deletePageSetup <AcadDocument> <PageSetupName>)
;;; (deletePageSetup (vla-get-activedocument (vlax-get-acad-object)) "PageSetupName")
(defun deletePageSetup (doc name)
  (vlax-for pc (vla-get-plotconfigurations doc)
    (if (= (strcase (vla-get-name pc)) (strcase name))
      (vla-delete pc)
    )
  )
)

;;; add a new page setup name to current layout-type based on current plot configuration
;;; (addPageSetup <AcadDocument> <PageSetupName>)
;;; (addPageSetup (vla-get-activedocument (vlax-get-acad-object)) "PageSetupName")
(defun addPageSetup (doc name / space pc lay)
  (deletePageSetup doc name)
  (if (= (getvar "ctab") "Model")
    (setq space :vlax-true
          lay (vla-get-Layout (vla-get-ModelSpace
                (vla-get-activedocument (vlax-get-acad-object)))))
    (setq space :vlax-false
          lay (vla-get-ActiveLayout (vla-get-activedocument
                (vlax-get-acad-object))))
  )
  (setq pc (vla-add
             (vla-get-plotconfigurations doc)
             name
             space))
  (vla-CopyFrom pc lay)
  (vla-put-name pc name)
)

;;; (getPageSetupName "Model")
;;; (getPageSetupName "Layout1")
;;; (getPageSetupName (getvar "ctab"))
;;; return value: PageSetupName or nil if Page Setup Name doesn't exist
(defun getPageSetupName (layout / laydict psn)
  (setq dn (cdr (assoc -1 (dictsearch (namedobjdict) "ACAD_LAYOUT"))))
  (setq laydict (dictsearch dn layout))
  (setq psn (member '(100 . "AcDbPlotSettings") laydict))
  (if (= (caadr psn) 1)			; Page Setup Name exist
    (setq psn (cdadr psn))
  )
)

;;; (getAllPageSetupName <AcadDocument>)
;;; (getAllPageSetupName (vla-get-activedocument (vlax-get-acad-object)))
;;; Example return: (("Model" . "PageSetupName") ("Layout1" . "PPA") ("Layout2"))
;;;                 Layout2 has no page setup name
(defun getAllPageSetupName (doc / layoutitem doc lst)
  (foreach layoutitem (layout-tab-list doc)
    (setq lst (cons (cons layoutitem (getPageSetupName layoutitem)) lst))
  )
  (reverse lst)
)

;; (layout-tab-list <AcadDocument> )
;;
;; Returns a list of tne names of all
;; layouts in the specified document,
;; in ascending tab-order.
;; TonyT
;; (layout-tab-list (vla-get-activedocument (vlax-get-acad-object)))
(defun layout-tab-list (doc / layouts)
   (mapcar 'vla-get-name
      (vl-sort
         (vlax-for layout (vla-get-layouts doc)
            (setq layouts (cons layout layouts))
         )
        '(lambda (a b)
            (< (vla-get-taborder a)
               (vla-get-taborder b)
            )
         )
      )
   )
)

;;; (layout-tab-list <AcadDocument> <Layout> <PageSetupName>)
;;; (setPageSetupName (vla-get-activedocument (vlax-get-acad-object)) "Model" "PageSetupName")
(defun setPageSetupName	(doc layout newpsn / pc layoutitem exist1 exist2)
  (vlax-for layoutitem (vla-get-Layouts doc)
    (if	(= (strcase (vla-get-name layoutitem)) (strcase layout))
      (setq exist1 T)
    )
  )
  (if exist1	; layout exist
    (vlax-for pc (vla-get-plotconfigurations doc)
      (if (and
	    (= (strcase (vla-get-name pc)) (strcase newpsn))
	    (if	(= (strcase layout) "MODEL")
	      (= (vla-get-ModelType pc) :vlax-true)
	      (= (vla-get-ModelType pc) :vlax-false)
	    )
	  )
	(setq exist2 T)
      )
    )
  )
  (if exist2	; page setup name exist for selected model type
    (command "._plot" "_n" layout newpsn "" "" "_y" "_n")
  )
)




