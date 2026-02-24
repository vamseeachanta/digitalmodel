;;;CADALYST 04/06   Tip2107: acad table to excel.lsp   AutoCAD Excel Interface   (c) 2006 Q.J. Chen 


;; The Aim of the Rountine: To transfer the acad table to excel
;; Notes: Now the program can only deal with the table drawed by line or lwpolyline, and the 
;;        table grid are N*M, not irregular, the rountine is tested under office97
;;        and I think it can also run under the after office version.

;; The command name :a2x

;  by qjchen at http://www.xdcad.net 2006.02.07
;; South China University of Technology, Architecture Department
;; Some code taken from a Korea Friend, Thanks to them


;; The main program
(defun c:a2x (/ p1 p2 p3 p4 p1a p2a p1b p4b pvlist phlist palllist newpvlist
		i j column row ss palist en ed ttext fn fh
	     )
  (setvar "osmode" 33)
  (setq p1 (getpoint "\nThe left Up corner point:"))
  (setq p3 (getpoint "\nThe Right Bottom corner point:"))
  (setvar "osmode" 0)
  (setq p2 (list (car p1) (car (cdr p3)) 0))
  (setq p4 (list (car p3) (car (cdr p1)) 0))
  (setq p1a (polar p1 0 1))
  (setq p2a (polar p2 0 1))
  (setq p1b (polar p1 (* pi 1.5) 1))
  (setq p4b (polar p4 pi 1))	       

; Get the Table's horizontal and vertical line's place
  (setq pvlist (vl-Get-Int-Pt p1a p2a))
  (setq pvlist (mapcar
		 '(lambda (x)
		    (polar x pi 1)
		  )
		 pvlist
	       )
  )				      
  (setq phlist (vl-Get-Int-Pt p1b p4b))
  (setq palllist (list pvlist))
  (setq i 1)
  (repeat (- (length phlist) 1)
    (setq newpvlist (mapcar
		      '(lambda (x)
			 (list (car (nth i phlist)) (car (cdr x))
			       (car (cddr x))
			 )
		       )
		      pvlist
		    )
    )
    (setq palllist (append
		     palllist
		     (list newpvlist)
		   )
    )
    (setq i (1+ i))
  )
  (setq column (length palllist))
  (setq row (length (nth 0 palllist)))
  (setq j 0)
  (setq finallist nil)
  (repeat (- row 1)
    (setq i 0
	  rowlist nil
    )
    (repeat (- column 1)
      (setq pa1 (nth j (nth i palllist)))
      (setq pa2 (nth (1+ j) (nth i palllist)))
      (setq pa3 (nth (1+ j) (nth (1+ i) palllist)))
      (setq pa4 (nth j (nth (1+ i) palllist)))
      (setq palist (list pa1 pa2 pa3 pa4))
      (SETQ SS (SSGET "WP" palist))
      (if (/= ss nil)
	(progn
	  (SETQ EN (SSNAME SS 0))
	  (SETQ ED (ENTGET EN))
	  (setq ttext (cdr (assoc 1 ed)))
	  (setq rowlist (append
			  rowlist
			  (list ttext)
			)
	  )
	)
	(setq rowlist (append
			rowlist
			(list " ")
		      )
	)
      )
      (setq i (1+ i))
    )
    (setq finallist (append
		      finallist
		      (list rowlist)
		    )
    )
    (setq j (1+ j))
  )
  
  
  ;;Now all the N horizontal and M vertical lines' intersecting points(N*M) are obtained				      				       
  (setq outlist finallist)	       
  
;;The subrounine to tranfer text to excel
  (2xl outlist)
)


;;; A subrountine from a Korea Friend for obtaining the intersection point of a line through 2 points with many other object
(defun vl-Get-Int-Pt (FirstPoint SecondPoint / acadDocument mSpace SSetName
				 SSets SSet reapp ex obj Baseline
		     )
  (vl-load-com)
  (setq acadDocument (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq mSpace (vla-get-ModelSpace acadDocument))
  (setq SSetName "MySSet")
  (setq SSets (vla-get-SelectionSets acadDocument))
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-add (list SSets
							       SSetName
							 )
			    )
      )
    (vla-clear (vla-Item SSets SSetName))
  )
  (setq SSet (vla-Item SSets SSetName))
  (setq Baseline (vla-Addline mspace (vlax-3d-point FirstPoint)
			      (vlax-3d-point SecondPoint)
		 )
  )
  (vla-SelectByPolygon SSet acSelectionSetFence
		       (kht:list->safearray (append
					      FirstPoint
					      SecondPoint
					    ) 'vlax-vbdouble
		       )
  )
  (vlax-for obj sset (if (setq ex (kht-intersect
						 (vlax-vla-object->ename BaseLine)
						 (vlax-vla-object->ename obj)
				  )
			 )
		       (setq reapp (append
				     reapp
				     ex
				   )
		       )
		     )
  )
  (vla-delete BaseLine)
  (setq reapp (vl-sort reapp '(lambda (e1 e2)
				(< (car e1) (car e2))
			      )
	      )
  )
  reapp
)


;;; My modify to omitting the text objects' intersection
(defun kht-intersect (en1 en2 / a b x ex ex-app c d e)
  (vl-load-com)
  (setq c (cdr (assoc 0 (entget en1)))
	d (cdr (assoc 0 (entget en2)))
  )
  (if (or
	(= c "TEXT")
	(= d "TEXT")
      )
    (setq e -1)
  )
  (setq En1 (vlax-ename->vla-object En1))
  (setq En2 (vlax-ename->vla-object En2))
  (setq a (vla-intersectwith en1 en2 acExtendNone))
  (setq a (vlax-variant-value a))
  (setq b (vlax-safearray-get-u-bound a 1))
  (if (= e -1)
    (setq b e)
  )
  (if (/= b -1)
    (progn
      (setq a (vlax-safearray->list a))
      (repeat (/ (length a) 3)
	(setq ex-app (append
		       ex-app
		       (list (list (car a) (cadr a) (caddr a)))
		     )
	)
	(setq a (cdr (cdr (cdr a))))
      )
      ex-app
    )
    nil
  )
)

(defun kht:list->safearray (lst datatype)
  (vlax-safearray-fill (vlax-make-safearray (eval datatype) (cons 0
								  (1-
								      (length lst)
								  )
							    )
		       ) lst
  )
)
;;; End of the Korea Friend's Subrountine


(defun TerminaExcel ()
  (vlax-release-object *cells*)
  (vlax-release-object *item*)
  (vlax-release-object *workbooks*)
  (vlax-release-object *Excel*)
)

(defun IniciaExcel (/ m)
  (vl-load-com)
  (setq m (vlax-get-or-create-object "excel.application"))
  (if (= (vla-get-visible m) :Vlax-false)
    (vla-put-visible (vlax-get-or-create-object "excel.application") T)
  )

  (setq *Excel* (vlax-get-or-create-object "excel.application"))
  (if (= (vlax-get-property *Excel* "activeworkbook") nil)
    (progn
      (setq *workbooks* (vlax-get-property *Excel* "workbooks"))
      (vlax-invoke-method *workbooks* "add")
      (setq deltaRow nil)
    )
  )

  (setq *workbooks* (vlax-get-property *Excel* "activeworkbook")
	*item* (vlax-get-property *workbooks* "activesheet")
	*cells* (vlax-get-property *item* "cells")
  )
  (if (= (vlax-get-object "Excel.Application") nil)
    (progn
      (vla-put-visible *Excel* T)
    )
  )


)



;; My subrounine to transfer the Table to excel
(defun 2xl (outlist / temp val cll rll cel ccel ccell curid curval curcell)
(IniciaExcel)
(setq list1 (conexcelcolumn))
 (setq curRow 1)
 (if (= deltaRow nil) (setq deltaRow 0))
 (repeat (length outList)
	    (setq temp 1)
	    (repeat (length (nth 0 outlist))
	     (setq val (nth (1- temp) (nth (- curRow 1) outList)))
	     (setq cll (nth temp list1))
	     (setq rll (itoa (+ curRow deltaRow)))
                (setq cel (strcat cll rll))
                (setq curId (strcat (nth temp list1) (itoa (+ curRow deltaRow)))
		    curCell (vlax-variant-value (vlax-invoke-method *item* "Evaluate"
								    curId
						)
			    )
		    curVal (nth (1- temp) (nth (- curRow 1) outList))
	      )
	      
	      (vlax-put-property curCell "Formula" curVal)
	      (vlax-release-object curCell)
	      (setq temp (1+ temp))
	    )
	    (setq curRow (1+ curRow))
	  )
(setq deltaRow  (+ deltaRow (- curRow 0)))
(TerminaExcel)
(princ)
)


;;;Subrouine to produce a list corresponding to the Excel's Column, For Example:A,B,...Z,AA,AB,....
(defun conexcelcolumn (/ a b list1)
  (setq a 65)
  (setq list1 nil)
  (repeat 26
    (setq list1 (append
		  list1
		  (list (chr a))
		)
    )
    (setq a (1+ a))
  )
  (setq a 65)
  (repeat 26
    (setq b 65)
    (repeat 26
      (setq list1 (append
		    list1
		    (list (strcat (chr a) (chr b)))
		  )
      )
      (setq b (1+ b))
    )
    (setq a (1+ a))
  )

  list1
)


(princ)


