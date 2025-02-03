;;  XRP2004.lsp   	   by Mark McDonough
;;  V3.3                   Oct.15, 1999
;;  Updated as XRP2004.lsp by Mark McDonough, 4/20/04


;;  Rev.1 05/03/99 updated to work for renamed XREFs and for AK2
;;  Rev.2 10/10/99 updated to work for Image paths
;;  Rev 3 04/20/04 updated to remove restrictions on long filenames
;;  Rev 4 12/14/06 updated to allow loading in running in AutoCAD 2007 (although it's not been tested in 2007)
;;  Rev 5 12/06/08 updated by Jimmy Bergmark
;;                 updated to allow running in AutoCAD 2010
;;                 updated to be possible to run without user interaction

;;  This lisp utility searches the block table, finds all Xrefs, then 
;;  repaths the Xrefs (if needed) with shorter "relative paths", thus 
;;  enhancing drawing file portability.  The program strips unneeded
;;  drive letter references and superfluous upper level directory path
;;  information, using instead the old DOS CHDIR or "CD ..\.." syntax.

;;  This program is supplied "AS IS".  The author specifically disclaims 
;;  all warranties, expressed or implied, regarding the merchantability
;;  or fitness for any purpose. The author does not warrant that the
;;  XRP program will be free of errors and assumes no liability for 
;;  damages, direct or consequentail, which may result from the use of 
;;  XRP.

;***new error definition
;-----------------------
(defun xrperr (msg)
   (if ce  (setvar "cmdecho" ce))
   (if v_r (setvar "visretain" v_r))
   (if r_a (setvar "regenmode" r_a))
   (setq *error* orgerr)   ;reset previous error def.
   (princ "\nCommand cancelled!")
   (prompt "\n ")
   (princ)
)

;1 loop through block list, see if any regular xrefs, if so count them
;---------------------------------------------------------------------
(defun ifxref (/ flag)
  (setq cnt nil cnttot nil)
  (prompt "\n ")
  (princ "\n")
  ;(princ "\nXRP - XREF Relative Path Utility")

  (if (null (setq blkdat (tblnext "block" T)))
    (princ "\n 0 Xrefs in this drawing.")
  )
  (if (null (setq image_ss (ssget "X" '((0 . "IMAGE")))))
    (princ "\n 0 Images in this drawing.")
  )

  (if blkdat
    (progn
      (setq blkdat 1 cnt 0 flag 0)
      (while blkdat
        (if (= flag 0) (setq blkdat (tblnext "block" T))
         	       (setq blkdat (tblnext "block"))
        )
        (setq flag 1)
        (if blkdat (if (member (cdr (assoc 70 blkdat)) '(4 12 36 44))
                       (setq cnt (1+ cnt))))
      )
    )
  )
  (if (= cnt 0)
      (princ "\n 0 Xrefs in this drawing!")
      (if cnt
       (progn 
         (princ (strcat "\n " (itoa cnt) " Xrefs  in this drawing."))
       )
      )
  )
  (if image_ss
      (progn 
        (setq flag 0)
        (princ (strcat "\n " (itoa (sslength image_ss)) " Images in this drawing (some might be duplicates)."))
      )
  )
  (if (>= cnt 1) (setq cnttot cnt))
  (if image_ss   (setq imagetot (sslength image_ss)))
  ;;(princ "\n   Hit any key to continue.")
  ;;(grread)
  (princ)
)

;the following function is called by the "repath" function, used for the
;summary message to indicate how many XREFs cannot be found (counts XREFs
;that are still attached but can't be found because someone has moved or 
;renamed the XREF drawings.
;---------------------------------------------------------------------------
(defun ifnf (/ flag)
   (setq blkdat 1 flag 0)
   (while blkdat
      (if (= flag 0) (setq blkdat (tblnext "block" T))
         	     (setq blkdat (tblnext "block"))
      )
      (setq flag 1)
      (if blkdat 
         (if (member (cdr (assoc 70 blkdat)) '(4 12)) (setq cntnf (1+ cntnf)))
      )
   )
)

;Step through block list, if item is an xref, determine its position 
;relative to base drawing and then repath it.
;------------------------------------------------------------------------
(defun repath ()
   (setq blkdat 1 flag 0 xnam nil cnt 0 cnt6 0 cntnf 0 cntdr 0)
   (while blkdat
     (if (= flag 0) (setq blkdat (tblnext "block" T))
      	            (setq blkdat (tblnext "block"))
     )
     (setq flag 1 dpath (getvar "dwgprefix"))
     (if blkdat 
       (if (member (cdr (assoc 70 blkdat)) '(4 12))
         (progn
           (setq xpath (strcase (cdr (assoc 1 blkdat)))
                 xnam  (realxnam)
                 xnam2 (strcase (cdr (assoc 2 blkdat)))
           )
           (princ (strcat "\nXref  " XNAM2 (pad) " **Not currently loaded, NOT REPATHED"))
         )
       )
     )
     (if blkdat 
       (if (member (cdr (assoc 70 blkdat)) '(36 44))
         (progn
           (setq xpath (strcase (cdr (assoc 1 blkdat)))
                 xnam  (realxnam)                        
                 xnam2 (strcase (cdr (assoc 2 blkdat)))

               ;;important revision
               ;;XNAM  = real XREF dwg name, stripped from ASSOC 1 fullname, w/o ".dwg"
               ;;      = external XREF name
               ;;XNAM2 = XREF name grabed from ASSOC 2 (always has no ".dwg" extension)
               ;;      = internal XREF name (used for reporting to screen)
               ;;This revision accounts for renamed XREFs!
           )
           (if (and (/= (substr xpath 1 1) (strcase (substr dpath 1 1)))
                    (wcmatch xpath "*:*")
               )
               (progn
                (princ (strcat "\nXref  " XNAM2 (pad) " **On different drive, NOT REPATHED"))
                       (setq cntdr (1+ cntdr))
               )
           (progn
           (trpath)
           (if (wcmatch xpath "*:*")
              (progn
                (compath)
	  	    (if (member (cdr (assoc 70 blkdat)) '(4 12))
                    (princ (strcat "\nXref  " XNAM2 (pad) "  **Not currently loaded, NOT REPATHED"))
       	    )
                (cond   ;***condition for lateral path xrefs... the majority
                        ;---------------------------------------------------
                  ((and (> (strlen X1path) 0) (> (strlen d1path) 0))
                    (progn
                      (repeat (cntsteps d1path)
                         (setq x1path (strcat ".." (chr 92) x1path))
                      )
                      (setq x1path (strcase (strcat x1path (xnamful)) T))
                      (if (findfile x1path) 
                        (progn
                          (command "xref" "path" xnam2 x1path)
                          (princ (strcat "\nXref  " XNAM2 (pad) " Repathed, lateral directory"))
                          (setq cnt (1+ cnt))
                        )
                        (progn
                          (princ (strcat "\nXref  " XNAM2 (pad) " **NOT FOUND, NOT REPATHED"))
                          (setq cntnf (1+ cntnf))
                        )
                      )
                      (setq xnam nil xnam2 nil)
                    )
                  )
                        ;***condition for xrefs directly above main drawing
                        ;--------------------------------------------------
                  ((and (= (strlen X1path) 0) (> (strlen d1path) 0))
                    (progn
                      (repeat (cntsteps d1path)
                         (setq x1path (strcat ".." (chr 92) x1path))
                      )
                      (setq x1path (strcase (strcat x1path (xnamful)) T))
                      (if (findfile x1path) 
                        (progn
                          (command "xref" "path" xnam2 x1path)
                          (princ (strcat "\nXref  " XNAM2 (pad) " Repathed, above base directory"))
                          (setq cnt (1+ cnt))
                        )
                        (progn
                          (princ (strcat "\nXref  " XNAM2 (pad) " **NOT FOUND, NOT REPATHED**"))
                          (setq cntnf (1+ cntnf))
                        )
                      )
                      (setq xnam nil xnam2 nil)
                    )
                  )
                        ;***condition for xrefs at same level as main drawing
                        ;----------------------------------------------------
                  ((and (= (strlen X1path) 0) (= (strlen d1path) 0))
                    (progn 
                      ;(if (or (> (strlen xnam2) 8) (wcmatch xnam2 "* *"))
                        ;(princ (strcat "\nXref  " XNAM2 (pad) " **Long filename used, NOT REPATHED"))
                        (progn
                          (princ (strcat "\nXref  " XNAM2 (pad) " Repathed, to base directory"))
                          (command "xref" "path" xnam2 (xnamful))
                          (setq xnam nil xnam2 nil cnt (1+ cnt))
		         )
                      ;)
                    )
                  )    
                        ;***condition for xrefs below main drawing level
                        ;-----------------------------------------------
                  ((and (> (strlen X1path) 0) (= (strlen d1path) 0))
                      (progn
                      (setq x1path (strcat (homepath) (chr 92) x1path))
                      (setq x1path (strcase (strcat x1path (xnamful)) T))
                      (if (findfile x1path) 
                        (progn (command "xref" "path" xnam2 x1path)
                               (princ (strcat "\nXref  " XNAM2 (pad) " Repathed, below base directory"))
                               (setq cnt (1+ cnt))
                        )
                        (progn
                          (princ (strcat "\nXref  " XNAM2 (pad) " **NOT FOUND, NOT REPATHED**"))
                          (setq cntnf (1+ cntnf))
                        )
                      )
                      (setq xnam nil xnam2 nil) 
                    )
                  )
                );end cond
              )  ;end progn
              (progn  ;***else, no drive letter and path is already localized
                      ;------------------------------------------------------
                (princ (strcat "\nXref  " XNAM2 (pad) " *does not need repathing*"))
                (setq cnt6 (1+ cnt6))
              )
           ) ;end if wcmatch xpath *.*
         ))
         );end progn
       )
     )
   )
)

;***This function lines up Xref path messages... just for looks
;   Numeric value can be increased to accomodate longer names
;--------------------------------------------------------------
(defun pad (/ spacer)
  (setq spacer " ")
  (if (< (strlen XNAM2) 24) 
      (repeat (- 24 (strlen XNAM2)) (setq spacer (strcat " " spacer)))
  )
  (eval spacer)
)

;***Chops path off of DWGPREFIX, isolating name of single base subdirectory
;--------------------------------------------------------------------------
(defun homepath ( / cnt4)
  (setq rchr nil cnt4  (strlen dpath))
  (if (= (substr dpath cnt4 1) "\\") 
    (setq dpath (substr dpath 1 (- (strlen dpath) 1)) cnt4 (1- cnt4))
  )
  (while cnt4
     (setq rchr (substr dpath cnt4 1))
     (if (= rchr "\\") 
        (setq dpath (strcat ".." (substr dpath cnt4)) cnt4 nil)
        (setq cnt4 (1- cnt4))
     )
  )
  (eval dpath)
)

;***Gets real (external) XREF name by parsing ASSOC 1 of the  
;   full xref pathname, (w/o the ".dwg" extension) = XNAM2
;-----------------------------------------------------------
(defun realxnam ( / cnt5)
  (setq rchr nil temp xpath cnt5 (strlen xpath))
  (if (wcmatch temp "*\\*")
   (while cnt5
     (setq rchr (substr temp cnt5 1))
     (if (= rchr "\\") 
        (setq temp (strcat (substr temp (1+ cnt5))) cnt5 nil)
        (setq cnt5 (1- cnt5))
     )
   )
  )
  (if (wcmatch (strcase temp) "*.DWG")
    (setq temp (substr temp 1 (- (strlen temp) 4)))
  )
  (eval temp)
)


;***Trims xref path by removing xref drawing name
;   This function is a necessary test in R12 & R13
;-------------------------------------------------
(defun trpath ()
  (if (wcmatch (strcase xpath) "*.DWG")(setq xpath (substr xpath 1 (- (strlen xpath) 4))))
  (setq xpath (substr xpath 1 (- (strlen xpath) (strlen xnam))))
)

;***Important function to determine xref's position relative to main dwg,
;   chops off path common to xref & base dwg, leaving unique path remainder.
;   The symbol "slashcnt" is used to backtrack the path, to ensure the common
;   path is a whole path that's truncated at a path delimitor, not at a 
;   similar named directory.
;   ----------------------------------------------------------------------
(defun compath (/ cnt2)
   (setq d1path (strcase (substr dpath 3))
         x1path (strcase (substr xpath 3))
         cnt2   (strlen d1path)
         slashcnt 0
   )
   (while (/= cnt2 0)
     (if (= (substr d1path 1 1) (substr x1path 1 1))
       (progn
	 (setq d1path (substr d1path 2) x1path (substr x1path 2))
         (if (or (= (strlen d1path) 0) (= (strlen x1path) 0)) 
             (setq cnt2 0)
         )
   	 (if (wcmatch (substr x1path 1 1) "\\")
             (setq slashcnt 0) (setq slashcnt (1+ slashcnt))
         )
       )
       (setq cnt2 0)
     )
   )
 ; (setq zzz x1path)
   (if (not (wcmatch (substr x1path 1 1) "\\"))
     (progn
       (setq x1path 
         (strcat 
           (substr xpath (- (strlen xpath) (+ slashcnt (- (strlen x1path)2))) (1- slashcnt))
            x1path
         )
       )
     )
   )
)

;***Counts # of backslashes in remainder path, thus determining 
;   number of steps to apply DOS CD ..\ syntax
;   -----------------------------------------------------------
(defun cntsteps (x / cnt3)
   (setq cnt3 0 slash 0 rchr nil)
   (while (/= cnt3 (strlen x))
      (setq cnt3 (1+ cnt3) rchr (substr x cnt3 1))
      (if (= rchr "\\") (setq slash (1+ slash)))
   )
   (eval slash)
)

;***Ensures repathed xref has a ".dwg" extension, for consistency
;   -------------------------------------------------------------
(defun xnamful ()
   (if (not (wcmatch XNAM "*.dwg"))
      (strcase (strcat XNAM ".dwg") T)
   )
)


;Step through IMAGE selection set, determine its position 
;relative to base drawing and then repath it.
;------------------------------------------------------------------------
(defun image_repath ()
  (setq flag 0 xnam nil imgcnt 0 imgok 0 imgnf 0 imgdr 0 #images 0
        image_elist1 (entget (ssname image_ss 0))
        image_elist2 (entget (cdr (assoc 340 image_elist1)))
	image_elist3 (entget (cdr (assoc 330 image_elist2)))
        image_names  (member (assoc 3 image_elist3) image_elist3)
	dpath	     (getvar "dwgprefix")
  )
  (while (/= imgcnt (length image_names));maybe should be less than  <
    (progn
      (setq xnam2 (strcase (cdr (nth imgcnt image_names)))
	    xpath (strcase (cdr (assoc 1 (entget (cdr (nth (1+ imgcnt) image_names))))))
	    image_wholepath xpath
	    istat (cdr (assoc 280 (entget (cdr (nth (1+ imgcnt) image_names)))))
      )		  ;if istat = 0, not loaded
	          ;XNAM gets set via the (img_path) function below
      (img_path)  ;delivers xnam, and modified xpath
      (if (= istat 0)
         (progn
           (princ (strcat "\nImage " XNAM2 (pad) " **Not currently loaded, NOT REPATHED"))
           (setq imgnf (1+ imgnf))
         )
	 (progn  ;eei
           (if (and (/= (substr xpath 1 1) (strcase (substr dpath 1 1)))
                    (wcmatch xpath "*:*")
               )
               (progn
                 (princ (strcat "\nImage " XNAM2 (pad) " **On different drive, NOT REPATHED"))
                 (setq imgdr (1+ imgdr))
	         (setq xnam nil xnam2 nil)
               )
	       (progn ;xyz
		(if (wcmatch xpath "*:*")
		  (progn  ;aai
	           (compath)
	   	   (cond
	     	     ;***condition for lateral path images...the majority
             	     ;---------------------------------------------------
             	     ((and (> (strlen X1path) 0) (> (strlen d1path) 0)) ;1st cond
               	       (progn
                       	  (repeat (cntsteps d1path)
                            (setq x1path (strcat ".." (chr 92) x1path))
                       	  )
                       	  (setq x1path (strcase (strcat x1path xnam)))
                 	  (if (findfile x1path) 
                   	    (progn
                               (princ (strcat "\nImage " XNAM2 (pad) " Repathed, lateral directory"))
                       	       (command "_image" "path" XNAM2 x1path)
                               (setq #images (1+ #images))
                   	    )
                   	    (progn
                               (princ (strcat "\nImage " XNAM2 (pad) " **NOT FOUND, NOT REPATHED"))
                               (setq imgnf (1+ imgnf))
                            )
                 	  )
                 	  (setq xnam nil xnam2 nil)
               	       )
             	    ) ;end 1st COND
                        ;***condition for images directly above main drawing
                        ;---------------------------------------------------
                    ((and (= (strlen X1path) 0) (> (strlen d1path) 0));2nd cond
                      (progn
                         (repeat (cntsteps d1path)
                           (setq x1path (strcat ".." (chr 92) x1path))
                         )
                         (setq x1path (strcase (strcat x1path xnam)))
                         (if (findfile x1path) 
                           (progn
                              (princ (strcat "\nImage " XNAM2 (pad) " Repathed, above base directory"))
                              (command "_image" "path" xnam2 x1path)
                              (setq #images (1+ #images))
                           )
                           (progn
                              (princ (strcat "\nImage " XNAM2 (pad) " **NOT FOUND, NOT REPATHED**"))
			      (setq x xnam y xpath x2 xnam2)
                              (setq imgnf (1+ imgnf))
                           )
                         )
                         (setq xnam nil xnam2 nil)
                      )
                     );end 2nd cond
                        ;***condition for images at same level as main drawing
                        ;-----------------------------------------------------
                    ((and (= (strlen X1path) 0) (= (strlen d1path) 0))
                      (progn 
                        ;(if (or (> (strlen xnam2) 8) (wcmatch xnam2 "* *")) 
                           ;(princ (strcat "\nImage " XNAM2 (pad) " **Long filename used, NOT REPATHED"))
			   (progn
                             (princ (strcat "\nImage " XNAM2 (pad) " Repathed, to base directory"))
                             (command "_image" "path" xnam2 xnam)
                             (setq xnam nil xnam2 nil #images (1+ #images))
                           )
                        ;)
                      )
                    )
                        ;***condition for images below main drawing level
                        ;------------------------------------------------
                    ((and (> (strlen X1path) 0) (= (strlen d1path) 0))
                      (progn
                         (setq x1path (strcat (homepath) (chr 92) x1path))
                         (setq x1path (strcase (strcat x1path xnam)))
                         (if (findfile x1path) 
                            (progn
                               (princ (strcat "\nImage " XNAM2 (pad) " Repathed, below base directory"))
			       (command "_image" "path" xnam2 x1path)
                               (setq #images (1+ #images))
                            )
                            (progn
                               (princ (strcat "\nImage " XNAM2 (pad) " **NOT FOUND, NOT REPATHED**"))
			       (setq x xnam y xpath x2 xnam2)
                               (setq imgnf (1+ imgnf))
                            )
                         )
                         (setq xnam nil xnam2 nil) 
                       )
                     )
	     
	   	  )   ;end Whole COND
		 )    ;end progn aai
              	 (progn  ;***else, no drive letter and path is already localized
                         ;------------------------------------------------------
                    (princ (strcat "\nImage " XNAM2 (pad) " *does not need repathing*"))
                    (setq imgok (1+ imgok))
                 )
		)     ;end if wcmatch
	      )       ;end progn xyz


	   ;=======
	   
	 );end if
        ) ;end progn eei
      )   ;end if istat
      (setq imgcnt (+ 2 imgcnt))
    )     ;end progn
  )	  ;end while
)	  ;end defun

(defun img_path ( / imgcnt2)
  (setq rchr nil imgcnt2  (strlen xpath))
  (if (= (substr xpath imgcnt2 1) "\\") 
      (setq xpath (substr xpath 1 (- (strlen xpath) 1)) imgcnt2 (1- imgcnt2))
  )  ;this will probably never be used
  (while (> imgcnt2 0)
     (setq rchr (substr xpath imgcnt2 1))
     (if (= rchr "\\") 
        (setq xnam (strcase (substr xpath (1+ imgcnt2)))
	      ;img_pathnam (strcat (strcase (substr xpath 1 (1- imgcnt2))) "\\")
	      xpath (strcase (substr xpath 1 imgcnt2))
	      imgcnt2 nil)
        (setq imgcnt2 (1- imgcnt2))
     )
  )
)


;Main xref repath function
;-------------------------
(defun xrp ( ) 
  (setq sca "scale" orgerr *error* *error* xrperr v# "v3")  
  (if (or (wcmatch (getvar "acadver") "*14*")(wcmatch (getvar "acadver") "*15*")
            (wcmatch (getvar "acadver") "*16*")(wcmatch (getvar "acadver") "*17*")
	  (wcmatch (getvar "acadver") "*18*")
       )
   (progn           
     (ifxref)
     (if (or image_ss (> cnt 0))
       (progn
	 (setq ce (getvar "cmdecho"))
   	 (setvar "cmdecho" 0)
         (setq r_a (getvar "regenmode"))
         (setq v_r (getvar "visretain"))
         (setvar "regenmode" 0)
         (setvar "visretain" 1)
         (repath)
	 (if image_ss (image_repath))
         (if (>= cnt 1) (command "regen"))
         (setvar "regenmode" r_a)
         ;;(textscr)
	 (xrp_report)
         (princ "\n ")
         (princ "\n ")
         ;;(princ "\nHit any key")
         ;;(grread) 
         (princ "\r           ")
         ;;(graphscr)
         (if v_r (setvar "visretain" v_r))
	 (if ce  (setvar "cmdecho" ce))
         (setq v_r nil ce nil cnt nil r_a nil flag nil rchr nil)
       )
     )
   )
   (alert "This version of XRP is designed for AutoCAD R14 - 2010 only")
  )
  (setq *error* orgerr orgerr nil)
  ;;(princ "\n Done with XRP - Xref Relative Path Utility v3.2 by MMcD")
  (princ)
)

(defun xrp_report ()
   (if (>= cnt 1)   ;***If xrefs exist, report status of repath operations
     (progn
       (prompt "\n ")
       (princ (strcat "\n"  (if (< cnttot 10) " " "") (itoa cnttot) " Xrefs Total"))
       (princ (strcat "\n-" (if (< cnt 10) "-" "") "-------------"))
       (princ (strcat "\n"  (if (< cnt 10) " " "") (itoa cnt) " Xrefs repathed"))
       (if (>= cnt6 1) 
           (princ (strcat "\n" (if (< cnt6 10) " " "") (itoa cnt6) 
                          " Xrefs did not need repathing"))
       )
       (ifnf)
       (if (>= cntdr 1)
           (princ (strcat "\n" (if (< cntdr 10) " " "") (itoa cntdr)  
                          " Xrefs on different drive, NOT REPATHED"))
       )
       (if (>= cntnf 1)
           (princ (strcat "\n" (if (< cntnf 10) " " "") (itoa cntnf)  
                          " Xrefs could not be found, NOT REPATHED"))
       )
       (prompt "\n ")
       (setq cnttot nil cnt6 nil cntdr nil cntnf nil)
     )
   )
   (if (> #images 0)
     (progn
       (prompt "\n ")
       (princ (strcat "\n"  (if (< (/(length image_names)2)10) " " "") (itoa (/(length image_names)2)) 
                      " Different Images Total"))
       (princ (strcat "\n-" (if (< #images 10) "-" "") "-----------------------"))
       (princ (strcat "\n"  (if (< #images 10) " " "") (itoa #images) " Images repathed"))
       (if (>= imgok 1) 
           (princ (strcat "\n" (if (< imgok 10) " " "") (itoa imgok) 
                          " Images did not need repathing"))
       )
       (if (>= imgdr 1)
           (princ (strcat "\n" (if (< imgdr 10) " " "") (itoa imgdr)  
                          " Images on different drive, NOT REPATHED"))
       )
       (if (>= imgnf 1)
           (princ (strcat "\n" (if (< imgnf 10) " " "") (itoa imgnf)  
                          " Images could not be found, NOT REPATHED"))
       )
       (prompt "\n ")
       ;(setq image_ss nil #images nil imgok nil imgdr nil imgnf nil)
     )
   )
)

(defun c:xrp () (xrp))

; Remove the ; in the row below to have the function automatically run when the lisp is loaded.
;(xrp)
(princ)