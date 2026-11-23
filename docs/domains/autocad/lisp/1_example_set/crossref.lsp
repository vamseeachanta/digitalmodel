
;; CROSSREF.LSP  Copyright 1990-1996  Tony Tanzillo  all rights reserved

;;

;;    Author: Tony Tanzillo, 

;;            Design Automation Consulting

;;            http://ourworld.compuserve.com/homepages/tonyt

;;    Permission to use, copy, modify, and distribute this software

;;    for any purpose and without fee is hereby granted, provided

;;    that the above copyright notice appears in all copies and

;;    that both that copyright notice and the limited warranty and

;;    restricted rights notice below appear in all copies and all

;;    supporting documentation, and that there is no charge or fee 

;;    charged in return for distribution or duplication.

;;

;;    This SOFTWARE and documentation are provided with RESTRICTED 

;;    RIGHTS. 

;;

;;    Use, duplication, or disclosure by the Government is subject 

;;    to restrictions as set forth in subparagraph (c)(1)(ii) of 

;;    the Rights in Technical Data and Computer Software clause at 

;;    DFARS 252.227-7013 or subparagraphs (c)(1) and (2) of the 

;;    Commercial Computer Software Restricted Rights at 48 CFR 

;;    52.227-19, as applicable. The manufacturer of this SOFTWARE 

;;    is Tony Tanzillo, Design Automation Consulting.

;;    

;;    NO WARRANTY

;;    

;;    ANY USE OF THIS SOFTWARE IS AT YOUR OWN RISK. THE SOFTWARE 

;;    IS PROVIDED FOR USE "AS IS" AND WITHOUT WARRANTY OF ANY KIND.  

;;    TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW, THE AUTHOR 

;;    DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING, BUT 

;;    NOT LIMITED TO, IMPLIED WARRANTIES OF MERCHANTABILITY AND 

;;    FITNESS FOR A PARTICULAR PURPOSE, WITH REGARD TO THE SOFTWARE. 

;;

;;    NO LIABILITY FOR CONSEQUENTIAL DAMAGES. TO THE MAXIMUM 

;;    EXTENT PERMITTED BY APPLICABLE LAW, IN NO EVENT SHALL 

;;    THE AUTHOR OR ITS SUPPLIERS BE LIABLE FOR ANY SPECIAL, 

;;    INCIDENTAL, INDIRECT, OR CONSEQUENTIAL DAMAGES WHATSOEVER 

;;    (INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF 

;;    BUSINESS PROFITS, BUSINESS INTERRUPTION, LOSS OF BUSINESS 

;;    INFORMATION, OR ANY OTHER PECUNIARY LOSS) ARISING OUT OF 

;;    THE USE OF OR INABILITY TO USE THE SOFTWARE PRODUCT, EVEN 

;;    IF THE AUTHOR HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH 

;;    DAMAGES.  BECAUSE SOME JURISDICTIONS DO NOT ALLOW EXCLUSION 

;;    OR LIMITATION OF LIABILITY FOR CONSEQUENTIAL OR INCIDENTAL 

;;    DAMAGES, THE ABOVE LIMITATION MAY NOT APPLY TO YOU.



;;	CROSSREF.LSP

;;	  

;;	Adds the CROSSREF command to AutoCAD (R10 THRU R13).

;;	  

;;	Searches block definitions for references to a specified layer,

;;	linetype, style, dimstyle, mlinestyle, or block, and reports the 

;;	names of all blocks that contain at least one reference to the 

;;	specified object(*).

;;	  

;;	Format:

;;	  

;;	Command: CROSSREF

;;	Cross reference Block/LType/Style/DimStyle/Mlinestyle/<LAyer>:  {select object type}

;;	Name of {type} to cross reference:                              {select object name}

;;	Scanning blocks for references to {type} {name}.

;;	Scanning block XXXXXXX....

;;	  

;;	CROSSREF will then display the name of every block definition in the

;;	drawing that contains at least one reference to the specified object.

;;	  

;;	R11 only:

;;	  

;;	An exception is layers.  A layer is always referenced by a block that

;;	was created when the layer was current, but CROSSREF can't detect this

;;	from the block information available to AutoLISP.  The only way to find

;;	out if a layer is referenced as the "creation layer" of a block, is to

;;	DXFOUT and search thru each ENDBLK entity, for references to the layer.



   (defun C:CROSSREF ( / tblname symname tblkey block found e bs)

     (  (lambda (msg)

           (initget "LAyer LType Style Dimstyle Mlinestyle Block")

           (setq tblname (strcase (cond ((getkword msg)) (t "LAyer")) t)))

        "\nCross reference Block/LType/Style/Dimstyle/Mlinestyle/<Layer>: ")



     (cond

        (  (= "" (setq symname

                    (strcase

                       (getstring

                          (strcat "\nName of "

                                  (strcase tblname)

                                  " to cross reference: "))))))

        (  (not (namedobjsearch tblname symname))

           (princ (strcat "\n" tblname  " " symname " not defined.")))

        (t (princ (strcat "\nScanning blocks for references to "

                          tblname " " symname ".\n"))

           (setq found  0

                 tblkey (get tblname '(  ("block" . 2)

                                         ("layer" . 8)

                                         ("ltype" . 6)

                                         ("style" . 7)

                                         ("dimstyle" . 3)

                                         ("mlinestyle" . 2))))

           (princ "\rScanning block: ")

           (while (setq block (tblnext "block" (not block)))

                  (bsprinc (get 2 block))

                  (setq e (get -2 block))

                  (while (and e (/= symname (get tblkey (entget e))))

                         (setq e (entnext e)))

                  (cond (e (setq found (1+ found) bs nil)

                           (princ (strcat "\r" (strfcase tblname)

                                          " " symname " is referenced"

                                          " in definition of block "

                                          (get 2 block)

                                          "\nScanning block: ")))))



           (princ (cond (  (zerop found)

                           (strcat "\nNo nested references to "

                                   tblname " " symname " found."))

                        (t (strcat "\nDone, " tblname " " symname

                                   " is referenced in " (itoa found)

                                   " block definition(s)."))))))

     (princ)

  )



  (setq *rubout* (strcat "\10 \10"))



  (defun rubout (n)

    (repeat n (princ *rubout*))

  )



  (defun bsprinc (s)

    (cond (bs (rubout bs)))

    (setq bs (strlen (princ s)))

  )



  (defun strfcase (s)

     (strcat (strcase (substr s 1 1)) (substr s 2))

  )



  (defun get (k l) (cdr (assoc k l)))

  

  (setq mlinedict (cdr (assoc -1 (dictsearch (namedobjdict) "acad_mlinestyle"))))



  (defun mlinesearch (name)

     (dictsearch mlinedict name)

  )

  

  (defun namedobjsearch (table name)

     (if (eq (strcase table) "MLINESTYLE")

         (mlinesearch name)

         (tblsearch table name)

     )

  )

         





  (princ "\n C:CROSSREF loaded.  Start command with CROSSREF.")

  (princ)



; ------------------------EOF CROSSREF.LSP----------------------------</PRE>

