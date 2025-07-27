;;---------------------=={  Text 2 MText Upgraded  }==---------------------------;;
;;                                                                               ;;
;;  Similar to the Txt2MTxt Express Tools function, but allows the user          ;;
;;  additional control over where the text is placed in the resultant MText.     ;;
;;                                                                               ;;
;;  The user can pick MText or DText, positioning such text using one of two     ;;
;;  modes: "New Line" or "Same Line". The Modes can be switched by pressing      ;;
;;  Space between picks.                                                         ;;
;;                                                                               ;;
;;  The user can also hold shift and pick text to keep the original text in      ;;
;;  place, and press "u" between picks to undo the last text pick.               ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  T2M                                                        ;;
;;                                                                               ;;
;;  Notes:-                                                                      ;;
;;  --------                                                                     ;;
;;  Shift-click functionality requires the user to have Express Tools installed. ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author: Lee Mac, Copyright © September 2009 - www.lee-mac.com                ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version:                                                                     ;;
;;                                                                               ;;
;;  1.0:  27/09/2009  -  First Release                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.1:  29/09/2009  -  Minor Bug Fixes                                         ;;
;;-------------------------------------------------------------------------------;;
;;  1.2:  29/09/2009  -  Fixed Alignment Bug                                     ;;
;;                    -  Added Code to match Height                              ;;
;;-------------------------------------------------------------------------------;;
;;  1.3:  01/10/2009  -  Added option to Copy Text.                              ;;
;;-------------------------------------------------------------------------------;;
;;  1.4:  01/10/2009  -  Added option to Undo Last text selection                ;;
;;-------------------------------------------------------------------------------;;
;;  1.5:  30/03/2010  -  Modified code to allow for mis-click.                   ;;
;;                    -  Updated UndoMarks.                                      ;;
;;-------------------------------------------------------------------------------;;
;;  1.6:  15/04/2010  -  MText objects now have correct width.                   ;;
;;                    -  Accounted for %%U symbol.                               ;;
;;-------------------------------------------------------------------------------;;
;;  1.7:  16/04/2010  -  Fixed %%U bug.                                          ;;
;;                    -  Trimmed Spaces when in 'Same Line' mode.                ;;
;;                    -  Fixed Width when Undo is used.                          ;;
;;                    -  Allowed Shift-Click to keep first text object selected. ;;
;;-------------------------------------------------------------------------------;;
;;  1.8:  10/05/2010  -  Allowed for UCS variations.                             ;;
;;                    -  Matched initial text rotation.                          ;;
;;-------------------------------------------------------------------------------;;
;;  1.9:  21/05/2010  -  Added ability to use SelectionSet to select text.       ;;
;;-------------------------------------------------------------------------------;;
;;  2.0:  07/06/2010  -  Fixed offset from cursor with rotated text.             ;;
;;-------------------------------------------------------------------------------;;

(defun c:t2m ( /  ;; -={ Local Functions }=-

                   *error* align_Mt Get_MTOffset_pt
                   GetTextWidth ReplaceUnderline

                  ;; -={ Local Variables }=-

                  CODE
                  DATA DOC
                  ELST ENT ET
                  FORMFLAG
                  GRDATA
                  LHGT LLST
                  MLST MSG
                  NOBJ NSTR
                  OBJ
                  SHFT SPC
                  TENT TOBJ TEXTSS
                  UFLAG UNDER
                  WLST
              
                  ;; -={ Global Variables }=-

                  ; *T2M_mode*  ~  Mode for line addition
)
  

;     --=={ Sub Functions  }==--      ;

  ;; -={ Error Handler }=-

  (defun *error* (err)
    (and uFlag (vla-EndUndoMark doc))
    (and tObj  (not (vlax-erased-p tObj)) (vla-delete tObj))
    
    (if eLst (mapcar (function entdel)
               (vl-remove-if (function null) eLst)))
    
    (or (wcmatch (strcase err) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " err " **")))
    (princ))

  
 ;;-------------------------------------------------------------------------------;;
  

  (defun align_Mt (obj / al)
    (cond (  (eq "AcDbMText" (vla-get-ObjectName obj))
             (vla-get-AttachmentPoint obj))

          (  (eq "AcDbText" (vla-get-ObjectName obj))
             (setq al (vla-get-Alignment obj))

             (cond (  (<= 0 al 2) (1+ al))
                   (  (<= 3 al 5) 1)
                   (t (- al 5))))))
  

 ;;-------------------------------------------------------------------------------;;
  

  (defun Get_MTOffset_pt ( obj pt / miP maP al )
    (vla-getBoundingBox obj 'miP 'maP)
    (setq miP (vlax-safearray->list miP)
          maP (vlax-safearray->list maP))

    (setq al (vla-get-AttachmentPoint obj) r (vla-get-rotation obj))

    (cond (  (or (eq acAttachmentPointTopLeft   al)
                 (eq acAttachmentPointTopCenter al)
                 (eq acAttachmentPointTopRight  al))
           
             (polar pt (- r (/ pi 2.)) (vla-get-Height obj)))

          (  (or (eq acAttachmentPointMiddleLeft   al)
                 (eq acAttachmentPointMiddleCenter al)
                 (eq acAttachmentPointMiddleRight  al))
           
             (polar pt (- r (/ pi 2.)) (+ (vla-get-Height obj)
                                          (/ (- (cadr maP) (cadr miP)) 2.))))
  
          (  (or (eq acAttachmentPointBottomLeft   al)
                 (eq acAttachmentPointBottomCenter al)
                 (eq acAttachmentPointBottomRight  al))
           
             (polar pt (- r (/ pi 2.)) (+ (vla-get-Height obj)
                                          (- (cadr maP) (cadr miP)))))))
  

 ;;-------------------------------------------------------------------------------;;
  

  (defun GetTextWidth (obj / tBox eLst)
    (cond (  (eq "AcDbText" (vla-get-objectname obj))

             (setq eLst (entget  (vlax-vla-object->ename obj))
                   tBox (textbox
                          (subst
                            (cons 1 (strcat "..." (cdr (assoc 1 eLst))))
                              (assoc 1 eLst) eLst)))

             (- (caadr tBox) (caar tBox)))

          (  (vla-get-Width obj))))
  

 ;;-------------------------------------------------------------------------------;;
  

  (defun ReplaceUnderline (str / i under)
    (if (vl-string-search "%%U" (strcase Str))
      (progn
        (while (and (< i (strlen Str))
                    (setq i (vl-string-search "%%U" (strcase Str) i)))
          (if under
            (setq Str (strcat (substr Str 1 i) "\\l" (substr Str (+ i 4))) i (+ i 4) under nil)
            (setq Str (strcat (substr Str 1 i) "\\L" (substr Str (+ i 4))) i (+ i 4) under t  )))
        
        (if under (setq str (strcat str "\\l")))))
    
    str)
  

;     --=={ Main Function  }==--
 

  (setq doc (vla-get-ActiveDocument
              (vlax-get-acad-object))

        spc (if (or (eq AcModelSpace (vla-get-activespace doc))
                    (eq :vlax-true   (vla-get-MSpace doc)))

              (vla-get-modelspace doc)
              (vla-get-paperspace doc)))
  
  (setq Et
      (and (vl-position "acetutil.arx" (arx))
           (not
             (vl-catch-all-error-p
               (vl-catch-all-apply
                 (function (lambda nil (acet-sys-shift-down))))))))

  (or *T2M_Mode* (setq *T2M_Mode* 0))
  (setq mLst '("New Line " "Same Line"))

  (while
    (progn
      (setq ent (car (entsel "\nSelect Text/MText [Shift-Click keep original]: ")))
      (and et (setq shft (acet-sys-shift-down)))
      
      (cond (  (not ent)
               (princ "\n** Nothing Selected **"))
            
            (  (not (wcmatch (cdr (assoc 0 (entget ent))) "*TEXT"))
               (princ "\n** Object is not Text **")))))

  (setq uFlag (not (vla-StartUndoMark doc)))

  (setq tObj
    (vla-AddMText spc
      
      (vla-get-InsertionPoint
        (setq obj (vlax-ename->vla-object ent))) (GetTextWidth obj)
      
          (ReplaceUnderline (vla-get-TextString obj))))

  (foreach p '(InsertionPoint Layer Color StyleName Height)
    (vlax-put-property tObj p
      (vlax-get-property obj p)))

  (vla-put-rotation tObj
    (if (eq "AcDbText" (vla-get-ObjectName obj))
      (- (vla-get-rotation obj)
         (angle '(0. 0. 0.)
           (trans (getvar 'UCSXDIR) 0 (trans '(0. 0. 1.) 1 0 t))))
      (vla-get-rotation obj)))
  
  (vla-put-AttachmentPoint tObj (align_Mt obj))

  (or (and shft
           (setq eLst (cons nil eLst)))
      (and (entdel ent)
           (setq eLst (cons ent eLst))))

  (princ (eval (setq msg '(strcat "\n~¤~  Current Mode: " (nth *T2M_mode* mLst) " ~¤~   [Space to Change]"
                                  "\n~¤~ Select Text to Convert [Shift-Click keep original] [Undo] <Place MText> ~¤~"))))

  (while
    (progn
      (setq grdata (grread 't 15 2)
            code   (car grdata) data (cadr grdata))

      (cond (  (and (= 5 code) (listp data))

               (vla-put-InsertionPoint tObj
                 (vlax-3D-point
                   (Get_MTOffset_pt tObj (trans data 1 0)))) t)

            (  (and (= 3 code) (listp data))

               (if (and (setq tEnt (car (nentselp data)))
                        (wcmatch (cdr (assoc 0 (entget tEnt))) "*TEXT"))
                 
                 (AddtoMTextSelection tEnt)

                 (progn
                   (vla-put-Visible tObj :vlax-false)

                   (if (setq textss (GetSelectionSet "\nPick Corner Point: " data '((0 . "TEXT,MTEXT"))))
                     (
                       (lambda ( i )
                         (while (setq e (ssname textss (setq i (1+ i))))
                           (AddtoMTextSelection e)
                         )
                         (princ (eval msg))
                       )
                       -1
                     )
                     (princ (strcat "\n** No Text/MText Selected **" (eval msg)))
                   )
                   
                   (vla-put-Visible tObj :vlax-true) t
                 )
               )
            )

            (  (= 25 code) nil)

            (  (= 2 code)

               (cond (  (= 13 data) nil)
                     
                     (  (= 32 data)
                      
                        (setq *T2M_mode* (- 1 *T2M_mode*))
                        (princ (eval msg)))
                     
                     (  (vl-position data '(85 117))
                      
                        (if (< 1 (length eLst))
                          (progn
                            
                            (vla-put-TextString tObj
                              (substr (vla-get-TextString tObj) 1 (car lLst)))

                            (vla-put-Width tObj (car wLst))
                            
                            (if (car eLst) (entdel (car eLst)))
                            (setq eLst (cdr eLst) lLst (cdr lLst) wLst (cdr wLst)) t)
                          
                          (progn
                            (princ "\n** Nothing to Undo **")
                            (princ (eval msg)))))                           
                            
                     (t )))

            (t ))))

  (setq uFlag (vla-EndUndoMark doc))
  (princ))

(defun AddtoMTextSelection ( tEnt / nStr nObj formflag )
  (setq lLst (cons (strlen (vla-get-TextString tObj)) lLst)
        wLst (cons (vla-get-Width tObj) wLst))
  
  (setq nStr
     (vla-get-TextString
       (setq nObj
          (vlax-ename->vla-object tEnt))) formflag nil)
  
  (vla-put-Width tObj
    ((if (= *T2M_mode* 1) + max)
      (vla-get-Width tObj) (GetTextWidth nObj)))
  
  (if (not (or (eq (vla-get-Color nObj) (vla-get-Color tObj))
             (vl-position (vla-get-Color nObj) '(255 0))))
    
    (setq nStr (strcat "\\C" (itoa (vla-get-Color nObj)) ";" nStr) formflag t))
  
  (setq nStr (ReplaceUnderline nStr))
  
  (if (not (or (eq (vla-get-Height nObj) (vla-get-Height tObj))
             (and lHgt (eq (vla-get-Height nObj) lHgt))))
    
    (setq nStr (strcat "\\H" (rtos (/ (float (vla-get-Height nObj))
                                      (cond (lHgt) ((vla-get-Height tObj)))) 2 2)  "x;" nStr)
      lHgt (vla-get-Height nObj) formflag t))
  
  (if (not (eq (vla-get-StyleName nObj) (vla-get-StyleName tObj)))
    (setq nStr
       (strcat "\\F" (vla-get-fontfile
                       (vla-item
                         (vla-get-TextStyles doc)
                         (vla-get-StyleName nObj))) ";" nStr) formflag t))
  
  (if formflag (setq nStr (strcat "{" nStr "}")))
  
  (vla-put-TextString tObj
    (strcat
      (vla-get-TextString tObj)
      (if (zerop *T2M_mode*)
        (strcat "\\P" nStr)
        (strcat " "  (vl-string-left-trim (chr 32) nStr)))))
  
  (vla-update tObj)
  (or (and et (acet-sys-shift-down)
        (setq eLst (cons nil eLst)))
    (and (entdel tEnt)
      (setq eLst (cons tEnt eLst)))) t)


(defun GetSelectionSet ( str pt filter / gr data pt1 pt2 lst )
  (princ str)

  (while (and (= 5 (car (setq gr (grread t 13 0)))) (listp (setq data (cadr gr))))
    (redraw)

    (setq pt1 (list (car data) (cadr pt) (caddr data))
          pt2 (list (car pt) (cadr data) (caddr data)))

    (grvecs
      (setq lst
        (list
          (if (minusp (- (car data) (car pt))) -30 30)
          pt pt1 pt pt2 pt1 data pt2 data
        )
      )
    )
  )

  (redraw)

  (ssget (if (minusp (car lst)) "_C" "_W") pt data filter)
)

(vl-load-com)
(princ "\n:: Text2MText.lsp | Version 2.0 | © Lee Mac 2009 www.lee-mac.com ::")
(princ "\n:: Type \"T2M\" to Invoke ::")
(princ)