;;;;Realization {Smirnoff}
;;; TC - Text to Text copy. Copy text from DIMENSION, TEXT, 
;;;MTEXT, ATTRIB, ATTDEF, ACAD_TABLE to one
(defun c:tc (/ actDoc vlaObj sObj sText curObj oldForm
        oType oldMode conFlag errFlag *error*)
  (vl-load-com)
      (setq actDoc(vla-get-ActiveDocument
        (vlax-get-acad-object)))
      (vla-StartUndoMark actDoc)
  (defun TTC_Paste(pasteStr / nslLst vlaObj hitPt
                   hitRes Row Column)
    (setq errFlag nil)
    (if
     (setq nslLst(nentsel "\nPaste text >"))
      (progn
  (cond
    (
     (and
       (= 4(length nslLst))
       (= "DIMENSION"(cdr(assoc 0(entget(car(last nslLst))))))
       ); end and
     (setq vlaObj
      (vlax-ename->vla-object
        (cdr(assoc -1(entget(car(last nslLst)))))))
     (if
       (vl-catch-all-error-p
         (vl-catch-all-apply
     'vla-put-TextOverride(list vlaObj pasteStr)))
         (progn
         (princ "\n Can't paste. Object may be on locked layer. ")
         (setq errFlag T)
         ); end progn
       ); end if
     ); end condition #1
    (
     (and
       (= 4(length nslLst))
       (= "ACAD_TABLE"(cdr(assoc 0(entget(car(last nslLst))))))
       ); end and
     (setq vlaObj
      (vlax-ename->vla-object
        (cdr(assoc -1(entget(car(last nslLst))))))
     hitPt(vlax-3D-Point(trans(cadr nslLst)1 0))
     hitRes(vla-HitTest vlaObj hitPt
        (vlax-3D-Point '(0.0 0.0 1.0)) 'Row 'Column)
           ); end setq
     (if(= :vlax-true hitRes)
     (progn
         (if
     (vl-catch-all-error-p
       (vl-catch-all-apply
         'vla-SetText(list vlaObj Row Column pasteStr)))
     (progn
       (princ "\n Can't paste. Object may be on locked layer. ")
       (setq errFlag T)
       ); end progn
     ); end if
         ); end progn
       ); end if
     ); end condition # 2
    (
     (and
       (= 4(length nslLst))
       (= "INSERT"(cdr(assoc 0(entget(car(last nslLst))))))
       ); end and
     (princ "\nCan't paste to block's DText or MText. Select Attribute ")
     (setq errFlag T)
     ); end condition #3
    (
     (and
       (= 2(length nslLst))
         (member(cdr(assoc 0(entget(car nslLst))))
           '("TEXT" "MTEXT" "ATTRIB" "ATTDEF"))
       ); end and
     (setq vlaObj
      (vlax-ename->vla-object(car nslLst)))
        (if
     (vl-catch-all-error-p
       (vl-catch-all-apply
         'vla-put-TextString(list vlaObj pasteStr)))
    (progn
       (princ "\nError. Can't pase text. ")
      (setq errFlag T)
      ); end progn
     ); end if
     ); end condition #4
    (T
     (princ "\nCan't paste. Invalid object. ")
     (setq errFlag T)
     ); end condition #5
    ); end cond
             T
      ); end progn
            nil
           ); end if
    ); end of TTC_Paste

    (defun TTC_MText_Clear(Mtext / Text Str)
    (setq Text "")
    (while(/= Mtext "")
      (cond
  ((wcmatch
     (strcase
       (setq Str
        (substr Mtext 1 2)))
                     "\\[\\{}`~]")
   (setq Mtext(substr Mtext 3)
         Text(strcat Text Str)
   ); end setq
  ); end condition #1
  ((wcmatch(substr Mtext 1 1) "[{}]")
    (setq Mtext
     (substr Mtext 2))
  ); end condition #2
  (
   (and
   (wcmatch
     (strcase
       (substr Mtext 1 2)) "\\P")
   (/=(substr Mtext 3 1) " ")
    ); end and
         (setq Mtext (substr Mtext 3)
               Text (strcat Text " ")
         ); end setq
   ); end condition #3
  ((wcmatch
     (strcase
       (substr Mtext 1 2)) "\\[LOP]")
    (setq Mtext(substr Mtext 3))
  ); end condition #4
  ((wcmatch
     (strcase
       (substr Mtext 1 2)) "\\[ACFHQTW]")
    (setq Mtext
     (substr Mtext
       (+ 2
          (vl-string-search ";" Mtext))))
  ); end condition #5
  ((wcmatch
     (strcase (substr Mtext 1 2)) "\\S")
    (setq Str(substr Mtext 3 (- (vl-string-search ";" Mtext) 2))
          Text(strcat Text (vl-string-translate "#^\\" " " Str))
          Mtext(substr Mtext (+ 4 (strlen Str)))
   ); end setq
   (print Str)
  ); end condition #6
  (T
   (setq Text(strcat Text(substr Mtext 1 1))
         Mtext (substr Mtext 2)
   )
  ); end condition #7
      ); end cond
    ); end while
  Text
); end of TTC_MText_Clear

  (defun TTC_Copy (/ sObj sText tType actDoc)
   (if
    (and
     (setq sObj(car(nentsel "\nCopy text... ")))
     (member(setq tType(cdr(assoc 0(entget sObj))))
      '("TEXT" "MTEXT" "ATTRIB" "ATTDEF"))
     ); end and
    (progn
      (setq actDoc(vla-get-ActiveDocument
        (vlax-get-Acad-object))
      sText(vla-get-TextString
       (vlax-ename->vla-object sObj))
      ); end setq
      (if(= tType "MTEXT")
  (setq sText(TTC_MText_Clear sText))
  ); end if
      ); end progn
    ); end if
    sText
    ); end of TTC_Copy
  (defun CCT_Str_Echo(paseStr / comStr)
    (if(< 20(strlen paseStr))
      (setq comStr
       (strcat
         (substr paseStr 1 17)"..."))
      (setq comStr paseStr)
      ); end if
    (princ
      (strcat "\nText = \"" comStr "\""))
    (princ)
    ); end of CCT_Str_Echo
    (defun *error*(msg)
    (vla-EndUndoMark
      (vla-get-ActiveDocument
        (vlax-get-acad-object)))
    (princ "\nQuit TTC")
    (princ)
    ); end of *error*
    (if(not ttc:Mode)(setq ttc:Mode "Multiple"))
    (initget "Multiple Pair-wise")
    (setq oldMode ttc:Mode
    ttc:Mode
     (getkword
       (strcat "\nSpecify mode [Multiple/Pair-wise] <" ttc:Mode ">: "))
    conFlag T
    paseStr ""
     ); end setq
    (if(null ttc:Mode)(setq ttc:Mode oldMode))
    (if(= ttc:Mode "Multiple")
      (progn
  (if(and(setq paseStr(TTC_Copy))conFlag)
    (progn
    (CCT_Str_Echo paseStr)
    (while(setq conFlag(TTC_Paste paseStr))T
      ); end while
    ); end progn
    ); end if
  ); end progn
      (progn
  (while
    (and conFlag paseStr)
    (setq paseStr(TTC_Copy))
    (if(and paseStr conFlag)
      (progn
    (CCT_Str_Echo paseStr)
    (setq errFlag T)
    (while errFlag
    (setq conFlag(TTC_Paste paseStr))
         );end while
       ); end progn
      ); end if
    ); end while
  ); end progn
      ); end if
   (vla-EndUndoMark actDoc)
   (princ "\nQuit TTC")
  (princ)
  ); end c:ttc
(princ "\n\t TTC - Text to Text copy. Copy text from DIMENSION, TEXT, MTEXT, ATTRIB, ATTDEF, ACAD_TABLE to one")