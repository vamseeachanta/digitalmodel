;;; a2w.lsp
;;; Exports selected text to Word document
;;; *********************************************************
(defun c:a2w (/ TxtSet Word Docs NewDoc Paragraphs Range
                    OldTxtList NewTxt IdTxtList FontName Txt Count
		    Flag#1 Flag#2)

(defun Dxf (Index)
 (cdr (assoc Index (entget (ssname TxtSet Count))))
);end Dxf

(defun ClearMTFormat (Str / Item TLength Char New);;; clear mtext format in string
  (setq Item 1 TLength (strlen Str) New "")
  (while (<= Item TLength)
   (setq Char (substr Str Item 1))
   (if	(= Char "\\")				
     (progn
      (setq Item (1+ Item))		
      (setq Char (substr Str Item 1))
      (cond
       ((member Char '("\\" "f" "F" "C" "H" "S" "T" "Q" "W"))
        (while (and (/= Char ";") (<= Item TLength))
         (setq Item (1+ Item))
         (setq Char (substr Str Item 1))
        );end while
       )
       ((= Char "P")
        (setq New (strcat New "\n"))
       )
       ((member Char '("{" "}"))
	(setq New (strcat New Char))
       )
      );end cond
     );end progn
     (if (not (member Char '("{" "}")))
      (setq New (strcat New Char))			
     );end if
    );end if
    (setq Item (1+ Item))
   );end while
   (cond (New) (T ""))
);end ClearMTFormat

(defun GetOpenDocs (Docs / Item Names);;; list of open Word documents
  (repeat (setq Item (vla-get-count Docs))
   (setq Names (cons (strcase (findfile (vla-get-fullname (vla-item Docs Item)))) Names)) 
   (setq Item (1- Item))
  );end repeat
  Names
);end GetOpenDocs
  
(princ "\nSelect TEXT (MTEXT) to be exported to Word document: ")
(cond 
 ((setq TxtSet (ssget '((0 . "*TEXT"))))
  (cond
  ((setq File (getfiled "Select Word document" (strcat (vl-filename-base (getvar "dwgname")) ".doc") "doc" 1))
   (vl-load-com)
   (prompt "\nExporting text to Word. Please wait...")
   (princ)
   (if (not (setq Word (vlax-get-object "Word.Application")));;;is already open ?
    (setq Word (vlax-get-or-create-object "Word.Application"));;; no open
    (setq Flag#1 T)
   );end if
   (cond
    (Word
    (if (not Flag#1) (vla-put-visible Word :vlax-false));;; hide window application
    (if (findfile File)
     (setq Flag#2 (member (strcase (findfile File)) (GetOpenDocs (vlax-get-property Word 'Documents)))
	   NewDoc (vlax-invoke-method (vlax-get-property Word 'Documents) 'Open File))
     (setq  NewDoc (vlax-invoke-method (vla-get-documents Word) 'add))
    );end if
    (setq Paragraphs (vlax-get-property NewDoc 'Paragraphs) Count 0)  
    (repeat (sslength TxtSet)
     (setq String (vla-get-TextString (vlax-ename->vla-object (ssname TxtSet Count))))
     (setq Range (vlax-get-property (vlax-get-property Paragraphs 'last) 'Range))
     (if (not (setq FontName (cdar (cdadr (assoc -3 (entget (tblobjname "STYLE" (Dxf 7)) '("ACAD")))))))
      (setq FontName (vl-filename-base (cdr (assoc 3 (tblsearch "STYLE" (Dxf 7))))))
     );end if
     (vlax-put-property (vlax-get-property Range 'Font) 'Name FontName)
     (if (= (Dxf 0) "MTEXT") (setq String (ClearMTFormat String)))
     (vlax-invoke-method Range 'InsertAfter (strcat String "\n"))
     (setq Count (1+ Count))
    );end repeat
    (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-saveas (list NewDoc File)))
      (prompt "\nProbably selected file is read-only. Cannot export text to this file. ")
      (vla-saveas NewDoc File);;; save document
    );end if
    (cond
     ((not Flag#1)
      (vla-quit Word 0)
     )
     (T (if (not Flag#2) (vla-close NewDoc));;; close application
        (vla-put-visible Word :vlax-true);;; show application
     )
    );end cond
    (mapcar 'vlax-release-object (list Word NewDoc Paragraphs Range));;; objects release
    (mapcar '(lambda (x) (set x nil)) '(Word NewDoc Paragraphs Range));;; null all variables
    )
   (T (prompt "\nCan't create Word document. Microsoft Ofice must be installed. "))
  );end cond Word
  )
  (T (prompt "\nFile no selected. "))
 );end cond
 )
 (T (prompt "\nNothing selected. "));;; text no selected
);end cond
 (princ)
);end c:a2w

(princ)

 
