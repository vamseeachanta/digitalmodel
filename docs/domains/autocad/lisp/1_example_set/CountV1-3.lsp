;;---------=={ Count.lsp - Advanced Block Counter }==---------;;
;;                                                            ;;
;;  Program will count the number of occurrences of all or    ;;
;;  selected standard and dynamic blocks in a drawing.        ;;
;;                                                            ;;
;;  The resultant count data is printed to the command line   ;;
;;  and may be optionally written to either a Text or CSV     ;;
;;  file, or, should the program is run in an AutoCAD Version ;;
;;  which supports a Table Object, the data may also be       ;;
;;  displayed in an AutoCAD Table.                            ;;
;;                                                            ;;
;;  All Table & File Headings and Block Preview may be        ;;
;;  altered using the Settings dialog.                        ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    05-06-2010                            ;;
;;                                                            ;;
;;  First Release.                                            ;;
;;------------------------------------------------------------;;
;;  Version 1.1    -    06-06-2010                            ;;
;;                                                            ;;
;;  Updated code to include settings dialog.                  ;;
;;  Added Undo Marks.                                         ;;
;;------------------------------------------------------------;;
;;  Version 1.2    -    06-06-2010                            ;;
;;                                                            ;;
;;  Fixed bug with 64-bit systems.                            ;;
;;------------------------------------------------------------;;
;;  Version 1.3    -    02-03-2011                            ;;
;;                                                            ;;
;;  Program completely rewritten.                             ;;
;;  Updated code to work without error on 64-bit systems by   ;;
;;  fixing bug with ObjectID subfunction - my thanks go to    ;;
;;  Jeff M for helping me solve this problem.                 ;;
;;  Added ability to write block count to Text/CSV Files.     ;;
;;------------------------------------------------------------;;

(defun c:Count

  ( /

   ;;  --=={ Local Functions }==--

   *error*
   _addtable
   _assoc++
   _countsettings
   _endundo
   _getblockname
   _getobjectid
   _getsavepath
   _is64bit
   _msgbox
   _open
   _padbetween
   _readconfig
   _startundo
   _writeconfig
   _writedcl

   ;;  --=={ Local Variables }==--

   acdoc
   acspc
   args
   blocks
   bt
   btitle
   cfgfname
   column
   ct
   ctitle
   data
   dc
   dcfname
   dctitle
   del
   doc
   file
   hasprev
   hastitle
   hp
   ht
   i
   key
   l
   ln
   lst
   maxl
   mt
   mtitle
   mutter
   n
   opt
   pt
   ptitle
   row
   rowitem
   savepath
   space
   ss
   sym
   symlist
   table
   tile
   title
   vallist
   value
   versionnumber

   ;; --=={ Global Variables }==--

   ; -None-

  )

  (vl-load-com)
  
  (setq VersionNumber "1-3")

  ;;----------------------------------------------------------;;
  ;;                     Local Functions                      ;;
  ;;----------------------------------------------------------;;

  (defun *error* ( msg )
    
    (if dc     (unload_dialog dc))
    (if acdoc  (_EndUndo acdoc))
    (if mutter (setvar 'NOMUTT mutter))
    (if file   (setq file (close file)))
    
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  ;............................................................;

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  ;............................................................;

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  ;............................................................;

  (defun _GetSavePath ( / tmp )
    (cond      
      ( (setq tmp (getvar 'ROAMABLEROOTPREFIX))

        (or (eq "\\"  (substr tmp (strlen tmp)))
            (setq tmp (strcat tmp "\\"))
        )
        (strcat tmp "Support")
      )
      ( (setq tmp (findfile "ACAD.pat"))

        (setq tmp (vl-filename-directory tmp))

        (and (eq "\\"  (substr tmp (strlen tmp)))
             (setq tmp (substr tmp (1- (strlen tmp))))
        )
        tmp
      )
    )
  )

  ;............................................................;

  (defun _WriteConfig ( fname lst / ofile )

    (if (setq ofile (open fname "w"))
      (progn        
        (foreach x lst (write-line (vl-prin1-to-string x) ofile))
        
        (setq ofile (close ofile))
        t
      )
    )
  )

  ;............................................................;
  
  (defun _ReadConfig  ( fname lst / ofile )

    (if (and (setq fname (findfile fname))
             (setq ofile (open fname "r")))
      (progn          
        (foreach x lst (set x (read (read-line ofile))))
        
        (setq ofile (close ofile))
        lst
      )
    )
  )

  ;............................................................;

  (defun _Assoc++ ( key lst )
    (
      (lambda ( pair )
        (if pair
          (subst (list key (1+ (cadr pair))) pair lst)
          (cons  (list key 1) lst)
        )
      )
      (assoc key lst)
    )
  )

  ;............................................................;

  (defun _PadBetween ( s1 s2 ch ln )
    (
      (lambda ( l1 l2 ch )
        (while (< (+ (length l1) (length l2)) ln) (setq l2 (cons ch l2)))
        (vl-list->string (append l1 l2))
      )
      (vl-string->list s1)
      (vl-string->list s2) (ascii ch)
    )
  )

  ;............................................................;
  
  (defun _GetBlockName ( obj )
    (vlax-get-property obj
      (if (vlax-property-available-p obj 'EffectiveName) 'EffectiveName 'Name)
    )
  )

  ;............................................................;

  (defun _Is64Bit nil (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE")))

  ;............................................................;

  (defun _GetObjectID ( doc obj )
    (if (_Is64Bit)
      (vla-get-Objectid32 obj)
      (vla-get-Objectid   obj)
    )
  )

  ;............................................................;

  (defun _MsgBox ( title flags msg / WSHShell result )
      
    (setq WSHShell (vlax-create-object "WScript.Shell"))
    (setq result   (vlax-invoke WSHShell 'Popup msg 0 title flags))
    (vlax-release-object WSHShell)
    result
  )

  ;............................................................;

  (defun _WriteDCL ( fname / ofile )

    (if (not (findfile fname))
      
      (if (setq ofile (open fname "w"))
        (progn
          (foreach str

            '("//------------=={ Count.dcl Dialog Definition }==-----------//"
              "//                                                          //"
              "//  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com     //"
              "//----------------------------------------------------------//"
              ""
              "ed12 : edit_box { edit_width = 12; fixed_width = true; alignment = centered; }"
              "tog  : toggle   { alignment = centered; }"
              ""
              "//----------------------------------------------------------//"
              "//                  Main Dialog Definition                  //"
              "//----------------------------------------------------------//"
              ""
              "count : dialog { key = \"dctitle\"; spacer;"
              ""
              "  : image { key = \"sep1\"; width = 33.26; height = 0.74; color = -15; }"
              ""
              "  : row {"
              "    : column { spacer; : ed12 { key = \"ptitle\"; } }"
              "    spacer;"
              "    : column {"
              "      : ed12 { key = \"mtitle\"; }"
              "      : ed12 { key = \"btitle\"; }"
              "    }"
              "    spacer;"
              "    : column { spacer; : ed12 { key = \"ctitle\"; } }"
              "  }"
              "  spacer;"
              "  : image { key = \"sep2\"; width = 33.26; height = 0.74; color = -15; }"
              ""
              "  : row {"
              "    spacer;"
              "    : tog { label = \"Block Preview\"; key = \"hasprev\" ; }"
              "    : tog { label = \"Table Title\";   key = \"hastitle\"; }"
              "    spacer;"
              "  }"
              "  : image { key = \"sep3\"; width = 33.26; height = 0.74; color = -15; }"
              "  spacer;"
              ""
              "  ok_cancel;"
              "}"
              "//----------------------------------------------------------//"
              "//                       End of File                        //"
              "//----------------------------------------------------------//"
             )
             (write-line str ofile)
          )
          (setq ofile (close ofile))
          t
        )
      )
      t
    )
  )

  ;............................................................;

  (defun _CountSettings ( fname dctitle args / dc mt pt bt ct hp ht )
   
    (cond
      ( (not (_WriteDCL fname))

        (_MsgBox "Warning" 16 "DCL File Could not be Written")
        (princ "\n** Dialog File Could not be Written")
      )
      ( (<= (setq dc (load_dialog fname)) 0)

        (_MsgBox "Warning" 16 "Dialog File not Found")
        (princ "\n** Dialog File not Found **")
      )
      ( (not (new_dialog "count" dc))

        (_MsgBox "Warning" 16 "Dialog Could not be Loaded")
        (princ "\n** Dialog Could not be Loaded **")
        (setq dc (unload_dialog dc))
      )
      (t
        (set_tile "dctitle" dctitle)

        (foreach x '("sep1" "sep2" "sep3")
          (start_image x) (mapcar 'vector_image '(0 0) '(6 5) '(300 300) '(6 5) '(8 7)) (end_image)
        )
        (mapcar 'set '(mt pt bt ct hp ht) (mapcar 'eval args))

        (foreach x args
          (set_tile (strcase (vl-princ-to-string x) t) (eval x))
        )
        (mode_tile "mtitle" (- 1 (atoi ht)))
        (mode_tile "ptitle" (- 1 (atoi hp)))
       
        (mapcar
         '(lambda ( tile sym )
            (action_tile tile (strcat "(setq " sym " $value)"))
          )
         '("mtitle" "ptitle" "btitle" "ctitle") '("mt" "pt" "bt" "ct")
        )
        (action_tile "hasprev"  "(mode_tile \"ptitle\" (- 1 (atoi (setq hp $value))))")
        (action_tile "hastitle" "(mode_tile \"mtitle\" (- 1 (atoi (setq ht $value))))")
       
        (action_tile "accept"   "(mapcar 'set args (list mt pt bt ct hp ht)) (done_dialog)")
        (start_dialog)
        (setq dc (unload_dialog dc))
      )
    )
    (mapcar 'eval args)
  )

  ;............................................................;
     
  (defun _AddTable ( doc space pt data hastitle title hasprev ptitle / _itemp )

    (defun _itemp ( collection item )
      (if
        (not
          (vl-catch-all-error-p
            (setq item
              (vl-catch-all-apply 'vla-item (list collection item))
            )
          )
        )
        item
      )
    )

    (
      (lambda ( table blocks ) (vla-put-RegenerateTableSuppressed table :vlax-true) (vla-put-StyleName table (getvar 'CTABLESTYLE))
        (if hasprev
          (progn (vla-SetText table 1 0 ptitle)
            (
              (lambda ( row )
                (mapcar
                  (function
                    (lambda ( block ) (setq row (1+ row)) (vla-SetCellType table row 0 acBlockCell)
                      (vlax-invoke table
                        (if (_Is64Bit)
                          'SetBlockTableRecordId32 
                          'SetBlockTableRecordID
                        )
                        row 0 (_GetObjectID doc (_itemp blocks block)) :vlax-true
                      )
                    )
                  )
                  (mapcar 'car (cdr data))
                )
              )
              1
            )
          )
        )
        (
          (lambda ( row )
            (mapcar
              (function
                (lambda ( rowitem ) (setq row (1+ row))
                  (
                    (lambda ( column )
                      (mapcar
                        (function
                          (lambda ( item )
                            (vla-SetText table row
                              (setq column (1+ column)) item
                            )
                          )
                        )
                        rowitem
                      )
                    )
                    (if hasprev 0 -1)
                  )
                )
              )
              data
            )
          )
          0
        )
        (if hastitle
          (vla-SetText table 0 0 title)
          (vla-deleterows table 0 1)    
        )
        (vla-put-RegenerateTableSuppressed table :vlax-false)
        table
      )
      (
        (lambda ( textheight )
          (vla-AddTable space (vlax-3D-point pt) (1+ (length data)) (+ (if hasprev 1 0) (length (car data))) (* 1.8 textheight)
            (* textheight
              (apply 'max
                (cons (/ (strlen title) (length (car data)))
                  (mapcar 'strlen (apply 'append data))
                )
              )
            )
          )
        )
        (vla-getTextHeight
          (_itemp
            (_itemp
              (vla-get-Dictionaries doc) "ACAD_TABLESTYLE"
            )
            (getvar 'CTABLESTYLE)
          )
          acDataRow
        )
      )
      (vla-get-blocks doc)
    )
  )

  ;............................................................;

  (defun _Open ( target / Shell result )
    
    (setq Shell (vla-getInterfaceObject (vlax-get-acad-object) "Shell.Application"))

    (setq result
      (and (or (eq 'INT (type target)) (setq target (findfile target)))
        (not
          (vl-catch-all-error-p
            (vl-catch-all-apply 'vlax-invoke (list Shell 'Open target))
          )
        )
      )
    )
    
    (vlax-release-object Shell)
    result
  )

  ;;----------------------------------------------------------;;
  ;;                       Main Routine                       ;;
  ;;----------------------------------------------------------;;

  (setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object))
        acspc (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'PaperSpace 'ModelSpace))
  )

  (if (not (vl-file-directory-p (setq SavePath (_GetSavePath))))
    (progn
      (princ "\n** Save Path not Valid **") (exit)
    )
  )

  (setq dcfname    (strcat SavePath "\\LMAC_Count_V" VersionNumber ".dcl")
        cfgfname   (strcat SavePath "\\LMAC_Count_V" VersionNumber ".cfg")
        dctitle    (strcat "Count V" (vl-string-translate "-" "." VersionNumber) " - Settings")
  )

  (setq SymList '(mtitle ptitle btitle ctitle hasprev hastitle)
        ValList  (list "Block Data" "Preview" "Block Name" "Count" "1" "1")
  )
  
  (or (findfile cfgfname)
      (_WriteConfig cfgfname ValList)
  )
  (_ReadConfig cfgfname SymList)

  (mapcar '(lambda ( sym value ) (or (boundp sym) (set sym value))) SymList ValList)

  ;............................................................;

  (setq mutter (getvar 'NOMUTT))
  (setvar 'NOMUTT 1)
  (princ "\nSelect Blocks to Count <All> : ")

  (cond
    (
      (not
        (progn
          (setq ss
            (cond
              ( (ssget      '((0 . "INSERT"))) )
              ( (ssget "_X" '((0 . "INSERT"))) )
            )
          )
          (setq mutter (not (setvar 'NOMUTT mutter)))
          ss
        )
      )

      (princ "\n--> No Blocks Found.")
    )
    (
      (progn
        (vlax-for obj (setq ss (vla-get-ActiveSelectionSet acdoc))
          (if
            (zerop
              (logand 45
                (cdr
                  (assoc 70
                    (tblsearch "BLOCK"
                      (setq n (_GetBlockName obj))
                    )
                  )
                )
              )
            )
            (setq l (_Assoc++ n l))
          )
        )
        (vla-delete ss)
        (setq i 0 l
          (vl-sort
            (mapcar
              (function
                (lambda ( x )
                  (if (< i (cadr x)) (setq i (cadr x))) (list (car x) (itoa (cadr x)))
                )
              )
              l
            )
            (function (lambda ( a b ) (< (car a) (car b))))
          )
        )
      )
      (setq maxL (- 57 (strlen (itoa i))))

      (princ (strcat "\n" (_PadBetween "Block Name" "Count" "." 60)))
      (princ (strcat "\n" (_PadBetween "" "" "-" 60)))

      (foreach x l
        (princ (strcat "\n" (_PadBetween (substr (car x) 1 maxL) (cadr x) "." 60)))
      )     
      (princ (strcat "\n" (_PadBetween "" "" "-" 60)))
      (terpri)
     
      (if (vlax-method-applicable-p acspc 'AddTable)
        (progn
          (while
            (progn (initget "Table File Settings Exit")
              (setq opt (getkword "\nOutput [Table/File/Settings] <Exit>: "))

              (cond
                (
                  (or (null opt) (eq "Exit" opt)) nil
                )
                (
                  (and (eq "Table" opt) (setq pt (getpoint "\nSpecify Point for Table: ")))

                  (_StartUndo acdoc)                 
                  (_AddTable acdoc acspc (trans pt 1 0) (cons (list btitle ctitle) l) (eq "1" hastitle) mtitle (eq "1" hasprev) ptitle)
                  (_EndUndo   acdoc)                 
                  nil
                )
                (
                  (eq "Settings" opt)

                  (mapcar 'set SymList (_CountSettings dcfname dctitle SymList))
                )
                (
                  (and (eq "File" opt)
                    (setq *file*
                      (getfiled "Create Output File"
                        (vl-filename-directory (cond ( *file* ) ( (getvar 'DWGPREFIX) ))) "csv;txt" 1
                      )
                    )
                  )

                  (if (setq file (open *file* "w"))
                    (cond
                      (
                        (eq ".CSV" (strcase (vl-filename-extension *file*)))

                        (if (eq "1" hastitle) (write-line mtitle file))

                        (foreach line (cons (list btitle ctitle) l)
                          (write-line (strcat (car line) "," (cadr line)) file)
                        )
                        (setq file (close file)) (_Open *file*)
                      )
                      (t
                        (if (eq "1" hastitle) (write-line mtitle file))

                        (setq maxL
                          (+ 7
                            (apply 'max
                              (mapcar
                                (function
                                  (lambda ( item ) (strlen (apply 'strcat item)))
                                )
                                (cons (list btitle ctitle) l)
                              )
                            )
                          )
                        )

                        (foreach line (cons (list btitle ctitle) l)
                          (write-line (_PadBetween (car line) (cadr line) " " maxL) file)
                        )
                        (setq file (close file)) (_Open *file*)
                      )
                    )
                    (princ "\n** Error Creating Output File **")
                  )
                  nil
                )
              )
            )
          )
        )
        (textscr)
      )
    )
    ( (princ "\n--> No Blocks Found.") )
  )
  
  (_WriteConfig cfgfname (mapcar 'eval SymList))
  (princ)
)

;............................................................;

(princ)
(princ "\n:: Count.lsp | Version 1.3 | © Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Type \"Count\" to Invoke ::")
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;