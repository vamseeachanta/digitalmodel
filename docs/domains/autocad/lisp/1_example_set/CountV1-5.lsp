;;--------------=={ Count.lsp - Advanced Block Counter }==--------------;;
;;                                                                      ;;
;;  This program enables the user to record the quantities of a         ;;
;;  selection or all standard or dynamic blocks in the working drawing. ;;
;;  The results of the block count may be displayed at the AutoCAD      ;;
;;  command-line, written to a Text or CSV file, or displayed in an     ;;
;;  AutoCAD Table, where available.                                     ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'count' at the AutoCAD              ;;
;;  command-line, the user is prompted to make a selection of standard  ;;
;;  or dynamic blocks to be counted by the program. At this prompt,     ;;
;;  the user may right-click or press 'Enter' to automatically count    ;;
;;  all blocks in the drawing.                                          ;;
;;                                                                      ;;
;;  Depending on the output setting, the results may then be printed    ;;
;;  to the AutoCAD command-line and displayed in the Text Window, or    ;;
;;  the user will be prompted to specify an insertion point for the     ;;
;;  table, or a filename & location for the Text or CSV output file.    ;;
;;                                                                      ;;
;;  The program settings may be configured using the 'countsettings'    ;;
;;  command; this command will present the user with a dialog interface ;;
;;  through which the data output, table & file headings, displayed     ;;
;;  columns, sorting field & sort order may each be altered.            ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2010-06-05                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2010-06-06                                      ;;
;;                                                                      ;;
;;  - Updated code to include Settings dialog.                          ;;
;;  - Added Undo Marks.                                                 ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2010-06-06                                      ;;
;;                                                                      ;;
;;  - Fixed bug with 64-bit systems.                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2011-03-02                                      ;;
;;                                                                      ;;
;;  - Program completely rewritten.                                     ;;
;;  - Updated code to work without error on 64-bit systems by fixing    ;;
;;    bug with ObjectID subfunction - my thanks go to member 'Jeff M'   ;;
;;    at theSwamp.org forums for helping me solve this problem.         ;;
;;  - Added ability to write block count to Text/CSV Files.             ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2014-06-15                                      ;;
;;                                                                      ;;
;;  - Program completely rewritten.                                     ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    2015-06-07                                      ;;
;;                                                                      ;;
;;  - Minor update to enable full compatibility with ZWCAD.             ;;
;;    (regeneratetablesuppressed property not available)                ;;
;;----------------------------------------------------------------------;;

(setq
    count:version "1-5"
    count:defaults
   '(
        (out "tab")
        (tg1 "1")
        (tg2 "1")
        (tg3 "1")
        (ed1 "Block Data")
        (ed2 "Preview")
        (ed3 "Block Name")
        (ed4 "Count")
        (srt "blk")
        (ord "asc")
    )
)

;;----------------------------------------------------------------------;;

(defun count:fixdir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)

;;----------------------------------------------------------------------;;

(defun count:getsavepath ( / tmp )
    (cond      
        (   (setq tmp (getvar 'roamablerootprefix))
            (strcat (count:fixdir tmp) "\\Support")
        )
        (   (setq tmp (findfile "acad.pat"))
            (count:fixdir (vl-filename-directory tmp))
        )
        (   (count:fixdir (vl-filename-directory (vl-filename-mktemp))))
    )
)

;;----------------------------------------------------------------------;;

(setq count:savepath (count:getsavepath) ;; Save path for DCL & Config files
      count:dclfname (strcat count:savepath "\\LMAC_count_V" count:version ".dcl")
      count:cfgfname (strcat count:savepath "\\LMAC_count_V" count:version ".cfg")
)

;;----------------------------------------------------------------------;;

(defun c:count

    (
        /
        *error*
        all
        col
        des dir
        ed1 ed2 ed3 ed4
        fil fnm fun
        hgt
        idx ins
        lst
        ord out
        row
        sel srt
        tab tg1 tg2 tg3 tmp
        xrf
    )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'vla-object (type tab))
                 (null (vlax-erased-p tab))
                 (= "AcDbTable" (vla-get-objectname tab))
                 (vlax-write-enabled-p tab)
                 (vlax-property-available-p tab 'regeneratetablesuppressed t)
            )
            (vla-put-regeneratetablesuppressed tab :vlax-false)
        )
        (if (and (= 'vla-object (type count:wshobject))
                 (not (vlax-object-released-p count:wshobject))
            )
            (progn
                (vlax-release-object count:wshobject)
                (setq count:wshobject nil)
            )
        )
        (count:endundo (count:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (not (findfile count:cfgfname))
        (count:writecfg count:cfgfname (mapcar 'cadr count:defaults))
    )
    (count:readcfg count:cfgfname (mapcar 'car count:defaults))
    (foreach sym count:defaults
        (if (not (boundp (car sym))) (apply 'set sym))
    )
    (if (and (= "tab" out) (not (vlax-method-applicable-p (vla-get-modelspace (count:acdoc)) 'addtable)))
        (setq out "txt")
    )
    
    (count:startundo (count:acdoc))

    (while (setq tmp (tblnext "block" (null tmp)))
        (if (= 4 (logand 4 (cdr (assoc 70 tmp))))
            (setq xrf (vl-list* "," (cdr (assoc 2 tmp)) xrf))
        )
    )
    (if xrf
        (setq fil  (list '(0 . "INSERT") '(-4 . "<NOT") (cons 2 (apply 'strcat (cdr xrf))) '(-4 . "NOT>")))
        (setq fil '((0 . "INSERT")))
    )
    
    (cond
        (   (null (setq all (ssget "_X" fil)))
            (count:popup
                "No Blocks Found" 64
                (princ "No blocks were found in the active drawing.")
            )
        )
        (   (and (= "tab" out) (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer)))))))
            (count:popup
                "Current Layer Locked" 64
                (princ "Please unlock the current layer before using this program.")
            )
        )
        (   (progn
                (setvar 'nomutt 1)
                (princ "\nSelect blocks to count <all>: ")
                (setq sel
                    (cond
                        (   (null (setq sel (vl-catch-all-apply 'ssget (list fil))))
                            all
                        )
                        (   (null (vl-catch-all-error-p sel))
                            sel
                        )
                    )
                )
                (setvar 'nomutt 0)
                (null sel)
            )
        )
        (   (or (= "com" out)
                (and (=  "tab" out) (setq ins (getpoint "\nSpecify point for table: ")))
                (and (/= "tab" out)
                    (setq fnm
                        (getfiled "Create Output File"
                            (cond
                                (   (and (setq dir (getenv "LMac\\countdir"))
                                         (vl-file-directory-p (setq dir (count:fixdir dir)))
                                    )
                                    (strcat dir "\\")
                                )
                                (   (getvar 'dwgprefix))
                            )
                            out 1
                        )
                    )
                )
            )
            (repeat (setq idx (sslength sel))
                (setq lst (count:assoc++ (count:effectivename (ssname sel (setq idx (1- idx)))) lst))
            )
            (if (= "blk" srt)
                (setq fun (eval (list 'lambda '( a b ) (list (if (= "asc" ord) '< '>) '(strcase (car a)) '(strcase (car b))))))
                (setq fun (eval (list 'lambda '( a b ) (list (if (= "asc" ord) '< '>) '(cdr a) '(cdr b)))))
            )
            (setq lst (vl-sort lst 'fun))
            (cond
                (   (= "com" out)
                    (defun prinn ( x ) (princ "\n") (princ x))
                    (prinn (count:padbetween "" "" "=" 60))
                    (if (= "1" tg1)
                        (progn
                            (prinn ed1)
                            (prinn (count:padbetween "" "" "-" 60))
                        )
                    )
                    (prinn (count:padbetween ed3 ed4 " " 55))
                    (prinn (count:padbetween "" "" "-"   60))
                    (if (= "1" tg3)
                        (foreach itm lst
                            (prinn (count:padbetween (car itm) (itoa (cdr itm)) "." 55))
                        )
                        (foreach itm lst (prinn (car itm)))
                    )
                    (prinn (count:padbetween "" "" "=" 60))
                    (textpage)
                )
                (   (= "tab" out)
                    (if (= "1" tg3)
                        (setq lst (mapcar '(lambda ( x ) (list (car x) (itoa (cdr x)))) lst))
                        (setq lst (mapcar '(lambda ( x ) (list (car x))) lst))
                    )
                    (setq hgt
                        (vla-gettextheight
                            (vla-item
                                (vla-item (vla-get-dictionaries (count:acdoc)) "acad_tablestyle")
                                (getvar 'ctablestyle)
                            )
                            acdatarow
                        )
                    )
                    (setq tab
                        (vla-addtable
                            (vlax-get-property (count:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                            (vlax-3D-point (trans ins 1 0))
                            (+ (length lst) 2)
                            (+ 1 (atoi tg2) (atoi tg3))
                            (* 2.5 hgt)
                            (* hgt
                                (max
                                    (apply 'max
                                        (mapcar 'strlen
                                            (append
                                                (if (= "1" tg2) (list ed2))
                                                (if (= "1" tg3) (list ed4))
                                                (cons ed3 (apply 'append lst))
                                            )
                                        )
                                    )
                                    (if (= "1" tg1) (/ (strlen ed1) (+ 1 (atoi tg2) (atoi tg3))) 0)
                                )
                            )
                        )
                    )
                    (if (vlax-property-available-p tab 'regeneratetablesuppressed t)
                        (vla-put-regeneratetablesuppressed tab :vlax-true)
                    )
                    (vla-put-stylename tab (getvar 'ctablestyle))
                    (setq col 0)
                    (mapcar
                       '(lambda ( a b ) (if (= "1" a) (progn (vla-settext tab 1 col b) (setq col (1+ col)))))
                        (list tg2 "1" tg3)
                        (list ed2 ed3 ed4)
                    )
                    (setq row 2)
                    (foreach itm lst
                        (if (= "1" tg2)
                            (count:setblocktablerecord tab row (setq col 0) (car itm))
                            (setq col -1)
                        )
                        (foreach txt itm
                            (vla-settext tab row (setq col (1+ col)) txt)
                        )
                        (setq row (1+ row))
                    )
                    (if (= "1" tg1)
                        (vla-settext tab 0 0 ed1)
                        (vla-deleterows tab 0 1)
                    )
                )
                (   (setenv "LMac\\countdir" (count:fixdir (vl-filename-directory fnm)))
                    (if
                        (
                            (if (= "txt" out)
                                count:writetxt
                                count:writecsv
                            )
                            (append
                                (if (= "1" tg1)
                                    (list (list ed1))
                                )
                                (if (= "1" tg3)
                                    (cons (list ed3 ed4) (mapcar '(lambda ( x ) (list (car x) (itoa (cdr x)))) lst))
                                    (cons (list ed3)     (mapcar '(lambda ( x ) (list (car x))) lst))
                                )
                            )
                            fnm
                        )
                        (princ (strcat "\nBlock data written to " fnm))
                        (count:popup "Unable to Create Output File" 48
                            (princ
                                (strcat
                                    "The program was unable to create the following file:\n\n"
                                    fnm
                                    "\n\nPlease ensure that you have write-permissions for the above directory."
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (*error* nil)
    (princ)
)

;;----------------------------------------------------------------------;;

(defun c:countsettings

    (
        /
        *error*
        dch des
        ord out out-fun
        srt
        tg1 tg1-fun tg2 tg2-fun tg3 tg3-fun
    )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'int (type dch))
                 (< 0 dch)
            )
            (unload_dialog dch)
        )
        (if (and (= 'vla-object (type count:wshobject))
                 (not (vlax-object-released-p count:wshobject))
            )
            (progn
                (vlax-release-object count:wshobject)
                (setq count:wshobject nil)
            )
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (not (findfile count:cfgfname))
        (count:writecfg count:cfgfname (mapcar 'cadr count:defaults))
    )
    (count:readcfg count:cfgfname (mapcar 'car count:defaults))
    (foreach sym count:defaults
        (if (not (boundp (car sym))) (apply 'set sym))
    )
    (cond
        (   (not (count:writedcl count:dclfname))
            (count:popup "DCL file could not be written" 48
                (princ
                    (strcat
                        "The DCL file required by this program could not be written to the following location:\n\n"
                        count:dclfname
                        "\n\nPlease ensure that you have write-permissions for the above directory."
                    )
                )
            )
        )
        (   (<= (setq dch (load_dialog count:dclfname)) 0)
            (count:popup "DCL file could not be loaded" 48
                (princ
                    (strcat
                        "The following DCL file required by this program could not be loaded:\n\n"
                        count:dclfname
                        "\n\nPlease verify the integrity of this file."
                    )
                )
            )
        )
        (   (not (new_dialog "dia" dch))
            (count:popup "DCL file contains an error" 48
                (princ
                    (strcat
                        "The program dialog could not be displayed as the following DCL file file contains an error:\n\n"
                        count:dclfname
                        "\n\nPlease verify the integrity of this file."
                    )
                )
            )
        )
        (   t
            (set_tile "dcl"
                (strcat
                    "Count.lsp Version "
                    (vl-string-translate "-" "." count:version)
                    " \\U+00A9 Lee Mac "
                    (menucmd "m=$(edtime,0,yyyy)")
                )
            )
            (if (and (= "tab" out) (not (vlax-method-applicable-p (vla-get-modelspace (count:acdoc)) 'addtable)))
                (progn
                    (mode_tile "tab" 1)
                    (setq out "txt")
                )
            )
            (   (setq tg1-fun (lambda ( val ) (mode_tile "ed1" (- 1 (atoi (setq tg1 val)))))) (set_tile "tg1" tg1))
            (action_tile "tg1" "(tg1-fun $value)")

            (   (setq tg2-fun (lambda ( val ) (mode_tile "ed2" (- 1 (atoi (setq tg2 val)))))) (set_tile "tg2" tg2))
            (action_tile "tg2" "(tg2-fun $value)")

            (   (setq tg3-fun (lambda ( val ) (mode_tile "ed4" (- 1 (atoi (setq tg3 val)))))) (set_tile "tg3" tg3))
            (action_tile "tg3" "(tg3-fun $value)")

            (foreach key '("ed1" "ed2" "ed3" "ed4")
                (set_tile key (eval (read key)))
                (action_tile key (strcat "(setq " key " $value)"))
            )
            (set_tile out "1")
            (   (setq out-fun
                    (lambda ( val )
                        (if (= "tab" (setq out val))
                            (progn
                                (mode_tile "tg2" 0)
                                (mode_tile "ed2" (- 1 (atoi tg2)))
                            )
                            (progn
                                (mode_tile "tg2" 1)
                                (mode_tile "ed2" 1)
                            )
                        )
                    )
                )
                out
            )
            (foreach key '("tab" "txt" "csv" "com")
                (action_tile key "(out-fun $key)")
            )
            (set_tile srt "1")
            (foreach key '("blk" "qty")
                (action_tile key "(setq srt $key)")
            )
            (set_tile ord "1")
            (foreach key '("asc" "des")
                (action_tile key "(setq ord $key)")
            )
            (if (= 1 (start_dialog))
                (count:writecfg count:cfgfname (mapcar 'eval (mapcar 'car count:defaults)))
            )
        )
    )
    (*error* nil)
    (princ)
)

;;----------------------------------------------------------------------;;
                
(defun count:popup ( ttl flg msg / err )
    (setq err (vl-catch-all-apply 'vlax-invoke-method (list (count:wsh) 'popup msg 0 ttl flg)))
    (if (null (vl-catch-all-error-p err))
        err
    )
)
 
;;----------------------------------------------------------------------;;
 
(defun count:wsh nil
    (cond (count:wshobject) ((setq count:wshobject (vlax-create-object "wscript.shell"))))
)

;;----------------------------------------------------------------------;;

(defun count:tostring ( arg / dim )
    (cond
        (   (= 'int (type arg))
            (itoa arg)
        )
        (   (= 'real (type arg))
            (setq dim (getvar 'dimzin))
            (setvar 'dimzin 8)
            (setq arg (rtos arg 2 15))
            (setvar 'dimzin dim)
            arg
        )
        (   (vl-prin1-to-string arg))
    )
)
 
;;----------------------------------------------------------------------;;
 
(defun count:writecfg ( cfg lst / des )
    (if (setq des (open cfg "w"))
        (progn
            (foreach itm lst (write-line (count:tostring itm) des))
            (setq des (close des))
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun count:readcfg ( cfg lst / des itm )
    (if
        (and
            (setq cfg (findfile cfg))
            (setq des (open cfg "r"))
        )
        (progn
            (foreach sym lst
                (if (setq itm (read-line des))
                    (set  sym (read itm))
                )
            )
            (setq des (close des))
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun count:writedcl ( dcl / des )
    (cond
        (   (findfile dcl))
        (   (setq des (open dcl "w"))
            (foreach itm
               '(
                    "//--------------------=={ Count Dialog Definition }==-------------------//"
                    "//                                                                      //"
                    "//  Dialog definition file for use in conjunction with Count.lsp        //"
                    "//----------------------------------------------------------------------//"
                    "//  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              //"
                    "//----------------------------------------------------------------------//"
                    ""
                    "b15 : edit_box"
                    "{"
                    "    edit_width = 16;"
                    "    edit_limit = 1024;"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "    horizontal_margin = none;"
                    "    vertical_margin = none;"
                    "}"
                    "b30 : edit_box"
                    "{"
                    "    edit_width = 52;"
                    "    edit_limit = 1024;"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "    horizontal_margin = none;"
                    "    vertical_margin = none;"
                    "}"
                    "tog : toggle"
                    "{"
                    "    vertical_margin = none;"
                    "    horizontal_margin = 0.2;"
                    "}"
                    "rwo : row"
                    "{"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "}"
                    "rrw : radio_row"
                    "{"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "}"
                    "dia : dialog"
                    "{"
                    "    key = \"dcl\";"
                    "    spacer_1;"
                    "    : boxed_column"
                    "    {"
                    "        label = \"Output\";"
                    "        : rrw"
                    "        {"
                    "            : radio_button { key = \"tab\"; label = \"Table\"; }"
                    "            : radio_button { key = \"txt\"; label = \"Text File\"; }"
                    "            : radio_button { key = \"csv\"; label = \"CSV File\"; }"
                    "            : radio_button { key = \"com\"; label = \"Command line\"; }"
                    "        }"
                    "        spacer;"
                    "    }"
                    "    : boxed_column"
                    "    {"
                    "        label = \"Headings\";"
                    "        spacer_1;"
                    "        : rwo"
                    "        {"
                    "            : tog { key = \"tg1\"; }"
                    "            : b30 { key = \"ed1\"; }"
                    "            : spacer"
                    "            {"
                    "                fixed_width = true;"
                    "                vertical_margin = none;"
                    "                width = 2.5;"
                    "            }"
                    "        }"
                    "        : rwo"
                    "        {"
                    "            spacer;"
                    "            : tog { key = \"tg2\"; }"
                    "            : b15 { key = \"ed2\"; }"
                    "            : b15 { key = \"ed3\"; }"
                    "            : b15 { key = \"ed4\"; }"
                    "            : tog { key = \"tg3\"; }"
                    "            spacer;"
                    "        }"
                    "        spacer_1;"
                    "    }"
                    "    : row"
                    "    {"
                    "        : boxed_column"
                    "        {"
                    "            label = \"Sort By\";"
                    "            : rrw"
                    "            {"
                    "                : radio_button { key = \"blk\"; label = \"Block Name\"; }"
                    "                : radio_button { key = \"qty\"; label = \"Quantity\"; }"
                    "            }"
                    "            spacer;"
                    "        }"
                    "        : boxed_column"
                    "        {"
                    "            label = \"Sort Order\";"
                    "            : rrw"
                    "            {"
                    "                : radio_button { key = \"asc\"; label = \"Ascending\"; }"
                    "                : radio_button { key = \"des\"; label = \"Descending\"; }"
                    "            }"
                    "            spacer;"
                    "        }"
                    "    }"
                    "    spacer_1; ok_cancel;"
                    "}"
                    ""
                    "//----------------------------------------------------------------------//"
                    "//                             End of File                              //"
                    "//----------------------------------------------------------------------//"
                )
                (write-line itm des)
            )
            (setq des (close des))
            (while (not (findfile dcl))) ;; for slow HDDs
            dcl
        )
    )
)

;;----------------------------------------------------------------------;;
 
(defun count:writecsv ( lst csv / des sep )
    (if (setq des (open csv "w"))
        (progn
            (setq sep (cond ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) (",")))
            (foreach row lst (write-line (count:lst->csv row sep) des))
            (close des)
            t
        )
    )
)
 
;;----------------------------------------------------------------------;;
 
(defun count:lst->csv ( lst sep )
    (if (cdr lst)
        (strcat (count:csv-addquotes (car lst) sep) sep (count:lst->csv (cdr lst) sep))
        (count:csv-addquotes (car lst) sep)
    )
)

;;----------------------------------------------------------------------;;
 
(defun count:csv-addquotes ( str sep / pos )
    (cond
        (   (wcmatch str (strcat "*[`" sep "\"]*"))
            (setq pos 0)    
            (while (setq pos (vl-string-position 34 str pos))
                (setq str (vl-string-subst "\"\"" "\"" str pos)
                      pos (+ pos 2)
                )
            )
            (strcat "\"" str "\"")
        )
        (   str   )
    )
)
 
;;----------------------------------------------------------------------;;
 
(defun count:writetxt ( lst txt / des )
    (if (setq des (open txt "w"))
        (progn
            (foreach itm lst (write-line (count:lst->str itm "\t") des))
            (close des)
            t
        )
    )
)
 
;;----------------------------------------------------------------------;;
 
(defun count:lst->str ( lst del )
    (if (cdr lst)
        (strcat (car lst) del (count:lst->str (cdr lst) del))
        (car lst)
    )
)

;;----------------------------------------------------------------------;;

(defun count:padbetween ( s1 s2 ch ln )
    (
        (lambda ( a b c )
            (repeat (- ln (length b) (length c)) (setq c (cons a c)))
            (vl-list->string (append b c))
        )
        (ascii ch)
        (vl-string->list s1)
        (vl-string->list s2)
    )
)

;;----------------------------------------------------------------------;;

(defun count:setblocktablerecord ( obj row col blk )
    (eval
        (list 'defun 'count:setblocktablerecord '( obj row col blk )
            (cons
                (if (vlax-method-applicable-p obj 'setblocktablerecordid32)
                    'vla-setblocktablerecordid32
                    'vla-setblocktablerecordid
                )
                (list
                    'obj 'row 'col
                    (list 'count:objectid (list 'vla-item (vla-get-blocks (count:acdoc)) 'blk))
                    ':vlax-true
                )
            )
        )
    )
    (count:setblocktablerecord obj row col blk)
)

;;----------------------------------------------------------------------;;

(defun count:objectid ( obj )
    (eval
        (list 'defun 'count:objectid '( obj )
            (cond
                (   (not (wcmatch (getenv "PROCESSOR_ARCHITECTURE") "*64*"))
                   '(vla-get-objectid obj)
                )
                (   (= 'subr (type vla-get-objectid32))
                   '(vla-get-objectid32 obj)
                )
                (   (list 'vla-getobjectidstring (vla-get-utility (count:acdoc)) 'obj ':vlax-false))
            )
        )
    )
    (count:objectid obj)
)

;;----------------------------------------------------------------------;;

(defun count:assoc++ ( key lst / itm )
    (if (setq itm (assoc key lst))
        (subst (cons key (1+ (cdr itm))) itm lst)
        (cons  (cons key 1) lst)
    )
)

;;----------------------------------------------------------------------;;

(defun count:effectivename ( ent / blk rep )
    (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**")
        (if
            (and
                (setq rep
                    (cdadr
                        (assoc -3
                            (entget
                                (cdr
                                    (assoc 330
                                        (entget
                                            (tblobjname "block" blk)
                                        )
                                    )
                                )
                               '("AcDbBlockRepBTag")
                            )
                        )
                    )
                )
                (setq rep (handent (cdr (assoc 1005 rep))))
            )
            (setq blk (cdr (assoc 2 (entget rep))))
        )
    )
    blk
)

;;----------------------------------------------------------------------;;

(defun count:startundo ( doc )
    (count:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun count:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun count:acdoc nil
    (eval (list 'defun 'count:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (count:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: Count.lsp | Version "
        (vl-string-translate "-" "." count:version)
        " | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: \"count\" - Main Program | \"countsettings\" - Settings ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;