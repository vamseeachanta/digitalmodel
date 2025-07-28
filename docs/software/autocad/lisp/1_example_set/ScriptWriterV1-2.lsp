;;---------------------------=={  Script Writer  }==-----------------------------;;
;;                                                                               ;;
;;  The program will allow the user to enter a line of script operations to be   ;;
;;  performed on a directory (and subdirectories) of drawings.                   ;;
;;                                                                               ;;
;;  When entering the script operations, the filename of the drawing is          ;;
;;  represented by the *file* token.                                             ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  WScript                                                    ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author: Lee Mac, Copyright © May 2010 - www.lee-mac.com                      ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version:                                                                     ;;
;;                                                                               ;;
;;  1.0:  31/05/2010  -  First Release                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.1:  01/06/2010  -  Added Filename / Load / Save / Clear buttons.           ;;
;;-------------------------------------------------------------------------------;;
;;  1.2:  02/06/2010  -  Added ability to import/export .scr files.              ;;
;;-------------------------------------------------------------------------------;;

(defun c:wScript

  ( /

      ;;         --=={ Local Functions }==--          ;

      *error*
      DIR_TEXT DIRDIALOG
      LOGO
      POPUP
      READ_CONFIG REMOVENTH
      WRITE_CONFIG

      ;;         --=={ Local Variables }==--          ;

      AC
      CFGFNAME
      DC DCFLAG DCFNAME DCTITLE DIR DOC
      LST
      OFILE
      PTR
      SAVEPATH SCRFNAME SCRLINE SCRLST SUB
      TMP
      UNDO
      VERSIONNUMBER

      ;;         --=={ Global Variables }==--          ;

      ; *SaveLst

  )

  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Preliminaries  }==--                         ;;
  ;;-------------------------------------------------------------------------------;;

  (setq VersionNumber "1.2")

  ;;-------------------------------------------------------------------------------;;

  (setq SavePath

    (cond
      
      ( (setq tmp (getvar 'ROAMABLEROOTPREFIX))

        (or (eq "\\" (substr tmp (strlen tmp)))
            (setq tmp (strcat tmp "\\")))

        (strcat tmp "Support\\")
      )
      ( (setq tmp (findfile "ACAD.pat"))

        (setq tmp (vl-filename-directory tmp))

        (or (eq "\\" (substr tmp (strlen tmp)))
            (setq tmp (strcat tmp "\\")))

        tmp
      )
      (t
        (popup "Warning" 16 "DCL Save Path not Valid")

        (exit)
      )
    )
  )
  
  ;;-------------------------------------------------------------------------------;;

  (setq dcfname    (strcat SavePath "LMAC_WScript_V" VersionNumber ".dcl")

        cfgfname   (strcat SavePath "LMAC_WScript_V" VersionNumber ".cfg")

        scrfname   (strcat SavePath "LMAC_WScript_V" VersionNumber ".scr")

        dctitle    (strcat "Script Writer V" VersionNumber))
  
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  
  
  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Local Functions  }==--                       ;;
  ;;-------------------------------------------------------------------------------;;

  (defun *error* ( msg )

    (and Undo  (vla-EndUndoMark doc))
    (and dc    (unload_dialog dc))    
    (and ofile (close ofile))
    
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))    
    (princ)
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Popup (title flags msg / WSHShell result)
    
    (setq WSHShell (vlax-create-object "WScript.Shell"))
    (setq result   (vlax-invoke WSHShell 'Popup msg 0 title flags))
    (vlax-release-object WSHShell)

    result
  )

  ;;-------------------------------------------------------------------------------;;

  (defun write_config ( fname lst / ofile )

    (if (setq ofile (open fname "w"))
      (progn
        
        (foreach x lst
          (write-line (vl-prin1-to-string x) ofile))
        
        (setq ofile (close ofile))

        t
      )
    )
  )

  ;-------------------------------------------------------------------------------;

  (defun read_config  ( fname lst / ofile )

    (if (and (setq fname (findfile fname))
             (setq ofile (open fname "r")))
      (progn

        (foreach x lst
          (set x
            (read
              (read-line ofile)
            )
          )
        )
        (setq ofile (close ofile))

        lst
      )
    )
  )

  ;-------------------------------------------------------------------------------;

  (defun Dir_Text ( key str )
    (set_tile key
      (if str
        (if (< 50 (strlen str))
          (strcat (substr str 1 47) "...") str
        )
        ""
      )
    )
  )

  ;-------------------------------------------------------------------------------;

  (defun DirDialog ( msg dir flag / Shell Fold Path )

    (setq Shell (vla-getInterfaceObject
                  (setq ac (vlax-get-acad-object)) "Shell.Application")
          Fold  (vlax-invoke-method Shell 'BrowseForFolder
                  (vla-get-HWND ac) msg flag dir))
    (vlax-release-object Shell)

    (if Fold
      (progn
        (setq Path (vlax-get-property
                     (vlax-get-property Fold 'Self) 'Path))
        (vlax-release-object Fold)

        (and (= "\\" (substr Path (strlen Path)))
             (setq Path (substr Path 1 (1- (strlen Path)))))
      )
    )
    Path
  )

  ;-------------------------------------------------------------------------------;

  (defun dcl_write ( fname / ofile )

    (if (not (findfile fname))

      (if (setq ofile (open fname "w"))
        (progn

          (foreach str

            '(
                "//-----------------------=={  WScript Dialog Definition  }==---------------------//"
                "//                                                                               //"
                "//  WScript.dcl to be used in conjunction with WScript.lsp.                      //"
                "//-------------------------------------------------------------------------------//"
                "//  Author: Lee Mac, Copyright © May 2010 - www.lee-mac.com                      //"
                "//-------------------------------------------------------------------------------//"
                ""
                "//  --=={ Sub-Assembly Definitions }==--"
                ""
                "butt12  : button { width = 12; fixed_width = true; alignment = centered; }"
                "butt15  : button { width = 15; fixed_width = true; alignment = centered; }"
                "butt15t : button { width = 15; fixed_width = true; alignment = centered; height = 2.5; fixed_height = true; }"
                ""
                "//-------------------------------------------------------------------------------//"
                "//                              Main Dialog Definition                           //"
                "//-------------------------------------------------------------------------------//"
                ""
                "wscript : dialog { key = \"dctitle\";"
                "  spacer;"
                ""
                "  : text     { label = \"Enter Script Operations, use * file*  token for drawing filename\";"
                "               alignment = left; }"
                ""
                "  spacer;"
                ""
                "  : edit_box { edit_width = 51.5; fixed_width = true; label = \"Script Line:\"; key = \"scr\"; }"
                ""
                "  spacer;"
                ""
                "  : row {"
                ""
                "    : butt15 { key = \"fn\"; label = \"&Filename\"   ; mnemonic = \"F\"; }"
                ""
                "    : butt15 { key = \"ld\"; label = \"&Load Script\"; mnemonic = \"L\"; }"
                ""
                "    : butt15 { key = \"sv\"; label = \"&Save Script\"; mnemonic = \"S\"; }"
                ""
                "    : butt15 { key = \"cl\"; label = \"&Clear\"      ; mnemonic = \"C\"; }"
                ""
                "  }"
                ""
                "  spacer;"
                ""
                "  : boxed_column { label = \"Drawing Directory\";"
                ""
                "    : row {"
                ""
                "      : text   { alignment = left; key = \"dir_text\"; }"
                ""
                "      : butt12 { key = \"dir\"; label = \"&Directory\"; mnemonic = \"D\"; }"
                ""
                "    }"
                ""
                "    : toggle { label = \"&Include Sub-Directories\"; key = \"sub_dir\"; mnemonic = \"I\"; }"
                ""
                "  spacer;"
                ""
                "  }"
                ""
                "  spacer;"
                ""
                "  : row {"
                ""
                "    : spacer  { width = 16.06; fixed_width = true; }"
                ""
                "    : butt15t { key = \"accept\"; label = \"&Run Script!\"; is_default = true; mnemonic = \"R\"; }"
                ""
                "    : butt15t { key = \"cancel\"; label = \"C&ancel\"     ; is_cancel  = true; mnemonic = \"a\"; }"
                ""
                "    : column { spacer;"
                ""
                "      : image   { key = \"logo\"; alignment = centered;"
                "                  width = 16.06 ; fixed_width  = true;"
                "                  height = 2.06 ; fixed_height = true; color = -15; }"
                "    }"
                ""
                "  }"
                ""
                "}"
                ""
                "loadscript : dialog { label = \"Select Script to Load\";"
                "  spacer;"
                ""
                "  : list_box { key = \"scrlst\"; width = 64; fixed_width = true; alignment = centered; }"
                ""
                "  spacer;"
                ""
                "  : row {"
                ""
                "    : butt15 { key = \"accept\"; label = \"&Load\"     ; is_default = true; mnemonic = \"L\"; }"
                ""
                "    : butt15 { key = \"cancel\"; label = \"&Cancel\"   ; is_cancel  = true; mnemonic = \"C\"; }"
                ""
                "    : butt15 { key = \"delete\"; label = \"&Delete\"   ; mnemonic = \"D\"; }"
                ""
                "    : butt15 { key = \"browse\"; label = \"&Browse...\"; mnemonic = \"B\"; }"
                ""
                "  }"
                ""
                "  spacer;"
                "  "
                "}"
                ""
                "//-------------------------------------------------------------------------------//"
                "//                                 End of File                                   //"
                "//-------------------------------------------------------------------------------//"
              )

            (write-line str ofile)
          )

          (setq ofile (close ofile))
          
        t)  ; File written successfully
        
    nil) ; File not Opened
      
  t)) ; DCL file already exists

  ;-------------------------------------------------------------------------------;

  (defun logo ( key )
    
    (start_image key)
    (mapcar 'vector_image
            '(22 21 1 0 0 0 0 7 0 0 0 0 1 6 6 6 6 7 43 36 27 36 30 21 21 21 22 22 22
              22 21 21 21 28 28 28 27 27 30 29 29 30 52 43 43 43 44 44 46 46 45 45 45
              45 52 52 52 51 51 51 51 51 52 62 65 66 68 68 68 68 67 67 75 75 75 74 74
              73 66 58 58 59 59 59 59 52 57 57 56 56 56 56 57 58 65 65 65 65 66 95 94
              94 92 91 91 91 90 89 89 88 87 86 85 74 74 75 75 76 77 78 79 80 81 82 83
              84 85 86 87 88 88 89 90 91 92 93 94 95 74 73 73 72 72 71 71 71 71 71 71
              71 72 72 72 73 84 83 82 81 80 79 79 78 77 77 76 76 76 76 76 77 77 78 79
              79 80 81 82 83 94 94 95 83 83 82 81 80 79 78 77 76 75 74 84 85 86 87 88
              89 89 90 91 91 91 91 92 95 94 93 92 91 90 89 89 88 87 86 85 84)

            '(20 20 23 23 23 24 24 0 0 0 0 1 1 20 1 1 1 0 2 24 7 15 0 0 0 0 1 1 23 23
              23 24 24 24 24 24 23 23 2 1 1 0 0 0 0 0 1 1 7 23 23 23 24 24 24 24 24 23
              23 1 1 1 0 10 16 19 21 22 23 24 24 24 24 24 24 23 23 22 4 4 5 5 6 6 7 24
              24 24 24 23 23 22 19 16 7 7 6 5 5 22 22 22 17 17 18 18 19 20 20 20 21 21
              21 21 22 23 23 23 24 24 24 25 25 25 25 25 25 25 25 24 24 24 23 23 22 22
              22 22 7 8 8 9 10 11 12 13 14 15 16 17 18 19 19 20 21 21 21 21 20 20 19 19
              18 17 16 15 14 13 12 12 11 10 9 9 8 8 8 7 7 7 7 4 4 4 4 4 4 4 5 5 6 6 7 7
              8 8 8 9 9 9 10 11 11 11 11 7 7 7 6 6 5 5 4 4 4 4 4 4)

            '(21 6 0 0 0 0 21 0 0 0 0 1 1 6 6 6 7 7 36 46 36 30 21 21 21 22 22 22 22 21
              21 21 28 28 28 27 27 27 29 29 30 30 43 43 43 44 44 43 46 45 45 45 45 52 52
              52 51 51 51 51 51 52 52 65 58 68 68 68 68 67 67 75 75 75 74 74 73 65 58 58
              59 59 59 59 51 57 57 56 56 56 56 57 66 62 65 65 65 66 66 94 94 95 91 91 91
              90 89 89 88 87 86 85 84 74 75 75 76 77 78 79 80 81 82 83 84 85 86 87 88 88
              89 90 91 92 93 94 95 92 73 73 72 72 71 71 71 71 71 71 71 72 72 72 73 74 83
              82 81 80 79 79 78 77 77 76 76 76 76 76 77 77 78 79 79 80 81 82 83 84 94 95
              94 83 82 81 80 79 78 77 76 75 74 74 85 86 87 88 89 89 90 91 91 91 91 92 95
              94 93 92 91 90 89 89 88 87 86 85 84 83)

            '(20 20 23 23 24 24 24 0 0 0 1 1 23 1 1 1 0 0 15 7 24 2 0 0 0 1 1 23 23 23 24
              24 24 24 24 23 23 7 1 1 0 0 0 0 0 1 1 2 23 23 23 24 24 24 24 24 23 23 1 1 1
              0 0 16 16 21 22 23 24 24 24 24 24 24 23 23 22 7 4 5 5 6 6 7 22 24 24 24 23
              23 22 19 19 10 7 6 5 5 4 22 22 22 17 18 18 19 20 20 20 21 21 21 21 22 23 23
              23 24 24 24 25 25 25 25 25 25 25 25 24 24 24 23 23 22 22 22 22 17 8 8 9 10 11
              12 13 14 15 16 17 18 19 19 20 21 21 21 21 20 20 19 19 18 17 16 15 14 13 12 12
              11 10 9 9 8 8 8 7 7 7 7 7 4 4 4 4 4 4 5 5 6 6 7 7 8 8 8 9 9 9 10 11 11 11 11
              7 7 7 6 6 5 5 4 4 4 4 4 4 4)

            '(178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178))
    
    (end_image)
  )

  ;-------------------------------------------------------------------------------;

  (defun MakeList ( key lst )

    (start_list key)
    (mapcar (function add_list) lst)
    (end_list)

    lst
  )

  ;-------------------------------------------------------------------------------;

  (defun RemoveNth ( n lst )
    (
      (lambda ( i )
        (vl-remove-if
          (function
            (lambda ( x )
              (= n (setq i (1+ i)))
            )
          )
          lst
        )
      )
      -1
    )
  )

  ;-------------------------------------------------------------------------------;

  (defun LoadScript ( handle / _read GetScriptLine ptr line tmp )

    (defun _read ( file / ofile nl )
      
      (cond
        ( (setq ofile (open file "r"))
         
          (while (and (setq nl (read-line ofile)) (eq "" nl)))

          (setq ofile (close ofile))
        )
      )
      nl
    )

    (defun GetScriptLine ( string / lst )

      (if (setq lst (StringParser string "\""))
        (apply (function strcat)
          (mapcar
            (function
              (lambda ( x )
                (if (or (not (eq "" (vl-filename-directory x)))
                        (findfile x))
                  "*file*"
                  x
                )
              )
            )
            lst
          )
        )
      )
    )               
        
    (cond
      ( (not (new_dialog "loadscript" handle))

        (popup "Warning" 16 "Load Dialog Could not be Loaded")
        (princ "\n** Load Dialog Could not be Loaded **")
      )
      (t

        (MakeList "scrlst" *SaveLst)
        (if *SaveLst (setq ptr (set_tile "scrlst" "0")))

        (action_tile "scrlst" "(setq ptr $value)")

        (action_tile "delete"
          (vl-prin1-to-string
            (quote
              (progn
                (if ptr
                  (progn
                    (MakeList "scrlst" (setq *SaveLst (RemoveNth (atoi ptr) *SaveLst)))
                    (setq ptr
                      (if *SaveLst
                        (set_tile "scrlst" "0")
                      )
                    )
                  )
                )
              )
            )
          )
        )

        (action_tile "browse"
          (vl-prin1-to-string
            (quote
              (progn
                (if (and (setq tmp   (getfiled "Select Script File" "" "scr" 16))
                         (setq line  (_read tmp))
                         (setq line  (GetScriptLine line)))
                  (progn
                    (setq *SaveLst (cons (setq ptr line) *SaveLst))
                    (done_dialog 1)
                  )
                )                    
              )
            )
          )
        )

        (action_tile "accept" "(if ptr (setq ptr (nth (atoi ptr) *SaveLst))) (done_dialog 1)")

        (action_tile "cancel" "(setq ptr nil) (done_dialog 0)")

        (start_dialog)
      )
    )
    ptr
  )

  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Main Function  }==--                         ;;
  ;;-------------------------------------------------------------------------------;;


  ;;----------------------------=={  Setup Defaults  }==---------------------------;;

  (or (findfile cfgfname)
      (write_config cfgfname (list "_.open *file* _.saveas  *file* _.close" (getvar 'DWGPREFIX) "1" 'nil)))

  (read_config cfgfname '(ScrLine Dir Sub *SaveLst))

  (or ScrLine (setq ScrLine "_.open *file* _.saveas  *file* _.close"))
  (or Dir     (setq Dir (getvar 'DWGPREFIX)))
  (or Sub     (setq Sub "1"))

  ;;-------------------------------------------------------------------------------;;
  
  
  (cond

    ( (not (DCL_Write dcfname))

      (popup "Warning" 16 "DCL File Could not be Written")
      (princ "\n** Dialog File Could not be Written")
    )
    ( (<= (setq dc (load_dialog dcfname)) 0)

      (popup "Warning" 16 "Dialog File not Found")
      (princ "\n** Dialog File not Found **")
    )
    ( (not (new_dialog "wscript" dc))

      (popup "Warning" 16 "Dialog Could not be Loaded")
      (princ "\n** Dialog Could not be Loaded **")
    )
    (t

      (mapcar (function set_tile) '("dctitle" "sub_dir" "scr") (list dctitle Sub ScrLine))

      (Dir_Text "dir_text"
        (setq Dir
          (cond
            ((vl-file-directory-p Dir) Dir) ((getvar 'DWGPREFIX))
          )
        )
      )

      (Logo "logo")

      (action_tile "dir"
        (vl-prin1-to-string
          (quote
            (progn
              (if (setq tmp (DirDialog "Select Directory of Drawings to Process..." nil 512))
                (Dir_Text "dir_text" (setq Dir tmp))
              )
            )
          )
        )
      )

      (action_tile "fn"
        (vl-prin1-to-string
          (quote
            (progn
              (set_tile "scr" (setq ScrLine (strcat ScrLine "*file*")))
            )
          )
        )
      )

      (action_tile "cl"
        (vl-prin1-to-string
          (quote
            (progn
              (set_tile "scr" (setq ScrLine ""))
              (mode_tile "scr" 2)
            )
          )
        )
      )

      (action_tile "ld"
        (vl-prin1-to-string
          (quote
            (progn
              (cond
                ( (setq tmp (LoadScript dc))

                  (setq ScrLine (set_tile "scr" tmp))
                )
              )
            )
          )
        )
      )

      (action_tile "sv"
        (vl-prin1-to-string
          (quote
            (progn
              (cond
                ( (zerop (strlen ScrLine))

                  (popup "Information" 48 "No Script Operations Entered!")
                )
                ( (< (length (setq ScrLst (StringParser ScrLine "*file*"))) 2)

                  (popup "Information" 64 "Delimiter *file* not found in Script String")
                )
                ( (and (setq tmp   (getfiled "Save Script As" "" "scr" 1))
                       (setq ofile (open tmp "w")))

                  (foreach filepath (GetAllFiles Dir (eq "1" Sub) "*.dwg")

                    (write-line (lst->str ScrLst (strcat (chr 34) filepath (chr 34))) ofile)

                  )
                  (setq ofile (close ofile))

                  (if (not (vl-position ScrLine *SaveLst))
                    (setq *SaveLst (cons ScrLine *SaveLst))
                  )

                  (popup "Information" 64 "Script Saved.")
                )
              )
            )
          )
        )
      )

      (action_tile "sub_dir" "(setq Sub $value)")

      (action_tile "scr"     "(setq ScrLine $value)")

      (action_tile "accept"
        (vl-prin1-to-string
          (quote
            (progn
              (cond
                ( (zerop (strlen ScrLine))

                  (popup "Information" 64 "Please Enter a Script Line")
                )
                ( (< (length (setq ScrLst (StringParser ScrLine "*file*"))) 2)

                  (popup "Information" 64 "Delimiter *file* not found in Script String")
                )                  
                ( (done_dialog 1) )
              )
            )
          )
        )
      )

      (setq dcFlag (start_dialog) dc (unload_dialog dc))

      (if (and (= 1 dcFlag) (setq ofile (open scrfname "w")))
        (progn
          (setq Undo (not (vla-StartUndoMark doc)))

          (foreach filepath (GetAllFiles Dir (eq "1" Sub) "*.dwg")

            (write-line (lst->str ScrLst (strcat (chr 34) filepath (chr 34))) ofile)

          )
          (setq ofile (close ofile))

          (write_config cfgfname (list ScrLine Dir Sub *SaveLst))

          (setq Undo (vla-EndUndomark doc))

          (vl-cmdf "_.script" scrfname)
        )
        (princ "\n*Cancel*")
      )
    )
  )
  (princ)
)

(defun StringParser ( str del / pos lst )
  ;; © Lee Mac
  (while (setq pos (vl-string-search del str))
    (setq lst (cons (substr str 1 pos) lst)
          str (substr str (+ pos 1 (strlen del))))
  )
  (reverse (cons str lst))
)

(defun lst->str ( lst del )
  ;; © Lee Mac
  (
    (lambda ( str )
      (foreach x (cdr lst)
        (setq str (strcat str del x))
      )
      str
    )
    (car lst)
  )
)

;; Get All Files (Lee Mac)
;; Retrieves all Files in a Directory (and SubDirectories) which optional filter
;; Dir      ~  [STR]   (optional) Directory to Search, if nil function prompts for directory
;; subs     ~  [BOOLE] if T, subdirectories are included.
;; filetype ~  [STR]   (optional) Filter for Files of a specific type.

(defun GetAllFiles ( Dir Subs Filetype / GetSubFolders ac Shell Fold Dir )
  
  (defun GetSubFolders (folder / _f)
    (mapcar
      (function
        (lambda ( f ) (setq _f (strcat folder "\\" f))
          (cons _f (apply (function append)
                          (GetSubFolders _f)))
        )
      )
      (cddr (vl-directory-files folder nil -1))
    )
  )

  (cond
    ( (not
        (or
          (and Dir (vl-file-directory-p Dir))
          (progn
            (setq Shell (vla-getInterfaceObject
                          (setq ac (vlax-get-acad-object)) "Shell.Application")
                  Fold  (vlax-invoke-method Shell 'BrowseForFolder
                          (vla-get-HWND ac) "Select Directory" 512))
            (vlax-release-object Shell)
            
            (if Fold
              (progn
                (setq Dir (vlax-get-property
                            (vlax-get-property Fold 'Self) 'Path))
                (vlax-release-object Fold)
                
                (and (= "\\" (substr Dir (strlen Dir)))
                     (setq Dir (substr Dir 1 (1- (strlen Dir)))))
                
                Dir
              )
            )
          )
        )
      )
    )
    ( (apply (function append)
        (vl-remove (quote nil)
          (mapcar
            (function
              (lambda ( Filepath )
                (mapcar
                  (function
                    (lambda ( Filename )
                      (strcat Filepath "\\" Filename)
                    )
                  )
                  (vl-directory-files Filepath Filetype 1)
                )
              )
            )
            (append (list Dir)
              (apply (function append)
                (if subs (GetSubFolders Dir))
              )
            )
          )
        )
      )
    )
  )
)

(vl-load-com)
(princ "\n:: Script Writer | Version 1.2 | © Lee Mac 2010 www.lee-mac.com ::")
(princ "\n:: Type \"WScript\" to Invoke ::")
(princ)

;;-------------------------------------------------------------------------------;;
;;                                 End of File                                   ;;
;;-------------------------------------------------------------------------------;;