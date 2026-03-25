;;---------------------------------=={ Steal }==---------------------------------;;
;;                                                                               ;;
;;  Program Overview                                                             ;;
;;  --------------------------------------                                       ;;
;;                                                                               ;;
;;  Allows the user to import (humourously: 'steal') items from another drawing  ;;
;;  into the active drawing.                                                     ;;
;;                                                                               ;;
;;  Upon running the program with 'Steal' at the AutoCAD command-line, the user  ;;
;;  is prompted to select a drawing to steal from (dwg/dwt/dws). Upon selection, ;;
;;  if the selected drawing contains items not already present in the current    ;;
;;  drawing, a dialog will appear detailing items available for import.          ;;
;;                                                                               ;;
;;  The user may choose multiple items from a list of:                           ;;
;;                                                                               ;;
;;    -  Blocks                                                                  ;;
;;    -  Layers                                                                  ;;
;;    -  Linetypes                                                               ;;
;;    -  Dimension Styles                                                        ;;
;;    -  Text Styles                                                             ;;
;;    -  Table Styles                                                            ;;
;;    -  MLeader Styles                                                          ;;
;;    -  MLine Styles                                                            ;;
;;    -  Layouts                                                                 ;;
;;    -  Page Setups                                                             ;;
;;    -  User Coordinate Systems                                                 ;;
;;    -  Views                                                                   ;;
;;    -  Layer States                                                            ;;
;;    -  Scales                                                                  ;;
;;    -  Materials                                                               ;;
;;    -  Viewports                                                               ;;
;;                                                                               ;;
;;  The above collections are listed in the left-hand list panel of the dialog,  ;;
;;  and items within each selected collection are listed to the right.           ;;
;;                                                                               ;;
;;  The user may import multiple items from several collections without exiting  ;;
;;  or restarting the program, since the dialog will remain until either the     ;;
;;  user presses the 'Done' button, or there is nothing left to import from the  ;;
;;  selected drawing.                                                            ;;
;;                                                                               ;;
;;  The user may also search the collections for a specific item to import       ;;
;;  using the search panel to the bottom of the dialog. The search is not        ;;
;;  case-sensitive and may use wildcards.                                        ;;
;;                                                                               ;;
;;  The program may also be called with the command 'StealAll' which will prompt ;;
;;  the user to select a drawing file then proceed to automatically steal        ;;
;;  everything from the selected drawing.                                        ;;
;;                                                                               ;;
;;  Another command: 'StealTemplate' will attempt to locate either the QNew      ;;
;;  template file, or a template file residing in the template path and display  ;;
;;  a dialog allowing the user to steal items from the located template file.    ;;
;;  If no template file is found, the file selection dialog will appear as       ;;
;;  normal.                                                                      ;;
;;                                                                               ;;
;;  Calling the Program as a Subfunction                                         ;;
;;  --------------------------------------                                       ;;
;;                                                                               ;;
;;  The program may also be called with optional parameters so that users may    ;;
;;  bypass the dialog interface or file selection dialog, or perhaps call the    ;;
;;  function from other programs or from an ACADDOC.lsp to import items          ;;
;;  automatically.                                                               ;;
;;                                                                               ;;
;;  Format:                                                                      ;;
;;                                                                               ;;
;;      (Steal <Dwg> <ItemList>)                                                 ;;
;;                                                                               ;;
;;  Arguments:                                                                   ;;
;;                                                                               ;;
;;  Dwg  [STR]  [Optional]                                                       ;;
;;  --------------------------------                                             ;;
;;  The full filename of the drawing from which items are to be imported.        ;;
;;                                                                               ;;
;;  ItemList  [LIST]  [Optional]                                                 ;;
;;  --------------------------------                                             ;;
;;  A list of items to be imported, in the following format:                     ;;
;;                                                                               ;;
;;  (                                                                            ;;
;;      (<Collection1> <Item1> <Item2>  ...  <ItemN>)                            ;;
;;      (<Collection2> <Item1> <Item2>  ...  <ItemN>)                            ;;
;;       ...                                                                     ;;
;;      (<CollectionN> <Item1> <Item2>  ...  <ItemN>)                            ;;
;;  )                                                                            ;;
;;                                                                               ;;
;;  Where 'Collection' is the name of a collection of items, and may be one of:  ;;
;;                                                                               ;;
;;     "Blocks"                                                                  ;;
;;     "Layers"                                                                  ;;
;;     "Linetypes"                                                               ;;
;;     "Text Styles"                                                             ;;
;;     "Dimension Styles"                                                        ;;
;;     "Layouts"                                                                 ;;
;;     "Views"                                                                   ;;
;;     "Materials"                                                               ;;
;;     "Viewports"                                                               ;;
;;     "Page Setups"                                                             ;;
;;     "User Coordinate Systems"                                                 ;;
;;     "Multileader Styles"                                                      ;;
;;     "Multiline Styles"                                                        ;;
;;     "Table Styles"                                                            ;;
;;     "Scales"                                                                  ;;
;;     "Layer States"                                                            ;;
;;                                                                               ;;
;;  Note that the collection keyword is *not* case-sensitive.                    ;;
;;                                                                               ;;
;;  'Item1' ... 'ItemN' are the names of specific items or wildcard patterns to  ;;
;;  match a number of items to be imported into the current drawing. Note that   ;;
;;  these are also *not* case-sensitive.                                         ;;
;;                                                                               ;;
;;  Examples                                                                     ;;
;;  --------------------------------------                                       ;;
;;                                                                               ;;
;;  The following example will attempt to import Layers: 'Layer1' & 'Layer2',    ;;
;;  and all Dimension Styles beginning with 'DimStyle' (not case-sensitive)      ;;
;;  from the drawing: 'C:\My Folder\MyDrawing.dwg' into the current drawing.     ;;
;;                                                                               ;;
;;  (Steal "C:\\My Folder\\MyDrawing.dwg"                                        ;;
;;     '(                                                                        ;;
;;          ("Layers" "Layer1" "Layer2")                                         ;;
;;          ("Dimension Styles" "DimStyle*")                                     ;;
;;      )                                                                        ;;
;;  )                                                                            ;;
;;                                                                               ;;
;;  Note that both arguments are optional, hence the program may be called...    ;;
;;                                                                               ;;
;;  (Steal nil                                                                   ;;
;;     '(                                                                        ;;
;;          ("Layers" "Layer1" "Layer2")                                         ;;
;;          ("Dimension Styles" "DimStyle*")                                     ;;
;;      )                                                                        ;;
;;  )                                                                            ;;
;;                                                                               ;;
;;  ...to prompt for a drawing from which to extract specific items, or...       ;;
;;                                                                               ;;
;;  (Steal "C:\\My Folder\\MyDrawing.dwg" nil)                                   ;;
;;                                                                               ;;
;;  ...to prompt for items to extract from a specific drawing.                   ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Function Syntax:  Steal  /  StealAll  /  StealTemplate                       ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author:                                                                      ;;
;;                                                                               ;;
;;  Copyright © 2012 Lee Mac  -  www.lee-mac.com                                 ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version History:                                                             ;;
;;  --------------------------------------                                       ;;
;;                                                                               ;;
;;  1.0:  16/01/2011  -  First Release.                                          ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.1:  16/01/2011  -  Added Layouts & Page Setups to list of Collections.     ;;
;;                    -  Added ability to import multiple items until dialog is  ;;
;;                       closed.                                                 ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.2:  18/01/2011  -  Omitted XRef items from collection lists.               ;;
;;                    -  Added Search bar to search for specific items.          ;;
;;                    -  Added Named Views & UCS to list of collections.         ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.3:  18/01/2011  -  Accounted for Nested blocks.                            ;;
;;                    -  Added LayerStates to list of collections.               ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.4:  19/01/2011  -  Added dwt/dws compatibility.                            ;;
;;                    -  Added command-line functionality.                       ;;
;;                    -  Added Scales to list of collections.                    ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.5:  07/09/2011  -  Added ability to use Wildcard patterns in ItemList      ;;
;;                       argument to import all items matching the pattern.      ;;
;;                    -  Added Materials & Viewports to list of collections.     ;;
;;                    -  Added 'StealAll' command to quickly steal everything    ;;
;;                       from a drawing.                                         ;;
;;                    -  Added 'StealTemplate' command to quickly steal items    ;;
;;                       from the QNewTemplate or other Template file.           ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.6:  29/04/2012  -  Fixed bug causing the program to crash for versions in  ;;
;;                       which some collections are not available.               ;;
;;                    -  Expanded program header and removed local functions to  ;;
;;                       improve program performance.                            ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;

(setq StealVersionNumber "1.6")

;;-------------------------------------------------------------------------------;;
;;                                Program Commands                               ;;
;;-------------------------------------------------------------------------------;;

(defun c:Steal nil (Steal nil nil))

(defun c:StealAll nil
    (Steal nil
       '(
            ("Blocks" "*")
            ("Layers" "*")
            ("Linetypes" "*")
            ("Text Styles" "*")
            ("Dimension Styles" "*")
            ("Layouts" "*")
            ("Views" "*")
            ("Materials" "*")
            ("Viewports" "*")
            ("Page Setups" "*")
            ("User Coordinate Systems" "*")
            ("Multileader Styles" "*")
            ("Multiline Styles" "*")
            ("Table Styles" "*")
            ("Scales" "*")
            ("Layer States" "*")
        )
    )
)

(defun c:StealTemplate nil
    (
        (lambda ( acprf / temp )
            (Steal
                (cond
                    (   (not (eq "" (setq temp (vla-get-qnewtemplatefile acprf))))
                        temp
                    )
                    (   (setq temp (car (vl-directory-files (vla-get-templatedwgpath acprf) "*.dwt" 1)))
                        (strcat (vl-string-right-trim "\\" (vla-get-templatedwgpath acprf)) "\\" temp)
                    )
                )
                nil
            )
        )
        (vla-get-files (vla-get-preferences (vlax-get-acad-object)))
    )
)

;;-------------------------------------------------------------------------------;;
;;                                Steal Subfunction                              ;;
;;-------------------------------------------------------------------------------;;

(defun Steal

    (
        dwg
        ItemList

        /

        *error*
        _performsearch

        _item _items
        acapp acdata acdic acdoc acext aclay
        collection collections
        data dbxdic dbxdoc dbxext dbxlay dc dcfname
        file
        i item items
        pair phrase
        search
    )
  
    (defun *error* ( msg )
        (vl-catch-all-apply
            (function
                (lambda nil
                    (if dbxDoc (vlax-release-object dbxDoc))
                    (if (and  file (eq 'FILE (type file)))
                        (setq file (close file))
                    )
                    (if (and dcfname (setq dcfname (findfile dcfname)))
                        (vl-file-delete dcfname)
                    )
                    (if dc (unload_dialog dc))
                )
            )
        )
        (if (null (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

;;-------------------------------------------------------------------------------;;
;;                                Main Function                                  ;;
;;-------------------------------------------------------------------------------;;
  
    (setq acapp (vlax-get-acad-object)
          acdoc (vla-get-ActiveDocument acapp)
          acdic (vla-get-Dictionaries acdoc)
          aclay (vla-get-Layers acdoc)
          acext (if (eq :vlax-true (vla-get-HasExtensionDictionary aclay))
                    (vla-getExtensionDictionary aclay)
                )
    )

    (cond
        (   (not
                (or dwg
                    (setq dwg
                        (getfiled "Select Drawing to Steal From" (cond ( *drawing* ) ( "" )) "dwg;dwt;dws" 16)
                    )
                )
            )
            (princ "\n*Cancel*")
        )
        (   (eq (strcase dwg) (strcase (vla-get-fullname acdoc)))
            (princ "\nCannot Steal from the Current Drawing.")
        )
        (   (not (setq dbxDoc (Steal:GetDocumentObject dwg)))
            (princ (strcat "\nUnable to Interface with drawing: " dwg))
        )
        (   (not
                (progn
                    (setq dbxdic (vla-get-Dictionaries dbxDoc)
                          dbxlay (vla-get-Layers       dbxdoc)
                          dbxext (if (eq :vlax-true (vla-get-HasExtensionDictionary dbxlay))
                                     (vla-getExtensionDictionary dbxlay)
                                 )
                    )
                    (setq data
                        (vl-remove 'nil
                            (mapcar
                                (function
                                    (lambda ( collection1 collection2 / items l n )
                                        (if
                                            (setq items
                                                (cond
                                                    (   (eq "Scales" (car collection1))
                                                        (if (and (cdr collection1) collection2)
                                                            (progn
                                                                (vlax-for s (cdr collection1)
                                                                    (setq l (cons (cdr (assoc 300 (entget (vlax-vla-object->ename s)))) l))
                                                                )
                                                                (vlax-for s collection2
                                                                    (if (not (vl-position (setq n (cdr (assoc 300 (entget (vlax-vla-object->ename s))))) l))
                                                                        (setq items (cons (cons n s) items))
                                                                    )
                                                                )
                                                                (vl-sort items (function (lambda ( a b ) (< (car a) (car b)))))
                                                            )
                                                        )
                                                    )
                                                    (   (and (cdr collection1) collection2)
                                                        (Steal:GetItemsinCollection (cdr collection1) collection2)
                                                    )
                                                )
                                            )
                                            (cons (car collection1) items)                     
                                        )
                                    )
                                )
                                (setq acdata
                                    (list
                                        (cons "Blocks"                  (Steal:GetProperty acdoc 'blocks))
                                        (cons "Layers"                  (Steal:GetProperty acdoc 'layers))
                                        (cons "Linetypes"               (Steal:GetProperty acdoc 'linetypes))
                                        (cons "Text Styles"             (Steal:GetProperty acdoc 'textstyles))
                                        (cons "Dimension Styles"        (Steal:GetProperty acdoc 'dimstyles))
                                        (cons "Layouts"                 (Steal:GetProperty acdoc 'layouts))
                                        (cons "Views"                   (Steal:GetProperty acdoc 'views))
                                        (cons "Materials"               (Steal:GetProperty acdoc 'materials))
                                        (cons "Viewports"               (Steal:GetProperty acdoc 'viewports))
                                        (cons "Page Setups"             (Steal:GetProperty acdoc 'plotconfigurations))
                                        (cons "User Coordinate Systems" (Steal:GetProperty acdoc 'usercoordinatesystems))
                                        (cons "Multileader Styles"      (Steal:GetItem acdic "ACAD_MLEADERSTYLE"))
                                        (cons "Multiline Styles"        (Steal:GetItem acdic "ACAD_MLINESTYLE" ))
                                        (cons "Table Styles"            (Steal:GetItem acdic "ACAD_TABLESTYLE" ))
                                        (cons "Scales"                  (Steal:GetItem acdic "ACAD_SCALELIST"  ))
                                        (cons "Layer States"            (Steal:GetItem acext "ACAD_LAYERSTATES"))
                                    )
                                )
                                (list
                                    (Steal:GetProperty dbxdoc 'blocks)
                                    (Steal:GetProperty dbxdoc 'layers)
                                    (Steal:GetProperty dbxdoc 'linetypes)
                                    (Steal:GetProperty dbxdoc 'textstyles)
                                    (Steal:GetProperty dbxdoc 'dimstyles)
                                    (Steal:GetProperty dbxdoc 'layouts)
                                    (Steal:GetProperty dbxdoc 'views)
                                    (Steal:GetProperty dbxdoc 'materials)
                                    (Steal:GetProperty dbxdoc 'viewports)
                                    (Steal:GetProperty dbxdoc 'plotconfigurations)
                                    (Steal:GetProperty dbxdoc 'usercoordinatesystems)
                                    (Steal:GetItem dbxdic "ACAD_MLEADERSTYLE")
                                    (Steal:GetItem dbxdic "ACAD_MLINESTYLE")
                                    (Steal:GetItem dbxdic "ACAD_TABLESTYLE")
                                    (Steal:GetItem dbxdic "ACAD_SCALELIST")
                                    (Steal:GetItem dbxext "ACAD_LAYERSTATES")
                                )
                            )
                        )
                    )
                )
            )
            (princ "\nNo Distinct Items found in Drawing.")
        )
        (   ItemList

            (setq data
                (mapcar
                    (function
                        (lambda ( x )
                            (cons (strcase (car x))
                                (mapcar
                                    (function
                                        (lambda ( y ) (cons (strcase (car y)) (cdr y)))
                                    )
                                    (cdr x)
                                )
                            )
                        )
                    )
                    data
                )
            )
            (setq acdata
                (mapcar
                    (function
                        (lambda ( x ) (cons (strcase (car x)) (cdr x)))
                    )
                    acdata
                )
            )

            (while (setq item (car ItemList))
                (setq collection (strcase (car item))
                      items      (mapcar 'strcase (cdr item))
                      ItemList   (cdr ItemList)
                      _item      nil
                      _items     nil
                )
                (cond
                    (   (or (null collection) (null items))
                    )
                    (   (null (assoc collection data))
                    )
                    (   (eq "LAYER STATES" collection)
                        (if layerstate-importfromdb
                            (foreach pair (cdr (assoc collection data))
                                (if
                                    (vl-some
                                        (function
                                            (lambda ( pattern ) (wcmatch (car pair) pattern))
                                        )
                                        items
                                    )
                                    (layerstate-importfromdb (car pair) dwg)
                                )
                            )
                        )
                    )
                    (
                        (progn
                            (foreach pair (cdr (assoc collection data))
                                (if
                                    (setq _item
                                        (vl-some
                                            (function
                                                (lambda ( pattern )
                                                    (if (wcmatch (car pair) pattern) (cdr pair))
                                                )
                                            )
                                            items
                                        )
                                    )
                                    (setq _items (cons _item _items))
                                )
                            )
                            _items
                        )
                        (vl-catch-all-error-p
                            (vl-catch-all-apply 'vla-CopyObjects
                                (list dbxDoc
                                    (vlax-make-variant
                                        (vlax-safearray-fill
                                            (vlax-make-safearray vlax-vbObject (cons 0 (1- (length _items)))) _items
                                        )
                                    )
                                    (cdr (assoc collection acdata))
                                )
                            )
                        )
                    )
                )
            )
        )
        (
            (not
                (and
                    (setq dcfname (vl-filename-mktemp nil nil ".dcl"))
                    (setq file (open dcfname "w"))
                    (progn
                        (foreach x
                           '(
                                "lbox  : list_box { width = 30; height =  20; fixed_width = true; fixed_height = true; alignment = centered; }"
                                "butt  : button   { width = 15; height = 2.5; fixed_width = true; fixed_height = true; }"
                                ""
                                "steal : dialog { key = \"dctitle\";"
                                "  spacer; : text { label = \"Copyright (c) Lee Mac 2012\"; alignment = right; } spacer;"
                                "  : row { : lbox { key = \"l1\"; }"
                                "          : lbox { key = \"l2\"; multiple_select = true; allow_accept = true; } }"
                                "  : row { alignment = centered;"
                                "          : edit_box { key = \"search1\"; }"
                                "          : button   { width = 12; fixed_width = true; height = 1.7; fixed_height = true;"
                                "                       key = \"search2\"; label = \"Search\"; } } spacer;"
                                "  : row { spacer;"
                                "          : butt { key = \"accept\"; is_default = true; label = \"Import\"; }"
                                "          : butt { key = \"cancel\"; is_cancel  = true; label = \"Done\";   } spacer; }"
                                "}"
                            )
                            (write-line x file)
                        )
                        (not (setq file (close file)))
                    )
                )
            )
            (princ "\nUnable to Write DCL File.")
        )
        (   (<= (setq dc (load_dialog dcfname)) 0)
            (princ "\nDCL File not Found.")
        )
        (   (not (new_dialog "steal" dc))
            (setq dc (unload_dialog dc))
            (princ "\nUnable to Load Dialog.")
        )
        (   t
            (setq *drawing* (vl-filename-directory dwg))
 
            (set_tile "dctitle" (strcat "Steal V" StealVersionNumber))

            (Steal:dcList  "l1" (setq collections (acad_strlsort (mapcar 'car data))))
            (set_tile "l1" (setq collection "0"))
           
            (Steal:dcList  "l2" (setq items (mapcar 'car (cdr (assoc (car collections) data)))))
            (set_tile "l2" (setq item "0"))

            (defun _PerformSearch nil
                (cond
                    (   (or (eq "" search) (not search))
                        (alert "Please Enter a Search String")
                        (mode_tile "search1" 2)
                    )
                    (   (not (setq pair (Steal:Search (strcat "*" search "*") data)))
                        (alert "No Items Found")
                        (mode_tile "search1" 2)
                    )
                    (   t
                        (setq collection (set_tile "l1" (itoa (vl-position (car pair) collections))))
                        (Steal:dcList "l2"
                            (setq items
                                (mapcar 'car (cdr (assoc (car pair) data)))
                            )
                        )
                        (setq item (set_tile "l2" (itoa (vl-position (cdr pair) items))))
                    )
                )
                (setq search (set_tile "search1" ""))
            )

            (action_tile "search1" "(setq search (strcase $value)) (if (= 1 $reason) (_PerformSearch))")
            (action_tile "search2" "(_PerformSearch)")
                                    
            (action_tile "l1"
                (vl-prin1-to-string
                    (quote
                        (progn
                            (Steal:dcList "l2"
                                (setq items
                                    (mapcar 'car
                                        (cdr
                                            (assoc
                                                (nth (atoi (setq collection $value)) collections)
                                                data
                                            )
                                        )
                                    )
                                )
                            )
                            (setq item
                                (set_tile "l2"
                                    (vl-string-trim "()"
                                        (vl-princ-to-string
                                            (cond
                                                (
                                                    (
                                                        (lambda ( _max )
                                                            (vl-remove-if-not (function (lambda ( x ) (< x _max)))
                                                                (read (strcat "(" item ")"))
                                                            )
                                                        )
                                                        (length items)
                                                    )
                                                )
                                                ( '("0") )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )

            (action_tile "l2" "(setq item $value)")

            (action_tile "accept"
                (vl-prin1-to-string
                    (quote
                        (progn
                            (
                                (lambda ( _collection _item / dbxitems )
                                    (if
                                        (cond
                                            (   (eq "Layer States" _collection)
                                                (if layerstate-importfromdb
                                                    (apply 'and
                                                        (
                                                            (lambda ( dbxitems )
                                                                (mapcar
                                                                    (function
                                                                        (lambda ( x )
                                                                            (layerstate-importfromdb (car (nth x dbxitems)) dwg)
                                                                        )
                                                                    )
                                                                    _item
                                                                )
                                                            )
                                                            (cdr (assoc _collection data))
                                                        )
                                                    )
                                                )
                                            )
                                            (
                                                (not
                                                    (vl-catch-all-error-p
                                                        (vl-catch-all-apply 'vla-CopyObjects
                                                            (list dbxDoc
                                                                (vlax-make-variant
                                                                    (vlax-safearray-fill (vlax-make-safearray vlax-vbObject (cons 0 (1- (length _item))))
                                                                        (
                                                                            (lambda ( dbxitems )
                                                                                (mapcar
                                                                                    (function
                                                                                        (lambda ( x ) (cdr (nth x dbxitems)))
                                                                                    )
                                                                                    _item
                                                                                )
                                                                            )
                                                                            (cdr (assoc _collection data))
                                                                        )
                                                                    )
                                                                )
                                                                (cdr (assoc _collection acdata))
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                        (if
                                            (and
                                                (setq data
                                                    (if
                                                        (setq dbxitems
                                                            (cond
                                                                (   (eq "Blocks" _collection)
                                                                    (Steal:GetItemsinCollection (vla-get-Blocks acdoc) (vla-get-blocks dbxdoc))
                                                                )
                                                                (   (Steal:RemoveItems _item (cdr (assoc _collection data)))   )
                                                            )
                                                        )
                                                        (subst (cons _collection dbxitems) (assoc _collection data) data)
                                                        (vl-remove (assoc _collection data) data)
                                                    )
                                                )
                                                (setq data
                                                    (if (eq "Layer States" _collection)
                                                        (if (setq dbxitems (Steal:GetItemsinCollection aclay dbxlay))
                                                            (subst (cons "Layers" dbxitems) (assoc "Layers" data) data)
                                                            (vl-remove (assoc "Layers" data) data)
                                                        )
                                                        data
                                                    )
                                                )
                                            )
                                            (progn
                                                (Steal:dcList  "l1" (setq collections (acad_strlsort (mapcar 'car data))))
                                              
                                                (setq collection
                                                    (set_tile "l1"
                                                        (if (< (atoi collection) (length collections)) collection "0")
                                                    )
                                                )
                                                (Steal:dcList "l2"
                                                    (setq items
                                                        (mapcar 'car
                                                            (cdr (assoc (nth (atoi collection) collections) data))
                                                        )
                                                    )
                                                )
                                                (setq item (set_tile "l2" "0"))
                                            )
                                            (done_dialog 1)
                                        )
                                    )
                                )
                                (nth (atoi collection) collections) (read (strcat "(" item ")"))
                            )
                        )
                    )
                )
            )
            (start_dialog)
            (setq dc (unload_dialog dc))
            (vla-Regen acdoc acAllViewports)
        )
    )
    (if (and dcfname (setq dcfname (findfile dcfname)))
        (vl-file-delete dcfname)
    )
    (if dbxDoc (vlax-release-object dbxDoc))
    (princ)
)

;;-------------------------------------------------------------------------------;;

(defun Steal:GetProperty ( object property )
    (if (vlax-property-available-p object property)
        (vlax-get-property object property)
    )
)

;;-------------------------------------------------------------------------------;;

(defun Steal:GetItemsinCollection ( collection1 collection2 / pair return )
    (vl-sort
        (progn
            (vlax-for item collection2
                (setq pair
                    (cons
                        (cond
                            (   (vlax-property-available-p item 'name)
                                (vla-get-name item)
                            )
                            (   (cdr (assoc 2 (entget (vlax-vla-object->ename item))))   )
                        )
                        item
                    )
                )
                (if
                    (not
                        (or
                            (member (car pair) '("" nil))
                            (wcmatch (car pair) "`**,*|*")
                            (Steal:GetItem collection1 (car pair))
                            (and
                                (vlax-property-available-p (cdr pair) 'isXref)
                                (eq :vlax-true (vla-get-isXref (cdr pair)))
                            )
                            (and
                                (vlax-property-available-p (cdr pair) 'isLayout)
                                (eq :vlax-true (vla-get-isLayout (cdr pair)))
                            )
                        )
                    )
                    (setq return (cons pair return))
                )
            )
            return
        )
        (function (lambda ( a b ) (< (car a) (car b))))
    )
)

;;-------------------------------------------------------------------------------;;

(defun Steal:GetItem ( collection item )
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

;;-------------------------------------------------------------------------------;;

(defun Steal:dcList ( key lst )
    (start_list key)
    (mapcar 'add_list lst)
    (end_list)
)

;;-------------------------------------------------------------------------------;;

(defun Steal:RemoveItems ( items lst / i )
    (setq i -1)
    (vl-remove-if (function (lambda ( x ) (member (setq i (1+ i)) items))) lst)
)

;;-------------------------------------------------------------------------------;;

(defun Steal:Search ( phrase data )
    (vl-some
        (function
            (lambda ( collection )
                (vl-some
                    (function
                        (lambda ( item )
                            (if (wcmatch (strcase (car item)) phrase)
                                (cons (car collection) (car item))
                            )
                        )
                    )
                    (cdr collection)
                )
            )
        )
        data
    )
)

;;-------------------------------------------------------------------------------;;

(defun Steal:GetDocumentObject ( filename / acdocs dbx )
    (vlax-map-collection (vla-get-Documents (vlax-get-acad-object))
        (function
            (lambda ( doc )
                (setq acdocs
                    (cons (cons (strcase (vla-get-fullname doc)) doc) acdocs)
                )
            )
        )
    )
    (cond
        (   (not (setq filename (findfile filename)))
            nil
        )
        (   (cdr (assoc (strcase filename) acdocs))
        )
        (   (not
                (vl-catch-all-error-p
                    (vl-catch-all-apply 'vla-open
                        (list (setq dbx (Steal:ObjectDBXDocument (vlax-get-acad-object))) filename)
                    )
                )
            )
            dbx
        )
    )
)

;;-------------------------------------------------------------------------------;;

(defun Steal:ObjectDBXDocument ( acapp / acver )
    (vla-GetInterfaceObject acapp
        (if (< (setq acver (atoi (getvar "ACADVER"))) 16)
            "ObjectDBX.AxDbDocument"
            (strcat "ObjectDBX.AxDbDocument." (itoa acver))
        )
    )
)

;;-------------------------------------------------------------------------------;;

(vl-load-com)
(princ)
(princ
    (strcat
        "\n:: Steal.lsp | Version "
        StealVersionNumber
        " | © Lee Mac "
        (menucmd "m=$(edtime,$(getvar,DATE),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Type \"Steal\" | \"StealAll\" | \"StealTemplate\" to Invoke ::"
    )
)
(princ)

;;-------------------------------------------------------------------------------;;
;;                                 End of File                                   ;;
;;-------------------------------------------------------------------------------;;