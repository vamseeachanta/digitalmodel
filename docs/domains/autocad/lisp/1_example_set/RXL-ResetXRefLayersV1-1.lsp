;;-------------------=={ Reset XRef Layers }==----------------;;
;;                                                            ;;
;;  Resets specific properties of all layers dependent on the ;;
;;  selected External Reference(s) to those set in the source ;;
;;  drawing file(s).                                          ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    19-11-2011                            ;;
;;                                                            ;;
;;  First Release.                                            ;;
;;------------------------------------------------------------;;
;;  Version 1.1    -    27-11-2011                            ;;
;;                                                            ;;
;;  Added code to search for XRef Source File in working      ;;
;;  directory & support directories if not found at XRef Path ;;
;;------------------------------------------------------------;;

(defun c:RXL nil (c:ResetXRefLayers))

(defun c:ResetXRefLayers

    ( / *error* _Settings _GetDocumentObject acapp acdoc acdocs acver dbxdoc dcl def han inc lst props sel tile tiles xrf )

    (defun *error* ( msg )
        (if (< 0 han) (setq han (unload_dialog han)))
        (if (and dcl  (setq dcl (findfile dcl))) (vl-file-delete dcl))
        (if (and dbxdoc (not (vlax-object-released-p dbxdoc))) (vlax-release-object dbxdoc))
        (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (defun _Settings ( code / bit dcl han tmp )
        (cond
            (
                (not
                    (and
                        (setq dcl (vl-filename-mktemp nil nil ".dcl"))
                        (setq tmp (open dcl "w"))
                        (progn
                            (foreach line
                               '(
                                    "rxl : dialog { label = \"Settings\"; spacer;"
                                    "    : boxed_column { label = \"Properties to Reset\"; width = 65.0; fixed_width = true; alignment = centered; spacer;"
                                    "        : row { alignment = centered; spacer; "
                                    "            : column {"
                                    "                : toggle { key = \"colour\";     label = \"Colour\"; }"
                                    "                : toggle { key = \"linetype\";   label = \"Linetype\"; }"
                                    "                : toggle { key = \"lineweight\"; label = \"Lineweight\"; }"
                                    "            }"
                                    "            : column {"
                                    "                : toggle { key = \"plot\";      label = \"Plot\"; }"
                                    "                : toggle { key = \"plotstyle\"; label = \"Plot Style\"; }"
                                    "                : toggle { key = \"frozenvp\";  label = \"Frozen in VP\"; }"
                                    "            }"
                                    "            : column {"
                                    "                : toggle { key = \"on\";     label = \"On\"; }"
                                    "                : toggle { key = \"locked\"; label = \"Locked\"; }"
                                    "                : toggle { key = \"frozen\"; label = \"Frozen\"; }"
                                    "            }"
                                    "            : column {"
                                    "                : toggle { key = \"description\"; label = \"Description\"; }"
                                    "                spacer;"
                                    "                : toggle { key = \"selectall\";   label = \"Select All\"; }"
                                    "            }"
                                    "        }"
                                    "        spacer;"
                                    "    }"
                                    "    spacer; ok_cancel;"
                                    "}"
                                )
                                (write-line line tmp)
                            )
                            (setq tmp (close tmp))
                            (while (null (findfile dcl)))
                            (< 0 (setq han (load_dialog dcl)))
                        )
                        (new_dialog "rxl" han)
                    )
                )
                (princ "\nError Loading Dialog.")
            )
            (   t
                (setq bit 1
                      tmp code
                )
                (if (= 1023 tmp)
                    (set_tile "selectall" "1")
                )
                (foreach tile
                    (setq tiles
                       '(
                            "colour"
                            "linetype"
                            "lineweight"
                            "plot"
                            "plotstyle"
                            "frozenvp"
                            "on"
                            "locked"
                            "frozen"
                            "description"
                        )
                    )
                    (if (= bit (logand tmp bit))
                        (set_tile tile "1")
                        (set_tile tile "0")
                    )
                    (action_tile tile
                        (strcat
                            "(setq tmp (boole 6 tmp " (itoa bit) "))"
                            "(set_tile \"selectall\" (if (= 1023 tmp) \"1\" \"0\")))"
                        )
                    )
                    (setq bit (lsh bit 1))
                )
                (action_tile "selectall"
                    (strcat
                        "(foreach tile tiles (set_tile tile $value))"
                        "(if (eq \"1\" $value)"
                        "    (setq tmp 1023)"
                        "    (setq tmp 0)"
                        ")"
                    )
                )                        
                (if (= 1 (start_dialog)) (setq code tmp))
            )
        )
        (if (< 0 han) (setq han (unload_dialog han)))
        (if (and dcl  (setq dcl (findfile dcl))) (vl-file-delete dcl))
        code
    )

    (defun _GetDocumentObject ( dbxdoc acdocs xref / path xpath )
        (setq xpath (cdr (assoc 1 (entget (tblobjname "BLOCK" xref)))))
        (cond
            (   (null
                    (or
                        (setq path (findfile xpath))
                        (setq path (findfile (strcat (vl-filename-base xpath) ".dwg")))
                    )
                )
                (princ (strcat "\nSource Drawing for " xref " not Found."))
                nil
            )
            (   (cdr (assoc path acdocs))
            )
            (   (null
                    (vl-catch-all-error-p
                        (vl-catch-all-apply 'vla-open (list dbxdoc path))
                    )
                )
                dbxdoc
            )
            (   (princ (strcat "\nUnable to Open " xref " Source Drawing."))
                nil
            )
        )
    )

    (if (null (setq props (getenv "LMac\\RXLProps")))
        (setq props (+ 1 2 4 8 512))
        (setq props (atoi props))
    )

    (while (setq def (tblnext "BLOCK" (null def)))
        (if (= 4 (logand 4 (cdr (assoc 70 def))))
            (setq lst (cons "," (cons (cdr (assoc 2 def)) lst)))
        )
    )

    (cond
        (   (null lst)
            (princ "\nNo XRefs found in Drawing.")
        )
        (   t
            (setq acapp  (vlax-get-acad-object)
                  acdoc  (vla-get-activedocument acapp)
                  dbxdoc (vla-GetInterfaceObject acapp
                             (if (< (setq acver (atoi (getvar "ACADVER"))) 16)
                                 "ObjectDBX.AxDbDocument"
                                 (strcat "ObjectDBX.AxDbDocument." (itoa acver))
                             )
                         )
                  acdocs (vlax-for doc (vla-get-documents acapp)
                             (setq acdocs (cons (cons (vla-get-fullname doc) doc) acdocs))
                         )
            )
            (while
                (progn
                    (setvar 'ERRNO 0)
                    (initget "Multiple All Settings")
                    (setq sel (entsel "\nSelect XRef to Reset [Multiple/All/Settings] <Exit>: "))
                    (cond
                        (   (= 7 (getvar 'ERRNO))
                            (princ "\nMissed, try again.")
                        )
                        (   (null sel)
                            nil
                        )
                        (   (eq "Multiple" sel)
                            (setvar 'NOMUTT 1)
                            (princ "\nSelect XRefs to Reset <Exit>: ")
                            (setq sel
                                (vl-catch-all-apply 'ssget
                                    (list
                                        (list '(0 . "INSERT") (cons 2 (apply 'strcat (cdr lst))))
                                    )
                                )
                            )
                            (setvar 'NOMUTT 0)
                            (if (and sel (not (vl-catch-all-error-p sel)))
                                (repeat (setq inc (sslength sel))
                                    (LM:ResetXRefLayers
                                        (setq xrf (cdr (assoc 2 (entget (ssname sel (setq inc (1- inc)))))))
                                        (_GetDocumentObject dbxdoc acdocs xrf)
                                        acdoc
                                        props
                                    )
                                )
                            )
                            nil
                        )
                        (   (eq "All" sel)
                            (while (setq def (tblnext "BLOCK" (null def)))
                                (if (= 4 (logand 4 (cdr (assoc 70 def))))
                                    (LM:ResetXRefLayers
                                        (setq xrf (cdr (assoc 2 def)))
                                        (_GetDocumentObject dbxdoc acdocs xrf)
                                        acdoc
                                        props
                                    )
                                )
                            )
                            nil
                        )
                        (   (eq "Settings" sel)
                            (setq props (_Settings props))
                        )
                        (   (vl-consp sel)
                            (if (eq "INSERT" (cdr (assoc 0 (setq sel (entget (car sel))))))
                                (if (= 4 (logand 4 (cdr (assoc 70 (tblsearch "BLOCK" (cdr (assoc 2 sel)))))))
                                    (LM:ResetXRefLayers
                                        (setq xrf (cdr (assoc 2 sel)))
                                        (_GetDocumentObject dbxdoc acdocs xrf)
                                        acdoc
                                        props
                                    )
                                    (princ "\nSelected Block is not an XRef.")
                                )
                                (princ "\nInvalid Object Selected.")
                            )
                        )
                    )
                )
            )
            (vla-regen acdoc acallviewports)
            (vlax-release-object dbxdoc)
        )
    )
    (setenv "LMac\\RXLProps" (itoa props))
    (princ)
)

;;------------------------------------------------------------;;

(defun LM:ResetXRefLayers ( xref xrdoc acdoc props / _GetLayerProperties ass bit data name pos value xdef )

    (defun _GetLayerProperties ( doc props / bit lst data )
        (vlax-for layer (vla-get-layers doc)
            (setq bit 1
                  lst nil
            )
            (foreach prop
               '(
                    color
                    linetype
                    lineweight
                    plottable
                    plotstylename
                    viewportdefault
                    layeron
                    lock
                    freeze
                    description
                )
                (if
                    (and
                        (vlax-property-available-p layer prop)
                        (= bit (logand bit props))
                    )
                    (setq lst (cons (cons bit (vlax-get-property layer prop)) lst))
                )
                (setq bit (lsh bit 1))
            )
            (setq data
                (cons
                    (cons
                        (strcase (vla-get-name layer))
                        (reverse lst)
                    )
                    data
                )
            )
        )
        data
    )
    
    (cond
        (   (null xrdoc)
            nil
        )
        (   (vl-catch-all-error-p
                (setq xdef
                    (vl-catch-all-apply 'vla-item (list (vla-get-blocks acdoc) xref))
                )
            )
            (princ "\nXRef not present in Drawing.")
            nil
        )
        (   (setq data (_GetLayerProperties xrdoc props))
            (vla-reload xdef)

            (vla-startundomark acdoc)
            (vlax-for layer (vla-get-layers acdoc)
                (setq bit 1)
                (if
                    (and
                        (setq pos (vl-string-position 124 (setq name (strcase (vla-get-name layer)))))
                        (eq (strcase xref) (substr name 1 pos))
                        (setq ass (cdr (assoc (substr name (+ 2 pos)) data)))
                    )
                    (foreach prop
                       '(
                            color
                            linetype
                            lineweight
                            plottable
                            plotstylename
                            viewportdefault
                            layeron
                            lock
                            freeze
                            description
                        )
                        (if
                            (and
                                (vlax-property-available-p layer prop t)
                                (= bit (logand bit props))
                                (setq value (cdr (assoc bit ass)))
                            )
                            (if (and (= 2 bit) (not (eq "CONTINUOUS" (strcase value))))
                                (vl-catch-all-apply 'vlax-put-property (list layer prop (strcat xref "|" value)))
                                (vl-catch-all-apply 'vlax-put-property (list layer prop value))
                            )
                        )
                        (setq bit (lsh bit 1))
                    )
                )
            )
            (vla-endundomark acdoc)
            t
        )
    )
)

;;------------------------------------------------------------;;

(vl-load-com)
(princ)
(princ "\n:: ResetXRefLayers.lsp | Version 1.1 | © Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Type \"ResetXRefLayers\" or \"RXL\" to Invoke ::")
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;