;;----------------------=={ Align Text to Curve }==---------------------;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  Program Overview                                                    ;;
;;  -----------------------------------                                 ;;
;;  This program enables the user to dynamically align a new or         ;;
;;  existing Text or MText object to a selected curve, with intuitive   ;;
;;  placement controls available.                                       ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  User Input                                                          ;;
;;  -----------------------------------                                 ;;
;;  Upon issuing the command syntax 'atc' at the AutoCAD command-line,  ;;
;;  the user is prompted to select a Text or MText object to align.     ;;
;;  At this prompt, the user also has the option to create a new Text   ;;
;;  or MText object, or configure the program settings.                 ;;
;;                                                                      ;;
;;  If the 'New' option is chosen, the user is prompted to enter the    ;;
;;  content for the new text object, or may press 'Enter' to return to  ;;
;;  the previous prompt.                                                ;;
;;                                                                      ;;
;;  If the 'Settings' option is chosen, the user is presented with a    ;;
;;  dialog interface through which several program parameters may be    ;;
;;  configured - these settings are detailed in the section below.      ;;
;;                                                                      ;;
;;  The user is then prompted to select a curve to which the text will  ;;
;;  be dynamically aligned. The program is compatible for use with      ;;
;;  Lines, LWPolylines, 2D (Heavy) Polylines, 3D Polylines, Arcs,       ;;
;;  Circles, Ellipses, Elliptical Arcs & Splines; furthermore, these    ;;
;;  objects may be primary or nested (to any depth) within a block or   ;;
;;  xref.                                                               ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  Dynamic Text Alignment                                              ;;
;;  -----------------------------------                                 ;;
;;  Following valid selection of a curve, the new or existing Text or   ;;
;;  MText object is dynamically aligned to the curve based on the       ;;
;;  position of the AutoCAD cursor.                                     ;;
;;                                                                      ;;
;;  During text alignment, several controls are available at the        ;;
;;  command-line to refine the text position & other properties; these  ;;
;;  controls are individually detailed below:                           ;;
;;                                                                      ;;
;;  [ Enter ]  -  (or Esc/Space/Right-Click) Exit program (Cancel)      ;;
;;  [ Click ]  -  Place text                                            ;;
;;  [  +/-  ]  -  Incrementally increase/decrease text offset           ;;
;;  [   O   ]  -  Specify exact text offset                             ;;
;;  [  </>  ]  -  Rotate text by 45 degrees                             ;;
;;  [   R   ]  -  Specify exact text rotation (relative to curve)       ;;
;;  [   Y   ]  -  Toggle text readability                               ;;
;;  [   B   ]  -  Toggle MText Background Mask                          ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  Program Settings                                                    ;;
;;  -----------------------------------                                 ;;
;;  Upon selecting the 'Settings' option when prompted, the user is     ;;
;;  presented with a dialog interface offering the following options:   ;;
;;                                                                      ;;
;;  Object type for new text: this setting determines whether the       ;;
;;  program will create a single-line Text object or MText object when  ;;
;;  the user opts to create a new text.                                 ;;
;;                                                                      ;;
;;  Justification for new text: this setting controls the justification ;;
;;  of any new text object created by the program.                      ;;
;;                                                                      ;;
;;  Text Offset Factor: this is the default offset factor of the text   ;;
;;  from the selected curve, as a multiple of the text height. This     ;;
;;  factor may also be zero if the text is to be positioned directly    ;;
;;  over the selected curve.                                            ;;
;;                                                                      ;;
;;  Text Rotation: this setting controls the default rotation of the    ;;
;;  text relative to the selected curve.                                ;;
;;                                                                      ;;
;;  Text Readability: this toggle determines whether the text should    ;;
;;  be rotated to preserve readability, i.e. the text will never appear ;;
;;  upside-down.                                                        ;;
;;                                                                      ;;
;;  Background Mask: this toggle controls whether a background mask is  ;;
;;  used when aligning MText objects.                                   ;;
;;                                                                      ;;
;;  Multiple Text Mode: if this setting is enabled, the program will    ;;
;;  continuously generate text objects to align with the selected curve ;;
;;  until the user exits the program.                                   ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  Notes                                                               ;;
;;  -----------------------------------                                 ;;
;;  The program is compatible with all full versions of AutoCAD         ;;
;;  supporting Visual LISP with ActiveX (COM) functionality (that is,   ;;
;;  AutoCAD 2000 onwards on a Windows OS).                              ;;
;;                                                                      ;;
;;  The program will perform successfully under all UCS & View          ;;
;;  settings and with Annotative Text Styles.                           ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    12-10-2013                                      ;;
;;                                                                      ;;
;;  First release - previously 'CurveAlignedTextV1-1.lsp'.              ;;
;;----------------------------------------------------------------------;;

(setq atc:version "1.2")

;;----------------------------------------------------------------------;;

(defun c:atc

    (
        /
        *error*
        ang
        bak
        cfg
        dcl def dis
        ent enx
        gr1 gr2
        hgt
        jus
        mat msg mtp
        nrm
        off
        pi2 prn prp pt1 pt2
        red rot
        sav sel str sym
        tmp txt typ
        uxa
    )

    (defun *error* ( msg )
        (if
            (and
                (= 'list (type def))
                (= 'str  (type cfg))
                (findfile cfg)
            )
            (atc:writeconfig cfg (mapcar 'eval (mapcar 'car def)))
        )
        (if
            (and
                (= 'vla-object (type txt))
                (not (vlax-erased-p txt))
                (vlax-write-enabled-p txt)
            )
            (if (= 'list (type prp))
                (foreach x prp
                    (if (vlax-property-available-p txt (car x) t)
                        (vl-catch-all-apply 'vlax-put-property (cons txt x))
                    )
                )
                (vl-catch-all-apply 'vla-delete (list txt))
            )
        )
        (if
            (and
                (= 'list  (type mat))
                (= 'ename (type ent))
                (entget ent)
            )
            (entdel ent)
        )
        (atc:endundo (atc:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (atc:startundo (atc:acdoc))
    (cond
        (   (or (atc:layerlocked (getvar 'clayer))
                (atc:layerlocked "0")
            )
            (princ "\nCurrent layer or layer \"0\" locked.")
        )
        (   (null (vl-file-directory-p (setq sav (atc:savepath))))
            (princ "\nSave path invalid.")
        )
        (   (progn
                (setq def
                   '(
                        (typ . "txt")
                        (jus . "Middle-Center")
                        (off . 1.0)
                        (rot . 0.0)
                        (red . t)
                        (bak . nil)
                        (mtp . nil)
                    )
                )
                (setq cfg (strcat sav "\\LMAC_ATC_V" (vl-string-translate "." "-" atc:version) ".cfg")
                      dcl (strcat sav "\\LMAC_ATC_V" (vl-string-translate "." "-" atc:version) ".dcl")
                )
                (if (not (findfile cfg))
                    (atc:writeconfig cfg (mapcar 'cdr def))
                )
                (atc:readconfig cfg (setq sym (mapcar 'car def)))

                (while
                    (progn
                        (setvar 'errno 0)
                        (initget "New Settings Exit")
                        (setq sel (entsel "\nSelect text to align [New/Settings] <Exit>: "))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\nMissed, try again.")
                            )
                            (   (= 'list (type sel))
                                (setq ent (car sel)
                                      enx (entget ent)
                                )
                                (cond
                                    (   (not (wcmatch (cdr (assoc 0 enx)) "TEXT,MTEXT"))
                                        (princ "\nObject must be either Text or MText.")
                                    )
                                    (   (atc:layerlocked (cdr (assoc 8 enx)))
                                        (princ "\nObject is on a locked layer.")
                                    )
                                    (   t
                                        (setq txt (vlax-ename->vla-object ent)
                                              prp (atc:getproperties txt)
                                        )
                                        nil
                                    )
                                )
                            )
                            (   (= "Exit" sel)
                                nil
                            )
                            (   (= "Settings" sel)
                                (mapcar 'set sym (atc:settings dcl (mapcar 'eval sym)))
                            )
                            (   (= "New" sel)
                                (= "" (vl-string-trim " \t\n" (setq str (getstring t "\nSpecify text <Select>: "))))
                            )
                        )
                    )
                )
                (not
                    (or (= 'vla-object (type txt))
                        (and (= 'str (type str)) (/= "" (vl-string-trim " \t\n" str)))
                    )
                )
            )
            (atc:writeconfig cfg (mapcar 'eval sym))
        )
        (   (progn
                (while
                    (progn
                        (setvar 'errno 0)
                        (setq sel (nentselp "\nSelect curve to align text <Exit>: "))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\nMissed, try again.")
                            )
                            (   (= 'ename (type (car sel)))
                                (if
                                    (not
                                        (or (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                                            (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-curve-getendparam (list (car sel)))))
                                        )
                                    )
                                    (princ "\nInvalid object selected.")
                                )
                            )
                        )
                    )
                )
                (null sel)
            )
        )
        (   (not
                (or
                    (and
                        (setq mat (caddr sel))
                        (setq ent (atc:copynested (car sel) mat))
                    )
                    (and
                        (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                        (setq ent (cdr (assoc 330 (entget (car sel)))))
                    )
                    (setq ent (car sel))
                )
            )
            (princ "\nUnable to recreate nested entity.")
        )
        (   t
            (if (null txt)
                (if (= "txt" typ)
                    (progn
                        (setq txt
                            (vla-addtext
                                (vlax-get-property (atc:acdoc)
                                    (if (= 1 (getvar 'cvport))
                                        'paperspace
                                        'modelspace
                                    )
                                )
                                str
                                (vlax-3D-point (trans (cadr sel) 1 0))
                                (atc:styleheight (getvar 'textstyle))
                            )
                        )
                        (vla-put-alignment txt
                            (eval
                                (cadr
                                    (assoc jus
                                       '(
                                            ("Left"          acalignmentleft)
                                            ("Center"        acalignmentcenter)
                                            ("Right"         acalignmentright)
                                            ("Middle"        acalignmentmiddle)
                                            ("Top-Left"      acalignmenttopleft)
                                            ("Top-Center"    acalignmenttopcenter)
                                            ("Top-Right"     acalignmenttopright)
                                            ("Middle-Left"   acalignmentmiddleleft)
                                            ("Middle-Center" acalignmentmiddlecenter)
                                            ("Middle-Right"  acalignmentmiddleright)
                                            ("Bottom-Left"   acalignmentbottomleft)
                                            ("Bottom-Center" acalignmentbottomcenter)
                                            ("Bottom-Right"  acalignmentbottomright)
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (progn
                        (setq txt
                            (vla-addmtext
                                (vlax-get-property (atc:acdoc)
                                    (if (= 1 (getvar 'cvport))
                                        'paperspace
                                        'modelspace
                                    )
                                )
                                (vlax-3D-point (trans (cadr sel) 1 0))
                                (   (lambda ( box ) (- (caadr box) (caar box)))
                                    (textbox
                                        (list
                                            (cons 01 (strcat str "."))
                                            (cons 40 (atc:styleheight (getvar 'textstyle)))
                                            (cons 07 (getvar 'textstyle))
                                        )
                                    )
                                )
                                str
                            )
                        )
                        (vla-put-attachmentpoint txt
                            (eval
                                (cadr
                                    (assoc jus
                                       '(
                                            ("Top-Left"      acattachmentpointtopleft)
                                            ("Top-Center"    acattachmentpointtopcenter)
                                            ("Top-Right"     acattachmentpointtopright)
                                            ("Middle-Left"   acattachmentpointmiddleleft)
                                            ("Middle-Center" acattachmentpointmiddlecenter)
                                            ("Middle-Right"  acattachmentpointmiddleright)
                                            ("Bottom-Left"   acattachmentpointbottomleft)
                                            ("Bottom-Center" acattachmentpointbottomcenter)
                                            ("Bottom-Right"  acattachmentpointbottomright)
                                        )
                                    )
                                )
                            )
                        )
                        (vla-put-height txt (atc:styleheight (getvar 'textstyle)))
                        (if bak (vla-put-backgroundfill txt :vlax-true))
                    )
                )
            )
            (if
                (and
                    (= "AcDbText" (vla-get-objectname txt))
                    (/= acalignmentleft (vla-get-alignment txt))
                )
                (setq prn 'textalignmentpoint)
                (setq prn 'insertionpoint)
            )
            (setq hgt (vla-get-height txt)
                  pi2 (/ pi -2.0)
                  nrm (trans '(0.0 0.0 1.0) 1 0 t)
                  uxa (if (= "AcDbText" (vla-get-objectname txt)) (angle '(0.0 0.0 0.0) (trans (getvar 'ucsxdir) 0 nrm t)) 0.0)
                  msg (strcat "\n[+/-] for [O]ffset | [</>] for [R]otation | Readabilit[y] |"
                          (if (= "AcDbMText" (vla-get-objectname txt))
                              " [B]ackground Mask | <[E]xit>: "
                              " <[E]xit>: "
                          )
                      )
            )
            (princ msg)
            (while
                (progn
                    (setq gr1 (grread t 15 0)
                          gr2 (cadr gr1)
                          gr1 (car  gr1)
                    )
                    (cond
                        (   (or (= 5 gr1) (= 3 gr1))
                            (setq pt2 (trans gr2 1 0)
                                  pt1 (vlax-curve-getclosestpointto ent pt2)
                            )
                            (if (not (equal pt1 pt2 1e-8))
                                (progn
                                    (setq dis (/ (* hgt off) (distance pt1 pt2))
                                          ang (+ (angle (trans pt1 0 1) gr2) uxa rot pi2)
                                    )
                                    (vlax-put-property txt prn (vlax-3D-point (mapcar '(lambda ( a b ) (+ a (* (- b a) dis))) pt1 pt2)))
                                    (vla-put-rotation  txt (if red (atc:readable ang) ang))
                                )
                            )
                            (cond
                                (   (= 5 gr1))
                                (   mtp
                                    (setq txt (vla-copy txt)
                                          prp nil
                                    )
                                    t
                                )
                            )
                        )
                        (   (= 2 gr1)
                            (cond
                                (   (member gr2 '(043 061))
                                    (setq off (+ off 0.1))
                                )
                                (   (member gr2 '(045 095))
                                    (setq off (- off 0.1))
                                )
                                (   (member gr2 '(044 060))
                                    (setq rot (+ rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(046 062))
                                    (setq rot (- rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(013 032 069 101))
                                    (*error* nil)
                                    nil
                                )
                                (   (member gr2 '(089 121))
                                    (if (setq red (not red))
                                        (princ "\n<Text Readability Enabled>")
                                        (princ "\n<Text Readability Disabled>")
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(066 098))
                                    (if (= "AcDbMText" (vla-get-objectname txt))
                                        (progn
                                            (vlax-put txt 'backgroundfill (~ (vlax-get txt 'backgroundfill)))
                                            (if (setq bak (= -1 (vlax-get txt 'backgroundfill)))
                                                (princ "\n<Background Mask On>")
                                                (princ "\n<Background Mask Off>")
                                            )
                                        )
                                        (princ "\nBackground mask only available with MText.")
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(082 114))
                                    (if (setq tmp (getangle (strcat "\nSpecify Rotation <" (angtos rot) ">: ")))
                                        (setq rot tmp)
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(079 111))
                                    (if (setq tmp (getdist (strcat "\nSpecify Offset <" (rtos (* hgt off)) ">: ")))
                                        (setq off (/ tmp hgt))
                                    )
                                    (princ msg)
                                )
                                (   t   )
                            )
                        )
                        (   (member gr1 '(11 25))
                            (*error* nil)
                            nil
                        )
                        (   t   )
                    )
                )
            )
            (if mat (entdel ent))
            (atc:writeconfig cfg (mapcar 'eval sym))
        )
    )
    (atc:endundo (atc:acdoc))
    (princ)
)

;;----------------------------------------------------------------------;;

(defun atc:readable ( a )
    (   (lambda ( a )
            (if (and (< (* pi 0.5) a) (<= a (* pi 1.5)))
                (atc:readable (+ a pi))
                a
            )
        )
        (rem (+ a pi pi) (+ pi pi))
    )
)

;;----------------------------------------------------------------------;;

(defun atc:styleheight ( sty / tmp )
    (if (zerop (setq tmp (cdr (assoc 40 (tblsearch "style" sty)))))
        (setq tmp (getvar 'textsize))
    )
    (if (atc:annotative-p sty)
        (/ tmp (cond ((getvar 'cannoscalevalue)) (1.0)))
        tmp
    )
)

;;----------------------------------------------------------------------;;

(defun atc:annotative-p ( sty )
    (and
        (setq sty (tblobjname "style" sty))
        (setq sty (cadr (assoc -3 (entget sty '("AcadAnnotative")))))
        (= 1 (cdr (assoc 1070 (reverse sty))))
    )
)

;;----------------------------------------------------------------------;;

(defun atc:copynested ( ent mat / enx tmp )
    (if (= 1 (cdr (assoc 66 (setq enx (entget ent)))))
        (progn
            (atc:entmakex enx)
            (setq ent (entnext ent)
                  enx (entget  ent)
            )
            (while (/= "SEQEND" (cdr (assoc 0 enx)))
                (atc:entmakex enx)
                (setq ent (entnext ent)
                      enx (entget  ent)
                )
            )
            (setq tmp (cdr (assoc 330 (entget (atc:entmakex enx)))))
        )
        (setq tmp (atc:entmakex enx))
    )
    (if tmp (vla-transformby (vlax-ename->vla-object tmp) (vlax-tmatrix mat)))
    tmp
)

;;----------------------------------------------------------------------;;

(defun atc:entmakex ( enx )
    (entmakex
        (append
            (vl-remove-if
                (function
                    (lambda ( x )
                        (or (member (car x) '(005 006 008 039 048 062 102 370))
                            (= 'ename (type (cdr x)))
                        )
                    )
                )
                enx
            )
           '(
                (006 . "CONTINUOUS")
                (008 . "0")
                (039 . 0.0)
                (048 . 1.0)
                (062 . 7)
                (370 . 0)
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun atc:getproperties ( obj )
    (vl-remove nil
        (mapcar
            (function
                (lambda ( prp )
                    (if (vlax-property-available-p obj prp t)
                        (list prp (vlax-get-property obj prp))
                    )
                )
            )
           '(
                insertionpoint
                textalignmentpoint
                backgroundfill
                rotation
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun atc:settings ( dcl lst / *error* alg bak dch jus mtp off off:str red rot rot:str typ typ:fun )

    (defun *error* ( msg )
        (if (< 0 dch)
            (unload_dialog dch)
        )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (cond
        (   (not (atc:writedcl dcl))
            (princ "\nDCL file could not be written.")
        )
        (   (<= (setq dch (load_dialog dcl)) 0)
            (princ "\nDCL file could not be loaded.")
        )
        (   (not (new_dialog "atc" dch))
            (princ "\nProgram dialog could not be loaded.")
        )
        (   t
            (mapcar 'set '(typ jus off rot red bak mtp) lst)
            
            (set_tile typ "1")
            (
                (setq typ:fun
                    (lambda ( typ )
                        (setq alg (atc:justlist typ))
                        (set_tile "jus"
                            (itoa
                                (cond
                                    (   (vl-position jus alg))
                                    (   (setq jus (car alg)) 0)
                                )
                            )
                        )
                        (if (= "mtx" typ)
                            (mode_tile "bak" 0)
                            (mode_tile "bak" 1)
                        )
                    )
                )
                typ
            )
            (action_tile "jus" "(setq jus (nth (atoi $value) alg))")
            (action_tile "txt" "(typ:fun (setq typ $key))")
            (action_tile "mtx" "(typ:fun (setq typ $key))")

            (set_tile    "off"  (setq off:str (rtos off)))
            (action_tile "off" "(setq off:str $value)")

            (set_tile    "rot"  (setq rot:str (angtos rot)))
            (action_tile "rot" "(setq rot:str $value)")

            (foreach key '("red" "bak" "mtp")
                (set_tile    key (if (eval (read key)) "1" "0"))
                (action_tile key (strcat "(setq " key " (= \"1\" $value))"))
            )
            (action_tile "accept"
                (vl-prin1-to-string
                   '(cond
                        (   (not (distof off:str))
                            (alert "\nOffset Factor must be numerical.")
                            (mode_tile "off" 2)
                        )
                        (   (not (angtof rot:str))
                            (alert "\nText Rotation must be numerical.")
                            (mode_tile "rot" 2)
                        )
                        (   (setq off (distof off:str)
                                  rot (angtof rot:str)
                            )
                            (done_dialog 1)
                        )
                    )
                )
            )
         
            (if (= 1 (start_dialog))
                (setq lst (list typ jus off rot red bak mtp))
            )
        )
    )
    (if (< 0 dch)
        (unload_dialog dch)
    )
    lst
)

;;----------------------------------------------------------------------;;

(defun atc:justlist ( typ / lst )
    (start_list "jus")
    (foreach itm
        (setq lst
            (append
                (if (= "txt" typ)
                   '(
                        "Left"
                        "Center"
                        "Right"
                        "Middle"
                    )
                )
               '(
                    "Top-Left"
                    "Top-Center"
                    "Top-Right"
                    "Middle-Left"
                    "Middle-Center"
                    "Middle-Right"
                    "Bottom-Left"
                    "Bottom-Center"
                    "Bottom-Right"
                )
            )
        )
        (add_list itm)
    )
    (end_list)
    lst
)

;;----------------------------------------------------------------------;;

(defun atc:layerlocked ( lay / def )
    (and
        (setq def (tblsearch "layer" lay))
        (= 4 (logand 4 (cdr (assoc 70 def))))
    )
)

;;----------------------------------------------------------------------;;

(defun atc:writedcl ( dcl / des )
    (cond
        (   (findfile dcl))
        (   (setq des (open dcl "w"))
            (foreach x
               '(
                    "edt : edit_box"
                    "{"
                    "    edit_width = 8;"
                    "    edit_limit = 10;"
                    "    alignment = left;"
                    "}"
                    "atc : dialog"
                    "{"
                    "    label = \"Settings\";"
                    "    spacer;"
                    "    : text"
                    "    {"
                    "        label = \"Object type for new text:\";"
                    "    }"
                    "    : radio_row"
                    "    {"
                    "        alignment = centered;"
                    "        fixed_width = true;"
                    "        : radio_button"
                    "        {"
                    "            key = \"txt\";"
                    "            label = \"Text\";"
                    "        }"
                    "        : radio_button"
                    "        {"
                    "            key = \"mtx\";"
                    "            label = \"MText\";"
                    "        }"
                    "    }"
                    "    spacer;"
                    "    : text"
                    "    {"
                    "        label = \"Justification for new text:\";"
                    "    }"
                    "    : popup_list"
                    "    {"
                    "        key = \"jus\";"
                    "    }"
                    "    spacer;"
                    "    : edt"
                    "    {"
                    "        key = \"off\";"
                    "        label = \"Offset Factor:\";"
                    "    }"
                    "    : edt"
                    "    {"
                    "        key = \"rot\";"
                    "        label = \"Text Rotation:\";"
                    "    }"
                    "    spacer;"
                    "    : toggle"
                    "    {"
                    "        key = \"red\";"
                    "        label = \"Retain Text Readability\";"
                    "    }"
                    "    : toggle"
                    "    {"
                    "        key = \"bak\";"
                    "        label = \"MText Background Mask\";"
                    "    }"
                    "    : toggle"
                    "    {"
                    "        key = \"mtp\";"
                    "        label = \"Multiple Text Mode\";"
                    "    }"
                    "    spacer;"
                    "    ok_cancel;"
                    "}"
                )
                (write-line x des)
            )
            (setq des (close des))
            (while (not (findfile dcl)))
            dcl
        )
    )
)

;;----------------------------------------------------------------------;;

(defun atc:writeconfig ( cfg lst / _tostring des )
 
    (defun _tostring ( x / dim )
        (cond
            (   (= 'int (type x))
                (itoa x)
            )
            (   (= 'real (type x))
                (setq dim (getvar 'dimzin))
                (setvar 'dimzin 0)
                (setq x (rtos x 2 8))
                (setvar 'dimzin dim)
                x
            )
            (   (vl-prin1-to-string x))
        )
    )
    
    (if (setq des (open cfg "w"))
        (progn
            (foreach x lst (write-line (_tostring x) des))
            (setq des (close des))
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun atc:readconfig ( cfg lst / des itm )
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

(defun atc:savepath ( / tmp )
    (if (setq tmp (getvar 'roamablerootprefix))
        (strcat (atc:fixdir tmp) "\\Support")
        (atc:fixdir (getvar 'tempprefix))
    )
)

;;----------------------------------------------------------------------;;

(defun atc:fixdir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)

;;----------------------------------------------------------------------;;

(defun atc:startundo ( doc )
    (atc:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun atc:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun atc:acdoc nil
    (eval (list 'defun 'atc:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (atc:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: AlignTextToCurve.lsp | Version "
        atc:version
        " | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"atc\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;