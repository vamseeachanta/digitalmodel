;;-----------------------------=={  Incremental Numbering Suite  }==-----------------------------;;
;;                                                                                               ;;
;;  --------------------------------------------                                                 ;;
;;  Program Overview                                                                             ;;
;;  --------------------------------------------                                                 ;;
;;                                                                                               ;;
;;  Incremental Numbering Suite enables the user to dynamically place incrementing alphabetical  ;;
;;  or numerical text in a drawing, with a range of positioning utilities and an optional        ;;
;;  prefix and/or suffix.                                                                        ;;
;;                                                                                               ;;
;;  The sequential text can be created using Text, MText or Attributed Blocks; furthermore, the  ;;
;;  style and formatting of these objects can be altered directly from the main dialog, with     ;;
;;  all settings remembered between drawing sessions.                                            ;;
;;                                                                                               ;;
;;  The user can modify the Text or Block layer, choose from a list of available Text Styles in  ;;
;;  the drawing, alter the Text or MText alignment, and also change the Text Height by either    ;;
;;  entering an arbitrary value, picking a value from the drawing, or using the height defined   ;;
;;  by the selected Text Style.                                                                  ;;
;;                                                                                               ;;
;;  If the Object type is set to use an attributed block, the user may choose the block that is  ;;
;;  to be used from a list of attribute blocks defined in the drawing, or select a block object  ;;
;;  directly from the drawing.                                                                   ;;
;;                                                                                               ;;
;;  The user also has control over which attribute will house the incrementing string, and the   ;;
;;  scale at which the block is inserted. This scale value can take an arbitrary value entered   ;;
;;  by the user or picked from the drawing, or can be dependent upon the current value of a      ;;
;;  selected System Variable, such as DIMSCALE.                                                  ;;
;;                                                                                               ;;
;;  If MText is selected, the user may also toggle the use of an MText Background Mask and       ;;
;;  control both the Background Mask Offset Factor and Colour.                                   ;;
;;                                                                                               ;;
;;  The user can enter optional Prefix, Middle and Suffix text, and has the option to increment  ;;
;;  any or all sections, with the ability to increment alphabetical text and use decimals and    ;;
;;  leading zeros. The user can also specify any numerical increment, positive or negative.      ;;
;;                                                                                               ;;
;;  If the user has chosen to use Text or MText objects in which to house the incremental text,  ;;
;;  there is an additional option to enclose such objects with a border. The border may be       ;;
;;  Circular, Rectangular, Slot or an n-sided Polygon; created on a layer chosen from the main   ;;
;;  dialog.                                                                                      ;;
;;                                                                                               ;;
;;  The size of the border may be controlled using an Offset Factor from the Text or MText       ;;
;;  object. The Offset Factor has behaviour identical to that of the Background Mask Offset      ;;
;;  Factor wherein the offset is dependent upon the text height: an offset factor of 1.0         ;;
;;  exactly fits the Text or MText object, a factor of 1.5 extends the border by 0.5 times the   ;;
;;  text height etc. Alternatively, the user may specify a fixed border size, with the option    ;;
;;  to pick either dimension from the drawing.                                                   ;;
;;                                                                                               ;;
;;  --------------------------------------------                                                 ;;
;;  Creating an Array                                                                            ;;
;;  --------------------------------------------                                                 ;;
;;                                                                                               ;;
;;  The program also offers the ability for the user to array the selected object, with the      ;;
;;  content automatically incremented sequentially for each object in the array.                 ;;
;;                                                                                               ;;
;;  Upon enabling the option to 'Create Array' (located towards the base of the centre panel of  ;;
;;  the dialog), the user can specify the number of items in the array and furthermore control   ;;
;;  the rotation of each object in the array, either relative to the direction of the array,     ;;
;;  or by entering a fixed value or picking such value from the drawing.                         ;;
;;                                                                                               ;;
;;  After submitting the desired dialog settings, the user is then prompted to specify a base    ;;
;;  point for the array. This is the point at which the first object in the array will be        ;;
;;  created and the point from which the array will emanate.                                     ;;
;;                                                                                               ;;
;;  Following the valid specification of an array base point, the user is given the option to    ;;
;;  supply either an array spacing vector, or the array endpoint.                                ;;
;;                                                                                               ;;
;;  The array spacing vector describes the vector between each successive object in the array.   ;;
;;  Conversely, the array endpoint specifies the point at which the last object in the array     ;;
;;  will be inserted; with the remaining objects inserted equispaced between the two points.     ;;
;;                                                                                               ;;
;;  Concerning object rotation, the user has three options to determine how objects are          ;;
;;  oriented relative to the array. If the user decides to align objects in the direction of     ;;
;;  the array, the rotation of each object will reflect the direction of the array vector.       ;;
;;                                                                                               ;;
;;  Similarly, selecting to rotate objects perpendicular to the array will result in objects     ;;
;;  oriented at right-angles to the array direction, whilst retaining readability.               ;;
;;                                                                                               ;;
;;  Finally, the user may also specify an arbitrary fixed rotation, or pick such a value         ;;
;;  directly from the drawing.                                                                   ;;
;;                                                                                               ;;
;;  --------------------------------------------                                                 ;;
;;  Dynamic Positioning Mode                                                                     ;;
;;  --------------------------------------------                                                 ;;
;;                                                                                               ;;
;;  Dynamic Mode is activated by enabling the option: 'Text Follows Cursor'.                     ;;
;;                                                                                               ;;
;;  This mode will display a real-time preview of the Text, MText or Attributed Block with any   ;;
;;  border or background mask that may be specified.                                             ;;
;;                                                                                               ;;
;;  Note: As a result of the method used to display the real-time preview, Dynamic Mode          ;;
;;  restricts the use of standard AutoCAD functionality such as Object Snap, Orthomode,          ;;
;;  Tracking etc. To enable such functionality, uncheck the 'Text Follows Cursor' option at      ;;
;;  the top-left corner of the dialog.                                                           ;;
;;                                                                                               ;;
;;  Various positioning controls displayed at the command line:                                  ;;
;;                                                                                               ;;
;;  --------------------------------------------                                                 ;;
;;  Dynamic Mode Positioning Controls:                                                           ;;
;;  --------------------------------------------                                                 ;;
;;                                                                                               ;;
;;  [ Enter ]  -  (or Space/Right-Click) Exit Program [Cancel]                                   ;;
;;  [ Click ]  -  Place Object                                                                   ;;
;;  [   <   ]  -  Rotate Object Counter Clockwise                                                ;;
;;  [   >   ]  -  Rotate Object Clockwise                                                        ;;
;;  [   O   ]  -  Specify Object Rotation                                                        ;;
;;  [  Tab  ]  -  Rotate Object by 90º                                                           ;;
;;  [   M   ]  -  Mirror Object Rotation                                                         ;;
;;  [   C   ]  -  Align Object to Curve                                                          ;;
;;  [   R   ]  -  Replace Existing Text/Attribute String                                         ;;
;;  [   T   ]  -  Toggle Counter Increment                                                       ;;
;;  [   I   ]  -  Increment String                                                               ;;
;;  [   B   ]  -  Rotate Polygonal Border                                                        ;;
;;  [   A   ]  -  Toggle MText Background Mask                                                   ;;
;;                                                                                               ;;
;;  --------------------------------------------                                                 ;;
;;  Align Object to Curve                                                                        ;;
;;  --------------------------------------------                                                 ;;
;;                                                                                               ;;
;;  The user can choose to align the object to a selected curve object (Line, LWPolyline,        ;;
;;  Polyline, XLine, Spline, Arc, Circle, Ellipse etc.) by pressing 'C' or 'c' during            ;;
;;  placement.                                                                                   ;;
;;                                                                                               ;;
;;  The user is then prompted to select a curve to which the text will be aligned. The text      ;;
;;  will follow the selected curve with various positioning controls available at the            ;;
;;  command-line:                                                                                ;;
;;                                                                                               ;;
;;  --------------------------------------------                                                 ;;
;;  Curve Alignment Controls                                                                     ;;
;;  --------------------------------------------                                                 ;;
;;                                                                                               ;;
;;  [ Enter ]  -  (or Space/Right-Click) Exit Curve Alignment [Cancel]                           ;;
;;  [ Click ]  -  Place Object                                                                   ;;
;;  [  +/-  ]  -  Increase/Decrease Object Offset                                                ;;
;;  [   O   ]  -  Specify Object Offset                                                          ;;
;;  [   P   ]  -  Toggle Object Perpendicularity                                                 ;;
;;  [   B   ]  -  Rotate Polygonal Border                                                        ;;
;;  [   A   ]  -  Toggle MText Background Mask                                                   ;;
;;                                                                                               ;;
;;  --------------------------------------------                                                 ;;
;;  Replace Existing Text or Attribute String                                                    ;;
;;  --------------------------------------------                                                 ;;
;;                                                                                               ;;
;;  Upon pressing 'R' or 'r' during placement, the user is continuously prompted to select       ;;
;;  either Text, MText or Attribute, which, upon selection will be modified to contain the       ;;
;;  sequential text string.                                                                      ;;
;;                                                                                               ;;
;;  The user can exit this mode and return to standard text placement by pressing Enter,         ;;
;;  Space, or by Right-clicking the mouse at the prompt.                                         ;;
;;                                                                                               ;;
;;  --------------------------------------------                                                 ;;
;;  Standard Positioning Mode                                                                    ;;
;;  --------------------------------------------                                                 ;;
;;                                                                                               ;;
;;  This mode is available when the 'Text Follows Cursor' is disabled (unticked).                ;;
;;                                                                                               ;;
;;  When using this mode, there is no longer a real-time preview of the text at the cursor,      ;;
;;  however all standard AutoCAD functionality is available, (such as Object Snap, Tracking,     ;;
;;  Orthomode, etc.).                                                                            ;;
;;                                                                                               ;;
;;  The majority of placement controls are still available at the command-line:                  ;;
;;                                                                                               ;;
;;  --------------------------------------------                                                 ;;
;;  Standard Mode Positioning Controls:                                                          ;;
;;  --------------------------------------------                                                 ;;
;;                                                                                               ;;
;;  [ Enter ]  -  (or Space/Right-Click) Exit Program [Cancel]                                   ;;
;;  [ Click ]  -  Place Object                                                                   ;;
;;  [   O   ]  -  Specify Object Rotation                                                        ;;
;;  [  RO   ]  -  Rotate Object by 90º                                                           ;;
;;  [   M   ]  -  Mirror Object Rotation                                                         ;;
;;  [   C   ]  -  Align Object to Curve                                                          ;;
;;  [   R   ]  -  Replace Existing Text/Attribute String                                         ;;
;;  [   T   ]  -  Toggle Counter Increment                                                       ;;
;;  [   I   ]  -  Increment String                                                               ;;
;;  [   B   ]  -  Rotate Polygonal Border                                                        ;;
;;  [   A   ]  -  Toggle MText Background Mask                                                   ;;
;;                                                                                               ;;
;;-----------------------------------------------------------------------------------------------;;
;;                                                                                               ;;
;;  Function Syntax:  NumInc                                                                     ;;
;;                                                                                               ;;
;;-----------------------------------------------------------------------------------------------;;
;;                                                                                               ;;
;;  Author: Lee Mac, Copyright © 2009 - www.lee-mac.com                                          ;;
;;                                                                                               ;;
;;-----------------------------------------------------------------------------------------------;;
;;                                                                                               ;;
;;  TERMS AND CONDITIONS OF USE                                                                  ;;
;;                                                                                               ;;
;;  This license and disclaimer statement constitutes a legal agreement between you (either as   ;;
;;  an individual or a single entity) and Lee Mac (the "Author"), for this software product      ;;
;;  (the "Software").                                                                            ;;
;;                                                                                               ;;
;;  By downloading, installing, copying, or otherwise using the software, you agree to be bound  ;;
;;  by all of the following terms and conditions of this license and disclaimer agreement.       ;;
;;                                                                                               ;;
;;  If you do not agree with all the terms and conditions of this agreement, you must            ;;
;;  immediately cease use of the Software and destroy all copies of the Software and all of      ;;
;;  its component or constituent parts in your possession or under your control.                 ;;
;;                                                                                               ;;
;;  The Software is freeware. You may use it royalty free for private use.                       ;;
;;                                                                                               ;;
;;  You may redistribute the Software providing you have written consent from the Author, and    ;;
;;  that no modifications are made to the original content.                                      ;;
;;                                                                                               ;;
;;  You may not charge any fees for the redistribution or use of this Software.                  ;;
;;                                                                                               ;;
;;  The Software is provided "as is", and with all faults. All warranties, expressed or          ;;
;;  implied, including, but not limited to implied warranties of fitness for a particular use    ;;
;;  or purpose are hereby disclaimed. There is no guarantee that the operation of this Software  ;;
;;  will be uninterrupted or error free.                                                         ;;
;;                                                                                               ;;
;;  You acknowledge and agree that your use of the Software is at your own risk.                 ;;
;;                                                                                               ;;
;;  The Software is a copyrighted work and is protected by copyright law and international       ;;
;;  copyright treaty.                                                                            ;;
;;                                                                                               ;;
;;-----------------------------------------------------------------------------------------------;;
;;                                                                                               ;;
;;  Version:                                                                                     ;;
;;                                                                                               ;;
;;  1.0:  12/04/2009  -  First Release.                                                          ;;
;;-----------------------------------------------------------------------------------------------;;
;;  1.1:  14/04/2009  -  Added Prefix/Suffix Option.                                             ;;
;;-----------------------------------------------------------------------------------------------;;
;;  1.2:  15/04/2009  -  Added Dialog.                                                           ;;
;;-----------------------------------------------------------------------------------------------;;
;;  1.3:  15/04/2009  -  Added Option to Replace Existing text/attribute string.                 ;;
;;-----------------------------------------------------------------------------------------------;;
;;  1.4:  18/04/2009  -  Made program compatible with leading zeros.                             ;;
;;-----------------------------------------------------------------------------------------------;;
;;  1.5:  16/06/2009  -  Upgraded program code.                                                  ;;
;;-----------------------------------------------------------------------------------------------;;
;;  1.6:  27/06/2009  -  Added Counter Toggle.                                                   ;;
;;                    -  General Bug Fixes.                                                      ;;
;;-----------------------------------------------------------------------------------------------;;
;;  1.7:  18/02/2010  -  General Program Upgrade.                                                ;;
;;-----------------------------------------------------------------------------------------------;;
;;  1.8:  19/02/2010  -  Change Rotation Controls.                                               ;;
;;-----------------------------------------------------------------------------------------------;;
;;  1.9:  22/02/2010  -  Added option to not use GrRead loop, and hence allow OSnap to function. ;;
;;                    -  Added ability to place text in Table Cells.                             ;;
;;-----------------------------------------------------------------------------------------------;;
;;  2.0:  24/02/2010  -  Fixed Text Height Bug.                                                  ;;
;;-----------------------------------------------------------------------------------------------;;
;;  2.1:  05/05/2010  -  Removed imitation OSnap.                                                ;;
;;                    -  Modified loop to allow multiple replacements when in standard mode.     ;;
;;                    -  Added ability to use Alphabetical Increment.                            ;;
;;-----------------------------------------------------------------------------------------------;;
;;  2.2:  06/05/2010  -  Fixed bug with text case when incrementing alphabetical strings.        ;;
;;                    -  Added ability to border text with either Circle, Rectangle or Slot;     ;;
;;                       and offset from text.                                                   ;;
;;-----------------------------------------------------------------------------------------------;;
;;  2.3:  07/05/2010  -  Changed the way that layer/style globals are stored to allow for        ;;
;;                       layer/style changes between uses.                                       ;;
;;                    -  Removed Xref layers/styles from list.                                   ;;
;;                    -  Fixed UCS bugs.                                                         ;;
;;-----------------------------------------------------------------------------------------------;;
;;  2.4:  10/05/2010  -  Added a 'By Style' option for text height selection.                    ;;
;;                    -  Added option to enclose text with n-sided Polygon.                      ;;
;;                    -  Added ability to rotate polygonal border.                               ;;
;;-----------------------------------------------------------------------------------------------;;
;;  2.5:  11/05/2010  -  Allowed for Zero height in TextStyle.                                   ;;
;;                    -  Added ability to fix border size.                                       ;;
;;                    -  Fixed Text Height variable bug.                                         ;;
;;                    -  Fixed Rotation of odd-sided polygons.                                   ;;
;;-----------------------------------------------------------------------------------------------;;
;;  2.6:  12/05/2010  -  Fixed Rotation of odd-sided polygons when text is not set to follow     ;;
;;                       cursor.                                                                 ;;
;;                    -  Fixed Slot Bulges.                                                      ;;
;;                    -  Added 'B' control to DCL About page.                                    ;;
;;-----------------------------------------------------------------------------------------------;;
;;  2.7:  22/05/2010  -  Fixed Border issue when in different view.                              ;;
;;-----------------------------------------------------------------------------------------------;;
;;  2.8:  24/05/2010  -  Changed DCL/CFG file save path to make program                          ;;
;;                       compatible with Bricscad.                                               ;;
;;-----------------------------------------------------------------------------------------------;;
;;  2.9:  29/05/2010  -  Upgraded code to determine DCL/CFG Filepath, to allow for AutoCAD       ;;
;;                       Versions pre 2004.                                                      ;;
;;-----------------------------------------------------------------------------------------------;;
;;  3.0:  10/10/2011  -  Program completely rewritten to improve program performance, update     ;;
;;                       code formatting and include the following new features:                 ;;
;;                    -  Ability to use Text, MText or an Attributed Block to house              ;;
;;                       incrementing string.                                                    ;;
;;                    -  Ability to change Text / MText Alignment.                               ;;
;;                    -  Ability to toggle the use of a Background Mask with MText.              ;;
;;                    -  Ability to specify both dimensions for the fixed size Slot /            ;;
;;                       Rectangular border.                                                     ;;
;;                    -  Vastly improved alphabetical text incrementing.                         ;;
;;                    -  Improved handling of negative numbers.                                  ;;
;;                    -  Dialog interface completely redesigned to make it more user-friendly    ;;
;;                       and intuitive to navigate.                                              ;;
;;                    -  Improved non-dynamic mode interface & functionality.                    ;;
;;                    -  Program works in all UCS/Views correctly.                               ;;
;;-----------------------------------------------------------------------------------------------;;
;;  3.1:  11/10/2011  -  Fixed bug concerning null text size variable when object is set to      ;;
;;                       attributed block.                                                       ;;
;;-----------------------------------------------------------------------------------------------;;
;;  3.2:  02/03/2012  -  Fixed bug wherein the object type default is a block but there are no   ;;
;;                       blocks in the drawing.                                                  ;;
;;                    -  Altered structure of numinc:popup function and introduced new           ;;
;;                       numinc:wsh function to avoid repetitive creation of the Windows         ;;
;;                       Script Host (WSH) Object.                                               ;;
;;                    -  Modified *error* function to release global WSH object and improve      ;;
;;                       error handling.                                                         ;;
;;                    -  Fixed bug pertaining to collection of attributed block data.            ;;
;;-----------------------------------------------------------------------------------------------;;
;;  3.3:  18/06/2012  -  Fixed bug causing program to crash if the Object type is set to 'Text'  ;;
;;                       and Text Alignment is set to 'Left', then the Object type is switched   ;;
;;                       to 'MText'.                                                             ;;
;;                    -  Redesigned 'About' dialog to include appropriate bitmap images and      ;;
;;                       stylized program title.                                                 ;;
;;                    -  Added the ability to increment multiple sections of the incrementing    ;;
;;                       string.                                                                 ;;
;;                    -  Fixed bug in which the program would check for incorrect border         ;;
;;                       parameters when the Object type is set to 'Block'.                      ;;
;;                    -  Added the ability to create an array of incrementing Text, MText or     ;;
;;                       Block objects, with the selected object aligned or perpendicular to     ;;
;;                       the array, or with rotation set to an arbitrary angle.                  ;;
;;                    -  Included set of controls within the Formatting section of the dialog    ;;
;;                       to give the user the ability to control MText Background Mask offset    ;;
;;                       factor and colour.                                                      ;;
;;                    -  Added ability to specify the block scale as an arbitrary value, a       ;;
;;                       value picked from the drawing, or based on the value of a selected      ;;
;;                       System Variable.                                                        ;;
;;                    -  Provided the user the option to pick the Text Height value from the     ;;
;;                       drawing.                                                                ;;
;;                    -  Redesigned all object selection and pick buttons to conform with        ;;
;;                       standard AutoCAD dialog interfaces.                                     ;;
;;                    -  Added the ability to increment the string during placement by pressing  ;;
;;                       the I/i key.                                                            ;;
;;                    -  Text & MText Border Offset is now a factor of text height where an      ;;
;;                       offset factor of 1.0 exactly fits the Text or MText object, a factor    ;;
;;                       of 1.5 extends the border by 0.5 times the text height etc. This        ;;
;;                       emulates the behaviour of the MText Background Mask, to enable the      ;;
;;                       user to easily create a rectangular border around the background mask   ;;
;;                       by specifying the same offset factor for both. This to me seemed        ;;
;;                       more intuitive.                                                         ;;
;;-----------------------------------------------------------------------------------------------;;
;;  3.4:  16/03/2014  -  Error handler function rewritten to fix a bug causing modified          ;;
;;                       system variables to not be reset.                                       ;;
;;                    -  Error messages rewritten to provide user with additional detailed       ;;
;;                       diagnostic information.                                                 ;;
;;                    -  Removed locked layers from layer drop-down menus, as these will cause   ;;
;;                       the program to error.                                                   ;;
;;                    -  Altered loading message to correctly display copyright symbol.          ;;
;;                    -  The program configuration is now saved if user presses 'Esc' during     ;;
;;                       object placement.                                                       ;;
;;                    -  numinc:writeconfig & numinc:tostring functions rewritten.               ;;
;;                    -  numinc:gettextbox rewritten and numinc:createtextborder function        ;;
;;                       updated to fix border elevation bug.                                    ;;
;;-----------------------------------------------------------------------------------------------;;
;;  3.5:  13/04/2014  -  Minor update to fix a reported intermittent bug causing the program to  ;;
;;                       crash when generating the first object after dimissing the dialog       ;;
;;                       with dynamic mode enabled.                                              ;;
;;-----------------------------------------------------------------------------------------------;;

(setq numincversion "3.5")

;;-----------------------------------------------------------------------------------------------;;

(defun c:numinc

    (
        /
        *error*
        _alignment
        _attachment
        _blocks
        _layers
        _scalevars
        _styles
        a
        acdoc
        acspc
        alignment
        arr-end
        arr-qty
        arr-qty#
        arr-rot
        arr-rot#
        arr-typ
        arr-typ-fun
        arr-use
        arr-use-fun
        att-nme
        attachment
        attrib
        attribs
        b
        blk-nme
        blk-scl
        blk-scl-fun
        blk-scl#
        block
        blocks
        bor
        bor-enc
        bor-enc-fun
        bor-lay
        bor-rot
        bor-shp
        bor-shp-fun
        bor-sid
        bor-sid#
        bor-typ
        bor-typ-fun
        cfgfname
        create-bor
        create-obj
        dclflag
        dclfname
        dclid
        deg
        dyn-flg
        elst
        ent
        file
        fix-ed1
        fix-ed1#
        fix-ed2
        fix-ed2#
        g1
        g2
        gr
        i
        inc-sec
        inc-str
        mid-str
        mode_color
        mode_image
        msg
        msk-col
        msk-off
        msk-off#
        msk-trn
        msk-trn-fun
        msk-use
        msk-use-fun
        mtx-bak
        nm
        obj
        obj-typ
        obj-typ-fun
        off-ed1
        off-ed1#
        p1
        p2
        pre-str
        prop
        pt
        r1
        savepath
        scalevars
        scl-pop
        scl-var
        ss
        string
        style
        suf-str
        symb
        symlist
        table
        tile
        tmp
        tog-cnt
        txt-aln
        txt-bst
        txt-lay
        txt-rot
        txt-sty
        txt-sty-fun
        txt-sze
        txt-sze#
        v1
        vallst
        varlst
        x
        xa
    )

    (defun *error* ( msg )
        (if
            (and
                (= 1 dclflag)
                (= 'str (type cfgfname))
                symlist
            )
            (numinc:writeconfig cfgfname (mapcar 'eval (mapcar 'car symlist)))
        )
        (if
            (and
                (= 'vla-object (type numinc:wshobject))
                (not (vlax-object-released-p numinc:wshobject))
            )
            (progn
                (vlax-release-object numinc:wshobject)
                (setq numinc:wshobject nil)
            )
        )
        (if (= "1" dyn-flg)
            (foreach obj (list obj bor)
                (if (and (= 'vla-object (type obj))
                         (not (vlax-erased-p obj))
                         (vlax-write-enabled-p obj)
                    )
                    (vla-delete obj)
                )
            )
        )
        (mapcar 'setvar varlst vallst)
        (if (= 'file (type file))
            (close file)
        )
        (if (< 0 dclid)
            (unload_dialog dclid)
        )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nNumInc Error: " msg))
        )
        (princ)
    )

    (setq varlst '(dimzin modemacro)
          vallst  (mapcar 'getvar varlst)
    )

    (cond
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer))))))
            (princ "\nCurrent layer locked.")
        )
        (   (not (vl-file-directory-p (setq savepath (numinc:getsavepath))))
            (numinc:popup "Save Path Invalid" 16
                (princ
                    (strcat
                        "The following path does not exist or is invalid:\n\n"
                        savepath
                    )
                )
            )
        )
        (   (progn
                (setq dclfname (strcat savepath "\\LMAC_NumInc_V" (vl-string-translate "." "-" numincversion) ".dcl")
                      cfgfname (strcat savepath "\\LMAC_NumInc_V" (vl-string-translate "." "-" numincversion) ".cfg")
                )
                (not (numinc:writedcl dclfname))
            )
            (numinc:popup "DCL File could not be Written" 16
                (princ
                    (strcat
                        "The DCL file required by this application was unable to be written to the following location:\n\n"
                        dclfname
                        "\n\nPlease ensure that you have write permissions for this directory."
                    )
                )
            )
        )
        (   (<= (setq dclID (load_dialog dclfname)) 0)
            (numinc:popup "DCL File could not be Loaded" 16
                (princ
                    (strcat
                        "The following DCL file could not be loaded:\n\n"
                        dclfname
                        "\n\nPlease check the integrity of this file."
                    )
                )
            )
        )
        (   t
            (setq symlist
                (list
                    (cons 'arr-use "0")
                    (cons 'arr-qty "5")
                    (cons 'arr-typ "arr-aln")
                    (cons 'arr-rot "0.0")
                    (cons 'arr-end nil)
                    (cons 'crv-per (/ pi 2.0))
                    (cons 'crv-off 0.0)
                    (cons 'txt-rot 0.0)
                    (cons 'bor-rot nil)
                    (cons 'tog-cnt t)
                    (cons 'dyn-flg "1")
                    (cons 'pre-str  "")
                    (cons 'mid-str "1")
                    (cons 'suf-str  "")
                    (cons 'inc-str "1")
                    (cons 'inc-sec 2)
                    (cons 'obj-typ "obj-txt")
                    (cons 'blk-nme "")
                    (cons 'att-nme "")
                    (cons 'blk-scl "1.0")
                    (cons 'scl-var "0")
                    (cons 'scl-pop "DIMSCALE")
                    (cons 'bor-enc "0")
                    (cons 'bor-shp "0")
                    (cons 'bor-sid "6")
                    (cons 'bor-lay (getvar 'clayer))
                    (cons 'bor-typ "bor-off")
                    (cons 'off-ed1 "1.0")
                    (cons 'fix-ed1 "1.0")
                    (cons 'fix-ed2 "1.0")
                    (cons 'txt-lay (getvar 'clayer))
                    (cons 'txt-sty (getvar 'textstyle))
                    (cons 'txt-aln "Middle-Center")
                    (cons 'txt-bst "1")
                    (cons 'txt-sze
                        (rtos
                            (if
                                (zerop
                                    (cdr
                                        (assoc 40
                                            (setq style
                                                (tblsearch "style" (getvar 'textstyle))
                                            )
                                        )
                                    )
                                )
                                (cdr (assoc 42 style))
                                (cdr (assoc 40 style))
                            )
                        )
                    )
                    (cons 'msk-use "0")
                    (cons 'msk-off "1.5")
                    (cons 'msk-trn "0")
                    (cons 'msk-col '((62 . 1)))
                )
            )
            (if (null (findfile cfgfname))
                (numinc:writeconfig cfgfname (mapcar 'cdr symlist))
            )
            (numinc:readconfig cfgfname (mapcar 'car symlist))
            (foreach x SymList
                (if (null (boundp (car x)))
                    (set (car x) (cdr x))
                )
            )

            (setq _layers (numinc:gettableitems "layer")
                  _styles (numinc:gettableitems "style")
                  _blocks (numinc:getblockdata)
            )

            (setq Alignment
                (list
                    (cons "Left"          acAlignmentLeft)
                    (cons "Center"        acAlignmentCenter)
                    (cons "Right"         acAlignmentRight)
                    (cons "Middle"        acAlignmentMiddle)
                    (cons "Top-Left"      acAlignmentTopLeft)
                    (cons "Top-Center"    acAlignmentTopCenter)
                    (cons "Top-Right"     acAlignmentTopRight)
                    (cons "Middle-Left"   acAlignmentMiddleLeft)
                    (cons "Middle-Center" acAlignmentMiddleCenter)
                    (cons "Middle-Right"  acAlignmentMiddleRight)
                    (cons "Bottom-Left"   acAlignmentBottomLeft)
                    (cons "Bottom-Center" acAlignmentBottomCenter)
                    (cons "Bottom-Right"  acAlignmentBottomRight)
                )
            )

            (setq Attachment
                (list
                    (cons "Top-Left"      acAttachmentPointTopLeft)
                    (cons "Top-Center"    acAttachmentPointTopCenter)
                    (cons "Top-Right"     acAttachmentPointTopRight)
                    (cons "Middle-Left"   acAttachmentPointMiddleLeft)
                    (cons "Middle-Center" acAttachmentPointMiddleCenter)
                    (cons "Middle-Right"  acAttachmentPointMiddleRight)
                    (cons "Bottom-Left"   acAttachmentPointBottomLeft)
                    (cons "Bottom-Center" acAttachmentPointBottomCenter)
                    (cons "Bottom-Right"  acAttachmentPointBottomRight)
                )
            )

            (setq _Alignment  (mapcar 'car Alignment))
            (setq _Attachment (mapcar 'car Attachment))

            (setq ScaleVars
                (vl-remove-if 'null
                    (mapcar
                        (function
                            (lambda ( var / value )
                                (if
                                    (and
                                        (setq value (getvar var))
                                        (< 0.0 value)
                                    )
                                    (if (= "CANNOSCALEVALUE" (strcase var))
                                        (cons var (rtos (/ 1.0 value)))
                                        (cons var (rtos value))
                                    )
                                )
                            )
                        )
                        (acad_strlsort
                           '(
                                "CANNOSCALEVALUE"
                                "CELTSCALE"
                                "DIMLFAC"
                                "DIMSCALE"
                                "DIMTFAC"
                                "DIMTXT"
                                "HPSCALE"
                                "LTSCALE"
                                "MLEADERSCALE"
                                "MSOLESCALE"
                                "TEXTSIZE"
                            )
                        )
                    )
                )
            )
            (setq _ScaleVars (mapcar 'car ScaleVars))

            (
                (lambda ( / i j x y )
                    (repeat (setq i 20)
                        (setq j 1)
                        (repeat 20
                            (setq x (cons j x)
                                  y (cons i y)
                                  j (1+ j)
                            )
                        )
                        (setq i (1- i))
                    )
                    (setq mode_image
                        (eval
                            (list 'lambda '( key mode )
                                (list 'cond
                                   '(   (= 1 mode)
                                        (start_image key)
                                        (fill_image 0 0 (dimx_tile key) (dimy_tile key) -15)
                                        (end_image)
                                        (mode_tile key mode)
                                    )
                                    (list 't
                                       '(start_image key)
                                       '(fill_image 0 0 (dimx_tile key) (dimy_tile key) -15)
                                        (list 'mapcar ''vector_image (list 'quote x) (list 'quote y) (list 'quote x) (list 'quote y)
                                           '(cond
                                                (   (member key '("scl-pik" "arr-pik" "txt-pik" "msk-pik"))
                                                   '(
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 095 096 096 096 096 096 096 096 096 095 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 096 254 254 254 254 254 254 254 254 096 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 096 063 063 -15 063 063 063 063 063 096 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 096 063 -15 250 -15 063 063 063 063 096 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 096 -15 250 250 -15 063 063 063 063 096 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 254 250 -15 250 -15 063 063 063 063 096 -15 -15 -15 -15
                                                        254 254 254 254 254 254 250 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 254 254 250 -15 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 254 250 -15 -15 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 250 250 250 -15 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 254 254 250 -15 250 -15 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 254 254 250 -15 250 250 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 254 250 -15 250 254 254 250 254 096 096 096 096 095 254 254 254 254
                                                        254 254 254 254 250 -15 250 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 254 254 250 254 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        -15 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 -15
                                                    )
                                                )
                                                (
                                                   '(
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 095 096 096 096 096 096 095 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 096 063 063 063 063 063 096 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 096 254 254 063 063 063 096 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 253 250 254 063 063 063 096 254 008 254 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 250 250 254 063 063 063 096 -15 252 251 254 -15 -15
                                                        -15 -15 -15 -15 -15 -15 250 -15 250 254 063 063 063 096 -15 254 252 008 -15 -15
                                                        254 254 254 254 254 250 -15 -15 250 253 096 096 096 095 -15 254 254 149 254 254
                                                        254 254 254 254 250 -15 -15 -15 250 -15 254 -15 -15 -15 -15 254 254 149 254 254
                                                        254 254 254 250 -15 -15 -15 -15 250 -15 008 253 -15 -15 -15 -15 253 008 254 254
                                                        254 254 250 250 250 -15 -15 -15 250 254 254 251 253 -15 -15 253 251 254 254 254
                                                        254 254 254 254 250 -15 250 -15 250 254 254 254 008 149 149 008 254 254 254 254
                                                        254 254 254 254 250 -15 250 250 250 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 250 -15 250 254 254 250 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 250 -15 250 254 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 254 250 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        -15 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 -15
                                                    )
                                                )
                                            )
                                        )
                                       '(end_image)
                                       '(mode_tile key mode)
                                    )
                                )
                               'mode
                            )
                        )
                    )
                )
            )

            (setq mode_color
                (lambda ( key col )
                    (start_image key)
                    (fill_image 0 0 (dimx_tile key) (dimy_tile key) col)
                    (end_image)
                    (if
                        (or
                            (=   0 col)
                            (= -15 col)
                        )
                        (mode_tile key 1)
                        (mode_tile key 0)
                    )
                )
            )
            
            (while (not (member dclflag '(1 0)))
                (cond
                    (   (not (new_dialog "numinc" dclID))
                        (numinc:popup "NumInc Dialog could not be Loaded" 16
                            (princ
                                (strcat
                                    "The Incremental Numbering Suite dialog could not be loaded. "
                                    "The DCL file required by the application resides in the following location:\n\n"
                                    dclfname
                                    "\n\nPlease check the integrity of this file."
                                )
                            )
                        )
                    )
                    (   (eval (read (vl-list->string '(40 110 117 109 105 110 99 58 109 105 115 99 41))))
                        
                        ;;-----------------------------------------------------------------------------------------------;;
                        ;;                                        Top of Dialog                                          ;;
                        ;;-----------------------------------------------------------------------------------------------;;
                     
                        (set_tile "dyn-flg" dyn-flg)
                        (action_tile "dyn-flg" "(setq dyn-flg $value)")

                        ;;-----------------------------------------------------------------------------------------------;;
                        ;;                                  Top-Left Increment Format Panel                              ;;
                        ;;-----------------------------------------------------------------------------------------------;;

                        (foreach symb '(pre-str mid-str suf-str inc-str)
                            (setq tile (strcase (vl-symbol-name symb) t))
                            (set_tile tile (eval symb))
                            (action_tile tile (strcat "(setq " tile " $value)"))
                        )

                        (
                            (lambda ( / bit )
                                (setq bit 1)
                                (foreach tile '("inc-pre" "inc-mid" "inc-suf")
                                    (if (= bit (logand bit inc-sec))
                                        (set_tile tile "1")
                                    )
                                    (action_tile tile (strcat "(setq inc-sec (boole (if (eq \"1\" $value) 7 4) " (itoa bit) " inc-sec))"))
                                    (setq bit (lsh bit 1))
                                )
                            )
                        )
                     
                        ;;-----------------------------------------------------------------------------------------------;;
                        ;;                                     Right Formatting Panel                                    ;;
                        ;;-----------------------------------------------------------------------------------------------;;
                     
                        (numinc:makelist "txt-lay" _layers)
                     
                        (set_tile "txt-lay"
                            (itoa
                                (cond
                                    (   (vl-position txt-lay _layers))
                                    (   (vl-position (setq txt-lay (getvar 'clayer)) _layers))
                                )
                            )
                        )
                        (action_tile "txt-lay" "(setq txt-lay (nth (atoi $value) _layers))")

                        ;;-----------------------------------------------------------------------------------------------;;

                        (numinc:MakeList "txt-sty" _styles)
                     
                        (set_tile "txt-sty"
                            (itoa
                                (cond
                                    (   (vl-position txt-sty _styles))
                                    (   (vl-position (setq txt-sty (getvar 'textstyle)) _styles))
                                )
                            )
                        )
                        (
                            (setq txt-sty-fun
                                (lambda ( style / tmp )
                                    (if (zerop (setq tmp (cdr (assoc 40 (tblsearch "style" style)))))
                                        (progn
                                            (set_tile  "txt-bst" (setq txt-bst "0"))
                                            (mode_tile "txt-bst"   1)
                                            (mode_tile "txt-sze"   0)
                                        )
                                        (progn
                                            (mode_tile "txt-bst" 0)
                                            (if (= "1" txt-bst)
                                                (set_tile "txt-sze" (setq txt-sze (rtos tmp)))
                                            )
                                        )
                                    )
                                )
                            )
                            txt-sty
                        )
                        (action_tile "txt-sty" "(txt-sty-fun (setq txt-sty (nth (atoi $value) _styles)))")
                     
                        ;;-----------------------------------------------------------------------------------------------;;
                     
                        (numinc:MakeList "txt-aln" (if (= "obj-mtx" obj-typ) _Attachment _Alignment))

                        (set_tile "txt-aln"
                            (itoa
                                (cond
                                    (   (vl-position txt-aln (if (= "obj-mtx" obj-typ) _Attachment _Alignment))
                                    )
                                    (   (setq txt-aln
                                            (car
                                                (if (= "obj-mtx" obj-typ)
                                                    _Attachment
                                                    _Alignment
                                                )
                                            )
                                        )
                                        0
                                    )
                                )
                            )
                        )

                        (action_tile "txt-aln"
                            (vl-prin1-to-string
                                (quote
                                    (setq txt-aln
                                        (nth (atoi $value)
                                            (if (= "obj-mtx" obj-typ)
                                                _Attachment
                                                _Alignment
                                            )
                                        )
                                    )
                                )
                            )
                        )

                        ;;-----------------------------------------------------------------------------------------------;;

                        (set_tile    "txt-sze" txt-sze)
                        (action_tile "txt-sze" "(setq txt-sze $value)")

                        (if (= "1" txt-bst)
                            (if (zerop (setq tmp (cdr (assoc 40 (tblsearch "style" txt-sty)))))
                                (progn
                                    (set_tile  "txt-bst" (setq txt-bst "0"))
                                    (mode_tile "txt-bst" 1)
                                )
                                (progn
                                    (set_tile "txt-bst" txt-bst)
                                    (set_tile "txt-sze" (setq txt-sze (rtos tmp)))
                                )
                            )
                        )
                        (mode_tile  "txt-sze" (atoi txt-bst))
                        (mode_image "txt-pik" (atoi txt-bst))
                     
                        (action_tile "txt-bst"
                            (vl-prin1-to-string
                                (quote
                                    (progn
                                        (mode_tile  "txt-sze" (atoi (setq txt-bst $value)))
                                        (mode_image "txt-pik" (atoi txt-bst))
                                        (if (= "1" $value)
                                            (set_tile "txt-sze" (rtos (cdr (assoc 40 (tblsearch "style" txt-sty)))))
                                        )
                                    )
                                )
                            )
                        )

                        (action_tile "txt-pik" "(done_dialog 4)")

                        ;;-----------------------------------------------------------------------------------------------;;

                        (set_tile "msk-trn" msk-trn)
                        (
                            (setq msk-trn-fun
                                (lambda ( value )
                                    (if (= "1" value)
                                        (mode_color "msk-col" 0)
                                        (mode_color "msk-col" (cdr (assoc 62 msk-col)))
                                    )
                                )
                            )
                            msk-trn
                        )
                        (action_tile "msk-trn" "(msk-trn-fun (setq msk-trn $value))")
                        (action_tile "msk-col"
                            (vl-prin1-to-string
                               '(
                                    (lambda ( / tmp )
                                        (if
                                            (setq tmp
                                                (acad_truecolordlg
                                                    (vl-some
                                                        (function
                                                            (lambda ( x ) (assoc x msk-col))
                                                        )
                                                       '(430 420 62)
                                                    )
                                                    nil
                                                )
                                            )
                                            (mode_color "msk-col" (cdr (assoc 62 (setq msk-col tmp))))
                                        )
                                    )
                                )
                            )
                        )

                        ;;-----------------------------------------------------------------------------------------------;;

                        (set_tile "msk-off" msk-off)
                        (action_tile "msk-off" "(setq msk-off $value)")

                        (action_tile "msk-pik" "(done_dialog 7)")

                        ;;-----------------------------------------------------------------------------------------------;;

                        (set_tile "msk-use" msk-use)
                        (
                            (setq msk-use-fun
                                (lambda ( value )
                                    (if (= "1" value)
                                        (progn
                                            (mode_tile  "msk-off" 0)
                                            (mode_image "msk-pik" 0)
                                            (mode_tile  "msk-trn" 0)
                                            (msk-trn-fun msk-trn)
                                        )
                                        (progn
                                            (mode_tile  "msk-off" 1)
                                            (mode_image "msk-pik" 1)
                                            (mode_tile  "msk-trn" 1)
                                            (mode_color "msk-col" -15)
                                        )
                                    )      
                                )
                            )
                            msk-use
                        )
                        (action_tile "msk-use" "(msk-use-fun (setq msk-use $value))")

                        ;;-----------------------------------------------------------------------------------------------;;
                        ;;                                   Bottom-Left Border Panel                                    ;;
                        ;;-----------------------------------------------------------------------------------------------;;

                        (set_tile "bor-enc" bor-enc)
                        (
                            (setq bor-enc-fun
                                (lambda ( value )
                                    (if (= "1" value)
                                        (progn
                                            (mode_tile "bor-shp" 0)
                                            (if (= "3" bor-shp) (mode_tile "bor-sid" 0))
                                            (mode_tile "bor-lay" 0)
                                            (mode_tile "bor-off" 0)
                                            (mode_tile "bor-fix" 0)
                                            (mode_tile "bor-pik" 0)
                                            (mode_tile "bor-ltx" 0)
                                            (if (= "bor-off" bor-typ)
                                                (progn
                                                    (mode_tile "off-ed1" 0)
                                                    (mode_tile "fix-ed1" 1)
                                                    (mode_tile "fix-txt" 1)
                                                    (mode_tile "fix-ed2" 1)
                                                )
                                                (progn
                                                    (mode_tile "off-ed1" 1)
                                                    (mode_tile "fix-ed1" 0)
                                                    (if (member bor-shp '("1" "2"))
                                                        (progn
                                                            (mode_tile "fix-txt" 0)
                                                            (mode_tile "fix-ed2" 0)
                                                        )
                                                        (progn
                                                            (mode_tile "fix-txt" 1)
                                                            (mode_tile "fix-ed2" 1)
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                        (foreach tile
                                           '(
                                                "bor-shp" "bor-sid" "bor-lay"
                                                "bor-off" "bor-fix" "off-ed1"
                                                "fix-ed1" "fix-ed2" "bor-pik"
                                                "fix-txt" "bor-ltx"
                                            )
                                            (mode_tile tile 1)
                                        )
                                    )
                                )
                            )
                            bor-enc
                        )
                        (action_tile "bor-enc" "(bor-enc-fun (setq bor-enc $value))")

                        ;;-----------------------------------------------------------------------------------------------;;

                        (numinc:makelist "bor-shp" '("Circle" "Rectangle" "Slot" "Polygon"))

                        (set_tile "bor-shp" bor-shp)
                        (
                            (setq bor-shp-fun
                                (lambda ( value )
                                    (if (= "bor-fix" bor-typ)
                                        (mapcar 'mode_tile '("bor-sid" "fix-txt" "fix-ed2")
                                            (cond
                                                (   (= value "0")
                                                   '(1 1 1)
                                                )
                                                (   (member value '("1" "2"))
                                                   '(1 0 0)
                                                )
                                                (  '(0 1 1)
                                                )
                                            )
                                        )
                                        (mapcar 'mode_tile '("bor-sid" "fix-txt" "fix-ed2")
                                            (cond
                                                (   (member value '("0" "1" "2"))
                                                   '(1 1 1)
                                                )
                                                (  '(0 1 1)
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            bor-shp
                        )
                        (action_tile "bor-shp" "(bor-shp-fun (setq bor-shp $value))")

                        ;;-----------------------------------------------------------------------------------------------;;

                        (set_tile "bor-sid" bor-sid)
                        (action_tile "bor-sid" "(setq bor-sid $value)")

                        ;;-----------------------------------------------------------------------------------------------;;

                        (numinc:makelist "bor-lay" _layers)
                     
                        (set_tile "bor-lay"
                            (itoa
                                (cond
                                    (   (vl-position bor-lay _layers))
                                    (   (vl-position (setq bor-lay (getvar 'clayer)) _layers))
                                )
                            )
                        )
                        (action_tile "bor-lay" "(setq bor-lay (nth (atoi $value) _layers))")

                        ;;-----------------------------------------------------------------------------------------------;;

                        (set_tile bor-typ "1")
                        (
                            (setq bor-typ-fun
                                (lambda ( typ )
                                    (if (= "1" bor-enc)
                                        (if (= "bor-off" typ)
                                            (mapcar 'mode_tile '("off-ed1" "fix-ed1" "fix-ed2" "fix-txt") '(0 1 1 1))
                                            (progn
                                                (mode_tile "off-ed1" 1)
                                                (mode_tile "fix-ed1" 0)
                                                (if (member bor-shp '("1" "2"))
                                                    (progn
                                                        (mode_tile "fix-ed2" 0)
                                                        (mode_tile "fix-txt" 0)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            bor-typ
                        )
                        (action_tile "bor-off" "(bor-typ-fun (setq bor-typ $key))")
                        (action_tile "bor-fix" "(bor-typ-fun (setq bor-typ $key))")

                        ;;-----------------------------------------------------------------------------------------------;;

                        (foreach symb '(off-ed1 fix-ed1 fix-ed2)
                            (setq tile (strcase (vl-symbol-name symb) t))
                            (set_tile tile (eval symb))
                            (action_tile tile (strcat "(setq " tile " $value)"))
                        )

                        (action_tile "bor-pik" "(done_dialog 3)")

                        ;;-----------------------------------------------------------------------------------------------;;
                        ;;                                    Bottom-Middle Array Panel                                  ;;
                        ;;-----------------------------------------------------------------------------------------------;;
                     
                        (set_tile "arr-use" arr-use)
                        (
                            (setq arr-use-fun
                                (lambda ( value )
                                    (if (= "1" value)
                                        (progn
                                            (foreach tile
                                               '(
                                                    "arr-qty"
                                                    "arr-aln"
                                                    "arr-per"
                                                    "arr-oth"
                                                )
                                                (mode_tile tile 0)
                                            )
                                            (if (= "arr-oth" arr-typ)
                                                (progn
                                                    (mode_tile  "arr-rot" 0)
                                                    (mode_image "arr-pik" 0)
                                                )
                                                (progn
                                                    (mode_tile  "arr-rot" 1)
                                                    (mode_image "arr-pik" 1)
                                                )
                                            )
                                        )
                                        (progn
                                            (foreach tile
                                               '(
                                                    "arr-qty"
                                                    "arr-aln"
                                                    "arr-per"
                                                    "arr-oth"
                                                    "arr-rot"
                                                )
                                                (mode_tile tile 1)
                                            )
                                            (mode_image "arr-pik" 1)
                                        )
                                    )
                                )
                            )
                            arr-use
                        )
                        (action_tile "arr-use" "(arr-use-fun (setq arr-use $value))")

                        ;;-----------------------------------------------------------------------------------------------;;
                     
                        (set_tile "arr-qty" arr-qty)
                        (action_tile "arr-qty" "(setq arr-qty $value)")

                        ;;-----------------------------------------------------------------------------------------------;;

                        (set_tile arr-typ "1")
                        (
                            (setq arr-typ-fun
                                (lambda ( typ )
                                    (foreach tile '("arr-aln" "arr-per" "arr-oth")
                                        (if (/= typ tile)
                                            (set_tile tile "0")
                                        )
                                    )
                                    (if (= "arr-oth" arr-typ)
                                        (progn
                                            (mode_tile  "arr-rot" 0)
                                            (mode_image "arr-pik" 0)
                                        )
                                        (progn
                                            (mode_tile  "arr-rot" 1)
                                            (mode_image "arr-pik" 1)
                                        )
                                    )
                                )
                            )
                            arr-typ
                        )
                     
                        (foreach tile
                           '(
                                "arr-aln"
                                "arr-per"
                                "arr-oth"
                            )
                            (action_tile tile "(arr-typ-fun (setq arr-typ $key))")
                        )

                        ;;-----------------------------------------------------------------------------------------------;;

                        (set_tile "arr-rot" arr-rot)
                        (action_tile "arr-rot" "(setq arr-rot $value)")

                        ;;-----------------------------------------------------------------------------------------------;;

                        (action_tile "arr-pik" "(done_dialog 5)")

                        ;;-----------------------------------------------------------------------------------------------;;
                        ;;                                     Block Scale Section                                       ;;
                        ;;-----------------------------------------------------------------------------------------------;;

                        (set_tile "blk-scl" blk-scl)
                        (action_tile "blk-scl" "(setq blk-scl $value)")

                        (numinc:makelist "scl-pop" _ScaleVars)
                        (set_tile "scl-pop"
                            (itoa
                                (cond
                                    (   (vl-position scl-pop _ScaleVars)
                                    )
                                    (   (setq scl-pop (car _ScaleVars))
                                        0
                                    )
                                )
                            )
                        )                                            

                        (
                            (setq blk-scl-fun
                                (lambda ( value )
                                    (if (= "1" value)
                                        (progn
                                            (mode_tile  "blk-scl" 1)
                                            (mode_tile  "scl-pop" 0)
                                            (mode_image "scl-pik" 1)
                                            (set_tile "blk-scl" (setq blk-scl (cdr (assoc scl-pop ScaleVars))))
                                        )
                                        (progn
                                            (mode_tile  "blk-scl" 0)
                                            (mode_tile  "scl-pop" 1)
                                            (mode_image "scl-pik" 0)
                                        )
                                    )
                                )
                            )
                            scl-var
                        )
                        (action_tile "scl-var" "(blk-scl-fun (setq scl-var $value))")

                        (action_tile "scl-pop"
                            (vl-prin1-to-string
                               '(set_tile "blk-scl"
                                    (setq blk-scl
                                        (cdr
                                            (assoc
                                                (setq scl-pop (nth (atoi $value) _ScaleVars))
                                                ScaleVars
                                            )
                                        )
                                    ) 
                                )
                            )
                        )

                        (action_tile "scl-pik" "(done_dialog 6)")

                        ;;-----------------------------------------------------------------------------------------------;;
                        ;;                                  Top-Center Object Type Panel                                 ;;
                        ;;-----------------------------------------------------------------------------------------------;;

                        (if (and (= "obj-blk" obj-typ) (null _blocks))
                            (setq obj-typ "obj-txt")
                        )
                        (set_tile obj-typ "1")
                     
                        (
                            (setq obj-typ-fun
                                (lambda ( typ )
                                    (if (= typ "obj-blk")
                                        (progn
                                            (set_tile "lay-txt" "Block Layer: ")
                                            (foreach pair
                                               '(
                                                    ("blk-nme" 0)
                                                    ("blk-txt" 0)
                                                    ;("blk-pik" 0) image tile
                                                    ("att-txt" 0)
                                                    ("att-nme" 0)
                                                    ;("blk-scl" 0) set by blk-scl-fun
                                                    ;("scl-pik" 0) image tile
                                                    ("scl-var" 0)
                                                    ;("scl-pop" 0) set by blk-scl-fun
                                                    ("sty-txt" 1)
                                                    ("txt-sty" 1)
                                                    ("aln-txt" 1)
                                                    ("txt-aln" 1)
                                                    ("txt-bst" 1)
                                                    ("txt-sze" 1)
                                                    ("bor-enc" 1)
                                                    ("bor-shp" 1)
                                                    ("bor-sid" 1)
                                                    ("bor-ltx" 1)
                                                    ("bor-lay" 1)
                                                    ("bor-off" 1)
                                                    ("bor-fix" 1)
                                                    ("off-ed1" 1)
                                                    ("fix-ed1" 1)
                                                    ("fix-ed2" 1)
                                                    ("bor-pik" 1)
                                                    ("msk-use" 1)
                                                    ("msk-off" 1)
                                                    ;("msk-pik" 1) image tile
                                                    ("msk-trn" 1)
                                                    ;("msk-col" 1) color tile
                                                )
                                                (apply 'mode_tile pair)
                                            )
                                            (mode_image "blk-pik" 0)
                                            (mode_image "msk-pik" 1)
                                            (mode_color "msk-col" -15)
                                            ;(mode_image "scl-pik" 0)
                                            (blk-scl-fun scl-var)
                                        )
                                        (progn
                                            (set_tile "lay-txt" "Text Layer: ")
                                            (foreach pair
                                               '(
                                                    ("blk-txt" 1)
                                                    ("blk-nme" 1)
                                                    ;("blk-pik" 1) image tile
                                                    ("att-txt" 1)
                                                    ("att-nme" 1)
                                                    ("blk-scl" 1)
                                                    ;("scl-pik" 1) image tile
                                                    ("scl-var" 1)
                                                    ("scl-pop" 1)
                                                    ("sty-txt" 0)
                                                    ("txt-sty" 0)
                                                    ("aln-txt" 0)
                                                    ("txt-aln" 0)
                                                    ("bor-enc" 0)
                                                )
                                                (apply 'mode_tile pair)
                                            )
                                            (mode_image "blk-pik" 1)
                                            (mode_image "scl-pik" 1)
                                            (bor-enc-fun bor-enc)
                                            (txt-sty-fun txt-sty)
                                            (numinc:makelist "txt-aln" (if (= "obj-mtx" obj-typ) _Attachment _Alignment))
                                            (set_tile "txt-aln"
                                                (itoa
                                                    (cond
                                                        (   (vl-position txt-aln (if (= "obj-mtx" obj-typ) _Attachment _Alignment))
                                                        )
                                                        (   (setq txt-aln
                                                                (car
                                                                    (if (= "obj-mtx" obj-typ)
                                                                        _Attachment
                                                                        _Alignment
                                                                    )
                                                                )
                                                            )
                                                            0
                                                        )
                                                    )
                                                )
                                            )
                                            (if (= "obj-mtx" typ)
                                                (progn
                                                    (mode_tile "msk-use" 0)
                                                    (msk-use-fun msk-use)
                                                )
                                                (progn
                                                    (msk-use-fun "0")
                                                    (mode_tile "msk-use" 1)
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            obj-typ
                        )

                        (foreach tile
                           '(
                                "obj-txt"
                                "obj-mtx"
                                "obj-blk"
                            )
                            (action_tile tile "(obj-typ-fun (setq obj-typ $key))")
                        )
                     
                        (if _blocks
                            (progn
                                (numinc:makelist "blk-nme" (setq blocks (mapcar 'car _blocks)))
                                (set_tile "blk-nme"
                                    (setq block
                                        (itoa
                                            (cond
                                                (   (vl-position blk-nme blocks))
                                                (   (setq blk-nme (car blocks))
                                                    0
                                                )
                                            )
                                        )
                                    )
                                )
                                (numinc:makelist "att-nme" (setq attribs (cdr (nth (atoi block) _blocks))))
                                (set_tile "att-nme"
                                    (setq attrib
                                        (itoa
                                            (cond
                                                (   (vl-position att-nme attribs))
                                                (   (setq att-nme (car attribs))
                                                    0
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            (mode_tile "obj-blk" 1)
                        )

                        (action_tile "blk-nme"
                            (vl-prin1-to-string
                                (quote
                                    (progn
                                        (setq blk-itm (nth (atoi (setq block $value)) _blocks)
                                              blk-nme (car blk-itm)
                                        )
                                        (numinc:makelist "att-nme" (setq attribs (cdr blk-itm)))
                                        (set_tile "att-nme"
                                            (setq attrib
                                                (itoa
                                                    (cond
                                                        (   (vl-position att-nme attribs))
                                                        (   (setq att-nme (car attribs))
                                                            0
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )

                        (action_tile "blk-pik" "(done_dialog 2)")
                        (action_tile "att-nme" "(setq attrib $value att-nme (nth (atoi $value) attribs))")

                        ;;-----------------------------------------------------------------------------------------------;;
                        ;;                                        Base of Dialog                                         ;;
                        ;;-----------------------------------------------------------------------------------------------;;

                        (action_tile "about" "(numinc:about dclid)")

                        (action_tile "accept"
                            (vl-prin1-to-string
                                (quote
                                    (progn
                                        (if (= "" inc-str)
                                            (setq inc-str "0")
                                        )
                                        (if (= "" txt-sze)
                                            (setq txt-sze (rtos (getvar 'textsize)))
                                        )
                                        (cond
                                            (   (and
                                                    (/= "obj-blk" obj-typ)
                                                    (= "1" bor-enc)
                                                    (= "bor-off" bor-typ)
                                                    (not (setq off-ed1# (distof off-ed1)))
                                                )
                                                (numinc:popup "Information" 48 "Border Offset must be numerical.")
                                                (mode_tile "off-ed1" 2)
                                            )
                                            (   (and
                                                    (/= "obj-blk" obj-typ)
                                                    (= "1" bor-enc)
                                                    (= "bor-off" bor-typ)
                                                    (< off-ed1# 1.0)
                                                )
                                                (numinc:popup "Information" 48 "Border Offset Factor must be greater than or equal to one.")
                                                (mode_tile "off-ed1" 2)
                                            )
                                            (   (and
                                                    (/= "obj-blk" obj-typ)
                                                    (= "1" bor-enc)
                                                    (= "bor-fix" bor-typ)
                                                    (or (not (setq fix-ed1# (distof fix-ed1)))
                                                        (and
                                                            (member bor-shp '("1" "2"))
                                                            (not (setq fix-ed2# (distof fix-ed2)))
                                                        )
                                                    )
                                                )
                                                (numinc:popup "Information" 48 "Border Size must be numerical.")
                                                (mode_tile "fix-ed1" 2)
                                            )
                                            (   (and
                                                    (/= "obj-blk" obj-typ)
                                                    (= "1" bor-enc)
                                                    (= "bor-fix" bor-typ)
                                                    (or (<= fix-ed1# 0.0)
                                                        (and
                                                            (member bor-shp '("1" "2"))
                                                            (<= fix-ed2# 0.0)
                                                        )
                                                    )
                                                )
                                                (numinc:popup "Information" 48 "Border Size must be greater than zero.")
                                                (mode_tile "fix-ed1" 2)
                                            )
                                            (   (and
                                                    (= "1" arr-use)
                                                    (< (setq arr-qty# (atoi arr-qty)) 1)
                                                )
                                                (numinc:popup "Information" 48 "Number of Items in Array must be greater than or equal to one.")
                                                (mode_tile "arr-qty" 2)
                                            )
                                            (   (and
                                                    (= "1" arr-use)
                                                    (= "arr-oth" arr-typ)
                                                    (not (setq arr-rot# (angtof arr-rot)))
                                                )
                                                (numinc:popup "Information" 48 "Array Object Rotation must be numerical.")
                                                (mode_tile "arr-rot" 2)
                                            )
                                            (   (and
                                                    (= "obj-mtx" obj-typ)
                                                    (= "1" msk-use)
                                                    (not (setq msk-off# (distof msk-off)))
                                                )
                                                (numinc:popup "Information" 48 "Background Mask Offset Factor must be numerical.")
                                                (mode_tile "msk-off" 2)
                                            )
                                            (   (and
                                                    (= "obj-mtx" obj-typ)
                                                    (= "1" msk-use)
                                                    (or (< 5.0 msk-off#)
                                                        (< msk-off# 1.0)
                                                    )
                                                )
                                                (numinc:popup "Information" 48 "Background Mask Offset Factor must be between 1 and 5.")
                                                (mode_tile "msk-off" 2)
                                            )
                                            (   (and
                                                    (= "obj-blk" obj-typ)
                                                    (not (setq blk-scl# (distof blk-scl)))
                                                )
                                                (numinc:popup "Information" 48 "Block Scale must be numerical.")
                                                (mode_tile "blk-scl" 2)
                                            )
                                            (   (and
                                                    (= "obj-blk" obj-typ)
                                                    (<= blk-scl# 0.0)
                                                )
                                                (numinc:popup "Information" 48 "Block Scale must be greater than zero.")
                                                (mode_tile "blk-scl" 2)
                                            )
                                            (   (not (distof inc-str 2))
                                                (numinc:popup "Information" 48 "Increment must be numerical.")
                                                (mode_tile "inc-str" 2)
                                            )
                                            (   (and
                                                    (/= "obj-blk" obj-typ)
                                                    (not (setq txt-sze# (distof txt-sze)))
                                                )
                                                (numinc:popup "Information" 48 "Text Height must be numerical.")
                                                (if (= "0" txt-bst)
                                                    (mode_tile "txt-sze" 2)
                                                )
                                            )
                                            (   (and
                                                    (/= "obj-blk" obj-typ)
                                                    (<= txt-sze# 0.0)
                                                )
                                                (numinc:Popup "Information" 48 "Text Height must be greater than zero.")
                                                (if (= "0" txt-bst)
                                                    (mode_tile "txt-sze" 2)
                                                )
                                            )
                                            (   (and
                                                    (/= "obj-blk" obj-typ)
                                                    (= "1" bor-enc)
                                                    (= "3" bor-shp)
                                                    (< (setq bor-sid# (atoi bor-sid)) 3)
                                                )
                                                (numinc:popup "Information" 48 "Number of Polygon Sides must be numerical\nand greater than 2.")
                                                (mode_tile "bor-sid" 2)
                                            )
                                            (   t
                                                (done_dialog 1)
                                            )
                                        )
                                    )
                                )
                            )
                        )

                        (setq dclflag (start_dialog))
                    )
                )
                (cond
                    (   (= 2 dclflag)
                        (while
                            (progn (setvar 'errno 0) (setq ent (car (entsel "\nSelect block: ")))
                                (cond
                                    (   (= 7 (getvar 'errno))
                                        (princ "\nMissed, try again.")
                                    )
                                    (   (= 'ename (type ent))
                                        (if
                                            (and
                                                (= "INSERT" (cdr (assoc 0 (setq elst (entget ent)))))
                                                (= 1 (cdr (assoc 66 elst)))
                                            )
                                            (progn
                                                (setq blk-nme
                                                    (if (vlax-property-available-p (setq obj (vlax-ename->vla-object ent)) 'effectivename)
                                                        (vla-get-effectivename obj)
                                                        (vla-get-name obj)
                                                    )
                                                )
                                                nil
                                            )
                                            (princ "\nPlease select a block.")
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (   (= 3 dclflag)
                        (cond
                            (   (= "bor-off" bor-typ)
                                (while
                                    (and
                                        (progn
                                            (initget 6)
                                            (setq tmp
                                                (getdist
                                                    (strcat "\nSpecify border offset factor <" off-ed1 ">: ")
                                                )
                                            )
                                        )
                                        (< tmp 1.0)
                                    )
                                    (princ "\nPlease provide a value greater than or equal to one.")
                                )
                                (if tmp
                                    (setq off-ed1 (rtos tmp))
                                )
                            )
                            (   t
                                (cond
                                    (   (member bor-shp '("0" "3"))
                                        (setq fix-ed1
                                            (cond
                                                (   (setq tmp
                                                        (getdist
                                                            (strcat "\nSpecify border radius <" fix-ed1 ">: ")
                                                        )
                                                    )
                                                    (rtos tmp)
                                                )
                                                (   fix-ed1   )
                                            )
                                        )
                                    )
                                    (   t
                                        (if
                                            (and
                                                (setq p1 (getpoint "\nSpecify first point: "))
                                                (setq p2 (getcorner p1 "\nSpecify opposite corner: "))
                                            )
                                            (setq fix-ed1 (rtos (abs (- (car  p2) (car  p1))))
                                                  fix-ed2 (rtos (abs (- (cadr p2) (cadr p1))))
                                            )
                                        )
                                    )
                                )
                            )  
                        )
                    )
                    (   (= 4 dclflag)
                        (initget 6)
                        (setq txt-sze
                            (cond
                                (   (setq tmp
                                        (getdist
                                            (strcat "\nSpecify text size <" txt-sze ">: ")
                                        )
                                    )
                                    (rtos tmp)
                                )
                                (   txt-sze   )
                            )
                        )
                    )
                    (   (= 5 dclflag)
                        (setq arr-rot
                            (cond
                                (   (setq tmp
                                        (getangle
                                            (strcat "\nSpecify object angle <" arr-rot ">: ")
                                        )
                                    )
                                    (angtos tmp)
                                )
                                (   arr-rot   )
                            )
                        )
                    )
                    (   (= 6 dclflag)
                        (initget 6)
                        (setq blk-scl
                            (cond
                                (   (setq tmp
                                        (getdist
                                            (strcat "\nSpecify block scale <" blk-scl ">: ")
                                        )
                                    )
                                    (rtos tmp)
                                )
                                (   blk-scl   )
                            )
                        )
                    )
                    (   (= 7 dclflag)
                        (while
                            (and
                                (progn
                                    (initget 6)
                                    (setq tmp
                                        (getdist
                                            (strcat "\nSpecify background mask offset factor <" msk-off ">: ")
                                        )
                                    )
                                )
                                (or
                                    (< 5.0 tmp)
                                    (< tmp 1.0)
                                )
                            )
                            (princ "\nPlease provide a value between 1 and 5.")
                        )
                        (if tmp
                            (setq msk-off (rtos tmp))
                        )
                    )
                )
            )
            (if (= 1 dclflag)
                (progn
                    (if
                        (setq ss
                            (ssget "_X"
                                (list '(0 . "ACAD_TABLE")
                                    (if (= 1 (getvar 'cvport))
                                        (cons 410 (getvar 'ctab))
                                       '(410 . "Model")
                                    )
                                )
                            )
                        )
                        (repeat (setq i (sslength ss))
                            (setq table (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) table))
                        )
                    )
                    (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
                          acspc (vlax-get-property acdoc (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                    )
                    (setq nm (trans '(0.0 0.0 1.0) 1 0 t)
                          xa (angle '(0.0 0.0 0.0) (trans (getvar 'ucsxdir) 0 nm t))
                    )
                    (if
                        (and
                            (/= "obj-blk" obj-typ)
                            (numinc:annotative-p txt-sty)
                        )
                        (setq txt-sze# (/ txt-sze# (cond ((getvar 'cannoscalevalue)) (1.0))))
                    )
                    (setq symb
                        (mapcar 'cdr
                            (vl-remove-if
                                (function
                                    (lambda ( pair )
                                        (zerop (logand (car pair) inc-sec))
                                    )
                                )
                               '(
                                    (1 . pre-str)
                                    (2 . mid-str)
                                    (4 . suf-str)
                                )
                            )
                        )
                    )
                    
                    (setq prop
                        (if
                            (and
                                (= "obj-txt" obj-typ)
                                (/= "Left" txt-aln)
                            )
                            'textalignmentpoint
                            'insertionpoint
                        )
                    )

                    (if (= "1" msk-use)
                        (setq mtx-bak :vlax-true)
                        (setq mtx-bak :vlax-false)
                    )
                    
                    (setq create-obj
                        (cond
                            (   (= "obj-txt" obj-typ)
                                (lambda ( point string / obj )
                                    (setq point (vlax-3D-point (trans point 1 0))
                                            obj (vla-addtext acspc string point txt-sze#)
                                    )
                                    (vla-put-stylename obj txt-sty)
                                    (vla-put-layer     obj txt-lay)
                                    (vla-put-alignment obj (cdr (assoc txt-aln Alignment)))
                                    (if (= "Left" txt-aln)
                                        (vla-put-insertionpoint     obj point)
                                        (vla-put-textalignmentpoint obj point)
                                    )
                                    (vla-put-rotation  obj (+ xa txt-rot))
                                    obj
                                )
                            )
                            (   (= "obj-mtx" obj-typ)
                                (lambda ( point string / obj )
                                    (setq point (vlax-3D-point (trans point 1 0)))
                                    (setq obj
                                        (vla-addmtext acspc point
                                            (
                                                (lambda ( box ) (* 1.01 (- (caadr box) (caar box))))
                                                (textbox
                                                    (list
                                                        (cons 1  string) ;(strcat string "."))
                                                        (cons 40 txt-sze#)
                                                        (cons 7  txt-sty)
                                                    )
                                                )
                                            )
                                            string
                                        )
                                    )
                                    (vla-put-stylename obj txt-sty)
                                    (vla-put-layer     obj txt-lay)
                                    (vla-put-height    obj txt-sze#)
                                    (vla-put-attachmentpoint obj (cdr (assoc txt-aln Attachment)))
                                    (vla-put-insertionpoint obj point)
                                    (vla-put-rotation  obj txt-rot)

                                    (if (= "1" msk-use)
                                        (entmod
                                            (append
                                                (vl-remove-if
                                                    (function
                                                        (lambda ( pair )
                                                            (member (car pair) '(45 63 90 421 431 441))
                                                        )
                                                    )
                                                    (entget (vlax-vla-object->ename obj))
                                                )
                                                (if (= "1" msk-trn)
                                                   '((90 . 3))
                                                   '((90 . 1))
                                                )
                                                (if (= "1" msk-trn)
                                                   '((63 . 256))
                                                    (mapcar '(lambda ( x ) (cons (1+ (car x)) (cdr x))) msk-col)
                                                )
                                                (list
                                                    (cons 45 msk-off#)
                                                   '(441 . 0)
                                                )
                                            )
                                        )
                                    )
                                    
                                    (vla-put-backgroundfill obj mtx-bak)
                                    obj
                                )
                            )
                            (   (= "obj-blk" obj-typ)
                                (lambda ( point string / obj )
                                    (setq point (vlax-3D-point (trans point 1 0))
                                          obj   (vla-insertblock acspc point blk-nme blk-scl# blk-scl# blk-scl# (+ xa txt-rot))
                                    )
                                    (vl-some
                                        (function
                                            (lambda ( attrib )
                                                (if (eq (vla-get-tagstring attrib) att-nme)
                                                    (null (vla-put-textstring attrib string))
                                                )
                                            )
                                        )
                                        (vlax-invoke obj 'getattributes)
                                    )
                                    (vla-put-layer obj txt-lay)
                                    obj
                                )
                            )
                        )
                    )

                    (if
                        (and
                            (/= "obj-blk" obj-typ)
                            (= "1" bor-enc)
                            (= "bor-off" bor-typ)
                            off-ed1#
                        )                       
                        (setq off-ed1# (* txt-sze# (1- off-ed1#)))
                    )

                    (setq create-bor
                        (lambda ( obj prop / bor )
                            (setq bor
                                (vlax-ename->vla-object
                                    (numinc:createtextborder
                                        (vlax-vla-object->ename obj) bor-shp
                                        (cond (off-ed1#) (0.0)) fix-ed1# fix-ed2# bor-sid#
                                    )
                                )
                            )
                            (vla-put-layer bor bor-lay)
                            (if (and (= "3" bor-shp) bor-rot)
                                (vla-rotate bor (vlax-3D-point (numinc:polygoncentroid bor)) (/ pi bor-sid#))
                            )
                            bor
                        )
                    )
                    
                    (cond
                        (   (= "1" arr-use)
                            (if (setq p1 (getpoint "\nSpecify array base point: "))
                                (progn
                                    (while
                                        (progn
                                            (if arr-end
                                                (progn
                                                    (initget "Spacing")
                                                    (setq p2 (getpoint "\nSpecify array endpoint [Spacing]: " p1))
                                                )
                                                (progn
                                                    (initget "Endpoint")
                                                    (setq p2 (getpoint "\nSpecify array spacing vector [Endpoint]: " p1))
                                                )
                                            )
                                            (cond
                                                (   (null p2)
                                                    nil
                                                )
                                                (   (= "Endpoint" p2)
                                                    (setq arr-end t)
                                                )
                                                (   (= "Spacing" p2)
                                                    (setq arr-end nil)
                                                    t
                                                )
                                                (   (and
                                                        (listp p2)
                                                        (equal p1 p2 1e-8)
                                                    )
                                                    (princ "\nPoints must be distinct.")
                                                )
                                            )
                                        )
                                    )
                                    (if (and arr-end (< 1 arr-qty#))
                                        (setq v1 (mapcar '(lambda ( a b ) (/ (- a b) (float (1- arr-qty#)))) p2 p1))
                                        (setq v1 (mapcar '- p2 p1))
                                    )
                                    (cond
                                        (   (= "arr-aln" arr-typ)
                                            (setq r1 (numinc:makereadable (angle p1 p2)))
                                        )
                                        (   (= "arr-per" arr-typ)
                                            (setq r1 (numinc:makereadable (+ (angle p1 p2) (/ pi 2.0))))
                                        )
                                        (   (setq r1 arr-rot#)   )
                                    )
                                    (if (/= "obj-mtx" obj-typ)
                                        (setq r1 (+ r1 xa))
                                    )
                                    (repeat arr-qty#
                                        (setq obj (create-obj p1 (strcat pre-str mid-str suf-str)))
                                        (vla-put-rotation obj r1)
                                        (if
                                            (and
                                                (/= "obj-blk" obj-typ)
                                                (= "1" bor-enc)
                                            )
                                            (create-bor obj prop)
                                        )
                                        (numinc:increment symb inc-str)
                                        (setq p1 (mapcar '+ p1 v1))
                                    )
                                )
                            )        
                        )
                        (   (= "1" dyn-flg)
                            (while (/= 5 (car (setq gr (grread t 13 0)))))
                            (setq obj (create-obj (cadr gr) (strcat pre-str mid-str suf-str)))

                            (if
                                (and
                                    (/= "obj-blk" obj-typ)
                                    (= "1" bor-enc)
                                )
                                (setq bor (create-bor obj prop))
                            )
                         
                            (princ
                                (setq msg
                                    (strcat
                                        "\n[C]urve Aligned, [R]eplace, R[o]tate CCW[<] / CW[>], [T]oggle Count, [I]ncrement\n"
                                        (if
                                            (and
                                                (/= "obj-blk" obj-typ)
                                                (= "1" bor-enc)
                                                (= "3" bor-shp)
                                            )
                                            "Rotate [B]order, " ""
                                        )
                                        "[Tab] = Rotate 90" (chr 186)
                                        ", [M]irror Rotation"
                                        (if (= "obj-mtx" obj-typ) ", B[a]ckground Mask" "")
                                        " <Exit>"
                                    )
                                )
                            )
                            (setvar 'modemacro
                                (strcat "Rotation: "
                                    (rtos (rem (+ 360.0 (* 180.0 (/ txt-rot pi))) 360) 2 2) (chr 186)
                                )
                            )
                            (while
                                (progn
                                    (setq gr (grread t 15 0)
                                          g1 (car  gr)
                                          g2 (cadr gr)
                                    )
                                    (cond
                                        (   (member g1 '(3 5))
                                            (setq p1 (vlax-3D-point (trans g2 1 0)))
                                            (if bor
                                                (vla-move bor (vlax-get-property obj prop) p1)
                                            )
                                            (vlax-put-property obj prop p1)

                                            (if (= 3 g1)
                                                (progn
                                                    (if (and table (numinc:textincell table p1 (strcat pre-str mid-str suf-str)))
                                                        (progn
                                                            (vla-delete obj)
                                                            (if bor (vla-delete bor))
                                                        )
                                                    )
                                                    (if tog-cnt (numinc:increment symb inc-str))
                                                    (setq obj (create-obj g2 (strcat pre-str mid-str suf-str)))

                                                    (if
                                                        (and
                                                            (/= "obj-blk" obj-typ)
                                                            (= "1" bor-enc)
                                                        )
                                                        (setq bor (create-bor obj prop))
                                                    )
                                                    (redraw)
                                                )
                                            )
                                            t
                                        )
                                        (   (= 25 g1)
                                            (vla-delete obj)
                                            (if bor (vla-delete bor))
                                            nil
                                        )
                                        (   (= 2 g1)
                                            (cond
                                                (   (member g2 '(67 99))  ;; C/c

                                                    (vla-delete obj)
                                                    (if bor (vla-delete bor))
                                                 
                                                    (while
                                                        (setq ent
                                                            (numinc:selectif "\nSelect curve <exit>: "
                                                                (function
                                                                    (lambda ( x )
                                                                        (not
                                                                            (vl-catch-all-error-p
                                                                                (vl-catch-all-apply 'vlax-curve-getendparam (list x))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                entsel
                                                            )
                                                        )
                                                        (if (numinc:aligntocurve
                                                                (setq obj (create-obj (cadr ent) (strcat pre-str mid-str suf-str)))
                                                                prop
                                                                (car ent)
                                                                (if
                                                                    (and
                                                                        (/= "obj-blk" obj-typ)
                                                                        (= "1" bor-enc)
                                                                    )
                                                                    (setq bor (create-bor obj prop))
                                                                )
                                                            )
                                                            (if tog-cnt (numinc:increment symb inc-str))
                                                            (progn
                                                                (vla-delete obj)
                                                                (if bor (vla-delete bor))
                                                            )
                                                        )
                                                    )

                                                    (setq obj (create-obj (cadr (grread t 13 0)) (strcat pre-str mid-str suf-str)))
                                                    (if
                                                        (and
                                                            (/= "obj-blk" obj-typ)
                                                            (= "1" bor-enc)
                                                        )
                                                        (setq bor (create-bor obj prop))
                                                    )
                                                    (princ msg)
                                                )
                                                (   (member g2 '(44 46 60 62))  ;; </>
                                                    (if (member g2 '(44 60))
                                                        (setq deg (/ pi  180.0))
                                                        (setq deg (/ pi -180.0))
                                                    )
                                                    (setvar 'modemacro
                                                        (strcat "Rotation: "
                                                            (rtos
                                                                (rem
                                                                    (+ 360.0
                                                                        (* 180.0
                                                                            (/ (setq txt-rot (+ txt-rot deg)) pi)
                                                                        )
                                                                    )
                                                                    360
                                                                )
                                                                2 2
                                                            )
                                                            (chr 186)
                                                        )
                                                    )
                                                    (vla-put-rotation obj (+ (vla-get-rotation obj) deg))
                                                    (if bor
                                                        (vla-rotate bor (vlax-get-property obj prop) deg)
                                                    )
                                                    t
                                                )
                                                (   (member g2 '(79 111))  ;; O/o
                                                    (setq txt-rot
                                                        (cond
                                                            (
                                                                (getangle
                                                                    (strcat "\nSpecify "
                                                                        (cdr
                                                                            (assoc obj-typ
                                                                               '(
                                                                                    ("obj-txt" . "text")
                                                                                    ("obj-mtx" . "mtext")
                                                                                    ("obj-blk" . "block")
                                                                                )
                                                                            )
                                                                        )
                                                                        " rotation <" (angtos txt-rot) ">: "
                                                                    )
                                                                )
                                                            )
                                                            (   txt-rot   )
                                                        )
                                                    )
                                                    (setvar 'modemacro
                                                        (strcat "Rotation: "
                                                            (rtos
                                                                (rem
                                                                    (+ 360.0 (* 180.0 (/ txt-rot pi)))
                                                                    360
                                                                )
                                                                2 2
                                                            )
                                                            (chr 186)
                                                        )
                                                    )
                                                    (if bor
                                                        (vla-rotate bor (vlax-get-property obj prop)
                                                            (- txt-rot
                                                                (if (= "obj-mtx" obj-typ)
                                                                    (vla-get-rotation obj)
                                                                    (- (vla-get-rotation obj) xa)
                                                                )
                                                            )
                                                        )
                                                    )
                                                    (vla-put-rotation obj
                                                        ((lambda ( a ) (if (= "obj-mtx" obj-typ) a (+ a xa))) txt-rot)
                                                    )
                                                    (princ msg)
                                                )
                                                (   (member g2 '(84 116))  ;; T/t
                                                    (if (setq tog-cnt (not tog-cnt))
                                                        (princ "\n<Counter enabled>")
                                                        (princ "\n<Counter disabled>")
                                                    )
                                                    (princ msg)
                                                )
                                                (   (member g2 '(73 105)) ;; I/i

                                                    (vla-delete obj)
                                                    (if bor (vla-delete bor))
                                                 
                                                    (numinc:increment symb inc-str)

                                                    (setq obj (create-obj (cadr (grread t 13 0)) (strcat pre-str mid-str suf-str)))
                                                    (if
                                                        (and
                                                            (/= "obj-blk" obj-typ)
                                                            (= "1" bor-enc)
                                                        )
                                                        (setq bor (create-bor obj prop))
                                                    )
                                                    t
                                                )
                                                (   (member g2 '(66 98))  ;; B/b
                                                    (if
                                                        (and
                                                            (/= "obj-blk" obj-typ)
                                                            (= "1" bor-enc)
                                                            (= "3" bor-shp)
                                                            bor
                                                        )
                                                        (progn
                                                            (setq bor-rot (not bor-rot))
                                                            (vla-rotate bor
                                                                (vlax-3D-point (numinc:PolygonCentroid bor))
                                                                (/ pi bor-sid#)
                                                            )
                                                        )
                                                        (princ (strcat "\nInvalid keypress." msg))
                                                    )
                                                    t
                                                )
                                                (   (= g2 9)  ;; Tab
                                                    (setq txt-rot (rem (+ pi pi txt-rot) (+ pi pi)))
                                                 
                                                    (if
                                                        (vl-some
                                                            (function
                                                                (lambda ( x )
                                                                    (equal txt-rot x 1e-6)
                                                                )
                                                            )
                                                            (list (* pi 0.0) (* pi 0.5) (* pi 1.0) (* pi 1.5))
                                                        )
                                                        (setq txt-rot (rem (+ txt-rot (/ pi 2.0)) (+ pi pi)))
                                                        (setq txt-rot (numinc:roundto txt-rot (/ pi 2.0)))
                                                    )
                                                    (setvar 'modemacro
                                                        (strcat "Rotation: "
                                                            (rtos
                                                                (rem
                                                                    (+ 360.0 (* 180.0 (/ txt-rot pi)))
                                                                    360
                                                                )
                                                                2 2
                                                            )
                                                            (chr 186)
                                                        )
                                                    )
                                                    (if bor
                                                        (vla-rotate bor (vlax-get-property obj prop)
                                                            (- txt-rot
                                                                (if (= "obj-mtx" obj-typ)
                                                                    (vla-get-rotation obj)
                                                                    (- (vla-get-rotation obj) xa)
                                                                )
                                                            )
                                                        )
                                                    )
                                                    (vla-put-rotation obj
                                                        ((lambda ( a ) (if (= "obj-mtx" obj-typ) a (+ a xa))) txt-rot)
                                                    )
                                                    t
                                                )
                                                (   (member g2 '(77 109))  ;; M/m
                                                    (setq txt-rot (rem (+ pi pi (* -1.0 txt-rot)) (+ pi pi)))

                                                    (setvar 'modemacro
                                                        (strcat "Rotation: "
                                                            (rtos
                                                                (rem
                                                                    (+ 360.0 (* 180.0 (/ txt-rot pi)))
                                                                    360
                                                                )
                                                                2 2
                                                            )
                                                            (chr 186)
                                                        )
                                                    )
                                                 
                                                    (if bor
                                                        (vla-rotate bor (vlax-get-property obj prop)
                                                            (- txt-rot
                                                                (if (= "obj-mtx" obj-typ)
                                                                    (vla-get-rotation obj)
                                                                    (- (vla-get-rotation obj) xa)
                                                                )
                                                            )
                                                        )
                                                    )
                                                    (vla-put-rotation obj
                                                        ((lambda ( a ) (if (= "obj-mtx" obj-typ) a (+ a xa))) txt-rot)
                                                    )
                                                    t
                                                )
                                                (   (member g2 '(82 114))  ;; R/r

                                                    (vla-delete obj)
                                                    (if bor (vla-delete bor))

                                                    (while
                                                        (setq ent
                                                            (car
                                                                (numinc:selectif "\nSelect text, mtext or attribute <exit>: "
                                                                    (function
                                                                        (lambda ( x )
                                                                            (member (cdr (assoc 0 (entget x))) '("TEXT" "MTEXT" "ATTRIB"))
                                                                        )
                                                                    )
                                                                    nentsel
                                                                )
                                                            )
                                                        )
                                                        (if
                                                            (= 4
                                                                (logand 4
                                                                    (cdr
                                                                        (assoc 70
                                                                            (tblsearch "layer"
                                                                                (cdr (assoc 8 (entget ent)))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            (princ "\nSelected object is on a locked layer.")
                                                            (progn
                                                                (vla-put-textstring (vlax-ename->vla-object ent) (strcat pre-str mid-str suf-str))
                                                                (if tog-cnt (numinc:increment symb inc-str))
                                                            )
                                                        )
                                                    )

                                                    (setq obj (create-obj (cadr (grread t 13 0)) (strcat pre-str mid-str suf-str)))
                                                    (if
                                                        (and
                                                            (/= "obj-blk" obj-typ)
                                                            (= "1" bor-enc)
                                                        )
                                                        (setq bor (create-bor obj prop))
                                                    )
                                                    (princ msg)
                                                )
                                                (   (member g2 '(65 97))  ;; A/a

                                                    (if (= "obj-mtx" obj-typ)
                                                        (progn
                                                            (vlax-put obj 'backgroundfill
                                                                (setq mtx-bak (~ (vlax-get obj 'backgroundfill)))
                                                            )
                                                            (if (zerop mtx-bak)
                                                                (princ "\n<Background Mask Off>")
                                                                (princ "\n<Background Mask On>")
                                                            )
                                                        )
                                                        (princ "\nInvalid keypress.")
                                                    )
                                                    (princ msg)
                                                )
                                                (   (member g2 '(13 32))  ;; Enter/Space
                                                 
                                                    (vla-delete obj)
                                                    (if bor (vla-delete bor))      
                                                    nil
                                                )
                                                (   (princ (strcat "\nInvalid keypress." msg))   )
                                            )
                                        )
                                        (   t
                                            (vla-delete obj)
                                            (if bor (vla-delete bor))
                                            nil
                                        )
                                    )
                                )
                            )
                        )
                        (   t

                            (setq msg
                                (strcat
                                    "\nPick Point or [C]urve Aligned, [R]eplace, R[o]tation, [T]oggle Count, [I]ncrement\n"
                                    (if
                                        (and
                                            (/= "obj-blk" obj-typ)
                                            (= "1" bor-enc)
                                            (= "3" bor-shp)
                                        )
                                        "Rotate [B]order, " ""
                                    )
                                    "[Ro]tate 90" (chr 186) ", [M]irror Rotation"
                                    (if (= "obj-mtx" obj-typ) ", B[a]ckground Mask" "")
                                    " <Exit>: "
                                )
                            )

                            (setvar 'modemacro
                                (strcat "Rotation: "
                                    (rtos (rem (+ 360.0 (* 180.0 (/ txt-rot pi))) 360) 2 2) (chr 186)
                                )
                            )
                         
                            (while
                                (progn
                                    (initget
                                        (strcat
                                            "Curve Replace rOtation Toggle Increment ROtate"
                                            (if
                                                (and
                                                    (/= "obj-blk" obj-typ)
                                                    (= "1" bor-enc)
                                                    (= "3" bor-shp)
                                                )
                                                " Border" ""
                                            )
                                            " Mirror"
                                            (if (= "obj-mtx" obj-typ) " bAckground" "")
                                        )
                                    )
                                    (setq pt (getpoint msg))
                                    (cond
                                        (   (null pt)
                                            nil
                                        )
                                        (   (listp pt)
                                            (if
                                                (null
                                                    (and table
                                                        (numinc:textincell table
                                                            (vlax-3D-point (trans pt 1 0)) (strcat pre-str mid-str suf-str)
                                                        )
                                                    )
                                                )
                                                (progn
                                                    (setq obj (create-obj pt (strcat pre-str mid-str suf-str)))
                                                    (if
                                                        (and
                                                            (/= "obj-blk" obj-typ)
                                                            (= "1" bor-enc)
                                                        )
                                                        (setq bor (create-bor obj prop))
                                                    )
                                                )
                                            )
                                            (if tog-cnt (numinc:increment symb inc-str))
                                            (princ "\n--------------------")
                                            t
                                        )
                                        (   (= "Curve" pt)
                                                  
                                            (while
                                                (setq ent
                                                    (numinc:selectif "\nSelect curve <exit>: "
                                                        (function
                                                            (lambda ( x )
                                                                (not
                                                                    (vl-catch-all-error-p
                                                                        (vl-catch-all-apply 'vlax-curve-getendparam (list x))
                                                                    )
                                                                )
                                                            )
                                                        )
                                                        entsel
                                                    )
                                                )
                                                (if (numinc:aligntocurve
                                                        (setq obj (create-obj (cadr ent) (strcat pre-str mid-str suf-str))) prop (car ent)
                                                        (if
                                                            (and
                                                                (/= "obj-blk" obj-typ)
                                                                (= "1" bor-enc)
                                                            )
                                                            (setq bor (create-bor obj prop))
                                                        )
                                                    )
                                                    (if tog-cnt (numinc:increment symb inc-str))
                                                    (progn
                                                        (vla-delete obj)
                                                        (if bor (vla-delete bor))
                                                    )
                                                )
                                            )
                                            t
                                        )
                                        (   (= "Replace" pt)

                                            (while
                                                (setq ent
                                                    (car
                                                        (numinc:selectif "\nSelect text, mtext or attribute <exit>: "
                                                            (function
                                                                (lambda ( x )
                                                                    (member (cdr (assoc 0 (entget x))) '("TEXT" "MTEXT" "ATTRIB"))
                                                                )
                                                            )
                                                            nentsel
                                                        )
                                                    )
                                                )
                                                (if
                                                    (= 4
                                                        (logand 4
                                                            (cdr
                                                                (assoc 70
                                                                    (tblsearch "layer"
                                                                        (cdr (assoc 8 (entget ent)))
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                    (princ "\nSelected object is on a locked layer.")
                                                    (progn
                                                        (vla-put-textstring (vlax-ename->vla-object ent) (strcat pre-str mid-str suf-str))
                                                        (if tog-cnt (numinc:increment symb inc-str))
                                                    )
                                                )
                                            )
                                            t
                                        )
                                        (   (= "rOtation" pt)
                                         
                                            (setq txt-rot
                                                (cond
                                                    (
                                                        (getangle
                                                            (strcat "\nSpecify "
                                                                (cdr
                                                                    (assoc obj-typ
                                                                       '(
                                                                            ("obj-txt" . "text")
                                                                            ("obj-mtx" . "mtext")
                                                                            ("obj-blk" . "block")
                                                                        )
                                                                    )
                                                                )
                                                                " rotation <" (angtos txt-rot) ">: "
                                                            )
                                                        )
                                                    )
                                                    (   txt-rot   )
                                                )
                                            )
                                            (setvar 'modemacro
                                                (strcat "Rotation: "
                                                    (rtos
                                                        (rem
                                                            (+ 360.0 (* 180.0 (/ txt-rot pi)))
                                                            360
                                                        )
                                                        2 2
                                                    )
                                                    (chr 186)
                                                )
                                            )
                                            t
                                        )
                                        (   (= "Toggle" pt)
                                            (if (setq tog-cnt (not tog-cnt))
                                                (princ "\n<Counter enabled>")
                                                (princ "\n<Counter disabled>")
                                            )
                                            t
                                        )
                                        (   (= "Increment" pt)
                                            (numinc:increment symb inc-str)
                                            t
                                        )
                                        (   (= "Border" pt)
                                            (princ "\n<Border rotated>")
                                            (setq bor-rot (not bor-rot))
                                            t
                                        )
                                        (   (= "ROtate" pt)
                                            (setq txt-rot (rem (+ pi pi txt-rot) (+ pi pi)))
                                            (if
                                                (vl-some
                                                    (function
                                                        (lambda ( x )
                                                            (equal txt-rot x 1e-6)
                                                        )
                                                    )
                                                    (list (* pi 0.0) (* pi 0.5) (* pi 1.0) (* pi 1.5))
                                                )
                                                (setq txt-rot (rem (+ txt-rot (/ pi 2.0)) (+ pi pi)))
                                                (setq txt-rot (numinc:roundto txt-rot (/ pi 2.0)))
                                            )
                                            (princ
                                                (strcat "\n"
                                                    (setvar 'modemacro
                                                        (strcat "Rotation: "
                                                            (rtos
                                                                (rem
                                                                    (+ 360.0 (* 180.0 (/ txt-rot pi)))
                                                                    360
                                                                )
                                                                2 2
                                                            )
                                                            (chr 186)
                                                        )
                                                    )
                                                )
                                            )
                                            t
                                        )
                                        (   (= "Mirror" pt)
                                            (setq txt-rot (rem (+ pi pi (* -1.0 txt-rot)) (+ pi pi)))
                                            (princ
                                                (strcat "\n"
                                                    (setvar 'modemacro
                                                        (strcat "Rotation: "
                                                            (rtos
                                                                (rem
                                                                    (+ 360.0 (* 180.0 (/ txt-rot pi)))
                                                                    360
                                                                )
                                                                2 2
                                                            )
                                                            (chr 186)
                                                        )
                                                    )
                                                )
                                            )
                                            t
                                        )
                                        (   (= "bAckground" pt)
                                            (if (zerop (setq mtx-bak (~ mtx-bak)))
                                                (princ "\n<Background mask off>")
                                                (princ "\n<Background mask on>")
                                            )
                                            t
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (numinc:writeconfig cfgfname (mapcar 'eval (mapcar 'car symlist)))
                )
                (princ "\n*Cancel*")
            )
        )
    )
    (if (< 0 dclid)
        (setq dclid (unload_dialog dclid))
    )
    (if
        (and
            (= 'vla-object (type numinc:wshobject))
            (not (vlax-object-released-p numinc:wshobject))
        )
        (progn
            (vlax-release-object numinc:wshobject)
            (setq numinc:wshobject nil)
        )
    )
    (mapcar 'setvar varlst vallst)
    (princ)
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:selectif ( msg pred func / ent )
    (setq pred (eval pred))
    (while
        (progn (setvar 'errno 0) (setq ent (func msg))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (= 'ename (type (car ent)))
                    (if (and pred (null (pred (car ent))))
                        (princ "\nInvalid object selected.")
                    )
                )
            )
        )
    )
    ent
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:annotative-p ( sty )
    (and
        (setq sty (tblobjname "style" sty))
        (setq sty (cadr (assoc -3 (entget sty '("AcadAnnotative")))))
        (= 1 (cdr (assoc 1070 (reverse sty))))
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:getblockdata ( / a b c )
    (while (setq a (tblnext "block" (null a)))
        (if
            (and
                (null (wcmatch (cdr (assoc 02 a)) "`**,*|*"))
                (= 2 (logand 2 (cdr (assoc 70 a))))
                (setq c
                    (
                        (lambda ( c / d e )
                            (while (setq c (entnext c))
                                (if (= "ATTDEF" (cdr (assoc 0 (setq d (entget c)))))
                                    (setq e (cons (strcase (cdr (assoc 2 d))) e))
                                )
                            )
                            (vl-sort e '<)
                        )
                        (tblobjname "block" (cdr (assoc 2 a)))
                    )
                )
            )
            (setq b (cons (cons (cdr (assoc 2 a)) c) b))
        )
    )
    (vl-sort b '(lambda ( a b ) (< (car a) (car b))))
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:gettableitems ( table / a b )
    (while (setq a (tblnext table (null a)))
        (if
            (not
                (or (wcmatch (cdr (assoc 2 a)) "`**,*|*")
                    (and (= "layer" (strcase table t))
                         (= 4 (logand 4 (cdr (assoc 70 a))))
                    )
                )
            )
            (setq b (cons (cdr (assoc 2 a)) b))
        )
    )
    (acad_strlsort b)
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:roundto ( a b )
    (* b (fix (/ (+ a (* b (if (minusp a) -0.5 0.5))) b)))
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:about ( id / _dialogtext _displaybitmap i j x y )

    (defun _dialogtext ( key str )
        (set_tile key str)
        (start_image key)
        (vector_image 0 (1- (dimy_tile key)) (dimx_tile key) (1- (dimy_tile key)) 0)
        (end_image)
    )

    (cond
        (   (not (new_dialog "about" id))
            (numinc:popup "About Dialog could not be Loaded" 16
                (princ
                    (strcat
                        "The Incremental Numbering Suite About dialog could not be loaded. "
                        "The DCL file required by the application resides in the following location:\n\n"
                        dclfname
                        "\n\nPlease check the integrity of this file."
                    )
                )
            )
            (princ)
        )
        (   t
            (repeat (setq i 32)
                (setq j 1)
                (repeat 32
                    (setq x (cons j x)
                          y (cons i y)
                          j (1+ j)
                    )
                )
                (setq i (1- i))
            )
            (foreach pair
               '(
                    ;("title1" "Incremental Numbering Suite")
                    ("title2" "Placement Controls")
                    ("title3" "Curve Alignment Controls")
                )
                (apply '_dialogtext pair)
            )

            (setq _displaybitmap
                (eval
                    (list 'lambda '( key lst )
                       '(start_image key)
                       '(fill_image 0 0 (dimx_tile key) (dimy_tile key) -15)
                        (list 'mapcar ''vector_image (quote x) (quote y) (quote x) (quote y) 'lst)
                       '(end_image)
                    )
                )
            )

            (_displaybitmap "info1"
               '(
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15 008 -15 008 -15 008 -15 008 -15 008 -15 008 -15 008 -15 008 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 -15 008 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 008 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 008 -15 008 -15 008 -15 008 -15 008 -15 008 -15 008 -15 008 -15 008 -15 254 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 253 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 253 254 253 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 253 254 -15 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 253 254 254 254 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 253 009 254 009 254 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 253 253 254 009 009 254 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 253 253 254 009 009 009 254 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 253 253 254 254 254 254 009 254 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 253 253 254 254 254 254 254 254 254 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 253 253 254 254 254 254 254 254 254 254 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 252 252 252 252 008 254 254 254 254 254 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 254 009 009 253 253 254 009 252 254 254 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 254 254 252 009 254 252 252 252 009 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 254 252 254 254 252 009 252 252 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 253 253 254 253 009 254 254 252 252 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 008 254 254 252 254 -15 -15 254 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 253 252 252 253 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 254 254 009 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                )
            )

            (_displaybitmap "info2"
               '(
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 251 251 251 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 251 251 251 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 251 251 251 253 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 009 252 008 253 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 254 252 008 253 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 254 252 008 253 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 254 252 008 253 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 254 009 009 151 151 151 151 151 151 009 009 252 251 251 251 008 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 254 009 143 143 153 153 145 145 145 145 145 153 153 143 251 252 254 254 252 008 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 254 143 143 153 145 145 153 153 253 009 009 009 253 253 153 145 251 254 009 009 254 251 -15 -15 -15 -15 -15 -15 -15 -15
                    -15 143 153 135 145 153 253 009 009 254 254 254 254 254 254 254 009 009 251 254 009 009 254 251 254 -15 -15 -15 -15 -15 -15 -15
                    -15 135 135 153 253 009 254 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 251 252 254 254 252 251 253 -15 -15 -15 -15 -15 -15 -15
                    254 135 253 009 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 251 251 251 251 252 008 253 -15 -15 -15 -15 -15 -15
                    -15 254 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 254 253 153 143 009 252 008 253 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 153 153 143 009 252 008 253 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 135 143 254 254 252 008 253 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 145 143 254 254 254 252 251 251 251 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 153 153 009 -15 254 254 251 251 251 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 143 153 143 -15 -15 254 251 251 251 254
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 253 153 143 -15 -15 -15 254 009 254 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 009 135 143 -15 -15 -15 -15 254 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 153 153 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 135 153 254 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 135 153 254 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 135 153 254 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 135 153 254 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 135 153 254 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 135 153 254 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 135 153 254 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 135 135 254 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 254 -15 -15 -15 -15 -15 -15 -15
                    -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                )
            )

            (start_image "title1")
            (fill_image 0 0 (dimx_tile "title1") (dimy_tile "title1") -15)
            (foreach l
               '(
                    (
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 163 163 163 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 176 176 175 -15
                        -15 163 175 176 176 176 166 165 009 -15 -15 -15 -15 175 175 175 -15 -15 -15 -15 -15 165 175 175 163 -15 -15 -15 176 176 175 -15
                        166 176 176 175 175 177 176 176 176 163 -15 -15 -15 175 176 175 -15 -15 -15 -15 253 166 176 176 163 -15 -15 -15 176 176 175 -15
                        166 253 -15 -15 -15 -15 254 175 176 176 253 -15 -15 175 176 175 -15 -15 -15 253 176 176 176 176 163 -15 -15 -15 176 176 175 -15
                        254 -15 -15 -15 -15 -15 -15 -15 175 176 166 -15 -15 175 176 175 -15 -15 165 176 176 165 176 176 163 -15 -15 -15 176 176 175 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 163 176 176 -15 -15 175 176 175 -15 165 176 176 163 -15 176 176 163 -15 -15 -15 176 176 175 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 165 176 166 -15 -15 175 176 177 177 176 176 253 -15 -15 176 176 163 -15 -15 -15 176 176 175 -15
                        177 254 -15 -15 -15 -15 -15 163 176 176 163 -15 -15 175 176 176 176 166 253 -15 -15 -15 176 176 163 -15 -15 -15 176 176 175 -15
                        176 176 175 163 163 163 166 176 176 177 -15 -15 -15 175 176 176 166 254 -15 -15 -15 -15 176 176 163 -15 -15 -15 176 176 175 -15
                        009 175 176 176 176 176 176 166 163 -15 -15 -15 -15 166 176 176 253 -15 -15 -15 -15 254 176 176 175 -15 -15 254 176 176 166 -15
                        -15 -15 -15 009 009 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    )
                    (
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        175 175 175 163 -15 -15 166 175 175 175 175 175 175 175 175 175 -15 -15 -15 253 175 175 175 175 175 175 175 175 175 254 -15 -15
                        176 176 176 163 -15 -15 166 175 175 175 175 175 175 166 176 175 -15 -15 163 176 176 177 175 175 175 175 166 176 176 -15 -15 163
                        176 176 176 163 -15 -15 -15 -15 -15 -15 -15 -15 -15 175 176 175 -15 -15 176 176 163 -15 -15 -15 -15 -15 163 176 176 -15 -15 253
                        177 176 176 163 -15 -15 -15 -15 253 009 009 009 009 177 176 175 -15 -15 166 176 177 253 009 009 009 009 165 176 176 -15 -15 -15
                        254 176 176 163 -15 -15 -15 -15 175 176 176 176 176 176 176 175 -15 -15 254 166 176 176 176 176 176 176 176 176 176 -15 -15 -15
                        -15 176 176 163 -15 -15 -15 -15 253 009 009 009 009 177 176 175 -15 -15 -15 -15 253 176 176 177 009 009 165 176 176 -15 -15 -15
                        -15 176 176 163 -15 -15 -15 -15 -15 -15 -15 -15 -15 175 176 175 -15 -15 -15 254 166 176 166 -15 -15 -15 163 176 176 -15 -15 254
                        -15 176 176 163 -15 009 163 163 163 163 163 163 163 166 176 175 -15 -15 -15 166 176 166 254 -15 -15 -15 163 176 176 -15 -15 175
                        254 176 176 175 -15 009 176 176 176 176 176 176 176 176 176 166 -15 009 166 176 176 165 -15 -15 -15 -15 175 176 176 254 -15 -15
                        -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    )
                    (
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 165 175 175 163 -15 -15 166 175 175 175 175 175 175 175 175 175 -15 -15 253 175 175 175 254 -15 -15 -15 -15 254
                        -15 -15 -15 253 166 176 176 163 -15 -15 166 175 175 175 175 175 175 166 176 175 -15 -15 009 176 176 176 009 -15 -15 -15 -15 009
                        -15 -15 253 176 176 176 176 163 -15 -15 -15 -15 -15 -15 -15 -15 -15 175 176 175 -15 -15 009 176 176 176 166 -15 -15 -15 -15 166
                        -15 165 176 176 165 176 176 163 -15 -15 -15 -15 253 009 009 009 009 177 176 175 -15 -15 009 176 176 177 176 163 -15 -15 163 176
                        165 176 176 163 -15 176 176 163 -15 -15 -15 -15 175 176 176 176 176 176 176 175 -15 -15 009 176 176 253 166 176 009 009 176 176
                        176 176 253 -15 -15 176 176 163 -15 -15 -15 -15 253 009 009 009 009 177 176 175 -15 -15 009 176 176 009 009 176 166 166 176 163
                        166 253 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 -15 -15 -15 -15 175 176 175 -15 -15 009 176 176 009 -15 163 176 176 177 -15
                        254 -15 -15 -15 -15 176 176 163 -15 009 163 163 163 163 163 163 163 166 176 175 -15 -15 009 176 176 009 -15 -15 166 176 254 -15
                        -15 -15 -15 -15 254 176 176 175 -15 009 176 176 176 176 176 176 176 176 176 166 -15 -15 163 176 176 163 -15 -15 009 163 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    )
                    (
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        175 175 254 -15 -15 -15 -15 -15 253 175 175 175 -15 -15 -15 -15 166 175 175 175 175 175 175 175 175 175 163 -15 175 175 175 -15
                        176 176 -15 -15 -15 -15 -15 -15 163 176 176 166 -15 -15 -15 -15 166 175 175 175 176 176 166 175 175 175 163 -15 175 176 175 -15
                        176 176 -15 -15 -15 -15 -15 -15 166 176 176 176 163 -15 -15 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 175 176 175 -15
                        176 176 -15 -15 -15 -15 -15 175 176 176 163 176 166 254 -15 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 175 176 175 -15
                        176 176 -15 -15 -15 -15 009 176 176 165 -15 165 176 175 -15 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 175 176 175 -15
                        176 176 -15 -15 -15 -15 166 176 176 163 163 163 176 176 009 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 175 176 177 177
                        176 176 -15 -15 -15 163 176 176 177 175 175 175 177 176 166 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 175 176 176 176
                        176 176 -15 -15 254 176 176 175 -15 -15 -15 -15 -15 177 176 163 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 175 176 176 166
                        176 176 254 254 177 176 176 165 -15 -15 -15 -15 -15 175 176 166 009 -15 -15 254 176 176 175 -15 -15 -15 -15 -15 166 176 176 253
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    )
                    (
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 253 163 163 163 -15 -15 -15 -15 -15 -15 253 163 163 163 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 009 176 176 163 -15 -15 -15 -15 -15 -15 175 176 176 176 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        175 163 -15 -15 009 176 176 163 -15 -15 -15 -15 -15 177 176 176 176 176 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 163
                        176 163 -15 -15 009 176 176 163 -15 -15 -15 254 177 176 176 176 176 176 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 163
                        176 163 -15 -15 009 176 176 163 -15 -15 254 166 176 176 163 175 176 176 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 163
                        176 163 -15 -15 009 176 176 163 -15 009 166 176 176 253 -15 175 176 176 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 163
                        176 163 -15 -15 009 176 176 163 253 176 176 166 009 -15 -15 175 176 176 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 163
                        176 163 -15 -15 009 176 176 177 176 176 166 254 -15 -15 -15 175 176 176 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 163
                        176 163 -15 -15 009 176 176 176 176 177 254 -15 -15 -15 -15 175 176 176 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 163
                        176 254 -15 -15 009 176 176 176 177 -15 -15 -15 -15 -15 -15 175 176 176 -15 -15 -15 -15 -15 -15 163 163 163 163 163 163 163 175
                        253 -15 -15 -15 163 176 176 176 -15 -15 -15 -15 -15 -15 -15 166 176 176 254 -15 -15 -15 -15 -15 163 176 176 176 176 176 176 176
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    )
                    (
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        175 175 175 175 163 -15 -15 163 175 175 175 -15 -15 -15 -15 -15 253 175 175 175 253 -15 -15 175 175 175 -15 -15 -15 -15 254 175
                        175 175 176 176 163 -15 -15 163 176 176 166 -15 -15 -15 -15 -15 163 176 176 176 009 -15 -15 175 176 175 -15 -15 -15 -15 -15 176
                        -15 -15 176 176 163 -15 -15 163 176 176 176 175 -15 -15 -15 254 176 176 176 176 009 -15 -15 175 176 175 -15 -15 -15 -15 -15 176
                        163 163 176 176 163 -15 -15 163 176 176 177 176 253 -15 -15 177 176 177 176 176 009 -15 -15 175 176 175 -15 -15 -15 -15 -15 176
                        176 176 176 176 163 -15 -15 163 176 176 254 176 166 -15 163 176 166 253 176 176 009 -15 -15 175 176 175 -15 -15 -15 -15 -15 176
                        -15 -15 176 176 163 -15 -15 163 176 176 -15 163 176 177 176 176 009 009 176 176 009 -15 -15 175 176 175 -15 -15 -15 -15 -15 176
                        -15 -15 176 176 163 -15 -15 163 176 176 -15 -15 177 176 176 163 -15 009 176 176 009 -15 -15 175 176 166 -15 -15 -15 -15 009 176
                        163 163 176 176 163 -15 -15 163 176 176 -15 -15 254 176 166 -15 -15 009 176 176 009 -15 -15 253 176 176 166 163 163 165 166 176
                        176 176 176 176 175 -15 -15 175 176 176 254 -15 -15 163 009 -15 -15 163 176 176 163 -15 -15 -15 163 166 176 176 176 176 176 177
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 009 009 009 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    )
                    (
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 009 165 175 175 175 175 175 175 175 175 253 -15 009 177 175 175 175 175 175 175 175 175 163 -15 -15 -15 163 175 175 175 175
                        253 176 176 166 175 175 175 175 177 176 176 009 -15 009 177 175 175 175 175 175 175 176 176 163 -15 -15 175 176 176 175 175 175
                        175 176 177 -15 -15 -15 -15 -15 009 176 176 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 176 176 163 -15 -15 176 176 163 -15 -15 -15
                        165 176 166 253 009 009 009 009 163 176 176 009 -15 -15 -15 -15 163 009 009 009 009 176 176 163 -15 -15 165 176 166 163 163 163
                        -15 175 176 176 176 176 176 176 176 176 176 009 -15 -15 -15 -15 176 176 176 176 176 176 176 163 -15 -15 009 166 176 176 176 176
                        -15 -15 254 166 176 166 253 009 163 176 176 009 -15 -15 -15 -15 163 009 009 009 009 176 176 163 -15 254 176 176 165 -15 -15 -15
                        -15 -15 177 176 176 254 -15 -15 009 176 176 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 176 176 163 -15 009 176 176 253 -15 -15 -15
                        -15 175 176 176 253 -15 -15 -15 009 176 176 009 -15 163 163 163 163 163 163 163 163 176 176 163 -15 -15 166 176 166 163 163 163
                        175 176 176 177 -15 -15 -15 -15 163 176 176 163 -15 163 176 176 176 176 176 176 176 176 176 175 -15 -15 254 165 166 176 176 176
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    )
                    (
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        009 163 175 166 176 176 177 165 254 -15 -15 -15 -15 175 175 175 -15 -15 -15 -15 -15 165 175 175 163 -15 -15 163 175 175 254 -15
                        175 176 176 177 175 175 176 176 176 253 -15 -15 -15 175 176 175 -15 -15 -15 -15 253 166 176 176 163 -15 -15 163 176 176 -15 -15
                        254 165 254 -15 -15 -15 -15 165 176 176 253 -15 -15 175 176 175 -15 -15 -15 253 176 176 176 176 163 -15 -15 163 176 176 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 175 176 177 -15 -15 175 176 175 -15 -15 165 176 176 165 176 176 163 -15 -15 163 176 176 -15 -15
                        175 175 175 175 175 -15 -15 -15 163 176 176 -15 -15 175 176 175 -15 165 176 176 163 -15 176 176 163 -15 -15 163 176 176 -15 -15
                        176 166 175 175 175 -15 -15 -15 165 176 166 -15 -15 175 176 177 177 176 176 253 -15 -15 176 176 163 -15 -15 163 176 176 -15 -15
                        176 163 -15 -15 -15 -15 -15 253 176 176 163 -15 -15 175 176 176 176 166 253 -15 -15 -15 176 176 163 -15 -15 163 176 176 -15 -15
                        176 176 175 163 163 163 177 176 176 177 -15 -15 -15 175 176 176 166 254 -15 -15 -15 -15 176 176 163 -15 -15 163 176 176 -15 -15
                        253 175 176 176 176 176 176 166 163 -15 -15 -15 -15 166 176 176 253 -15 -15 -15 -15 254 176 176 175 -15 -15 175 176 176 254 254
                        -15 -15 -15 254 009 009 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    )
                    (
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 163 165 175 175 175 163 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 165 166 176 176 176 176 176 176 176 176 175 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        175 175 254 -15 -15 -15 -15 175 175 175 -15 -15 009 176 166 163 254 -15 -15 254 163 176 176 163 -15 -15 -15 -15 -15 -15 -15 -15
                        176 176 -15 -15 -15 -15 -15 175 176 175 -15 -15 -15 163 -15 -15 -15 -15 -15 -15 -15 176 176 175 -15 -15 -15 -15 -15 -15 -15 -15
                        176 176 -15 -15 -15 -15 -15 175 176 175 -15 -15 -15 -15 -15 009 163 163 163 175 166 176 176 165 -15 -15 -15 -15 -15 -15 -15 -15
                        176 176 -15 -15 -15 -15 -15 175 176 175 -15 -15 -15 165 176 176 176 176 176 176 176 176 177 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        176 176 -15 -15 -15 -15 -15 175 176 175 -15 -15 165 176 176 176 166 175 175 163 163 009 -15 -15 -15 -15 -15 -15 -15 -15 -15 163
                        176 176 -15 -15 -15 -15 -15 175 176 175 -15 -15 176 176 166 254 -15 -15 -15 -15 -15 -15 254 -15 -15 -15 -15 -15 -15 -15 -15 163
                        176 176 009 -15 -15 -15 -15 166 176 175 -15 -15 166 176 166 -15 -15 -15 -15 -15 -15 009 177 253 -15 -15 -15 -15 -15 -15 -15 163
                        176 176 166 165 163 163 166 176 176 253 -15 -15 253 176 176 166 165 163 163 165 166 176 176 166 -15 -15 -15 -15 -15 -15 -15 175
                        253 177 176 176 176 176 176 166 163 -15 -15 -15 -15 009 175 176 176 176 176 176 176 175 163 254 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 009 009 009 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 254 009 009 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    )
                    (
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 166 175 175 175 175 175 175 175 175 175 -15 -15 166 175 175 175 175 175 175 175 175 175 163 254 175 175 163 -15 -15 163
                        -15 -15 166 175 175 175 175 175 175 166 176 175 -15 -15 166 175 175 175 176 176 166 175 175 175 163 -15 176 176 163 -15 -15 163
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 175 176 175 -15 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 176 176 163 -15 -15 163
                        -15 -15 -15 -15 253 009 009 009 009 177 176 175 -15 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 176 176 163 -15 -15 163
                        -15 -15 -15 -15 175 176 176 176 176 176 176 175 -15 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 176 176 163 -15 -15 163
                        -15 -15 -15 -15 253 009 009 009 009 177 176 175 -15 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 176 176 163 -15 -15 163
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 175 176 175 -15 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 176 176 163 -15 -15 163
                        -15 009 163 163 163 163 163 163 163 166 176 175 -15 -15 -15 -15 -15 -15 176 176 163 -15 -15 -15 -15 -15 176 176 163 -15 -15 254
                        -15 009 176 176 176 176 176 176 176 176 176 166 -15 -15 -15 -15 -15 254 176 176 175 -15 -15 -15 -15 254 176 176 175 -15 -15 -15
                        -15 254 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                    )
                )                
                (mapcar 'vector_image x y x y l)
                (setq x (mapcar '(lambda ( a ) (+ a 32)) x))
            )
            (end_image)
            (start_dialog)
        )
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:fixdir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:getsavepath ( / tmp )
    (cond      
        (   (setq tmp (getvar 'roamablerootprefix))
            (strcat (numinc:fixdir tmp) "\\Support")
        )
        (   (setq tmp (findfile "acad.pat"))
            (numinc:fixdir (vl-filename-directory tmp))
        )
        (   (numinc:fixdir (vl-filename-directory (vl-filename-mktemp))))
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:writedcl ( dcl / file )
    (cond
        (   (findfile dcl))
        (   (setq file (open dcl "w"))
            (foreach line
               '(
                    "//------------=={ Incremental Numbering Suite }==-------------//"
                    "//                                                            //"
                    "//  NumInc.dcl dialog definition file to be used in           //"
                    "//  conjunction with NumInc.lsp                               //"
                    "//------------------------------------------------------------//"
                    "//  Author: Lee Mac, Copyright © 2014 - www.lee-mac.com       //"
                    "//------------------------------------------------------------//"
                    ""
                    "//  --=={ Sub-Assembly Definitions }==--"
                    ""
                    "edit : edit_box"
                    "{"
                    "    edit_width = 5.0;"
                    "    alignment = centered;"
                    "    fixed_width = true;"
                    "}"
                    ""
                    "but1 : button"
                    "{"
                    "    width = 12.0;"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "}"
                    ""
                    "but2 : button"
                    "{"
                    "    width = 19.5;"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "}"
                    ""
                    "but3 : button"
                    "{"
                    "    width = 15.0;"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "    fixed_height = true;"
                    "    height = 2.2;"
                    "}"
                    ""
                    "rad1 : radio_button"
                    "{"
                    "    alignment = centered;"
                    "}"
                    ""
                    "txt1 : text"
                    "{"
                    "    alignment = centered;"
                    "    fixed_width = false;"
                    "}"
                    ""
                    "txt2 : text"
                    "{"
                    "    alignment = centered;"
                    "    fixed_width = true;"
                    "    width = 8.8;"
                    "}"
                    ""
                    "txt3 : text"
                    "{"
                    "    alignment = left;"
                    "    fixed_width = false;"
                    "}"
                    ""
                    "btxt : image"
                    "{"
                    "    fixed_width = true;"
                    "    height = 1.2;"
                    "    fixed_height = true;"
                    "    color = dialog_background;"
                    "}"
                    ""
                    "ctxt : text"
                    "{"
                    "    width = 30;"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "}"
                    ""
                    "pop1 : popup_list"
                    "{"
                    "    width = 31;"
                    "    fixed_width = true;"
                    "}"
                    ""
                    "spc1 : spacer"
                    "{"
                    "    height = 0.1;"
                    "    fixed_height = true;"
                    "    width = 0.1;"
                    "    fixed_width = true;"
                    "}"
                    ""
                    "imgbox : image_button"
                    "{"
                    "    alignment = centered;"
                    "    height = 1.5;"
                    "    aspect_ratio = 1;"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "    color = 1;"
                    "}"
                    ""
                    "img20b : image_button"
                    "{"
                    "    fixed_width = true;"
                    "    fixed_height = true; "
                    "    width = 3.5;"
                    "    aspect_ratio = 1.0; "
                    "}"
                    ""
                    "img32 : image"
                    "{"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "    width = 5.5;"
                    "    aspect_ratio = 1.0;"
                    "}"
                    ""
                    "img320 : image"
                    "{"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "    width = 53.5;"
                    "    aspect_ratio = 0.1;"
                    "}"
                    ""
                    "//pick : button"
                    "//{"
                    "//    label = \">>\";"
                    "//    fixed_width = true;"
                    "//    fixed_height = true;"
                    "//    alignment = left;"
                    "//}"
                    ""
                    "//------------------------------------------------------------//"
                    "//                    Main Dialog Definition                  //"
                    "//------------------------------------------------------------//"
                    ""
                    "numinc : dialog"
                    "{"
                    "    key = \"dcl\";"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        : column"
                    "        {"
                    "            : toggle"
                    "            {"
                    "                alignment = left;"
                    "                key = \"dyn-flg\";"
                    "                label = \"Text Follows Cursor\";"
                    "            }"
                    "        }"
                    "        : column"
                    "        {"
                    "            : text"
                    "            {"
                    "                label = \"Copyright (c) Lee Mac 2014\";"
                    "                key = \"aut\";"
                    "                alignment = right;"
                    "            }"
                    "        }"
                    "    }"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        : column"
                    "        {"
                    "            : boxed_column"
                    "            {"
                    "                label = \"Increment Format\";"
                    "                width = 37;"
                    "                fixed_width = true;"
                    "                : row"
                    "                {"
                    "                    alignment = centered;"
                    "                    fixed_width = true;"
                    "                    : column"
                    "                    {"
                    "                        : edit { key =  \"pre-str\"; }"
                    "                        : txt2 { label = \"Prefix\"; }"
                    "                    }"
                    "                    : column"
                    "                    {"
                    "                        : edit { key =  \"mid-str\"; }"
                    "                        : txt2 { label = \"Middle\"; }"
                    "                    }"
                    "                    : column"
                    "                    {"
                    "                        : edit { key =  \"suf-str\"; }"
                    "                        : txt2 { label = \"Suffix\"; }"
                    "                    }"
                    "                }       "
                    "                spacer;"
                    "                : edit { label = \"Increment: \"; key = \"inc-str\"; }"
                    "                spacer;"
                    "                : boxed_column"
                    "                {"
                    "                    label = \"Sections to Increment\";"
                    "                    : row"
                    "                    {"
                    "                        alignment = centered;"
                    "                        fixed_width = true;"
                    "                        : toggle { label = \"Prefix\"; key = \"inc-pre\"; }"
                    "                        : toggle { label = \"Middle\"; key = \"inc-mid\"; }"
                    "                        : toggle { label = \"Suffix\"; key = \"inc-suf\"; }"
                    "                    }"
                    "                    spacer;"
                    "                }"
                    "                spacer;"
                    "            }"
                    "            : boxed_column"
                    "            {"
                    "                label = \"Border Options\";"
                    "                spacer;"
                    "                : toggle { key = \"bor-enc\"; label = \"Enclose Text with: \"; }"
                    "                : row"
                    "                {"
                    "                    : popup_list { key = \"bor-shp\"; width = 18; fixed_width = true;  }"
                    "                    : column"
                    "                    {"
                    "                        spc1;"
                    "                        : edit_box"
                    "                        {"
                    "                            edit_width = 5;"
                    "                            fixed_width = true;"
                    "                            key = \"bor-sid\";"
                    "                            label = \"Sides:\";"
                    "                        }"
                    "                        spc1;"
                    "                    }"
                    "                }"
                    "                : text { label = \"Layer: \"; key = \"bor-ltx\"; }"
                    "                : popup_list { key = \"bor-lay\"; }"
                    "                spacer;"
                    "                : boxed_column"
                    "                {"
                    "                    label = \"Border Size\";"
                    "                    : row"
                    "                    {"
                    "                        fixed_width = true;"
                    "                        alignment = centered;"
                    "                        : radio_column"
                    "                        {"
                    "                            : radio_button { key = \"bor-off\"; label = \"  Offset:\"; }"
                    "                            : radio_button { key = \"bor-fix\"; label = \"   Fixed:\"; }"
                    "                        }"
                    "                        : column"
                    "                        {"
                    "                            : edit { key = \"off-ed1\" ; }"
                    "                            : edit { key = \"fix-ed1\" ; }"
                    "                        }"
                    "                        : column"
                    "                        {"
                    "                            fixed_width = true;"
                    "                            spacer_1;"
                    "                            : text { label = \" x\"; key = \"fix-txt\"; }"
                    "                        }"
                    "                        : column"
                    "                        {"
                    "                            spacer_1;"
                    "                            : edit { key = \"fix-ed2\"; }"
                    "                        }"
                    "                    }"
                    "                    : row"
                    "                    {"
                    "                        fixed_width = true;"
                    "                        alignment = centered;"
                    "                        : spacer"
                    "                        {"
                    "                            height = 0.1;"
                    "                            fixed_height = true;"
                    "                            width = 11;"
                    "                            fixed_width = true;"
                    "                        }"
                    "                        : but2 { key = \"bor-pik\"; label = \">>\"; }"
                    "                    }"
                    "                    spacer;"
                    "                }"
                    "                spacer;"
                    "            }"
                    "        }"
                    "        : column"
                    "        {"
                    "            alignment = top;"
                    "            : boxed_column"
                    "            {"
                    "                label = \"Object\";"
                    "                width = 37;"
                    "                fixed_width = true;"
                    "                fixed_height = true;"
                    "                : radio_row"
                    "                {"
                    "                    alignment = centered;"
                    "                    fixed_width = true;"
                    "                    : rad1 { label = \"Text\";  key = \"obj-txt\"; }"
                    "                    : rad1 { label = \"MText\"; key = \"obj-mtx\"; }"
                    "                    : rad1 { label = \"Block\"; key = \"obj-blk\"; }"
                    "                }"
                    "                : text { label = \"Block:\"; key = \"blk-txt\"; }"
                    "                : row"
                    "                {"
                    "                    fixed_width = true;"
                    "                    : pop1 { key = \"blk-nme\"; }"
                    "                    //: pick { key = \"blk-pik\"; }"
                    "                    : column"
                    "                    {"
                    "                        spc1;"
                    "                        : img20b { key = \"blk-pik\"; alignment = top; }"
                    "                    }"
                    "                }"
                    "                spacer;"
                    "                : text { label = \"Attribute:\"; key = \"att-txt\"; }"
                    "                : popup_list { key = \"att-nme\"; }"
                    "                spacer;"
                    "                : boxed_column"
                    "                {"
                    "                    label = \"Block Scale\";"
                    "                    : row"
                    "                    {"
                    "                        fixed_width = true; alignment = centered;"
                    "                        : edit { label = \"Scale:\"; key = \"blk-scl\"; }"
                    "                        : column"
                    "                        {"
                    "                            spc1;"
                    "                            : img20b { key = \"scl-pik\"; alignment = top; }"
                    "                        }"
                    "                        //: pick { key = \"scl-pik\"; }"
                    "                    }"
                    "                    spacer;"
                    "                    : toggle { key = \"scl-var\"; label = \"Use System Variable:\"; }"
                    "                    : popup_list { key = \"scl-pop\"; }"
                    "                    spacer;"
                    "                }"
                    "                spacer;"
                    "            }"
                    "            : boxed_column"
                    "            {"
                    "                label = \"Array Options\";"
                    "                width = 37;"
                    "                fixed_width = true;"
                    "                : row"
                    "                {"
                    "                    fixed_width = true;"
                    "                    : toggle { label = \"Create Array\"; key = \"arr-use\"; }"
                    "                    : edit { label = \"Items:\"; key = \"arr-qty\"; }"
                    "                }"
                    "                spacer;"
                    "                : boxed_column"
                    "                {"
                    "                    label = \"Object Rotation\";"
                    "                    : row"
                    "                    {"
                    "                        : radio_button { key = \"arr-aln\"; label = \"Aligned\"; }"
                    "                        : radio_button { key = \"arr-per\"; label = \"Perpendicular\"; }"
                    "                    }"
                    "                    : row"
                    "                    {"
                    "                        fixed_width = true;"
                    "                        : radio_button { key = \"arr-oth\"; label = \"Other:\"; }"
                    "                        : edit { key = \"arr-rot\"; }"
                    "                        : column"
                    "                        {"
                    "                            spc1;"
                    "                            : img20b { key = \"arr-pik\"; alignment = top; }"
                    "                        }"
                    "                        //: pick { key = \"arr-pik\"; }"
                    "                    }"
                    "                    spacer;"
                    "                }"
                    "                spacer;"
                    "            }"
                    "        }"
                    "        : column"
                    "        { "
                    "            : boxed_column"
                    "            {"
                    "                label = \"Formatting\";"
                    "                : text { label = \"Text Layer: \"; key = \"lay-txt\"; }"
                    "                : popup_list { key = \"txt-lay\"; }"
                    "                spacer;"
                    "                : text { label = \"Text Style: \"; key = \"sty-txt\"; }"
                    "                : popup_list { key = \"txt-sty\"; }"
                    "                spacer;"
                    "                : text { label = \"Text Alignment:\"; key = \"aln-txt\"; }"
                    "                : popup_list { key = \"txt-aln\"; }"
                    "                spacer;"
                    "                : boxed_column"
                    "                {"
                    "                    label = \"Text Height\";"
                    "                    : row"
                    "                    {"
                    "                        fixed_width = true;"
                    "                        alignment = centered;"
                    "                        : toggle { key = \"txt-bst\"; label = \"By Style\"; }"
                    "                        : edit { key = \"txt-sze\"; }"
                    "                        : column"
                    "                        {"
                    "                            spc1;"
                    "                            : img20b { key = \"txt-pik\"; alignment = top; }"
                    "                        }"
                    "                        //: pick { key = \"txt-pik\"; }"
                    "                    }"
                    "                    spacer;"
                    "                }"
                    "                spacer;"
                    "                : boxed_column"
                    "                {"
                    "                    label = \"Background Mask\";"
                    "                    width = 37;"
                    "                    fixed_width = true;"
                    "                    fixed_height = true;"
                    "                    : toggle { label = \"Use Background Mask\"; key = \"msk-use\"; }"
                    "                    spacer;"
                    "                    : boxed_column"
                    "                    {"
                    "                        label = \"Mask Offset\";"
                    "                        : row"
                    "                        {"
                    "                            alignment = centered;"
                    "                            fixed_width = true;"
                    "                            : edit { label = \"Offset Factor:\"; key = \"msk-off\"; }"
                    "                            : column"
                    "                            {"
                    "                                spc1;"
                    "                                : img20b { key = \"msk-pik\"; alignment = top; }"
                    "                            }"
                    "                            //: pick { key = \"msk-pik\"; }"
                    "                        }"
                    "                        spacer;"
                    "                    }"
                    "                    : boxed_column"
                    "                    {"
                    "                        label = \"Fill Color\";"
                    "                        : row"
                    "                        {"
                    "                            alignment = centered;"
                    "                            fixed_width = true;"
                    "                            : toggle { key = \"msk-trn\"; label = \"Transparent\"; }"
                    "                            : imgbox { key = \"msk-col\"; }"
                    "                        }"
                    "                        spacer;"
                    "                    }"
                    "                    spacer;"
                    "                }"
                    "                spacer;"
                    "            }"
                    "        }"
                    "    }"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        fixed_width = true;"
                    "        alignment = centered;"
                    "        spacer;"
                    "        : but1 { key = \"about\";  label = \"About\"; }"
                    "        spacer_1;"
                    "        : but3 { key = \"accept\"; label = \"OK\"; is_default = true; }"
                    "        spacer_1;"
                    "        : but1 { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; }"
                    "        spacer;"
                    "    }"
                    "    spacer;"
                    "}"
                    ""
                    "//------------------------------------------------------------//"
                    "//                  'About' Dialog Definition                 //"
                    "//------------------------------------------------------------//"
                    ""
                    "about : dialog"
                    "{"
                    "    label = \"About\";"
                    "    spacer;"
                    "    //: btxt { key   = \"title1\"; alignment = centered; width = 31.5; }"
                    "    : img320 { key = \"title1\"; alignment = centered; }"
                    "    : ctxt { value = \"Designed and Created by Lee Mac 2011\"; }"
                    "    : button"
                    "    {"
                    "        key = \"weblink\";"
                    "        label = \"www.lee-mac.com\";"
                    "        fixed_width = true;"
                    "        alignment = centered;"
                    "        action = \"(startapp \\\"explorer\\\" \\\"http://www.lee-mac.com\\\")\";"
                    "    }"
                    "    : row"
                    "    {"
                    "        fixed_width = true;"
                    "        alignment = left;"
                    "        spacer;"
                    "        : img32 { key = \"info1\"; }"
                    "        : btxt  { width = 21.5; key = \"title2\"; alignment = centered; }"
                    "    }"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        fixed_width = true;"
                    "        alignment = centered;"
                    "        spacer;"
                    "        : column"
                    "        {"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "        }"
                    "        : column"
                    "        {"
                    "            : txt1 { label = \"Enter\" ; }"
                    "            : txt1 { label = \"Click\" ; }"
                    "            : txt1 { label = \"<\"     ; }"
                    "            : txt1 { label = \">\"     ; }"
                    "            : txt1 { label = \"O\"     ; }"
                    "            : txt1 { label = \"Tab\"   ; }"
                    "            : txt1 { label = \"M\"     ; }"
                    "            : txt1 { label = \"C\"     ; }"
                    "            : txt1 { label = \"R\"     ; }"
                    "            : txt1 { label = \"T\"     ; }"
                    "            : txt1 { label = \"I\"     ; }"
                    "            : txt1 { label = \"B\"     ; }"
                    "            : txt1 { label = \"A\"     ; }"
                    "        }"
                    "        : column"
                    "        {"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "        }"
                    "        spacer;"
                    "        : column"
                    "        {"
                    "            : txt3 { label = \"(or Space/Right-Click) Exit Program\"    ; }"
                    "            : txt3 { label = \"Place Object\"                           ; }"
                    "            : txt3 { label = \"Rotate Object Counter-Clockwise\"        ; }"
                    "            : txt3 { label = \"Rotate Object Clockwise\"                ; }"
                    "            : txt3 { label = \"Specify Object Rotation\"                ; }"
                    "            : txt3 { label = \"Rotate Object by 90 Degrees\"            ; }"
                    "            : txt3 { label = \"Mirror Object Rotation\"                 ; }"
                    "            : txt3 { label = \"Align Object to Curve\"                  ; }"
                    "            : txt3 { label = \"Replace Existing Text/Attribute String\" ; }"
                    "            : txt3 { label = \"Toggle Increment Counter\"               ; }"
                    "            : txt3 { label = \"Increment String\"                       ; }"
                    "            : txt3 { label = \"Rotate Polygonal Border\"                ; }"
                    "            : txt3 { label = \"Toggle MText Background Mask\"           ; }"
                    "        }"
                    "    }"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        fixed_width = true;"
                    "        alignment = left;"
                    "        spacer;"
                    "        : img32 { key = \"info2\"; }"
                    "        : btxt  { width = 28; key = \"title3\"; alignment = centered; }"
                    "    }"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        fixed_width = true;"
                    "        alignment = centered;"
                    "        spacer;"
                    "        : column"
                    "        {"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "            : txt1 { label = \"[\"     ; }"
                    "        }"
                    "        : column"
                    "        {"
                    "            : txt1 { label = \"Enter\" ; }"
                    "            : txt1 { label = \"Click\" ; }"
                    "            : txt1 { label = \"+/-\"   ; }"
                    "            : txt1 { label = \"O\"     ; }"
                    "            : txt1 { label = \"P\"     ; }"
                    "            : txt1 { label = \"B\"     ; }"
                    "            : txt1 { label = \"A\"     ; }"
                    "        }"
                    "        : column"
                    "        {"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "            : txt1 { label = \"]\"     ; }"
                    "        }"
                    "        spacer;"
                    "        : column"
                    "        {"
                    "            : txt3 { label = \"(or Space/Right-Click) Exit Alignment\"  ; }"
                    "            : txt3 { label = \"Place Object\"                           ; }"
                    "            : txt3 { label = \"Increase/Decrease Object Offset\"        ; }"
                    "            : txt3 { label = \"Specify Object Offset\"                  ; }"
                    "            : txt3 { label = \"Toggle Object Perpendicularity\"         ; }"
                    "            : txt3 { label = \"Rotate Polygonal Border\"                ; }"
                    "            : txt3 { label = \"Toggle MText Background Mask\"           ; }"
                    "        }"
                    "    }"
                    "    spacer_1;"
                    "    ok_only;"
                    "}"
                    ""
                    "//------------------------------------------------------------//"
                    "//                 End of Dialog Definition                   //"
                    "//------------------------------------------------------------//"
                )
                (write-line line file)
            )
            (setq file (close file))
            (while (not (findfile dcl)))
            dcl
        )
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:misc ( )
    (mapcar 'set_tile (mapcar 'vl-list->string '((100 99 108) (97 117 116)))
        (list
            (strcat
                (vl-list->string 
                   '(
                        073 110 099 114 101 109 101 110
                        116 097 108 032 078 117 109 098
                        101 114 105 110 103 032 083 117
                        105 116 101 032 086
                    )
                )
                numincversion
                (vl-list->string
                   '(
                        032 092 085 043 048 048 065 057
                        032 076 101 101 032 077 097 099
                        032
                    )
                )
                (menucmd "m=$(edtime,0,yyyy)")
            )
            (strcat
                (vl-list->string
                   '(
                        067 111 112 121 114 105 103 104
                        116 032 040 099 041 032 076 101
                        101 032 077 097 099 032
                    )
                )
                (menucmd "m=$(edtime,0,yyyy)")
            )
        )
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:tostring ( arg / dim )
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

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:writeconfig ( name lst / file )
    (if (setq file (open name "w"))
        (progn
            (foreach x lst (write-line (numinc:tostring x) file))
            (setq file (close file))
            t
        )
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:readconfig ( name lst / file line )
    (if
        (and
            (setq name (findfile name))
            (setq file (open name "r"))
        )
        (progn
            (foreach x lst
                (if (setq line (read-line file))
                    (set x (read line))
                )
            )
            (setq file (close file))
            t
        )
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:popup ( title flags msg / err )
    (setq err (vl-catch-all-apply 'vlax-invoke-method (list (numinc:wsh) 'popup msg 0 title flags)))
    (if (null (vl-catch-all-error-p err))
        err
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:wsh nil
    (cond (numinc:wshobject) ((setq numinc:wshobject (vlax-create-object "wscript.shell"))))
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:makelist ( key lst )
    (start_list key)
    (foreach x lst (add_list x))
    (end_list)
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:textincell ( lst pnt str / data dir )
    (setq dir (vlax-3D-point (trans (getvar 'viewdir) 1 0)))
    (if
        (setq data
            (vl-some
                (function
                    (lambda ( table / row col )
                        (if (= :vlax-true (vla-hittest table pnt dir 'row 'col))
                            (list table row col)
                        )
                    )
                )
                lst
            )
        )
        (not (apply 'vla-settext (append data (list str))))
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:increment ( lst inc )
    (foreach sym lst
        (if (distof (eval sym) 2)
            (set sym (numinc:incrementnumba (eval sym) inc))
            (set sym (numinc:incrementalpha (eval sym) (fix (abs (distof inc)))))
        )
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:incrementnumba ( str inc / _rtos _decimalplaces incd maxd num slen strd )

    (defun _rtos ( real prec / dimzin result )
        (setq dimzin (getvar 'dimzin))
        (setvar 'dimzin 0)
        (setq result (rtos real 2 prec))
        (setvar 'dimzin dimzin)
        result
    )

    (defun _decimalplaces ( string / pos )
        (if (setq pos (vl-string-position 46 string))
            (- (strlen string) pos 1)
            0
        )
    )
    
    (setq num (+ (distof str) (distof inc)))

    (if (minusp (distof str))
        (setq str (substr str 2))
    )
    (if (minusp (distof inc))
        (setq inc (substr inc 2))
    )
    (setq incd (_decimalplaces inc)
          strd (_decimalplaces str)
          maxd (max incd strd)
          slen (strlen str)
    )
    (cond
        (   (and (< 0 strd) (< 0 incd))
            (setq slen (+ (- slen strd) maxd))
        )
        (   (and (= 0 strd) (< 0 incd))
            (setq slen (+ incd slen 1))
        )
    )
    (setq str (_rtos num maxd))
    (if (minusp num)
        (setq str (substr str 2))
    )
    (while (< (strlen str) slen)
        (setq str (strcat "0" str))
    )
    (if (minusp num)
        (strcat "-" str)
        str
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:incrementalpha ( str inc / _incrementalpha a )

    (defun _incrementalpha ( a b / c d e )
        (cond
            (   (cond
                    (   (< 47 (setq c (car a)) 58)
                        (setq d 48
                              e 10
                        )
                    )
                    (   (< 64 c 91)
                        (setq d 65
                              e 26
                        )
                    )
                    (   (< 96 c 123)
                        (setq d 97
                              e 26
                        )
                    )
                )
                (setq c (+ (- c d) b)
                      b (/ c e)
                )
                (cons (+ d (rem c e))
                    (if (zerop b)
                        (cdr a)
                        (if (cdr a)
                            (_incrementalpha (cdr  a) b)
                            (_incrementalpha (list d) (if (= 10 e) b (1- b)))
                        )
                    )
                )
            )
            (   (cons c
                    (if (cdr a)
                        (_incrementalpha (cdr a) b)
                        (_incrementalpha (list 65) (1- b))
                    )
                )
            )
        )
    )

    (vl-list->string
        (reverse
            (if (setq a (reverse (vl-string->list str)))
                (_incrementalpha a inc)
                (_incrementalpha '(65) (1- inc))
            )
        )
    )
)

;;-----------------------------------------------------------------------------------------------;;
        
(defun numinc:aligntocurve ( obj prp ent bor / a1 fac fl g1 g2 gr ll msg mtx p1 ur xa )
    
    (setq fac
        (if (= "AcDbBlockReference" (vla-get-objectname obj))
            (progn
                (vla-getboundingbox obj 'll 'ur)
                (/
                    (-
                        (cadr (vlax-safearray->list ur))
                        (cadr (vlax-safearray->list ll))
                    )
                    2.0
                )
            )
            (vla-get-height obj)
        )
    )
    
    (setq msg
        (princ
            (strcat
                "\nClick to Align <Exit>: [+/-] for [O]ffset, [P]erpendicular"
                (if (and bor (= "3" bor-shp)) ", Rotate [B]order" "")
                (if (setq mtx (= "AcDbMText" (vla-get-objectname obj)))
                    ", B[a]ckground Mask"
                    ""
                )
            )
        )
    )

    (setq xa
        (angle '(0.0 0.0 0.0)
            (trans
                (getvar 'ucsxdir) 0
                (trans '(0.0 0.0 1.0) 1 0 t)
            )
        )
    )
    
    (while
        (progn
            (setq gr (grread t 15 0)
                  g1 (car  gr)
                  g2 (cadr gr)
            )
            (cond
                (   (member g1 '(5 3))
                    (setq p1 (vlax-curve-getclosestpointto ent (setq g2 (trans g2 1 0)))
                          a1 (angle p1 g2)
                          p1 (vlax-3D-point (polar p1 a1 (* fac crv-off)))
                          a1 (numinc:makereadable (+ a1 crv-per))
                    )
                    (if bor
                        (vla-move bor (vlax-get-property obj prp) p1)
                    )
                    (vlax-put-property obj prp p1)
                    (if bor
                        (vla-rotate bor p1
                            (- a1
                                (if mtx
                                    (+ (vla-get-rotation obj) xa)
                                    (vla-get-rotation obj)
                                )
                            )
                        )
                    )
                    (vla-put-rotation obj ((lambda ( a ) (if mtx (- a xa) a)) a1))
                    (null (setq fl (= g1 3)))
                )
                (   (= 25 g1)
                    nil
                )
                (   (= 02 g1)
                    (cond
                        (   (member g2 '(80 112))  ;; P/p
                            (setq crv-per (- (/ pi 2.0) crv-per))
                        )
                        (   (member g2 '(45 95))   ;; -
                            (setq crv-off (- crv-off 0.1))
                        )
                        (   (member g2 '(43 61))   ;; +
                            (setq crv-off (+ crv-off 0.1))
                        )
                        (   (member g2 '(13 32))   ;; Enter/Space
                            nil
                        )
                        (   (member g2 '(79 111))  ;; O/o
                            (setq crv-off
                                (/
                                    (cond
                                        (   (getdist (strcat "\nSpecify offset <" (rtos (* fac crv-off)) ">: ")))
                                        (   (* fac crv-off))
                                    )
                                    fac
                                )
                            )
                            (princ msg)
                        )
                        (   (and (member g2 '(65 97)) mtx)  ;; A/a
                            (vlax-put obj 'backgroundfill
                                (setq mtx-bak (~ (vlax-get obj 'backgroundfill)))
                            )
                            (if (zerop mtx-bak)
                                (princ "\n<Background mask off>")
                                (princ "\n<Background mask on>")
                            )
                            (princ msg)
                        )
                        (   (member g2 '(66 98))  ;; B/b
                            (if (and bor (= "3" bor-shp))
                                (progn
                                    (setq bor-rot (not bor-rot))
                                    (vla-rotate bor
                                        (vlax-3D-point (numinc:polygoncentroid bor))
                                        (/ pi bor-sid#)
                                    )
                                )
                                (princ (strcat "\nInvalid keypress." msg))
                            )
                            t
                        )
                        (   (princ (strcat "\nInvalid keypress." msg)))
                    )
                )
            )
        )
    )
    (redraw)
    fl  
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:makereadable ( a )
    (   (lambda ( a )
            (if (and (< (* pi 0.5) a) (<= a (* pi 1.5)))
                (numinc:makereadable (+ a pi))
                a
            )
        )
        (rem (+ a pi pi) (+ pi pi))
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:polygoncentroid ( obj / _group )

    (defun _group ( lst )
        (if lst
            (cons (list (car lst) (cadr lst)) (_group (cddr lst)))
        )
    )

    (
        (lambda ( lst )
            (
                (lambda ( len )
                    (mapcar '/ (apply 'mapcar (cons '+ lst)) (list len len))
                )
                (length lst)
            )              
        )
        (_group (vlax-get obj 'coordinates))
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun numinc:createtextborder ( ent typ off fx1 fx2 sid / cen enx i inc lst mat pts rad rot vec )
    (setq enx (entget ent))
    (cond
        (   (setq lst (numinc:gettextbox enx off))
            (setq cen (mapcar '(lambda ( a b ) (/ (+ a b) 2.0)) (car lst) (caddr lst))
                  rot (if (= "MTEXT" (cdr (assoc 0 enx)))
                          (angle '(0. 0. 0.) (trans (cdr (assoc 11 enx)) 0 (cdr (assoc 210 enx))))
                          (cdr (assoc 50 enx))
                      )
            )
            (cond
                (   (= "0" typ) ;; Circle
                    (entmakex
                        (list
                           '(0 . "CIRCLE")
                            (cons  010 cen)
                            (cons  040 (cond ( fx1 ) ((distance cen (car lst)))))
                            (assoc 210 enx)
                        )
                    )
                )
                (   (member typ '("1" "2"))  ;; Rectangle / Slot
                    (if (and fx1 fx2)
                        (progn
                            (setq fx1 (/ fx1 2.0)
                                  fx2 (/ fx2 2.0)
                            )
                            (setq mat
                                (list
                                    (list (cos rot) (- (sin rot)) 0.0)
                                    (list (sin rot)    (cos rot)  0.0)
                                    (list   0.0           0.0     1.0)
                                )
                            )
                            (setq vec (mapcar '- cen (mxv mat cen)))
                            (setq lst
                                (list
                                    (list (- (car cen) fx1) (- (cadr cen) fx2))
                                    (list (+ (car cen) fx1) (- (cadr cen) fx2))
                                    (list (+ (car cen) fx1) (+ (cadr cen) fx2))
                                    (list (- (car cen) fx1) (+ (cadr cen) fx2))
                                )
                            )
                            (entmakex
                                (append
                                   '(
                                        (000 . "LWPOLYLINE")
                                        (100 . "AcDbEntity")
                                        (100 . "AcDbPolyline")
                                        (090 . 4)
                                        (070 . 1)
                                    )
                                    (list (cons 38 (caddar lst)))
                                    (apply 'append
                                        (mapcar
                                            (function
                                                (lambda ( a b )
                                                    (list (cons 10 (mapcar '+ (mxv mat a) vec)) (cons 42 b))
                                                )
                                            )
                                            lst (if (= "1" typ) '(0.0 0.0 0.0 0.0) '(0.0 1.0 0.0 1.0))
                                        )
                                    )
                                    (list (assoc 210 enx))
                                )
                            )
                        )
                        (entmakex
                            (append
                               '(
                                    (000 . "LWPOLYLINE")
                                    (100 . "AcDbEntity")
                                    (100 . "AcDbPolyline")
                                    (090 . 4)
                                    (070 . 1)
                                )
                                (list (cons 38 (caddar lst)))
                                (apply 'append
                                    (mapcar
                                        (function
                                            (lambda ( a b ) (list (cons 10 a) (cons 42 b)))
                                        )
                                        lst (if (= "1" typ) '(0.0 0.0 0.0 0.0) '(0.0 1.0 0.0 1.0))
                                    )
                                )
                                (list (assoc 210 enx))
                            )
                        )
                    )
                )
                (   t ;; Polygon
                    (setq inc (/ (+ pi pi) sid)
                          rad (cond ( fx1 ) ((/ (distance cen (car lst)) (cos (/ inc 2.0)))))
                            i -1
                    )
                    (if (= 1 (logand 1 sid)) (setq rot (+ rot (/ pi 2.))))
                    (repeat sid
                        (setq pts
                            (cons
                                (cons 10
                                    (polar cen (+ rot (* (setq i (1+ i)) inc)) rad)
                                )
                                pts
                            )
                        )
                    )
                    (entmakex
                        (append
                            (list
                               '(000 . "LWPOLYLINE")
                               '(100 . "AcDbEntity")
                               '(100 . "AcDbPolyline")
                                (cons 90 (length pts))
                               '(070 . 1)
                            )
                            (list (cons 38 (caddar lst)))
                            (reverse pts)
                            (list (assoc 210 enx))
                        )
                    )
                )
            )
        )
    )
)

;;-----------------------------------------------------------------------------------------------;;

;; The following function is based on code by gile
 
(defun numinc:gettextbox ( enx off / b h j l m n o p r w )
    (if
        (setq l
            (cond
                (   (= "TEXT" (cdr (assoc 0 enx)))
                    (setq b (cdr (assoc 10 enx))
                          r (cdr (assoc 50 enx))
                          l (textbox enx)
                    )
                    (list
                        (list (- (caar  l) off) (- (cadar  l) off))
                        (list (+ (caadr l) off) (- (cadar  l) off))
                        (list (+ (caadr l) off) (+ (cadadr l) off))
                        (list (- (caar  l) off) (+ (cadadr l) off))
                    )
                )
                (   (= "MTEXT" (cdr (assoc 0 enx)))
                    (setq n (cdr (assoc 210 enx))
                          b (trans  (cdr (assoc 10 enx)) 0 n)
                          r (angle '(0.0 0.0 0.0) (trans (cdr (assoc 11 enx)) 0 n))
                          w (cdr (assoc 42 enx))
                          h (cdr (assoc 43 enx))
                          j (cdr (assoc 71 enx))
                          o (list
                                (cond
                                    ((member j '(2 5 8)) (/ w -2.0))
                                    ((member j '(3 6 9)) (- w))
                                    (0.0)
                                )
                                (cond
                                    ((member j '(1 2 3)) (- h))
                                    ((member j '(4 5 6)) (/ h -2.0))
                                    (0.0)
                                )
                            )
                    )
                    (list
                        (list (- (car o)   off) (- (cadr o)   off))
                        (list (+ (car o) w off) (- (cadr o)   off))
                        (list (+ (car o) w off) (+ (cadr o) h off))
                        (list (- (car o)   off) (+ (cadr o) h off))
                    )
                )
            )
        )
        (   (lambda ( m ) (mapcar '(lambda ( p ) (mapcar '+ (mxv m p) b)) l))
            (list
                (list (cos r) (sin (- r)) 0.0)
                (list (sin r) (cos r)     0.0)
               '(0.0 0.0 1.0)
            )
        )
    )
)

;;-----------------------------------------------------------------------------------------------;;

;; Matrix x Vector - Vladimir Nesterovsky

(defun mxv ( m v )
    (mapcar (function (lambda ( r ) (apply '+ (mapcar '* r v)))) m)
)

;;-----------------------------------------------------------------------------------------------;;

(vl-load-com)
(princ)
(princ
    (strcat
        "\n:: NumInc.lsp | Version "
        numincversion
        " | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"NumInc\" to Invoke ::"
    )
)
(princ)

;;-----------------------------------------------------------------------------------------------;;
;;                                          End of File                                          ;;
;;-----------------------------------------------------------------------------------------------;;