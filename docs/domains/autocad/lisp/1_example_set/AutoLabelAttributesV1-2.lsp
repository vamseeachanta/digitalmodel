;;---------------------=={ AutoLabel Attributes }==---------------------;;
;;                                                                      ;;
;;  This program will automatically populate a specific attribute tag   ;;
;;  with a unique label within a set of attributed blocks, renumbering  ;;
;;  if blocks are added, copied or erased.                              ;;
;;                                                                      ;;
;;  The program uses an Object Reactor to monitor modification events   ;;
;;  for the set of all attributed blocks with a block name matching     ;;
;;  a block name or wildcard pattern specified within the program       ;;
;;  source code.                                                        ;;
;;                                                                      ;;
;;  Following modification to any matching attributed block, a Command  ;;
;;  Reactor will trigger the program to automatically renumber a        ;;
;;  specific attribute tag held by all matching attributed blocks in    ;;
;;  the active layout of the drawing.                                   ;;
;;                                                                      ;;
;;  The block references are numbered in the order in which they are    ;;
;;  encountered in the drawing database of the active drawing           ;;
;;  (that is, the order in which the blocks were created).              ;;
;;                                                                      ;;
;;  The program also allows the user to specify a numbering prefix &    ;;
;;  suffix, the starting number for the numbering, and the number of    ;;
;;  characters to be used for fixed length numbering with leading zeros ;;
;;  (i.e. if the numbering length is set to 2, the program will number  ;;
;;  the blocks 01,02,03,...,10,11,12).                                  ;;
;;                                                                      ;;
;;  The autonumbering functionality is automatically enabled on drawing ;;
;;  startup when the program is loaded, and may be subsequently enabled ;;
;;  or disabled manually using the commands 'AUTOLABELON' &             ;;
;;  'AUTOLABELOFF' respectively.                                        ;;
;;----------------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com                 ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2011-09-14                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2015-09-20                                      ;;
;;                                                                      ;;
;;  - Program entirely rewritten.                                       ;;
;;  - Added callback function to handle command cancelled & command     ;;
;;    failed events when modifying autonumbered blocks.                 ;;
;;  - Added the ability to specify a numbering prefix & suffix,         ;;
;;    specify a starting number, and use fixed length numbering         ;;
;;    (i.e numbering with leading zeros: 01,02,...,10).                 ;;
;;  - Block Name & Attribute Tag parameters may now use wildcards to    ;;
;;    match multiple block names & tags (the first attribute tag which  ;;
;;    matches the wildcard pattern will be numbered).                   ;;
;;  - Incorporated compatibility for Multiline Attributes.              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2015-09-27                                      ;;
;;                                                                      ;;
;;  - Program modified to only increment numbering counter if an        ;;
;;    attribute matching the target tag name is found.                  ;;
;;  - Implemented compatibility for multileader blocks.                 ;;
;;----------------------------------------------------------------------;;

(setq

;;----------------------------------------------------------------------;;
;;                               Settings                               ;;
;;----------------------------------------------------------------------;;

    autolabel:blockname "myblock"  ;; Name of block to be updated (not case-sensitive / may use wildcards)
    autolabel:blocktag  "mytag"    ;; Attribute tag to be updated (not case-sensitive / may use wildcards)
    autolabel:prefix    ""         ;; Numbering prefix (use "" for none)
    autolabel:suffix    ""         ;; Numbering suffix (use "" for none)
    autolabel:start     1          ;; Starting number
    autolabel:length    2          ;; Fixed length numbering (set to zero if not required)
    autolabel:startup   t          ;; Enable on drawing startup (t=enable / nil=disable)
    autolabel:objtype   3          ;; Bit-coded integer > 0 (1=attributed blocks; 2=multileader blocks)

;;----------------------------------------------------------------------;;

)

;;----------------------------------------------------------------------;;
;;                             Main Program                             ;;
;;----------------------------------------------------------------------;;

(defun autolabel:objectreactorcallback:renumberblocks ( own rtr arg )
    (if (null autolabel:commandreactor)
        (setq autolabel:commandreactor
            (vlr-command-reactor "autolabel"
               '(
                    (:vlr-commandended     . autolabel:commandreactorcallback:renumberblocks)
                    (:vlr-commandcancelled . autolabel:commandreactorcallback:cancelled)
                    (:vlr-commandfailed    . autolabel:commandreactorcallback:cancelled)
                )
            )
        )
    )
    (princ)
)

(defun autolabel:commandreactorcallback:cancelled ( rtr arg )
    (if (= 'vlr-command-reactor (type autolabel:commandreactor))
        (progn
            (vlr-remove autolabel:commandreactor)
            (setq autolabel:commandreactor nil)
        )
    )
    (princ)
)

(defun autolabel:commandreactorcallback:renumberblocks ( rtr arg / att blk idx num obj oid sel )
    (if (= 'vlr-command-reactor (type autolabel:commandreactor))
        (progn
            (vlr-remove autolabel:commandreactor)
            (setq autolabel:commandreactor nil)
        )
    )
    (if (= 'vlr-object-reactor (type autolabel:objectreactor))
        (vlr-remove autolabel:objectreactor)
    )
    (if
        (and (not autolabel:undoflag)
            (setq sel
                (ssget "_X"
                    (append
                        (if (= 3 (logand 3 autolabel:objtype))
                           '((-4 . "<OR"))
                        )
                        (if (= 1 (logand 1 autolabel:objtype))
                            (list '(-4 . "<AND") '(0 . "INSERT") '(66 . 1) (cons 2 (strcat "`*U*," autolabel:blockname)) '(-4 . "AND>"))
                        )
                        (if (= 2 (logand 2 autolabel:objtype))
                           '((0 . "MULTILEADER"))
                        )
                        (if (= 3 (logand 3 autolabel:objtype))
                           '((-4 . "OR>"))
                        )
                        (if (= 1 (getvar 'cvport))
                            (list (cons 410 (getvar 'ctab)))
                           '((410 . "Model"))
                        )
                    )
                )
            )
        )
        (progn
            (setq num autolabel:start)
            (repeat (setq idx (sslength sel))
                (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
                (if (= "AcDbBlockReference" (vla-get-objectname obj))
                    (if (setq att (autolabel:getattribute obj))
                        (progn
                            (vla-put-textstring att
                                (strcat
                                    autolabel:prefix
                                    (autolabel:padzeros (itoa num) autolabel:length)
                                    autolabel:suffix
                                )
                            )
                            (setq num (1+ num))
                            (autolabel:addowner obj)
                        )
                    )
                    (if (and (= acblockcontent (vla-get-contenttype obj))
                             (wcmatch (setq blk (strcase (vla-get-contentblockname obj))) autolabel:blockname)
                             (setq oid (autolabel:getattributetagid blk))
                        )
                        (progn
                            (autolabel:setblockattributevalue obj oid
                                (strcat
                                    autolabel:prefix
                                    (autolabel:padzeros (itoa num) autolabel:length)
                                    autolabel:suffix
                                )
                            )
                            (setq num (1+ num))
                            (autolabel:addowner obj)
                        )
                    )
                )
            )
        )
    )
    (if (= 'vlr-object-reactor (type autolabel:objectreactor))
        (vlr-add autolabel:objectreactor)
    )
    (princ)
)

(defun autolabel:commandreactorcallback:undocheck ( rtr arg )
    (setq autolabel:undoflag (= (strcase (car arg) t) "undo"))
    (princ)
)

(defun autolabel:commandreactorcallback:blockinserted ( rtr arg / att blk ent enx idx new num obj oid sel )
    (if
        (and
            (not autolabel:undoflag)
            (wcmatch (strcase (car arg) t)
                (strcat
                    (if (= 1 (logand 1 autolabel:objtype)) "-insert,insert,executetool" "")
                    (if (= 3 (logand 3 autolabel:objtype)) "," "")
                    (if (= 2 (logand 2 autolabel:objtype)) "mleader" "")
                )
            )
            (setq ent (entlast))
            (setq new (vlax-ename->vla-object ent))
            (setq enx (entget ent))
            (or
                (and
                    (= 1 (logand 1 autolabel:objtype))
                    (= "INSERT" (cdr (assoc 0 enx)))
                    (= 1 (cdr (assoc 66 enx)))
                    (wcmatch (autolabel:effectivename new) autolabel:blockname)
                )
                (and
                    (= 2 (logand 2 autolabel:objtype))
                    (= "MULTILEADER" (cdr (assoc 0 enx)))
                    (= acblockcontent (vla-get-contenttype new))
                    (wcmatch (strcase (vla-get-contentblockname new)) autolabel:blockname)
                )
            )
            (setq sel
                (ssget "_X"
                    (append
                        (if (= 3 (logand 3 autolabel:objtype))
                           '((-4 . "<OR"))
                        )
                        (if (= 1 (logand 1 autolabel:objtype))
                            (list '(-4 . "<AND") '(0 . "INSERT") '(66 . 1) (cons 2 (strcat "`*U*," autolabel:blockname)) '(-4 . "AND>"))
                        )
                        (if (= 2 (logand 2 autolabel:objtype))
                           '((0 . "MULTILEADER"))
                        )
                        (if (= 3 (logand 3 autolabel:objtype))
                           '((-4 . "OR>"))
                        )
                        (if (= 1 (getvar 'cvport))
                            (list (cons 410 (getvar 'ctab)))
                           '((410 . "Model"))
                        )
                    )
                )
            )
        )
        (progn
            (setq num (1- autolabel:start))
            (repeat (setq idx (sslength sel))
                (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
                (if (= "AcDbBlockReference" (vla-get-objectname obj))
                    (if (autolabel:getattribute obj)
                        (setq num (1+ num))
                    )
                    (if (and (= acblockcontent (vla-get-contenttype obj))
                             (wcmatch (setq blk (strcase (vla-get-contentblockname obj))) autolabel:blockname)
                             (autolabel:getattributetagid blk)
                        )
                        (setq num (1+ num))
                    )
                )
            )
            (if (= "AcDbBlockReference" (vla-get-objectname new))
                (if (setq att (autolabel:getattribute new))
                    (progn
                        (vla-put-textstring att
                            (strcat
                                autolabel:prefix
                                (autolabel:padzeros (itoa num) autolabel:length)
                                autolabel:suffix
                            )
                        )
                        (autolabel:addowner new)
                    )
                )
                (if (setq oid (autolabel:getattributetagid (vla-get-contentblockname new)))
                    (progn
                        (autolabel:setblockattributevalue new oid 
                            (strcat
                                autolabel:prefix
                                (autolabel:padzeros (itoa num) autolabel:length)
                                autolabel:suffix
                            )
                        )
                        (autolabel:addowner new)
                    )
                )
            )
        )
    )
    (princ)
)

(defun autolabel:addowner ( obj )
    (if
        (and
            (= 'vlr-object-reactor (type autolabel:objectreactor))
            (not (member obj (vlr-owners autolabel:objectreactor)))
        )
        (vlr-owner-add autolabel:objectreactor obj)
    )
)

(defun autolabel:getattribute ( blk )
    (if (wcmatch (strcase (autolabel:effectivename obj)) autolabel:blockname)
        (vl-some
           '(lambda ( att )
                (if (wcmatch (strcase (vla-get-tagstring att)) autolabel:blocktag) att)
            )
            (vlax-invoke blk 'getattributes)
        )
    )
)

(defun autolabel:getattributetagid ( blk )
    (eval
        (list 'defun 'autolabel:getattributetagid '( blk / itm tmp )
            (list 'cond
               '(   (setq itm (assoc (strcase blk) autolabel:attributetagids))
                    (cdar (vl-member-if '(lambda ( att ) (wcmatch (car att) autolabel:blocktag)) (cdr itm)))
                )
                (list
                    (list 'vlax-for 'obj (list 'vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) 'blk)
                       '(if
                            (and
                                (= "AcDbAttributeDefinition" (vla-get-objectname obj))
                                (= :vlax-false (vla-get-constant obj))
                            )
                            (setq tmp
                                (cons
                                    (cons
                                        (strcase (vla-get-tagstring obj))
                                        (autolabel:objectid obj)
                                    )
                                    tmp
                                )
                        	)
                        )
                    )
                   '(setq autolabel:attributetagids (cons (cons (strcase blk) tmp) autolabel:attributetagids))
                   '(autolabel:getattributetagid blk)
                )
            )
        )
    )
    (autolabel:getattributetagid blk)
)

(defun autolabel:setblockattributevalue ( obj idx str )
    (if (vlax-method-applicable-p obj 'setblockattributevalue32)
        (defun autolabel:setblockattributevalue ( obj idx str ) (vla-setblockattributevalue32 obj idx str))
        (defun autolabel:setblockattributevalue ( obj idx str ) (vla-setblockattributevalue   obj idx str))
    )
    (autolabel:setblockattributevalue obj idx str)
)

(defun autolabel:objectid ( obj )
    (if (vlax-property-available-p obj 'objectid32)
        (defun autolabel:objectid ( obj ) (vla-get-objectid32 obj))
        (defun autolabel:objectid ( obj ) (vla-get-objectid   obj))
    )
    (autolabel:objectid obj)
)

(defun autolabel:effectivename ( obj )
    (if (vlax-property-available-p obj 'effectivename)
        (defun autolabel:effectivename ( obj ) (strcase (vla-get-effectivename obj)))
        (defun autolabel:effectivename ( obj ) (strcase (vla-get-name obj)))
    )
    (autolabel:effectivename obj)
)

(defun autolabel:padzeros ( str len )
    (if (< (strlen str) len)
        (autolabel:padzeros (strcat "0" str) len)
        str
    )
)

(defun autolabel:disable ( key )
    (foreach grp (vlr-reactors :vlr-command-reactor :vlr-object-reactor)
        (foreach obj (cdr grp)
            (if (= key (vlr-data obj)) (vlr-remove obj))
        )
    )
    (setq autolabel:undoflag       nil
          autolabel:objectreactor  nil
          autolabel:commandreactor nil
    )
)

(defun autolabel:enable ( key )
    (autolabel:disable key)
    (vlr-set-notification
        (setq autolabel:objectreactor
            (vlr-object-reactor nil key
               '(
                    (:vlr-erased   . autolabel:objectreactorcallback:renumberblocks)
                    (:vlr-copied   . autolabel:objectreactorcallback:renumberblocks)
                    (:vlr-unerased . autolabel:objectreactorcallback:renumberblocks)
                )
            )
        )
        'active-document-only
    )
    (vlr-set-notification
        (vlr-command-reactor key
           '(
                (:vlr-commandwillstart . autolabel:commandreactorcallback:undocheck)
                (:vlr-commandended     . autolabel:commandreactorcallback:blockinserted)
            )
        )
        'active-document-only
    )
    (autolabel:commandreactorcallback:renumberblocks nil nil)
    (princ
        (strcat
            "\nAutonumbering enabled for tags matching \""
            autolabel:blocktag
            "\" within "
            (if (= 1 (logand 1 autolabel:objtype)) "blocks" "")
            (if (= 3 (logand 3 autolabel:objtype)) " & " "")
            (if (= 2 (logand 2 autolabel:objtype)) "multileaders" "")
            " matching \""
            autolabel:blockname
            "\"."
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;
;;                         Loading Expressions                          ;;
;;----------------------------------------------------------------------;;

(   (lambda nil
        (vl-load-com)
        (cond
            (   (vl-some
                    (function
                        (lambda ( val par )
                            (if (/= 'str (type val))
                                (princ (strcat "\nThe " par " parameter must be a valid string."))
                            )
                        )
                    )
                    (list
                        autolabel:blockname
                        autolabel:blocktag
                        autolabel:prefix
                        autolabel:suffix
                    )
                   '(
                        "Block Name"
                        "Attribute Tag"
                        "Numbering Prefix"
                        "Numbering Suffix"
                    )
                )
            )
            (   (/= 'int (type autolabel:start))
                (princ "\nThe Starting Number parameter must hold an integer value.")
            )
            (   (/= 'int (type autolabel:length))
                (princ "\nThe Fixed Length Numbering parameter must hold an integer value.")
            )
            (   (not
                    (and
                        (= 'int (type autolabel:objtype))
                        (< 0 autolabel:objtype)
                        (< 0 (logand 3 autolabel:objtype))
                    )
                )
                (princ "\nThe Object Type parameter must hold a bit-coded integer value between 1 & 3.")
            )
            (   (setq autolabel:blockname (strcase autolabel:blockname)
                      autolabel:blocktag  (strcase autolabel:blocktag)
                )
                (defun c:autolabelon nil
                    (autolabel:enable "autolabel")
                )
                (defun c:autolabeloff nil
                    (autolabel:disable "autolabel")
                    (princ "\nAutonumbering disabled.")
                    (princ)
                )
                (if autolabel:startup (autolabel:enable "autolabel"))
            )
        )
        (princ)
    )
)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;