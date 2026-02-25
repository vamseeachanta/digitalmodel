;;------------------------=={ Mask }==------------------------;;
;;                                                            ;;
;;  Enables the user to modify all properties of the          ;;
;;  background mask for a selection of multiple MText and     ;;
;;  MLeader objects simultaneously.                           ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    16-04-2012                            ;;
;;                                                            ;;
;;  First Release.                                            ;;
;;------------------------------------------------------------;;
;;  Version 1.1    -    06-02-2013                            ;;
;;                                                            ;;
;;  Changed command syntax to 'bmask' since 'mask' is an      ;;
;;  existing command in AutoCAD Civil 3D.                     ;;
;;------------------------------------------------------------;;

(defun c:bmask

    (
        /
        *error*
        _ssget
        _imgbox
        _substonce
        _function1
        _function2
        _function3
        _mleadfunction
        _mtextfunction

        dcf dch def dis
        enx
        fds
        inc
        sel
        tmp
    )

;;------------------------------------------------------------;;
;;  First-time default settings                               ;;
;;------------------------------------------------------------;;

    (setq def
       '(
            (maskon "0" )       ;; Use Background Mask
            (offset  1.5)       ;; Mask Offset
            (transp "0" )       ;; Transparent Mask
            (mcolor ((62 . 1))) ;; Mask Colour
        )
    )

;;------------------------------------------------------------;;

    (defun *error* ( msg )
        (if (< 0 dch)
            (setq dch (unload_dialog dch))
        )
        (if (= 'file (type fds))
            (setq fds (close fds))
        )
        (if (and tmp (findfile tmp))
            (vl-file-delete tmp)
        )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

;;------------------------------------------------------------;;

    (defun _ssget ( msg params / sel )
        (princ msg)
        (setvar 'nomutt 1)
        (setq sel (vl-catch-all-apply 'ssget params))
        (setvar 'nomutt 0)
        (if (and sel (null (vl-catch-all-error-p sel)))
            sel
        )
    )

;;------------------------------------------------------------;;

    (defun _imgbox ( key aci )
        (start_image key)
        (fill_image 0 0 (dimx_tile key) (dimy_tile key) aci)
        (end_image)
    )

;;------------------------------------------------------------;;

    (defun _substonce ( a b l )
        (if l
            (if (equal b (car l))
                (cons a (cdr l))
                (cons (car l) (_substonce a b (cdr l)))
            )
        )
    )

;;------------------------------------------------------------;;

    (cond
        (   (null
                (setq sel
                    (_ssget "\nSelect MText or MLeaders: "
                       '(
                            "_:L" ((0 . "MTEXT,MULTILEADER"))
                        )
                    )
                )
            )
        )
        (   (null
                (and
                    (setq tmp (vl-filename-mktemp nil nil ".dcl"))
                    (setq fds (open tmp "w"))
                    (progn
                        (foreach line
                           '(
                                "imgbox : image_button"
                                "{"
                                "    alignment = centered;"
                                "    height = 1.5;"
                                "    aspect_ratio = 1;"
                                "    fixed_width = true;"
                                "    fixed_height = true;"
                                "    color = 1;"
                                "}"
                                "mask : dialog"
                                "{"
                                "    label = \"Background Mask\";"
                                "    spacer;"
                                "    : toggle"
                                "    {"
                                "        label = \"Use Background Mask\";"
                                "        key = \"use\";"
                                "    }"
                                "    spacer;"
                                "    : boxed_column"
                                "    {"
                                "        label = \"Mask Offset\";"
                                "        : row"
                                "        {"
                                "            alignment = centered;"
                                "            : edit_box"
                                "            {"
                                "                key = \"off\";"
                                "            }"
                                "            : button"
                                "            {"
                                "                label = \">>\";"
                                "                key = \"pick\";"
                                "                fixed_width = true;"
                                "            }"
                                "        }"
                                "        spacer;"
                                "    }"
                                "    spacer;"
                                "    : boxed_column"
                                "    {"
                                "        label = \"Fill Color\";"
                                ""
                                "        : row"
                                "        {"
                                "            alignment = centered;"
                                "            fixed_width = true;"
                                "            : toggle"
                                "            {"
                                "                key = \"trans\";"
                                "                label = \"Transparent\";"
                                "            }"
                                "            : imgbox"
                                "            {"
                                "                key = \"col\";"
                                "            }"
                                "        }"
                                "        spacer;"
                                "    }"
                                "    spacer;"
                                "    ok_cancel;"
                                "}"
                            )
                            (write-line line fds)
                        )
                        (setq fds (close fds))
                        (< 0 (setq dch (load_dialog tmp)))
                    )
                )
            )
            (princ "\nUnable to Load Dialog.")
        )
        (   t
            (foreach pair def
                (if (not (boundp (car pair)))
                    (apply 'set pair)
                )
            )                    
         
            (while (not (member dcf '(0 1)))
                (cond
                    (   (null (new_dialog "mask" dch))
                        (princ "\nCannot find Mask Dialog Definition.")
                        (setq dcf 0)
                    )
                    (   t
                        (
                            (setq _function1
                                (lambda ( )
                                    (_imgbox "col"
                                        (cond
                                            (   (= "0" maskon) -15)
                                            (   (= "1" transp)   0)
                                            (   (cdr (assoc 62 mcolor)))
                                            (   -15   )
                                        )
                                    )
                                )
                            )
                        )
                        (
                            (setq _function2
                                (lambda ( value )
                                    (_function1)
                                    (mode_tile "col" (atoi value))
                                )
                            )
                            (set_tile "trans" transp)
                        )
                        (
                            (setq _function3
                                (lambda ( value )
                                    (setq value (- 1 (atoi value)))
                                    (foreach tile '("off" "pick" "trans" "col")
                                        (mode_tile tile value)
                                    )
                                    (_function2 (if (= "0" maskon) "1" transp))
                                )
                            )
                            (set_tile "use" maskon)
                        )
                        
                        (action_tile "trans" "(_function2 (setq transp $value))")
                        (action_tile "use"   "(_function3 (setq maskon $value))")
                    
                        (set_tile "off" (rtos offset 2))
                        (action_tile "off"
                            (vl-prin1-to-string
                               '(if
                                    (or
                                        (null (setq dis (distof $value)))
                                        (< 5.0 dis)
                                        (< dis 1.0)
                                    )
                                    (progn
                                        (alert "Please provide a value between 1 and 5.")
                                        (set_tile "off" (rtos offset 2))
                                        (mode_tile "off" 2)
                                    )
                                    (set_tile "off" (rtos (setq offset dis) 2))
                                )
                            )
                        )

                        (action_tile "col"
                            (vl-prin1-to-string
                               '(if (setq col (acad_truecolordlg (vl-some '(lambda ( x ) (assoc x mcolor)) '(430 420 62)) nil))
                                    (_imgbox "col" (cdr (assoc 62 (setq mcolor col))))
                                )
                            )
                        )

                        (action_tile "pick" "(done_dialog 2)")
                     
                        (setq dcf (start_dialog))
                    )
                )
                (if
                    (and (= 2 dcf)
                        (progn
                            (while
                                (not
                                    (or (null (setq dis (getdist (strcat "\nPick Mask Offset Factor <" (rtos offset 2) ">: "))))
                                        (<= 1.0 dis 5.0)
                                    )
                                )
                                (princ "\nOffset must be between 1 and 5.")
                            )
                            dis
                        )
                    )
                    (setq offset dis)
                )
            )

            (if (= 1 dcf)
                (progn
                    (setq _MTextFunction
                        (if (= "1" maskon)
                            (eval
                                (list 'lambda '( enx )
                                    (list 'entmod
                                        (list 'append
                                           '(vl-remove-if
                                                (function
                                                    (lambda ( pair )
                                                        (member (car pair) '(45 63 90 421 431 441))
                                                    )
                                                )
                                                enx
                                            )
                                            (quote (list (cons 90 (if (= "1" transp) 3 1))))
                                            (quote
                                                (if (= "1" transp)
                                                   '((63 . 256))
                                                    (mapcar '(lambda ( x ) (cons (1+ (car x)) (cdr x))) mcolor)
                                                )
                                            )
                                            (quote (list (cons 45 offset) '(441 . 0)))
                                        )
                                    )
                                )
                            )
                            (lambda ( enx )
                                (vla-put-backgroundfill (vlax-ename->vla-object (cdr (assoc -1 enx))) :vlax-false)
                            )
                        )
                    )
                    (setq _MLeadFunction
                        (if (= "1"maskon)
                            (eval
                                (list 'lambda '( enx )
                                    (list 'foreach 'item
                                        (quote
                                            (list
                                                (cons 091 (LM:Color->MLeaderColor (if (= "1" transp) '((62 . 256)) mcolor)))
                                                (cons 141 offset)
                                                (cons 291 (if (= "1" transp) 1 0))
                                               '(292 . 1)
                                            )
                                        )
                                       '(setq enx (_substonce item (assoc (car item) enx) enx))
                                    )
                                   '(entmod enx)
                                )
                            )
                            (lambda ( enx )
                                (entmod (_substonce '(292 . 0) (assoc 292 enx) enx))
                            )
                        )
                    )                                    
                    (repeat (setq inc (sslength sel))
                        (setq enx (entget (ssname sel (setq inc (1- inc)))))
                        (if (= "MTEXT" (cdr (assoc 0 enx)))
                            (_MTextFunction enx)
                            (_MLeadFunction enx)
                        )
                    )
                )
                (princ "\n*Cancel*")
            )
        )
    )
    (if (< 0 dch)
        (setq dch (unload_dialog dch))
    )
    (if (and tmp (findfile tmp))
        (vl-file-delete tmp)
    )
    (princ)
)

;; Color -> MLeader Color  -  Lee Mac
;; Args: list of 62 / 420 DXF group values

(defun LM:Color->MLeaderColor ( c / x )
    (cond
        (   (setq x (cdr (assoc 420 c)))
            (+ -1040187392 x)
        )
        (   (zerop (setq x (cdr (assoc 62 c))))
            -1056964608
        )
        (   (= 256 x)
            -1073741824
        )
        (   (< 0 x 256)
            (+ -1023410176 x)
        )
    )
)

;;------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: Mask.lsp | Version 1.1 | © Lee Mac "
        (menucmd "m=$(edtime,$(getvar,DATE),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Type \"bmask\" to Invoke ::"
    )
)
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;