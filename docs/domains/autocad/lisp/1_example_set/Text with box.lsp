;;----------------------=={ Associative Textbox }==---------------------;;
;;                                                                      ;;
;;  ----------------------------------                                  ;;
;;  Program Overview                                                    ;;
;;  ----------------------------------                                  ;;
;;                                                                      ;;
;;  This program enables the user to create an associative textbox      ;;
;;  surrounding a Text or MText object in the drawing. The size,        ;;
;;  position, orientation & rotation of the resulting textbox will      ;;
;;  be automatically updated following modification to the associated   ;;
;;  Text or MText object.                                               ;;
;;                                                                      ;;
;;  The user may create an associative textbox by issuing the 'tbox'    ;;
;;  command at the AutoCAD command-line. Upon calling the program, the  ;;
;;  user is prompted to specify an Offset Factor for the textbox, and   ;;
;;  the shape of the textbox to be created.                             ;;
;;                                                                      ;;
;;  The Offset Factor controls the dimensions of the textbox relative   ;;
;;  to the size of the associated annotation object and is a function   ;;
;;  of the text height. To provide an example, specifying an Offset     ;;
;;  Factor of 0.35 will result in a textbox which is offset from the    ;;
;;  text object by 0.35 multiplied by the text height of the object.    ;;
;;                                                                      ;;
;;  Upon entering a valid Offset Factor, the user may choose from       ;;
;;  three types of textbox: Circle, Rectangle or Slot. The circular     ;;
;;  textbox will have diameter equal to the length of the diagonal of   ;;
;;  the rectangular textbox. The Slot textbox will have dimensions      ;;
;;  equal to the rectangular textbox, however, the vertical edges       ;;
;;  (relative to the text object) will be semi-circular.                ;;
;;                                                                      ;;
;;  The program will remember the previously used settings for both     ;;
;;  the offset factor & textbox type, and these settings will be        ;;
;;  available as default options at both prompts throughout the active  ;;
;;  drawing session.                                                    ;;
;;                                                                      ;;
;;  The user will then be prompted to select either a Text or MText     ;;
;;  object to enclose with the textbox. At this prompt the user will    ;;
;;  be notified if the selected Text or MText object already possesses  ;;
;;  an associative textbox.                                             ;;
;;                                                                      ;;
;;  Textbox associativity may be removed at any time using by issuing   ;;
;;  the 'rtbox' (Remove Textbox) command at the AutoCAD command-line.   ;;
;;  When called, the user is prompted to select either a text or        ;;
;;  textbox object to remove the associativity between the pair. At     ;;
;;  this prompt the user also has the option to remove associativity    ;;
;;  from all textboxes in the drawing.                                  ;;
;;                                                                      ;;
;;  Note that this program will only remove the associative nature of   ;;
;;  the textbox - the user may subsequently delete the text or textbox  ;;
;;  object if required.                                                 ;;
;;                                                                      ;;
;;  ----------------------------------                                  ;;
;;  Associative Behaviour                                               ;;
;;  ----------------------------------                                  ;;
;;                                                                      ;;
;;  The program utilises a combination of Object Reactors & Command     ;;
;;  Reactors to monitor modification events to either the annotation    ;;
;;  object or surrounding textbox and will update either object         ;;
;;  accordingly.                                                        ;;
;;                                                                      ;;
;;  If the annotation object (Text or MText) is modified, the           ;;
;;  surrounding textbox will be automatically updated to reflect the    ;;
;;  modifications.                                                      ;;
;;                                                                      ;;
;;  If the textbox is modified the program will automatically revert    ;;
;;  all modifications applied to the textbox so as to retain the        ;;
;;  correct size & position relative to the associated annotation       ;;
;;  object.                                                             ;;
;;                                                                      ;;
;;  If the annotation object is copied, the associative textbox will    ;;
;;  also be copied and will form a new pair of associative text &       ;;
;;  textbox objects.                                                    ;;
;;                                                                      ;;
;;  If either annotation object or textbox are erased during the        ;;
;;  drawing session, the textbox associativity will not be lost until   ;;
;;  the drawing is closed. This enables the user to retain textbox      ;;
;;  associativity following an undo of the erase operation.             ;;
;;                                                                      ;;
;;  All associative data is stored within the Extended Entity Data      ;;
;;  (xData) of both the annotation object and associated textbox        ;;
;;  object. This xData is removed upon using the 'rtbox' command on a   ;;
;;  text & textbox pair.                                                ;;
;;                                                                      ;;
;;  The use of xData enables the retention of textbox associativity     ;;
;;  between drawing sessions by rebuilding the required reactors        ;;
;;  based on the available xData when the program is loaded.            ;;
;;                                                                      ;;
;;  Please note that this program will need to be loaded to enable      ;;
;;  associative behaviour for existing textbox objects in a drawing.    ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    30-04-2013                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    30-04-2013                                      ;;
;;                                                                      ;;
;;  Fixed bug causing the program to crash during loading if reactors   ;;
;;  whose reactor data is of a data type other than string are already  ;;
;;  running in the active drawing session.                              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    01-05-2013                                      ;;
;;                                                                      ;;
;;  Fixed bug causing MText & LWPolyline objects residing in Paperspace ;;
;;  layouts to be transferred to Modelspace.                            ;;
;;----------------------------------------------------------------------;;

(setq tbox:app "LMAC_TBOX") ;; AppID

;;----------------------------------------------------------------------;;
;;  c:tbox  -  Create associative textbox                               ;;
;;----------------------------------------------------------------------;;

(defun c:tbox ( / *error* box ent enx )
    (setq *error*
       '(   ( m )
            (if (not (wcmatch (strcase m t) "*cancel*,*exit*"))
                (princ (strcat "\nError: " m))
            )
            (princ)
        )
    )
    (setq tbox:off
        (cond
            (   (getdist (strcat "\nSpecify Offset Factor <" (rtos (cond (tbox:off) ((setq tbox:off 0.35))) 2 2) ">: ")))
            (   tbox:off   )
        )
    )
    (initget "Circle Slot Rectangle")
    (setq tbox:typ
        (cond
            (   (getkword (strcat "\nEnclose Text with [Circle/Slot/Rectangle] <" (cond (tbox:typ) ((setq tbox:typ "Rectangle"))) ">: ")))
            (   tbox:typ   )
        )
    )
    (while
        (progn (setvar 'errno 0) (setq ent (car (entsel "\nSelect Text or MText: ")))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (= 'ename (type ent))
                    (setq enx (entget ent (list tbox:app)))
                    (cond
                        (   (not (member (cdr (assoc 0 enx)) '("TEXT" "MTEXT")))
                            (princ "\nObject is not Text or MText.")
                        )
                        (   (assoc -3 enx)
                            (princ "\nObject already has an associative textbox.")
                        )
                        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (cdr (assoc 8 enx)))))))
                            (princ "\nObject is on locked layer.")
                        )
                    )
                )
            )
        )
    )
    (if ent
        (progn
            (regapp tbox:app)
            (if (setq box (tbox:createbox enx (strcase tbox:typ) tbox:off))
                (progn
                    (entmod
                        (append (vl-remove (assoc 40 enx) enx)
                            (list
                                (list -3
                                    (list tbox:app
                                       '(1002 . "{")
                                        (cons 1005 (cdr (assoc 5 (entget box))))
                                        (cons 1000 (strcase tbox:typ))
                                        (cons 1040 tbox:off)
                                       '(1002 . "}")
                                    )
                                )
                            )
                        )
                    )
                    (if (= 'vlr-object-reactor (type tbox:textreactor))
                        (vlr-owner-add tbox:textreactor (vlax-ename->vla-object ent))
                        (vlr-set-notification
                            (setq tbox:textreactor
                                (vlr-object-reactor (list (vlax-ename->vla-object ent)) "tbox-textreactor"
                                   '(
                                        (:vlr-modified . tbox:textcallback)
                                        (:vlr-copied   . tbox:textcopied)
                                    )
                                )
                            )
                            'active-document-only
                        )
                    )
                    (if (= 'vlr-object-reactor (type tbox:tboxreactor))
                        (vlr-owner-add tbox:tboxreactor (vlax-ename->vla-object box))
                        (vlr-set-notification
                            (setq tbox:tboxreactor
                                (vlr-object-reactor (list (vlax-ename->vla-object box)) "tbox-tboxreactor"
                                   '(
                                        (:vlr-modified . tbox:tboxcallback)
                                        (:vlr-copied   . tbox:tboxcopied)
                                    )
                                )
                            )
                            'active-document-only
                        )
                    )
                )
                (princ "\nUnable to create textbox for selected object.")
            )
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;
;;  c:rtbox  -  Remove associative textbox(es)                          ;;
;;----------------------------------------------------------------------;;

(defun c:rtbox ( / *error* all bob box ent enx inc lck obj )
    (setq *error*
       '(   ( m )
            (foreach lay lck (vla-put-lock lay :vlax-true))
            (if (not (wcmatch (strcase m t) "*cancel*,*exit*"))
                (princ (strcat "\nError: " m))
            )
            (princ)
        )
    )
    (cond
        (   (null (setq all (ssget "_X" (list '(0 . "TEXT,MTEXT,CIRCLE,LWPOLYLINE") (list -3 (list tbox:app))))))
            (princ "\nNo associative textboxes found.")
        )
        (   t
            (while
                (progn
                    (setvar 'errno 0)
                    (initget "All Exit")
                    (setq ent (entsel "\nSelect text or textbox to remove associativity [All/Exit] <Exit>: "))
                    (cond
                        (   (= 7 (getvar 'errno))
                            (princ "\nMissed, try again.")
                        )
                        (   (= "Exit" ent)
                            nil
                        )
                        (   (= "All" ent)
                            (setq lck (tbox:unlocklayers))
                            (repeat (setq inc (sslength all))
                                (setq ent (ssname all (setq inc (1- inc))))
                                (tbox:removeowner (vlax-ename->vla-object ent))
                                (tbox:removexdata ent)
                            )
                            (foreach lay lck (vla-put-lock lay :vlax-true))
                            (princ "\nAll textbox associativity removed.")
                            nil
                        )
                        (   (= 'ename (type (setq ent (car ent))))
                            (cond
                                (   (null (assoc -3 (setq enx (entget ent (list tbox:app)))))
                                    (princ "\nObject does not have an associative textbox.")
                                )
                                (   (null (vlax-write-enabled-p (setq obj (vlax-ename->vla-object ent))))
                                    (princ "\nObject is on a locked layer.")
                                )
                                (   (and
                                        (setq box (handent (cdr (assoc 1005 (cdadr (assoc -3 enx))))))
                                        (setq bob (vlax-ename->vla-object box))
                                        (null (vlax-write-enabled-p bob))
                                    )
                                    (princ
                                        (strcat
                                            "\nAssociated "
                                            (if (member (cdr (assoc 0 (entget box))) '("TEXT" "MTEXT"))
                                                "text" "box"
                                            )
                                            " is on locked a layer."
                                        )
                                    )
                                )
                                (   t
                                    (tbox:removeowner obj)
                                    (tbox:removexdata ent)
                                    (if bob (tbox:removeowner bob))
                                    (if (and box (entget box))
                                        (tbox:removexdata box)
                                    )
                                    (princ "\nTextbox associativity removed.")
                                    nil
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;
;;                           Support Functions                          ;;
;;----------------------------------------------------------------------;;

(defun tbox:textcallback ( owner reactor params / ent enx typ val )
    (if
        (and
            (vlax-read-enabled-p owner)
            (progn (vla-getxdata owner tbox:app 'typ 'val) val)
            (setq val (mapcar 'cons (vlax-safearray->list typ) (mapcar 'vlax-variant-value (vlax-safearray->list val))))
            (setq ent (handent (cdr (assoc 1005 val))))
            (setq enx (entget ent))
            (vlax-write-enabled-p (vlax-ename->vla-object ent))
        )
        (if (= "AcDbMText" (vla-get-objectname owner)) ;; MText DXF data is not immediately updated after mtedit
            (tbox:tboxcallback (vlax-ename->vla-object ent) nil nil)
            (tbox:updatebox (entget (vlax-vla-object->ename owner)) (cdr (assoc 1000 val)) (cdr (assoc 1040 val)) enx)
        )
    )
    (princ)
)

(defun tbox:textcopied ( owner reactor params )
    (if (/= 0 (car params))
        (progn
            (setq tbox:owner (append tbox:owner (list (car params))))
            (vlr-command-reactor "tbox-textcopiedcommreactor"
               '(
                    (:vlr-commandended     . tbox:textcopiedcommandended)
                    (:vlr-commandcancelled . tbox:textcopiedcommandcancelled)
                    (:vlr-commandfailed    . tbox:textcopiedcommandcancelled)
                )
            )
        )
    )
    (princ)
)

(defun tbox:textcopiedcommandended ( reactor params / box ent enx val )
    (vlr-remove reactor)
    (if
        (and
            (setq ent (car tbox:owner))
            (setq enx (entget ent (list tbox:app)))
            (member (cdr (assoc 0 enx)) '("TEXT" "MTEXT"))
            (setq val (cdadr (assoc -3 enx)))
            (setq box (tbox:createbox enx (cdr (assoc 1000 val)) (cdr (assoc 1040 val))))
        )
        (progn
            (entmod
                (append (vl-remove (assoc 40 enx) (entget ent))
                    (list
                        (list -3
                            (list tbox:app
                               '(1002 . "{")
                                (cons  1005 (cdr (assoc 5 (entget box))))
                                (assoc 1000 val)
                                (assoc 1040 val)
                               '(1002 . "}")
                            )
                        )
                    )
                )
            )
            (if (= 'vlr-object-reactor (type tbox:textreactor))
                (vlr-owner-add tbox:textreactor (vlax-ename->vla-object ent))
            )
            (if (= 'vlr-object-reactor (type tbox:tboxreactor))
                (vlr-owner-add tbox:tboxreactor (vlax-ename->vla-object box))
            )                    
        )            
    )
    (setq tbox:owner (cdr tbox:owner))
    (princ)
)

(defun tbox:textcopiedcommandcancelled ( reactor params )
    (vlr-remove reactor)
    (setq tbox:owner nil)
    (princ)
)

(defun tbox:tboxcallback ( owner reactor params )
    (setq tbox:owner (cons owner tbox:owner))
    (vlr-command-reactor "tbox-commreactor"
       '(
            (:vlr-commandended     . tbox:commandended)
            (:vlr-commandcancelled . tbox:commandcancelled)
            (:vlr-commandfailed    . tbox:commandcancelled)
        )
    )
    (princ)
)

(defun tbox:commandended ( reactor params / box txt typ val )
    (vlr-remove reactor)
    (if
        (and
            (setq box (car tbox:owner))
            (vlax-read-enabled-p  box)
            (vlax-write-enabled-p box)
            (progn (vla-getxdata  box tbox:app 'typ 'val) val)
            (setq val (mapcar 'cons (vlax-safearray->list typ) (mapcar 'vlax-variant-value (vlax-safearray->list val))))
            (setq txt (entget (handent (cdr (assoc 1005 val)))))
        )
        (tbox:updatebox txt (cdr (assoc 1000 val)) (cdr (assoc 1040 val)) (entget (vlax-vla-object->ename box)))
    )
    (setq tbox:owner (cdr tbox:owner))
    (princ)
)

(defun tbox:commandcancelled ( reactor params )
    (vlr-remove reactor)
    (setq tbox:owner nil)
    (princ)
)

(defun tbox:tboxcopied ( owner reactor params )
    (if (/= 0 (car params))
        (progn
            (setq tbox:owner (append tbox:owner (list (car params))))
            (vlr-command-reactor "tbox-tboxcopiedcommreactor"
               '(
                    (:vlr-commandended     . tbox:tboxcopiedcommandended)
                    (:vlr-commandcancelled . tbox:tboxcopiedcommandcancelled)
                    (:vlr-commandfailed    . tbox:tboxcopiedcommandcancelled)
                )
            )            
        )
    )
    (princ)
)

(defun tbox:tboxcopiedcommandended ( reactor params / ent )
    (vlr-remove reactor)
    (if
        (and
            (setq ent (car tbox:owner))
            (member (cdr (assoc 0 (entget ent))) '("CIRCLE" "LWPOLYLINE"))
        )
        (entdel ent)
    )
    (setq tbox:owner (cdr tbox:owner))
    (princ)
)

(defun tbox:tboxcopiedcommandcancelled ( reactor params )
    (vlr-remove reactor)
    (setq tbox:owner nil)
    (princ)
)

(defun tbox:createbox ( enx typ off / lst )
    (if (setq lst (tbox:textbox enx (* off (cdr (assoc 40 enx)))))
        (if (= "CIRCLE" typ)
            (entmakex
                (list
                   '(0 . "CIRCLE")
                    (cons 10 (tbox:mid (car lst) (caddr lst)))
                    (cons 40 (/ (distance (car lst) (caddr lst)) 2.0))
                    (assoc 210 enx)
                    (list -3
                        (list tbox:app
                           '(1002 . "{")
                            (cons 1005 (cdr (assoc 5 enx)))
                           '(1000 . "CIRCLE")
                            (cons 1040 off)
                           '(1002 . "}")
                        )
                    )
                )
            )
            (entmakex
                (append
                   '(
                        (0 . "LWPOLYLINE")
                        (100 . "AcDbEntity")
                        (100 . "AcDbPolyline")
                        (90 . 4)
                        (70 . 1)
                    )
                    (list (cons 38 (caddar lst)))
                    (apply 'append
                        (mapcar
                            (function
                                (lambda ( a b )
                                    (list (list 10 (car a) (cadr a)) (cons 42 b))
                                )
                            )
                            lst
                            (if (= "SLOT" typ)
                               '(0.0 1.0 0.0 1.0)
                               '(0.0 0.0 0.0 0.0)
                            )
                        )
                    )
                    (list (assoc 210 enx)
                        (list -3
                            (list tbox:app
                               '(1002 . "{")
                                (cons 1005 (cdr (assoc 5 enx)))
                                (cons 1000 typ)
                                (cons 1040 off)
                               '(1002 . "}")
                            )
                        )
                    )                        
                )
            )
        )
    )
)

(defun tbox:updatebox ( enx typ off box / lst )
    (if (= 'vlr-object-reactor (type tbox:tboxreactor))
        (vlr-remove tbox:tboxreactor)
    )
    (if (setq lst (tbox:textbox enx (* off (cdr (assoc 40 enx)))))
        (if (= "CIRCLE" typ)
            (entmod
                (subst
                    (cons  10 (tbox:mid (car lst) (caddr lst)))
                    (assoc 10 box)
                    (subst
                        (cons  40 (/ (distance (car lst) (caddr lst)) 2.0))
                        (assoc 40 box)
                        (subst
                            (assoc 210 enx)
                            (assoc 210 box)
                            box
                        )
                    )
                )
            )
            (entmod
                (append
                    (subst
                        (cons  38 (caddar lst))
                        (assoc 38 box)
                        (reverse (member (assoc 38 box) (reverse box)))
                    )
                    (apply 'append
                        (mapcar
                            (function
                                (lambda ( a b )
                                    (list (list 10 (car a) (cadr a)) (cons 42 b))
                                )
                            )
                            lst
                            (if (= "SLOT" typ)
                               '(0.0 1.0 0.0 1.0)
                               '(0.0 0.0 0.0 0.0)
                            )
                        )
                    )
                    (list (assoc 210 enx))
                )
            )
        )
    )
    (if (= 'vlr-object-reactor (type tbox:tboxreactor))
        (vlr-add tbox:tboxreactor)
    )
)

(defun tbox:mid ( a b )
    (mapcar (function (lambda ( a b ) (/ (+ a b) 2.0))) a b)
)

(defun tbox:unlocklayers ( / lck )
    (vlax-for lay (vla-get-layers (LM:acdoc))
        (if (= :vlax-true (vla-get-lock lay))
            (progn
                (vla-put-lock lay :vlax-false)
                (setq lck (cons lay lck))
            )
        )
    )
    lck
)

(defun tbox:removexdata ( ent / enx )
    (if (member (cdr (assoc 0 (setq enx (entget ent)))) '("TEXT" "MTEXT"))
        (entmod (append (vl-remove (assoc 40 enx) enx) (list (list -3 (list tbox:app)))))
        (entmod (append enx (list (list -3 (list tbox:app)))))
    )
)

(defun tbox:removeowner ( obj )
    (if (wcmatch (vla-get-objectname obj) "AcDb*Text")
        (if (= 'vlr-object-reactor (type tbox:textreactor))
            (vlr-owner-remove tbox:textreactor obj)
        )
        (if (= 'vlr-object-reactor (type tbox:tboxreactor))
            (vlr-owner-remove tbox:tboxreactor obj)
        )
    )
)        
                                
;; The following function is based on code by gile

(defun tbox:textbox ( enx off / b h j l m n o p r w )
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

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;
;;                            Loading Function                          ;;
;;----------------------------------------------------------------------;;

(
    (lambda ( / *error* box ent inc lck obj sel txt )
        (setq *error*
           '(   ( m )
                (foreach lay lck (vla-put-lock lay :vlax-true))
                (if (not (wcmatch (strcase m t) "*cancel*,*exit*"))
                    (princ (strcat "\nError: " m))
                )
                (princ)
            )
        )
        (vl-load-com)
        (setq lck (tbox:unlocklayers))
        (foreach typ (vlr-reactors :vlr-object-reactor :vlr-command-reactor)
            (foreach vlr (cdr typ)
                (if (and (= 'str (type (vlr-data vlr))) (wcmatch (vlr-data vlr) "tbox-*reactor"))
                    (vlr-remove vlr)
                )
            )
        )
        (if (setq sel (ssget "_X" (list '(0 . "TEXT,MTEXT,CIRCLE,LWPOLYLINE") (list -3 (list tbox:app)))))
            (progn
                (repeat (setq inc (sslength sel))
                    (setq ent (ssname sel (setq inc (1- inc)))
                          obj (vlax-ename->vla-object ent)
                    )
                    (if (member (cdr (assoc 0 (entget ent))) '("TEXT" "MTEXT"))
                        (setq txt (cons obj txt))
                        (setq box (cons obj box))
                    )
                )
                (if txt
                    (vlr-set-notification
                        (setq tbox:textreactor
                            (vlr-object-reactor txt "tbox-textreactor"
                               '(
                                    (:vlr-modified . tbox:textcallback)
                                    (:vlr-copied   . tbox:textcopied)
                                )
                            )
                        )
                        'active-document-only
                    )
                )
                (if box
                    (vlr-set-notification
                        (setq tbox:tboxreactor
                            (vlr-object-reactor box "tbox-tboxreactor"
                               '(
                                    (:vlr-modified . tbox:tboxcallback)
                                    (:vlr-copied   . tbox:tboxcopied)
                                )
                            )
                        )
                        'active-document-only
                    )
                )
            )
        )
        (foreach lay lck (vla-put-lock lay :vlax-true))
        (setq tbox:owner nil)
        (princ)
    )
)

;;----------------------------------------------------------------------;;

(princ
    (strcat
        "\n:: AssociativeTextbox.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,$(getvar,date),YYYY)")
        " www.lee-mac.com"
        "\n:: Available Commands:"
        "\n::     \"tbox\"  -  Create Associative Textbox"
        "\n::    \"rtbox\"  -  Remove Associative Textbox"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;