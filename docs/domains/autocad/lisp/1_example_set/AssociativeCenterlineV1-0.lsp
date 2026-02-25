;;--------------=={ Associative Centerlines }==---------------;;
;;                                                            ;;
;;  Uses reactors to update centerlines following             ;;
;;  modification of associated circles. Stores entity handles ;;
;;  in entity xData to enable reactor rebuild upon loading,   ;;
;;  allowing retention of associativity between sessions.     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    12-05-2011                            ;;
;;------------------------------------------------------------;;

(setq cl:ratio 1.25 cl:app "LMAC_CL")

;;------------------------------------------------------------;;

(defun c:cl ( / _line ss e c r l1 l2 )
  (if
    (and
      (setq ss
        (ssget
          (list '(0 . "CIRCLE") '(-4 . "<NOT") (list -3 (list cl:app)) '(-4 . "NOT>"))
        )
      )
      (or (tblsearch "APPID" cl:app) (regapp cl:app))
    )
    (progn
      (defun _line ( p1 p2 h )
        (entmakex
          (list (cons 0 "LINE") (cons 10 p1) (cons 11 p2)
            (list -3
              (list cl:app
                (cons 1002 "{") (cons 1005 h) (cons 1002 "}")
              )
            )
          )
        )
      )
      (repeat (setq i (sslength ss))
        (setq e  (entget (ssname ss (setq i (1- i))))
              h  (cdr (assoc  5 e))
              c  (cdr (assoc 10 e))
              r  (* cl:ratio (cdr (assoc 40 e)))
              l1 (_line (polar c 0. r) (polar c pi r) h)
              l2 (_line (polar c (/ pi 2.) r) (polar c (/ (* 3. pi) 2.) r) h)
        )
        (entmod
          (list (assoc -1 e)
            (list -3
              (list cl:app
                (cons 1002 "{")
                (cons 1005 (cdr (assoc 5 (entget l1))))
                (cons 1005 (cdr (assoc 5 (entget l2))))
                (cons 1002 "}")
              )
            )
          )
        )
        (vlr-object-reactor (list (vlax-ename->vla-object (cdr (assoc -1 e)))) (list cl:app h)
          (list
            (cons :vlr-modified 'cl:circle:callback)
          )
        )
        (vlr-object-reactor (mapcar 'vlax-ename->vla-object (list l1 l2)) (list cl:app h)
          (list
            (cons :vlr-modified 'cl:line:callback)
          )
        )
      )
    )
  )
  (princ)
)

;;------------------------------------------------------------;;

(defun c:clremove ( / _massoc ss fl i e r d h x )

  (defun _massoc ( x l )
    (if (setq a (assoc x l))
      (cons (cdr a) (_massoc x (cdr (member a l))))
    )
  )
  
  (princ "\nSelect Circles to Remove Associativity <All>: ")
  (setq fl (list '(0 . "CIRCLE") (list -3 (list cl:app))) i -1)
  
  (if
    (setq ss
      (cond
        ( (ssget fl) )
        ( (ssget "_X" fl) )
      )
    )
    (while (setq e (ssname ss (setq i (1+ i)))) (setq e (entget e (list cl:app)))
      (foreach r (cdar (vlr-reactors :vlr-object-reactor))
        (if
          (and
            (setq d (vlr-data r))
            (listp d)
            (eq cl:app (car d))
            (or (not (cadr d)) (eq (cdr (assoc 5 e)) (cadr d)))
          )
          (vlr-remove r)
        )
      )
      (foreach h (_massoc 1005 (cdadr (assoc -3 e)))
        (if (setq x (entget (handent h)))
          (entmod (list (assoc -1 x) (list -3 (list cl:app))))
        )
      )
      (entmod (list (assoc -1 e) (list -3 (list cl:app))))
    )
  )
  (princ)
)      

;;------------------------------------------------------------;;

(defun cl:circle:callback ( owner reactor params / xtyp xval c r )
  (if
    (and
      (vlax-read-enabled-p owner)
      (progn (vla-getxdata owner cl:app 'xtyp 'xval) xval)
      (setq
        c (vlax-get owner 'center)
        r (* cl:ratio (vlax-get owner 'radius))
      )
    )
    (mapcar
      (function
        (lambda ( h a ) 
          (if (or (entget (setq h (handent h))) (entdel h))
            (entmod
              (list (cons -1 h) (cons 10 (polar c a r)) (cons 11 (polar c (+ a pi) r)))
            )
          )
        )
      )
      (cddr (mapcar 'vlax-variant-value (vlax-safearray->list xval))) (list 0. (/ pi 2.))
    )
  )
  (princ)
)

;;------------------------------------------------------------;;

(defun cl:line:callback ( owner reactor params )
  (setq *data (list owner reactor))
  (vlr-command-reactor (list cl:app)
    (list
      (cons :vlr-commandended     'cl:line:modify)
      (cons :vlr-commandcancelled 'cl:line:cancelled)
      (cons :vlr-commandfailed    'cl:line:cancelled)
    )
  )
  (vlr-remove reactor)
  (princ)  
)

;;------------------------------------------------------------;;

(defun cl:line:modify ( reactor params / xtyp xval h ) (vlr-remove reactor)
  (if
    (and *data (not (vlax-erased-p (car *data))) (progn (vla-getxdata (car *data) cl:app 'xtyp 'xval) xval)    
      (or
        (entget
          (setq h
            (handent
              (caddr
                (mapcar 'vlax-variant-value (vlax-safearray->list xval))
              )
            )
          )
        )
        (entdel h)
      )
    )
    (progn
      (cl:circle:callback (vlax-ename->vla-object h) nil nil)
      (vlr-add (cadr *data))
      (setq *data nil)
    )
  )   
  (princ)
)

;;------------------------------------------------------------;;

(defun cl:line:cancelled ( reactor params ) (vlr-remove reactor)
  (if *data
    (progn
      (vlr-add (cadr *data))
      (setq *data nil)
    )
  )
  (princ)
)

;;------------------------------------------------------------;;

(
  (lambda ( / r d s i e o xtyp xval )
    (foreach r (cdar (vlr-reactors :vlr-object-reactor))
      (if (and (setq d (vlr-data r)) (listp d) (eq cl:app (car d)))
        (vlr-remove r)
      )
    )
    (if (setq s (ssget "_X" (list '(0 . "CIRCLE") (list -3 (list cl:app)))))
      (repeat (setq i (sslength s))
        (setq e (ssname s (setq i (1- i))))
        (vlr-object-reactor (list (setq o (vlax-ename->vla-object e))) (list cl:app (cdr (assoc 5 (entget e))))
          (list
            (cons :vlr-modified 'cl:circle:callback)
          )
        )
        (vla-getxdata o cl:app 'xtyp 'xval) (setq xval (mapcar 'vlax-variant-value (vlax-safearray->list xval)))
        (vlr-object-reactor
          (mapcar
            (function
              (lambda ( h )
                (or (entget (setq h (handent h))) (entdel h)) (vlax-ename->vla-object h)
              )
            )
            (list (caddr xval) (cadddr xval))
          )
          (list cl:app (cdr (assoc 5 (entget e)))) (list (cons :vlr-modified 'cl:line:callback))
        )
      )
    )
  )
)

(vl-load-com) (princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;