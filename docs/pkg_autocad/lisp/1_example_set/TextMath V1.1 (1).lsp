;;;¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,;;;
;;;ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,¤º°`°º¤;;;
;;                                                                               ;;
;;                                                                               ;;
;;                          --=={  Text Math  }==--                              ;;
;;                                                                               ;;
;;  The program will allow the user to perform mathematical operations on Text,  ;;
;;  MText, Dimensions & Attributes containing numerical data.                    ;;
;;                                                                               ;;
;;  The user is prompted to select an operation (Addition, Subtraction,          ;;
;;  Division & Multiplication); and an operand. The user is then prompted to     ;;
;;  make a selection of objects to modify.                                       ;;
;;                                                                               ;;
;;  If a non-commutative operation is chosen, the user will be prompted to       ;;
;;  specify the order of operation.                                              ;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  TextMath  /  TMA                                           ;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  AUTHOR:                                                                      ;;
;;                                                                               ;;
;;  Copyright © Lee McDonnell, April 2010. All Rights Reserved.                  ;;
;;                                                                               ;;
;;      { Contact: Lee Mac @ TheSwamp.org, CADTutor.net }                        ;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  VERSION:                                                                     ;;
;;                                                                               ;;
;;  ø 1.0   ~¤~     9th April 2010   ~¤~   º First Release                       ;;
;;...............................................................................;;
;;  ø 1.1   ~¤~    11th April 2010   ~¤~   º Updated to include Dimensions.      ;;
;;                                         º Updated ParseNumbers function.      ;;
;;...............................................................................;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;;¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,;;;
;;;ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,¤º°`°º¤;;;

(defun c:TMA nil (c:TextMath)) 

(defun c:TextMath (/ *error*  ParseNumbers PerformOperation SubstAtN
                     DOC E ELST FUN I OP OPERAND O ORD SS TMP UFLAG X)
  (vl-load-com)
  ;; Lee Mac  ~  09.04.10

  (defun *error* (msg)
    (and UFlag (vla-EndUndoMark doc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ))
  

  (defun ParseNumbers (str / isString isNumber lst tmp)

    (defun isString (x lst)

      (cond (  (null lst) (list x))

            (  (< 47 (car lst) 58)

               (cons x (isNumber (chr (car lst)) (cdr lst))))

            (  (= 45 (car lst))

               (if (and (cadr lst)
                          (numberp
                            (read (setq tmp (strcat "-" (chr (cadr lst)))))))
                   
                 (cons x (isNumber tmp (cddr lst)))
                 (isString (strcat x (chr (car lst))) (cdr lst))))

            (t (isString (strcat x (chr (car lst))) (cdr lst)))))

    (defun isNumber (x lst)
      
      (cond (  (null lst) (list (read x)))

            (  (= 46 (car lst))
             
               (if (and (cadr lst)
                        (numberp
                          (read (setq tmp (strcat x "." (chr (cadr lst)))))))
                 
                 (isNumber tmp (cddr lst))
                 (cons (read x) (isString (chr (car lst)) (cdr lst)))))

            (  (< 47 (car lst) 58)

               (isNumber (strcat x (chr (car lst))) (cdr lst)))

            (t (cons (read x) (isString (chr (car lst)) (cdr lst))))))

    (if (setq lst (vl-string->list str))
      ((if (or (and (= 45 (car lst))
                    (< 47 (cadr lst) 58))
               (< 47 (car lst) 58)) isNumber isString) (chr (car lst)) (cdr lst))))
  

  (defun PerformOperation (func str operand o)
    (apply (function strcat)
           (mapcar (function (lambda (x) (if (vl-position (type x) '(INT REAL))
                                           (vl-princ-to-string (apply func (if o (list x operand)
                                                                                 (list operand x)))) x)))
                   (ParseNumbers str))))
  

  (defun SubstAtN (new n lst)
    (cond (  (null lst) nil)
          (  (zerop n) (cons new (cdr lst)))
          (  (cons (car lst) (SubstAtN new (1- n) (cdr lst))))))
  

  (or *tMath* (setq *tMath* "Add"))  
  (setq op '(("Add" . +) ("Subtract" . -) ("Divide" . /) ("Multiply" . *)))

  (initget "Add Subtract Divide Multiply")
  (setq *tMath* (cond ((getkword (strcat "\nChoose Operation [Add/Subtract/Divide/Multiply] <" *tMath* "> : ")))
                      (*tMath*)))

  (setq fun (cdr (assoc *tMath* op)))
  (initget 1)
  (setq x (getreal "\nSpecify Operand: "))
  (if (and (zerop (rem x 1)) (not (eq '/ fun))) (setq x (fix x)))

  (setq ord
    (if (vl-position fun '(- /))
      (progn
        (initget "A B")
        (eq "A" (getkword (strcat "\n[A] (x " (vl-princ-to-string fun) " " (vl-princ-to-string   x) ") or [B] ("
                                              (vl-princ-to-string   x) " " (vl-princ-to-string fun) " x) ? : "))))))

  (if (and (setq i -1 ss (ssget "_:L" '((-4 . "<OR")
                                          (0 . "TEXT,MTEXT,DIMENSION")
                                            (-4 . "<AND")
                                              (0 . "INSERT")
                                              (66 . 1)
                                            (-4 . "AND>")
                                        (-4 . "OR>"))))
           
           (setq uFlag (not (vla-StartUndoMark
                              (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))))))
    (progn
    
      (while (setq e (ssname ss (setq i (1+ i))))
        (setq eLst (entget e))
        
        (cond (  (eq "INSERT" (cdr (assoc 0 eLst)))

                 (while (not (eq "SEQEND" (cdr (assoc 0 (setq eLst (entget (setq e (entnext e))))))))
                   (entmod
                     (subst
                       (cons 1 (PerformOperation fun (cdr (assoc 1 eLst)) x ord)) (assoc 1 eLst) eLst))))

              (  (wcmatch (cdr (assoc 0 eLst)) "TEXT,DIMENSION")

                 (entmod (subst (cons 1 (PerformOperation fun (cdr (assoc 1 eLst)) x ord)) (assoc 1 eLst) eLst)))

              (  (  (lambda ( i )
                      (mapcar
                        (function
                          (lambda (pair)
                            (setq i (1+ i))
                            
                            (if (vl-position (car pair) '(1 3))
                              (setq eLst (SubstAtN (cons (car pair)
                                                         (PerformOperation fun (cdr pair) x ord)) i eLst)))))
                        eLst))
                   -1)

                 (entmod eLst))))
      
      (setq uFlag (vla-EndUndoMark doc))))

  (princ))

(princ "\nø¤º°`°º¤ø  TextMath.lsp ~ Copyright © by Lee McDonnell  ø¤º°`°º¤ø")
(princ "\n   ~¤~      ...Type \"TextMath\" or \"TMA\" to Invoke...       ~¤~   ")
(princ)


;;;¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,;;;
;;                                                                               ;;
;;                             End of Program Code                               ;;
;;                                                                               ;;
;;;ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,¤º°`°º¤;;;

  

  