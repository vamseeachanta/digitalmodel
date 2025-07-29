;;; -*-  Mode: LISP -*- Syntax: AutoLISP (C) Benjamin Olasov 1988, 1989
;;; Displays and modifies the properties of individual entities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File: CHG.LSP Copyright (C) Benjamin Olasov    Graphic Systems, Inc.    ;;;
;;; Inquiries:                                                              ;;;
;;;                                                                         ;;;
;;;     Benjamin Olasov                                                     ;;;
;;;     Graphic Systems, Inc.:                                              ;;;
;;;                                                                         ;;;
;;;                    New York, NY:   PH (212) 725-4617                    ;;;
;;;                    Cambridge, MA:  PH (617) 492-1148                    ;;;
;;;                    MCI-Mail:       GSI-NY   344-4003                    ;;;
;;;                    Arpanet:        olasov@cs.columbia.edu               ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This program is provided 'as is' without warranty of any kind, either 
;; expressed or implied, including, but not limited to the implied warranties of
;; merchantability and fitness for a particular purpose.  The entire risk as to
;; the quality and performance of the program is with the user.  Should the 
;; program prove defective, the user assumes the entire cost of all necessary 
;; servicing, repair or correction. 
;; AutoLisp and AutoCad are registered trademarks of AutoDesk, Inc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; CHG displays and modifies the properties of individual entities.          ;;
;;                                                                           ;;
;; CHG creates a numbered menu of the selected entities properties, and      ;;
;; then prompts the user to select the number of the property to modify.     ;;
;; CHG then prompts for a new value for that property, which may be a        ;;
;; point (list), real, integer, or string.                                   ;;
;;                                                                           ;;
;; Any changes made by CHG can be undone using AutoCad's 'U' command.        ;;
;; Doing so will return the drawing to its state before using CHG.           ;;
;;                                                                           ;;
;; An example use of CHG:                                                    ;;
;; In a drawing containing two valid blocks A and B, an individual           ;;
;; iteration of block A can be transformed to an iteration of block B by     ;;
;; giving B as its new name. All of its previous insertion parameters will   ;;
;; remain the same, but its identity will be changed to block B. If the      ;;
;; name of the layer in which the entity resides is changed to the name of   ;;
;; an existing layer, the entity will change its residence to that layer.    ;;
;; However, if the new layer name is the name of a non-existing layer, a     ;;
;; layer with that name will be created, and the entity will be transferred  ;;
;; to that layer.                                                            ;;
;;                                                                           ;;
;; Syntax: CHG                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(gc)
(vmon)
(princ "\nLoading- please wait.")

(defun descriptor (key)
       (cond ((null key) nil)
             ((= key 0) "ENTITY TYPE: ")
             ((= key 1) "TEXT VALUE: ")
             ((and (= key 2)
                   (= (cdr (assoc 0 entity)) "ATTDEF")) "ATTRIBUTE TAG: ")
             ((and (= key 2)
                  (= (cdr (assoc 0 entity)) "INSERT")) "BLOCK NAME: ")
             ((= key 2) "NAME: ")
             ((or (= key 3)
                  (= key 4)) "OTHER NAME VALUES: ")
             ((= key 5) "HANDLE <RO>: ")
             ((= key 6) "LINETYPE NAME <RO>: ")
             ((= key 7) "TEXT STYLE NAME <RO>: ")
             ((= key 8) "LAYER: ")
             ((= key 9) "VARIABLE NAME IDENTIFIER: ")
             ((and (= key 10)
                   (= (cdr (assoc 0 entity)) "INSERT")) "INSERTION BASE: ")
             ((= key 10) "ORIGIN POINT: ")
             ((and (>= key 11)
                   (<= key 18)) "OTHER POINT COORDINATE: ")
             ((= key 20) "PRIMARY Y COORDINATE: ")
             ((and (>= key 21) (<= key 28)) "OTHER Y COORDINATE: ")
             ((and (>= key 31) (<= key 36)) "OTHER Z COORDINATE: ")
             ((= key 38) "ELEVATION: ")
             ((= key 39) "THICKNESS: ")
             ((and (>= key 40)
                   (<= key 48)
                   (or (= (CDR (ASSOC 0 ENTITY)) "CIRCLE")
                       (= (CDR (ASSOC 0 ENTITY)) "ARC"))) "RADIUS: ")
             ((and (>= key 40)
                    (<= key 48)
                   (or (= (cdr (assoc 0 entity)) "TEXT")
                       (= (cdr (assoc 0 entity)) "ATTDEF"))) "TEXT HEIGHT: ")
             ((and (= key 41)
                   (= (cdr (assoc 0 entity)) "INSERT")) "X SCALE FACTOR: ")
             ((and (= key 42)
                   (= (cdr (assoc 0 entity)) "INSERT")) "Y SCALE FACTOR: ")
             ((and (= key 43)
                   (= (cdr (assoc 0 entity)) "INSERT")) "Z SCALE FACTOR: ")
             ((and (>= key 40)
                    (<= key 48)) "FLOATING POINT VALUE: ")
             ((= key 49) "REPEATED VALUE: ")
             ((and (>= key 50)
                   (<= key 58)) "ANGLE: ")
             ((= key 62) "COLOR NUMBER <RO>: ")
             ((= key 66) "ENTITIES FOLLOW <RO>: ")
             ((= key 71) "MIRROR DIRECTION: ")
             ((and (>= key 70) (<= key 78)) "INTEGER VALUE: ")
             ((or (= key 210)
                  (= key 220)
                  (= key 230)) "EXTRUSION DIRECTION COORDINATES: ")
             ((= key 999) "COMMENTS: ")
             (T "UNCLASSIFIED VALUE: ")))

(princ ".")

(defun format-input (key / val label)
       (if (null key) nil
           (progn (setq val (cdr (assoc key entity)))
                  (cond ((= (type val) 'STR)
                         (graphscr)
                         (setq label (descriptor key))
                         (princ (strcat "\nCurrent " label))
                         (princ val)
                         (getstring T (strcat "\nNew " label)))
                        ((= (type val) 'REAL)
                         (cond ((and (>= key 40)
                                     (<= key 48)
                                (or (= (cdr (assoc 0 entity)) "CIRCLE")
                                    (= (cdr (assoc 0 entity)) "ARC")))
                                (setvar "coords" 2)
                                (graphscr)
                                (princ "\nCurrent angle: ")
                                (princ val)
                                (getdist (cdr (assoc 10 entity)) "\nNew radius: "))
                               ((and (>= key 50) (<= key 58))
                                (setvar "coords" 2)
                                (graphscr)
                                (princ "\nCurrent angle: ")
                                (princ val)
                                (getangle (cdr (assoc 10 entity)) "\nNew angle: "))
                              (T (graphscr)
                                 (setq label (descriptor key))
                                 (princ (strcat "\nCurrent " label))
                                 (princ val)
                                 (getreal (strcat "\nNew " label)))))
                         ((= (type val)  'INT)
                          (setq label (descriptor key))
                          (princ (strcat "\nCurrent " label))
                          (princ val)
                          (getint (strcat "\nNew " label)))
                         ((= (type val) 'LIST)
                          (graphscr)
                          (setvar "coords" 2)
                          (princ "\nCurrent point value: ")
                          (princ val)
                          (getpoint val "\nNew point: "))))))

(princ ".")

(defun C:CHG (/ entity counter ctr num tag new)
       (if (setq ename (entsel))
           (progn (setq entity (cdr (setq ent (entget (car ename))))
                        header (strcat (cdr (assoc 0 entity)) " PROPERTY TABLE")
                        num_props (length (cdr entity))
                        counter 0
                        ctr 0)
                  (textscr)
                  (repeat 24 (terpri))
                  (repeat (- 38 (/ (strlen header) 2)) (princ "\260"))
                  (princ (strcat " " header " "))
                  (repeat (- 38 (/ (strlen header) 2)) (princ "\260"))
                  (repeat (fix (/ (- 24 num_props) 2.0)) (terpri))
                  (mapcar '(lambda (e)
                                   (setq counter (1+ counter))
                                   (princ (strcat (if (< counter 10)
                                                      (strcat " " (itoa counter)) 
                                                      (itoa counter))
                                                  "]  "
                                                  (descriptor (car e))))
                                   (princ (cdr e))
                                   (princ "\n"))
                          (cdr entity))
                  (repeat (fix (/ (- 24 num_props) 2.0)) (terpri))
                  (setq num (getint "Number of property to change: "))
                  (if (and num
                           (> num 0)
                           (<= num num_props))
                      (progn (setq tag (car (nth (1- num) (cdr entity)))
                                   new (format-input tag)
                                   ent (subst (cons tag new)
                                              (assoc tag entity) ent)
                                   cmd (getvar "cmdecho"))
                             (setvar "cmdecho" 0)
                             (command "undo" "mark")
                             (setvar "cmdecho" cmd)
                             (entmod ent))
                      (princ "\nInvalid number.")))
           (princ "\nNo entity selected."))
       (princ))

(princ "\nFunction C:CHG loaded. Type CHG to start.")
(princ)

