;--------- CONCEAL.LSP   Sinom Jones   Sept 1987

;     This macro performs a "TRIM" om selected entities
;    an transfers the "EXCESS" section of the entity onto
;    a user defined layer. This can be used with a layer
;    with a "hidden" linetype to illustrate a section of a
;    part obscured from view.

;    If an entity is selected that does not intersect
;    a boundary, it is "CHANGED" to the transfer layer.

;    The macro will only work with LINES,ARCS and CIRCLES.
;    All other entities will be ignored.

;    Enter a null reponse to the entity selection prompt
;    to terminate the command.


; Function to store system variables
(defun MODES (a)
   (setq MLST '())
   (repeat (length a)
      (setq MLST (append MLST (list (list (car a) (getvar (car a))))))
      (setq a (cdr a)))
)

; Function to reset system variables
(defun MODER ()
   (repeat (length MLST)
      (setvar (caar MLST) (cadar MLST))
      (setq MLST (cdr MLST))
   )
)


; Function to conceal arcs
(defun XARC (/ cen r)
    (command "LAYER" "S" $ln "")
    (setq a (cdr (assoc 50 e1)))
    (setq b (cdr (assoc 51 e1)))
    (setq cen (cdr (assoc 10 e1)))
    (setq r (cdr (assoc 40 e1)))
    (command "TRIM" ss "" (cadr e) "")
    (setq e2 (entget (car e)))
    (setq c (cdr (assoc 50 e2)))
    (setq d (cdr (assoc 51 e2)))
    (cond ((equal e1 e2)
           (command "CHANGE" (cdar e1) "" "P" "LA" $ln "")
          )
          ((and (equal a c) (/= b d) (equal el (entlast)))
           (command "ARC" (polar cen d r)
                          "c" cen
                          (polar cen b r)
           )
          )
          ((and (equal b d) (/= a c) (equal el (entlast)))
           (command "ARC" (polar cen a r)
                          "c" cen
                          (polar cen c r)
           )
          )
          (T
           (setq x (cdr (assoc 50 (entget (entlast)))))
           (setq y (cdr (assoc 51 (entget (entlast)))))
           (cond ((and (equal a c) (equal b y))
                  (command "ARC" (polar cen d r)
                                 "c" cen
                                 (polar cen x r)
                  )
                 )
           )
          )
    )
    (command "LAYER" "S" cl "")
)

; Function to conceal circles
(defun XCICLES (/ cen r)
    (command "LAYER" "S" $ln "")
    (setq el (entlast))
    (setq cen (cdr (assoc 10 e1)))
    (setq r (cdr (assoc 40 e1)))
    (command "TRIM" ss "" (cadr e) "")
    (setq e2 (entget (car e)))
    (cond  ((equal e1 e2)
            (command "CHANGE" (cdar e1) "" "P" "LA" $ln "")
           )
           ((= (cdr (assoc 0 e2)) "ARC")
            (setq r (cdr (assoc 40 e2)))
            (setq cen (cdr (assoc 10 e2)))
            (setq a (polar cen (cdr (assoc 50 e2)) r))
            (setq b (polar cen (cdr (assoc 51 e2)) r))
            (command "ARC" b "C" cen A)
           )
    )
    (command "LAYER" "S" cl "")
)

; Function to conceal lines
(defun XLINE ()
    (command "LAYER" "S" $ln "")
    (setq a (cdr (assoc 10 e1)))
    (setq b (cdr (assoc 11 e1)))
    (command "TRIM" ss "" (cadr e) "")
    (setq e2 (entget (car e)))
    (setq c (cdr (assoc 10 e2)))
    (setq d (cdr (assoc 11 e2)))
    (cond ((equal e1 e2)
           (command "CHANGE" (cdar e1) "" "P" "LA" $ln "")
          )
          ((and (equal a c) (/= b d) (equal el (entlast)))
           (command "LINE" d b "")
          )
          ((and (equal b d) (/= a c) (equal el (entlast)))
           (command "LINE" a c "")
          )
          (T
           (setq x (cdr (assoc 10 (entget (entlast)))))
           (setq y (cdr (assoc 11 (entget (entlast)))))
           (cond ((and (equal a c) (equal b y))
                  (command "LINE" d x "")
                 )
           )
          )
    )
    (command "LAYER" "S" cl "")
)

(defun C:CEe (/ cen r e el e1 e2 a b c cl d ln ss yn x y)

  (modes '("CMDECHO" "BLIPMODE" "HIGHLIGHT"))
  (setvar "CMDECHO" 0)
  (setvar "BLIPMODE" 0)
  (setq cl (getvar "CLAYER"))
  (if (null $ln) (setq $ln cl))
  (while (null ln)
         (setq ln (getstring (strcat
                               "\nTransfer layer name <"
                               $ln
                               ">: "
                             )
                  )
         )
         (if (= ln "") (setq ln $ln))
         (if (not (tblsearch "LAYER" ln))
             (progn
              (initget "Yes No")
              (setq prmpt (strcat "\nLayer "
                                  (strcase ln t)
                                  " does not exist. Create? <N>: "
                          )
              )
              (setq yn (getkword prmpt))
              (if (= yn "Yes")
                  (command "LAYER" "n" ln "")
                  (setq ln nil)
              )
             )
         )
  )

  (setq $ln ln)
  (prompt "\nSelect trim boundaries: ")
  (setq ss (ssget))
  (setvar "HIGHLIGHT" 0)
  (setq e (entsel "\nSelect entity to conceal "))
  (while e
    (setq e1 (entget (car e)))
    (setq el (entlast))
    (cond  ((= (cdr (assoc 0 e1)) "LINE")
            (xline)
           )
           ((= (cdr (assoc 0 e1)) "ARC")
            (xarc)
           )
           ((= (cdr (assoc 0 e1)) "CIRCLE")
            (xcircle)
           )
           (T (prompt "\nUnsuitable entity selected. "))
    )
    (setq e (entsel "\nSelect entity to conceal "))
  )
  (moder)
  (setvar "highlight" 1)
  (setvar "blipmode" 1)

  (princ)
)
