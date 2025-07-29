(defun DTR (A) (* PI (/ A 180.0)))
(defun c:DIMROTATE (/ DIM_CNT DIM_SS1 DIM_OBJ DIM_ANG)
(command ".undo" "group")
(setq DIM_CNT 0
DIM_SS1 (ssget '((0 . "DIMENSION")))
)
(if DIM_SS1
(progn
(repeat (sslength DIM_SS1)
(setq DIM_OBJ (entget (ssname DIM_SS1 DIM_CNT)))
(setq DIM_ANG (- (cdr (assoc 51 DIM_OBJ)) (DTR 180)))
(setq DIM_OBJ (subst (cons 51 DIM_ANG) (assoc 51 DIM_OBJ) DIM_OBJ))
(entmod DIM_OBJ)
(princ (strcat "\rRotating dimension "
(itoa DIM_CNT)
" of "
(itoa (sslength DIM_SS1))
)
)
(setq DIM_CNT (1+ DIM_CNT))
)
(princ (strcat "\rRotating dimension "
(itoa DIM_CNT)
" of "
(itoa (sslength DIM_SS1))
)
)
)
)
(command ".undo" "end")
(princ)
)