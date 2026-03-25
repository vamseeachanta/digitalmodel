; Automatic coordinate labeling
; Edwin Prakoso
; http://cad-notes.com
;
; Limitation
; ----------
; Will use current leader style and current units setting

(defun c:lb (/  p x y ptcoord textloc)
  (while
    (setq p (getpoint "\nPick Point: "))
    (setq textloc (getpoint "\nPick Label Location: "))
    (setq x (rtos (car p)))
    (setq y (rtos (cadr p)))
    (setq ptcoord (strcat x ", " y))
    (command "_LEADER" p textloc "" ptcoord "")
  )
)