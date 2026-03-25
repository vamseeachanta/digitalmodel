(defun c:TERS (/ cscss)(setq cscss (getvar "CMDECHO"))(setvar "CMDECHO" 0)
(COMMAND "_.-VBARUN" "TERSECTPRO2X")(setvar "CMDECHO" cscss)(PRINC))
(defun c:TERY (/ cscss)(setq cscss (getvar "CMDECHO"))(setvar "CMDECHO" 0)
(COMMAND "_.-VBARUN" "INTERPOLATEABCX")(setvar "CMDECHO" cscss)(PRINC))