(defun c:setlayer (/ va)
 (setvar "cmdecho" 0)
 (setq va (cdr (assoc 8 (entget
                         (car (entsel "Pick entity to set layer: "))))))
 (command "layer" "set" va "")
)
