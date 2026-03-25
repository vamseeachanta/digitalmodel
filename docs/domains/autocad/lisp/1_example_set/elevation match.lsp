;to take object to required elevation by typing value

(dEFUN C:E3()
(SETQ JB(GETREAL "ENTER ELEVATION FOR POINT: ")
      JM(SSGET))
(COMMAND "CHANGE" JM "" "P" "E" JB "")
(COMMAND "ATTEDIT" "" "" "" "" PAUSE "" "V" "R" (RTOS JB 2 3) ""))