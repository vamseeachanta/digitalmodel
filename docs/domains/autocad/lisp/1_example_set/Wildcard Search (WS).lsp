

;;----------------------------------
;; Title    : Wildcard Search Lisp            
;; Command  : WS and WS?
;; www.cadlispandtips.com
;;----------------------------------                      



(defun c:WS ( / name ) 

 ;;; input Text to Select
  (if (not namef) (setq namef "1*,A*"))
  (setq name (getstring (strcat "\nType Text Content <" namef ">: ")))
  (if (= name "") (setq name namef) (setq namef name))
  (setq TX (ssget "_X" (list '(0 . "TEXT,MTEXT") (cons 1 name))))
  (sssetfirst nil TX)
  (princ)
)
(princ "\nLisp Command: WS ,For help : WS?")
(princ)

(alert "----------------------------- Wildcard Search Lisp ------------------------------

\n Command : WS and WS?

\n CHARACTER'S DEFINITIONS:
\n #   (Pound) Matches any numeric digit. 
\n @   (At) Matches any alphabetic character. 
\n .   (Period) Matches any non-alphanumeric character. 
\n *   (Asterisk) Matches any string and can be used anywhere in the search string. 
\n ?   (Question mark) Matches any single character; ex: ?BC matches ABC, 3BC.   
\n ~   (Tilde) Matches anything; ex: ~*AB*matches all strings that don’t contain AB. 
\n [ ]  Matches any one of the characters enclosed; ex: [AB]C matches AC and BC. 
\n [~]  Matches any character not enclosed; ex: [~AB]C matches XC but not AC. 
\n [-]  Specifies a range; ex: [A-G]C matches AC, BC, and so on to GC. 
\n `   (Reverse quote) Reads the next character literally; ex: `~AB matches ~AB.



\nBijoy manoharan\nApr 2016\nwww.cadlispandtips.com")


(defun c:WS? ()
(alert "--------------------------- CHARACTER'S DEFINITIONS ---------------------------

\n #   (Pound) Matches any numeric digit. 
\n @   (At) Matches any alphabetic character. 
\n .   (Period) Matches any non-alphanumeric character. 
\n *   (Asterisk) Matches any string and can be used anywhere in the search string. 
\n ?   (Question mark) Matches any single character; ex: ?BC matches ABC, 3BC.   
\n ~   (Tilde) Matches anything; ex: ~*AB*matches all strings that don’t contain AB. 
\n [ ]  Matches any one of the characters enclosed; ex: [AB]C matches AC and BC. 
\n [~]  Matches any character not enclosed; ex: [~AB]C matches XC but not AC. 
\n [-]  Specifies a range; ex: [A-G]C matches AC, BC, and so on to GC. 
\n `   (Reverse quote) Reads the next character literally; ex: `~AB matches ~AB.
\n
\n Command : WS 

\nBijoy manoharan\nApr 2016\nwww.cadlispandtips.com")

)
(princ)