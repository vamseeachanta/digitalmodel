;;;This WSNUM.LSP routine enables you to enter a prefix for sequentially
;;;numbered text which uses the Dtext routine to place text on the current
;;;layer in the current text style.
;;;
;;;This file has been edited from the 'cabnum.lsp' routine
;;;originally created by Elise Moss of Moss Designs.
;;;www.mossdesigns.com, August 2001
;;;
;;;
;;;Michael E. Beall
;;;www.autocadtrainerguy.com, January 2002
;;;502.633.3994 (voice + FAX)

(defun c:wsnum	()
					; get first number 
  (setq prefix (getstring "\n Prefix <WS>? "))
  (if (= prefix "")
    (setq prefix "WS")
  )
  (setq stnum (getint "\nStarting number? "))
  (setq label (strcat prefix " " (itoa stnum)))
  (setq incrnum (getreal "\nIncrement numbers by <1>? "))
  (if (= incrnum nil)
    (setq incrnum 1)
  )
  (setq txtht (getreal "\n Set textheight to: "))
  (setq placepoint (getpoint "\Select text location: "))
  (command "text" placepoint txtht "0" label)
  (setq stnum (+ stnum incrnum))
  (setq label (strcat prefix " " (itoa stnum)))
  (setq placepoint (getpoint "\Select text location: "))
  (while (/= placepoint nil)
    (command "text" placepoint txtht "0" label)
    (setq stnum (+ stnum incrnum))
    (setq label (strcat prefix " " (itoa stnum)))
    (setq placepoint (getpoint "\Select text location: "))
  )					;end while
  (princ)
)					; end defun
(prompt "\nType wsnum to run. ")

