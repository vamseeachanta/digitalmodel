;**************************************************************************** 
;                                                                             
;             SCREEN1                                                         
;                     ษออออออออออออออออออออออออออออออป                        
;                     บ                              บ                        
;                     บ                              บ                        
;                     บ     ANSI KEY PROGRAMMING     บ                        
;You should exit      บ                              บ                        
;using bs keyออ>      บUse the 'BACKSPACE' key to endบ                        
;                     บ                              บ                        
;                     บ                              บ                        
;                     บ     By Walt Craig 9/08/89    บ                        
;                     บ      Demo for AutoLISP.      บ                        
;                     บ                              บ                        
;                     บ                              บ                        
;                     ศออออออออออออออออออออออออออออออผ                        
;                                                                             
;                                                                                
;                                                                                
;Strike the key to redefine: 1 > {here you might hit alt-t}                      

;**************************************************************************** 
;                                                                             
;             SCREEN2                                                         
;                     ษออออออออออออออออออออออออออออออป                        
;                     บ                              บ                        
;                     บ                              บ                        
;                     บ     ANSI KEY PROGRAMMING     บ                        
;Just hit return      บ                              บ                        
;if you don't want    บUse the 'BACKSPACE' key to endบ                        
;to redefine this     บ                              บ                        
;key.                 บ                              บ                        
;                     บ     By Walt Craig 9/08/89    บ                        
;                     บ      Demo for AutoLISP.      บ                        
;                     บ                              บ                        
;                     บ                              บ                        
;                     ศออออออออออออออออออออออออออออออผ                        
;                                                                             
;                                                                                
;String to replace with: TRIM SI AUTO 
;                                    ^
;                                    |
;                Note: add a trailing space for <enter>
;  At this point SCREEN1 will reappear. Hit the backspace key and alt-t will
;now have this command.
;  I never did figure out how one could have a ' " ' in redefinition.
;(i.e. AutoLISP loads ect.). If you know...tell me please...curious.
;  You should edit this program to your liking..colors..prompts..ect.
;Have fun.
;
;****************************************************************************
;					
;by Walt Craig - Just for the fun of it.
;		 Final Edit 9/30/89
;
;ANSI - This program is an example of the power given the user with ansi.sys
;	installed.  The program allows the user to program function keys 2 -
;	5 (disallowing Autocad's overides) and all Alt-alpha combinations.
;
;	The user should remember that Autocad uses the space bar as an enter
;	key. 
;
;      Example:  ===>TRIM SI AUTO 
;*****************************************************************************
(defun c:ansi(/ len line rep index rst asc errst)
(setq olderr *error* *error* anserr)
(textscr)
(bright)
(drawbox)
(write-line(strcat(chr 27)"[3;"(chr 34)" "(chr 34)"p"))
(if asclst
	(progn
	(unload (car asclst))
	(setq len(length asclst))
	(repeat (- len 1)
		(setq asclst(cdr asclst))
		(unload (car asclst))
	);end repeat
	(setq asclst nil)
	);end progn
);end if
(setq index 1 line "\r                                                                               " 
	rep (strcat line "\rString to replace with: " )rst '(2 1) errst
	"******INVALID KEY...TRY AGAIN*******")
(while rst
	(prod3 "22" "1")
	(princ line)(princ "\rStrike the key to redefine: ")(princ index)(princ " >")
	(setq rst(grread))
	(if(and(>= (cadr rst) 144)(<= (cadr rst) 191))
		(progn ;valid range of change
		(setq asc(cadr rst)quate (chr 34))
		(setq asc(- asc 128))
		(setq asc(rtos asc 2 0))
		(if(= index 1)(setq asclst(list asc))(setq asclst(append asclst(list asc))))
		(prod3 "21" "1")(rv)(prod5 line)
		(normal)(bright)
		(prod3 "22" "1")
		(if(= (car rst) 2)(setq string(getstring t rep)))
		(if(/= string "")
		(write-line(strcat(chr 27)"[0;"asc";"quate string quate "p"))
		)
		(prin1)	
        	(setq index(1+ index))
		);end of range check
		(progn		
		(if(and(= (car rst)2)(= (cadr rst)8))(setq rst nil)
 			(progn
 			(blink)
			(rv)
			(prod3 "21" "1")
			(prod5 errst)
			(normal)
 			(bright)
 			);end progn
		);end of invalid range
	);end of if range test
);end of the repeat
)
(prompt "\e[1m")
(prompt "\e[44m\n\n")
(setq *error* olderr)
(cls) 
(write-line(strcat(chr 27)"[3;3p"))
(prod3 "14" "1")
(prod5 "End program.")
(princ "\n \n \n ")
;(bright)
(prin1)
);end c:ansi
(defun unload(asc)
(write-line(strcat(chr 27)"[0;"asc";0;"asc"p"))
)
(defun cls()(textscr)(prompt "\e[2J")(princ))
(defun bright()(textscr)(prompt "\e[0m")(princ))
(defun rv()(textscr)(prompt "\e[7m")(princ))
(defun blink()(textscr)(prompt "\e[5m")(princ))
(defun normal()(textscr)(prompt "\e[0m")(princ))
(defun prod1()(prompt "\e[s")(princ))	
(defun prod2()(prompt "\e[u")(princ))
(defun prod3(y x)(prompt (strcat "\e[" y ";" x "H"))(princ))	
(defun prod4(no dir)(prompt  (strcat "\e[" NO DIR))(princ))	
(defun prod5(str / str2)(prompt "\e[;1A")(setq str2(itoa(- 40(/(strlen str)2))))
(prompt(strcat "\e[" str2 "C"))(prompt str)(princ)
)
(defun note()
(cls)
(prod3 "9" "1")
(prod5 "ANSI KEY PROGRAMMING")
(prod3 "11" "1")
(prod5 "Use the 'BACKSPACE' key to end")
(prod3 "14" "1")
(prod5 "By Walt Craig 9/08/89")
(prod3 "15" "1")
(prod5 "Demo for AutoLISP.")
)
(defun prod6(code)(prompt (chr code)))
(defun prod7()(prod3 (itoa y)(itoa x)))
(defun prod8(y x lcode rcode rep repcode)
(prod7)
(prod6 lcode)
(repeat rep
	(setq x(1+ x))
	(prod7)
	(prod6 repcode)
)
(prod6 rcode)
)
(defun prod9(y x rep repcode)
(prod7)
(prod6 repcode)
(repeat rep
	(setq y(1+ y))
	(prod7)
	(prod6 repcode)
)
)
(defun cblue(str)(textscr)(prompt "\e[34m")(prompt str)(princ))
(defun drawbox()
(rv)
(note)
(bright)
(cblue "")
(prod8 5 25 201 187 30 205)
(prod9 6 25 10 186)
(prod9 6 56 10 186)
(prod8 17 25 200 188 30 205)
(princ)
(normal)
(bright)
)
(defun anserr(st)
(setq *error* olderr)
(write-line(strcat(chr 27)"[3;3p"))
(prompt "\e[44m\n\n")
(princ st)
)
(prin1)
