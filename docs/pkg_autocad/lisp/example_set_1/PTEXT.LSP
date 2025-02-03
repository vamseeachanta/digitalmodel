;;;   PText.lsp  
;;;   (C) Copyright 1988-1992 by Autodesk, Inc.
;;;    
;;;   This program is copyrighted by Autodesk, Inc. and is  licensed
;;;   to you under the following conditions.  You may not distribute
;;;   or  publish the source code of this program in any form.   You
;;;   may  incorporate this code in object form in derivative  works
;;;   provided  such  derivative  works  are  (i.) are  designed and
;;;   intended  to  work  solely  with  Autodesk, Inc. products, and
;;;   (ii.)  contain  Autodesk's  copyright  notice  "(C)  Copyright
;;;   1988-1992 by Autodesk, Inc."
;;;
;;;   AUTODESK  PROVIDES THIS PROGRAM "AS IS" AND WITH  ALL  FAULTS.
;;;   AUTODESK  SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF  MER-
;;;   CHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,  INC.
;;;   DOES  NOT  WARRANT THAT THE OPERATION OF THE PROGRAM  WILL  BE
;;;   UNINTERRUPTED OR ERROR FREE.
;;;   
;;;   by Jan S. Yoder  
;;;   with thanks to Kieran McKeogh for suggesting how to handle control  
;;;   characters.  
;;;   15 February 1990  
;;;
;;;   Version 1.11
;;;     29 January 1991 -- JSY  : More minor bug fixes.
;;;     11 January 1991 -- JSY  : Numerous minor bug fixes.
;;;     
;;;----------------------------------------------------------------------------  ;;;   
;;; DESCRIPTION
;;;   PTEXT -- Paragraph text processor.
;;;   
;;;   Text can be entered directly on the AutoCAD text screen, in one of four
;;;   modes; Left justified, Center or Right justified, or Fit between two 
;;;   line endpoints.  Word wrapping will occur based on some rudimentary 
;;;   assumptions which are necessary until and if a function can be provided
;;;   for determining the actual size of a text item at any given font and
;;;   number and size of characters.
;;;   
;;;   The actual number of characters of "slack", the number of allowable extra
;;;   characters beyond the predetermined maximum, can be set by the user.
;;;   
;;;   This processor works by reading keyboard input via (grread) and based
;;;   on this input, causing the current text entity to be regenerated.  This
;;;   routine can probably be made to operate unacceptably slowly by doing 
;;;   one or more of the following:
;;;   
;;;     Operating the routine in multiple viewports where the text entities'
;;;     layer is ON in all of them.
;;;     
;;;     Working on fairly long text strings;  say, greater than 30 characters.
;;;     
;;;     Operating on a slow processor.
;;;   
;;;   The best method is to work on a layer which is exclusively visible in
;;;   the current viewport, and on fairly short strings.
;;;
;;;   The options are:
;;;   
;;;    Command: ptext  
;;;    Center/Edit/Fit/Load-file/Right/Slack/<Start point>:   
;;;         
;;;  Left, Center, right, and Fit justified text entry types are supported   
;;;  for text entry.  The editing portion of this routine should work on  
;;;  all of the Release 11 justification options.  This has not been tested!  
;;;      
;;;  The following control characters allow a "cursor" composed of a set of  
;;;  underline control codes to move around within a set of text entities.  
;;;    
;;;       ^A -- Append a space after the current cursor position and  
;;;             move the cursor to that position.  
;;;       ^B -- Go to the beginning of the line.  
;;;       ^D -- Move the cursor down a line; maintains the current letter   
;;;             position.  This position may appear to be different due to  
;;;             character kerning within a font.  
;;;       ^E -- Go to the end of the current line.  
;;;       ^H -- Backspace key.  
;;;       ^I -- Toggle insert/overwrite mode.  
;;;       ^L -- Move the cursor to the left  -- non-destructive cursor.  
;;;   RETURN -- Return; move any characters to the right of the cursor  
;;;             down to the next line and push the remaining lines down  
;;;             one "interline spacing" amount.  
;;;       ^N -- Go to the end of the last text entity in the list.  
;;;       ^R -- Move the cursor to the right -- non-destructive cursor.  
;;;       ^T -- Go to the start of the first text entity in the list.  
;;;       ^U -- Move the cursor up a line; maintains the current letter   
;;;             position.  This position may appear to be different due to  
;;;             character kerning within a font.  
;;;       ^Z -- Exit text entry.  
;;;       -  -- Hyphen character.  
;;;          -- The delete key deletes the current character, and if there is  
;;;             no character and the cursor is at the end of a line and there  
;;;             are more lines, then the next line is pulled up onto the   
;;;             current line and any remaining lines are moved up one   
;;;             "interline spacing" amount.  
;;;     
;;;   ^U and ^X are interchanged between DOS and UNIX machines due to low  
;;;   level character swapping by the operating system, so either of these  
;;;   combinations will cause the cursor to move up a line on either   
;;;   machine type.  
;;;   
;;;   Local variables representing key code values for translation   
;;;   to other keyboard codes, if necessary, are listed at the top   
;;;   of the (ptext) defun.  
;;;  
;;;----------------------------------------------------------------------------  
  
  
(defun ptext (/ pt_ver pt_err        pt_oe  pt_oce pt_sty pt_twf   
                char   insert grp_72 pt_spt pt_rpt str          
                line_l pt_cl  pt_str cont   sset   j      TX:LST ent          
                pt_ils cont1  ans    temp   sl     pt_rsp pt_msp pt_tsp   
                return OK2BRK max_j  diff   nchars dir    dstrct pt_obm  
                pt_te EDIT_T  pnding pt_dth  
                P_SLCK P_BEGL P_HLPD P_HLPU P_DWNL P_ENDL P_BACK   
                P_ISRT P_JUST P_LEFT P_RTRN P_ENDT P_RGHT P_BEGT   
                P_UPLD P_UPLU P_QUIT P_SPCE P_HYPH P_DDEL P_UDEL  
             )  
  
  (setq pt_ver "1.11")                ; Reset this local if you make a change.  
  
  ;;  
  ;; Internal error handler defined locally  
  ;;  
  
  (defun pt_err (s)                   ; If an error (such as CTRL-C) occurs  
                                      ; while this command is active...  
    (if (/= s "Function cancelled")  
      (if (= s "quit / exit abort")  
        (princ)  
        (princ (strcat "\nError: " s))  
      )  
    )  
    (if (null pt_GEX)
      (progn
        (entdel (cdr(assoc -1 pt_te))) ; Delete the test text entity.
        (entmod (subst (cons 1 str) (assoc 1 ent) ent))
      )
    )
    (command "_.UNDO" "_END")  
    (if ll_oe                         ; If an old error routine exists  
      (setq *error* pt_oe)            ; then, reset it   
    )  
    (setvar "blipmode" pt_obm)        ; Restore blipmode  
    (setvar "cmdecho" pt_oce)         ; Reset command echoing on error  
    (princ)  
  )  
    
  (if *error*                         ; Set our new error handler  
    (setq pt_oe *error* *error* pt_err)   
    (setq *error* pt_err)   
  )  
  (setq pt_oce (getvar "cmdecho"))    ; Save current state of command echoing  
  (setq pt_obm (getvar "blipmode"))   ; Save current state of blipmode  
  (setvar "cmdecho" 0)                ; Turn off command echoing  
  (setvar "blipmode" 0)               ; Turn off blipmode  
  (command "_.UNDO" "_GROUP")  
  
  (if (null pt_GEX)  
    (progn
      (cond
        ((or (= (substr (getvar "PLATFORM") 1 4) "OS/2")   
             (= (substr (getvar "PLATFORM") 1 3) "DOS"))   
          (or
            (setq temp (findfile "ptext.exe"))
            (setq temp (findfile "ads/ptext.exe"))
          ) 
        )
        ((= (substr (getvar "PLATFORM") 1 7) "386 DOS")   
          (or
            (setq temp (findfile "ptext.exp"))
            (setq temp (findfile "ads/ptext.exp"))
          ) 
        )
        (T
          (or
            (setq temp (findfile "ptext"))
            (setq temp (findfile "ads/ptext"))
          ) 
        )
      )  
      (if temp
        (if (null (xload temp))  
          (progn  
            (princ "\nCouldn't load the Ptext executable. ")  
            (if (and pt_sup pt_uls get_ch pt_gll pt_cll pt_psl
                     is_num pt_csl pt_gpl pt_gpr pt_sjk pt_mc
                     pt_bul pt_sil pt_ael pt_ail pt_ats pt_pnl
                     pt_pl pt_gmp pt_waw pt_pww pt_pee pt_mne
                     pt_fww pt_dal dr_txt round)
              
              (princ (strcat "\nRunning the AutoLisp version "  
                "for demonstration purposes only. "))  
              (progn  
                (princ "\nThis version of Ptext.lsp requires it. ")  
                (exit)  
              )
            )
          )
          (princ "\n\n\nRunning the ADS version of PTEXT. ") 
        )  
        (if (and pt_sup pt_uls get_ch pt_gll pt_cll pt_psl
                 is_num pt_csl pt_gpl pt_gpr pt_sjk pt_mc
                 pt_bul pt_sil pt_ael pt_ail pt_ats pt_pnl
                 pt_pl pt_gmp pt_waw pt_pww pt_pee pt_mne
                 pt_fww pt_dal dr_txt round)
                    
          (progn  
            (princ "\nCouldn't find the Ptext executable. ")  
            (princ (strcat "\nRunning the AutoLisp version "  
              "for demonstration purposes only. "))  
          )
          (progn  
            (princ "\nCouldn't find the Ptext executable. ")  
            (princ "\nThis version of Ptext.lsp requires it. ")  
            (exit) 
          )
        )  
      ) 
    )
    (princ "\n\n\nRunning the ADS version of PTEXT. ") 
  )  
  ;; These are the machine codes for the various cursor motion controls
  ;; for both UNIX and DOS which are reported by calls to (ads_grread).  
  ;; 
  ;; Note that UNIX machines can redefine the low level codes and the 
  ;; actions of the keys marked "DELETE" and the backspace key, as well
  ;; others.  They may not operate as documented.  Check the codes returned
  ;; by (grread) "key press" against the table below to determine which
  ;; codes to change for your system.
  ;; 
  ;; They may need translation for other country keyboard codes.
  ;;
  ;; The codes listed here are used only if the PTEXT executable is not
  ;; in use.  Change the source code for the executable if you wish to
  ;; change the response of any of the keys and recompile it as described
  ;; in the ADS User Guide.
  
  (if (null pt_GEX)  
    (setq P_APPN   1                  ; ^A Append a space at current pt_rsp.  
          P_BEGL   2                  ; ^B Beginning of line.  
          P_DWNL   4                  ; ^D Down a line.  
          P_ENDL   5                  ; ^E End of line.  
          P_BACK   8                  ; ^H Backspace.  
          P_ISRT   9                  ; ^I Toggle insert/overwrite mode.  
          P_LEFT  12                  ; ^L Left  -- non-destructive cursor.  
          P_RTRN  13                  ;    Return.  
          P_ENDT  14                  ; ^N End of text.  
          P_RGHT  18                  ; ^R Right -- non-destructive cursor.  
          P_BEGT  20                  ; ^T Beginning of text.  
          P_UPLD  21                  ; ^U Up a line (DOS).  
          P_UPLU  24                  ; ^X Up a line (UNIX).  
          P_QUIT  26                  ; ^Z Exit text entry.  
          P_SPCE  32                  ;    Spacebar.  
          P_HYPH  45                  ; -  Hyphen character.  
          P_UDEL 127                  ;    Delete character (UNIX).  
          P_DDEL 211                  ;    Delete character (DOS).  
          P_HLPD 222                  ;^F1 Help screen on DOS  
          P_HLPU  31                  ;^?  Help screen on UNIX  
    )  
  )         
  (setq pt_sty (tblsearch "style" (getvar "textstyle"))  
        pt_dth (cdr(assoc 40 pt_sty))  
        pt_twf (cdr(assoc 41 pt_sty))  
        pt_toa (cdr(assoc 50 pt_sty)) 
        pt_stn (cdr(assoc  2 pt_sty)) 
        P_SLCK 0                      ; No slack characters  
        char   P_SPCE                 ; "space" character  
        insert T                      ; Start in insert mode.  
  )  
  
  ;; Make a frozen layer for determining the length of a text string.  
  (if (null (setq temp (tblsearch "layer" "frozen_text")))  
    (command "_.LAYER" "_NEW" "FROZEN_TEXT" "_FREEZE" "FROZEN_TEXT" "")  
    (if (= (logand (cdr(assoc 70 temp)) 1) 0)  
      (command "_.LAYER" "_FREEZE" "FROZEN_TEXT" "")  
      (princ)  
    )  
  )  
  (if (= (getvar "handles") 0)        ; Handles are off  
    (progn  
      (initget "Yes No")  
      (if (= (getkword "\nTurn On handles?  <Y>: ") "No")  
        (progn  
          (princ   
            "\nHandles must be turned ON for this routine to continue. ")  
          (exit)  
        )  
        (command "_.HANDLES" "_ON")  
      )  
    )  
  )  
  (if (null pt_GEX)  
    (progn  
      ;; Create a dummy text entity on this layer.  
      (command "_.TEXT" "_R" "0,0" "" "" "Ptext")  
      (setq pt_te (entget(entlast))  
            pt_te (subst (cons 8 "frozen_text") (assoc 8 pt_te) pt_te)  
      )  
      (entmod pt_te)  
    )  
  )  
  
  (princ (strcat "\nThe Paragraph Text Editor, Version "  
                 pt_ver
                 ", (c) 1990 by Autodesk, Inc. "))  
  (pt_opt)                            ; Get options from user  
  (if (null EDIT_T)  
    (progn  
      (pt_sth)                        ; Set text height.  
      (if (/= grp_72 5)  
        (pt_sra)                      ; Set rotation angle.  
        (setq pt_ra (angle pt_spt pt_rpt))  
      )  
      (setq pt_ils (pt_sis))          ; Set the spacing between lines.  
      (if (/= grp_72 5)  
        (pt_sml)                      ; Set the length of the lines.  
        (setq pt_mll (distance pt_spt pt_rpt))  
      )  
    )  
  )  
  (if (null str)  
    (setq str T)  
  )        
  (grtext -2 "Ptext: Insert mode")  
  (if (null pt_GEX)  
    (progn  
      (setq line_l 0    
            pt_cl  0   
            pt_str ""  
      )  
      (while (/= str "")  
        (pt_sup)                      ; Set up  
        (while (get_ch)               ; Get characters  
  
          ;; Maximum line length plus "slack" amount not yet reached...  
          
          (if (< line_l pt_mll)  
            (pt_pl)                   ; Process the line  
            (progn  
              (setq temp (pt_waw))    ; Set up to wrap at word  
              (pt_pww temp)           ; Wrap at word  
            )  
          )  
          (setq char nil)  
        )  
      )  
    )  
    (progn  
      (setq EDIT_T (if (null EDIT_T) 0 1))  
      (if (null pt_rpt) (setq pt_rpt (list 0.0 0.0 0.0)))
      (setq pt_sty (cdr(assoc 2 pt_sty)))
      (setq err  
        (pt_GEX pt_spt pt_rpt grp_72 pt_th  pt_ra   
                pt_mll pt_ils (atof pt_ver) pt_sty EDIT_T)   
      )  
    )  
  )  
  (if (null pt_GEX)  
    (entdel (cdr(assoc -1 pt_te)))    ; Delete the test text entity.  
  )  
  (command "_.UNDO" "_END")  
  (setvar "blipmode" pt_obm)          ; Restore blipmode  
  (setvar "cmdecho" pt_oce)           ; Restore command echoing  
  (princ)  
)  
;;;  
;;; Get options  
;;;  
;;; pt_opt == PText_OPTions  
;;;  
(defun pt_opt (/ cont)  
  (setvar "cmdecho" 0)  
  (while (null cont)  
      
    (setq cont T)   
  
    (initget "Center Edit Fit Load-file Right Slack ?")  
    (setq pt_spt (getpoint   
      "\nCenter/Edit/Fit/Right/Slack/?/<Start point>: "))  
    
    (cond   
      ((= pt_spt "Center")  
        (setq grp_72 1)  
        (initget 1)  
        (setq pt_spt (getpoint "\nCenter baseline point: "))  
        (setq pt_spt (trans pt_spt 1 0))  
    
      )  
      ((= pt_spt "Edit")  
        (princ (strcat   
          "\nSelect the text for editing.  Start with the first line"  
          "\nof the paragraph, and select the lines in order... "))  
        (setq sset (ssget))  
        (if sset  
          (progn  
            (setq j 0)  
            (setq temp   (entget (ssname sset j))  
                  ename1 (cdr(assoc -1 temp))  
                  k      (cdr(assoc 210 temp))  
                  fd     (open "ptext.hdl" "w")  
            )  
            (if (null fd)
              (progn
                (princ 
                  "\nCouldn't open handle file \"ptext.hdl\" for writing. ")
                (exit)
              )
            )
            (if (> (sslength sset) 1)  
              (setq temp   (entget (ssname sset (1+ j)))  
                    ename2 (cdr(assoc -1 temp))  
              )  
            )  
              
            (repeat (sslength sset)  
              (if (null pt_GEX)  
                (progn  
                  (if (= (cdr(assoc 0 (entget (ssname sset j)))) "TEXT")  
                    (setq TX:LST (if TX:LST   
                                   (append TX:LST (list (ssname sset j)))  
                                   (list (ssname sset j))  
                                 )  
                          j      (1+ j)  
                          gottxt T
                    )  
                  )  
                )  
                (progn  
                  (if (and  
                        (= (cdr(assoc 0 (entget (ssname sset j)))) "TEXT")  
                        (equal (cdr(assoc 210 (entget (ssname sset j)))) k)  
                      )  
                    (progn  
                      (if (> j 0) (princ "\n" fd))  
                      (princ (cdr(assoc 5 (entget (ssname sset j)))) fd)
                      (setq gottxt T)
                    )  
                  )  
                  (setq j      (1+ j))  
                )  
              )  
            )  
            (if gottxt
              (progn
                (if (null pt_GEX)  
                  (setq ent    (entget(nth 0 TX:LST)))  
                  (progn  
                    (close fd)  
                    (setq ent    (entget(ssname sset 0)))  
                  )  
                )  
                (setq pt_spt (cdr(assoc 10 ent))  
                      str    (cdr(assoc 1  ent))  
                      pt_th  (cdr(assoc 40 ent))  
                      pt_ra  (cdr(assoc 50 ent))  
                      grp_72 (cdr(assoc 72 ent))  
                )  
                (if (= grp_72 5)          ; Fit text  
                  (setq pt_rpt (cdr(assoc 11 ent))  
                        pt_mll (distance pt_spt pt_rpt)  
                  )  
                  (setq pt_mll (pt_sml))  ; Set maximum line length  
                )  
                (setq pt_ils (pt_gis)     ; Get interline spacing.  
                      EDIT_T T            ; Set a flag that we are editing.  
                )  
              )
              (progn
                (setq cont nil)
                (princ "\nNo text entities selected. ")
              )
            )
          )  
          (progn  
            (setq cont nil)           ; Repeat the first prompt.  
            (princ "\nNo text entities selected. ")  
          )  
        )  
      )  
      ((= pt_spt "Fit")  
        (setq grp_72 5)  
        (initget 1)  
        (setq pt_spt (getpoint "\nLeft baseline point: "))  
        (initget 1)  
        (setq pt_rpt (getpoint pt_spt "\nRight baseline point: "))  
        (setq char nil)  
        (setq pt_spt (trans pt_spt 1 0))  
        (setq pt_rpt (trans pt_rpt 1 0))  
      )  
      ((= pt_spt "Right")  
        (setq grp_72 2)  
        (initget 1)  
        (setq pt_spt (getpoint "\nRight baseline point: "))  
        (setq pt_spt (trans pt_spt 1 0))  
    
      )  
      ((= pt_spt "Slack")  
        (setq cont nil)  
        (if (null P_SLCK) (setq P_SLCK 1))  
        (initget 4)  
        (setq pt_spt (getint (strcat  
          "\nNumber of characters of slack <" (itoa P_SLCK) ">: ")))  
        (if pt_spt (setq P_SLCK pt_spt))  
      )  
      ((= (type pt_spt) 'LIST)        ; A point was entered  
        (setq grp_72 0)  
        (setq pt_spt (trans pt_spt 1 0))  
    
      )  
      ((= pt_spt "?")  
        (pt_phs T)  
        (setq cont nil)  
      )  
      (T  
        (command "_.UNDO" "_END")  
        (exit)                        ; Null entry -- get out.  
    
      )  
    )  
  )  
)  
;;;  
;;; The help screen.  
;;;  
;;; pt_phs == PText_Print_Help_Screens  
;;;  
(defun pt_phs (temp)  
  (if textpage (textpage) (textscr))  
  (if temp  
    (progn  
      (princ 
        "\nText may be entered in one of four modes; Left, Center, Right,")  
      (princ "\nor Fit justified.  You may move the cursor to any position")  
      (princ "\nwithin the text you are entering at any time while entering")  
      (princ 
        "\ntext, or you may edit existing lines of text by selecting the")  
      (princ "\nEdit option, and picking the lines of text you wish to edit.")  
      (princ "\n")  
      (princ 
        "\nYou must pick the text entities in the correct order that you ")  
      (princ "\nwant them to be edited, as entering additional text on one")  
      (princ 
        "\nline may cause the line to be \"wrapped\" onto the next line ")  
      (princ "\nthat you selected.")  
      (princ "\n  ")  
      (princ 
        "\nThe following control characters allow a \"cursor\" composed ")  
      (princ "\nof a set of underline and/or overline control codes to move ")  
      (princ "\naround within a set of text entities.")  
      (princ "\n")  
      (princ "\n<more>")  
      (grread)  
      (princ "\r        ")  
    )  
  )  
  (if temp  
    (progn  
      (princ "\n     ^F1 (DOS) or ")  
      (princ "\n     ^?  (UNIX) -- This help screen. ")  
    )  
  )  
  (princ "\n")  
  (princ "\n     ^A  -- (A)ppend a space after the current cursor position ")  
  (princ "\n            and move the cursor to that position.")  
  (princ "\n     ^B  -- Move the cursor to the (B)eginning of the line.")  
  (princ 
    "\n     ^D  -- Move the cursor (D)own a line; maintains the current ")  
  (princ "\n            letter position.  This position may appear to be ")  
  (princ "\n            different due to character kerning within a font.")  
  (princ "\n     ^E  -- Go to the (E)nd of the current line.")  
  (princ "\n     ^H  -- Backspace key.")  
  (princ "\n     ^I  -- Toggle (I)nsert/overwrite mode.")  
  (princ "\n     ^L  -- Move the cursor to the (L)eft.")  
  (if temp  
    (progn  
      (princ "\n RETURN  -- Return; move any characters to the right of the ")  
      (princ "\n            cursor down to the next line and push the ")  
      (princ "\n            remaining lines down by the \"interline space\" ")  
      (princ "\n            distance.")  
    )  
  )  
  (princ "\n     ^N  -- Move the cursor to the e(N)d of ")  
  (princ "\n            the last text entity in the list.")  
  (princ "\n     ^R  -- Move the cursor to the (R)ight.")  
  (princ "\n     ^T  -- Move the cursor to (T)op or the start of ")  
  (princ "\n            the first text entity in the list.")  
  (princ "\n     ^U  -- Move the cursor (U)p a line.")  
  (princ "\n     ^Z  -- Exit text entry.")  
  (princ "\n")  
  (if (null temp)  
    (progn  
      (princ "\nPress any key to return to your drawing.")  
      (grread)  
      (princ "\r                                          ")  
      (princ "\n")  
      (princ "\n")  
      (princ "\nText: ")  
      (princ str)  
    )  
  )  
  (if temp  
    (progn  
      (princ "\n<more>")  
      (grread)  
      (princ "\r        ")  
      (princ 
        "\n DELETE  -- The delete key deletes the current character.  If")  
      (princ 
        "\n            the cursor is at the end of a line and there are ")  
      (princ "\n            more lines in the list, then the next line is ")  
      (princ 
        "\n            pulled up onto the current line and any remaining ")  
      (princ "\n            lines are moved up by the \"interline space\" ")  
      (princ "\n            distance.")  
      (princ "\n ")  
      (princ 
        "\nInserting a hyphen character in a word allows that word to be")  
      (princ "\n\"wrapped\" at the hyphen.")  
      (princ "\n")  
    )  
  )  
  (princ)  
)  
;;;  
;;; Set the height of the text entities.  
;;; Defaults to "0.2" if not preset in the style symbol table.  
;;;  
;;; pt_sth == PText_Set_Text_Height  
;;;  
(defun pt_sth ()  
  (initget 6)  
  (if (= pt_dth 0.0) (setq pt_dth 0.2))
  (setq ans (getdist (trans pt_spt 0 1) (strcat "\nText height <"   
                                    (if pt_th (rtos pt_th) (rtos pt_dth))   
                                    ">: ")))  
  (if ans  
    (setq pt_th ans)  
    (if (null pt_th)   
      (setq pt_th 0.2)  
    )  
  )  
)  
;;;  
;;; Set the rotation angle for the text.  
;;; Defaults to "0" if not preset in the style symbol table.  
;;;  
;;; pt_sra == PText_Set_Rotation_Angle  
;;;  
(defun pt_sra ()  
  (if (= (logand (cdr(assoc 70 pt_sty)) 4) 4)   
    (setq temp 270)   
    (setq temp 0)  
  )  
  (setq ans (getorient (trans pt_spt 0 1) (strcat   
    "\nRotation angle <" (if pt_ra (angtos pt_ra) (itoa temp)) ">: ")))  
  (if ans  
    (setq pt_ra ans)                  ; in radians  
    (setq pt_ra (/ temp (/ 180 pi)))  
  )  
)  
;;;  
;;; Get the spacing between the "baseline" of lines of text.  
;;; Defaults to 1.5 times the text height.  
;;; "Temp" is the group code to use.  
;;;  
;;; pt_gis == PText_Get_Interline_Spacing  
;;;  
(defun pt_gis (/ temp)  
  (if (> (sslength sset) 1)  
    (progn  
      (if (or (= grp_72 0) (= grp_72 5))  
        (setq temp 10)  
        (setq temp 11)  
      )  
      (distance (cdr(assoc temp (entget ename1)))  
                (cdr(assoc temp (entget ename2)))  
      )  
    )  
    (progn  
      (setq pt_ils (pt_sis))  
    )  
  )  
)  
;;;  
;;; Set the spacing between the "baseline" of lines of text.  
;;; Defaults to 1.5 times the text height.  
;;;  
;;; pt_sis == PText_Set_Interline_Spacing  
;;;  
(defun pt_sis ()  
  (setq pt_ils (* pt_th 1.5))  
  (initget 6)  
  (setq ans (getdist (trans pt_spt 0 1) (strcat   
    "\nInter-line spacing <" (if pt_ils (rtos pt_ils) "0.3") ">: ")))  
  (if ans  
    (if (= (logand (cdr(assoc 70 pt_sty)) 4) 4)   
      (- ans)   
      ans  
    )  
    (if (= (logand (cdr(assoc 70 pt_sty)) 4) 4)   
      (- (* 1.5 (if pt_th pt_th 0.2)))  
      (* 1.5 (if pt_th pt_th 0.2))  
    )  
  )  
)  
;;;  
;;; Set the maximum line length.  
;;; Defaults to 2 units.  
;;; Sets the global pt_mll.  
;;;  
;;; pt_sml == PText_Set_Maximum_line_Length  
;;;  
(defun pt_sml ()  
  (if (null pt_mll)   
    (setq pt_mll (* pt_th 10.0))  
  )  
  (initget 6)  
  (setq ans (getdist (trans pt_spt 0 1) (strcat   
    "\nMaximum line length" (if pt_mll   
                              (strcat " <"  (rtos pt_mll) ">: ")  
                              ": ")))  
  )  
  (if ans  
    (setq pt_mll ans)  
  )  
  (+ pt_mll (* 0.9 P_SLCK pt_twf pt_th))  
)  
;;;  
;;; All functions defined following this line up to the final c: function  
;;; definitions at the end of the file are duplicated in ptext.c and are  
;;; included here to allow you to execute the PTEXT command without using  
;;; an ADS routine.  You may want to try this to see the difference in speed  
;;; between the command running as a pure AutoLisp application versus one  
;;; that has been ported to ADS.  
;;;   
;;; In order to run the AutoLisp version, rename the PTEXT executable, and  
;;; then run PTEXT.  If this routine cannot find an executable with the   
;;; name of PTEXT (the extension varies), then it runs only the AutoLisp  
;;; version.  Otherwise, the ADS version is loaded and run.  
;;;   
;;; If you are never going to run the AutoLisp version, then the code   
;;; following this up to the final c: definitions may be deleted.  
;;;  
;;; ------------------ Cut here ----------------------------------  
  
;;;  
;;; Set up before getting keyboard input.  
;;;   
;;; The counter "pt_cl" is the number of the current line   
;;; starting at "1".  It is always one ahead of the number  
;;; required by the lisp expression (nth <n> <list>) which  
;;; starts its numbering at "0".  This counter is used througout  
;;; for accessing text entities from the list TX:LST.  
;;;  
;;; pt_sup == PText_Set_UP  
;;;  
(defun pt_sup ()  
  (setq pt_rsp 1   
        char   P_SPCE  
        pt_cl  (1+ pt_cl)   
        pt_vsp 1  
  )  
  (setq str    (if (= (type str) 'STR) str (chr char))  
        strlst (pt_psl str)           ; Parse string to list   
        pt_msp (pt_cll (length strlst) T) ; Check line length  
        pt_rsp (pt_cll pt_vsp nil)    ; Get character position/size  
        pt_str (pt_uls str pt_rsp)    ; Underline character 1  
  )  
  (if ent  
    (progn  
      (setq ent (subst (cons 1 pt_str) (assoc 1 ent) ent))  
      (entmod ent)  
    )  
    (progn  
      (dr_txt pt_str)                 ; Draw the text string - sets ent  
      (setq TX:LST (if TX:LST  
                     (pt_ael (cdr(assoc -1 ent)) pt_cl TX:LST)  
                     (list (cdr(assoc -1 ent)))  
                   )  
      )  
    )  
  )  
  (princ "\nText: ")  
  (princ str)  
)  
;;;  
;;; Turn on underlining for an apparent character location (j)  
;;; in a string (s).  Return the string with underlining.  
;;;  
;;; pt_uls == PText_UnderLine_String  
;;;    
(defun pt_uls (s j / temp)  
  (setq temp (strlen s))  
  (if (> temp 0)  
    (if (and (> j 0) (<= j temp))  
      (strcat (substr s 1 (pt_csl 1 j))  
              (strcat "%%u" (if insert "" "%%o")  
                      (nth (1- j) strlst)   
                      "%%u" (if insert "" "%%o")  
              )   
              (substr s (1+ (pt_csl 1 (1+ j))))  
      )  
      (strcat (substr s 1 (pt_csl 1 j)) "%%u %%u")  
    )  
    "%%u %%u"  
  )  
)  
;;;  
;;; Get a character from the keyboard  
;;;  
;;;  
(defun get_ch (/ return)  
  ;; Disallow all input except the keyboard.  
  (while (/= (car (setq char (grread nil))) 2) (princ))  
  (setq char (cadr char))  
  (cond  
    ((= char P_RTRN)  
      (pt_pnl)                        ; Process newline  
      (setq return nil)  
    )  
    ((= char P_ISRT)  
      (if insert                      ; Toggle insert mode...  
        (progn  
          (setq insert nil)  
          (grtext -2 "Ptext: Overwrite mode")  
        )  
        (progn  
          (setq insert T)  
          (grtext -2 "Ptext: Insert mode")  
        )  
      )   
      (setq return T)  
    )  
    ;; Backspace key -- destructive cursor  
    ((= char P_BACK)  
      (pt_mc "LEFT" T)                ; Move cursor  
      (setq return T)  
    )  
    ((or (= char P_DDEL)   
         (= char P_UDEL))             ; Delete key  
      (pt_mc "DEL" nil)  
      (setq return T)  
    )  
    ;; ALT - A key  -- Append a space to the current cursor position.  
    ((= char P_APPN)  
      (setq char P_SPCE)  
      (setq pt_rsp (pt_ats char T))   ; Add character to string  
      (pt_mc "RIGHT" nil)             ; Move cursor  
      (setq return T)  
    )  
    ((= char P_BEGL)                  ; ALT - B key : Beginning of line  
      (pt_mc "HOME" nil)  
      (setq return T)  
    )  
    ((= char P_DWNL)                  ; ALT - D key : Move down a line  
      (pt_mc "DOWN" nil)  
      (setq return T)  
    )  
    ((= char P_ENDL)                  ; ALT - E key : End of the line  
      (pt_mc "END" nil)  
      (setq return T)  
    )  
    ((= char P_LEFT)                  ; ALT - L key : Move left   
      (pt_mc "LEFT" nil)  
      (setq return T)  
    )  
    ((= char P_ENDT)                  ; ALT - N key : Move to bottom of text  
      (pt_mc "BOTTOM" nil)  
      (setq return T)  
    )  
    ((= char P_RGHT)                  ; ALT - R key : Move right  
      (pt_mc "RIGHT" nil)  
      (setq return T)  
    )  
    ((= char P_BEGT)                  ; ALT - T key : Move to top of text  
      (pt_mc "TOP" nil)  
      (setq return T)  
    )  
    ((or (= char P_UPLD)   
         (= char P_UPLU))             ; ALT - U key (DOS or UNIX)  
      (pt_mc "UP" nil)  
      (setq return T)  
    )  
    ((= char P_QUIT)                  ; ALT - Z key -- exit.  
      (initget "Yes No")  
      (if (= (getkword "\nExit from text entry? <Y>: ") "No")  
        (progn  
          (setq return T)  
          (princ "\nText: ")  
          (princ str)  
        )  
        (progn  
          (entmod (subst (cons 1 str) (assoc 1 ent) ent))  
          (setq str    ""  
                return nil  
          )  
        )  
      )  
    )  
    ((= char P_DEL)                   ; Delete -- delete character at cursor  
      (pt_mc "DEL" nil)  
      (setq return T)  
    )  
    ((or (= char P_HLPD) (= char P_HLPU)) ; ^F1 or ^? -- Help screen.  
      (pt_phs nil)  
      (setq return T)  
    )  
    ((= (chr char) "%")  
      (cond   
        ((= pnding nil) (setq pnding 1))  
        ((= pnding 1)   (setq pnding 2))  
        ((= pnding 2)   (setq pnding nil))  
        (T  
          (exit)  
        )  
      )  
      (setq pt_rsp (pt_ats char nil)  
            pt_vsp (pt_cll pt_rsp T)  
      )  
      (setq return T)  
    )  
    ; Else return T  
    (T  
      (if (= char P_HYPH)   
        (setq OK2BRK T)  
        (setq OK2BRK nil)  
      )  
      (if (> pnding 1)  
        (cond  
          ((or (= (chr char) "o")     ; overline  
              (= (chr char) "u")      ; underline  
              (= (chr char) "d")      ; degrees  
              (= (chr char) "p")      ; plus/minus  
              (= (chr char) " ")      ; space  
           )  
            (setq pt_rsp (- (pt_ats char nil) 2)  
                  strlst (pt_psl str) ; Parse string to list   
                  pt_vsp (pt_cll pt_rsp T)  
                  pnding nil  
            )  
          )  
          ((is_num (chr char))  
            (terpri)  
            (if (< pnding 4)  
              (setq pt_rsp (pt_ats char nil)  
                    pnding (1+ pnding)  
                    pt_vsp (pt_cll pt_rsp T)  
              )  
              (setq pt_rsp (- (pt_ats char nil) (- pnding 2))  
                    strlst (pt_psl str) ; Parse string to list   
                    pnding nil  
                    pt_vsp (pt_cll pt_rsp T)  
              )  
            )  
          )  
          (T   
            (setq pt_rsp (pt_ats char nil)  
                  pt_vsp (pt_cll pt_rsp T)  
                  pnding nil  
            )  
          )  
        )  
        (setq pt_rsp (pt_ats char nil)  
              pt_vsp (pt_cll pt_rsp T)  
        )  
      )  
      (setq return T) ; Return value  
    )  
  )  
  (if (/= char P_RTRN)  
    (progn  
      ;; set the current string postion (pt_vsp) after allowing for   
      ;; various control character codes such as %%d or %%p.  
      (setq pt_rsp (pt_cll pt_vsp nil)  
            ;; set the string that gets printed on-screen via (entmod).  
            pt_str (pt_uls str pt_rsp)  
            ;; set the maximum string postion (pt_msp) after allowing for   
            ;; various control character codes such as %%d or %%p.  
            pt_msp (pt_cll (length strlst) T)  
            ;; set the line length up to the current adjusted string position.  
            line_l (pt_gll (pt_csl 1 pt_rsp))  
      )  
    )  
  )  
  return  
)  
;;;  
;;; Get the length of a text line by making a dummy text entity  
;;; on the frozen text layer.  This entity will contain the current  
;;; text string without the underline/overline cursor characters  
;;; up to the current cursor position.  
;;;  
;;; Return the distance between the right and left points of the   
;;; right justified text string.  
;;;  
;;; pt_gll == PText_Get_Line_Length  
;;;  
(defun pt_gll (pt_rsp)  
  (setq pt_te (subst (cons 1 (pt_sjk 1 pt_rsp)) (assoc 1 pt_te) pt_te))  
  (setq pt_te (subst (cons 40 pt_th) (assoc 40 pt_te) pt_te))  
  (setq pt_te (subst (cons 41 pt_twf) (assoc 41 pt_te) pt_te))  
  (setq pt_te (subst (cons 51 pt_toa) (assoc 51 pt_te) pt_te))  
  (setq pt_te (subst (cons 7  pt_stn) (assoc  7 pt_te) pt_te))  
  (entmod pt_te)  
  (setq pt_te (entget(cdr(assoc -1 pt_te))))  
  (distance (cdr(assoc 10 pt_te)) (cdr(assoc 11 pt_te)))  
)  
;;;  
;;; Check the string list "strlst" for control characters.  If "diff" is T,  
;;; then return the number of visible characters, else return the number of   
;;; the item in the list which matches the current visual string position.  
;;; N_chars is global to this routine, and specifies how many characters  
;;; to delete if deleting a special symbol.  
;;;  
;;; pt_cll == PText_Check_Line_Length  
;;;  
(defun pt_cll (max diff / temp j)  
  (setq temp   0  
        j      0  
        k      0  
        nchars 0  ; global, local to (ptext).  
  )  
  (while (and (< k max) (< j (length strlst)))  
    (cond   
      ((or (= (nth j strlst) "%%o")   ; overline  
           (= (nth j strlst) "%%u"))  ; underline  
        (if diff   
          (setq k (1+ k))  
        )  
        (setq j (1+ j))  
      )  
      (T  
        (setq temp   (1+ temp)        ; diff count  
              nchars (strlen (nth j strlst))  
              j      (1+ j)  
              k      (1+ k)  
        )  
      )  
    )  
  )  
  (if diff (if (> temp 0) temp 1) j)  
)  
;;;  
;;; Parse the string "str" into a list of strings, one string for each  
;;; visual character or control character set.    
;;; N_chars is global to this routine, and specifies how many characters  
;;; to delete if deleting a special symbol.  
;;;  
;;; pt_psl == PText_Parse_String_to_List  
;;;  
(defun pt_psl (str / max temp j k tmplst)  
  (setq max  (strlen str)  
        j    1  
        k    0  
        x    1  
        nchars 0  ; global, local to (ptext).  
  )  
  (while (<= j  (strlen str))  
    (if (= (setq temp (substr str j 1)) "%")  
      (progn  
        (if (= (substr str (setq j (1+ j)) 1) "%")  
          (progn  
            (setq j (1+ j))  
            (cond   
              ((= (substr str j 1) " ") ; space  
                (setq tmplst (if tmplst   
                               (append tmplst (list "%" "%" " "))  
                               (list "%" "%" " ")  
                             )  
                )  
              )  
              ((or (= (substr str j 1) "%")  ; percent  
                   (= (substr str j 1) "d")  ; degrees  
                   (= (substr str j 1) "p")  ; plus/minus  
                   (= (substr str j 1) "o")  ; overline  
                   (= (substr str j 1) "u")) ; underline  
                (setq temp   (substr str (- j 2) 3)  
                      j      (1+ j)  
                )  
              )  
              ((is_num (substr str j 1))  
                (while (and (< k 3) (is_num (substr str (+ j k) 1)))  
                  (setq k      (1+ k))  
                )  
                (setq temp (substr str (- j 2) (+ 2 k))  
                      j    (+ j k)  
                )  
              )  
              (T  
                (setq j (1+ j))  
              )  
            )  
            (setq tmplst (if tmplst   
                           (append tmplst (list temp))  
                           (list temp)  
                         )  
            )  
          )  
          (progn  
            (setq tmplst (if tmplst   
                           (append tmplst (list temp))  
                           (list temp)  
                         )  
                  tmplst (append tmplst (list (substr str j 1)))  
                  j      (1+ j)  
            )  
          )  
        )  
      )  
      (progn  
        (setq tmplst (if tmplst   
                       (append tmplst (list temp))  
                       (list temp)  
                     )  
              j      (1+ j)  
        )  
      )  
    )  
  )  
  tmplst  
)  
;;;  
;;; Is the character (string) a number...  
;;;  
;;;  
(defun is_num (char)  
  (if   
    (or   
      (= char "0") (= char "1") (= char "2") (= char "3") (= char "4")  
      (= char "5") (= char "6") (= char "7") (= char "8") (= char "9")  
    )  
    T  
    nil  
  )  
)  
;;;  
;;; Count the number of characters in the list of strings up to the   
;;; current string position from the starting point.  
;;;  
;;; pt_csl == PText_Count_String_Length  
;;;  
(defun pt_csl (j k / temp)  
  (setq temp 0)  
  (while (and (> j 0) (< j k) (<= j (length strlst)))  
    (setq temp (+ temp (strlen (nth (1- j) strlst)))  
          j    (1+ j)  
    )  
  )  
  temp  
)  
;;;  
;;; Get the real string position of the next visual character to the   
;;; left of the current cursor posistion.  
;;;  
;;; pt_gpl == PText_Get_next_start_Position_Left  
;;;  
(defun pt_gpl (temp)  
  (setq j (pt_cll temp T))  
  (while (and (> temp 0) (= j (setq k (pt_cll temp T))))  
    (setq temp (1- temp))  
  )  
  k  
)  
;;;  
;;; Get the real string position of the next visual character to the   
;;; right of the current cursor posistion.  
;;;  
;;; pt_gpr == PText_Get_next_start_Position_Right  
;;;  
(defun pt_gpr (temp)  
  (pt_cll temp nil)  
)  
;;;  
;;; Strcat from the list "strlst" from "j" position to "k" position.  
;;; Return the string or "".  
;;;  
;;; pt_sjk == PText_Strcat_from_J_to_K  
;;;  
(defun pt_sjk (j k / temp)  
  (setq temp 0  
        l    0  
  )  
  (if (and (<= j (length strlst)) (<= j k))  
    (progn  
      (while (< temp (+ j l))  
        (if (or (= (nth temp strlst) "%%o")  ; overline  
                (= (nth temp strlst) "%%u")) ; underline  
            (setq l (1+ l))  
        )  
        (setq temp (1+ temp))  
      )        
      (setq temp "")  
      (while (and (<= (+ j l) (length strlst)) (<= j k))  
        (setq temp (strcat temp (nth (1- (+ j l)) strlst)))  
        (if (or (= (nth (1- (+ j l)) strlst) "%%o")  ; overline  
                (= (nth (1- (+ j l)) strlst) "%%u")) ; underline  
            (setq k (1+ k))  
        )  
        (setq j (1+ j))        
      )  
    )  
    (setq temp "")  
  )  
  temp  
)  
      
;;;  
;;; Move the cursor the direction "dir" and if the second argument is T,  
;;; then erase the character under the new cursor location.  
;;;  
;;; pt_mc == PText_Move_Cursor  
;;;  
(defun pt_mc (dir dstrct)  
  (cond  
    ((= dir "LEFT")  
      (if dstrct                      ; deleting text  
        (if (> pt_vsp 1)              ; if not at the beginning of a line  
          ;; subtract one visual character from the current position.  
          (progn  
            (setq pt_vsp (1- pt_vsp)  
                  pt_rsp (pt_cll pt_vsp nil)  
                  str    (strcat  
                           (pt_sjk 1 (- pt_vsp 1))  
                           (pt_sjk (1+ pt_vsp) pt_msp)  
                         )  
                  strlst (pt_psl str) ; Parse string to list   
            )  
            (repeat 5   
              (princ (chr P_BACK))  
              (princ (chr P_SPCE))  
              (princ (chr P_BACK))  
            )  
          )  
          (progn                      ; AT the beginning of the text line...  
            (if (> pt_cl 1)           ; if not at the first line...  
              (pt_dal T)              ; back up a line, destructive  
            )  
          )    
        )  
        (if (> pt_vsp 1)              ; NOT deleting text...  
          (setq pt_vsp (1- pt_vsp)  
                pt_rsp (pt_gpl pt_vsp)  
          )  
          (if (> pt_cl 1)             ; if not at the first line...  
            (pt_dal nil)              ; back up a line, non-destructive  
          )  
        )  
      )  
    )  
    ((= dir "RIGHT")  
      (if dstrct                      ; overwriting text  
        (if (< pt_vsp pt_msp)         ; if not at the end of a line  
          (setq pt_vsp (1+ pt_vsp)  
                pt_rsp (pt_gpr pt_vsp)  
                str    (strcat  
                         (pt_sjk 1 (- pt_rsp 2))  
                         " "  
                         (pt_sjk pt_rsp pt_msp)  
                       )  
                strlst (pt_psl str)   ; Parse string to list   
          )  
        )  
      )  
      (if (< pt_vsp pt_msp)           ; NOT deleting text and ...  
                                      ; NOT at the end of a line...  
        (setq pt_vsp (1+ pt_vsp)  
              pt_rsp (pt_gpr pt_vsp)  
        )  
        ;; else  
        (if (< pt_cl (length TX:LST)) ; AT the end of a line...  
          ;; If not at the last line in the edit list...  
          (progn  
            ;; Modify the current entity to remove the cursor.  
            (entmod (subst (cons 1 str) (assoc 1 ent) ent))  
            (setq pt_cl  (1+ pt_cl)   ; add one to current line counter  
                  ;; get the ename from TX:LST for the new current line.  
                  ent    (entget(nth (1- pt_cl) TX:LST))  
                  ;; get the string in ent  
                  str    (cdr(assoc 1 ent))
                  strlst (pt_psl str) ; Parse string to list   
                  ;; Actual under-line postion checked in (get_ch)  
                  pt_vsp 1    
            )  
          )  
          ;; Else do nothing.  
        )  
      )  
    )  
    ((= dir "HOME")  
      ;; Actual under-line postion checked in (get_ch)  
      (setq pt_vsp 1)         
    )  
    ((= dir "END")  
      ;; Actual under-line postion checked in (get_ch)  
      (setq pt_vsp pt_msp)    
    )  
    ((= dir "TOP")  
      (entmod (subst (cons 1 str) (assoc 1 ent) ent))  
      (setq pt_cl  1  
            ent    (entget(nth 0 TX:LST))  
            str    (cdr(assoc 1 ent))  
            strlst (pt_psl str)       ; Parse string to list   
            ;; Actual under-line postion checked in (get_ch)  
            pt_vsp 1          
      )  
    )  
    ((= dir "BOTTOM")  
      (entmod (subst (cons 1 str) (assoc 1 ent) ent))  
      (setq pt_cl  (length TX:LST)  
            ent    (entget(last TX:LST))  
            str    (cdr(assoc 1 ent))  
            strlst (pt_psl str)       ; Parse string to list   
            ;; Actual under-line postion checked in (get_ch)  
            pt_vsp pt_msp     
      )  
      (terpri)
    )  
    ((= dir "DEL")  
      (if (< pt_vsp pt_msp)           ; if not at the end of the line...  
        (progn  
          (setq temp (strlen str)  
                str    (strcat  
                         (pt_sjk 1 (1- pt_rsp))  
                         (pt_sjk (1+ pt_rsp) pt_msp)  
                       )  
                strlst (pt_psl str)                ; Parse string to list   
          )  
          (repeat (1+ (- temp (strlen str)))   
            (princ (chr P_BACK))  
            (princ (chr P_SPCE))  
            (princ (chr P_BACK))  
          )  
        )  
        ;; else, at the last character in the line...  
        (if (= (substr str pt_rsp 1) " ") ; if it is a blank...  
          (pt_bul (1+ pt_cl))         ; Bring up lines following this line.  
          ;; else, replace the current character with a space.  
          (setq str    (strcat (pt_sjk 1 (1- pt_rsp)) " ")  
                strlst (pt_psl str)   ; Parse string to list   
                pt_spt (polar pt_spt (+ pt_ra (/ pi 2)) pt_ils)  
          )  
        )  
      )  
    )  
    ((= dir "UP")  
      (if (and TX:LST (> pt_cl 1))    ; Never let pt_cl below 1.  
        (progn  
          (entmod (subst (cons 1 str) (assoc 1 ent) ent))  
          (setq pt_cl  (1- pt_cl)  
                ent    (entget(nth (1- pt_cl) TX:LST))  
                str    (cdr(assoc 1 ent))  
                strlst (pt_psl str)   ; Parse string to list   
                tvsp   (length strlst)
          )  
          (if (< tvsp pt_vsp) (setq pt_vsp tvsp))
          (terpri)  
        )  
      )  
    )  
    ((= dir "DOWN")  
      (if (and TX:LST (< pt_cl (length TX:LST)))  
        (progn  
          (entmod (subst (cons 1 str) (assoc 1 ent) ent))  
          (setq pt_cl  (1+ pt_cl)  
                ent    (entget(nth (1- pt_cl) TX:LST))  
                str    (cdr(assoc 1 ent))  
                strlst (pt_psl str)   ; Parse string to list   
                tvsp   (length strlst)
          )  
          (if (< tvsp pt_vsp) (setq pt_vsp tvsp))
          (terpri)  
        )  
      )  
    )  
  )  
  (princ "\rText: ")  
  (princ str)  
  pt_rsp  
)  
;;;  
;;; Bring up lines of text when deleting at the end of a line of text.  
;;;  
;;; pt_bul == PText_Bring_Up_Lines  
;;;  
(defun pt_bul (line)  
  (if (< pt_cl (length TX:LST))  
    (progn  
      (setq str  (strcat  
                   (substr str 1 (- pt_rsp 1))  
                   (cdr(assoc 1 (entget (nth pt_cl TX:LST))))  
                 )  
            strlst (pt_psl str)       ; Parse string to list   
            sset (ssadd)  
            j    pt_cl  
      )  
      (entdel (nth pt_cl TX:LST))  
      (setq TX:LST (pt_sil line TX:LST))  
      (while (< j (length TX:LST))  
        (ssadd (nth j TX:LST) sset)  
        (setq j (1+ j))  
      )  
      (if (> (sslength sset) 0)  
        (command "_.MOVE"   
                 sset   
                 ""   
                 pt_spt   
                 (polar pt_spt (+ pt_ra (/ pi 2)) pt_ils)
        )  
      )  
    )  
  )  
  ;; Else, do nothing.  
)  
;;;  
;;; Strip the item from the list of enames in TX:LST.  
;;;  
;;; pt_sil == PText_Strip_Item_from_List  
;;;  
(defun pt_sil (temp lst / j k tmplst)  
  (setq j   0  
        k   (length lst)  
  )  
  (while (< j k)  
    (if (= j (1- temp))  
      (setq j (1+ j))                 ; Skip the entry...  
      ;; else  
      (setq tmplst (if tmplst  
                     (append tmplst (list (nth j lst)))  
                     (list (nth j lst))  
                   )  
            j      (1+ j)  
      )  
    )  
  )  
  tmplst  
)  
;;;  
;;; Add the entity name to the list of enames in TX:LST.  
;;; TX:LST must have at least one member.  
;;;  
;;; pt_ael == PText_Add_Ename_to_List  
;;;  
(defun pt_ael (ename temp lst / j k tmplst)  
  (setq j   0  
        k   (length lst)  
  )  
  (while (< j k)  
    (setq tmplst (if tmplst  
                   (append tmplst (list (nth j lst)))  
                   (list (nth j lst))  
                 )  
          j      (1+ j)  
    )  
    (if (= j (1- temp))  
      (setq tmplst (append tmplst (list ename)))  
    )  
  )  
  tmplst  
)  
;;;  
;;; Add the item (ename) to the list (lst).  
;;;  
;;; pt_ail == PText_Add_Item_to_List  
;;;  
(defun pt_ail (ename temp lst / j k tmplst)  
  (setq j   0  
        k   (length lst)
  )  
  (while (< j k)  
    (if (= j temp)  
      (setq tmplst (if tmplst  
                     (append tmplst (list ename))  
                     (list ename)  
                   )  
            temp   nil  
      )  
    )  
    (setq tmplst (if tmplst  
                   (append tmplst (list (nth j lst)))
                   (list (nth j lst))  
                 )  
          j      (1+ j)  
    )  
  )  
  (if temp (setq tmplst (if tmplst  
                          (append tmplst (list ename))  
                          (list ename) 
                        ) 
           )  
  )  
  tmplst
)  
;;;  
;;; Add a character to a string  
;;;  
;;; pt_ats == PText_Add_char_To_String  
;;;  
(defun pt_ats (char appnd)  
  ;; Add item (chr char) to list "strlst".  
  (setq strlst (pt_ail (chr char) (if appnd pt_rsp (1- pt_rsp)) strlst))  
  (if insert  
    (progn
      (if (not appnd) (setq pt_vsp (1+ pt_vsp)))
      (setq pt_msp (1+ pt_msp))
    )  
  )  
  ;; If overwriting...  
  (if (null insert)  
    ;; Subtract item "pt_rsp" from list "strlst".  
    (if (< pt_rsp pt_msp) (setq strlst (pt_sil pt_rsp strlst)))  
  )  
  (setq str    (pt_sjk 1 (length strlst)))  
  (princ "\rText: ")  
  (princ str)  
  (1+ pt_rsp)   
)  
;;;  
;;; Process a newline character  
;;;  
;;; pt_pnl == PText_Process_NewLine  
;;;  
(defun pt_pnl (/ sset j)  
  (if ent ; There should (!) always be an entity at this point...  
    (progn  
      ;; Get the correct "start point" for the current type of text entity...  
      ;; This should correctly handle mixed text justification types.   
      (setq pt_spt (cdr(assoc (if (or (= grp_72 2) ; Right justified  
                                      (= grp_72 1) ; Left justified  
                                  ) 11 10) ent))  
            pt_spt (polar pt_spt (- pt_ra (/ pi 2)) pt_ils)  
      )  
      (if (= grp_72 5)                ; if Fit text  
        (setq pt_rpt (cdr(assoc 11 ent))  
              pt_rpt (polar pt_rpt (- pt_ra (/ pi 2)) pt_ils)  
         )  
      )  
      (setq ent (subst (cons 1 (substr str 1 (1- pt_rsp))) (assoc 1 ent) ent))  
      (if (= (cdr(assoc 1 ent)) "")  
        (setq ent (subst (cons 1 " ") (assoc 1 ent) ent))  
      )  
      (entmod ent)  
      ;; Null line at end of paragraph...  
      (if (and (= pt_cl (length tx:lst)) (= str " "))   
        (setq str "")                 ; Exit from routine.  
        ;; else  
        (progn  
          (setq str (substr str pt_rsp)   ; The balance of the string.
                sl     (strlen str)  
                pt_tsp (pt_cll sl nil)  
                pt_str (pt_uls str pt_tsp)  
                sset   (ssadd)  
                j      pt_cl  
          )  
          (while (< j (length TX:LST))  
            (ssadd (nth j TX:LST) sset)  
            (setq j (1+ j))  
          )  
          (if (> (sslength sset) 0)  
            (command "_.MOVE"   
                     sset   
                     ""   
                     pt_spt   
                     (polar pt_spt (- pt_ra (/ pi 2)) pt_ils)  
            )  
          )  
        )  
      )  
      (setq ent nil)  
      (setq sl 0)  
    )  
    (progn  
      (princ "\nError processing newline character. ")  
      (exit)  
    )  
  )  
)  
;;;  
;;; Process line  
;;;  
;;; pt_pl == PText_Process_Line  
;;;  
(defun pt_pl ()  
  (if ent                             ; Modify the text string  
    (entmod (setq ent (subst (cons 1 pt_str) (assoc 1 ent) ent)))  
    ;; else  
    (progn  
      (dr_txt pt_str)                 ; Draw the text string  
      (setq TX:LST (if TX:LST  
                     (pt_ael (cdr(assoc -1 ent)) pt_cl TX:LST)  
                     (list (cdr(assoc -1 ent)))  
                   )  
      )  
    )  
  )  
)  
;;;  
;;; Get the maximum string position allowed on a line given the   
;;; current contents of the variable "str".  Uses (pt_gll).  
;;;  
;;; pt_gmp == PText_Get_Maximum_string_Position  
;;;  
(defun pt_gmp (/ temp)  
  ;; Get a temporary projected number of characters allowed on a line.  
  ;; Check this against the actual line length of the real characters,  
  ;; adding one character until either the end of the string is reached   
  ;; or the maximum line length is reached.  Then start subtracting   
  ;; characters until either a space or hypen is found, or the start of  
  ;; the string is reached.  If the start of the string is reached, then  
  ;; search forward on the string looking for the end of the word.  If  
  ;; the end of the word is not found then return 1, else return the   
  ;; wrap position on the line.  
       
  (setq temp (round (/ pt_mll (* 0.8 pt_twf pt_th))))  
  (while (and (< temp pt_rsp) (< (pt_gll temp) pt_mll))  
    (setq temp (1+ temp))  
  )  
  (while (> (pt_gll temp) pt_mll)  
    (setq temp (1- temp))  
  )  
  (while (and (> temp 1)   
              (/= (nth (1- temp) strlst) " ") ; Back up until a space  
              (/= (nth (1- temp) strlst) "-") ; or hyphen is found...  
         )  
    (setq temp (1- temp))  
  )  
  (if (= temp 1)  
    (progn  
      (setq temp (round (/ pt_mll (* 0.8 pt_twf pt_th))))  
      (while (and (< temp pt_rsp)   
                  (/= (nth (1- temp) strlst) " ") ; Back up until a space  
                  (/= (nth (1- temp) strlst) "-") ; or hyphen is found...  
             )  
        (setq temp (1+ temp))  
      )  
    )  
  )  
  (if (= temp pt_rsp)  
    1  
    temp  
  )  
)  
;;;  
;;; Wrap the line at the end of the previous word, if there is one.  
;;; Otherwise, if the line is short and/or the word is long enough  
;;; to occupy the entire line length, then extend the word.  
;;;  
;;; pt_waw == PText_Wrap_At_Word  
;;;  
(defun pt_waw (/ temp)  
  (setq temp (pt_gmp))                ; Get the maximum string position.  
  
  (if (= grp_72 5)                    ; Fit text...  
    ;; Set up to test if we are within the last 1/4 of the word,  
    ;; and if so, we will cram the whole word on the line.  
    (progn  
      (setq line_l (* 0.9   
                      pt_twf   
                      pt_th   
                      (- pt_rsp (* (- pt_rsp temp) 0.25)))  
      )  
    )  
    ;; Else, set a dummy value large enough to trip the next test.  
    (setq line_l (* 2 line_l))   
  )  
  (pt_cll temp T) ; Return the visual string position.  
)  
;;;  
;;; Process word wrap.  
;;;  
;;; pt_pww == PText_Process_Word_Wrap  
;;;  
(defun pt_pww (loc / sset j)  
  ;; Not at the start of a line and line is longer than  maximum specified...  
  (if (and (> loc 1) (> line_l pt_mll))  
    ;; Wrapping a text line...  
    (progn  
      (pt_pee) ; Process existing entity  
        
      (pt_mne) ; Make the new text line here...  
    )  
    ;; Extending a text line...  
    (progn  
         
      ;; set the string that gets printed on-screen via (entmod).  
      (setq pt_str (pt_uls str pt_rsp))  
      (setq ent (subst (cons 1 pt_str) (assoc 1 ent) ent))  
      (entmod ent)  
    )  
  )  
)  
;;;  
;;; Process the existing string, entmoding it to its final form.  
;;;  
;;; pt_pee == PText_Process_Existing_Entity  
;;;  
(defun pt_pee ()  
  (repeat (- (pt_cll pt_msp nil) loc) (princ (chr P_BACK))  
                                      (princ (chr P_SPCE))   
                                      (princ (chr P_BACK))  
  )  
  (princ "\rText: ")  
  ;; Strip trailing space.  
  (while (= (nth (1- loc) strlst) " ") (setq loc (1- loc)))  
  (setq ent    (subst   
                 (cons 1 (princ (pt_sjk 1 loc))) ; print the string  
                 (assoc 1 ent)   
                 ent  
               )  
        pt_cl  (1+ pt_cl)  
        pt_spt (cdr(assoc (if (or (= grp_72 2)   
                                  (= grp_72 1)) 11 10) ent))  
        str    (pt_sjk (+ loc 2)  (length strlst))  
        strlst (pt_psl str)           ; Parse string to list   
        pt_msp (pt_cll (length strlst) T) ; Check line length  
        pt_vsp (pt_cll (- pt_rsp loc 1) T)  
        pt_rsp (pt_cll pt_vsp nil)    ; Get character position/size  
  )  
  (if (< pt_vsp 1) (setq pt_vsp 1))  
  (entmod ent)  
)  
;;;  
;;; Make a new entity after a word wrap.  
;;;  
;;; pt_mne == PText_Make_New_Entity  
;;;  
(defun pt_mne ()  
  (princ "\nText: ")  
  (princ str)  
  ;; set the string that gets printed on-screen via (entmod).  
  (setq pt_str (pt_uls str pt_rsp))  
  
  (if (<= pt_cl (length TX:LST))  
    (progn  
      (setq pt_spt (cdr(assoc (if (or (= grp_72 2)   
                                      (= grp_72 1)) 11 10) ent))  
      )  
      (if (= grp_72 5)                ; if Fit text  
        (setq pt_rpt (cdr(assoc 11 ent))  
              pt_rpt (polar pt_rpt (- pt_ra (/ pi 2)) pt_ils)  
        )  
      )  
    )  
    ;; else  
    (princ)  
  )  
  
  (pt_fww)  
  
  (setq pt_spt (polar pt_spt (- pt_ra (/ pi 2)) pt_ils))
  (dr_txt pt_str)                     ; Draw the text string  
  (setq TX:LST (if TX:LST  
                 (pt_ael (cdr(assoc -1 ent)) pt_cl TX:LST)  
                 (list (cdr(assoc -1 ent)))  
               )  
  )  
)  
;;;  
;;; Finish up a word wrap; get ready for the next character.  
;;; Move any entities that may follow down by one space.  
;;;  
;;; pt_fww == PText_Finish_Word_wrap  
;;;  
(defun pt_fww ()  
  (setq sset   (ssadd)  
        j      (1- pt_cl)  
  )  
  (while (< j (length TX:LST))  
    (ssadd (nth j TX:LST) sset)  
    (setq j (1+ j))  
  )  
  (if (> (sslength sset) 0)  
    (command "_.MOVE"   
             sset   
             ""   
             pt_spt   
             (polar pt_spt (- pt_ra (/ pi 2)) pt_ils)  
    )  
  )  
)  
;;;  
;;; Delete a line and back up to the previous one  
;;;  
;;; pt_dal == PText_Delete_A_Line  
;;;  
(defun pt_dal (temp / )  
  ;; Remove last ename if at the maximum string position discounting  
  ;; control characters.  Both should be 1.  
  (if (= pt_vsp pt_msp)   
    (if temp   
      (progn  
        (repeat (1+ (strlen str))   
          (princ (chr P_BACK))  
          (princ (chr P_SPCE))  
          (princ (chr P_BACK))  
        )  
        (princ "*Deleted*")  
      )  
      (progn  
        (entmod (subst (cons 1 str) (assoc 1 ent) ent))  
        (setq str "")  
      )  
    )  
    (entmod (subst (cons 1 str) (assoc 1 ent) ent))  
  )  
  (terpri)
  (setq pt_cl (1- pt_cl)  
        ent   (if (and (> pt_cl 0) (> (length TX:LST) 0))  
                (entget (nth (1- pt_cl) TX:LST))  
                nil  
              )  
  )  
  (if ent  
    (progn  
      (if temp   
        (progn  
          (pt_bul (1+ pt_cl))        ; Bring up lines following this line.  
          (setq st     (pt_psl (cdr(assoc 1 ent)))
                str    (strcat (cdr(assoc 1 ent)) str)
          )
        )  
        (setq str    (cdr(assoc 1 ent)))  
      )  
      (setq strlst (pt_psl str)  
            pt_vsp (1+ (pt_cll (length st) nil))  
            pt_msp (pt_cll (length strlst) T)  
            pt_spt (polar pt_spt (+ pt_ra (/ pi 2)) pt_ils)  
      )  
      (princ str)  
      (if (= grp_72 5)  
        (setq pt_rpt (polar pt_rpt (+ pt_ra (/ pi 2)) pt_ils))  
      )  
    )  
    (progn  
      (setq TX:LST nil  
            ent    nil  
      )  
    )  
  )  
)  
;;;  
;;; Draw each text item  
;;;  
;;;  
(defun dr_txt (str / j)  
  (setq j (* pt_ra (/ 180 pi)))       ; rotation angle in decimal degrees.  
  (cond  
    ((= grp_72 0) (command "_.TEXT" pt_spt pt_th j str))  
    ((= grp_72 1) (command "_.TEXT" "_C" pt_spt pt_th j str))  
    ((= grp_72 2) (command "_.TEXT" "_R" pt_spt pt_th j str))  
    ((= grp_72 5) (command "_.TEXT" "_F" pt_spt pt_rpt pt_th str))  
  )  
  (setq ent (entget(entlast)))  
  (setq pt_spt (cdr(assoc (if (or (= grp_72 2) (= grp_72 1)) 11 10) ent)))  
  (setq pt_spt (polar pt_spt (- pt_ra (/ pi 2)) pt_ils))  
  (if (= grp_72 5)  
    (setq pt_rpt (polar pt_rpt (- pt_ra (/ pi 2)) pt_ils))  
  )  
  ent  
)  
  
;;;  
;;; Round a number off to the nearest integer correctly  
;;;  
;;;  
(defun round (num)  
  (if (>= (- num (fix num)) 0.5)  
    (fix (1+ num))  
    (fix num)  
  )  
)  
;;; ------------------ Cut here ----------------------------------  
;;;  
;;; C: function definition.  
;;;  
(defun c:pt () (ptext))  
(defun c:ptext () (ptext))  
(princ "\n\tC:PText loaded.  Start command with PT or PTEXT.")  
(princ)  
