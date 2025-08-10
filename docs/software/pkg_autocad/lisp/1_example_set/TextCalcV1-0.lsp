;;-------------------=={ Text Calculator }==------------------;;
;;                                                            ;;
;;  Allows the user to perform arithmetical operations on     ;;
;;  numerical data within text.                               ;;
;;                                                            ;;
;;  User is prompted to select text containing numerical data ;;
;;  then either choose an arithmetical operation or place     ;;
;;  the result of the current calculation in the form of an   ;;
;;  MText object in the drawing.                              ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    07-04-2011                            ;;
;;  First Release.                                            ;;
;;------------------------------------------------------------;;

(defun c:TC nil (c:TextCalc))

(defun c:TextCalc ( / *error* _StartUndo _EndUndo _Select _Str a acdoc dcf dch f file num ops pt regex str tmp ) (vl-load-com)

  (defun *error* ( msg )
    (if acdoc (_EndUndo acdoc))
    (if (< 0 dch)  (unload_dialog dch))
    (if (setq tmp  (findfile tmp)) (vl-file-delete tmp))
    (if (and regex (not (vlax-object-released-p regex))) (vlax-release-object regex))
    (if (and msg   (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))) (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )
  
  (defun _Select ( msg / entity num dtext mtext )
    (while
      (progn (setvar 'ERRNO 0) (setq entity (car (nentsel msg)))
        (cond
          (
            (= 7 (getvar 'ERRNO))

            (princ "\n--> Missed, Try again.")
          )
          (
            (eq 'ENAME (type entity))

            (if (wcmatch (cdr (assoc 0 (entget entity))) "*TEXT,ATTRIB,*LEADER,*DIMENSION")
              (if
                (and
                  (progn (LM:GetTrueContent regex entity 'dtext 'mtext) dtext)
                  (setq num (LM:ParseNumbers dtext))
                )
                (not
                  (or
                    (and (= 1 (length num)) (setq num (car num)))
                    (and (setq num (car (LM:ListBox "Select Number to Use" (mapcar 'vl-princ-to-string num) nil))) (setq num (read num)))
                  )
                )
                (princ "\n--> Text does not contain numerical data.")
              )
              (princ "\n--> Invalid Object Selected.")
            )
          )
        )
      )
    )
    num
  )

  (defun _str ( n )
    (cond
      ( (eq 'INT  (type n)) (itoa n))
      ( (eq 'REAL (type n)) (rtos n))
      ( (eq 'STR  (type n)) n)
      ( (vl-princ-to-string n))
    )
  )

  (setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object))
        regex (vlax-create-object "VBScript.RegExp")
  )

  (cond             
    (
      (not
        (and (setq file (open (setq tmp (vl-filename-mktemp nil nil ".dcl")) "w"))
          (foreach line
           '(
              "_button  : image_button { width = 8.33; height = 3.85; fixed_width = true; fixed_height = true; alignment = centered; color = -15; }"
              ""
              "textcalc : dialog { key = \"dcltitle\"; spacer;"
              "  : row {"
              "    : _button { label = \"+\"; key = \"+\"; }"
              "    : _button { label = \"-\"; key = \"-\"; }"
              "    : _button { label = \"×\"; key = \"*\"; }"
              "    : _button { label = \"÷\"; key = \"/\"; }"
              "  }"
              "  spacer;"
              "  : button { key = \"accept\"; label = \"Place Result >>\"; is_default = true; height = 2; fixed_height = true; }"
              "  : button { key = \"cancel\"; label = \"Cancel\";           is_cancel = true; }"
              "}"
            )
            (write-line line file)
          )
          (not (setq file (close file))) (< 0 (setq dch (load_dialog tmp)))
        )
      )

      (princ "\n--> Error Loading Dialog.")
    )
    ( (setq num (_Select "\nSelect Text Containing Numerical Data: "))

      (setq str (_str num) ops '(("+" . " + ") ("-" . " - ") ("*" . " × ") ("/" . " ÷ ")))

      (while (not (member dcf '(1 0)))
        (cond
          (
            (not (new_dialog "textcalc" dch))

            (setq dcf 0)
            (princ "\n--> Error Loading Dialog.")
          )
          (t           
            (foreach x
             '(
                ("+"
                  (033 032 031 030 029 028 028 028 027 026 025 024 023 022 022 022 021 020 019 018 017 016 015 014 013 012 011 010 009 041 040 039 038
                   037 036 035 034)
                  (027 027 027 027 027 011 027 041 041 041 041 041 041 011 027 041 027 027 027 027 027 027 027 027 027 027 028 029 029 029 029 028 027
                   027 027 027 027)
                  (023 023 023 023 023 009 023 039 009 009 009 009 009 009 023 039 023 023 023 023 023 023 023 023 023 023 022 021 021 021 021 022 023
                   023 023 023 023)
                )
                ("-"
                  (037 036 035 034 033 032 031 030 029 028 027 026 025 024 023 022 021 020 019 018 017 016 015 014 013 012 011 010 009 041 040 039 038)
                  (027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 027 028 029 029 029 029 028 027)
                  (023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 023 022 021 021 021 021 022 023)
                )
                ("/"
                  (027 026 026 026 025 025 025 024 024 024 023 023 023 022 022 022 021 020 019 018 017 016 015 014 013 012 011 010 009 041 040 039 038
                   037 036 035 034 033 032 031 030 029 028 028 028 027 027)
                  (041 016 027 041 016 027 041 016 027 041 015 027 040 014 027 039 027 027 027 027 027 027 027 027 027 027 028 029 029 029 029 028 027
                   027 027 027 027 027 027 027 027 027 014 027 039 015 027)
                  (035 009 023 034 009 023 034 009 023 034 009 023 035 011 023 036 023 023 023 023 023 023 023 023 023 023 022 021 021 021 021 022 023
                   023 023 023 023 023 023 023 023 023 011 023 036 009 023)
                )
                ("*"
                  (027 026 025 024 023 022 022 021 021 020 020 019 019 018 018 017 017 016 016 015 015 014 014 013 013 012 012 011 011 040 040 039 039
                   038 038 037 037 036 036 035 035 034 034 033 033 032 032 031 031 030 030 029 029 028)           
                  (030 029 028 029 030 025 031 024 032 023 033 022 034 021 035 020 036 019 039 018 040 017 039 016 038 016 037 016 036 015 035 016 036
                   016 037 016 038 017 039 018 040 019 037 020 036 021 035 022 034 023 033 024 032 031)           
                  (020 021 022 021 020 019 025 018 026 017 027 016 028 015 029 014 030 010 031 010 032 011 033 012 034 013 034 014 034 015 035 014 034
                   013 034 012 034 011 033 010 032 013 031 014 030 015 029 016 028 017 027 018 026 019)
                )
              )
              (start_image (car x))
              (apply 'mapcar (cons '(lambda ( x y z ) (vector_image x y x z 178)) (cdr x)))
              (end_image)
              (action_tile (car x) (strcat "(princ (strcat \"\n\" str (cdr (assoc (setq f $key) ops)))) (done_dialog 2)"))
            )
            (set_tile "dcltitle" "Text Calculator")
           
            (setq dcf (start_dialog))

            (if (and (= 2 dcf) (setq a (_Select "\nSelect Text Containing Numerical Data: ")))
              (cond
                (
                  (eq f "/")
                 
                  (if (equal 0.0 (setq a (float a)) 1e-14)
                    (princ "\n--> Invalid Operation: Divide by Zero.")
                    (progn
                      (setq num ((eval (read f)) num a))
                      (princ (strcat "\n" str (cdr (assoc f ops)) (_str a) " = " (setq str (_str num))))
                    )
                  )
                )
                ( (setq num ((eval (read f)) num a))

                  (princ (strcat "\n" str (cdr (assoc f ops)) (_str a) " = " (setq str (_str num))))
                )
              )
            )
            (princ)
          )
        )
      )
     
      (if (and (= 1 dcf) (setq pt (getpoint "\nSpecify Point for Result: ")))
        (progn
          (_StartUndo acdoc)
          
          (vla-AddMtext
            (vlax-get-property acdoc
              (if (= 1 (getvar 'CVPORT)) 'Paperspace 'Modelspace)
            )
            (vlax-3D-point (trans pt 1 0)) 0. (_str num)
          )

          (_EndUndo acdoc)
        )
      )          
    )
  )

  (*error* nil)
  (princ)
)

;;-------------------=={ Parse Numbers }==--------------------;;
;;                                                            ;;
;;  Parses a list of numerical values from a supplied string. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  s  - String to process                                    ;;
;;------------------------------------------------------------;;
;;  Returns:  List of numerical values found in string.       ;;
;;------------------------------------------------------------;;

(defun LM:ParseNumbers ( s )
  (
    (lambda ( l )
      (read
        (strcat "("
          (vl-list->string
            (mapcar
              (function
                (lambda ( a b c )
                  (if
                    (or
                      (< 47 b 58)
                      (and (= 45 b) (< 47 c 58) (not (< 47 a 58)))
                      (and (= 46 b) (< 47 a 58))
                      (= 32 b)
                    )
                    b 32
                  )
                )
              )
              (cons nil l) l (append (cdr l) (list nil))
            )
          )
          ")"
        )
      )          
    )
    (vl-string->list s)
  )
)

;;-----------------------=={ List Box }==---------------------;;
;;                                                            ;;
;;  Displays a List Box allowing the user to make a selection ;;
;;  from the supplied data.                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  title    - List Box Dialog title                          ;;
;;  data     - List of Strings to display in the List Box     ;;
;;  multiple - Boolean flag to determine whether the user     ;;
;;             may select multiple items (T=Allow Multiple)   ;;
;;------------------------------------------------------------;;
;;  Returns:  List of selected items, else nil.               ;;
;;------------------------------------------------------------;;

(defun LM:ListBox ( title data multiple / file tmp dch return )
  ;; © Lee Mac 2011
  
  (cond
    (
      (not
        (and (setq file (open (setq tmp (vl-filename-mktemp nil nil ".dcl")) "w"))
          (write-line
            (strcat "listbox : dialog { label = \"" title
              "\"; spacer; : list_box { key = \"list\"; multiple_select = "
              (if multiple "true" "false") "; } spacer; ok_cancel;}"
            )
            file
          )
          (not (close file)) (< 0 (setq dch (load_dialog tmp))) (new_dialog "listbox" dch)
        )
      )
    )
    (
      t     
      (start_list "list")
      (mapcar 'add_list data) (end_list)

      (setq return (set_tile "list" "0"))
      (action_tile "list" "(setq return $value)")

      (setq return
        (if (= 1 (start_dialog))
          (mapcar '(lambda ( x ) (nth x data)) (read (strcat "(" return ")")))
        )
      )          
    )
  )
  
  (if (< 0 dch) (unload_dialog dch))
  (if (setq tmp (findfile tmp)) (vl-file-delete tmp))

  return
)

;;------------------=={ Get True Content }==------------------;;
;;                                                            ;;
;;  Returns the unformatted string associated with the        ;;
;;  supplied entity, in formats compatible with Text & MText  ;;
;;  objects.                                                  ;;
;;                                                            ;;
;;  The arguments *dtextstring & *mtextstring should be       ;;
;;  supplied with quoted symbols (other than those symbols    ;;
;;  used by the arguments themselves). The unformatted        ;;
;;  strings suitable for Text & MText objects will henceforth ;;
;;  be bound to the supplied symbol arguments respectively.   ;;
;;                                                            ;;
;;  Note that it is the caller's responsibility to create and ;;
;;  release the RegularExpressions (RegExp) object. This      ;;
;;  object may be created using the                           ;;
;;  Programmatic Identifier: "VBScript.RegExp".               ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  RegExp       - RegularExpressions (RegExp) Object         ;;
;;  entity       - Ename whose text content is to be returned ;;
;;  *dtextstring - (output) Unformatted string compatible     ;;
;;                 with Text entities                         ;;
;;  *mtextstring - (output) Unformatted string compatible     ;;
;;                 with MText entities                        ;;
;;------------------------------------------------------------;;
;;  Returns:    This function always returns nil              ;;
;;------------------------------------------------------------;;

(defun LM:GetTrueContent ( RegExp entity *dtextstring *mtextstring / _Replace _AllowsFormatting _GetTextString )

  (defun _Replace ( new old string )
    (vlax-put-property RegExp 'pattern old) (vlax-invoke RegExp 'replace string new)
  )

  (defun _AllowsFormatting ( entity / object )    
    (or (wcmatch (cdr (assoc 0 (entget entity))) "MTEXT,MULTILEADER,*DIMENSION")      
      (and
        (eq "ATTRIB" (cdr (assoc 0 (entget entity))))
        (vlax-property-available-p (setq object (vlax-ename->vla-object entity)) 'MTextAttribute)
        (eq :vlax-true (vla-get-MTextAttribute object))
      )
    )
  )

  (defun _GetTextString ( entity )
    (
      (lambda ( entity / _type elist db ds )
        (cond
          ( (eq (setq _type (cdr (assoc 0 (setq elist (entget entity))))) "TEXT")
           
            (cdr (assoc 1 (reverse elist)))
          )
          ( (wcmatch _type "*DIMENSION")

            (if (setq db (tblobjname "BLOCK" (cdr (assoc 2 elist))))
              (while (and (setq db (entnext db)) (not ds))
                (if (eq "MTEXT" (cdr (assoc 0 (setq elist (entget db)))))
                  (setq ds (cdr (assoc 1 elist)))
                )
              )
            )
            ds
          )          
          ( (eq "MULTILEADER" _type)

            (cdr (assoc 304 elist))
          )
          ( (wcmatch _type "ATTRIB,MTEXT")

            (
              (lambda ( string )
                (mapcar
                  (function
                    (lambda ( pair )
                      (if (member (car pair) '(1 3))
                        (setq string (strcat string (cdr pair)))
                      )
                    )
                  )
                  elist
                )
                string
              )
              ""
            )
          )
        )
      )
      entity
    )
  )

  (
    (lambda ( string )
      (if string
        (progn
          (mapcar
            (function
              (lambda ( x ) (vlax-put-property RegExp (car x) (cdr x)))
            )
            (list (cons 'global actrue) (cons 'ignorecase acfalse) (cons 'multiline actrue))
          )
          (if (_AllowsFormatting entity)
            (mapcar
              (function
                (lambda ( x ) (setq string (_Replace (car x) (cdr x) string)))
              )
             '(
                ("Ð"       . "\\\\\\\\")
                (" "       . "\\\\P|\\n|\\t")
                ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
                ("$1"      . "[\\\\]({)|{")
              )
            )
            (setq string (_Replace "" "%%[OoUu]" (_Replace "Ð" "\\\\" string)))
          )
          (set *mtextstring (_Replace "\\\\" "Ð" (_Replace "\\$1$2$3" "(\\\\[ACcFfHLlOoPpQSTW])|({)|(})" string)))
          (set *dtextstring (_Replace "\\"   "Ð" string))
        )
      )
    )
    (_GetTextString entity)
  )
  nil
)

;;------------------------------------------------------------;;

(princ)
(princ "\n:: TextCalc.lsp | Version 1.0 | © Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Type \"TextCalc\" or \"TC\" to Invoke ::")
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;