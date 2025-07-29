;;;----------------------------------------------------------------------------
;;;   FILTER.LSP  Version 0.5
;;;   Copyright (C) 1991-1992 by Autodesk, Inc.
;;;      
;;;   Permission to use, copy, modify, and distribute this software 
;;;   for any purpose and without fee is hereby granted, provided 
;;;   that the above copyright notice appears in all copies and that 
;;;   both that copyright notice and this permission notice appear in 
;;;   all supporting documentation.
;;;
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED
;;;   WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.
;;;   
;;;----------------------------------------------------------------------------
;;;  DESCRIPTION
;;;
;;;  Dialogue front end to (ssget).  Allows customers to create, save and
;;;  apply filter lists for entity selection via a dialogue interface.
;;;  Uses FILTER.DCL.
;;;
;;;----------------------------------------------------------------------------
;;;
;;; ===========================================================================
;;; ===================== load-time error checking ============================
;;;

  (defun ai_abort (app msg)
     (defun *error* (s)
        (if old_error (setq *error* old_error))
        (princ)
     )
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       )
     )
     (exit)
  )

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

  (cond
     (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.

     (  (not (findfile "ai_utils.lsp"))                     ; find it
        (ai_abort "FILTER"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "FILTER" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "FILTER" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================

;;;----------------------------------------------------------------------------
;;; The Main function.  Variables are initialised, and the dialogue box is
;;; prepared and activated.  A while loop is used to allow the dialogue box
;;; to be hidded for entity selection.
;;;----------------------------------------------------------------------------
(defun c:filter(/ 
             a                 edit_item         just_name      selection_list    
             add_to_list       edit_this         label          single_table      
             after_errno       enable_disable    lisp_error                   
             all_lisp_list     entity_ename      lisp_pos       str1              
             appid_str         entity_lisp       list1          str2              
             bit_flag          entity_lisp_init  list_name      string            
             c1                entity_lisp_list  list_str       str_name          
             c2                entity_type       load_err       str_pos           
             check_color       error_msg         load_log       str_val           
             check_int         filter_main       lts            
             clear_list        filter_err        make_list      table_item        
             cmd               filter_gc         n              table_list        
             cnum              filter_lisp_list  n1             table_match       
             color_no          filter_list       name           table_name        
             current_filter    filter_nfl        named_lists    temp              
             current_line      filter_str_list   new_length     temp_lisp_list    
             dcl_id            f_err             new_lisp       temp_list         
             delete_list       gc_name           new_str        temp_ss           
                                                 nfl_lisp       temp_str          
                                                 nfl_str        the_list          
                               globals           olderr         title             
                               good_value        op             update            
                                                 ops_3          val               
                                                 pat            value             
                                                 pat_match      what_is_it        
                                                 pick           what_next         
                                                 pick_list                
                                                 poly_val       which_box         
                                                 pos            which_list        
                                                 pr             ws                
                               hmmm              redefine       x_op              
                               huh               remove         x_value           
                               i                 remove_flag    y_op              
                               init_lists        ri_ops         y_value           
                                                rm_item        z_op              
                               item              rs_err         z_value           
                               item1             s             
                               item2             save_all      
             dp1               item_index        save_as       
             dp2               j                 select        ret_list
             )
  ;;
  ;; Action on Add Selected Entity button.
  ;;
  (defun do_select_entity ()
    (setq edit_item (atoi (get_tile "filter_str_list")))
    (done_dialog 2)
  )
  ;;
  ;; Action on Remove button.
  ;;
  (defun do_remove ()
    (setq remove_flag 1)
    (remove)
    (setq remove_flag 0)
  )
  ;;
  ;; Initialise the English list and corresponding group code list.  The list
  ;; of operators is also initialised.
  ;;
  (defun init_lists()
    (setq filter_list (list 
                        "Arc"          "Arc Center"         "Arc Radius"
                        "Attribute"    "Attribute Position" "Attribute Tag" 
                        "Block"        "Block Name"         "Block Position"
                                       "Block Rotation"
                        "Circle"       "Circle Center"      "Circle Radius" 
                        "Color"         
                        "Dimension"    "Dimension Style" 
                        "Elevation"     
                        "Layer"
                        "Line"         "Line Start"         "Line End"
                        "Linetype"
                        "Normal Vector"
                        "Point"        "Point Position"
                        "Polyline"      
                        "Shape"        "Shape Position"     "Shape Name"                                            
                        "Solid"        "Solid Point 1"      "Solid Point 2"
                                       "Solid Point 3"      "Solid Point 4"
                        "Text"         "Text Position"      "Text Value"
                                       "Text Style Name"    "Text Height" 
                                       "Text Rotation"
                        "Trace"        "Trace Point 1"      "Trace Point 2"
                                       "Trace Point 3"      "Trace Point 4"
                        "3dface"       "3dface Point 1"     "3dface Point 2"
                                       "3dface Point 3"     "3dface Point 4"
                        "Thickness"
                        "Viewport"     "Viewport Center" 
                        "Xdata ID"
                        "** Begin  AND"
                        "** End    AND"
                        "** Begin  OR"
                        "** End    OR"
                        "** Begin  XOR"
                        "** End    XOR"
                        "** Begin  NOT"
                        "** End    NOT"
                  )
    )

    (setq filter_gc (list 
                         0 10 40  
                         0 10  2  
                         0  2 10 50 
                         0 10 40
                         62
                         0  3 
                         38 
                         8
                         0 10 11
                         6
                         210
                         0 10
                         0 
                         0 10  2
                         0 10 11 12 13
                         0 10  1  7 40 50
                         0 10 11 12 13
                         0 10 11 12 13
                         39
                         0 10 
                         -3
                         "<AND" "AND>"
                         "<OR"  "OR>"
                         "<XOR" "XOR>"
                         "<NOT" "NOT>"
        )
    )
    (setq ri_ops (list "=" "!=" "<" "<=" ">" ">=" "*"))  
  )
  ;;
  ;; Function to reset the error tile.
  ;;
  (defun rs_err()
    (set_tile "error" "")
  )
  ;;
  ;; Function called by SELECT button.  Used to bring the Color dialogue and
  ;; the symbol table dialogues.
  ;;
  (defun select (/ current_filter selection_list color_no poly_val str 
                   table_name
                )
    (setq current_filter (nth (atoi (get_tile "filter_by")) filter_list))
    (cond 
      ((= "Color" current_filter)   ; if Color 
        (if (setq color_no (acad_colordlg 1 t))       ; and a color is selected
          (set_tile "x_value" (itoa color_no))     
        )
      )
      (t (cond 
           ((= "Block Name"      current_filter) (setq table_name "Block"))
           ((= "Dimension Style" current_filter) (setq table_name "Dimstyle")) 
           ((= "Layer"           current_filter) (setq table_name "Layer"))
           ((= "Linetype"        current_filter) (setq table_name "Ltype"))
           ((= "Text Style Name" current_filter) (setq table_name "Style"))
           ((= "Xdata ID"  current_filter) (setq table_name "Appid"))
           (t (princ "Programming error in select"))
         )
         (if (setq selection_list (reverse (single_table table_name 
                   (strcat "Select " current_filter "(s)" ))))
           (progn 
             (setq n   0
                   str "")
             (while (< n (length selection_list))
               (setq str (strcat (nth n selection_list) "," str))
               (setq n (1+ n))
             )
             (set_tile "x_value" (substr str 1 (1- (strlen str))))
           )
        )
      )
    )
  )
  ;;
  ;;  Deletes the current named list from the list of named lists.
  ;;
  (defun delete_list()
    (if (/= 0 (setq pick_list (atoi (get_tile "named_lists"))))
      (progn 
        (setq all_lisp_list (rm_item pick_list all_lisp_list))
        (save_all)
        (start_list "named_lists")
        (mapcar 'add_list all_lisp_list)
        (end_list)
        (set_tile "named_lists" "0")
        (setq filter_str_list ai_str|*unnamed)
        (setq filter_lisp_list ai_lisp|*unnamed)
        (start_list "filter_str_list")
        (mapcar 'add_list filter_str_list)
        (end_list)
      )
      (set_tile "error" "Cannot delete *unnamed Filter list.")
    )
  )
  ;;
  ;; Retrieves the named lists from file. (NFL = Named Filter Lists)
  ;;
  (defun load_log(/ filter_nfl nfl_lisp nfl_str current_line)
    (if (setq filter_nfl (open "filter.nfl" "r"))
      (progn 
        (setq current_line (read-line filter_nfl))
        (while (and (/= "" current_line)
                    (/= nil current_line)
                    (/= ":" (substr current_line 1 1))) ; skip comments
         (setq current_line (read-line filter_nfl))
        )
        (while current_line                       ; get lisp
          (setq name (substr current_line 10))     ; get list name
          (setq all_lisp_list (cons name all_lisp_list))
          (setq current_line (read-line filter_nfl))
          (while (/= ":" (substr current_line 1 1))
            (setq nfl_lisp (cons (read current_line) nfl_lisp))
            (setq current_line (read-line filter_nfl))
          )
          (set (read (strcat "ai_lisp|" name)) (reverse nfl_lisp))
          (setq nfl_lisp '())
          (setq current_line (read-line filter_nfl))         ; get str
          (set (read (strcat "ai_str|" name)) '())
          (while (and current_line (/= ":" (substr current_line 1 1)))
            (setq nfl_str (cons current_line nfl_str))
            (setq current_line (read-line filter_nfl))
          )
          (set (read (strcat "ai_str|" name)) (reverse (cons "" nfl_str)))
          (setq nfl_str '())
        )
        (if (and all_lisp_list 
                 (< (length all_lisp_list) (getvar "maxsort"))
            )
          (setq all_lisp_list (acad_strlsort all_lisp_list))
        )
        (start_list "named_lists")
        (mapcar 'add_list all_lisp_list)
        (end_list)
        (set_tile "named_lists" "0")
      )
    )
  )
  ;;
  ;;  Saves named lists to file.
  ;;
  (defun save_all(/ filter_nfl)
    (if (setq filter_nfl (open "filter.nfl" "w"))
      (progn
        (write-line "Filter.nfl  --  Do not edit this file." filter_nfl)
        (if (< 1 (length all_lisp_list))
          (progn
            (foreach n all_lisp_list
              (if (/= n "*unnamed")
                (progn
                  (write-line (strcat ":ai_lisp|" n) filter_nfl)
                  (foreach n1 
                    (reverse (lts (eval (read (strcat "ai_lisp|" n))) 1))
                    (write-line n1 filter_nfl)
                  )
                  (write-line (strcat ":ai_str|" n) filter_nfl)
                  (foreach n1 (eval (read (strcat "ai_str|" n)))
                    (if (/= "" n1) (write-line n1 filter_nfl))
                  )
                )
              )
            )
          )
        )
        (close filter_nfl)
      )
      (alert (strcat "Cannot save Filter list to file \n"
                     " - directory must have write permission."
             )
      )
    )
  )
  ;;
  ;; If not the *unnamed list, make current the selected one.
  ;;
  (defun named_lists()
;    (cond
;      ((/= "0" (get_tile "named_lists"))
        (setq list_name (nth (atoi (get_tile "named_lists")) all_lisp_list))
        (setq filter_lisp_list 
              (eval (read 
                      (strcat "ai_lisp|" list_name)
              ))
        )
        (setq filter_str_list 
              (eval (read 
                      (strcat "ai_str|" list_name)
              ))
        )
        (start_list "filter_str_list")
        (mapcar 'add_list filter_str_list)
        (end_list)
;      )
;    )
  )

  ;;
  ;; Check the entered name and if valid, save it.
  ;;
  (defun save_as()
    (setq list_name (ai_strtrim (get_tile "new_name")))
    (cond
      ((or (= nil list_name)(= "" list_name))
        (set_tile "error" "Null Filter name not allowed.")
      )
      ((wcmatch list_name "*[]`#`@`.`?`*`~`[`,`'!%^&()+={}|`\\:;\"<>/]*")
        (set_tile "error" "Invalid characters in Filter name.")
      )
      ((= "*unnamed" list_name) 
        (set_tile "error" "Invalid Filter name.")
      )
      ((and (member list_name all_lisp_list) (not (redefine))))
      ((lisp_error)) 
      (T 
        (set (read (eval (strcat "ai_lisp|" list_name))) filter_lisp_list)
        (set (read (eval (strcat "ai_str|" list_name))) filter_str_list)
        (if (not (member list_name all_lisp_list)) ; add if not member
          (progn
            (setq all_lisp_list (cons list_name all_lisp_list))
            (if (and all_lisp_list
                     (< (length all_lisp_list) (getvar "maxsort"))
                )
              (setq all_lisp_list (acad_strlsort all_lisp_list))
            )
          )
        )
        (start_list "named_lists")
        (mapcar 'add_list all_lisp_list)
        (end_list)
        (set_tile "named_lists" (itoa (what_pos list_name all_lisp_list)))
        (save_all)
      ) 
    )
  )
  ;;
  ;; If the entered name for the filter list matches an existing name, call
  ;; dialogue for confirmation to redefine it.  T is returned if OK to redefine.
  ;;
  (defun redefine ()
    (if (not (new_dialog "already_exists" dcl_id)) (exit))   
    (action_tile "redefine" "(done_dialog 2)")
    (action_tile "cancel" "(done_dialog 0)")
    (if (= (start_dialog) 2) t)           ; return t on Redefine, nil on cancel
  )
  ;;
  ;; Debugging routine.
  ;;
  (defun pr()
    (princ filter_str_list)
    (princ filter_lisp_list)
  )
  ;;
  ;;  Routine that updates the current English and Lisp lists to contain the
  ;;  new English and Lisp arguments.
  ;;
  (defun update (new_str new_lisp / str1 str2 i edit_this lisp_pos temp_str)
    ; find current position in filter_lisp_list (list of lists)
    (setq i -1)
    (setq edit_this -1)        ; corresponding item in lisp list.
    (setq lisp_pos -1)         ; so that length below occurs
    (if filter_lisp_list
      (progn
        (while (< edit_this str_pos)  ; until they are equal
          (setq i (1+ i))
          (if (not (and (= -4 (car (nth i filter_lisp_list)))   
                       (not (member 
                              (cdr (nth i filter_lisp_list)) 
                              '("<AND" "AND>" "<OR"  "OR>" 
                               "<XOR" "XOR>" "<NOT" "NOT>")
                            )
                       )
                   )
              )
            (progn 
              (setq edit_this (1+ edit_this))        
            )
          )
        )
        (if (and (< 0 i) 
                 (and (= -4 (car (nth (1- i) filter_lisp_list)))   
                      (not (member 
                             (cdr (nth (1- i) filter_lisp_list)) 
                             '("<AND" "AND>" "<OR"  "OR>" 
                               "<XOR" "XOR>" "<NOT" "NOT>")
                           )
                      )
                 )
            )
          (setq lisp_pos (1- i))
          (setq lisp_pos i)
        )
      )
    )
    ; join lisp lists
    (setq i 0)
    (setq str1 '()) (setq str2 '())
    (if (<= 0 lisp_pos)
      (progn 
        (while (< i lisp_pos)
          (setq str1 (cons (nth i filter_lisp_list) str1))   
          (setq i (1+ i))
        )
        (setq str1 (reverse str1))
        (setq temp_str (reverse filter_lisp_list))
        (setq i 0)
        (while (<= i (- (- (length filter_lisp_list) lisp_pos) 1))
          (setq str2 (cons (nth i temp_str) str2))   
          (setq i (1+ i))
        )
      )
    )
    (setq filter_lisp_list (append str1 new_lisp str2))
    (setq ai_lisp|*unnamed filter_lisp_list)
    ; join string lists
    (setq i 0)
    (setq str1 '()) (setq str2 '())
    (while (< i str_pos)
      (setq str1 (cons (nth i filter_str_list) str1))   
      (setq i (1+ i))
    )
    (setq str1 (reverse str1))
    (setq temp_str (reverse filter_str_list))
    (setq i 0)
    (while (<= i (- (- (length filter_str_list) str_pos) 1))
      (setq str2 (cons (nth i temp_str) str2))   
      (setq i (1+ i))
    )
    (setq filter_str_list (append str1 new_str str2))
    (setq ai_str|*unnamed filter_str_list)
    ; Update displayed string list
    (start_list "filter_str_list")
    (mapcar 'add_list filter_str_list)
    (end_list)

    (setq new_length (length new_str))   ; length of new string list.
    (cond
     ((/= (1- (length filter_str_list)) str_pos)
       (set_tile "filter_str_list" 
                 (itoa (setq str_pos (+ str_pos new_length)))
       )
     )
     ((and (= (1- (length filter_str_list)) str_pos)
           (/= 1 (length filter_str_list))
      )
       (set_tile "filter_str_list" (itoa (1- str_pos)))
     )
     (T)
    )
  )
  ;;
  ;; Disables the controls when an filter is chosen from the list of possible 
  ;; filters
  ;;
  (defun grey_filter ( )
    (setq pick (nth (atoi (get_tile "filter_by")) filter_list))
    (enable_disable pick)
  )
  ;;
  ;; Disables the controls according to current selection.
  ;;
  (defun enable_disable(string)
    (cond 
      ((member string '(
                      "Arc"   "Attribute" "Block" "Circle" "Dimension" "Line" 
                      "Point" "Polyline"  "Shape"  "Solid"  "Trace"    "3dface" 
                      "Viewport"
                       "** Begin  AND" "** End    AND"
                       "** Begin  OR"  "** End    OR"
                       "** Begin  XOR" "** End    XOR"
                       "** Begin  NOT" "** End    NOT"
       ))
       (mode_tile "x_op" 1) (mode_tile "x_value" 1) (mode_tile "x_text" 1)
       (mode_tile "y_op" 1) (mode_tile "y_value" 1) (mode_tile "y_text" 1)
       (mode_tile "z_op" 1) (mode_tile "z_value" 1) (mode_tile "z_text" 1)
       (mode_tile "select" 1)
      )
      ((member string '(
                       "Arc Center"
                       "Attribute Position"
                       "Block Position"
                       "Circle Center"
                       "Line Start"          "Line End"
                       "Point Position" 
                       "Shape Position"
                       "Solid Point 1"       "Solid Point 2"   "Solid Point 3"
                       "Solid Point 4"
                       "Text Position"
                       "Trace Point 1"       "Trace Point 2"   "Trace Point 3"
                       "Trace Point 4"
                       "3dface Point 1"      "3dface Point 2"  "3dface Point 3"
                       "3dface Point 4"
                       "Viewport Center"
                       )
       )
        (mode_tile "x_op" 0) (mode_tile "x_value" 0) (mode_tile "x_text" 0)
        (mode_tile "y_op" 0) (mode_tile "y_value" 0) (mode_tile "y_text" 0)
        (mode_tile "z_op" 0) (mode_tile "z_value" 0) (mode_tile "z_text" 0)
        (mode_tile "select" 1)
      )
      ((member string '(
                       "Elevation" "Thickness"
                       "Arc Radius"
                       "Block X Scale"   "Block Y Scale" "Block Z Scale"
                       "Block Rotation"
                       "Circle Radius"
                       "Text Height"     "Text Rotation"
                       )
       )
        (mode_tile "x_op" 0) (mode_tile "x_value" 0) (mode_tile "x_text" 0)
        (mode_tile "y_op" 1) (mode_tile "y_value" 1) (mode_tile "y_text" 1)
        (mode_tile "z_op" 1) (mode_tile "z_value" 1) (mode_tile "z_text" 1)
        (mode_tile "select" 1)
      )
      ((member string '(
                       "Color"                    
                       )
       )
        (mode_tile "x_op" 0) (mode_tile "x_value" 0) (mode_tile "x_text" 0)
        (mode_tile "y_op" 1) (mode_tile "y_value" 1) (mode_tile "y_text" 1)
        (mode_tile "z_op" 1) (mode_tile "z_value" 1) (mode_tile "z_text" 1)
        (mode_tile "select" 0)
      )
      ((member string '(
                       "Dimension Type" 
                       "Polyline Flags" "Viewport Status"                   
                       )
       )
        (mode_tile "x_op" 0) (mode_tile "x_value" 0) (mode_tile "x_text" 0)
        (mode_tile "y_op" 1) (mode_tile "y_value" 1) (mode_tile "y_text" 1)
        (mode_tile "z_op" 1) (mode_tile "z_value" 1) (mode_tile "z_text" 1)
        (mode_tile "select" 1)
      )
      ((member string '(
                       "Attribute Tag"  
                       "Text Value"  
                       "Shape Name"
                       )
       )
        (mode_tile "x_op" 1) (mode_tile "x_value" 0) (mode_tile "x_text" 0)
        (mode_tile "y_op" 1) (mode_tile "y_value" 1) (mode_tile "y_text" 1)
        (mode_tile "z_op" 1) (mode_tile "z_value" 1) (mode_tile "z_text" 1)
        (mode_tile "select" 1)
      )
      ((member string '(
                       "Block Name" 
                       "Dimension Style" 
                       "Layer" 
                       "Linetype" 
                       "Text Style Name" 
                       "Xdata ID"
                       )
       )       
        (mode_tile "x_op" 1) (mode_tile "x_value" 0) (mode_tile "x_text" 0)
        (mode_tile "y_op" 1) (mode_tile "y_value" 1) (mode_tile "y_text" 1)
        (mode_tile "z_op" 1) (mode_tile "z_value" 1) (mode_tile "z_text" 1)
        (mode_tile "select" 0)
      )
      ((member string '(
                       "Normal Vector"
                       )
       )       
        (mode_tile "x_op" 0) (mode_tile "x_value" 0) (mode_tile "x_text" 0)
        (mode_tile "y_op" 1) (mode_tile "y_value" 0) (mode_tile "y_text" 0)
        (mode_tile "z_op" 1) (mode_tile "z_value" 0) (mode_tile "z_text" 0)
        (mode_tile "select" 1)
      )
    )
  )
  ;;
  ;; Add the selected filter, operator, and value to list.
  ;;
  (defun add_to_list (/ gc_name op val str_val)
    (setq str_pos (atoi (get_tile "filter_str_list")))  ; item in string list.
    (setq op nil)(setq val nil)(setq str_val nil)
    (setq gc_name (nth (atoi (get_tile "filter_by")) filter_list))
    (cond
      ((member gc_name '(
                        "Arc Center"
                        "Attribute Position"
                        "Block Position"
                        "Circle Center"
                        "Line Start"         "Line End"
                        "Point Position" 
                        "Shape Position"
                        "Solid Point 1"      "Solid Point 2"   "Solid Point 3"
                        "Solid Point 4"
                        "Text Position"
                        "Trace Point 1"      "Trace Point 2"   "Trace Point 3"
                        "Trace Point 4"
                        "3dface Point 1"     "3dface Point 2"  "3dface Point 3"
                        "3dface Point 4"
                        "Viewport Center"
                        )
       )     
        (setq op (cons -4 (strcat 
                            (setq x_op (nth (atoi (get_tile "x_op")) ri_ops)) 
                            ","
                            (setq y_op (nth (atoi (get_tile "y_op")) ri_ops)) 
                            ","
                            (setq z_op (nth (atoi (get_tile "z_op")) ri_ops))
                           )
                  )
        )
        (cond
          ((not (setq x_value 
                      (ai_num (get_tile "x_value") "Invalid X coordinate." 0)
                )
           )
           (mode_tile "x_value" 2)
          )
          ((not (setq y_value 
                      (ai_num (get_tile "y_value") "Invalid Y coordinate." 0)
                )
           )
           (mode_tile "y_value" 2)
          )
          ((not (setq z_value 
                      (ai_num (get_tile "z_value") "Invalid Z coordinate." 0)
                )
           )
           (mode_tile "z_value" 2)
          )
          (T (setq val 
                   (list 
                         (nth (what_pos gc_name filter_list) filter_gc) 
                         x_value 
                         y_value 
                         z_value
                   )
             )
             (setq str_val (strcat gc_name "\tX\t" x_op "\t" (rtos x_value) 
                                           "\tY\t" y_op "\t" (rtos y_value)
                                           "\tZ\t" z_op "\t" (rtos z_value)
                           )
             )
          )
        )
      )
      ((member gc_name '("Normal Vector"))
        (setq op (cons -4 (setq x_op (nth (atoi (get_tile "x_op")) ri_ops))))
        (cond
          ((not (setq x_value 
                      (ai_num (get_tile "x_value") "Invalid X coordinate." 0)
                )
           )
           (mode_tile "x_value" 2)
          )
          ((not (setq y_value 
                      (ai_num (get_tile "y_value") "Invalid Y coordinate." 0)
                )
           )
           (mode_tile "y_value" 2)
          )
          ((not (setq z_value 
                      (ai_num (get_tile "z_value") "Invalid Z coordinate." 0)
                )
           )
           (mode_tile "z_value" 2)
          )
          (T (setq val (list 
                             (nth (what_pos gc_name filter_list) filter_gc) 
                             x_value 
                             y_value 
                             z_value
                       )
             )
             (setq str_val (strcat gc_name     "\tX\t" x_op "\t" (rtos x_value) 
                                               "\tY\t" x_op "\t" (rtos y_value)
                                               "\tZ\t" x_op "\t" (rtos z_value)
                           )
             )
          )
        )
      )
      ((member gc_name '(
                          "Elevation" "Thickness"
                          "Arc Radius"
                          "Block X Scale"   "Block Y Scale" "Block Z Scale"
                          "Circle Radius"
                          "Text Height"    
                        )
       )     
        (setq op (cons -4 (setq x_op (nth (atoi (get_tile "x_op")) ri_ops))))
        (cond
          ((not (setq x_value 
                      (ai_num (get_tile "x_value") "Invalid number." 0)
                )
           )
           (mode_tile "x_value" 2)
          )
          (T (setq val 
                   (cons (nth (what_pos gc_name filter_list) filter_gc) 
                         x_value
                   )
             )
             (setq str_val (strcat gc_name  "\t\t" x_op "\t" (rtos x_value)))
          )
        )
      )
      ((member gc_name '(
                          "Block Rotation"
                          "Text Rotation"
                        )
       )     
        (setq op (cons -4 (setq x_op (nth (atoi (get_tile "x_op")) ri_ops))))
        (cond
          ((not (setq x_value 
                      (ai_angle (get_tile "x_value") "Invalid angle.")
                )
           )
           (mode_tile "x_value" 2)
          )
          (T (setq val 
                   (cons (nth (what_pos gc_name filter_list) filter_gc) 
                         x_value
                   )
             )
             (setq str_val (strcat gc_name  "\t\t" x_op "\t" (rtos x_value)))
          )
        )
      )
      ((member gc_name '(
                         "Color" 
                        )
       )
        (setq op (cons -4 (setq x_op (nth (atoi (get_tile "x_op")) ri_ops))))
        (cond
          ((not (setq x_value 
                      (check_color (get_tile "x_value"))
                )
           )
           (mode_tile "x_value" 2)
          )
          (T (setq val 
                   (cons (nth (what_pos gc_name filter_list) filter_gc) 
                         x_value
                   )
             )
             (cond 
               ((= 0 x_value)   (setq x_value "0 - By Block"))
               ((= 1 x_value)   (setq x_value "1 - Red"))
               ((= 2 x_value)   (setq x_value "2 - Yellow"))
               ((= 3 x_value)   (setq x_value "3 - Green"))
               ((= 4 x_value)   (setq x_value "4 - Cyan"))
               ((= 5 x_value)   (setq x_value "5 - Blue"))
               ((= 6 x_value)   (setq x_value "6 - Magenta"))
               ((= 7 x_value)   (setq x_value "7 - White"))
               ((= 256 x_value) (setq x_value "256 - By Layer"))
               (t (setq x_value (itoa x_value)))
             )
             (setq str_val (strcat gc_name "\t\t" x_op "\t" x_value))
          )
        )
      )
      ((member gc_name '(
                          "Attribute Tag" "Block Name" 
                          "Dimension Style" 
                          "Layer" "Linetype" 
                          "Shape Name"
                          "Text Value" "Text Style Name" 
                        )
       )         
        (cond
          ((= "" (setq x_value (ai_strtrim (get_tile "x_value"))))
            (mode_tile "x_value" 2)
          )
          (T (setq val 
                   (cons (nth (what_pos gc_name filter_list) filter_gc) 
                         x_value
                   )
             )
             (setq str_val (strcat gc_name "\t\t=\t" x_value))
          )
        )
      )
      ((member gc_name '("Xdata ID"))
        (cond
          ((= "" (setq x_value (ai_strtrim (get_tile "x_value"))))
            (mode_tile "x_value" 2)
          )
          (T (setq val 
                     (cons 
                       (nth (what_pos gc_name filter_list) filter_gc) 
                       (list (list x_value))
                     )
             )
             (setq str_val (strcat gc_name "\t\t=\t" x_value))
          )
        )
      )
      ((member gc_name '(
                        "Arc"   "Circle" "Dimension" "Line" 
                        "Point" "Polyline"  "Shape"  "Solid"  "Text" "Trace"  
                        "3dface" "Viewport"
                        )
       )
        (setq val (cons 0 gc_name))
        (setq str_val (strcat "Entity         \t\t=\t" gc_name)) 
      )
      ((member gc_name '(
                         "Attribute"  
                        )
       )
        (setq val (cons 0 "ATTDEF"))
        (setq str_val (strcat "Entity         \t\t=\t" gc_name)) 
      )
      ((member gc_name '(
                         "Block" 
                        )
       )
        (setq val (cons 0 "INSERT"))
        (setq str_val (strcat "Entity         \t\t=\t" gc_name)) 
      )
      ((member gc_name '(
                        "** Begin  AND"  "** End    AND"
                        "** Begin  OR"   "** End    OR"
                        "** Begin  XOR"  "** End    XOR"
                        "** Begin  NOT"  "** End    NOT"
                        )
       )
        (setq val (cons -4  (nth (what_pos gc_name filter_list) filter_gc)))
        (setq str_val (strcat gc_name "\t")) 
      )
      (T)
    )
    (cond 
      ((and op val str_val)
        (update (list str_val) (list op val))
        (set_tile "named_lists" "0")
      )
      ((and val str_val)
        (update (list str_val) (list val))
        (set_tile "named_lists" "0")
      )
      (T)
    )
  )
  ;;
  ;; Check if value passed is a valid color integer.  If valid, return the 
  ;; integer, else nil.
  ;;
  (defun check_color(value)
    (if (or (wcmatch value "*@*,*.*")  ;  alphabetic or nonalphanumeric.
             (>  0 (distof value))
             (<  256 (distof value))
        )
      (progn (set_tile "error" "Invalid Color number.") nil)
      (atoi value) 
    )
  )
  ;;
  ;; Check if value passed is an integer.  If valid, return the integer, else
  ;; nil.
  ;;
  (defun check_int(value)
    (if (and (wcmatch value "*@*,*.*")  ;  
             (<= 0 value)
             (<  256 value))
      (progn (set_tile "error" "Invalid Color number.") nil)
      (atoi value) 
    )
  )
  ;;
  ;; Pass an item and a list and recieve a number showing it's position in  
  ;; the list, nil otherwise.  Item must be in the list, and the list must 
  ;; contain unique names. 0 if first item.                               
  ;;
  (defun what_pos (item the_list / pos)
    (setq pos (- (length the_list)
                 (length (member item the_list)))
    )          
  )
  ;;
  ;; Remove item from English and Lisp lists.
  ;;
  (defun remove()
    (setq str_pos (atoi (get_tile "filter_str_list")))  ; item in string list.
    (if (/= (1- (length filter_str_list)) str_pos) ; don't remove the blank 
      (progn
        ; strip 1 item from string list
        (setq filter_str_list (rm_item str_pos filter_str_list))
        (setq ai_str|*unnamed filter_str_list)
        (setq i -1)
        (setq edit_this -1)        ; corresponding item in lisp list.
        (while (< edit_this str_pos)  ; until they are equal
          (setq i (1+ i))
          (if (not (and (= -4 (car (nth i filter_lisp_list)))  
                        (not (member (cdr (nth i filter_lisp_list)) 
                                  '("<AND" "AND>" "<OR"  "OR>" 
                                    "<XOR" "XOR>" "<NOT" "NOT>")
                             )
                        )
                   )
              )
            (setq edit_this (1+ edit_this))        
          )
        )
        (setq filter_lisp_list (rm_item i filter_lisp_list))
        (setq ai_lisp|*unnamed filter_lisp_list)
        (set_tile "named_lists" "0")
        (if (and (< 0 i) 
                 (and (= -4 (car (nth (1- i) filter_lisp_list)))   
                      (not (member (cdr (nth (1- i) filter_lisp_list)) 
                                  '("<AND" "AND>" "<OR"  "OR>" 
                                    "<XOR" "XOR>" "<NOT" "NOT>")
                           )
                      )
                 )
            )
          (progn
            (setq filter_lisp_list (rm_item (1- i) filter_lisp_list))
            (setq ai_lisp|*unnamed filter_lisp_list)
          )
        )
        (if (= 1 remove_flag)   ; only redisplay if remove, not with substitute
          (progn 
            (start_list "filter_str_list")
            (mapcar 'add_list filter_str_list)
            (end_list)
            ;; set highlight after removing item unless it's a blank list.
            (cond
              ((/= (1- (length filter_str_list)) str_pos)
                (set_tile "filter_str_list" (itoa str_pos))
              )
              ((and (= (1- (length filter_str_list)) str_pos)
                    (/= 1 (length filter_str_list))
               )
                (set_tile "filter_str_list" (itoa (1- str_pos)))
              )
              (T)
            )
          )
        )
      )
    )
  )
  ;;
  ;; Pass a number and a list and recieve the list back with that item missing.
  ;;
  (defun rm_item (value the_list)
    (setq temp_lisp_list '())
    (setq j 0)
    (foreach n the_list
      (if (/= value j)
        (setq temp_lisp_list (cons n temp_lisp_list))
      )
      (setq j (1+ j))
    )
    (setq temp_lisp_list (reverse temp_lisp_list))
  )
  ;;
  ;;  Get the fields of the highlighted item and place them in the edit area.
  ;;
  (defun do_edit()
    (setq edit_item (atoi (get_tile "filter_str_list")))  ;item in string list.
    (if (/= "" (nth edit_item filter_str_list)) 
      (progn 
        (setq i -1)
        (setq edit_this -1)        ; corresponding item in lisp list.
        (while (< edit_this edit_item)  ; until they are equal
          (setq i (1+ i))
          (if (not (and (= -4 (car (nth i filter_lisp_list)))  
                        (not (member (cdr (nth i filter_lisp_list)) 
                                    '("<AND" "AND>" "<OR"  "OR>"
                                      "<XOR" "XOR>" "<NOT" "NOT>")
                             )
                        )
                   )
              )
            (setq edit_this (1+ edit_this))   
          )
        )
        (setq gc_name (nth edit_item filter_str_list))
        (setq a 1)
        (while (/= "\t" (substr gc_name a 1))
          (setq a (1+ a))
        )
        (setq str_name (substr gc_name 1 (1- a)))
        (setq j 1)(setq ws nil)
        (cond 
          ((member (car (nth i filter_lisp_list)) '(-4 -3 1 2 3 6 8 38 39 40 
                                                    41 43 44 45 50 51 62 66 
                                                    70 71 10 11 12 13 14 15 
                                                    16 210))
            (set_tile "filter_by" 
                      (itoa (- (length filter_list) 
                               (length (member str_name filter_list))
                            )
                      )
            )
          )
          ((member (car (nth i filter_lisp_list)) '(0))
            (cond
              ((= "ATTDEF" (cdr (nth i filter_lisp_list)))
                (set_tile "filter_by" 
                          (itoa (what_pos "Attribute" filter_list))
                )
              )
              ((= "INSERT" (cdr (nth i filter_lisp_list)))
                (set_tile "filter_by" (itoa (what_pos "Block" filter_list)))
              )
              (T
                (set_tile "filter_by" 
                          (itoa (- (length filter_list)
                                   (length 
                                     (member 
                                       (strcat 
                                         (substr 
                                           (cdr (nth i filter_lisp_list)) 
                                           1 1
                                         )
                                         (strcase 
                                           (substr 
                                             (cdr (nth i filter_lisp_list)) 
                                             2
                                           ) 
                                           T
                                         )   
                                       )      
                                       filter_list
                                     )
                                   )
                                )
                          )
                    )
                )
            )
          )
          (T (princ "Error in setting by_filter - group code missing"))
        )
        (enable_disable str_name)
        (cond 
          ((member (car (nth i filter_lisp_list)) '(10 11 12 13 14 15 16))
            (set_tile "x_value" (rtos (cadr (nth i filter_lisp_list))))
            (set_tile "y_value" (rtos (caddr (nth i filter_lisp_list))))
            (set_tile "z_value" (rtos (cadddr (nth i filter_lisp_list))))
            (setq ops_3 (cdr (nth (1- i) filter_lisp_list)))
            (setq j 1)
            (setq c1 nil) (setq c2 nil)
            (while (<= j (strlen ops_3))
              (cond 
                ((and (= "," (substr ops_3 j 1))
                      (= nil c1)) 
                  (setq c1 j)
                )
                ((and (= "," (substr ops_3 j 1))
                      (/= nil c1)) 
                  (setq c2 j)
                )
              )
              (setq j (1+ j))
            )
            (set_tile "x_op" 
                      (rtos (- (length ri_ops) 
                               (length 
                                 (member (substr ops_3 1 (- c1 1)) ri_ops))
                               )
                      )
            ) 
            (set_tile "y_op" 
                      (rtos (- (length ri_ops) 
                               (length 
                                 (member 
                                   (substr ops_3 (1+ c1) (1- (- c2 c1))) 
                                   ri_ops
                                 )
                               )
                            )
                      )        
            ) 
            (set_tile "z_op" 
                      (rtos (- (length ri_ops) 
                               (length (member (substr ops_3 (1+ c2)) ri_ops))
                            )
                      )
            )  
          )
          ((member (car (nth i filter_lisp_list)) '(210))
            (set_tile "x_value" (rtos (cadr (nth i filter_lisp_list))))
            (set_tile "y_value" (rtos (caddr (nth i filter_lisp_list))))
            (set_tile "z_value" (rtos (cadddr (nth i filter_lisp_list))))
            (set_tile "x_op" 
                      (rtos (- (length ri_ops)
                               (length 
                                 (member 
                                   (cdr (nth (- i 1) filter_lisp_list)) 
                                   ri_ops
                                 )
                               )
                            )
                      )
            )   
          )
          ((member (car (nth i filter_lisp_list)) '(38 39 40 41 44 45 50 51))  
            (set_tile "x_value" (rtos (cdr (nth i filter_lisp_list))))
            (set_tile "x_op" 
                      (rtos (- (length ri_ops)
                               (length 
                                 (member 
                                   (cdr (nth (- i 1) filter_lisp_list)) 
                                   ri_ops
                                 )
                               )
                            )
                      )
            )          
          )
          ((member (car (nth i filter_lisp_list)) '(66 70 71))      ; integers
            (set_tile "x_value" (itoa (cdr (nth i filter_lisp_list))))
            (set_tile "x_op" 
                      (rtos (- (length ri_ops)
                               (length 
                                 (member 
                                   (cdr (nth (- i 1) filter_lisp_list)) 
                                   ri_ops
                                 )
                               )
                            )
                      )
            )    
          )
          ((member (car (nth i filter_lisp_list)) '(62))      ;  Color
            (set_tile "x_value" (itoa (cdr (nth i filter_lisp_list))))
            (set_tile "x_op" 
                      (rtos (- (length ri_ops)
                               (length 
                                 (member 
                                   (cdr (nth (- i 1) filter_lisp_list)) 
                                   ri_ops
                                 )
                               )
                            )
                      )
            )   
          )
          ((member (car (nth i filter_lisp_list)) '(2 3 5))      ; strings
            (set_tile "x_value" (cdr (nth i filter_lisp_list)))
          )
          ((member (car (nth i filter_lisp_list)) '(6 8))      ;  table strings
            (set_tile "x_value" (cdr (nth i filter_lisp_list)))
          )
          ((member (car (nth i filter_lisp_list)) '(-3))      ; xdata
            (set_tile "x_value" (caadr (nth i filter_lisp_list)))
          )
          ((member (car (nth i filter_lisp_list)) '(0))     ; 0 code is special
          )
          ((member (car (nth i filter_lisp_list)) '(-4))      ; -4 code is special
          )
        )
      )
    )
  )
  ;;
  ;; Clears the list.
  ;;
  (defun clear_list()
    (setq filter_lisp_list '())
    (setq filter_str_list '(""))
    (setq str_pos 0)
    (setq ai_lisp|*unnamed filter_lisp_list)
    (setq ai_str|*unnamed filter_str_list)
    (set_tile "named_lists" "0")
    (start_list "filter_str_list")
    (mapcar 'add_list filter_str_list)
    (end_list)
  )
  ;;
  ;;  Hide the dialogue, allow user selection of an entity, get the relevant
  ;;  information, translate to English, add both Lisp and English to relevant
  ;;  lists at current cursor position.
  ;;
  (defun get_entity()
    (setq entity_lisp '())
    (if (setq entity_ename (entsel))
      (progn
        (setq entity_lisp_init (cdr (entget (car entity_ename) (list "*"))))
        (setq entity_type (cdar entity_lisp_init))
        (cond 
          ((= entity_type "ARC")      (do_arc))
          ((= entity_type "CIRCLE")   (do_circle))
          ((= entity_type "INSERT")   (do_block))
          ((= entity_type "LINE")     (do_line))
          ((= entity_type "POINT")    (do_point))
          ((= entity_type "POLYLINE") (do_polyline))
          ((= entity_type "SHAPE")    (do_shape))
          ((= entity_type "SOLID")    (do_solid))
          ((= entity_type "TEXT")     (do_text))
          ((= entity_type "ATTDEF")   (do_attdef))
          ((= entity_type "TRACE")    (do_trace))
          ((= entity_type "3DFACE")   (do_3dface))
          ((= entity_type "VIEWPORT") (do_viewport))
          ((= entity_type "DIMENSION") (do_dimension))
          (T (temp))
        )
        (update (lts entity_lisp 0) entity_lisp_list)
      )
    )
  )
  ;;
  ;;  Arc
  ;;
  (defun do_arc()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Arc"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 10  (car n)) (group_10 "Arc Center"))
        ((= 40  (car n)) (group_40 "Arc Radius"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
        (t)
      )
    )
  )
  ;;
  ;;  Attribute Definition.
  ;;
  (defun do_attdef()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Attribute"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 2   (car n)) (group_8  "Attribute Tag"))
        ((= 10  (car n)) (group_10 "Attribute Position"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;  Insert Entity aka block
  ;;
  (defun do_block()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Block"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 2   (car n)) (group_8  "Block Name"))
        ((= 10  (car n)) (group_10 "Block Position"))
        ((= 50  (car n)) (group_40 "Block Rotation"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;; Circle
  ;;
  (defun do_circle()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Circle"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 10  (car n)) (group_10 "Circle Center"))
        ((= 40  (car n)) (group_40 "Circle Radius"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;  Dimension
  ;;
  (defun do_dimension()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Dimension"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 3   (car n)) (group_8   "Dimension Style"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;  Line
  ;;
  (defun do_line()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Line"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 10  (car n)) (group_10 "Line Start"))
        ((= 11  (car n)) (group_10 "Line End"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;  Point
  ;;
  (defun do_point()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Point"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 10  (car n)) (group_10 "Point Position"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;  Polyline
  ;;
  (defun do_polyline()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Polyline"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;  Shape
  ;;
  (defun do_shape()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Shape"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 10  (car n)) (group_10 "Shape Position"))
        ((= 2   (car n)) (group_8  "Shape Name"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;  Solid
  ;;
  (defun do_solid()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Solid"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 10  (car n)) (group_10 "Solid Point 1"))
        ((= 11  (car n)) (group_10 "Solid Point 2"))
        ((= 12  (car n)) (group_10 "Solid Point 3"))
        ((= 13  (car n)) (group_10 "Solid Point 4"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;  Text
  ;;
  (defun do_text()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Text"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 10  (car n)) (group_10 "Text Position"))
        ((= 1   (car n)) (group_8  "Text Value"))
        ((= 7   (car n)) (group_8  "Text Style Name"))
        ((= 40  (car n)) (group_40 "Text Height"))
        ((= 50  (car n)) (group_40 "Text Rotation"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;   Trace
  ;;
  (defun do_trace()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Trace"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 10  (car n)) (group_10 "Trace Point 1"))
        ((= 11  (car n)) (group_10 "Trace Point 2"))
        ((= 12  (car n)) (group_10 "Trace Point 3"))
        ((= 13  (car n)) (group_10 "Trace Point 4"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;   3Dface
  ;;
  (defun do_3dface()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "3dface"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 10  (car n)) (group_10 "3dface Point 1"))
        ((= 11  (car n)) (group_10 "3dface Point 2"))
        ((= 12  (car n)) (group_10 "3dface Point 3"))
        ((= 13  (car n)) (group_10 "3dface Point 4"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;  Viewport
  ;;
  (defun do_viewport()
    (foreach n entity_lisp_init
      (cond 
        ((= 0   (car n)) (group_0  "Viewport"))
        ((= 8   (car n)) (group_8  "Layer"))
        ((= 6   (car n)) (group_8  "Linetype"))
        ((= 38  (car n)) (group_40 "Elevation"))
        ((= 39  (car n)) (group_40 "Thickness"))
        ((= 62  (car n)) (group_62 "Color"))
        ((= 10  (car n)) (group_10 "Viewport Center"))
        ((= 210 (car n)) (group_210)) 
        ((= -3  (car n)) (group_-3))
      )
    )
  )
  ;;
  ;;   Group code 0
  ;;
  (defun group_0(label)
    (setq entity_lisp (cons (list "Entity \t\t=\t" label) entity_lisp))
    (setq entity_lisp_list (list n))
  )
  ;;
  ;;   Strings
  ;;
  (defun group_8(label)
    (setq entity_lisp (cons (list label "\t\t=\t" (cdr n)) entity_lisp))
    (setq entity_lisp_list (reverse (cons n (reverse entity_lisp_list))))
  )
  ;;
  ;;   Coordinate
  ;;
  (defun group_10(label)
    (if (not (assoc 6 entity_lisp_list))
      (progn 
        (setq entity_lisp 
              (cons (list "Linetype\t\t=\t" "BYLAYER") entity_lisp)
        )
        (setq entity_lisp_list 
              (reverse (cons (cons 6 "BYLAYER") (reverse entity_lisp_list)))
        )
      )
    )
    (if (not (assoc 62 entity_lisp_list))
      (progn 
        (setq entity_lisp (cons (list "Color\t\t=\t" "BYLAYER") entity_lisp))
        (setq entity_lisp_list 
              (reverse (cons 
                         (cons 62 256) 
                         (cons (cons -4 "=") (reverse entity_lisp_list))
                       )
               )
        )
      )
    )
    (setq entity_lisp (cons (list label 
                                   "\tX\t=\t" (rtos (cadr n))
                                   "\tY\t=\t" (rtos (caddr n))
                                   "\tZ\t=\t" (rtos (cadddr n))
                             )
                             entity_lisp))
    (setq entity_lisp_list 
          (reverse 
            (cons n (cons (cons -4 "=,=,=") (reverse entity_lisp_list)))
          )
    )
  )
  ;;
  ;;   Reals
  ;;
  (defun group_40(label)
    (setq entity_lisp (cons (list label "\t\t=\t" (rtos (cdr n))) entity_lisp))
    (setq entity_lisp_list 
          (reverse (cons n (cons (cons -4 "=") (reverse entity_lisp_list))))
    )
  )
  ;;
  ;;   Color
  ;;
  (defun group_62(label / str)
    (setq str (cdr n))
    (cond 
      ((= 0 str)   (setq str "0 - By Block"))
      ((= 1 str)   (setq str "1 - Red"))
      ((= 2 str)   (setq str "2 - Yellow"))
      ((= 3 str)   (setq str "3 - Green"))
      ((= 4 str)   (setq str "4 - Cyan"))
      ((= 5 str)   (setq str "5 - Blue"))
      ((= 6 str)   (setq str "6 - Magenta"))
      ((= 7 str)   (setq str "7 - White"))
      ((= 256 str) (setq str "256 - By Layer"))
      (t (setq str (itoa str)))
    )
    (setq entity_lisp (cons (list label "\t\t=\t" str) entity_lisp))
    (setq entity_lisp_list 
          (reverse (cons n (cons (cons -4 "=") (reverse entity_lisp_list))))
    )
  )
  ;;
  ;;   Normal Vector
  ;;
  (defun group_210()
    (if (not (assoc 6 entity_lisp_list))
      (progn 
        (setq entity_lisp 
              (cons (list "Linetype\t\t=\t" "BYLAYER") entity_lisp)
        )
        (setq entity_lisp_list 
              (reverse (cons (cons 6 "BYLAYER") (reverse entity_lisp_list)))
        )
      )
    )
    (if (not (assoc 62 entity_lisp_list))
      (progn 
        (setq entity_lisp (cons (list "Color\t\t=\t" "BYLAYER") entity_lisp))
        (setq entity_lisp_list (reverse (cons (cons 62 256) (cons (cons -4 "=") (reverse entity_lisp_list)))))
      ) 
    )
    (setq entity_lisp (cons (list "Normal Vector" 
                                   "\tX\t=\t" (rtos (cadr n))
                                   "\tY\t=\t" (rtos (caddr n))
                                   "\tZ\t=\t" (rtos (cadddr n))
                             )
                             entity_lisp))
    (setq entity_lisp_list 
          (reverse (cons n (cons (cons -4 "=") (reverse entity_lisp_list))))
    )
  )
  ;;
  ;;    Xdata ID
  ;;
  (defun group_-3()
    (setq appid_str "")
    (if (< 1 (length n)) 
      (progn 
        (foreach n1 (cdr n) 
          (setq appid_str (strcat (car n1) "," appid_str))
        )
        (setq appid_str (substr appid_str 1 (1- (strlen appid_str))))
        (setq entity_lisp (subst (list "Xdata ID \t\t=\t" appid_str) 
                                 n 
                                 entity_lisp
                          )
        )
        (setq entity_lisp_list 
              (reverse 
                (cons (list -3 (list appid_str)) (reverse entity_lisp_list))
              )
        )
      )
    )
  )
  ;;
  ;; Changes list of lists to list of strings, if bit_flag = 1 return parens.
  ;;
  (defun lts (the_list bit_flag / n n1 dp1 dp2)       
    (setq list_str '())       ; for display in list box.
    (foreach n the_list 
      (cond
        ((= -3 (car n)) 
          (setq str (strcat "( -3" "(" "\"" (caadr n) "\"" "))" ))
        )
        ((/= (type (cdr n)) 'LIST)     ; is it a dotted pair or a list.
          (setq str "")
          (setq dp1 (what_is_it (car n)))  ; broken out for clarity.
          (setq dp2 (what_is_it (cdr n)))
          ; cdrs are strings except for color, elevation, and thickness
          (if (member (car n) '(38 39 40 41 42 43 50 51 62))
            (setq str (strcat "(" dp1 " . " dp2 ")"))
            (setq str (strcat "(" dp1 " . " "\"" dp2 "\""  ")"))
          )
        )
        (T
          (setq str "")
          (cond 
            ((= 1 bit_flag)
              (foreach n1 n
                (setq str (strcat str (what_is_it n1) " "))
              )
            )
            ((= 0 bit_flag)
              (foreach n1 n
                (setq str (strcat str (what_is_it n1)))
              )
            )
          )
          ; get rid of last white space
          (if (= 1 bit_flag)
            (setq str (strcat "(" (substr str 1 (- (strlen str) 1)) ")" ))
          )
        )
      )
      (setq list_str (cons str list_str))   
    )
  )
  ;;
  ;;  What type is it ?? 
  ;;
  (defun what_is_it (huh / hmmm)
    (cond  
      ((= (type huh) 'INT)  (setq hmmm (itoa huh)))
      ((= (type huh) 'REAL) (setq hmmm (rtos huh (getvar "lunits") 16)))
      ((= (type huh) 'STR)  (setq hmmm huh ))
    ) 
    hmmm      
  )    
  ;;
  ;; Check Lisp list for errors.
  ;;
  (defun lisp_error(/ after_errno temp_ss)
    (setq temp_ss (ssget "X" filter_lisp_list))
    (setq after_errno (getvar "errno"))
    (cond
      ((= 56 after_errno) 
        (set_tile "error" "Invalid Filter list - Premature end.")
      )
      ((= 57 after_errno) 
        (set_tile "error" "Invalid Filter list - Missing Test Operand.")
      )
       ((= 58 after_errno) 
       (set_tile "error" "Invalid Filter list - Invalid op_code string.")
      )
      ((= 59 after_errno) 
        (set_tile "error" "Invalid Filter list - Empty clause/bad nesting.")
      )
      ((= 60 after_errno) 
        (set_tile "error" "Invalid Filter list - Begin/end clause mismatch.")
      )
      ((= 61 after_errno) 
        (set_tile "error" 
                  "Invalid Filter list - Wrong number of XOR/NOT operands."
        )
      )
      ((= 62 after_errno) 
        (set_tile "error" "Invalid Filter list - Maximum nesting exceeded.")
      )
      ((= 63 after_errno) 
        (set_tile "error" "Invalid Filter list - Invalid group code.")
      )
      ((= 64 after_errno) 
        (set_tile "error" "Invalid Filter list - Invalid string test.")
      )
      ((= 65 after_errno) 
        (set_tile "error" "Invalid Filter list - Invalid vector test.")
      )
      ((= 66 after_errno) 
        (set_tile "error" "Invalid Filter list - Invalid real test.")
      )
      ((= 67 after_errno) 
        (set_tile "error" "Invalid Filter list - Invalid integer test.")
      )
      (t nil)
    )
  )
  ;;
  ;; Puts up dialogue for table selection, returns a list of strings on OK and
  ;; nil on Cancel.
  ;;
  (defun single_table (table_name title / pat what_next selection_list)
    (if (not (new_dialog "single_table" dcl_id)) (exit))
    (setq table_list (ai_table table_name 8)) ; List items in specified table.
    (setq pat "*")                ; Set pattern to all items initially.
    (set_tile "pattern" pat)        ; Set the pattern to *.
    (set_tile "title" title)  ; Set the dialogue title to whatever.
    (pat_match pat "table_match")   

    ;; Define what happens when each button is pressed.
    (action_tile "pattern" 
                 "(pat_match (setq pat (strcase $value)) \"table_match\")")

    (action_tile "table_match" "(make_list)")
    ;; return the selection_list on OK.
    (setq what_next (start_dialog))
    (if (= 1 what_next) selection_list nil)  ; return list on OK
  )
  ;;
  ;; Add to Selection List.
  ;;
  (defun make_list (/ item_index string temp_list a)
    (setq selection_list '())     ; initialise list
    (setq string (get_tile "table_match"))
    (setq a 0)
    (while (/= (read string) nil)
      (setq item_index (itoa (read string))) 
      (setq string (substr string (+ 2 (strlen item_index))))
      (setq selection_list 
             (cons (nth (atoi item_index) table_match) selection_list))
      (setq a (1+ a))
    )
    (setq selection_list (reverse selection_list))
    (set_tile "error" (strcat (itoa a) " " table_name "(s) selected."))
  )
  ;;
  ;; This function displays the table list based on the pattern.
  ;;
  (defun pat_match (pat which_box / which_list a)
    (setq which_list '())
    (setq a 0)
    (foreach n table_list 
      (if (wcmatch n pat) 
        (progn 
          (setq which_list (cons n which_list))
        )
      )
      (setq a (1+ a))
    )    
    ;; Alphabetize the matched list.
    (if (and which_list 
             (< (length which_list) (getvar "maxsort"))
        )
      (setq which_list (acad_strlsort which_list))
    ) 
    (start_list which_box)
    (mapcar 'add_list which_list)
    (end_list)
    (set (read (eval which_box)) which_list)
  )
  ;;
  ;; If an error occurs on reading filter.nfl, it is due to a syntax error 
  ;; introduced by someone editing the file.   
  ;;
  (defun load_error (s)                     
    (princ "\nSyntax error in Named Filter List file.  Delete filter.nfl.")
    (if old_error (setq *error* old_error))   ; Restore old *error* handler
    (princ)
  )

  ;; 
  ;; Put up the dialogue.
  ;;
  (defun filter_main()

    ;; Set up error function.
    (setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
          old_error  *error*            ; save current error function
          *error* load_error              ; new error function
    )

    (setvar "cmdecho" 0)

    (setq str_pos           nil
          what_next         3
          filter_str_list   '("")
          filter_lisp_list  '()
          all_lisp_list     '("*unnamed")
    )
    (load_log)     ; if there is a file containing named filter lists, load it.

    (setq *error* ai_error)  ; After loading reset error to normal error.

    (init_lists)   ; initialise the big lists.
    (while (< 1 what_next)             ; loop for hiding dialogue.
      (if (not (new_dialog "filter" dcl_id)) (exit))
      (set_tile "x_value" "0.0000")   ; some default values
      (set_tile "y_value" "0.0000")
      (set_tile "z_value" "0.0000")
      (mode_tile "x_text" 1)(mode_tile "y_text" 1)(mode_tile "z_text" 1)
      (mode_tile "x_op" 1) (mode_tile "x_value" 1)
      (mode_tile "y_op" 1) (mode_tile "y_value" 1)
      (mode_tile "z_op" 1) (mode_tile "z_value" 1)
      (mode_tile "select" 1)

      (start_list "filter_by")           ; the list of possible filters
      (mapcar 'add_list filter_list)
      (end_list)

      (start_list "filter_str_list")     ; the list of chosen filters
      (mapcar 'add_list filter_str_list)
      (end_list)

      (start_list "named_lists")         ; the list of named filter lists
      (mapcar 'add_list all_lisp_list)
      (end_list)
    
      (start_list "x_op")                ; the list of X coordinate filters
      (mapcar 'add_list ri_ops)
      (end_list)

      (start_list "y_op")
      (mapcar 'add_list ri_ops)          ; the list of Y coordinate filters
      (end_list)

      (start_list "z_op")                ; the list of Z coordinate filters
      (mapcar 'add_list ri_ops)
      (end_list)
   
      (if (not str_pos) (setq str_pos 0)) ; position within string list

      (if (not pick)                   ; current selection in possible filters
        (progn 
          (setq pick "Arc")
          (set_tile "filter_by" "0")
        )
        (progn 
          (set_tile "filter_by" (itoa (what_pos pick filter_list)))
          (grey_filter)
        )
      )
      ;; Get the default named list from ai_defaults.
      (if (and (= 3 what_next)
               (setq last_name (cadr (assoc "filter" ai_defaults)))
          )
        (progn
          (if (not (member last_name all_lisp_list))  ; may no longer exist.
            (setq last_name "*unnamed")
          )
          (setq pick_list (what_pos last_name all_lisp_list))
          (set_tile "named_lists" (itoa pick_list))
          (setq filter_str_list (eval (read (strcat "ai_str|" last_name))))
          (setq filter_lisp_list (eval (read (strcat "ai_lisp|" last_name))))
          (start_list "filter_str_list")
          (mapcar 'add_list filter_str_list)
          (end_list)
        )
        (progn
          (setq pick_list 0)
          (set_tile "named_lists" "0") 
          (setq last_name "*unnamed")
        )
      )

      (set_tile "filter_str_list" (itoa str_pos))

      (action_tile "filter_str_list" "(rs_err)(setq str_pos (atoi $value))")
      (action_tile "select_entity"   "(rs_err)(do_select_entity)")
      (action_tile "edit"            "(rs_err)(do_edit)")
      (action_tile "clear_list"      "(rs_err)(clear_list)")
      (action_tile "remove"          "(rs_err)(do_remove)")
      (action_tile "filter_by"       "(rs_err)(grey_filter)")
      (action_tile "select"          "(rs_err)(select)")
      (action_tile "add_to_list"     "(rs_err)(add_to_list)")
      (action_tile "substitute"      "(rs_err)(remove)(add_to_list)")
      (action_tile "apply"           "(if (not (lisp_error))(done_dialog 1))")
      (action_tile "save_as"         "(rs_err)(save_as)")
      (action_tile "named_lists"     "(rs_err)(named_lists)")
      (action_tile "delete_list"     "(rs_err)(delete_list)")
      (action_tile "help"            "(acad_helpdlg \"acad.hlp\" \"filter\")")

      (setq what_next (start_dialog))
      (if (= 2 what_next) (get_entity))
    )    
    (if (= 1 what_next) 
      (progn
        ;; Use this name as the default next time.
        (if (not list_name) (setq list_name "*unnamed"))
        (if (assoc "filter" ai_defaults)
          (setq ai_defaults (subst (list "filter" list_name) 
                                   (assoc "filter" ai_defaults)
                                   ai_defaults
                            )
          )
          (setq ai_defaults (cons (list "filter" list_name) ai_defaults))
        )
        (princ "\nApplying filter to selection.  ")
        (setq ret_list (ssget filter_lisp_list))
        (princ "\nExiting filtered selection.  ")
      )
    )
    (foreach n all_lisp_list      ; set all named lists to nil
      (if (/= n "*unnamed")
        (progn 
          (set (read (strcat "ai_str|" n)) nil)
          (set (read (strcat "ai_lisp|" n)) nil)
        )
      )
    )
    (setq *error* old_error) 
    (setvar "cmdecho" old_cmd)
  )

  (cond
     (  (not (ai_transd)))                       ; transparent OK
     (  (not (ai_acadapp)))                      ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "filter"))))  ; is .DCL file loaded?
     (t (filter_main))                          ; proceed!
  )

  ;; Return the list is there is a command active, else exit quietly.
  (if (and (/= 0 (getvar "cmdactive"))
           (= 1 what_next)
      )
    ret_list
    (princ)
  )
)

;;;----------------------------------------------------------------------------
(princ "  FILTER loaded.  ")
(princ)

