;;$Id: eedcnvrt.xsp,v 1.4 1997/04/29 08:41:42 satsysa Exp $
; Next available MSG number is    44
; MODULE_ID EEDCNVRT_LSP_
;;;
;;;    eedcnvrt.lsp
;;;    
;;;    (C) Copyright 1995,1996,1997 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;.


(defun c:EEDCONVERT ( / ss prev_error prev_cmdecho prev_hilite)
	(setq prev_error *error*)

	(if (not (listp *error*))
		(defun *error* (st)
			(setq *error* prev_error)
			(princ st)
			(princ)
		)
	)

	(if (not ade_version) (progn ; ade is not loaded yet...
		(alert "ADE must be loaded prior to running this program.")
		(exit)
		)
	)

	(princ "\nSelect objects to convert (<Enter> for All):")

	(setq ss (ssget))
	(if (not ss) (progn
		;(princ "\nSearching for objects in the drawing with EED...")
		(setq ss (ssget "_X"))
		)
	)
	(if (not ss)
		(princ "\nNo objects in the drawing have EED.")
		; else
		(progn
			(princ "\nFound ")(princ (sslength ss))
			(princ " objects with EED.")
			(do_conversion ss "")
		)
	)
	(setq ss Nil)
	(princ)
)

(defun get_names (ss / cnt ename name_list var_list)
	(princ "\nGathering field names...")
	(princ "\nProcessing 0")
	(setq name_list nil)
	(setq cnt 0)
	(repeat (sslength ss)
		(setq ename (ssname ss cnt))
		(setq cnt (1+ cnt))
		(princ "\015Processing ")(princ cnt)

		(setq var_list (get_eed ename T))
		(setq name_list (update_namelist name_list var_list))
	)
	(princ " done.")
	name_list
)
	
			
(defun update_namelist (name_list var_list / appname applist var)
	(foreach appname var_list

		; get the list of fields for the application

		(setq applist (cdr (assoc (car appname) name_list)))

		; for each var in the appname, see if its already in 
		; the list.  If no, add it...

		(foreach var (cdr appname)
			(setq var (strcase var))
			(if (not (member var applist))
				(setq applist (append applist (list var)))
			)
		)
		(setq applist (cons (car appname) applist))
		(if (not (assoc (car appname) name_list))
			(setq name_list (cons applist name_list))
			(setq name_list 
				(subst applist (assoc (car appname) name_list) name_list))
		)

	)
	name_list
)


(defun do_conversion (ss Where / eed_list user_vars)
	(setq eed_list (get_names ss))

	(if (not eed_list)
		(princ "\nNo EED variables were found attached to the objects.")
		; else
		(progn
			(setq user_vars (get_uservars eed_list))
			(if user_vars
				(convert_data 
					ss 
					(car user_vars) ; flag to delete EED
					(cadr user_vars) ; list of tables and fields to create
				)
			)
		)
	)
)

; Prompt the user for the Tables to create...
(defun get_uservars (name_list / dcl_id fields)
	(setq dcl_id (load_dialog "eedcnvrt.dcl"))
	(if (> dcl_id 0)
		(progn
		    (setq fields (ade_getfieldnames 
		    	name_list 
				Nil ; existing list
				(ade_odtablelist)
		    	   
		    	dcl_id
	    	))
	        (unload_dialog dcl_id)
		)
		; else
		(alert "Cannot load dialog file eedcnvrt.dcl.")
	)
	fields
)
; convert_data - converts EED to Object Data

(defun convert_data (ss removeEED user_eedlist / ename cnt eed_data recids)

	; step through each object, converting the data

	(setq cnt 0)
	(princ "\nConverting data...\n")

	; PJL April 29, 1997
	(setq recids Nil)
	(foreach table user_eedlist
		(setq recid (ade_odnewrecord (car table)))
		(setq recids (cons recid recids))
	)
	(setq recids (reverse recids))

	(repeat (sslength ss)
		(setq ename (ssname ss cnt))
		(setq cnt (1+ cnt))
		(princ "\015Processing ")(princ cnt)

		(eed_to_od ename removeEED user_eedlist recids) ; ; PJL April 29, 1997
	)
	; PJL April 29, 1997
	(foreach recid recids
		(ade_odfreerec recid)
	)

	(princ " done.")
)

(defun create_od_table (user_eedlist / table_defn new_tabldefn tableGood)
	(setq tableGood T)
	(setq new_tabledefn Nil)
	(foreach table user_eedlist
		(if (= (cadr table) "NEW") (progn
			(setq table_defn (create_od_defn table))
			(ade_errclear)
			(ade_oddefinetab table_defn)
			(if (> (ade_errqty) 0)
				(progn
				; didn't create table successfully...
					(alert (ade_errmsg))
					(ade_errclear)
					(setq new_tabledefn (cons table new_tabledefn))
					(setq tableGood Nil)

				)
				; else successfully created the table - set status to OLD
				(progn
					(setq new_tabledefn
						(cons	
							(list
								(car table)
								"OLD"
								(caddr table)
							)
							new_tabledefn
						)
					)
				
				)
			))
			; else table already exists
			(setq new_tabledefn (cons table new_tabledefn))

		) ; end of if


	) ; end of foreach
	(if tableGood
		T ; return successful Creation of Table
		; else return the updated table definitions
		(reverse new_tabledefn)
	)
	
)


; convert the data for one object

(defun eed_to_od (ename removeEED user_eedlist rec_ids / 
				  eed_data table recid field eedvar
			      found tabname appname value recnum status index)

	; add a record for each table and fill the values from
	; the eed data passed in...

	(setq eed_data (get_eed ename Nil))
	(setq index 0)
	(foreach table user_eedlist
		(setq tabname (car table))
		(setq recid (nth index rec_ids))
		(setq index (1+ index))
		
		(setq found Nil)
		(foreach field (caddr table)
			(setq appname (nth 1 field) eedvar (nth 2 field))
			(setq value (get_eed_value appname eedvar eed_data))
			
			; check if a value was found.  If yes, add the value
			(if value (progn
				(setq value (fix_type (nth 3 field) value))
				(ade_odpresetfield recid (car field) value)
				(if removeEED 
					(setq eed_data (eed_remove appname eedvar eed_data))
				)
				(setq found T))
				; else
				(progn
					(setq value (fix_type (nth 3 field) ""))
					(ade_odpresetfield recid (car field) value)
				)

			)
		)
		(if found (ade_odattachrecord ename recid))	
	) ; end foreach
	(if removeEED (eed_update ename eed_data))

)

; Find the value associate of the Variable under the
; specific application name in the EED Data that is on
; an object and return this value...

(defun get_eed_value (appname eedvar eed_data / values)
	(setq values (cdr (assoc appname eed_data)))
	(if values
		(cadr (assoc eedvar values))
		Nil	
	)
)

(defun eed_remove (appname eedvar eed_data / new_eed new_appdata datagroup)
	(foreach datagroup eed_data
		(if (= (car datagroup) appname)
			(progn ; we found the right appname, now get the field
				(setq new_appdata Nil)
				(foreach eed (cdr datagroup)
					(if (/= (car eed) eedvar)
						(setq new_appdata (cons eed new_appdata))
					)
				)
				(setq new_eed (cons (cons appname (reverse new_appdata)) new_eed))
			)
			; 
			(setq new_eed (cons datagroup new_eed))
		)
	)

	(reverse new_eed)
)


(defun parse_commas (astring / tmp_str index in_string curr_char)
   (setq tmp_str "" index 1 in_string Nil)
   (while (and (<= index (strlen astring)) 
   	  	       (or in_string (/= (substr astring index 1) ",")))
      (setq curr_char (substr astring index 1))
	  (if (= curr_char "\"")
		(setq in_string (if in_string Nil T))
        (setq tmp_str (strcat tmp_str curr_char ))
	  )

      (setq index (1+ index))
   )
   (if (> index (strlen astring)) 
      (list tmp_str) 
      (cons tmp_str (parse_commas (substr astring (1+ index))))
   )
)





; ----- function to control conversion dialog ----

(defun ade_getfieldnames (eed_list od_list od_tables dcl_id / curr_od curr_eed)

	; support functions for the dialog
	(defun initialize_eed (eed_list)
		; put list of application names into popup list
		(start_list "ID_PLIST_APPNAME")
		(mapcar 'add_list (mapcar 'car eed_list))
		(end_list)

		(setq CURR_EED (car eed_list))
		(initialize_vars (cdr CURR_EED))
	)

	(defun initialize_vars (var_list)
		; put list of variables into list box
		(start_list "ID_LBOX_VARIABLES")
		(foreach var var_list
			(if (= (type var) 'STR)
				(add_list var)
			)
		)
		(end_list)
		(mode_tile "ID_BUTT_ADD" 1)
	)
	(defun initialize_od (od_list)
		; put list of table names into popup list
		(start_list "ID_PLIST_TABLES")
		(mapcar 'add_list (mapcar 'car od_list))
		(end_list)

		(if od_list (progn 
			(setq CURR_OD (car od_list))
			(if (initialize_fields (nth 2 CURR_OD)) 
				(mode_tile "ID_BUTT_OK" 0)
		    )
		    
			) ; end progn
		)
	)

	(defun initialize_fields (field_list / allow_ok)
		; put list of fields into list box
		(start_list "ID_LBOX_FIELDS")
		(setq allow_ok T)
		(foreach field field_list
		    (add_list (format_field field))
			(if (or (= (nth 1 field) "")(= (nth 2 field) ""))
			    (setq allow_ok Nil)
		    )
		)
		(end_list)
		(mode_tile "ID_BUTT_EDITFIELD" 1)
		(mode_tile "ID_BUTT_REMOVE" 1)
		allow_ok
	)

	(defun format_field (field)
		(if (= (nth 2 field) "")
			(strcat "[...] " (nth 0 field))
			(strcat "[" (nth 2 field) "] " (nth 0 field))
		)
	)

	; --- support function for APPNAME popup list

	(defun act_newappnameselected ()
	    (setq CURR_EED (nth (atoi (get_tile "ID_PLIST_APPNAME")) eed_list))
		(initialize_vars (cdr CURR_EED)) 
	)

	; --- support function for VARIABLES list box

	(defun act_variableselected ()
		(if (or (= (get_tile "ID_LBOX_VARIABLES") "") (not CURR_OD))
			(mode_tile "ID_BUTT_ADD" 1)
			(mode_tile "ID_BUTT_ADD" 0)
		)
	)	

	; --- support function for Select All  (Variables) button

	(defun act_SelectAll_Vars ()
		(setq cnt 0)
		(repeat (length CURR_EED)
			(set_tile "ID_LBOX_VARIABLES" (itoa cnt))
			(setq cnt (1+ cnt))
		)
		(if CURR_OD (mode_tile "ID_BUTT_ADD" 0))

	)
	; --- support function for Select None  (Variables) button

	(defun act_SelectNone_Vars ()
		(set_tile "ID_LBOX_VARIABLES" "")
		(mode_tile "ID_BUTT_ADD" 1)
	)

	; --- support function for Select All  (Fields) button

	(defun act_SelectAll_FIELDS ()
		(setq cnt 0)
		(if CURR_OD
			(repeat (length (nth 2 CURR_OD))
				(set_tile "ID_LBOX_FIELDS" (itoa cnt))
				(setq cnt (1+ cnt))
			)
		)
		(if CURR_OD (mode_tile "ID_BUTT_REMOVE" 0))
	)

	; --- support function for Select None  (Fields) button

	(defun act_SelectNone_FIELDS ()
		(set_tile "ID_LBOX_FIELDS" "")
		(mode_tile "ID_BUTT_REMOVE" 1)
	)

	; --- support function for ADD... button

	(defun act_addselected ()
		(setq vars (read (strcat "(" (get_tile "ID_LBOX_VARIABLES") ")")))
		(setq fields (read (strcat "(" (get_tile "ID_LBOX_FIELDS") ")")))
		; check if too many vars have been selected...
		(if (and 
			(= (nth 1 CURR_OD) "OLD") ; it's an existing table 
			(> (length vars) (length fields)) ; too many vars selected
			) 
			(alert "Cannot add new fields to an existing table.")
			; else
			(progn
				(setq CURR_OD 
					(add_selectedvars vars CURR_EED fields CURR_OD))
				(setq CURR_EED (eed_updatestatus vars CURR_EED))
				(initialize_vars (cdr CURR_EED))
				(if (initialize_fields (nth 2 CURR_OD))
					(mode_tile "ID_BUTT_OK" 0)
				)

			)
		)
	)

	; --- support function for REMOVE... button

	; NOTE: the only time this button is available is if one or more
	; fields have been selected

	(defun act_removeselected ( / new_fields)
		(setq fields_selected
			 (read (strcat "(" (get_tile "ID_LBOX_FIELDS") ")")))

		; remove the selected items from the CURR_OD list and
		; restored then in the EED List....

		(setq curr_fields (nth 2 CURR_OD) cnt 0)
		(foreach item curr_fields
			(if (member cnt fields_selected) (progn 
				(setq eed_name (nth 2 item) app_name (nth 1 item))
				(if (= (strcase app_name) (strcase (car CURR_EED)))
					(setq CURR_EED (eed_restorestatus eed_name CURR_EED))
					)
				) ; end progn

				; else

				(setq new_fields (cons item new_fields))
			)
			(setq cnt (1+ cnt))
		) 
		(setq CURR_OD (list 
			(nth 0 CURR_OD)
			(nth 1 CURR_OD)
			(reverse new_fields)
			)
		)
		(initialize_vars (cdr CURR_EED))
		(initialize_fields (nth 2 CURR_OD))
	)

	; --- support function for EDIT... button

	(defun act_editselected ( / one_field)
		(setq field (atoi (get_tile "ID_LBOX_FIELDS")))
		(setq new_field (nth field (caddr curr_od)))
		(setq new_field (ade_editonefield new_field dcl_id))
		(if new_field (progn
			(setq fields (nth 2 CURR_OD))
			(setq fields (replace-nth fields field new_field))
			(initialize_fields fields)
			(setq CURR_OD (list (car CURR_OD)(cadr CURR_OD) fields))
			)
		)
	)


	; --- support function for NEW... button

	(defun act_getnewtable ( / table)
		(setq od_list (od_updateentry CURR_OD od_list))
		(setq table (ade_getnewtablename od_tables (car Curr_EED) dcl_id))
		(if table (progn
			(setq od_list (cons table od_list))
			(initialize_od od_list)
			; if fields selected, turn on ADD button...
			(if (/= (get_tile "ID_LBOX_VARIABLES") "")
				(mode_tile "ID_BUTT_ADD" 0)
			)); end progn
		)
	)

	; --- support function for TABLE popup list

	(defun act_newtableselected ()
		(setq od_list (od_updateentry CURR_OD od_list))
		(setq CURR_OD (nth (atoi (get_tile "ID_PLIST_TABLES")) od_list))
		(initialize_fields (nth 2 CURR_OD))
	)

	; --- support function for FIELDS list box

	(defun act_fieldselected ( / items)
		(setq items (get_tile "ID_LBOX_FIELDS")) 
		(setq items (read (strcat "(" items ")")))
		(if (and items (= (nth 1 CURR_OD) "NEW"))
			(mode_tile "ID_BUTT_REMOVE" 0)
			(mode_tile "ID_BUTT_REMOVE" 1)
		)
		(if (/= (length items) 1)
			(mode_tile "ID_BUTT_EDITFIELD" 1)
			(if (= (nth 1 CURR_OD) "NEW")
				(mode_tile "ID_BUTT_EDITFIELD" 0)
			)
		)
	)	

	; --- support function for OK Button 

	; PJL - Apr 9, 1997 - updated to check if table exists
	(defun act_OK_button ( / status result)
		(setq od_list (od_updateentry CURR_OD od_list))
		(setq status (cnv_display_warning dcl_id))
		(if status 	
			(progn ; try to create the tables.
				(setq result (create_od_table od_list))
				(if (= result T)
					(progn
						(setq remove_eed (get_tile "ID_TOGG_REMOVE_EED"))
						(if (= remove_eed "0")(setq remove_eed Nil))
						(setq od_list (list remove_eed od_list))
						(done_dialog 1)
					)
					; else something went wrong creating tables
					(progn
						(setq od_list result)
						(setq CURR_OD (nth (atoi (get_tile "ID_PLIST_TABLES")) od_list))
					)
				)
			)
		)
	)
			
	
	; --------  Main control for the dialog ....


    (if (new_dialog "ADE_CONVERSION_DIALOG" dcl_id) (progn
	   (mode_tile "ID_BUTT_OK" 1)
	   (action_tile "ID_BUTT_OK" "(act_OK_button)")
	   (action_tile "ID_PLIST_APPNAME" "(act_newappnameselected)")
	   (action_tile "ID_LBOX_VARIABLES" "(act_variableselected)")
	   (action_tile "ID_PLIST_TABLES" "(act_newtableselected)")
	   (action_tile "ID_LBOX_FIELDS" "(act_fieldselected)")
	   (action_tile "ID_BUTT_NEWTABLE" "(act_getnewtable)")
	   (action_tile "ID_BUTT_ADD" "(act_addselected)")
	   (action_tile "ID_BUTT_REMOVE" "(act_removeselected)")
	   (action_tile "ID_BUTT_EDITFIELD" "(act_editselected)")
	   (action_tile "ID_BUTT_ALLVARS" "(act_SelectAll_Vars)")
	   (action_tile "ID_BUTT_NOVARS" "(act_SelectNone_Vars)")
	   (action_tile "ID_BUTT_ALLFIELDS" "(act_SelectAll_Fields)")
	   (action_tile "ID_BUTT_NOFIELDS" "(act_SelectNone_FIELDS)")
	   (initialize_eed eed_list)
	   (initialize_od od_list)
       (setq status (start_dialog))
	   ) ; end progn
	   (alert "\nNo dialog titled ADE_CONVERSION_DIALOG exists in dcl file.")
	)

	(if (= status 1) od_list Nil)

)

;------ function to control dialog to get table name

(defun ade_getnewtablename (od_tables Curr_AppName dcl_id / table)

	; support functions for dialog

	(defun get_tablename ( / index)
	    (cond 
	    	((= (get_tile "ID_RBUTT_NEWTAB") "1")  ; new table
				(setq table (get_tile "ID_EBOX_TABNAME"))
				(if (> (strlen table) 0) 
					(if (ade_odtabledefn table) (progn ; table already exists
						(alert "Table name already exists.")
						(ade_errclear)
						(mode_tile "ID_EBOX_TABNAME" 2)
						)
						(progn

						    ; PJL - April 9, 1997
							(ade_errclear)
							
							(ade_oddefinetab 
								(list 
									(cons "TableName" table)
									(cons "TableDesc" table)
								)
							)
							(if (or (= (ade_errcode) 3100) ; invalid table name
								    (> (strlen table) 31)
								)
								(progn
									(alert (strcat (ade_errmsg) " - " table))
									(ade_errclear)
									(mode_tile "ID_EBOX_TABNAME" 2)
								)
								; else - it's a valid table name
								(progn
	
									(setq table (list table "NEW"))	
									(done_dialog 1)
								)
							)
						)
					)
					; else
					(progn
						(alert "New table name not specified.")
						(mode_tile "ID_EBOX_TABNAME" 2)
					)
				) ; end if
			)
			(T ; existing table
				(setq index (get_tile "ID_PLIST_TABNAME"))
				(setq table (nth (atoi index) od_tables))
				(setq table (ade_getodinfo table))
				(done_dialog 1)
			)
		)
	)
	(defun cnv_radio1_hit ()
	    (set_tile "ID_RBUTT_EXISTINGTAB" "0")
		(mode_tile "ID_EBOX_TABNAME" 0)
		(mode_tile "ID_EBOX_TABNAME" 2)
		(mode_tile "ID_PLIST_TABNAME" 1)
	)
	(defun cnv_radio2_hit ()
	    (set_tile "ID_RBUTT_NEWTAB" "0")
		(mode_tile "ID_PLIST_TABNAME" 0)
		(mode_tile "ID_EBOX_TABNAME" 1)
		(set_tile "ID_TEXT_ERRMSG" "")
	)



	; --------  Main control for the dialog ....


    (if (new_dialog "CNV_NEWTABLE_DIALOG" dcl_id) (progn
	   (action_tile "ID_BUTT_OK" "(get_tablename)")
	   (action_tile "ID_RBUTT_NEWTAB" "(cnv_radio1_hit)")
	   (action_tile "ID_RBUTT_EXISTINGTAB" "(cnv_radio2_hit)")
	   (set_tile "ID_EBOX_TABNAME" Curr_AppName)
	   (if (not od_tables)
	   	   (mode_tile "ID_ROW_ROW2" 1)
		   ; else
		    (progn
				(start_list "ID_PLIST_TABNAME")
				(foreach tabname od_tables
					(add_list tabname)
				)
				(end_list)
			)
	   )
       (setq action (start_dialog))
	   ) ; end progn
	   (alert "\nNo dialog titled CNV_NEWTABLE_DIALOG exists in dcl file.")
	)

	(if (= action 1) table Nil)

)


;------ function to control dialog to edit a field 

(defun ade_editonefield (field dcl_id / table action appname varname)

	; support functions for dialog
	(setq field_types '("Character" 
						"Integer" 
						"Real"
						)
	)

	(defun initialize_edit_dialog ()
		(set_tile "ID_TEXT_APPNAME" (setq appname (nth 1 field)))
		(set_tile "ID_TEXT_VARNAME" (setq varname (nth 2 field)))
		(set_tile "ID_EBOX_FLDNAME" (nth 0 field))
		(set_tile "ID_EBOX_DESCRIPTION" (nth 4 field))
		(setq datatype (nth 3 field))
		(cond
			((= (strcase (substr datatype 1 1)) "C")
				(set_tile "ID_PLIST_DATATYPE" "0")
			)
			((= (strcase (substr datatype 1 1)) "I")
				(set_tile "ID_PLIST_DATATYPE" "1")
			)
			(T 
				(set_tile "ID_PLIST_DATATYPE" "2")
			)
		)

	)

	(defun Get_FieldValues ()
		(setq fldname (get_tile "ID_EBOX_FLDNAME"))
		(if (> (strlen fldname) 0) (progn
			(setq fld_list  (list
				(get_tile "ID_EBOX_FLDNAME")
				appname
				varname
				(nth 
					(atoi (get_tile "ID_PLIST_DATATYPE"))
					field_types
				)
				(get_tile "ID_EBOX_DESCRIPTION")
				)
			)
			(done_dialog 1)
			) ; end progn

			; else
			(progn
				(setq tmp_msg "Field name was not specified.")
				(set_tile "ID_TEXT_ERRMSG" tmp_msg)
				(mode_tile "ID_EBOX_FLDNAME" 2)
			)
		)
 	)



	; --------  Main control for the dialog ....


    (if (new_dialog "CNV_EDITFIELD_DIALOG" dcl_id) (progn
	   (action_tile "ID_BUTT_OK" "(Get_FieldValues)")
	   (start_list "ID_PLIST_DATATYPE")
	   (mapcar 'add_list field_types)
	   (end_list)
	   (initialize_edit_dialog)
       (setq action (start_dialog))
	   ) ; end progn
	   (alert "\nNo dialog titled CNV_EDITFIELD_DIALOG exists in dcl file.")
	)

	(if (= action 1) fld_list Nil)

)


; - Object Data List Manipulation routines...

; od_updateentry - updates a list of fields within the list of
; tables....

(defun od_updateentry (one_item od_list / tmp_list)

	(foreach item od_list
		(setq tmp_list
			(cons 
				(if (= (car one_item) (car item)) 
					one_item
					item
				)
				tmp_list
			)
		)

    ) ; end foreach

	(reverse tmp_list)
)

; - add_selectedvars : adds the list of selected variables into
; the field list...

(defun add_selectedvars (vars curr_eed fields curr_od / var_i fldlist tmplist)
	
	; get list of current EED Var names -those enclosed in lists are 
	; currently not available...
		
	(setq appname (car curr_eed))
	(setq var_list Nil)
	(foreach var (cdr curr_eed)
		(if (= (type var) 'STR)
		   (setq var_list (cons var var_list))
		)
	)
	(setq var_list (reverse var_list))

	; initialize var index
	(setq var_i 0)

	; get just field names
	(setq fldlist (nth 2 curr_od))

	; define function to get next variable name (returns Nil if no more)
	(defun get_varname ( / var_index)
		(setq var_index (nth var_i vars))
		(setq var_i (1+ var_i))
		(if var_index
			(nth var_index var_list)
			Nil
		)
	)

	; now step through each variable seeing if it is to be updated...

	(foreach field_i fields 
		(setq varname (get_varname))

		(if varname (progn   ; we have a variable to update...

			; so get old data
			(setq curr_data (nth field_i fldlist))

			; update the old data with new data
			(setq new_data 
				(list 
					(car curr_data) 
					appname 
					varname 
					(nth 3 curr_data)
					(nth 4 curr_data)
				))

			; update the field list with the new data
			(setq fldlist
				(subst 
					new_data
					(nth field_i fldlist)
					fldlist
				)  
			)
		))
	) ; end foreach

	; see if there are more eed vars to add to the list
	(while (setq varname (get_varname))
		(setq tmplist 
			(cons 
				(list 
					(validate_fieldname varname) 
					appname varname 
					(if (= (substr varname 1 1) "#") "Real" "Character") 
					varname
				) 
				tmplist
			))
	)

	; if yes, then check if this is an existing table.  If yes,
	; we need to display a message can't that isn't allowed.
	; Otherwise add the new fields to the updated field list
	(if tmplist
		(if fldlist
			(setq fldlist (append fldlist (reverse tmplist)))
			(setq fldlist (reverse tmplist))
		)
	)

	; return the updated list
	(list (car curr_od) (cadr curr_od) fldlist)

)


(defun eed_updatestatus (vars CURR_EED / cnt tmplist)
	(setq cnt 0)
	(setq tmplist (list (car CURR_EED)))
	(foreach x (cdr CURR_EED)
	
		(if (and (not (listp x))(member cnt vars))
			(setq tmplist (cons (list x) tmplist))
			(setq tmplist (cons x tmplist))
		)
		(if (not (listp x))(setq cnt (1+ cnt)))
	)
	(reverse tmplist)
)

;  reenables an EED item in the current list of EED variables.

(defun eed_restorestatus (varname CURR_EED / tmplist)
	(setq tmplist (list (car CURR_EED)))
	(foreach x (cdr CURR_EED)
		(if (and (listp x) (= (strcase (car x)) (strcase varname)))
			(setq tmplist (cons (car x) tmplist))
			(setq tmplist (cons x tmplist))
		)

	)
	(reverse tmplist)
)


; to do - fix this so it returns the correct data...

(defun ade_getodinfo (table / tableinfo colinfo) ; PJL Feb 18/97
	(setq tableinfo (ade_odtabledefn table))
	(setq columns (cdr (assoc "Columns" tableinfo)))
	(foreach col columns
		(setq colinfo 
			(cons
				(list 
					(cdr (assoc "ColName" col)) 
					""
					""
					(cdr (assoc "ColType" col)) 
					(cdr (assoc "ColDesc" col)) 
				)
				colinfo
			)
		)
	)
	(list table "OLD" (reverse colinfo))  ; PJL - Apr 9, 1997 (added reverse)
)

(defun replace-nth (alist index new_value / cnt newlist) 
	(setq cnt 0)
	(setq newlist nil)
	(foreach item alist	
		(if (= cnt index)
			(setq newlist (cons new_value newlist))
			(setq newlist (cons item newlist))
		)
		(setq cnt (1+ cnt))
	)
	(reverse newlist)
)


; Makes a field name valid

(defun validate_fieldname (aname / curr_char)

    (setq cnt 1)
	(setq newname "")
	
	; remove leading #
	(if (= (substr aname 1 1) "#")
	   (setq aname (substr aname 2))
	)

	(repeat (strlen aname)
		(setq curr_char (substr aname cnt 1))
		
		(if (= curr_char "#")
			(setq curr_char "NO")
			; else
			(if (not (wcmatch (strcase curr_char) "@,#,[-_$]"))
				(setq curr_char "_")
			)
		)
		(setq newname (strcat newname curr_char))
		(setq cnt (1+ cnt))
	)

	newname
)

;  Routine to retrieve ADE 1.0 EED


(defun get_eed (ename names_only / applist)
	(setq applist (entget ename '("*")))

	(setq applist (cdr (assoc -3 applist)))
	(if applist
		(get_alleed  applist names_only)
		Nil

	)
)

(defun eed_update (ename eed_data / applist new_eed all_newEED EED_Removed
								    ent_data appname app_eed var onevar)

	(setq ent_data (entget ename '("*")))

	(setq applist (cdr (assoc -3 ent_data)))
	(if applist
		(progn
			(setq all_newEED Nil)
			(foreach appname applist
				(setq new_eed Nil)
				(setq app_eed (cdr (assoc (car appname) eed_data)))
				(foreach var (cdr appname)
				   
					(if (or (/= (car var) 1000)
							(not (setq onevar (parse_equal (cdr var))))
							(assoc (car onevar) app_eed)
						)
						(setq new_eed (cons var new_eed))

						; else we must remove eed
						(setq EED_Removed T)
					)
				)
				(setq all_newEED 
					(cons 
						(cons (car appname) (reverse new_eed))
						all_newEED
					)
				)
			)
			(if EED_Removed
				(progn
					(setq new_entdata (append (entget ename) (list (cons -3 (reverse all_newEED)))))
					(entmod new_entdata)
					(entupd ename)
				)
			)
		)
	) ; end if
)




(defun get_alleed (applist names_only / Xdata)
	(foreach appname applist
		(setq eed_vars (get_eedvars (cdr appname) names_only))
		(if eed_vars 
			(setq xdata 
				(cons (cons (car appname) eed_vars) Xdata))
		)
	)
	xdata
)


(defun get_eedvars (appdata  names_only / varlist onevar)
	(foreach var appdata 
		(if (= (car var) 1000)
			(if (setq onevar (parse_equal (cdr var)))
				(if names_only
					(setq varlist (cons (car onevar) varlist))
					(setq varlist (cons onevar varlist))
				)
			)
		)
	)
	(reverse varlist)
)

(defun parse_equal (astring / tmp_str index)
   (setq tmp_str "" index 1)
   (while (and (<= index (strlen astring)) (/= (substr astring index 1) "="))
      (setq tmp_str (strcat tmp_str (substr astring index 1)))
      (setq index (1+ index))
   )
   (if (> index (strlen astring)) 
      Nil
      (list (strcase tmp_str) (substr astring (1+ index)))
   )
)


;; Dialog to display warning message....


(defun cnv_display_warning (dcl_id / action)

    (if (new_dialog "CNV_WARNING_MESSAGE" dcl_id) (progn
	   (action_tile "ID_BUTT_OK" "(done_dialog 1)")
	   (set_tile "ID_DIALOG_TITLE" "Conversion Note")
	   (start_list "ID_LBOX_MESSAGE")
	   (add_list "AutoCAD Map provides two commands for converting Release 1.0")
	   (add_list "EED to AutoCAD Map object data:")
	   (add_list " ")
	   (add_list "    Convert ADE 1.0 Drawings converts each AutoCAD Map Release 1.0 EED")
	   (add_list "    field into an individual object data table containing a single field.")
	   (add_list " ")
	   (add_list "    Convert EED (Advanced) converts AutoCAD Map Release 1.0 EED")
	   (add_list "    into tables with multiple fields.")
	   (add_list " ")
	   (add_list "WARNING! If you selected the Remove EED After Conversion option")
	   (add_list "of the Convert ADE 1.0 Drawing command, you will not be able to")
	   (add_list "recover the original data.")
	 
	   (end_list)
	   
       (setq action (start_dialog))
	   ) ; end progn
	   
	)

	(= action 1) 

)


;;  Temporary Routines


(defun xd_odaddrecord (ename tabname)
	(redraw ename 3)
	(if debug (progn
		(princ "\nCreating record for ") (princ tabname)
	))
	(ade_odaddrecord ename tabname)
)

(defun xd_odsetfield (ename table field value recnum)
	(if debug (progn
		(princ "\nAssigning value ")(prin1 value)
		(princ " to field ")(princ field)(princ " in table ")
		(princ table)
		)
	)
	(ade_odsetfield ename table field recnum value)
)

(defun create_od_defn (table / col columns new_table)
    (foreach col (nth 2 table)

		(setq columns (append (list (list
			(cons "ColName" (nth 0 col))
			(cons "ColDesc" (nth 4 col))
			(cons "ColType" (nth 3 col))
			(cons "DefaultVal" (if (= (nth 3 col) "Character") "_" 0.0) )
			))
			columns

		))
	)


	(setq new_table (list
	    (cons "TableName" (car table))
	    (cons "TableDesc" (car table))
		(cons "Columns" (reverse columns))
	))
)

(defun fix_type (field_type value)
	(if (not value)(setq value ""))
    (cond 
		((= field_type "Character") 
			(if (= (type value) 'Str) 
				(strip_quotes value)
				""
			)
		)
		((= field_type "Real") (atof value))
		(T (atoi value))
	)
)

(defun strip_quotes (value)
    (if (not value)(setq value ""))
	(if (> (strlen value) 0) (progn
    	(if (= (substr value 1 1) "'")
			(if (> (strlen value) 1)
		    	(setq value (substr value 2))
	    		(setq value "")
			)
    	)

    	(if (= (substr value (strlen value)) "'")
			(if (> (strlen value) 1)
	    		(setq value (substr value 1 (1- (strlen value))))
	    		(setq value "")
			)
    	)
	))
    value
)



;;------------------End of temporary routines...

(princ "\nType EEDCONVERT to start the command.")
(princ)


