;;;Elise Moss
;;;www.mossdesigns.com
;;;December 2006

;;;this routine changes all MTEXT to bylayer regardless of what color was set inside of the mtext editor


(defun c:cmtxt ()
   ; first select all mtext
   (setq mtext-collection (ssget "x" (list (cons 0 "MTEXT")))) 
   ; now we have to loop thru and change each object
   ;;how many objects are there?
   (setq collection-no (sslength mtext-collection))
   ;;initialize counter
   (setq index 0)
   ;;;start the loop
   (while (< index collection-no)
     ; get the entity name
     (setq mtext-obj (ssname mtext-collection index))
     ; get the entity list
     (setq mtext-data (entget mtext-obj))
     ;;the text color is indicated in the text value ex: \\C5;
     (setq text-value (cdr(assoc 1 mtext-data)))
     ;;search the text for color code
     (setq flag (vl-string-search "\\C" text-value ))
     ; if flag is nil, the mtext is already set to bylayer
     (if (= flag nil)
       (setq index (+ index 1))
     ); end if
     ;;if flag isn't nil then we need to strip the color code out and set to bylayer
    (if (/= flag nil)
      (progn
      ;;locate the start point of the text
      (setq text-start (vl-string-search ";" text-value ))
      (setq new-text-value (substr text-value (+ text-start 2)))
      ;;need to delete the } at the end of the string
      (setq end-place (vl-string-search "}" new-text-value ))
      (if (/= end-place nil)
	(progn
          (setq text-len (strlen new-text-value))
          (setq ntext-value (substr new-text-value 1 (- end-place 1)))
          ;;we have now stripped the color coding out of the mtext, time to redefine the entity data (subst newitem olditem lst)
	)
	(setq ntext-value new-text-value)
      )
      (setq mtext-data (subst (cons 1 ntext-value) (assoc 1 mtext-data)  mtext-data))
      ;then update the entity
      (entmod mtext-data)
      (entupd mtext-obj)
      ; roll the counter
      (print index)
      (setq index (+ index 1))
      );; end progn
    );end if
   )			     ;; end while



);end defun