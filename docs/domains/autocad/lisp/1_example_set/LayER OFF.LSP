
(Defun C:LO ()

  (init_bonus_error 
    (list
      (list "cmdecho" 0)
      nil     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error

  (Command "_.-LAYER" "_Of" "*" "")
  (prompt "\nAll layers have been turned off.")

  (restore_old_error)

  (princ)
)
