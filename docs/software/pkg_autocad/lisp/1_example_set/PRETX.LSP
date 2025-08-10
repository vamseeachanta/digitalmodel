;PRETX - Prefix all text with a string
;
;Jeffery P Sanders
;
(defun C:PRETX (/ sset len cnt ed en xscmde)
   (setq xscmde (getvar "cmdecho"))                  ;get echo setvar
   (setvar "cmdecho" 0)                              ;turn echo off
   (setq str (getstring T "\n \nPrefix:  "))         ;get prefix string
   (setq cnt 0 sset (ssget))                         ;select objects
   (princ "\n \n.....Working......")                 ;clear command line
   (setq len(sslength sset))                         ;get length of set
   (while (> len 0)                                  ;while length > 0
    (setq en (ssname sset 0))                        ;get entity name
    (setq ed(entget en))                             ;get entity data
    (if (= (cdr (assoc 0 ed)) "TEXT")                ;if entity is text
     (progn                                          ;
      (setq ed                                       ;
       (subst (cons 1 (strcat str (cdr(assoc 1 ed))));add prefix to text
        (assoc 1 ed)                                 ;
        ed                                           ; 
       )                                             ;
      )                                              ;
      (entmod ed)                                    ;update database
      (setq cnt(+ cnt 1))                            ;count changes
     )                                               ;
    )                                                ;
    (setq sset (ssdel en sset))                      ;delete from set
    (setq len(- len 1))                              ;decrease length of set
   )                                                 ;
   (setvar "cmdecho" xscmde)                         ;reset echo to previous
   (princ "\n ")                                     ;
   (princ "\nPrefixed ")                             ;
   (princ cnt)                                       ;print number of changes
   (princ " Lines Of Text With ")(princ str)         ;
   (princ "\n ")                                     ;
   (princ)                                           ;
)                                                    ; end

