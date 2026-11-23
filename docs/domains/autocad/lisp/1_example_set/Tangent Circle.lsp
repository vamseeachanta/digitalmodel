
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;; Title    : Tangent Circle               ;;   
                             ;; Written  : Bijoy manoharan              ;;       
                             ;; Web page : www.cadlispandtips.com       ;;
                             ;; Command  : TTT                          ;;       
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 
 (defun trapt (errmsg)
    (setvar "OSMODE" osm)
    (setq *error* temperr)
(princ "\n           @ @  \n            O \n Oops!!! Invalid Selection")
    (princ)
   ) ;defun



(defun c:ttt(/ osm)

         (setq temperr *error*)
         (setq *error* trapt)
         (setq osm (getvar "OSMODE"))
         (setvar "OSMODE" 256)

(command "_circle" "_3p" pause pause pause)

(setvar "OSMODE" osm)
(princ "\n.     \n. \n. Circle Placed")

(princ)
)

(princ "\nTangent Circle Lisp Command : TTT")
(princ)