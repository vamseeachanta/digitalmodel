(defun tke_dien(/ lbc f1 f2 list_tke list_new p temp1 l i j  x y)
;----------------
(defun lbc(p1 len color) 
 (command "_.LINE" (list (+ (* tlv len)(car p1)) (cadr p1)) (list (+ (car p1)(* tlv len))(- (cadr p1) (* tlv 8)))"")
 (command "_.CHPROP" (entlast) "" "C" color "")
)
 
(if (and(setq f1(open "c:\\acad\\tkxd\\dien\\tke_dien.txt" "r"))
        (setq f2(open "c:\\acad\\tkxd\\dien\\dsdien.txt" "r")) )(progn
  (setq list_tke NIL list_new NIL)
  (while (setq p(read-line f1)) (setq list_tke(cons p list_tke)) ) (close f1)
  (setq list_tke(reverse list_tke))
  (while list_tke
   (setq temp1(atoi(car list_tke)) list_tke(cdr list_tke) l(length list_tke) i 0 j 1)
   (while (< i l)
    (if (= temp1  (atoi(nth i list_tke))) 
     (setq j(1+ j) l(1- l) list_tke(del_item list_tke i) i(1- i) )   
    )
    (setq i(1+ i))
   )
   (if (/= temp1 0)(setq list_new(cons (strcat (itoa j) "    "(itoa temp1)) list_new)))
 );End while  
 (while (setq p(read-line f2)) (setq list_tke(cons p list_tke)) ) (close f2)
 (setq list_tke(reverse list_tke)
       p (getpoint"\nInsert point:"))
;--------Ve bang---------
  (if p (progn
   (setq i 0 l(length list_new) list_new(reverse list_new))
   (command "_.INSERT" "c:\\acad\\tkxd\\dien\\td_btkd" p tlv "" "")   
   (repeat l
    (setq x(car p) y(cadr p)
           temp1 (atoi(substr (nth i list_new) 4)) ) 
    (lbc p 0 "3")(lbc p 20 "9")(lbc p 100 "9")
    (lbc p 120 "9")(lbc p 140 "3") 
    (command "_.INSERT" (strcat "c:\\acad\\tkxd\\dien\\d_$" (itoa temp1)) 
       (list (+ x (* 10 tlv))(- y (* 4 tlv))) tlv "" "" "$")  
    (wtxt (cut (substr (nth i list_new)1 3) )   (list (+ x (* tlv 130))(- y(* tlv 4)))"MC" (* tlv 2.6) 0 1 "2") 
    (wtxt (cut (substr (nth (- temp1 1) list_tke)6 11) )   (list (+ x (* tlv 110))(- y(* tlv 4)))"MC" (* tlv 2.6) 0 1 "2") 
    (wtxt (cut (substr (nth (- temp1 1) list_tke)17))   (list (+ x (* tlv 25))(- y(* tlv 4)))"ML" (* tlv 2.6) 0 0.8 "2") 
    (setq p(list x (- y(* tlv 8))) i(1+ i))
   );End repeat 
   (setq list_tke NIL list_new NIL)(GC)
  ))
;-------Ve xong bang-----
 )(alert"I'm sorry. File not found! Please call to HUAC.") )
)