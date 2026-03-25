;(setvar "MENUECHO" 1)
      (setq  ll_a (if ll_a ll_a nil)
             ll_e (if ll_e  ll_e nil) 
             ll_e2 (if ll_e2  ll_e2 nil)
             ll_F (if ll_F  ll_F nil)
             ll_t (if ll_t  ll_t nil)
             ll_p0 (if ll_p0  ll_p0 nil)
             ll_ola (if ll_ola  ll_ola nil)
             ll_olo (if ll_olo  ll_olo nil)
             ll_feet (if ll_feet  ll_feet nil)
             ll_n (if ll_n  ll_n nil)
             ll_olo2 (if ll_olo2  ll_olo2 nil)
             ll_npole (if ll_npole  ll_npole nil)
             ll_set (if ll_set  ll_set nil)
             ll_magv (if ll_magv  ll_magv nil)
        )
		
(defun c:magvar()
  (setq magv1 
     (getangle 
	    (strcat "Enter mag variation (- for east) < " (nangtos ll_magv) "> :")    
     )
  )
  (if magv1 (setq ll_magv magv1))
  (PRIN1)
)  
(if ll_clean (ll_clean);do it
;else define ll_clean for later use
    ;put globals in front of clean if projection not yet setup
      (defun LL_clean( / i item)
        (setq i 0)
        (while (not (equal (setq item (nth i atomlist)) 'LL_CLEAN ))
          (if (=  (type (eval item )) 'FILE )
             (close (eval item))
          
          )
          (setq i (1+ i))
        )
        (setq atomlist (member 'LL_CLEAN atomlist))
        (prin1)
      )
);end of if

;references to equation numbers refer to the guide publication,
;"Map Projections Used by the USGS" pp250-252

;func ll-lamb_setup accepts a list of
; 1) standard parallel S. lat in radians (stlas)
; 2) standard parallel N. lat in radians (stlan) (may be same as 3 for single std par)
; 3) origin latitude in radians (LL_OLA)
; 4) origin longitude in radians (LL_OLO)
;
; as (list stlas stlan LL_OLA LL_OLO)
;

(defun ll-lamb_setup (stlas stlan LL_OLA LL_OLO);  / m1 m2 t1 t1 t0)
;sets up inviolate globals ll_a ll_e ll_e2 ll_F ll_t ll_p0  ll_ola ll_olo for map transformations
; for a single projection,called by function set_projection
;set up clarke 1866 constants
  (setq ll_feet T)
  (setq ll_a 6378206.4 ;meters earth radius
        ll_e 0.0822719 ;excentricity
        ll_e2 (expt ll_e 2.0) ;e squared
  )

  (if ll_feet (setq ll_a (* ll_a 3.2808333333))) ;survey feet in a meter

;setup constants for reference projection selected values
;from equation 12-15
  (setq m1 (/ (cos stlas) (sqrt (- 1.0 (* ll_e2 (expt (sin stlas) 2.0)))))
        m2 (/ (cos stlan) (sqrt (- 1.0 (* ll_e2 (expt (sin stlan) 2.0)))))
       ;from equation 13-9, see function tval
        t1 (tval stlas)
        t2 (tval stlan)
        t0 (tval LL_OLA)
       ;from equation 13-8  
        ll_n (/ 
            (log (/ m1 m2))
            (log (/ t1 t2)) ;this is natural log (base e)
          )
       ;from equation 13-10
        ll_F (/ m1 (* ll_n (expt t1 ll_n)))
       ;from equation 13-7a
        ll_p0 (* ll_a ll_F (expt t0 ll_n))
  )
);end of defun ll-lamb_setup


;begin comps specific to point in transformation from lat long to lambert
;conformal conic coords
;expects 
;1 longitude in radians
;2 latitude in radians
;
;assumes ll-lamb_setup has been run, requires functions tan and tval
; returns x,y,k where x and y are easting and northing in meters and 
;          k is the scale factor along the parallel
; through the subject point (list x y k)
; assumes clarke 1866 ellipsoid:

(defun ll-lamb	(plo pla / p theta x y m k)
    ;from equation 13-9
  (setq   ll_t (tval pla)
    ;compute negative longitude if greater than 180d
          ll_olo2 (if (> ll_olo pi) (- ll_olo (* 2.0 pi) ) ll_olo)
    ;from equation 13-7
         p (* ll_a ll_F (expt ll_t ll_n))
    ;from equation 12-4
         theta (* ll_n (- plo ll_olo2))
    ;from equation 12-1 and 12-2
         x (* p (sin theta))
    
         y (- ll_p0 (* p (cos theta)))
    ;from equations 12-15 and 12-16
         m (/ (cos pla) (expt 
                           (- 1.0 (* ll_e2 (expt (sin pla) 2.0)))
                           0.5
                        )
           )

         k (/ (* p ll_n) (* ll_a m))
  )
  (list x y k);values returned in list, x,y coords and k, local e-w scale
);end of defun ll-lamb

         

        
(defun tan(x) ;the standard tangent function
  (/ (sin x) (cos x))
)


;tval computes various t values ,used by ll-lamb and ll-lamb_setup
;implements equation 13-9
(defun tval(lat)
       
           (/ (tan (- (/ pi 4.0) (/ lat 2.0)))
              
              (expt
                (/ (- 1.0 
                     
                     (* ll_e (sin lat))
                   )
                   (+ 1.0 (* ll_e (sin lat)))
                )
                (/ ll_e 2.0)
              )

           )
)



;degrees to radians

(defun torad(x)
  (/ x (/ 360 (* 2.0 pi)))
)

 
;begin comps specific to point in transformation from lambert
;conformal conic coords to lat long in radians
;expects 
;1 x in meters
;2 y in meters
;
;returns (list longitude in radians, latitude in radians) 
;assumes ll-lamb_setup has been run, requires functions tan 

(defun lamb-ll(xy / x y x2 y2 p02 sgn theta phi lamda p) ;lamda used because lambda is A-lisp func

  (setq x (nth 0 xy) y (nth 1 xy))
;from equation 12-10
  (setq p (expt 
            (+ (expt x 2.0)
              (expt 
                 (- ll_p0
                    y
                 )
                 2.0
              )
            )   
            0.5 ; to the 1/2 power
          )
;negate x,y,ll_p0 if necessary:
         sgn (sgnset ll_n)
         x2 (* x sgn)
         y2 (* y sgn)
         p02 (* ll_p0 sgn)

;use two operand form of atan to derive theta
;from equation 12-11
        theta (atan
                  x2
                  (- p02 y2)
              )

;lamda is the longitude, neg values are west
;using equation 12-9
         lamda (+ (/ theta ll_n)
                   ll_olo2
                )
;from equation 13-11				
         ll_t (expt 
               (/ p
                  (* ll_a ll_F)
               )

               (/ 1 ll_n)
           )
          
          
  ) ;end of setq

;use iteratation to find phi (latitude)

;trial value:
  (setq phi (- (/ pi 2.0) (* 2.0 (atan ll_t)))
  )
;then iterating thru equation 7-9
  (while (> (abs (- phi (setq phi (nextphi phi)))) 1e-8) ;closer than .01"
  )
  ;return
  (list phi lamda) ;returns latitude,longitude in radians

)
  
;returns real number 1.0 with sign of x
(defun sgnset(x)
  (if (< x 0) -1.0 1.0)
)

;returns dec degrees given radians
(defun todeg(x)
  (* x (/ 360 (* 2.0 pi)))
)

;nextphi used in iterative computation of phi in lamb-ll
;used to implement equation 7-9
(defun nextphi(phi)
   (- (/ pi 2.0) 
     (* 2.0
       (atan
         (* ll_t 
           (expt
              (/ 
                (- 1.0 
                  (* ll_e (sin phi))
                )
                (+ 1.0
                   (* ll_e (sin phi))
                )
              )
              (/  ll_e   2.0)
           )
         )
       );end of atan
     )
   )
);end of defun nextphi
  

(defun c:set_project( / stlas stlan magv1)
;set project requests latitude for two standard parallels
;                     lat and long for origin point
; passes these as stlas (south) stlan (north) LL_OLO and LL_OLA
;  to function ll-lamb_setup which sets up global constants for the desired
;  projection
;  saves projection parameters in userr1-4 respectively

  (setvar "aunits" 1) ;use dms
  (setvar "auprec" 5) ;express .1 seconds
;  (initget 1)
  (setq stlas (getangle (strcat "\nEnter Lat. of Southern standard parallel <"
                                (angtos (getvar "userr1"))
                                ">: "
                        )
              )
  )
  (if (null stlas) (setq stlas (getvar "userr1"))
)
;  (initget 1)
  (setq stlan (getangle (strcat "\nEnter Lat. of Northern standard parallel <"
                                (angtos (getvar "userr2"))
                                ">: "
                        )
              )
  )
  (if (null stlan) (setq stlan (getvar "userr2"))
)
;  (initget 1)
  (setq LL_OLO   (getangle (strcat "\nEnter Long. of origin <"
                                (long_string (getvar "userr3"))
                                ">: "
                        )
              )
  )
  (if (numberp ll_olo)(setq ll_olo (* -1.0 ll_olo)))
    (setq ll_olo2 (if (> ll_olo pi) (- ll_olo (* 2.0 pi) ) ll_olo))
   (if (numberp ll_olo) (SETQ LL_OLO (REM LL_OLO (* 2.0 PI))))
  (if (> LL_OLO pi) (setq LL_OLO (-  LL_OLO (* 2.0 pi))))
  (if (null LL_OLO) (setq LL_OLO (getvar "userr3"))
)
;  (initget 1)
  (setq LL_OLA (getangle (strcat "\nEnter Lat. of origin <"
                                (angtos (getvar "userr4"))
                                ">: "
                        )
              )
  )
  (if (null LL_OLA) (setq LL_OLA (getvar "userr4"))
)
  (ll-lamb_setup stlas stlan LL_OLA LL_OLO)
;and set up drawing resident variables to represent projection
  (setvar "userr1" stlas)
  (setvar "userr2" stlan)
  (setvar "userr3" LL_OLO)
  (setvar "userr4" LL_OLA)
  (setq ll_set T);truth value shows projection setup is done (global)
  (setq ll_npole (ll-lamb 0 (/ pi 2.0))); ll_npole will be used to
                                               ;compute true az
  (setq ll_npole (reverse (cdr (reverse ll_npole))));drop scale factor
  (setq ll_magv 0 magv1 nil)
;  (newmag)
  (prompt "\nSheet parameters set")
  (prin1)

)

;(defun long_string (r / sgn);parameter is real value in radians,returns longitude
;;east or West in current angtos form
;;  (print r)
;;  (setq sgn (if (= 0.0  r) 0 (/ r (abs r))))
;  (setq r (* 1.0 (rem r (* 2.0 pi))))
;;  (print r)
;  (if (or (< pi r) (< r 0))
;      (strcat (angtos (- (* 2.0 pi) r)) " W.")
;      (strcat (angtos r) " E.")
;  )
;)

(defun long_string (r / sgn);parameter is real value in radians,returns longitude
;  (print r)
  (if (or (< 0 r ) (< r (* -1.0 pi)))
    (progn (setq r (- (* 2.0 pi) r)) (setq sgn " E."))
    (setq sgn " W.")
  )
  (setq r (* 1.0 (rem r (* 2.0 pi))))
; (print r)
  (if (or (< pi r) (< r 0))
      (strcat (angtos (- (* 2.0 pi) r)) sgn)
      (strcat (angtos r) sgn)
  )
)

;function llpt returns x,y position to caller as list when latitude and longitude
;are supplied

(defun llpt( / la lo)
  (setq la (getangle "\nLatitude  : "))
  (setq lo (getangle "\nLongitude : "))
  (setq lo (* -1.0 lo));negate longitude, user inputs pos for west, neg for east
  
  (ll-lamb lo la)
)
;command function llpoint requests a latitude/longitude, places a point at
;the resulting lambert coords

(defun c:llpoint( / pxy lat long)
  (setvar "AUNITS" 1) ;USE DMS
  (initget 1)
  (setq lat (getangle (strcat "\nLatitude ie. "
                                (angtos 0.22222) ;in current angle form
                                " : "
                       )
             )
  )
  (initget 1)
  (setq long (getangle "\nLongitude  : "))
  (setq long (* -1.0 long));negate longitude, user inputs pos for west, neg for east
  (if (> long pi) (setq long (-  long (* 2.0 pi))))
  (setq pxy (ll-lamb long lat ))
  (command "point" pxy)
  (prin1)
)
;command function llrept prints the lat and long of a selected point
;in the lambert projection
;returns nil
(defun c:llrept( / h lo min)

  (setq h (lamb-ll (getpoint "\nPoint for lat-long report : ")) ) 
;  (if (< ( nth 1 h) 0)
;    (setq lo (- (* 2.0 pi) (nth 1 h)) min "-")
;    (setq lo (nth 1 h) min "")
;  )
  (print (strcat "Latitude = " (angtos (nth 0 h))))
  (print (strcat "Longitude = "  (long_string (nth 1 h))))
;  (print (angtos (nth 1 h)))
  (prin1)
)


;command aptblok places a block apt at the specified lat,long,with attributes
;lat,long,description

(defun c:aptblok( / lat long pxy facname)
  (setvar "AUNITS" 1) ;USE DMS
  (initget 1)
  (setq lat (getangle (strcat "\nLatitude ie. "
                                (angtos 0.22222) ;in current angle form
                                " : "
                       )
             )
  )
  (initget 1)
  (setq long (getangle "\nLongitude  : "))
    (setq long (* -1.0 long));negate longitude, user inputs pos for west, neg for east

  (if (> long pi) (setq long (-  long (* 2.0 pi))))
  (setq pxy (ll-lamb long lat ))
  (initget 1)
  (setq facname (getstring "\nEnter facility name : " T))
  (command "insert" "apt" pxy 1 1 0 (angtos lat)(long_string long) facname)
  (prin1)
)
      

;function lamb_dist returns the approximate great  circle dist from p1 to p2
;uses the scale factors for p1,p2 and midpoint (approx) weighting the
;midpoint scale factor x4, and each endpoint scale x1

(defun lamb_dist(p1 p2 / dist lscl mp mplamb llm lls lle splamb eplamb s)
  (setq p1 (list (nth 0 p1)(nth 1 p1)))
  (setq p2 (list (nth 0 p2)(nth 1 p2)))
  (setq dist 
     (distance ;planar distance, eliminate zval if any
        p1 p2
     )
   )
   (setq mp 
          (list (/ (+ (car p1)(car p2)) 2.0) ;mean of x
                (/ (+ (nth 1 p1)(nth 1 p2)) 2.0);mean of y
          )
   )
   (setq llm (lamb-ll mp)
         lls (lamb-ll p1)
         lle (lamb-ll p2)
   )
   (setq mplamb (ll-lamb (nth 1 llm)(nth 0 llm)))
   (setq splamb (ll-lamb (nth 1 lls)(nth 0 lls)))
   (setq eplamb (ll-lamb (nth 1 lle)(nth 0 lle)))
   (setq s (/ (+ (nth 2 splamb)(* 4.0 (nth 2 mplamb))(nth 2 eplamb)) 6.0)); apply 1-4-1 scale rule

   (/ dist s) ;return distance * scale by 1-4-1 rule
)
;command function forward allows the selection of two points, returns the 
;distance and the forward true azimuth first toward second
  
(defun c:forward( /   p1 p2 d ll1 ll2) ;az
  (setq p1 (getpoint "\nfirst point : ")
        p2 (getpoint "\nsecond point : ")
  )
  (setq 
        d   (lamb_dist p1 p2 )
        ll1 (lamb-ll p1)
        ll2 (lamb-ll p2)
         
       az   (gcaz ll1 ll2)

  )
  (if (< (nth 1 ll2)(nth 1 ll1));if p2 is west of p1
    (setq az (- (* pi 2) az))
  )

  (print (strcat (rtos d) " feet"))
  (print (strcat (rtos (/ d 5280.0)) " statute miles"))
  (print (strcat (rtos (/ d 6076.115)) " nm. "))
  (print (strcat (angtos az) " forward azimuth (true)"))
  (print (strcat (angtos (+ az ll_magv)) "forward azimuth  (magnetic)"))
  (prin1)
)


(defun ll-xyz(lamda phi rad / r1 x y z );lamda is longitude, phi is lat
  (setq r1 (* rad (cos lamda))
        z  (* rad (sin lamda))
        x  (* r1 (sin phi))
        y  (* r1 (cos phi))
  )
  ;return
  (list x y z)
)

;xyzdist returns 3d dist on a straight line
;between two 3dpoints, superseded by dist in release 10

(defun xyzdist(xyz1 xyz2 / xyzd) ;two 3d points
  (setq xyzd (mapcar '- xyz1 xyz2)) ;makes xyzd a 3dpoint, the difference between two points
  (sqrt
    (+ (expt (nth 2 xyzd) 2.0) ;z squared
       (expt (sqrt
                (+
                  (expt (nth 0 xyzd) 2.0)
                  (expt (nth 1 xyzd) 2.0)
                )            
              )
              2.0
        )
      )
    )
)          
 


;function gcaz takes two ll points, returns
; the forward azimuth in radians of the sphere
; where o is sphere's origin (0,0,0)
; ll1 is occupied pt (c)
; ll2 is sighted pt  (b)
; a is xyz of north pole
; cs2 is point on o c where ocb is right angle
; cs is point oc where oca is right angle
; the angle included by the parallels of lines cs2 to b and cs to a
; passing thru the origin has the same measure as the
; the angle of intersection of the great circles thru c and b and thru a and c
; at c
; 
;great circle, ll1 toward ll2

(defun gcaz(ll1 ll2 / a b c coa cob cosa cosb cs cs2 ang)
  (setq c (ll-xyz (nth 0 ll1) (nth 1 ll1) 1)
        b (ll-xyz (nth 0 ll2) (nth 1 ll2) 1)
        a (ll-xyz (/ pi  2.0) 1 1)
        coa (gcird c a)
        cob (gcird c b)
        cosa (cos coa)
        cosb (cos cob)
        cs (mapcar '(lambda (x) (* cosa x)) c) ;proportion c by cosine of < a  o c
        cs2 (mapcar '(lambda (x) (* cosb x)) c);same for < b o c
        ang (acos (3dang cs2 b cs a))
  )
  
)

;function gcird computes the included angle between the sphere's radii thru any two
;lat/long positions in radians  . this is equivalent to the great circle dist
; between the two positions in terms of the radius of the sphere.


(defun gcird(p1 p2 / hd pr ang)
  (setq 
        
        hd 
           (/
             (xyzdist p1 p2)
             2.0
           )
        pr
          (sqrt
            (-  1 ;       for acutal dist in feet use (expt sprad 2.0)
               (expt hd 2.0)
            )
          )
        ang
          (* 2.0 (atan hd pr))
  )
  ang
)



;3dang returns the angle included between two 3 d lines
;as the angle which would be formed by their parallels passing
;thru the origin, used for computing the great circle forward azimuths

(defun 3dang( s1 e1 s2 e2 / orig ol1 ol2 d1 d2 m ccos)
  (setq ;l1   (entget (car (entsel "\npick first line")))
        ;l2   (entget (car (entsel "\npick second line")))
        orig (list 0 0 0)
        ol1  (mapcar '- s1 e1);reduce lines to dx,dy,dz points
        ol2  (mapcar '- s2 e2); same
        d1   (xyzdist orig ol1)
        d2   (xyzdist orig ol2)
        m    (apply '+ (mapcar '* ol1 ol2));makes m =  (x1*x2 + y1*y2 + z1*z2)
        ccos (/ m (* d1 d2))
        

  )
)
;returns the inverse cosine of x
(defun acos(x / qx)
    (if (< (abs (- x 1.0)) 1e-13)(setq x 1.0))
	(if (< (abs (- x -1.0)) 1e-13)(setq x -1.0))
    (if (= x 1.0)
         0
         (- (/ pi 2.0)  
           (atan x (sqrt (+ (* (* -1.0 x) x) 1.0)))
         )
    )
)
;(defun newmag()
;  (setq magv1 (getangle (strcat "Enter mag variation (- for east) < " (nangtos ll_magv) "> :")    )  )+
;  (if magv1 (setq ll_magv magv1))
;)

(defun nangtos(x) ;returns a string, the signed angle (0-180) of x
  (if (> x pi)(strcat "-" (angtos (- (* 2.0 pi) x )))(angtos x))
)
(if (null ll_set) (c:set_project))
