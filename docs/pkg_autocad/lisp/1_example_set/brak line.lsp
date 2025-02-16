;;----------------=={ Add LWPolyline Vertex }==---------------;;
;;                                                            ;;
;;  Adds a new vertex to an LWPolyline at a point specified   ;;
;;  by the user; compatible with LWPolylines at any           ;;
;;  orientation, with varying width and arc segments.         ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    17-12-2012                            ;;
;;                                                            ;;
;;  First release.                                            ;;
;;------------------------------------------------------------;;

(defun c:apv ( / a b e h l n p r w x z )
    (while
        (progn (setq p (getpoint "\nPick Point for New Vertex: "))
            (cond
                (   (null p) nil)
                (   (null (setq e (nentselp p)))
                    (princ "\nPoint does not lie on an LWPolyline.")
                )
                (   (= 4 (length e))
                    (princ "\nObject is Nested.")
                )
                (   (/= "LWPOLYLINE" (cdr (assoc 0 (entget (setq e (car e))))))
                    (princ "\nObject is not an LWPolyline.")
                )
            )
        )
    )
    (if (and p e
            (setq p (vlax-curve-getclosestpointto e (trans p 1 0))
                  n (vlax-curve-getparamatpoint e p)
            )
        )
        (if (not (equal n (fix n) 1e-8))
            (progn
                (setq e (entget e)
                      h (reverse (member (assoc 39 e) (reverse e)))
                      l (LM:LWVertices e)
                      z (assoc 210 e)
                )
                (repeat (fix n)
                    (setq a (cons (car l) a)
                          l (cdr l)
                    )
                )
                (setq x (car l)
                      r (- n (fix n))
                      w (cdr (assoc 40 x))
                      w (+ w (* r (- (cdr (assoc 41 x)) w)))
                      b (atan (cdr (assoc 42 x)))
                )
                (entmod
                    (append h
                        (apply 'append (reverse a))
                        (list
                            (assoc 10 x)
                            (assoc 40 x)
                            (cons  41 w)
                            (cons  42 (tan (* r b)))
                        )
                        (list
                            (cons  10 (trans p 0 (cdr z)))
                            (cons  40 w)
                            (assoc 41 x)
                            (cons  42 (tan (* (- 1.0 r) b)))
                        )
                        (apply 'append (cdr l))
                        (list z)
                    )
                )
            )
        )
    )
    (princ)
)

;; Tangent  -  Lee Mac
;; Args: x - real

(defun tan ( x )
    (if (not (equal 0.0 (cos x) 1e-10))
        (/ (sin x) (cos x))
    )
)

;; LW Vertices  -  Lee Mac
;; Returns a list of lists in which each sublist describes
;; the position, starting width, ending width and bulge of the
;; vertex of a supplied LWPolyline

(defun LM:LWVertices ( e )
    (if (setq e (member (assoc 10 e) e))
        (cons
            (list
                (assoc 10 e)
                (assoc 40 e)
                (assoc 41 e)
                (assoc 42 e)
            )
            (LM:LWVertices (cdr e))
        )
    )
)

(vl-load-com) (princ)

;;------------------------------------------------------------;;
;;                        End of File                         ;;
;;------------------------------------------------------------;;