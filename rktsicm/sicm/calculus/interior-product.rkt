#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require "../general/assert.rkt"
         "dgutils.rkt"
         "form-fields.rkt"
         "vector-fields.rkt"
         "wedge.rkt"
         )

(define (interior-product X)
  (assert (vector-field? X) "X not a vector field: interior-product")  
  (define (ix alpha)
    (assert (form-field? alpha) "alpha not a form field: interior-product")
    (let ((p (get-rank alpha)))
      (assert (> p 0) "Rank of form not greater than zero: interior-product")
      (define (the-product . args)
	(assert (= (length args) (- p 1))
		"Wrong number of arguments to interior product")
	(apply alpha (cons X args)))
      (procedure->nform-field the-product
			      (- p 1)
			      `((interior-product ,(diffop-name X))
				,(diffop-name alpha)))))
  ix)

#|
;;; Claim L_x omega = i_x d omega + d i_x omega (Cartan Homotopy Formula)

(install-coordinates R3-rect (up 'x 'y 'z))

(define R3-rect-point ((R3-rect '->point) (up 'x0 'y0 'z0)))

(define X (literal-vector-field 'X R3-rect))
(define Y (literal-vector-field 'Y R3-rect))
(define Z (literal-vector-field 'Z R3-rect))
(define W (literal-vector-field 'W R3-rect))

(define alpha
  (compose (literal-function 'alpha (-> (UP Real Real Real) Real))
	   (R3-rect '->coords)))
(define beta
  (compose (literal-function 'beta (-> (UP Real Real Real) Real))
	   (R3-rect '->coords)))
(define gamma
  (compose (literal-function 'gamma (-> (UP Real Real Real) Real))
	   (R3-rect '->coords)))

(define omega
  (+ (* alpha (wedge dx dy))
     (* beta (wedge dy dz))
     (* gamma (wedge dz dx))))

(define ((L1 X) omega)
  (+ ((interior-product X) (d omega))
     (d ((interior-product X) omega))))


(pec ((- (((Lie-derivative X) omega) Y Z)
	 (((L1 X) omega) Y Z))
      ((R3-rect '->point) (up 'x0 'y0 'z0))))
#| Result:
0
|#

(pec (let ((omega (literal-1form-field 'omega R3-rect)))
       ((- (((Lie-derivative X) omega) Y)
	   (((L1 X) omega) Y))
	((R3-rect '->point) (up 'x0 'y0 'z0)))))
#| Result:
0
|#

(pec (let ((omega (* alpha (wedge dx dy dz))))
       ((- (((Lie-derivative X) omega) Y Z W)
	   (((L1 X) omega) Y Z W))
	((R3-rect '->point) (up 'x0 'y0 'z0)))))
#| Result:
0
|#

|#
