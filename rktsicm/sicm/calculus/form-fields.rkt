#lang racket/base

(provide (except-out (all-defined-out) assign-operation))

(require "../rkt/fixnum.rkt"
         "../general/assert.rkt"
         "../kernel-gnrc.rkt"
         "dgutils.rkt"
         "manifold/manifold-point.rkt"
         "indexed/types.rkt"
         "vector-fields.rkt"
         "wedge.rkt"
         )
(define-values (assign-operation form-fields:assign-operations)
  (make-assign-operations 'form-fields))

;;;; 1Form fields

;;; A form-field of rank n is an operator that takes n vector fields
;;; to a real-valued function on the manifold.  A 1form field takes a
;;; single vector field.

(define (form-field? fop)
  (and (operator? fop)
       (eq? (operator-subtype fop) wedge)))

(define (1form-field? fop)
  (and (form-field? fop) (= (get-rank fop) 1)))

;;; 1form fields multiply by wedge.  
;;;   (See wedge.scm for definition of get-rank.)

(define (procedure->1form-field fp [name 'unnamed-1form-field])
  (let ((the-field (make-operator fp name wedge)))
    (declare-argument-types! the-field (list vector-field?))
    the-field))

(define (ff:zero vf) zero-manifold-function)

(define (ff:zero-like op)
  (assert (form-field? op) "ff:zero-like")
  (make-operator ff:zero
	   'ff:zero
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(assign-operation generic:zero-like ff:zero-like form-field?)


(define (ff:zero? ff)
  (assert (form-field? ff) "ff:zero?")
  (eq? (operator-procedure ff) ff:zero))

(assign-operation generic:zero? ff:zero? form-field?)


;;; A 1form is specified by a function that gives components, in a
;;; down tuple, relative to a coordinate system.

(define ((1form-field-procedure components coordinate-system) vf)
  (define (internal vf)
    (assert (vector-field? vf))
    (compose (* components
		(vector-field->components vf coordinate-system))
	     (coordinate-system '->coords)))
  (s:map/r internal vf))


(define (components->1form-field components coordinate-system
                                 [name `(1form-field ,components)])
  (procedure->1form-field
   (1form-field-procedure components coordinate-system)
   name))


;;; We can extract the components function for a form, given a
;;; coordinate system.

(define (1form-field->components form coordinate-system)
  (assert (form-field? form) "Bad form field: 1form-field->components")
  (let ((X (coordinate-system->vector-basis coordinate-system)))
    (compose (form X) (coordinate-system '->point))))


;;; It is often useful to construct a 1form field

(define (literal-1form-field name coordinate-system)
  (let ((n (coordinate-system 'dimension)))
    (let ((function-signature
	   (if (fix:= n 1) (-> Real Real) (-> (UP* Real n) Real))))
      (let ((components
	     (s:generate n 'down
			 (lambda (i)
			   (literal-function (string->symbol
					      (string-append
					       (symbol->string name)
					       "_"
					       (number->string i)))
					     function-signature)))))
	(components->1form-field components coordinate-system name)))))

;;; To get the elements of a coordinate basis for the 1-form fields

(define ((coordinate-basis-1form-field-procedure coordinate-system . i) vf)
  (define (internal vf)
    (assert (vector-field? vf)
	    "Bad vector field: coordinate-basis-1form-field")
    (vf (compose (apply component i) (coordinate-system '->coords))))
  (s:map/r internal vf))

(define (coordinate-basis-1form-field coordinate-system name . i)
  (procedure->1form-field
   (apply coordinate-basis-1form-field-procedure coordinate-system i)
   name))

#|
(define (coordinate-system->1form-basis coordinate-system)
  (s:map (lambda (chain)
	   (apply coordinate-basis-1form-field
		  coordinate-system
		  `(w ,@chain)
		  chain))
	 (coordinate-system 'access-chains)))
|#

(define (coordinate-system->1form-basis coordinate-system)
  (coordinate-system 'coordinate-basis-1form-fields))


#|
(define ((coordinate-system->1form-basis-procedure coordinate-system) vf)
  (vf (coordinate-system '->coords)))
|#

;;; Given component functions defined on manifold points and a 1-form
;;; basis, to produce the 1-form field as a linear combination.

;TODO: check this * doesn't need to be g:*
(define (basis-components->1form-field components 1form-basis)
  (procedure->1form-field
   (lambda (v)
     (* components (1form-basis v)))))

(define (1form-field->basis-components w vector-basis)
  (s:map/r w vector-basis))



;;; This is one of the two incompatible definitions of "differential".
;;; This differential is a special case of exterior derivative.
;;; The other one appears in maps.scm.

(define (function->1form-field f)
  (define (internal v)
    (assert (vector-field? v))
    (lambda (m) ((v f) m)))
  (assert (function? f))
  (procedure->1form-field
   (lambda (v) (s:map/r internal v))
   `(d ,(diffop-name f))))

(define differential-of-function function->1form-field)

#|
(install-coordinates R3-rect (up 'x 'y 'z))

(define mr ((R3-rect '->point) (up 'x0 'y0 'z0)))

(define a-1form
  (components->1form-field
   (down (literal-function 'ax (-> (UP* Real) Real))
	 (literal-function 'ay (-> (UP* Real) Real))
	 (literal-function 'az (-> (UP* Real) Real)))
   R3-rect))

(define a-vector-field
  (components->vector-field
   (up (literal-function 'vx (-> (UP* Real) Real))
       (literal-function 'vy (-> (UP* Real) Real))
       (literal-function 'vz (-> (UP* Real) Real)))
   R3-rect))

(pec ((a-1form a-vector-field) mr))
#| Result:
(+ (* (vx (up x0 y0 z0)) (ax (up x0 y0 z0)))
   (* (vy (up x0 y0 z0)) (ay (up x0 y0 z0)))
   (* (vz (up x0 y0 z0)) (az (up x0 y0 z0))))
|#

(pec ((1form-field->components a-1form R3-rect) (up 'x0 'y0 'z0)))
#| Result:
(down (ax (up x0 y0 z0)) (ay (up x0 y0 z0)) (az (up x0 y0 z0)))
|#

(install-coordinates R3-cyl (up 'r 'theta 'zeta))

(pec ((1form-field->components a-1form R3-cyl) (up 'r0 'theta0 'z0)))
#| Result:
(down
 (+ (* (sin theta0) (ay (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))
    (* (cos theta0) (ax (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0))))
 (+ (* -1 r0 (sin theta0) (ax (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))
    (* r0 (cos theta0) (ay (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0))))
 (az (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))
|#

|#

#|
(define mr ((R3-rect '->point) (up 'x0 'y0 'z0)))
(define mp ((R3-cyl '->point) (up 'r0 'theta0 'z0)))

((dx d/dx) mr)
;Value 1

((dx d/dx) mp)
;Value 1

(pec ((1form-field->components dr R3-rect) (up 'x0 'y0 'z0)))
#| Result:
(down (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
      (/ y0 (sqrt (+ (expt x0 2) (expt y0 2))))
      0)
|#

(pec ((1form-field->components dtheta R3-rect) (up 'x0 'y0 'z0)))
#| Result:
(down (/ (* -1 y0) (+ (expt x0 2) (expt y0 2)))
      (/ x0 (+ (expt x0 2) (expt y0 2)))
      0)
|#

(pec (((+ (* 'w_0 dr) (* 'w_1 dtheta)) (+ (* 'V^0 d/dx) (* 'V^1 d/dy))) mp))
#| Result:
(+ (* V^0 w_0 (cos theta0))
   (* V^1 w_0 (sin theta0))
   (/ (* -1 V^0 w_1 (sin theta0)) r0)
   (/ (* V^1 w_1 (cos theta0)) r0))
|#

(pec
 (((components->1form-field (1form-field->components
			     (+ (* 'w_0 dr) (* 'w_1 dtheta))
			     R3-rect)
			    R3-rect)
   (+ (* 'V^0 d/dx) (* 'V^1 d/dy)))
  mp))
#| Result:
(+ (* V^0 w_0 (cos theta0))
   (* V^1 w_0 (sin theta0))
   (/ (* -1 V^0 w_1 (sin theta0)) r0)
   (/ (* V^1 w_1 (cos theta0)) r0))
|#



(define counter-clockwise (- (* x d/dy) (* y d/dx)))

(define outward (+ (* x d/dx) (* y d/dy)))


(pec ((dx counter-clockwise) mr))
#| Result:
(* -1 y0)
|#

(pec ((dx outward) mr))
#| Result:
x0
|#

(pec ((dr counter-clockwise) mp))
#| Result:
0
|#

(pec ((dr outward) mp))
#| Result:
r0
|#

(pec ((dr outward) mr))
#| Result:
(sqrt (+ (expt x0 2) (expt y0 2)))
|#

(pec (((* x dy) (+ (* 'u d/dx) (* 'v d/dy))) mr))
#| Result:
(* v x0)
|#

(pec ((dr d/dr) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))))
#| Result:
1
|#

(pec ((dr d/dtheta) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))))
#| Result:
0
|#

(pec ((dtheta d/dr) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))))
#| Result:
0
|#

(pec ((dtheta d/dtheta) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))))
#| Result:
1
|#
|#
