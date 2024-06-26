#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" iota generate-uninterned-symbol)
         "form-fields.rkt"
         "manifold.rkt"
         "vector-fields.rkt"
         )

;;bdk;; start original file

;;;; Testing a function for being a tensor field.

;;; To be a tensor field a function must be linear
;;; over the scalar function field in each of its
;;; arguments.  

;;; Each argument of a tensor field must be either
;;; a one-form field or a vector field.

;;; The test is done with respect to some coordinate
;;; system.  The types of the arguments are specified
;;; in a list.

(define (tensor-test T types coordsys)
  (let ((args (map (literal-field coordsys) types))
	(f ((literal-field coordsys) 'scalar)))
    (map (lambda (i)
	    (let ((thing
		   ((literal-field coordsys) (ref types i))))
	      ((- (apply T
			 (list-with-substituted-coord
			  args i
			   (+ (* f (ref args i))
			      thing)))
		  (+ (* f (apply T args))
		     (apply T
			    (list-with-substituted-coord
			     args i
			     thing))))
	       (typical-point coordsys))))
	  (iota (length types)))))


(define ((literal-field coordsys) type)
    (case type
      ((scalar function)
       (literal-manifold-function
	(generate-uninterned-symbol 'g)
	coordsys))
      ((up vector)
       (literal-vector-field
	(generate-uninterned-symbol 'v)
	coordsys))
      ((down 1form one-form)
       (literal-1form-field
	(generate-uninterned-symbol 'omega)
	coordsys))
      (else
       (error "Bad type list" type))))

#|
(tensor-test
 (Riemann (covariant-derivative (literal-Cartan 'G R3-rect)))
 '(1form vector vector vector)
 R3-rect)
#|
(0 0 0 0)
|#


(define ((F nabla) omega u v)
  (omega ((nabla u) v)))

(tensor-test
 (F (covariant-derivative (literal-Cartan 'G R3-rect)))
 '(1form vector vector)
 R3-rect)
#|
(0 0 <Mess>)
|#

(define ((G nabla) omega u v)
  (omega ((torsion-vector nabla) u v)))

(tensor-test
 (G (covariant-derivative (literal-Cartan 'G R3-rect)))
 '(1form vector vector)
 R3-rect)
#|
(0 0 0)
|#
|#