#lang racket/base

(provide (except-out (all-defined-out) assign-operation))

(require "../general/assert.rkt"
         "../kernel/generic.rkt"
         "../kernel/types.rkt"
         "../kernel/utils.rkt"
         "units.rkt")
(define-values (assign-operation with-units:assign-operations)
  (make-assign-operations 'with-units))

;TODO
(define angular '*angular*)

;;;; Quantities with units

;;; If set to #t allows contageous no-unit combinations.
(define *permissive-units* #f)

;;; Quantities without explicit units are assumed unitless.

#|
;;; In types.scm
(define with-units-type-tag '*with-units*)

(define (with-units? x)
  (and (pair? x)
       (eq? (car x) with-units-type-tag)))
|#
(add-to-numerical-quantity? (λ (x) (and (with-units? x) (numerical-quantity? (u:value x)))))

(define (without-units? x)
  (not (with-units? x)))

(define (unitless-quantity? x)
  (unitless? (u:units x)))

(define (u:arity x)
  (g:arity (u:value x)))

(define (u:value x)
  (cond ((with-units? x) (cadr x))
	((units? x) 1)
	(else x)))

(define (u:units x)
  (cond ((with-units? x) (caddr x))
	((units? x) x)
	(else &unitless)))

(define (units:= x y)
  (or (g:zero? x)
      (g:zero? y)
      (equal? (u:units x) (u:units y))))

(define (angular? x)
  (equal? (u:units x) angular))


(define (with-units value units)
  (if (equal? units &unitless)
      value
      (list with-units-type-tag value units)))


(define (has-units? value unit)
  (units:= value unit))

(define (u:type x)
  (g:type (u:value x)))

(define (u:zero-like x)			;can add to anything with same units
  (with-units (g:zero-like (u:value x))
    (u:units x)))

(define (u:one-like x)			;can multiply anything with same units
  (with-units (g:one-like (u:value x))
    &unitless))

(define (u:zero? x)
  (g:zero? (u:value x)))

(define (u:one? x)
  (g:one? (u:value x)))

(define (u:= x y)
  (and (units:= x y)
       (g:= (u:value x) (u:value y))))

(define (u:< x y)
  (and (units:= x y)
       (g:< (u:value x) (u:value y))))

(define (u:<= x y)
  (and (units:= x y)
       (g:<= (u:value x) (u:value y))))

(define (u:> x y)
  (and (units:= x y)
       (g:> (u:value x) (u:value y))))

(define (u:>= x y)
  (and (units:= x y)
       (g:>= (u:value x) (u:value y))))

(define (u:negate x)
  (with-units (g:negate (u:value x)) (u:units x)))

(define (u:invert x)
  (with-units (g:invert (u:value x)) (invert-units (u:units x))))

(define (u:sqrt x)
  (with-units (g:sqrt (u:value x)) (expt-units (u:units x) 1/2)))

(define (u:sin x)
  (assert (unitless-quantity? x) "Arg to sin not dimensionless")
  (with-units (g:sin (u:value x)) &unitless))

(define (u:cos x)
  (assert (unitless-quantity? x) "Arg to cos not dimensionless")
  (with-units (g:cos (u:value x)) &unitless))

(define (u:exp x)
  (assert (unitless-quantity? x) "Arg to exp not dimensionless")
  (with-units (g:exp (u:value x)) &unitless))

(define (u:+ x y)
  (cond ((g:zero? x) y)
	((g:zero? y) x)
	((units:= x y)
	 (with-units (g:+ (u:value x) (u:value y)) (u:units x)))
	((and *permissive-units*
	      (or (without-units? x) (without-units? y)))
	 (g:+ (u:value x) (u:value y)))
	(else (error "Units do not match: +" x y))))

(define (u:- x y)
  (cond ((g:zero? y) x)
	((g:zero? x) (u:negate y))
	((units:= x y)
	 (with-units (g:- (u:value x) (u:value y)) (u:units x)))	
	((and *permissive-units*
	      (or (without-units? x) (without-units? y)))
	 (g:- (u:value x) (u:value y)))
	(else (error "Units do not match: -" x y))))

(define (u:* x y)
  (with-units (g:* (u:value x) (u:value y))
    (*units (u:units x) (u:units y))))

(define (u:/ x y)
  (with-units (g:/ (u:value x) (u:value y))
    (/units (u:units x) (u:units y))))

(define (u:*u x u)
  (u:* x (with-units 1 u)))

(define (u:u* u x)
  (u:* (with-units 1 u) x))

(define (u:t*u t u)
  (u:*u (with-units t &unitless) u))

(define (u:u*t t u)
  (u:u* u (with-units t &unitless)))


(define (u:/u x u)
  (u:/ x (with-units 1 u)))

(define (u:u/ u x)
  (u:/ (with-units 1 u) x))

(define (u:t/u t u)
  (u:/u (with-units t &unitless) u))

(define (u:u/t t u)
  (u:u/ u (with-units t &unitless)))

(define (u:expt x y)
  (if (unitless-quantity? y)
      (with-units (g:expt (u:value x) (u:value y))
	(expt-units (u:units x) (u:value y)))
      (error "Exponent must be unitless: expt" x y)))


(define (u:make-rectangular x y)
  (cond ((g:zero? y) x)
	((g:zero? x)
	 (with-units (g:make-rectangular (u:value x) (u:value y)) (u:units y)))
	((units:= x y)
	 (with-units (g:make-rectangular (u:value x) (u:value y)) (u:units x)))
	((and *permissive-units* (or (without-units? x) (without-units? y)))
	 (g:make-rectangular (u:value x) (u:value y)))
	(else (error "Units do not match: make-rectangular" x y))))

(define (u:make-polar r theta)
  (if (angular? theta)
      (with-units
       (g:make-polar (u:value r) (u:value theta))
       (u:units r))
      (error "Theta must be angular: make-polar" r theta)))

(define (u:real-part z)
  (with-units (g:real-part (u:value z)) (u:units z)))

(define (u:imag-part z)
  (with-units (g:imag-part (u:value z)) (u:units z)))

(define (u:magnitude z)
  (with-units (g:magnitude (u:value z)) (u:units z)))

(define (u:angle z)
  (with-units (g:angle (u:value z)) angular))
	       
(define (u:conjugate z)
  (with-units (g:conjugate (u:value z)) (u:units z)))

(define (u:atan2 y x)
  (cond ((units:= x y)
	 (with-units (g:atan2 (u:value y) (u:value x)) angular))
	((and *permissive-units* (or (without-units? x) (without-units? y)))
	 (g:atan2 (u:value y) (u:value x)))
	(else (error "Units do not match: atan2" y x))))

(define (non-unit? x)
  (not (and (pair? x) (eq? (car x) '*unit*))))


(assign-operation 'type             u:type             with-units?)

(assign-operation 'arity            u:arity            with-units?)

(assign-operation 'zero-like        u:zero-like        with-units?)
(assign-operation 'one-like         u:one-like         with-units?)

(assign-operation 'zero?            u:zero?            with-units?)

;;; The following causes (/ (& 1 &ampere) (& 1 &volt)) to return
;Value 22: (*with-units* 1 #(0 0 0 1 0 0 0))
;(assign-operation 'one?             u:one?             with-units?)

(assign-operation 'negate           u:negate           with-units?)
(assign-operation 'invert           u:invert           with-units?)

(assign-operation 'sqrt             u:sqrt             with-units?)

#|
(assign-operation 'sin              u:sin              angular?)
(assign-operation 'cos              u:cos              angular?)
(assign-operation 'exp              u:cos              angular?)
|#

(assign-operation '=          u:=            with-units? with-units?)
(assign-operation '<          u:<            with-units? with-units?)
(assign-operation '<=         u:<=           with-units? with-units?)
(assign-operation '>          u:>            with-units? with-units?)
(assign-operation '>=         u:>=           with-units? with-units?)

(assign-operation '+   u:+     with-units?             not-differential-or-compound?)
(assign-operation '+   u:+     not-differential-or-compound?  with-units?)

(assign-operation '-   u:-     with-units?             not-differential-or-compound?)
(assign-operation '-   u:-     not-differential-or-compound?  with-units?)

(assign-operation '*   u:*     with-units?             not-differential-or-compound?)
(assign-operation '*   u:*     not-differential-or-compound?  with-units?)

(assign-operation '*   u:*u    with-units?               units?)
(assign-operation '*   u:u*    units?                    with-units?)

(assign-operation '*   u:t*u    not-d-c-u?               units?)
(assign-operation '*   u:u*t    units?                   not-d-c-u?)


(assign-operation '/   u:/     with-units?              not-differential-or-compound?)
(assign-operation '/   u:/     not-differential-or-compound?  with-units?)

(assign-operation '/   u:/u    with-units?               units?)
(assign-operation '/   u:u/    units?                    with-units?)

(assign-operation '/   u:t/u    not-d-c-u?                units?)
(assign-operation '/   u:u/t    units?                    not-d-c-u?)

;(assign-operation 'dot-product  u:dot-product  with-units? with-units?)

(assign-operation 'expt       u:expt         with-units?  not-differential-or-compound?)

;(assign-operation 'gcd        u:gcd          with-units? with-units?)

(assign-operation 'make-rectangular    u:make-rectangular with-units? with-units?)
(assign-operation 'make-polar          u:make-polar       with-units? any?)
(assign-operation 'real-part           u:real-part        with-units?)
(assign-operation 'imag-part           u:imag-part        with-units?)
(assign-operation 'magnitude           u:magnitude        with-units?)
(assign-operation 'angle               u:angle            with-units?)

(assign-operation 'conjugate           u:conjugate        with-units?)

;(assign-operation 'atan1               u:atan            with-units?)
(assign-operation 'atan2               u:atan2            with-units? with-units?)


(assign-operation 'solve-linear-right   u:/     with-units?              not-differential-or-compound?)
(assign-operation 'solve-linear-right   u:/     not-differential-or-compound?  with-units?)

(assign-operation 'solve-linear-right   u:/u    with-units?               units?)
(assign-operation 'solve-linear-right   u:u/    units?                    with-units?)

(assign-operation 'solve-linear-right   u:t/u    not-d-c-u?                units?)
(assign-operation 'solve-linear-right   u:u/t    units?                    not-d-c-u?)


(assign-operation 'solve-linear-left   (lambda (x y) (u:/ y x))     not-differential-or-compound?              with-units?)
(assign-operation 'solve-linear-left   (lambda (x y) (u:/ y x))     with-units?           not-differential-or-compound?)

(assign-operation 'solve-linear-left   (lambda (x y) (u:/u y x))    units?                 with-units?)
(assign-operation 'solve-linear-left   (lambda (x y) (u:u/ y x))    with-units?            units?)

(assign-operation 'solve-linear-left   (lambda (x y) (u:t/u y x))    units?                not-d-c-u?)
(assign-operation 'solve-linear-left   (lambda (x y) (u:u/t y x))    not-d-c-u?            units?)


(assign-operation 'solve-linear   (lambda (x y) (u:/ y x))     not-differential-or-compound?              with-units?)
(assign-operation 'solve-linear   (lambda (x y) (u:/ y x))     with-units?           not-differential-or-compound?)

(assign-operation 'solve-linear   (lambda (x y) (u:/u y x))    units?                 with-units?)
(assign-operation 'solve-linear   (lambda (x y) (u:u/ y x))    with-units?            units?)

(assign-operation 'solve-linear   (lambda (x y) (u:t/u y x))    units?                not-d-c-u?)
(assign-operation 'solve-linear   (lambda (x y) (u:u/t y x))    not-d-c-u?            units?)



#|
(pe (definite-integral
      (lambda (r)
	(/ (* :G earth-mass (& 1 &kilogram))
	   (square (+ earth-radius r))))
      (& 0 &meter) (& 1 &meter)))
(& 9.824031599863007 &joule)
|#


