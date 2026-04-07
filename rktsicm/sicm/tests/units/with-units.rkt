#lang racket/base

(require rackunit
         "../../kernel-gnrc.rkt"
         "../../units/with-units.rkt"
         "../../units/units.rkt"
         "../../units/SI-units.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "units/with-units"
   (test-case
    "with-units"
    (check-true (without-units? 1))
    (check-true (without-units? &meter))
    (check-true (without-units? (with-units 1 &unitless)))
    (check-false (without-units? (with-units 1 (with-units 1 (make-unit 's #() 2)))))
    (check-true (unitless-quantity? 1))
    (check-false (unitless-quantity? (with-units 1 (make-unit 's #() 2))))
    (check-equal? (with-units 1 &unitless) 1)
    (check-equal? (with-units 1 &meter) (list with-units-type-tag 1 &meter))
    (check-false (without-units? (with-units 1 &meter)))
    (check-equal? (u:value 1) 1)
    (check-equal? (u:value (with-units 1 &unitless)) 1)
    (check-equal? (u:value (with-units sqrt &unitless)) sqrt)
    (check-equal? (u:value (with-units sqrt &meter)) sqrt)
    (check-equal? (u:value &meter) 1)
    (check-equal? (u:units 1) &unitless)
    (check-equal? (u:units (with-units 1 &unitless)) &unitless)
    (check-equal? (u:units (with-units sqrt &unitless)) &unitless)
    (check-equal? (u:units (with-units sqrt &meter)) &meter)
    (check-equal? (u:units &meter) &meter)
    (check-false (non-unit? &meter))
    (check-true (non-unit? &unitless))
    (check-true (non-unit? (with-units 1 &meter))))
   (test-case
    "u:arity"
    (check-equal? (u:arity (with-units 1 &meter)) *at-least-zero*)
    (check-equal? (u:arity (with-units sqrt &meter)) *exactly-one*))
   (test-case
    "units:="
    (check-true  (units:= 1 (with-units 1 &unitless)))
    (check-false (units:= 1 (with-units 1 &meter)))
    (check-true  (units:= (with-units 1 &meter) (with-units 1 &meter)))
    (check-true  (units:= (with-units 0 &kilogram) (with-units 1 &meter)))
    (check-true  (units:= (with-units 1 &kilogram) (with-units 0 &meter))))
   (test-case
    "angular?"
    (check-true  (angular? (with-units 1 angular)))
    (check-false (angular? (with-units 1 &meter))))
   (test-case
    "has-units?"
    (check-true  (has-units? (with-units 3 &meter) &meter))
    (check-false (has-units? (with-units 3 &meter) &kilogram)))
   (test-case
    "u:type"
    (check-equal? (u:type &meter) number-type-tag)
    (check-equal? (u:type 1) number-type-tag)
    (check-equal? (u:type (with-units #(1) &meter)) vector-type-tag))
   (test-case
    "zero/one"
    (check-equal? (u:zero-like &meter) (with-units 0 &meter))
    (check-equal? (u:one-like &meter) 1)
    (check-true (u:zero? 0))
    (check-true (u:zero? (with-units 0 &meter)))
    (check-true (u:zero? (with-units #(0) &meter)))
    (check-false (u:zero? (with-units #(1) &meter)))
    (check-true (u:one? 1))
    (check-true (u:one? (with-units 1 &meter)))
    (check-false (u:one? (with-units #(1) &meter)))
    (check-false (u:one? (with-units #(0) &meter))))
   (test-case
    "= < <= > >="
    (check-true (u:= 1 (with-units 1 &unitless)))
    (check-true (u:= &meter (with-units 1 &meter)))
    (check-false (u:= (with-units 1 &meter) (with-units 1 &kilogram)))
    (check-false (u:= (with-units 2 &meter) (with-units 1 &meter)))
    
    (check-true (u:< 1 (with-units 2 &unitless)))
    (check-true (u:< &meter (with-units 2 &meter)))
    (check-false (u:< (with-units 1 &meter) (with-units 2 &kilogram)))
    (check-false (u:< (with-units 1 &meter) (with-units 1 &meter)))
    
    (check-true (u:<= 1 (with-units 2 &unitless)))
    (check-true (u:<= 1 (with-units 1 &unitless)))
    (check-true (u:<= &meter (with-units 2 &meter)))
    (check-true (u:<= &meter (with-units 1 &meter)))
    (check-false (u:<= (with-units 1 &meter) (with-units 2 &kilogram)))
    (check-false (u:<= (with-units 1 &meter) (with-units 1 &kilogram)))
    (check-false (u:<= (with-units 1 &meter) (with-units 0 &meter)))
    
    (check-true (u:> 2 (with-units 1 &unitless)))
    (check-true (u:> &meter (with-units 0 &meter)))
    (check-false (u:> (with-units 2 &meter) (with-units 1 &kilogram)))
    (check-false (u:> (with-units 1 &meter) (with-units 1 &meter)))
    
    (check-true (u:>= 2 (with-units 1 &unitless)))
    (check-true (u:>= 1 (with-units 1 &unitless)))
    (check-true (u:>= &meter (with-units 0 &meter)))
    (check-true (u:>= &meter (with-units 1 &meter)))
    (check-false (u:>= (with-units 2 &meter) (with-units 1 &kilogram)))
    (check-false (u:>= (with-units 1 &meter) (with-units 1 &kilogram)))
    (check-false (u:>= (with-units 1 &meter) (with-units 2 &meter))))
   (test-case
    "- / sqrt sin cos exp"
    (check-equal? (u:negate (with-units 'x &meter)) (with-units (- 'x) &meter))
    (check-equal? (u:invert (with-units 'x &meter)) (with-units (/ 'x) (invert-units &meter)))
    (check-equal? (u:sqrt (with-units 'x (*units &meter &meter))) (with-units (sqrt 'x) &meter))
    (check-equal? (u:sin (with-units 'x &unitless)) (sin 'x))
    (check-equal? (u:cos (with-units 'x &unitless)) (cos 'x))
    (check-equal? (u:exp (with-units 'x &unitless)) (exp 'x))
    (check-exn #px"Arg to sin not dimensionless" (λ () (u:sin (with-units 'x &meter))))
    (check-exn #px"Arg to cos not dimensionless" (λ () (u:cos (with-units 'x &meter))))
    (check-exn #px"Arg to exp not dimensionless" (λ () (u:exp (with-units 'x &meter))))
    )
   (test-case
    "u+"
    (check-equal? (u:+ 0 (with-units 'x &meter)) (with-units 'x &meter))
    (check-equal? (u:+ &meter 0) &meter)
    (check-equal? (u:+ &meter (with-units 'x &meter)) (with-units (+ 1 'x) &meter))
    (check-exn #px"Units do not match: +" (λ () (u:+ 1 &meter)))
    (set-permissive-units?! #t)
    (check-equal? (u:+ 1 &meter) 2)
    (check-equal? (u:+ &kilogram &meter) 2)
    (check-exn #px"Units do not match: +" (λ () (u:+ (with-units 1 &kilogram) (with-units 1 &meter))))
    (set-permissive-units?! #f))
   (test-case
    "u-"
    (check-equal? (u:- 0 (with-units 'x &meter)) (with-units (- 'x) &meter))
    (check-equal? (u:- &meter 0) &meter)
    (check-equal? (u:- &meter (with-units 'x &meter)) (with-units (- 1 'x) &meter))
    (check-exn #px"Units do not match: -" (λ () (u:- 1 &meter)))
    (set-permissive-units?! #t)
    (check-equal? (u:- 1 &meter) 0)
    (check-equal? (u:- &kilogram &meter) 0)
    (check-exn #px"Units do not match: -" (λ () (u:- (with-units 1 &kilogram) (with-units 1 &meter))))
    (set-permissive-units?! #f))
   (test-case
    "u* u:*u u:u* u:t*u u:u*t"
    (check-equal? (u:* &meter &kilogram) (with-units 1 (*units &meter &kilogram)))
    (check-equal? (u:* 'x &kilogram) (with-units 'x &kilogram))
    (check-equal? (u:*u (with-units 4 &meter) &meter) (with-units 4 (*units &meter &meter)))
    (check-equal? (u:u* &kilogram (with-units 4 &meter)) (with-units 4 (*units &kilogram &meter)))
    (check-exn #px"" (λ () (u:*u &meter (with-units 4 &meter))))
    (check-exn #px"" (λ () (u:u* (with-units 4 &meter) &meter)))
    (check-equal? (u:t*u 5 &meter) (with-units 5 &meter))
    (check-equal? (u:u*t &meter 5) (with-units 5 &meter))
    (check-exn #px"" (λ () (u:t*u (with-units 4 &meter) 5)))
    (check-exn #px"" (λ () (u:u*t 5 (with-units 4 &meter)))))
   (test-case
    "u/ u:/u u:u/ u:t/u u:u/t"
    (check-equal? (u:/ (with-units 5 &meter) &kilogram) (with-units 5 (/units &meter &kilogram)))
    (check-equal? (u:/ &meter (with-units 'x &kilogram)) (with-units (/ 'x) (/units &meter &kilogram)))
    (check-equal? (u:/u (with-units 4 &meter) &meter) 4)
    (check-equal? (u:u/ &kilogram (with-units 4 &meter)) (with-units 1/4 (/units &kilogram &meter)))
    (check-exn #px"" (λ () (u:/u &meter (with-units 4 &meter))))
    (check-exn #px"" (λ () (u:u/ (with-units 4 &meter) &meter)))
    (check-equal? (u:t/u 5 &meter) (with-units 5 (/units &unitless &meter)))
    (check-equal? (u:u/t &meter 5) (with-units 1/5 &meter))
    (check-exn #px"" (λ () (u:t/u (with-units 4 &meter) 5)))
    (check-exn #px"" (λ () (u:u/t 5 (with-units 4 &meter)))))
   (test-case
    "u:expt"
    (check-equal? (u:expt (with-units 'x &meter) 3) (with-units (expt 'x 3) (expt-units &meter 3)))
    (check-exn #px"Exponent must be unitless: expt" (λ () (u:expt (with-units 'x &meter) &kilogram)))
    ;; TODO -- this should work, not?
    (check-exn #px"" (λ () (u:expt (with-units 'x &meter) 'y))))
   (test-case
    "make-rectangular"
    (check-equal? (u:make-rectangular (with-units 'x &meter) 0) (with-units 'x &meter))
    (check-equal? (u:make-rectangular 0 (with-units 'y &meter)) (with-units (make-rectangular 0 'y) &meter))
    (check-equal? (u:make-rectangular (with-units 'x &meter) (with-units 'y &meter)) (with-units (make-rectangular 'x 'y) &meter))
    (check-exn #px"Units do not match: make-rectangular" (λ () (u:make-rectangular (with-units 'x &meter) (with-units 'y &kilogram))))
    (set-permissive-units?! #t)
    (check-equal? (u:make-rectangular 1 &meter) 1+1i)
    (check-equal? (u:make-rectangular &kilogram &meter) 1+1i)
    (check-exn #px"Units do not match: make-rectangular" (λ () (u:make-rectangular (with-units 1 &kilogram) (with-units 1 &meter))))
    (set-permissive-units?! #f))
   (test-case
    "make:polar"
    (skip (u:make-polar (with-units 'x &meter) 1)) ;; this should work, right? ... otherwise sin...
    (check-equal? (u:make-polar (with-units 'x &meter) (with-units 0 angular)) (with-units (make-polar 'x 0) &meter))
    (check-equal? (u:make-polar 0 (with-units 'y angular)) (with-units (make-polar 0 'y) &unitless))
    (check-equal? (u:make-polar (with-units 'x &meter) (with-units 'y angular)) (with-units (make-polar 'x 'y) &meter))
    (check-exn #px"Theta must be angular: make-polar" (λ () (u:make-polar (with-units 'x &meter) (with-units 'y &kilogram)))))
   (test-case
    "complex other"
    (check-equal? (u:real-part (with-units 3+5i &meter)) (with-units 3 &meter))
    (check-equal? (u:imag-part (with-units 3+4i &meter)) (with-units 4 &meter))
    (check-equal? (u:magnitude (with-units 3+4i &meter)) (with-units 5 &meter))
    (check-equal? (u:angle (with-units 3+4i &meter)) (with-units 0.9272952180016122 angular))
    (check-equal? (u:conjugate (with-units 3+4i &meter)) (with-units 3-4i &meter))
    (check-equal? (u:atan2 (with-units 4 &meter) (with-units 3 &meter)) (with-units 0.9272952180016122 angular))
    (check-exn #px"Units do not match: atan2" (λ () (u:atan2 (with-units 1 &kilogram) (with-units 1 &meter))))
    (set-permissive-units?! #t)
    (check-equal? (u:atan2 1 &meter) 0.7853981633974483)
    (check-equal? (u:atan2 &kilogram &meter) 0.7853981633974483)
    (check-exn #px"Units do not match: atan2" (λ () (u:atan2 (with-units 1 &kilogram) (with-units 1 &meter))))
    (set-permissive-units?! #f))
   (test-case
    "generics"
    (with-units:assign-operations #t)
    (check-equal? (solve-linear-left 'x (with-units 1 &meter)) (with-units (/ 'x) &meter))
    (check-equal? (solve-linear-left (with-units 1 &meter) 'x) (with-units 'x (/units &unitless &meter)))
    
    (check-equal? (solve-linear-left &kilogram (with-units 3 &meter)) (with-units 3 (/units &meter &kilogram)))
    (check-equal? (solve-linear-left (with-units 3 &meter) &kilogram) (with-units 1/3 (/units &kilogram &meter)))
    
    (check-equal? (solve-linear-left &kilogram 3) (with-units 3 (/units &unitless &kilogram)))
    (check-equal? (solve-linear-left 3 &kilogram) (with-units 1/3 (/units &kilogram &unitless)))
    
    (check-equal? (solve-linear 'x (with-units 1 &meter)) (with-units (/ 'x) &meter))
    (check-equal? (solve-linear (with-units 1 &meter) 'x) (with-units 'x (/units &unitless &meter)))
    
    (check-equal? (solve-linear &kilogram (with-units 3 &meter)) (with-units 3 (/units &meter &kilogram)))
    (check-equal? (solve-linear (with-units 3 &meter) &kilogram) (with-units 1/3 (/units &kilogram &meter)))
    
    (check-equal? (solve-linear &kilogram 3) (with-units 3 (/units &unitless &kilogram)))
    (check-equal? (solve-linear 3 &kilogram) (with-units 1/3 (/units &kilogram &unitless)))

    (check-equal? (apply (with-units sqrt &meter) '(4)) (with-units 2 &meter))
    (check-equal? (apply (with-units sqrt &meter) (list (with-units 4 (*units &meter &meter)))) (with-units 2 &meter))
    )
   
   (skip ;; TODO under permisive rules (+ &kilogram &meter) = 2 :: that can't be the intention
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))