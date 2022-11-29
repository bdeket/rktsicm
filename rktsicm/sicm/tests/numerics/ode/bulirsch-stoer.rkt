#lang racket/base

(require rackunit
         "../../../numerics/ode/bulirsch-stoer.rkt"
         "../../../numerics/ode/advance.rkt"
         "../../helper.rkt"
         (only-in racket/list take)
         )


(define the-tests
  (test-suite
   "numerics/ode/bulirsch-stoer"
   (check-within ((advance-generator
                   (bulirsch-stoer-lisptran		;integrator
                    (lambda (vin vout)			;x'= x
                      (vector-set! vout 0
                                   (vector-ref vin 0)))
                    1					;1 dimension
                    .0001))				;error tolerated
                  #(1.0)
                  1.0
                  0.1
                  0.5					;no step larger than .5
                  (lambda (ns dt h cont)
                    (cons (list dt ns) (cont)))
                  (lambda (ns dt sdt)
                    ;; assert ns = #(2.718...)
                    ;; assert dt = 1.000...+-
                    (list ns dt sdt)))
                 '((0. #(1.))
                   (.1 #(1.1051708858929685))
                   (.25 #(1.2840251054195329))
                   (.47500000000000003 #(1.6080138082200066))
                   (.8125 #(2.2535342510080656))
                   #(2.7182794600110927) 1. .28125)
                 .0001)
   (check-within (take ((advance-generator
                   (bulirsch-stoer-lisptran		;integrator
                    (lambda (vin vout)
                      (vector-set! vout 0 1.0)
                      (vector-set! vout 1 (- 0.0 (vector-ref vin 2)))
                      (vector-set! vout 2  (vector-ref vin 1)))
                    3					;3 dimensions
                    1e-12))				;error tolerated
                  #(0.0 1.0 0.0)
                  (* 2 (angle -1))
                  0.1
                  1.0					;no step larger than 1.0
                  (lambda (ns dt h cont)
                    (cont))
                  (lambda (ns dt sdt)
                    ;; assert ns = #(2.718...)
                    ;; assert dt = 1.000...+-
                    (list ns dt sdt))) 2)
                 '(#(6.283185307179586 .9999999999999994 1.3276830294967203e-15) 6.283185307179586 #;1.4325904607693793)
                 1e-12)
   (test-case
    ""
    (define (f x) (sin (/ 1.0 x)))
    (check-within (take ((advance-generator
                    (bulirsch-stoer-lisptran		;integrator
                     (lambda (vin vout)
                       (let ((x (vector-ref vin 0)))
                         (vector-set! vout 0 1.0)
                         (vector-set! vout 1 (- 0.0 (/ (cos (/ 1.0 x)) (* x x))))))
                     2
                     1e-14))				;error tolerated
                   (vector -2.0 (f -2.0))
                   1.9
                   0.1
                   1.0
                   (lambda (ns dt h cont)
                     (cont))
                   (lambda (ns dt sdt)
                     ;; assert ns = #(2.718...)
                     ;; assert dt = 1.000...+-
                     (list ns dt sdt))) 2)
                  '(#(-.10000000000000009 .5440211108893656) 1.9 #;2.7269736328124856e-2)
                  1e-12))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))