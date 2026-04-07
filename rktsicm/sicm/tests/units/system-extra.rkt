#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "units/system"
   (test-case
    "with-units -> conversion"
    (check-equal? (with-units->expression SI &foot)
                  '(& .3048 &meter))
    (check-equal? (simplify-units &foot)
                  '(& .3048 &meter))
    (check-equal? (with-si-units->expression &foot)
                  '(& .3048 &meter))
    (check-equal? (with-units->expression SI (& 2 &foot))
                  '(& .6096 &meter))
    (check-equal? (with-units->expression SI (/ (* :k (& 300 &kelvin)) :e))
                  '(& 2.5852030245154888e-2 &volt))
    (check-equal? (with-units->expression SI :c)
                  '(& 299792458. (* &meter (expt &second -1))))
    (check-equal? (with-units->expression SI :h)
                  '(& 6.62606896e-34 (* (expt &meter 2) &kilogram (expt &second -1)))))
   (test-case
    "from with-units"
    (check-within (definite-integral
                    (lambda (r)
                      (/ (* :G earth-mass (& 1 &kilogram))
                         (square (+ earth-radius r))))
                    (& 0 &meter) (& 1 &meter))
                  (& 9.826519781124519 &joule)
                  1e-15))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))