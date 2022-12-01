#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(define the-tests
  (test-suite
   "simplify/expand-nary"
   (check-equal? (with-units->expression SI &foot)
                 '(& .3048 &meter))
   (check-equal? (with-units->expression SI (& 2 &foot))
                 '(& .6096 &meter))
   #;#;#;
   (check-equal? (with-units->expression SI (/ (* 1.380658e-23 (& 300 &kelvin)) 1.60217733e-19))
                 '(& .02585215707677003 &volt))
   (check-equal? (with-units->expression SI 2.99792458e8)
                 '(& 299792458. (* &meter (expt &second -1))))
   (check-equal? (with-units->expression SI 6.6260755e-34)
                 '(& 6.6260755e-34 (* (expt &meter 2) &kilogram (expt &second -1))))
   (check-within (let ([:G (& 6.67259e-11 (/ (* &newton &meter &meter) &kilogram))]
                       [earth-mass (& 5.976e24 &kilogram)]
                       [earth-radius (& 6371e3 &meter)])
                   (definite-integral
                     (lambda (r)
                       (/ (* :G earth-mass)
                          (square (+ earth-radius r))))
                     (& 0 &meter) (& 1 &meter)))
                 (& 9.824031599863007 &joule)
                 1e-14)
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))