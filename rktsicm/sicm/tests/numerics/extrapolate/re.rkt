#lang racket/base

(require rackunit
         "../../../kernel-intr.rkt"
         "../../../numerics/extrapolate/re.rkt"
         "../../helper.rkt"
         )


(define (refine-by-doubling s) (/ s (sqrt (+ 2 (sqrt (- 4 (* s s)))))))
(define (stream-of-iterates next value)
  (cons-stream value (stream-of-iterates next (next value))))
(define side-lengths
  (stream-of-iterates refine-by-doubling (sqrt 2)))
(define side-numbers
  (stream-of-iterates (lambda (n) (* 2 n)) 4))
(define (semi-perimeter length-of-side number-of-sides)
  (* (/ number-of-sides 2) length-of-side))
(define archimedean-pi-sequence
  (map-streams semi-perimeter side-lengths side-numbers))

(define the-tests
  (test-suite
   "numerics/extrapolate/re"
   ;;Archimedean computation of Pi.
   (check-within (stream->list (stream-take archimedean-pi-sequence 24))
                 '(2.8284271247461903
                   3.0614674589207183
                   3.1214451522580524
                   3.1365484905459393
                   3.140331156954753
                   3.141277250932773
                   3.1415138011443013
                   3.141572940367092
                   3.14158772527716
                   3.1415914215112
                   3.141592345570118
                   3.141592576584873
                   3.1415926343385636
                   3.141592648776986
                   3.1415926523865916
                   3.1415926532889933
                   3.1415926535145937
                   3.141592653570994
                   3.141592653585094
                   3.141592653588619
                   3.1415926535895
                   3.1415926535897203
                   3.1415926535897754
                   3.141592653589789)
                 1e-13)
   (check-within (stream->list (stream-take (richardson-sequence archimedean-pi-sequence 2 2) 7))
                 '(2.8284271247461903
                   3.1391475703122276
                   3.1415903931299374
                   3.141592653286045
                   3.1415926535897865
                   3.141592653589793
                   3.1415926535897936)
                 1e-13)
   (check-equal? (guess-ord-and-inc archimedean-pi-sequence)
                 '(2. 2.))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))