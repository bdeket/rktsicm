#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/coord"
   (test-case "ORIG:define-coordinates"
              (define-coordinates (up x y) R2-rect)
              (check-equal?
               (x ((R2-rect '->point) (up 'a 'b)))
               'a)

              (check-equal?
               ((d/dx x) ((R2-rect '->point) (up 'a 'b)))
               1)

              (define-coordinates (up p q) R2-rect)
              (check-unique-match?
               (simplify
                (R2-rect 'coordinate-function-specs))
               [fct]
               `(up (p ,fct) (q ,fct))))

   (test-case "ORIG:using-coordinates"
              (using-coordinates
               (up x y) R2-rect
               (check-equal?
                (x ((R2-rect '->point) (up 'a 'b)))
                'a))

              (using-coordinates
               (up x y) R2-rect
               (check-equal?
                ((d/dx x) ((R2-rect '->point) (up 'a 'b)))
                1)))

   (test-case "ORIG:coordinates"
              (define-coordinates (up x y) R2-rect)

              (check-equal?
               (simplify (x ((R2-polar '->point) (up 'r 'theta))))
               '(* r (cos theta)))

              (using-coordinates
               (up x y) R2-polar
               (check-equal?
                (simplify (x ((R2-rect '->point) (up 'a 'b))))
                '(sqrt (+ (expt a 2) (expt b 2)))))

              (check-equal?
               (x ((R2-rect '->point) (up 'a 'b)))
               'a))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))