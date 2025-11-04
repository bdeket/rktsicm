#lang racket/base

(require rackunit
         "../../generic.rkt"
         "../helper.rkt"
         )

(kernel:assign-operations #t)
(define 1+ (+ 1 2e-16))
(provide the-tests)
(define the-tests
  (test-suite
   "kernel/heuristic"
   (test-case
    "heuristic-canonicalize-real"
    (check-equal? (heuristic-canonicalize-real (angle -1) #f) (angle -1))
    (check-equal? (expression (heuristic-canonicalize-real (angle -1) #t)) ':pi)
    (check-equal? (expression (heuristic-canonicalize-real (* 2 1+ (angle -1)) #t)) '(* 2 :pi))
    (check-equal? (heuristic-canonicalize-real 1+) 1)
    (check-equal? (heuristic-canonicalize-real 1+ #t) 1)
    (check-equal? (heuristic-canonicalize-real 1+ #t 2) 0)
    (check-equal? (heuristic-canonicalize-real #i1/3) 1/3)
    (check-equal? (heuristic-canonicalize-real 0.12345678910111213) 0.12345678910111213)
    )
   (test-case
    "heuristic-round-real"
    (check-equal? (heuristic-round-real 1+) 1)
    (check-equal? (heuristic-round-real 1+ 1e-10) 1)
    (check-equal? (heuristic-round-real (+ 1+ 1e-14) 1e-10) 1)
    (check-equal? (heuristic-round-real 1+ 2.) 0))
   (test-case
    "heuristic-canonicalize-complex"
    (check-equal? (heuristic-canonicalize-complex (make-rectangular (angle -1) #i1/7) #f)
                  (make-rectangular (angle -1) 1/7))
    (check-equal? (expression (heuristic-canonicalize-complex (make-rectangular (angle -1) (exp 1)) #t))
                  '(+ :pi (* 0+1i (exp 1))))
    (check-equal? (expression (heuristic-canonicalize-complex (make-rectangular :phi 0.5772156649015329)))
                  '(+ :phi (* 0+1i :euler)))
    (check-equal? (heuristic-canonicalize-complex 1+1e-20i #t) 1)
    (check-equal? (heuristic-canonicalize-complex 1+1e-20i #t 2) 0)
    (check-equal? (heuristic-canonicalize-complex 1e-20+1i #t) +i)
    (check-equal? (heuristic-canonicalize-complex (* 3 1+) #t) 3)
    (check-equal? (expression (heuristic-canonicalize-complex (make-polar (sqrt 3) (* 1/3 :pi))))
                  '(* (sqrt 3) (+ (cos (* 1/3 :pi)) (* 0+1i (sin (* 1/3 :pi))))))
    )
   (test-case
    "heuristic-round-complex"
    (check-equal? (heuristic-round-complex (make-rectangular 1+ 1e-30)) 1)
    (check-equal? (heuristic-round-complex (make-rectangular 1+ 1e-30) 1e-100) 1)
    (check-equal? (heuristic-round-complex (make-rectangular 1+ 1e-8)  1e-7) 1)
    (check-equal? (heuristic-round-complex (make-rectangular 1+ 1e-8)  1e-15) 1+1e-8i)
    (check-equal? (heuristic-round-complex (make-rectangular 1+ 1e-30) 2.) 0))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))