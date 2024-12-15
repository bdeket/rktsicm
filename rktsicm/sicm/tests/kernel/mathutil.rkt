#lang racket/base

(require rackunit
         "../../kernel/mathutil.rkt"
         "../../kernel/express.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/mathutil"
   ;; === cstm ===
   (test-case
    "ref"
    (local-require (only-in "../../kernel/cstm/generic.rkt" g:derivative)
                   (only-in "../../kernel/matrices.rkt" matrix-by-rows)
                   (only-in "../../kernel/cstm/pseries.rkt" exp-series)
                   (only-in "../../kernel/cstm/structs.rkt" up down)
                   (only-in "../../rkt/applyhook.rkt" apply-hook-extra)
                   (only-in racket/stream stream))
    (check-equal? (ref-internal 'any '()) 'any)
    ;; procedures
    ;;;; operator
    (let ([F0 g:derivative]
          [F1 (ref-internal g:derivative '(0))])
      (check-equal? (procedure-arity F1) (procedure-arity F0))
      (check-equal? (apply-hook-extra F1) '(*operator* #f (compose (component 0) derivative) 1))
      (check-equal? ((F1 (λ (x) (vector x))) 't) 1))
    ;;;; normal fun
    (let* ([F0 (λ (x y z) (vector x y z))]
           [F1 (ref-internal F0 '(1))])
      (check-true (procedure? F1))
      (check-equal? (procedure-arity F1) (procedure-arity F0))
      (check-equal? (F1 'a 'b 'c) 'b))
    ;; structures
    ;;;; vector
    (check-equal? (ref-internal #(0 1 2 3) '(2)) 2)
    (check-equal? (ref-internal #(0 1 2 3) '((2 4))) #(2 3))
    ;;;; structure
    (check-equal? (ref-internal (down (down (up 'a 'b 'c))) '(-1 -1 -2)) 'b)
    (check-equal? (ref-internal (down (down (up 'a 'b 'c))) '(-1 -1 (-3 -2))) (up 'a 'b))
    ;; check with down because up = vector
    (check-equal? (ref-internal (down (down (down 'a 'b 'c))) '(-1 -1 (0 2))) (down 'a 'b))
    ;;;; matrix
    (check-equal? (ref-internal (matrix-by-rows '(a b c d)) '(1)) 'b)
    (check-equal? (ref-internal (matrix-by-rows '(a b c d)) '((1 2))) (matrix-by-rows '(b)))
    (check-equal? (ref-internal (matrix-by-rows '(a) '(b) '(c) '(d)) '(2)) 'c)
    (check-equal? (ref-internal (matrix-by-rows '(a) '(b) '(c) '(d)) '((-2 -1)))
                  (matrix-by-rows '(c) '(d)))
    (check-exn #px"Not enuf indices -- REF"
               (λ () (ref-internal (matrix-by-rows '(a b) '(c d)) '(0))))
    (check-equal? (ref-internal (matrix-by-rows '(a b) (list 'c (up 'd 'e))) '(1 1 1)) 'e)
    (check-equal? (ref-internal (matrix-by-rows '(a b c d) '(e f g h)) '((0 2) (0 2)))
                  (matrix-by-rows '(a b) '(e f)))
    (check-exn #px"Not enuf indices -- REF"
               (λ () (ref-internal (matrix-by-rows '(a b) '(c d)) '((0 2)))))
    ;;;; series
    (check-equal? (ref-internal exp-series '(1)) 1)
    ;;;; stream
    (check-equal? (ref-internal (stream 'a (up 1 2 3)) '(1 -1)) 3)
    ;;;; list
    (check-equal? (ref-internal (list 'a '(1 2 3)) '(1 -1)) 3)
    (check-equal? (ref-internal '((1 2 3)(1 2 3 4 5)) '(1 (2 4))) '(3 4))
    ;;;; string
    (check-equal? (ref-internal "da'swadanders" '(4)) #\w)
    (check-exn #px"String has no substructure -- REF" (λ () (ref-internal "d" '(0 0))))
    (check-equal? (ref-internal "da'swadanders" '((3 6))) "swa")
    (check-exn #px"String has no substructure -- REF" (λ () (ref-internal "d" '((0 5) 0))))

    ;; bad selectors
    (check-exn #px"Unknown compound -- G:REF"
               (λ () (ref-internal (exn "an error" (current-continuation-marks)) '(0))))
    (check-exn #px"Unknown compound -- G:REF"
               (λ () (ref-internal (exn "an error" (current-continuation-marks)) '((0 0)))))
    (check-exn #px"Unknown selector type -- REF" (λ () (ref-internal #() '(1.5))))
    (check-exn #px"Bad index -- REF" (λ () (ref-internal #(0 1) '(-5))))
    (check-exn #px"Bad index -- REF" (λ () (ref-internal #(0 1) '( 3))))
    (check-exn #px"Bad index -- REF" (λ () (ref-internal #(0 1) '((-2 -5)))))
    (check-exn #px"Bad index -- REF" (λ () (ref-internal #(0 1) '((0 3)))))
    (skip ;; better error message / blame would be preferable
     (check-exn #px"Bad index -- REF" (λ () (ref-internal #(0 1 2 3 4 5) '((3 0))))))

    (check-equal? (g:ref (matrix-by-rows (list (down (list (stream "halo"))))) 0 0 0 0 0 '(1 4))
                  "alo")
    (check-equal? ((component 0 0 0 0 0 '(1 4))
                   (matrix-by-rows (list (down (list (stream "halo"))))))
                  "alo"))
   (test-case
    "derived generic ops"
    (local-require "../../kernel/numeric.rkt")
    (check-equal? (expression (g:cube 'x)) '(* x x x))
    (check-equal? (g:log10 10) 1.)
    (check-equal? (g:log2 4) 2.)
    (check-equal? (g:exp10 2) 100)
    (check-equal? (g:exp2 3) 8)
    (check-equal? (expression (g:tan 'x)) '(/ (sin x) (cos x)))
    (check-equal? (expression (g:cot 'x)) '(/ (cos x) (sin x)))
    (check-equal? (expression (g:sec 'x)) '(/ 1 (cos x)))
    (check-equal? (expression (g:csc 'x)) '(/ 1 (sin x)))
    (check-equal? (expression (g:tanh 'x)) '(/ (sinh x) (cosh x)))
    (check-equal? (expression (g:sech 'x)) '(/ 1 (cosh x)))
    (check-equal? (expression (g:csch 'y)) '(/ 1 (sinh y)))
    (check-= (g:asinh (sinh 1)) 1. 1e-15)
    (check-= (g:acosh (cosh 1+i)) 1.+1.i 1e-15)
    (check-= (g:atanh (tanh .3)) .3 1e-15)
    (check-equal? (g:identity 'x) 'x))
   (test-case
    "arg-shift / scale"
    (check-equal? (expression ((g:arg-shift list 1 2 3) 'a 'b 'c)) '((+ 1 a) (+ 2 b) (+ 3 c)))
    (check-equal? (expression ((g:arg-scale list 1 2 3) 'a 'b 'c)) '(a (* 2 b) (* 3 c))))
   (test-case
    "size"
    (local-require (only-in "../../kernel/matrices.rkt" matrix-by-rows)
                   (only-in "../../kernel/cstm/pseries.rkt" exp-series)
                   (only-in "../../kernel/cstm/structs.rkt" up down)
                   (only-in racket/stream stream))
    (check-equal? (g:size #(0 1 2)) 3)
    (check-equal? (g:size (matrix-by-rows '(1 2 3) '(4 5 6))) 6)
    (check-equal? (g:size (down 1 2 (down 3 (up 4)))) 3)
    (check-equal? (g:size exp-series) #f)
    (check-equal? (g:size (stream 1 2 3)) #f)
    (check-equal? (g:size '(1 2 (3 (4)))) 3)
    (check-equal? (g:size "tismewa") 7)
    (check-exn #px"Unknown compound -- G:size"
               (λ () (g:size (exn "an error" (current-continuation-marks))))))
   (test-case
    "compose"
    ;; same tests as kernel/utils
    (check-equal? ((g:compose vector list box) 1) #((#&1)))
    (check-equal? ((g:compose) 'niks) 'niks)
    (check-equal? ((g:compose vector) 'niks) #(niks))
    (check-equal? ((g:compose vector list) 'niks) #((niks)))
    (check-equal? ((g:compose vector list box) 1) #((#&1)))
    (check-equal? ((g:compose-2 vector list) 'niks) #((niks)))
    (check-equal? ((g:compose-2 vector (list list box)) 1) #((1) #&1))

    (check-exn #px"g:compose-bin\\+1: contract violation\n  expected: first procedure that accepts 1 argument\n  given:"
               (λ () (g:compose-bin (λ (x y) (+ x y)) (λ (x y) (* x y)))))
    (let ([F (g:compose-bin (λ (x) (- x)) (λ (x y) (* x y)))])
      (check-equal? (procedure-arity F) 2)
      (check-equal? (F 3 6) -18))
    (let ([F (g:compose-bin (λ (x) (- x)) (λ (x . y) (apply * x y)))])
      (check-equal? (procedure-arity F) (arity-at-least 1))
      (check-equal? (F 2 3 4) -24))
    (let ([F (g:compose-bin (λ (x) (- x)) (case-lambda [(x) x][(x y z . q) (apply * x y z q)]))])
      (check-equal? (procedure-arity F) (list 1 (arity-at-least 3)))
      (check-equal? (F 2) -2)
      (check-equal? (F 2 1/2 2 1/2 2 1/2) -1))
    
    (check-exn #px"g:compose-bin\\+n: contract violation\n  expected: first procedure that accepts 2 argument\\(s\\)\n  given:"
               (λ () (g:compose-bin (λ (x) (- x)) (list (λ (x y) (* x y)) (λ y (apply * y))))))
    (let ([F (g:compose-bin (λ (x y) (- x y)) (list (λ (x y) (* x y)) (λ _ 3)))])
      (check-equal? (procedure-arity F) 2)
      (check-equal? (F 3 6) 15))
    (let ([F (g:compose-bin (λ (x y) (- x y)) (list (λ (x y . z) (apply * x y z)) (λ _ 3)))])
      (check-equal? (procedure-arity F) (arity-at-least 2))
      (check-equal? (F 3 1 6) 15))
    (let ([F (g:compose-bin (λ (x y) (- x y)) (list (λ _ 3) (case-lambda [(x) x][(x y z . q) (apply * x y z q)])))])
      (check-equal? (procedure-arity F) (list 1 (arity-at-least 3)))
      (check-equal? (F 2) 1)
      (check-equal? (F 2 1/2 2 1/2 2 1/2) 2))
    ;; now check g:apply is used
    ;; to evaluate generics need to be loaded... (g:apply to be specific)
    (local-require (prefix-in u: "../../kernel/utils.rkt"))
    (define F (vector (λ (x) x)))
    (check-exn #px"" (λ () ((u:compose F) 1))) ;; only fails at execution
    (check-not-exn (λ () (g:compose F)))
    (check-exn #px"" (λ () ((u:compose-2 F F) 1))) ;; only fails at execution
    (check-not-exn (λ () (g:compose-2 F F)))
    (check-exn #px"" (λ () (u:compose-bin F F)))
    (check-not-exn (λ () (g:compose-bin F F)))
    )
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))