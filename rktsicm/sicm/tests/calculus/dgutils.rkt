#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper+scm.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/dgutils"
   (test-case
    "diffop-name"
    (check-equal? (diffop-name d) 'd)
    (check-equal? (diffop-name (literal-function 'f)) 'f)
    (check-equal? (diffop-name (d (literal-function 'f))) '(d f))
    (check-equal? (diffop-name +) '+))
   (test-case
    "s:sigma/r"
    (check-equal? (s:sigma/r invert #()) 0)
    (check-equal? (s:sigma/r invert #(3)) 1/3)
    (check-equal? (s:sigma/r list #(1)#(1)) '(1 1))
    (check-equal? (s:sigma/r vector #(1 2)#(2 5)) #(3 7))
    (check-equal? (s:sigma/r * (up 1 2 3 4) #(a b c d)) (+ (* 1 'a) (* 2 'b) (* 3 'c) (* 4 'd)))
    (check-equal? (s:sigma/r * (up #(1 2) #(3 4)) #(#(a b) #(c d)))
                  (+ (* 1 'a) (* 2 'b) (* 3 'c) (* 4 'd))))
   (test-case
    "s:sigma/r/l"
    (check-equal? (s:sigma/r/l invert '(#())) 0)
    (check-equal? (s:sigma/r/l invert '(#(3))) 1/3)
    (check-equal? (s:sigma/r/l list '(#(1)#(1))) '(1 1))
    (check-equal? (s:sigma/r/l vector '(#(1 2)#(2 5))) #(3 7))
    (check-equal? (s:sigma/r/l * (list (up 1 2 3 4) #(a b c d))) (+ (* 1 'a) (* 2 'b) (* 3 'c) (* 4 'd)))
    (check-equal? (s:sigma/r/l * (list (up #(1 2) #(3 4)) #(#(a b) #(c d))))
                  (+ (* 1 'a) (* 2 'b) (* 3 'c) (* 4 'd))))
   (test-case
    "s:sigma"
    (check-equal? (s:sigma invert #()) 0)
    (check-equal? (s:sigma invert #(3)) 1/3)
    (check-equal? (s:sigma list #(1)#(1)) '(1 1))
    (check-equal? (s:sigma vector #(1 2)#(2 5)) #(3 7))
    (check-equal? (s:sigma * (up 1 2 3 4) #(a b c d)) (+ (* 1 'a) (* 2 'b) (* 3 'c) (* 4 'd)))
    (check-equal? (s:sigma * (up #(1 2) #(3 4)) #(#(a b) #(c d)))
                  (up (up (+ 'a (* 3 'c)) (+ (* 2 'a) (* 4 'c)))
                      (up (+ 'b (* 3 'd)) (+ (* 2 'b) (* 4 'd))))))
   (test-case
    "s:sigma/l"
    (check-equal? (s:sigma/l invert '(#())) 0)
    (check-equal? (s:sigma/l invert '(#(3))) 1/3)
    (check-equal? (s:sigma/l list '(#(1)#(1))) '(1 1))
    (check-equal? (s:sigma/l vector '(#(1 2)#(2 5))) #(3 7))
    (check-equal? (s:sigma/l * (list (up 1 2 3 4) #(a b c d))) (+ (* 1 'a) (* 2 'b) (* 3 'c) (* 4 'd))))
   (test-case
    "memoized-simplify"
    (define expr (cons '+ (for/list ([i (in-range 1000000)]) i)))
    (collect-garbage 'major)
    (define t0 (current-milliseconds))
    (memoized-simplify expr)
    (define t1 (current-milliseconds))
    (memoized-simplify expr)
    (define t2 (current-milliseconds))
    (define % (/ (- t2 t1) (- t1 t0) 0.25))
    (check-true (< % 0.5)))
   (test-case
    "simplify-numerical-expression"
    (check-equal? (simplify-numerical-expression 1) 1)
    (define g (gensym))
    (check-equal? (simplify-numerical-expression g) g)
    (check-equal? (simplify-numerical-expression (+ 4 'a)) (+ 4 'a))
    (check-equal? (simplify-numerical-expression
                  (/ 1 (+ (/ 1 'r1) (/ 1 'r2))))
                 (/ (* 'r1 'r2) (+ 'r1 'r2)))
    ;; simplify needs to preserve extra properties
    (define exp0 (/ 1 (+ (/ 1 'r1) (/ 1 'r2))))
    (add-property! exp0 'some 'property)
    (define exp1 (/ (* 'r1 'r2) (+ 'r1 'r2)))
    (add-property! exp1 'some 'property)
    (check-equal? (simplify-numerical-expression exp0)
                  exp1))
   (test-case
    "with-incremental-simplifier"
    ;; each intermediate step is simplified (see numsymb make-numsymb-expression)
    ;; TODO ;; this should probably be moved
    (check-equal? (expression (sqrt (exp (log 'a)))) '(sqrt (exp (log a))))
    (check-equal? (with-incremental-simplifier (λ () (sqrt (exp (log 'a))))) (sqrt 'a))
    (parameterize ([enable-constructor-simplifications? #f])
      (check-equal? (symb:add 4 '(+ 4 8)) '(+ 4 4 8))
      (check-equal? (with-incremental-simplifier (λ () (symb:add 4 '(+ 4 8)))) 16)))
   (test-case
    "R2 rectangular"
    (define R2 R2-rect)
    (install-coordinates R2 (up 'x 'y))
    (define chi-R2 (R2 '->coords))
    (define chi-inverse-R2 (R2 '->point))
    (define R2-basis (coordinate-system->basis R2))
    (check-simplified? (s:sigma/r (lambda (e) 
                                    ((e (compose (literal-function 'f (-> (UP Real Real) Real))
                                                 chi-R2))
                                     (chi-inverse-R2 (up 'x0 'y0))))
                                  (basis->vector-basis R2-basis))
                       '(+ (((partial 1) f) (up x0 y0)) (((partial 0) f) (up x0 y0)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))