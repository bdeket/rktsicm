#lang racket/base

(require rackunit
         racket/list
         (only-in "../../kernel.rkt") ;; generics need to be loaded
         "../../simplify/simplify.rkt"
         "../helper.rkt")

(define (=> a b) (list '=> a b))
(define (<= a b c) (list '<= a b c))

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/simplify"
   (test-case
    "make-analyzer | expression analyzer"
    (define a1 ((expression-analyzer (make-analyzer => <= `(add1))) '(add1 x)))
    (check-equal? (take a1 3) `(<= (add1 x) ,=>))
    (check-true (and (procedure? (list-ref a1 3)) (= 2 (procedure-arity (list-ref a1 3)))))

    (define a2 ((expression-analyzer (make-analyzer => <= `(add1))) '(add1 (expt x 2))))
    (check-unique-match? (take a2 3)
                         (kernel)
                         `(<= (add1 ,kernel) ,=>))
    (check-true (and (procedure? (list-ref a2 3)) (= 2 (procedure-arity (list-ref a2 3)))))

    (define a3 ((expression-analyzer (make-analyzer => <= `(add1 expt))) '(add1  (expt x 2))))
    (check-equal? (take a3 3) `(<= (add1 (expt x 2)) ,=>))
    (check-true (and (procedure? (list-ref a3 3)) (= 2 (procedure-arity (list-ref a3 3)))))

    (define a4 ((expression-analyzer (make-analyzer => <= `(add1 +))) '(+ (add1 (expt x 2)) (expt x 2))))
    (check-unique-match? (take a4 3)
                         (kernel)
                         `(<= (+ (add1 ,kernel) ,kernel) ,=>))
    (check-true (and (procedure? (list-ref a4 3)) (= 2 (procedure-arity (list-ref a4 3)))))

    (check-unique-match? (take ((expression-analyzer (make-analyzer => <= `(add1 + expt))) '(+ (add1 (expt x 2)) (expt x 2))) 3)
                  ()
                  `(<= (+ (add1 (expt x 2)) (expt x 2)) ,=>))

    (check-unique-match? (take ((expression-analyzer (make-analyzer => <= `(add1 + expt))) '(+ (add1 (expt x 2.3)) (expt x 2.3))) 3)
                         (kernel)
                         `(<= (+ (add1 ,kernel) ,kernel) ,=>))

    (set!-inhibit-expt-simplify? #f)
    (check-unique-match? (take ((expression-analyzer (make-analyzer => <= `(add1 + expt))) '(+ (add1 (expt x 2.3)) (expt x 2.3))) 3)
                         ()
                         `(<= (+ (add1 (expt x 2.3)) (expt x 2.3)) ,=>))
    (set!-inhibit-expt-simplify? #t)

    (check-unique-match? (take ((expression-analyzer (make-analyzer => <= `(add1 +))) '(+ (add1 (expt x 2)) (expt y 2))) 3)
                         (kx ky)
                         `(<= (+ (add1 ,kx) ,ky) ,=>))

    (check-unique-match? (take ((expression-analyzer (make-analyzer => <= `(add1 +))) '(+ (add1 (* x y)) (* x 1 y))) 3)
                         (kxy)
                         `(<= (+ (add1 ,kxy) ,kxy) ,=>))
    (check-unique-match? (take ((expression-analyzer (make-analyzer => <= `(add1 +))) '(+ (add1 x) (* x 1))) 3)
                         ()
                         `(<= (+ (add1 x) x) ,=>))
    (check-unique-match? (take ((expression-analyzer (make-analyzer => <= `(add1 +))) '(+ (add1 (++ (+ 1) x)) x)) 3)
                         (k++)
                         `(<= (+ (add1 ,k++) x) ,=>)))
   (test-case
    "make-analyzer | aux-var"
    (define ANA (make-analyzer => <= `(add1)))
    (check-equal? ((auxiliary-variable-fetcher ANA)) '())
    ((expression-analyzer ANA) '(add1 (expt x 2)))
    (check-unique-match? ((auxiliary-variable-fetcher ANA))
                         (k)
                         `((,k (expt x 2))))
    ((initializer ANA))
    (check-equal? ((auxiliary-variable-fetcher ANA)) '()))
   (test-case
    "make-analyzer | priority"
    (define ANA (make-analyzer => <= '(+)))
    (define less?_1 (list-ref ((expression-analyzer ANA) '(+ x y)) 3))
    (check-true  (less?_1 'x 'y))
    (check-false (less?_1 'y 'x))
    (check-equal? ((priority-setter ANA) 'y 'x) '(y x))
    (define less?_2 (list-ref ((expression-analyzer ANA) '(+ x y)) 3))
    (check-true  (less?_2 'y 'x))
    (check-false (less?_2 'x 'y))
    ;; it affects internal state:
    (check-true  (less?_1 'y 'x))
    (check-false (less?_1 'x 'y))
    ;; a seen variable comes before an unseen
    (check-true  (less?_1 'x 'a))
    (check-false (less?_1 'a 'x))
    ((initializer ANA))
    (check-true  (less?_1 'x 'y))
    (check-false (less?_1 'y 'x))
    ;; if it is an expression it is made into a kernel
    ((priority-setter ANA) '(+ 3 4))
    (check-unique-match? ((auxiliary-variable-fetcher ANA)) (k) `((,k (+ 3 4)))))
   (test-case
    "make-analyzer | simplify-expression"
    (define ANA (make-analyzer => <= '(+)))
    (define A (list-ref ((expression-simplifier ANA) '(+ (* 2 x) (+ 0 y))) 1))
    (check-unique-match? A
                         (k)
                         `(+ ,k (+ 0 y)))
    ;; using the simplifier again same kernels are preserved
    (check-equal? (list-ref ((expression-simplifier ANA) '(+ (+ 0 y) (* 2 x))) 1)
                  `(+ (+ 0 y) ,(cadr A)))
    ;; using the default-simplifier a new analysis is started
    (define B (list-ref ((default-simplifier ANA) '(+ (* 2 x) (+ 0 y))) 1))
    (check-unique-match? B
                         (k)
                         `(+ ,k (+ 0 y)))
    (check-false (eq? (cadr A) (cadr B))))

   (test-case
    "???:simplify"
    (let ([E '(- a (* b (- (exp  (/ (- 3 c) d)) 1)))])
      (check-equal? (pcf:simplify E)
                    '(+ a (* (+ 1 (* -1 (exp (/ (+ 3 (* -1 c)) d)))) b)))
      (check-equal? (fpf:simplify E)
                    '(+ (* -1 b (exp (/ (+ 3 (* -1 c)) d))) a b))
      (check-equal? (rcf:simplify E)
                    '(+ a (* (+ 1 (* -1 (exp (/ (+ 3 (* -1 c)) d)))) b))))
    (let ([E '(/ (* (+ x 2) y) (+ x 2))])
      (check-equal? (pcf:simplify E)
                    ;; TODO : is this ok? we switched order (matrices?)
                    '(/ (+ (* y x) (* 2 y)) (+ 2 x)))
      (check-equal? (fpf:simplify E)
                    '(/ (+ (* x y) (* 2 y)) (+ 2 x)))
      (check-equal? (rcf:simplify E)
                    'y)))

   (test-case
    "todo"

    fpf:analyzer
    fpf:simplify

    pcf:analyzer
    pcf:simplify

    rcf:analyzer
    rcf:simplify)
   ;**************************************************************************************************
   (test-case
    "part 1"
    (void ((initializer rcf:analyzer)))

    (check-unique-match? ((expression-analyzer rcf:analyzer)
                          '(- i (* Is (- (exp (/ (- v2 v3) Vt)) 1))))
                         (kernel17)
                         `(+ (* (+ 1 (* -1 ,kernel17)) Is) i))
    (check-unique-match? ((auxiliary-variable-fetcher rcf:analyzer))
                         (kernel16 kernel17)
                         (list-no-order `(,kernel16 (/ (+ v2 (* -1 v3)) Vt))
                                        `(,kernel17 (exp ,kernel18)))
                         #:when (eq? kernel16 kernel18))
    (check-unique-match? ((expression-analyzer rcf:analyzer)
                          '(exp (/ (- v3 v2) (- Vt))))
                         (kernel17)
                         `(,@kernel17))
    (check-equal? ((expression-simplifier rcf:analyzer)
                   '(- i (* Is (- (exp (/ (- v2 v3) Vt)) 1))))
                  '(+ (* (+ 1 (* -1 (exp (/ (+ v2 (* -1 v3)) Vt)))) Is) i)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))