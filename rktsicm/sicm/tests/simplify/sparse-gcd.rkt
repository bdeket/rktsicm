#lang s-exp "../../generic.rkt"

(require rackunit
         "../../rkt/glue.rkt"
         "../../simplify/sparse-gcd.rkt"
         "../../simplify.rkt"
         "../helper.rkt")

(define (gcd-test d f g)
      (let ((pd (fpf:expression-> d (lambda (p v) p)))
            (pf (fpf:expression-> f (lambda (p v) p)))
            (pg (fpf:expression-> g (lambda (p v) p))))
        (let ((pdf (fpf:* pd pf)) (pdg (fpf:* pd pg)))
          (sparse-gcd (fpf:->sparse pdf) (fpf:->sparse pdg)
                      (lambda (g)
                        (if (equal? (sort g sparse-term->)
                                    (fpf:->sparse pd))
                            #t
                            (println (list g (fpf:->sparse pd)))))
                      (lambda () #f)))))

(define (mkTerms . a) (map (λ (x) (fpf:make-term (car x) (cadr x))) a))
(define (randExp [n 5] [m 20]) (build-list n (λ _ (random m))))
(define (randTerm [n 5] [m 20] [t 50])
  (define T (if (number? t)
                (if (exact? t) (λ () (- (random (* 2 t)) t)) (λ () (- (* 2 t (random)) t)))
                t))
  (sparse-term (randExp n m) (T)))
(define (randS [r 5] [n 5] [m 20] [t 50])
  (S:clean (build-list r (λ _ (randTerm n m t)))))
(define (S:clean F)
  (for/fold ([H #hash()]
             #:result (sort (for/list ([(k v) (in-hash H)] #:unless (= v 0)) (fpf:make-term k v)) fpf:>exponents? #:key fpf:exponents))
            ([t (in-list (fpf:terms F))])
    (hash-update H (fpf:exponents t) (λ (x) (+ x (fpf:coefficient t))) 0)))
(define A+ '(((4 2 2) . 1) ((3 3 2) . -4) ((3 2 3) . 12) ((2 4 2) . 6) ((2 3 3) . -36) ((2 2 4) . 54) ((1 5 2) . -4) ((1 4 3) . 36) ((1 3 4) . -108) ((1 2 5) . 108) ((0 6 2) . 1) ((0 5 3) . -12) ((0 4 4) . 54) ((0 3 5) . -108) ((0 2 6) . 81) ((5 1 1) . -2) ((4 2 1) . 8) ((4 1 2) . -24) ((3 3 1) . -12) ((3 2 2) . 72) ((3 1 3) . -108) ((2 4 1) . 8) ((2 3 2) . -72) ((2 2 3) . 216) ((2 1 4) . -216) ((1 5 1) . -2) ((1 4 2) . 24) ((1 3 3) . -108) ((1 2 4) . 216) ((1 1 5) . -162) ((6 0 0) . 1) ((5 1 0) . -4) ((5 0 1) . 12) ((4 2 0) . 6) ((4 1 1) . -38) ((4 0 2) . 54) ((3 3 0) . -4) ((3 2 1) . 44) ((3 1 2) . -132) ((3 0 3) . 108) ((2 4 0) . 1) ((2 3 1) . -24) ((2 2 2) . 126) ((2 1 3) . -216) ((2 0 4) . 81) ((1 4 1) . 8) ((1 3 2) . -72) ((1 2 3) . 216) ((1 1 4) . -216) ((0 5 1) . -2) ((0 4 2) . 24) ((0 3 3) . -108) ((0 2 4) . 216) ((0 1 5) . -162) ((5 0 0) . 2) ((4 1 0) . -8) ((4 0 1) . 24) ((3 2 0) . 12) ((3 1 1) . -72) ((3 0 2) . 108) ((2 3 0) . -8) ((2 2 1) . 72) ((2 1 2) . -216) ((2 0 3) . 216) ((1 4 0) . 2) ((1 3 1) . -24) ((1 2 2) . 108) ((1 1 3) . -216) ((1 0 4) . 162) ((4 0 0) . 1) ((3 1 0) . -4) ((3 0 1) . 12) ((2 2 0) . 6) ((2 1 1) . -36) ((2 0 2) . 54) ((1 3 0) . -4) ((1 2 1) . 36) ((1 1 2) . -108) ((1 0 3) . 108) ((0 4 0) . 1) ((0 3 1) . -12) ((0 2 2) . 54) ((0 1 3) . -108) ((0 0 4) . 81)))
(define B+ '(((2 4 4) . 1) ((1 5 4) . -2) ((1 4 5) . 6) ((0 6 4) . 1) ((0 5 5) . -6) ((0 4 6) . 9) ((3 3 3) . -4) ((2 4 3) . 8) ((2 3 4) . -24) ((1 5 3) . -4) ((1 4 4) . 24) ((1 3 5) . -36) ((4 2 2) . 6) ((3 3 2) . -12) ((3 2 3) . 36) ((2 4 2) . 6) ((2 3 3) . -40) ((2 2 4) . 54) ((1 4 3) . 8) ((1 3 4) . -24) ((0 5 3) . -4) ((0 4 4) . 24) ((0 3 5) . -36) ((5 1 1) . -4) ((4 2 1) . 8) ((4 1 2) . -24) ((3 3 1) . -4) ((3 2 2) . 36) ((3 1 3) . -36) ((2 3 2) . -24) ((2 2 3) . 72) ((1 4 2) . 12) ((1 3 3) . -72) ((1 2 4) . 108) ((6 0 0) . 1) ((5 1 0) . -2) ((5 0 1) . 6) ((4 2 0) . 1) ((4 1 1) . -18) ((4 0 2) . 9) ((3 2 1) . 24) ((3 1 2) . -72) ((2 3 1) . -12) ((2 2 2) . 78) ((2 1 3) . -108) ((1 3 2) . -12) ((1 2 3) . 36) ((0 4 2) . 6) ((0 3 3) . -36) ((0 2 4) . 54) ((5 0 0) . 4) ((4 1 0) . -8) ((4 0 1) . 24) ((3 2 0) . 4) ((3 1 1) . -36) ((3 0 2) . 36) ((2 2 1) . 24) ((2 1 2) . -72) ((1 3 1) . -12) ((1 2 2) . 72) ((1 1 3) . -108) ((4 0 0) . 6) ((3 1 0) . -12) ((3 0 1) . 36) ((2 2 0) . 6) ((2 1 1) . -40) ((2 0 2) . 54) ((1 2 1) . 8) ((1 1 2) . -24) ((0 3 1) . -4) ((0 2 2) . 24) ((0 1 3) . -36) ((3 0 0) . 4) ((2 1 0) . -8) ((2 0 1) . 24) ((1 2 0) . 4) ((1 1 1) . -24) ((1 0 2) . 36) ((2 0 0) . 1) ((1 1 0) . -2) ((1 0 1) . 6) ((0 2 0) . 1) ((0 1 1) . -6) ((0 0 2) . 9)))
(define C+ #( (((2 2 2) . 1) ((1 3 2) . -2) ((1 2 3) . 6) ((0 4 2) . 1) ((0 3 3) . -6) ((0 2 4) . 9) ((3 1 1) . -2) ((2 2 1) . 4) ((2 1 2) . -12) ((1 3 1) . -2) ((1 2 2) . 12) ((1 1 3) . -18) ((4 0 0) . 1) ((3 1 0) . -2) ((3 0 1) . 6) ((2 2 0) . 1) ((2 1 1) . -8) ((2 0 2) . 9) ((1 2 1) . 4) ((1 1 2) . -12) ((0 3 1) . -2) ((0 2 2) . 12) ((0 1 3) . -18) ((3 0 0) . 2) ((2 1 0) . -4) ((2 0 1) . 12) ((1 2 0) . 2) ((1 1 1) . -12) ((1 0 2) . 18) ((2 0 0) . 1) ((1 1 0) . -2) ((1 0 1) . 6) ((0 2 0) . 1) ((0 1 1) . -6) ((0 0 2) . 9))))

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/sparse"
   (test-case
    "sparse-univariate-normalize"
    (check-equal? (sparse-univariate-normalize '() 3) '())
    (check-equal? (sparse-univariate-normalize '( ((1) . 2) ) 3) '( ((1) . 2/3) ))
    (check-equal? (sparse-univariate-normalize '( ((4) . 2) ((2) . 6) ((2) . 9) ) 3)
                  '( ((4) . 2/3) ((2) . 2) ((2) . 3) )))
   (test-case
    "sparse-univariate-scale"
    (check-equal? (sparse-univariate-scale '() 3) '())
    (check-equal? (sparse-univariate-scale '( ((1) . 2) ) 3) '( ((1) . 6) ))
    (check-equal? (sparse-univariate-scale '( ((4) . 2) ((2) . 6) ((2) . 9) ) 3)
                  '( ((4) . 6) ((2) . 18) ((2) . 27) )))
   (test-case
    "sparse-univariate-constant"
    (check-equal? (sparse-univariate-constant 3) '( ((0) . 3) ))
    (check-equal? sparse-univariate-one '( ((0) . 1) )))
   (test-case
    "sparse-univariate-pseudo-remainder"
    ;; second can not be '()
    (check-equal? (sparse-univariate-pseudo-remainder '() '( ((1) . 3) ))
                  '())
    (check-equal? (sparse-univariate-pseudo-remainder '( ((3) . 2) ) '( ((1) . 3) ))
                  '())
    (check-equal? (sparse-univariate-pseudo-remainder '( ((3) . 6) ) '( ((4) . 3) ))
                  '( ((3) . 6) )))
   (test-case
    "sparse-base-content"
    (check-equal? (sparse-base-content '( ((1) . 3) )) 3)
    (check-equal? (sparse-base-content '( ((1) . 3) ((0) . 2) )) 1)
    (check-equal? (sparse-base-content '( ((1) . 1) ((0) . 2) )) 1)
    (check-equal? (sparse-base-content '( ((3) . 15) ((1) . 35) ((0) . 20) )) 5)
    (check-equal? (sparse-univariate-primitive-part '()) '())
    (check-equal? (sparse-univariate-primitive-part '( ((1) . 3) )) '( ((1) . 1) ))
    (check-equal? (sparse-univariate-primitive-part '( ((3) . 15) ((1) . 35) ((0) . 20) ))
                  '( ((3) . 3) ((1) . 7) ((0) . 4) )))
   (test-case
    "sparse-univariate-gcd"
    (check-equal? (sparse-univariate-gcd '() '()) '())
    (check-equal? (sparse-univariate-gcd '() '( ((1) . 3) )) '( ((1) . 3) ))
    (check-equal? (sparse-univariate-gcd '( ((1) . 3) ) '()) '( ((1) . 3) ))
    (check-equal? (sparse-univariate-gcd '( ((0) . 3) ) '( ((4) . 12) ((0) . 3))) '( ((0) . 3) ))
    (check-equal? (sparse-univariate-gcd '( ((4) . 12) ((0) . 3)) '( ((0) . 3) )) '( ((0) . 3) ))
    (check-equal? (sparse-univariate-gcd '( ((4) . 1) ) '( ((3) . 1) )) '( ((3) . 1) ))
    (check-equal? (sparse-univariate-gcd '( ((4) . 3) ) '( ((3) . 1) )) '( ((3) . 1) ))
    (check-equal? (sparse-univariate-gcd '( ((4) . 1) ) '( ((3) . 3) )) '( ((3) . 1) ))
    (check-equal? (sparse-univariate-gcd '( ((4) . 2) ) '( ((3) . 3) )) '( ((3) . 1) ))
    (check-equal? (sparse-univariate-gcd '( ((4) . 3) ) '( ((3) . 9) )) '( ((3) . 3) ))
    (check-equal? (sparse-univariate-gcd '( ((4) . 3) ((0) . 6) ) '( ((3) . 9) )) '( ((0) . 3) ))
    
    (set!-ugcd-wallp? #t)
    (check-equal? (out->string (sparse-univariate-gcd '( ((4) . 3) ((0) . 6) ) '( ((3) . 9) )))
                  (string-append "'((ppu: (((4) . 1) ((0) . 2))) (ppv: (((3) . 1))))\n"
                                 "'((ppu: (((3) . 1))) (ppv: (((0) . 1))))\n"))
    (set!-ugcd-wallp? #f)
    (set!-ugcd-testing? #t)
    (check-not-exn (λ () (sparse-univariate-gcd '( ((4) . 3) ((0) . 6) ) '( ((3) . 9) ))))
    (set!-ugcd-testing? #f))
   (test-case
    "make-interpolation-args"
    (check-equal? (make-interpolation-args 0) '())
    (check-equal? (make-interpolation-args 1) '(2))
    (check-equal? (make-interpolation-args 2) '(5 3))
    (check-equal? (make-interpolation-args 2) '(11 7))
    (check-equal? (make-interpolation-args 3) '(19 17 13))
    (reset-interpolation-args! '(5 6 2) 'not 'used)
    (check-equal? (make-interpolation-args 3) '(23 19 17)))
   (test-case
    "sparse-multivariate-gcd-helper"
    (random-seed 1)
    (check-equal? (sparse-multivariate-gcd-helper '( ((0 0 0) . 3) ) '( ((0 0 0) . 6) ) 3 '(0 0 0) vector error)
                  (vector '( ((0 0 0) . 3) )))
    (check-equal? (sparse-multivariate-gcd-helper '( ((0 0 2) . 4) ((0 0 1) . -20))
                                                  '( ((0 1 0) . 1) ((0 0 0) . -7))
                                                  3 '(0 2 1) vector error)
                  ;; forces first branch: when '(x 7 5) is used as starting vector
                  #( ( ((0 0 0) . 4) ) ))
    (check-equal? (sparse-multivariate-gcd-helper '(((0 0 0 1 1 0 0 0) . 1) ((1 0 0 0 0 0 0 1) . 1) ((0 0 1 0 0 1 0 0) . 1) ((0 1 0 0 0 0 1 0) . 1))
                                                  '(((0 0 0 0 1 0 0 0) . 1) ((0 0 0 0 0 1 0 0) . 1) ((0 0 0 0 0 0 1 0) . 1) ((0 0 0 0 0 0 0 1) . 1))
                                                  8 '(0 0 0 0 1 1 1 1) vector error)
                  ;; from (simplify (up '(/ (+ (* a d^) (* a^ d) (* b c^) (* b^ c)) (+ a b c d))))
                  ;; forces 4th branch
                  #( (((0 0 0 0 0 0 0 0) . 1)) ))
    (check-equal? (sparse-multivariate-gcd-helper '(((0 0 0 0 0 3 0 0 0) . 1) ((0 0 0 0 0 2 1 0 0) . -1) ((0 0 0 0 0 2 0 1 0) . -1) ((0 0 0 0 0 2 0 0 1) . -1) ((0 0 0 0 0 1 1 1 0) . 1) ((0 0 0 0 0 1 1 0 1) . 1) ((0 0 0 0 0 1 0 1 1) . 1) ((0 0 0 0 0 0 1 1 1) . -1))
                                                  '(((0 0 0 0 0 1 2 0 0) . 1) ((0 0 0 0 0 1 1 1 0) . -1) ((0 0 0 0 0 1 1 0 1) . -1) ((0 0 0 0 0 1 0 1 1) . 1) ((0 0 0 0 0 0 3 0 0) . -1) ((0 0 0 0 0 0 2 1 0) . 1) ((0 0 0 0 0 0 2 0 1) . 1) ((0 0 0 0 0 0 1 1 1) . -1))
                                                  9 '(0 0 0 0 0 1 1 1 1) vector list)
                  ;; from (simplify ((Lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x))
                  ;; forces 3th branch & 5_2th lu-decompose -> win ->lose
                  #( ( ((0 0 0 0 0 1 0 0 0) . 2) ((0 0 0 0 0 0 1 0 0) . -2) ) ))
    (check-equal? (sparse-multivariate-gcd-helper A+ B+ 3 '(6 6 6) vector error)
                  ;; forces 5_2th lu-decompose -> fail branch
                  C+)
    ;; missing test for 5_0th branch
    ;; missing test for 5_1th branch
    ;; missing test for 5_2th lu-decompose -> cont -> univariate-interpolate-values branch
    ;; lose branch
    (set!-sgcd-restart-limit 0)
    (check-equal? (sparse-multivariate-gcd-helper '( ((0 0 2) . 4) ((0 0 1) . -20))
                                                  '( ((0 1 0) . 1) ((0 0 0) . -7))
                                                  3 '(0 2 1) vector (λ () 'fail))
                  'fail)
    (set!-sgcd-restart-limit 200))
   (test-case
    "sparse-multivariate-gcd-helper + wallp / tuning"
    (set!-sgcd-wallp? #t)
    (random-seed 1)
    (check-equal? (out->string (sparse-multivariate-gcd-helper '( ((0 0 0) . 3) ) '( ((0 0 0) . 6) ) 3 '(0 0 0) vector error))
                  (string-append "'(sparse-gcd: (P (((0 0 0) . 3))) (Q (((0 0 0) . 6))) (n 3) (ds (0 0 0)))\n"
                                 "'(restart 0)\n"
                                 "'(restarted (rargs0 (3 2)) (P0 (((0) . 3))) (Q0 (((0) . 6))) (g0 (((0) . 3))))\n"
                                 "'(stage (k 2) (g (((0 0) . 3))) (rargs (2)) (skeleton ((0 0))) (Pk (((0 0 0) . 3))) (Qk (((0 0 0) . 6))) (trial-arglists ((7 5))) (Gks ((((0) . 3)))) (GkSkels (((0)))))\n"
                                 "'(after-lu (xk+1s (11)) (coeffs ((3))))\n"
                                 "'(clp (css ((3))) (cps ()))\n"
                                 "'(clp (css ()) (cps ((((0) . 3)))))\n"
                                 "'(gk (((0 0 0) . 3)))\n"
                                 "'(divide won)\n"))
    (check-not-false (regexp-match #px"'\\(g=zero! 1 \\(7 5\\)\\)"
                                   ;; forces first branch: when '(x 7 5) is used as starting vector
                                   (out->string (sparse-multivariate-gcd-helper '( ((0 0 2) . 4) ((0 0 1) . -20))
                                                                                '( ((0 1 0) . 1) ((0 0 0) . -7))
                                                                                3 '(0 2 1) vector error))))
    (check-not-false (regexp-match #px"'\\(divide lost\\)"
                                   ;; forces 3th branch & 5_2th lu-decompose -> win ->lose
                                   (out->string (sparse-multivariate-gcd-helper '(((0 0 0 0 0 3 0 0 0) . 1) ((0 0 0 0 0 2 1 0 0) . -1) ((0 0 0 0 0 2 0 1 0) . -1) ((0 0 0 0 0 2 0 0 1) . -1) ((0 0 0 0 0 1 1 1 0) . 1) ((0 0 0 0 0 1 1 0 1) . 1) ((0 0 0 0 0 1 0 1 1) . 1) ((0 0 0 0 0 0 1 1 1) . -1))
                                                                                '(((0 0 0 0 0 1 2 0 0) . 1) ((0 0 0 0 0 1 1 1 0) . -1) ((0 0 0 0 0 1 1 0 1) . -1) ((0 0 0 0 0 1 0 1 1) . 1) ((0 0 0 0 0 0 3 0 0) . -1) ((0 0 0 0 0 0 2 1 0) . 1) ((0 0 0 0 0 0 2 0 1) . 1) ((0 0 0 0 0 0 1 1 1) . -1))
                                                                                9 '(0 0 0 0 0 1 1 1 1) vector list))))
    (check-not-false (regexp-match #px"'\\(singular\\)"
                                   ;; forces 5_2th lu-decompose -> fail branch
                                   (out->string (sparse-multivariate-gcd-helper A+ B+ 3 '(6 6 6) vector error))))
    ;; TODO: missing test for 5_0th branch
    ;; TODO: missing test for 5_1th branch
    #;(check-not-false (regexp-match #px"'\\(GkSkels not all the same\\)"
                                     ???))
    ;; TODO: missing test for 5_2th lu-decompose -> cont -> univariate-interpolate-values branch
    #;(check-not-false (regexp-match #px"'\\(Too many GkTerms\\)"
                                     ???))
    (set!-sgcd-wallp? #f)
    
    (set!-sgcd-tuning? #t)
    (random-seed 1)
    (check-equal? (out->string (sparse-multivariate-gcd-helper '( ((0 0 0) . 3) ) '( ((0 0 0) . 6) ) 3 '(0 0 0) vector error))
                  "'(restarts= 0 P= (((0 0 0) . 3)) Q= (((0 0 0) . 6)) G= (((0 0 0) . 3)) n= 3 ds= (0 0 0))\n")
    (set!-sgcd-tuning? #f))

   (test-case
    "sparse-multivariate-gcd"
    (check-equal? (sparse-multivariate-gcd '( ((2 0) . 9) ((0 2) . -9) )
                                           '( ((1 0) . 3) ((0 1) . 3) )
                                           2 '(2 2) vector error)
                  (vector '( ((1 0) . 1) ((0 1) . 1) )))
    (check-equal? (sparse-multivariate-gcd '(((2 0 1) . 1) ((0 2 0) . -1))
                                           '(((1 1 0) . 1))
                                           3 '(2 2 1) vector error)
                  (vector '( ((0 0 0) . 1) )))
    ;; TODO: missing test for "(null? g)" return (is this possible?)
    )
   (test-case
    "sparse-multivariate-gcd"
    (random-seed 1)
    (check-equal? (sparse-heuristic-gcd '( ((2 0) . 9) ((0 2) . -9) )
                                        '( ((1 0) . 3) ((0 1) . 3) )
                                        2 '(2 2) vector (λ () 'fail))
                  'fail)
    (random-seed 1)
    (check-equal? (sparse-heuristic-gcd '(((2 0 1) . 1) ((0 2 0) . -1))
                                        '(((1 1 0) . 1))
                                        3 '(2 2 1) vector (λ () 'fail))
                  'fail)
    (random-seed 6)
    (check-equal? (sparse-heuristic-gcd '(((2 0 1) . 1) ((0 2 0) . -1))
                                        '(((1 1 0) . 1))
                                        3 '(2 2 1) vector (λ () 'fail))
                  #( ( ((0 0 0) . 1) ) ))
    (random-seed 14)
    (check-equal? (sparse-heuristic-gcd '(((2) . 3) ((0) . 4))
                                        '(((1) . 5))
                                        1 '(2) vector (λ () 'fail))
                  #( ( ((0) . 1) ) )))
   (test-case
    "sparse-monomial-gcd"
    (check-equal? (sparse-monomial-gcd '((2 0 3) . 15) '((1 1 1) . 35))
                  '((1 0 1) . 5)))
   (test-case
    "sparse-content"
    (check-equal? (sparse-content '( ((2 5) . 7) )) '((2 5) . 7))
    (check-equal? (sparse-content '( ((0 0) . 1) ((2 5) . 1))) '((0 0) . 1))
    (check-equal? (sparse-content '( ((3 2) . 35) ((2 3) . 21) )) '((2 2) . 7)))
   (test-case
    "sparse-gcd-wrapper"
    (define (win . p) `(win: ,@p))
    (define (lose . p) `(lose: ,@p))
    (define (next p1 p2 . rst) `(next: ,p1 ,p2))
    (check-equal? (sparse-gcd-wrapper '() '( ((2 5) . 7) ) win lose next)
                  '(win: ( ((2 5) . 7) )))
    (check-equal? (sparse-gcd-wrapper '( ((2 5) . 7) ) '() win lose next)
                  '(win: ( ((2 5) . 7) )))
    (check-equal? (sparse-gcd-wrapper '( ((2) . 21) ) '( ((3) . 35) ) win lose next)
                  '(win: ( ((2) . 7) )))
    (check-equal? (sparse-gcd-wrapper '( ((2 5) . 21) ) '( ((2 5) . 21) ) win lose next)
                  '(win: ( ((2 5) . 21) )))
    (check-equal? (sparse-gcd-wrapper '( ((0 0) . 1) ) '( ((2 5) . 21) ) win lose next)
                  '(win: ( ((0 0) . 1) )))
    (check-equal? (sparse-gcd-wrapper '( ((2 5) . 21) ) '( ((0 0) . 1) ) win lose next)
                  '(win: ( ((0 0) . 1) )))
    (check-equal? (sparse-gcd-wrapper '( ((2 5) . 21) ((0 0) . 1))
                                      '( ((5 2) . 15) ((0 0) . 1))
                                      win lose next)
                  `(next: ( ((2 5) . 21) ((0 0) . 1))  ( ((5 2) . 15) ((0 0) . 1) )))
    (check-equal? (sparse-gcd-wrapper '( ((2 5) . 21) ((0 0) . 14))
                                      '( ((5 2) . 15) ((0 0) . 1))
                                      win lose next)
                  `(next: ( ((2 5) . 3) ((0 0) . 2))  ( ((5 2) . 15) ((0 0) . 1) )))
    (check-equal? (sparse-gcd-wrapper '( ((2 5) . 21) ((0 0) . 1))
                                      '( ((5 2) . 15) ((0 0) . 5))
                                      win lose next)
                  `(next: ( ((2 5) . 21) ((0 0) . 1))  ( ((5 2) . 3) ((0 0) . 1) )))
    (check-equal? (sparse-gcd-wrapper '( ((2 5) . 21) ((0 0) . 18))
                                      '( ((5 2) . 15) ((0 0) . 5))
                                      win lose next)
                  `(next: ( ((2 5) . 7) ((0 0) . 6))  ( ((5 2) . 3) ((0 0) . 1) )))
    (check-equal? (sparse-gcd-wrapper '( ((2 5) . 21) )
                                      '( ((5 2) . 15) )
                                      win lose next)
                  `(next: ( ((0 0) . 1) ) ( ((0 0) . 1) ))))
   (test-case
    "sparse-gcd"
    (define (win . p) `(win: ,@p))
    (define (lose . p) `(lose: ,@p))
    ;; solved in wrapper
    (check-equal? (sparse-gcd '() '( ((2 5) . 7) ) win lose)
                  '(win: ( ((2 5) . 7) )))
    (check-equal? (sparse-gcd '( ((2 5) . 7) ) '() win lose)
                  '(win: ( ((2 5) . 7) )))
    (check-equal? (sparse-gcd '( ((2) . 21) ) '( ((3) . 35) ) win lose)
                  '(win: ( ((2) . 7) )))
    (check-equal? (sparse-gcd '( ((2 5) . 21) ) '( ((2 5) . 21) ) win lose)
                  '(win: ( ((2 5) . 21) )))
    (check-equal? (sparse-gcd '( ((0 0) . 1) ) '( ((2 5) . 21) ) win lose)
                  '(win: ( ((0 0) . 1) )))
    (check-equal? (sparse-gcd '( ((2 5) . 21) ) '( ((0 0) . 1) ) win lose)
                  '(win: ( ((0 0) . 1) )))
    ;; solved in further-work
    (check-equal? (sparse-gcd '( ((2 5) . 21) )
                              '( ((5 2) . 15) )
                              win lose)
                  ;; all-zeros?
                  `(win: ( ((2 2) . 3) ) ))
    (random-seed 1) ;; win via heuristics
    (check-equal? (sparse-gcd '(((2 5) . 15) ((5 5) . 18))
                              '(((5 2) . 6) ((4 4) . 6))
                              win lose)
                  '(win: ( ((2 2) . 3) ) ))
    (parameterize ([*heuristic-sparse-gcd-enabled* #f])
      (random-seed 1) ;; skip heuristics
      (check-equal? (sparse-gcd '(((2 5) . 15) ((5 5) . 18))
                                '(((5 2) .  6) ((4 4) . 6))
                                win lose)
                    '(win: ( ((2 2) . 3) ) )))
    (random-seed 3) ;; fail heuristics
    (check-equal? (sparse-gcd '(((2 5) . 15) ((5 5) . 18))
                              '(((5 2) .  6) ((4 4) . 6))
                              win lose)
                  `(win: ( ((2 2) . 3) ) )))
   (test-case
    "poly/gcd/sparse"
    (define (win . p) `(win: ,@p))
    (define (lose . p) `(lose: ,@p))
    (check-equal? (poly/gcd/sparse '( ((1) . 5.0) ) '( ((1) . 5) ) win lose)
                  '(win: ( ((0) . 1) ) ))
    (check-equal? (poly/gcd/sparse '( ((1) . 5) ) '( ((1) . a) ) win lose)
                  '(win: ( ((0) . 1) ) ))
    (check-equal? (poly/gcd/sparse '(((2 5) . 15) ((5 5) . 18))
                                   '(((5 2) .  6) ((4 4) . 6))
                                   win lose)
                  `(win: ( ((2 2) . 3) ) ))
    (check-exn #px"Unequal arities--poly/gcd/sparse"
               (λ () (poly/gcd/sparse '( ((0 1) . 2) ) '( ((1) . 2) ) win lose))))
   (test-case
    "poly/gcd-sparse"
    (check-equal? (poly/gcd-sparse (pcf:expression-> '(+ (* 4 x) (* 2 y x x)) (λ (p v) p))
                                   (pcf:expression-> '(+ (* 4 x x) (* 2 y y x)) (λ (p v) p)))
                  (pcf:expression-> '(+ (* 2 x) (* 0 y)) (λ (p v) p)))
    (check-exn #px"Unequal arities -- poly:gcd"
               (λ () (poly/gcd-sparse (pcf:expression-> '(+ (* 4 x) (* 2 x x)) (λ (p v) p))
                                      (pcf:expression-> '(+ (* 4 x x) (* 2 y y x)) (λ (p v) p)))))
    (set!-sgcd-restart-limit 0)
    (random-seed 1141274609)
    #;(check-equal? (poly/gcd/sparse '(((4 2 2 4 6 4 3 1 5) . 118) ((6 3 0 4 1 3 6 3 0) . 106) ((0 6 1 5 6 2 4 2 0) . 372) ((1 6 4 2 1 5 0 3 2) . -310) ((4 1 0 6 3 1 4 3 0) . 206))
                                   '(((1 0 2 6 2 5 5 5 3) . -455) ((3 6 0 1 4 3 0 3 3) . 462) ((2 2 6 0 4 2 0 5 0) . 353))
                                   (λ _ #t) (λ _ #f))
                  #f)
    (set!-sgcd-restart-limit 200))
   ;**************************************************************************************************
   (check-equal? (sparse-univariate-gcd
                  '(((8) . 1) ((6) . 1) ((4) . -3) ((3) . -3) ((2) . 8) ((1) . 2) ((0) . -5))
                  '(((6) . 3) ((4) . 5) ((2) . -4) ((1) . -9) ((0) . 21)))
                 '(((0) . 1)))
   ;;Test repaired by gjs on 16 Aug 2021
   (test-case
    "1"
    
    (define d1 '(+ (expt x1 2) x1 3))
    (define f1 '(+ (* 2 (expt x1 2)) (* 2 x1) 1))
    (define g1 '(+ (expt x1 2) (* 2 x1) 2))
    (check-true (gcd-test d1 f1 g1)))
   (test-case
    "2"
    (define d2
      '(+ (* 2 (expt x1 2) (expt x2 2))
          (* x1 x2)
          (* 2 x1)))
    (define f2
      '(+ (expt x2 2)
          (* 2 (expt x1 2) x2)
          (expt x1 2)
          1))
    (define g2
      '(+ (* (expt x1 2) (expt x2 2))
          (* (expt x1 2) x2)
          (* x1 x2)
          (expt x1 2)
          x1))
    (check-true (gcd-test d2 f2 g2)))
   (test-case
    "3"
    (define d3
      '(+ (* x2 x2 x3 x3)
          (* x2 x2 x3)
          (* 2 x1 x1 x2 x3)
          (* x1 x3)))
    (define f3
      '(+ (* x3 x3)
          (* x2 x2 x3)
          (* x1 x1 x2 x3)
          (* x1 x3)
          (* x1 x1 x2 x2)))
    (define g3
      '(+ (* x2 x3)
          (* 2 x1 x3)
          x3
          x1))
    (check-true (gcd-test d3 f3 g3)))
   (test-case
    "4"
    (define d4
      '(+ (* x1 x1 x4 x4)
          (* x2 x2 x3 x4)
          (* x1 x1 x2 x4)
          (* x2 x4)
          (* x1 x1 x2 x3)))
    (define f4
      '(+ (* x1 x2 x3 x3 x4 x4)
          (* x1 x3 x3 x4 x4)
          (* x1 x4 x4)
          (* x4 x4)
          (* x1 x3 x4)))
    (define g4
      '(+ (* x1 x3 x3 x4 x4)
          (* x3 x3 x4 x4)
          (* x4 x4)
          (* x1 x2 x2 x3 x4)
          (* x1 x2 x2)))
    (check-true (gcd-test d4 f4 g4)))
   (test-case
    "5"
    (define d5
      '(+ (* x1 x1 x1 x2 x2 x3 x3 x4 x5 x5)
          (* x1 x2 x2 x5 x5)
          (* x1 x1 x1 x3 x4 x4 x5)
          (* x1 x1 x1 x2 x3 x3 x4 x5)
          (* x1 x1 x2 x3 x3 x4 x4)))
    (define f5
      '(+ (* x1 x2 x2 x5 x5)
          (* x1 x2 x3 x3 x4 x5)
          (* x1 x2 x3 x3 x4 x4)
          (* x1 x2 x2 x4 x4)
          1))
    (define g5
      '(+ (* x1 x3 x3 x4 x5 x5)
          (* x2 x5 x5)
          (* x1 x2 x4 x5)
          (* x2 x5)
          (* x1 x2 x3 x4 x4)))
    (check-true (gcd-test d5 f5 g5)))
   (test-case
    "6"
    (define d6
      '(+ (* x1 x2 x4 x4 x5 x5 x6 x6)
          (* x1 x2 x2 x3 x3 x4 x5 x5 x6 x6)
          (* x1 x1 x3 x6 x6)
          (* x1 x1 x2 x3 x3 x4 x5 x5 x6)
          (* x1 x1 x3 x5 x6)))
    (define f6
      '(+ (* x1 x1 x2 x4 x5 x5 x6 x6)
          (* x1 x3 x5 x5 x6 x6)
          (* x1 x2 x2 x6 x6)
          (* x1 x1 x2 x2 x3 x3 x5 x6)
          (* x1 x3 x3 x4 x5)))
    (define g6
      '(+ (* x2 x2 x3 x3 x4 x5 x5 x6)
          (* x1 x4 x4 x5 x6)
          (* x2 x2 x3 x3 x4 x5 x6)
          (* x1 x2 x2 x3 x4 x4 x6)
          (* x1 x1 x3 x5 x5)))
    (check-true (gcd-test d6 f6 g6)))
   (test-case
    "7"
    (define d7
      '(+ (* x1 x2 x2 x4 x4 x6 x6 x7 x7)
          (* x1 x1 x3 x4 x6 x6 x7 x7)
          (* x3 x3 x4 x4 x7 x7)
          (* x1 x1 x2 x4 x4 x6)
          (* x3 x4 x5 x5)))
    (define f7
      '(+ (* x1 x1 x2 x4 x4 x5 x6 x6 x7 x7)
          (* x1 x2 x3 x6 x7)
          (* x3 x4 x4 x5 x5 x7)
          (* x1 x1 x2 x3 x4 x4 x5 x6)))
    (define g7
      '(+ (* x1 x3 x5 x6 x6 x7 x7)
          (* x2 x2 x3 x3 x4 x4 x5 x6 x7 x7)
          (* x4 x6 x7 x7)
          (* x1 x1 x2 x3 x5 x6 x7)
          (* x1 x1 x3 x3 x4 x5 x5)))
    (check-true (gcd-test d7 f7 g7)))
   (test-case
    "8"
    (define d8
      '(+ (* x2 x2 x4 x5 x6 x7 x8 x8)
          (* x1 x1 x2 x3 x3 x4 x4 x6 x6 x7 x7 x8)
          (* x1 x1 x3 x4 x4 x6 x6 x7 x7)
          (* x1 x1 x2 x2 x3 x3 x4 x5 x5 x6 x7 x7)
          (* x2 x2 x4 x6)))
    (define f8
      '(+ (* x1 x1 x2 x2 x3 x4 x4 x5 x6 x6 x8 x8)
          (* x2 x5 x6 x6 x8 x8)
          (* x1 x1 x2 x2 x3 x3 x4 x4 x6 x6 x7 x7 x8)
          (* x1 x1 x3 x3 x4 x5 x5 x7 x7 x8)
          (* x1 x2 x2 x3 x3 x5 x5 x7)))
    (define g8
      '(+ (* x1 x4 x4 x6 x6 x7 x8 x8)
          (* x1 x2 x2 x4 x4 x5 x5 x6 x6 x8)
          (* x1 x1 x2 x3 x4 x4 x6 x6 x8)
          (* x1 x1 x2 x2 x3 x3 x4 x5 x5 x8)
          (* x1 x2 x4 x4 x5 x5)))
    (check-true (gcd-test d8 f8 g8)))
   (test-case
    "10"
    (define d10
      '(+ (* x1 x2 x2 x4 x4 x8 x9 x9 x10 x10)
          (* x2 x2 x4 x5 x5 x6 x7 x9 x10 x10)
          (* x1 x1 x2 x3 x5 x5 x7 x7 x9 x9)
          (* x1 x3 x3 x4 x4 x7 x7 x9 x9)
          (* x1 x1 x3 x4 x7 x7 x8 x8)))
    (define f10
      '(+ (* x1 x2 x3 x3 x4 x6 x7 x8 x9 x9 x10 x10)
          (* x2 x2 x3 x3 x4 x4 x6 x6 x9 x10 x10)
          (* x1 x2 x2 x3 x3 x4 x5 x6 x7 x8 x8 x9 x9 x10)
          (* x1 x1 x2 x4 x4 x5 x5 x8 x8 x9 x9 x10)
          (* x3 x4 x4 x5 x6 x7 x7 x9 x10)))
    (define g10
      '(+ (* x1 x2 x2 x3 x3 x5 x5 x6 x6 x7 x8 x9 x9 x10 x10)
          (* x3 x8 x9 x9 x10 x10)
          (* x1 x2 x2 x3 x4 x5 x5 x6 x6 x8 x8 x9 x10)
          (* x1 x3 x6 x7 x8 x10)
          (* x4 x4 x5 x5 x6 x6 x7 x9 x9)))
    (check-true (gcd-test d10 f10 g10)))
   (test-case
    "10a"
    (define d10a
      '(+ (* 2 x1 x2 x2 x4 x4 x8 x9 x9 x10 x10)
          (* 3 x2 x2 x4 x5 x5 x6 x7 x9 x10 x10)
          (* 4 x1 x1 x2 x3 x5 x5 x7 x7 x9 x9)
          (* 5 x1 x3 x3 x4 x4 x7 x7 x9 x9)
          (* 6 x1 x1 x3 x4 x7 x7 x8 x8)
          7))
    (define f10a
      '(+ (* 8 x1 x2 x3 x3 x4 x6 x7 x8 x9 x9 x10 x10)
          (* 9 x2 x2 x3 x3 x4 x4 x6 x6 x9 x10 x10)
          (* 10 x1 x2 x2 x3 x3 x4 x5 x6 x7 x8 x8 x9 x9 x10)
          (* 11 x1 x1 x2 x4 x4 x5 x5 x8 x8 x9 x9 x10)
          (* 12 x3 x4 x4 x5 x6 x7 x7 x9 x10)
          13))
    (define g10a
      '(+ (* 14 x1 x2 x2 x3 x3 x5 x5 x6 x6 x7 x8 x9 x9 x10 x10)
          (* 15 x3 x8 x9 x9 x10 x10)
          (* 16 x1 x2 x2 x3 x4 x5 x5 x6 x6 x8 x8 x9 x10)
          (* 17 x1 x3 x6 x7 x8 x10)
          (* 18 x4 x4 x5 x5 x6 x6 x7 x9 x9)
          19))
    (check-true (gcd-test d10a f10a g10a)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))