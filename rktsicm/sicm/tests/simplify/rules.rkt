#lang racket/base

(require rackunit
         (for-syntax racket/base racket/syntax)
         racket/list
         "../../simplify/rules.rkt"
         "../../general/notes.rkt"
         (only-in "../../kernel.rkt" :pi) ;; we need the scmutils-base-environment loaded
         (only-in (submod "../../general/memoize.rkt" ALL) *memoizers* memoizer-fun memoizer-info)
         "../helper.rkt")

(define (check-setter setter var)
  (check-exn #px"argument must be a boolean.:"
             (λ () (setter 'wrong)))
  (define cur (var))
  (define M (car (hash-values (cdr *memoizers*))))
  ((memoizer-fun M) 1)
  (check-false (equal? (take ((memoizer-info M)) 2) '(0 0)))
  (setter #f) (check-false (var))
  (check-equal? (take ((memoizer-info M)) 2) '(0 0))
  (setter #t) (check-true  (var))
  (setter (var)))

(define (check-predicate pred true false)
  (for ([t (in-list true )]) (check-true  (pred t) (format "(~a ~a)" (object-name pred) t)))
  (for ([f (in-list false)]) (check-false (pred f) (format "(~a ~a)" (object-name pred) f))))

(define-syntax-rule (check-notes notes rst ...) (check-equal? (out->string (clear-notes!)(show-notes)) (format notes rst ...)))
(define-syntax-rule (no-new-notes) (check-notes "\n#| |#"))

(define (check-settings vec)
  (check-equal? (vector log-exp-simplify?
                        exponent-product-simplify?
                        ^1/2->sqrt?
                        sqrt-factor-simplify?
                        aggressive-atan-simplify?
                        inverse-simplify?
                        sin-cos-simplify?
                        half-angle-simplify?
                        ignore-zero?
                        commute-partials?
                        divide-numbers-through-simplify?
                        trig-product-to-sum-simplify?
                        (symbol? :pi))
                vec))
(define startup-settings (vector #t #t #t #t #t #t #t #t #t #t #t #t #f))

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/rule-simplifier"
   (test-case
    "setters"
    (check-setter log-exp-simplify                (λ () log-exp-simplify?))
    (check-setter exponent-product-simplify       (λ () exponent-product-simplify?))
    (check-setter ^1/2->sqrt                      (λ () ^1/2->sqrt?))
    (check-setter sqrt-factor-simplify            (λ () sqrt-factor-simplify?))
    (check-setter aggressive-atan-simplify        (λ () aggressive-atan-simplify?))
    (check-setter inverse-simplify                (λ () inverse-simplify?))
    (check-setter sin-cos-simplify                (λ () sin-cos-simplify?))
    (check-setter half-angle-simplify             (λ () half-angle-simplify?))
    (check-setter ignore-zero-simplify            (λ () ignore-zero?))
    (check-setter commute-partials-simplify       (λ () commute-partials?))
    (check-setter divide-numbers-through-simplify (λ () divide-numbers-through-simplify?))
    (check-setter trig-product-to-sum-simplify    (λ () trig-product-to-sum-simplify?)))
   (test-case
    "predicates"
    (check-predicate negative-number?       '(-2.4 -3) '(x -3-5i 4 0))
    (check-predicate complex-number?        '(+2+4i +2.5+4.1i) '(x 0.+0.i 3))
    (check-predicate imaginary-number?      '(+4i +4.5i) '(x +3+5i +0+0.i))
    (check-predicate imaginary-integer?     '(+4i) '(#;!!!-> +4.0i x +1+4i +4.1i 3))
    (check-predicate non-integer?           '(x 4.2 +4i) '(3 4.0))
    (check-predicate even-integer?          '(2. 4 -2) '(x 3 2.1 2+2i))
    (check-predicate odd-integer?           '(3. 5 -3) '(x 2 3.1 3+5i))
    (check-predicate even-positive-integer? '(2 4) '(x -2 3 2.1 2+2i)) ;;TODO: fails with 2. should it?
    (check-predicate odd-positive-integer?  '(3 5) '(x -3 4 3.1 3+3i)) ;;TODO: fails with 5. should it?
    (check-predicate exact-integer>3?       '(4 5) '(x 3 5.0 -5 5.1 5+5i))
    (check-predicate at-least-two?          '(2 5 9.4) '(x 1 -7  3+3i -8.))
    (check-predicate more-than-two?         '(5 9.4) '(x 2 -7  3+3i -8.)))
   (test-case
    "universal-reductions"
    (check-settings startup-settings)
    (skip
     ;;TODO;; this depends on :pi being a symbol? (or not) => fix units/constans -> symbolic-constants
     )
    )
   (test-case
    "non-negative-factors"
    (check-settings startup-settings)
    (check-false  (non-negative-factors 3 -5 'id))
    (check-equal? (non-negative-factors 3  5 'id) 'OK)
    (check-equal? (non-negative-factors 3 'x 'id) 'noted)
    (check-notes "\n#| \n'(assuming (non-negative? x))\n'(id)\n|#")
    (check-equal? (non-negative-factors 'y 3 'id) 'OK)
    (check-notes "\n#| \n'(assuming (non-negative? y))\n'(id)\n|#")
    (no-new-notes))
   (test-case
    "list<"
    (check-settings startup-settings)
    (check-true  (list< '() '(1)))
    (check-true  (list< '(1) '(4)))
    (check-true  (list< '(1 3) '(1 4)))
    (check-false (list< '(4) '(1)))
    (check-false (list< '(1) '(1)))
    (check-false (list< '(1) '())))
   (test-case
    "pi-predicates"
    (check-settings startup-settings)
    (check-predicate zero-mod-pi?   '(:pi (* 4 :pi)) '(:2pi (* 1/2 :pi) 3 x)) ;;TODO: :2pi should be translated
    (check-predicate pi/2-mod-2pi?  '((* 1/2 :pi) (* -7/2 :pi)) '(0 :pi (* -1/2 :pi) 3 x)) ;;TODO: :pi/2 should be translated
    (check-predicate -pi/2-mod-2pi? '((* -1/2 :pi) (* 7/2 :pi)) '(0 :pi (*  1/2 :pi) 3 x))
    (check-predicate pi/2-mod-pi?   '((* -1/2 :pi) (* -7/2 :pi)) '(0 :pi 3 x))
    (check-predicate zero-mod-2pi?  '((* 6 :pi) 0) '(:pi x 3 (* 1/2 :pi)))
    (check-predicate pi-mod-2pi?    '(:pi (* 7 :pi)) '(0 (* 2 :pi) x 3 (* 1/2 :pi)))
    (check-predicate pi/4-mod-pi?   '((* 1/4 :pi) (* -3/4 :pi)) '(0 :pi (* 7/4 :pi) 3 x))
    (check-predicate -pi/4-mod-pi?  '((* -1/4 :pi) (* 7/4 :pi)) '(0 :pi (* -3/4 :pi) 3 x)))
   (test-case
    "half-angle-formula"
    (check-settings startup-settings)
    ;;TODO;; can we improve this? (the assume! part)
    (check-equal? (sin-half-angle-formula 'theta)
                  '(sqrt (/ (- 1 (cos theta)) 2)))
    (check-notes "\n#| \n'(assuming (non-negative? (+ (* 2 :pi) (* -1 theta) (* 4 :pi (floor (/ theta (* 4 :pi)))))))\n'(sin-half-angle-formula)\n|#")
    (check-equal? (cos-half-angle-formula 'theta)
                  '(sqrt (/ (+ 1 (cos theta)) 2)))
    (check-notes "\n#| \n'(assuming (non-negative? (+ :pi theta (* 4 :pi (floor (/ (- :pi theta) (* 4 :pi)))))))\n'(cos-half-angle-formula)\n|#")
    (no-new-notes))
   (test-case
    "flush-obvious-ones"
    (check-settings startup-settings)
    (check-equal? (sincos-flush-ones `(+ (expt (sin x) 7) x c (expt (cos x) 5)))
                  '(+ (* (expt (sin x) 2) (expt (sin x) 1) (expt (sin x) 2) (expt (sin x) 2))
                      (* (expt (cos x) 2) (expt (cos x) 1) (expt (cos x) 2))
                      x c))
    (check-equal? (sincos-flush-ones `(+ (expt (sin x) 2) x c (expt (cos x) 2)))
                  '(+ 1 x c))
    (no-new-notes))
   (test-case
    "simplify-and-canonicalize"
    (check-settings startup-settings)
    (check-equal? ((simplify-and-canonicalize (λ (x) x) error) 'expr)
                  ;; if the new expr is the same don't canonicalize
                  'expr)
    (check-equal? ((simplify-and-canonicalize (λ (x) 4) (λ (x) 5)) 'expr)
                  ;; if the new expr is different: canonicalize
                  5))
   (test-case
    "simplify-until-stable"
    (check-settings startup-settings)
    ;; keep applying the rules and canon until answer is the same
    (define (idd x) x)
    (define (rev x) (cons (car x) (reverse (cdr x))))
    (define (srt x) (cons (car x) (sort (cdr x) <)))
    (check-equal? ((simplify-until-stable idd error) 'expr) 'expr)
    (check-equal? ((simplify-until-stable rev (λ (x) '(+ 5 4 8 3 4 2 9))) '(+ 5 4 8 3 4 2 9))
                  '(+ 5 4 8 3 4 2 9))
    (check-equal? ((simplify-until-stable rev idd) '(+ 5 4 8 3 4 2 9))
                  '(+ 9 2 4 3 8 4 5))
    (check-equal? ((simplify-until-stable rev srt) '(+ 5 4 8 3 4 2 9))
                  '(+ 2 3 4 4 5 8 9))
    (check-equal? ((simplify-until-stable (λ (x) (if (null? (cdr x)) x (cons (car x) (cddr x))))
                                          rev) '(+ 5 4 8 3 4 2 9))
                  '(+)))
   (test-case
    "only-if"
    (check-settings startup-settings)
    (check-equal? ((only-if number? exp) 0) 1)
    (check-equal? ((only-if number? exp) 'x) 'x))
   (test-case
    "clear-square-roots-of-perfect-squares"
    (check-settings startup-settings)
    (check-equal? (clear-square-roots-of-perfect-squares '(/ (sqrt (+ (expt a 2) (* 2 a b) (expt b 2)))
                                                             (+ a b)))
                  1)
    (check-notes "\n#| \n'(assuming (non-negative? (+ a b)))\n'(root-out-squares)\n|#")
    (sqrt-expt-simplify #f)
    (check-equal? (clear-square-roots-of-perfect-squares '(/ (sqrt (+ (expt a 2) (* 2 a b) (expt b 2)))
                                                             (+ a b)))
                  '(/ (sqrt (+ (expt a 2) (* 2 a b) (expt b 2))) (+ a b)))
    (sqrt-expt-simplify #t)
    (no-new-notes))

   (test-case
    "trig-exand"
    (check-settings startup-settings)
    ;; not sure if these are good tests...
    (define expr '(+ (magnitude (expt x 4)) (sin (+ (exp (log x)) y)) (expt (sin z) 2) (expt (cos (+ x y)) 3) (expt (cos z) 2)))
    (define ans1 '(+ 1 (* (expt x 2) (expt (conjugate x) 2)) (expt (cos (+ x y)) 3) (sin (+ x y))))
    (define ans2 '(/ (+ 4
                       (* (expt (cos x) 3) (expt (cos y) 3))
                       (* -3 (expt (cos x) 3) (cos y) (expt (sin y) 2))
                       (* -9 (expt (cos x) 2) (sin x) (expt (cos y) 2) (sin y))
                       (* 3 (expt (cos x) 2) (sin x) (expt (sin y) 3))
                       (* -3 (cos x) (expt (sin x) 2) (expt (cos y) 3))
                       (* 9 (cos x) (expt (sin x) 2) (cos y) (expt (sin y) 2))
                       (* 3 (expt (sin x) 3) (expt (cos y) 2) (sin y))
                       (* -1 (expt (sin x) 3) (expt (sin y) 3))
                       (* 3 (cos x) (cos y))
                       (* -3 (sin x) (sin y))
                       (* 4 (cos y) (sin (exp (log x))))
                       (* 4 (sin y) (cos (exp (log x))))
                       (* 4 (sqrt (* (expt x 4) (expt (conjugate x) 4)))))
                    4))
    (check-equal? (trigexpand expr) ans2)
    (no-new-notes)
    (check-equal? (trigcontract expr) ans2)
    (no-new-notes)
    ;; TODO this test fails when run with the tester in BC
    ;;      if run in drracket or individually it works correctly
    ;;      find out what is the state influencing the test
    (check-equal? (full-simplify expr)
                  '(/ (+ (* 8 (expt (conjugate x) 2) (expt x 2))
                         (* (+ (* (+ (* (+ (* (+ (* (+ (* 5 (cos x)) (* 0-3i (sin x))) (cos x))
                                                 (* -3 (expt (sin x) 2)))
                                              (cos x))
                                           (* 0-3i (expt (sin x) 3)))
                                        (cos y))
                                     (* (+ (* (+ (* (+ (* 0-3i (cos x)) (* -21 (sin x))) (cos x))
                                                 (* 0-3i (expt (sin x) 2)))
                                              (cos x))
                                           (* 3 (expt (sin x) 3)))
                                        (sin y)))
                                  (cos y))
                               (* (+ (* (+ (* (+ (* -3 (cos x)) (* 0-3i (sin x))) (cos x))
                                           (* 21 (expt (sin x) 2)))
                                        (cos x))
                                     (* 0-3i (expt (sin x) 3)))
                                  (expt (sin y) 2))
                               (* (+ (* (+ (* 3 (cos x)) (* 0+3i (sin x))) (cos x)) (* 3 (expt (sin x) 2)))
                                  (cos x))
                               (* (+ 8 (* 0+3i (expt (sin x) 2))) (sin x)))
                            (cos y))
                         8
                         (* (+ (* (+ (* (+ (* (+ (* 0-3i (cos x)) (* 3 (sin x))) (cos x))
                                           (* 0-3i (expt (sin x) 2)))
                                        (cos x))
                                     (* -5 (expt (sin x) 3)))
                                  (expt (sin y) 2))
                               (* (+ (* (+ (* 0+3i (cos x)) (* -3 (sin x))) (cos x))
                                     8
                                     (* 0+3i (expt (sin x) 2)))
                                  (cos x))
                               (* -3 (expt (sin x) 3)))
                            (sin y)))
                      8))
    (check-notes "\n#| \n'(assuming (= (sqrt (expt x 4)) (expt x 2)))\n'(simsqrt1)\n\n'(assuming (= (sqrt (expt (conjugate x) 4)) (expt (conjugate x) 2)))\n'(simsqrt1)\n\n'(assuming (non-negative? (expt x 4)))\n'(e1)\n\n'(assuming (non-negative? (expt (conjugate x) 4)))\n'(e1)\n|#")
    (check-equal? (oe-simplify expr) ans1)
    (check-notes "\n#| \n'(assuming (= (sqrt (expt (conjugate x) 4)) (expt (conjugate x) 2)))\n'(simsqrt1)\n\n'(assuming (= (sqrt (expt x 4)) (expt x 2)))\n'(simsqrt1)\n\n'(assuming (non-negative? (expt (conjugate x) 4)))\n'(e1)\n\n'(assuming (non-negative? (expt x 4)))\n'(e1)\n|#")
    (check-equal? (easy-simplify expr) ans1)
    (check-notes "\n#| \n'(assuming (= (sqrt (expt (conjugate x) 4)) (expt (conjugate x) 2)))\n'(simsqrt1)\n\n'(assuming (= (sqrt (expt x 4)) (expt x 2)))\n'(simsqrt1)\n\n'(assuming (non-negative? (expt (conjugate x) 4)))\n'(e1)\n\n'(assuming (non-negative? (expt x 4)))\n'(e1)\n|#")
    (check-equal? (new-simplify expr) ans1)
    (check-notes "\n#| \n'(assuming (= (sqrt (expt (conjugate x) 4)) (expt (conjugate x) 2)))\n'(simsqrt1)\n\n'(assuming (= (sqrt (expt x 4)) (expt x 2)))\n'(simsqrt1)\n\n'(assuming (non-negative? (expt (conjugate x) 4)))\n'(e1)\n\n'(assuming (non-negative? (expt x 4)))\n'(e1)\n|#"))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))