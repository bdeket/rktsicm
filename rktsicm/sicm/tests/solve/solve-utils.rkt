#lang s-exp "../../generic.rkt"

(require rackunit)

(require "../../solve.rkt"
         "../../general/list-utils.rkt"
         "../helper.rkt")

(define-syntax-rule (hypothetical x) (make-hypothetical 'x #f))

(define Es (list (make-equation '(+ x 3) '(A))))
(define Vs (list 'x))
(define Ss (list (make-substitution 'y '(+ x 3) '(B))))
(define Ts (list (make-equation '(* z 4) '(C))))
(define (sol #:E [E Es] #:V [V Vs] #:S [S Ss] #:T [T Ts])
  (make-solution E V S T))

;***************************************************************************************************
;* from solve-utils.rkt                                                                            *
;***************************************************************************************************
;;; Examples of use
(provide the-tests)
(define the-tests
  (test-suite
   "solve/solve-utils"
   ;; ************ predicates and accumulators ************
   (test-case
    "initialize-solver"
    (define (all) (list (*complete-solutions*) (*underdetermined-solutions*) (*with-residual-equations*) (*with-tough-equations*) (*with-extra-equations*)))
    (check-equal? (all) '(()()()()()))
    (*complete-solutions* 1) (*underdetermined-solutions* 2) (*with-residual-equations* 3) (*with-tough-equations* 4) (*with-extra-equations* 5)
    (check-equal? (all) '(1 2 3 4 5))
    (initialize-solver)
    (check-equal? (all) '(()()()()())))
   (test-case
    "accumulators"
    (define (all) (list (*complete-solutions*) (*underdetermined-solutions*) (*with-residual-equations*) (*with-tough-equations*) (*with-extra-equations*)))
    (define-syntax-rule (parm body ...)
      (parameterize ([*complete-solutions* '()][*underdetermined-solutions* '()][*with-residual-equations* '()][*with-tough-equations* '()][*with-extra-equations* '()])
        body ...))
    (define-syntax-rule (pprm name (a ...) (b ...))
      (parm (name S1)
            (check-equal? (all) (list a ... (list S1) b ...))
            (name S1)
            (check-equal? (all) (list a ... (list S1) b ...))
            (name S2)
            (check-equal? (all) (list a ... (list S2 S1) b ...))))
    (check-equal? (all) '(()()()()()))
    (parm (check-equal? (all) '(()()()()())))
    (define S1 (sol))
    (define S2 (sol #:V '(z)))
    (pprm accumulate-complete-solutions () ('() '() '() '()))
    (pprm accumulate-underdetermined-solutions ('()) ('() '() '()))
    (pprm accumulate-residual-equation-solutions ('() '()) ('() '()))
    (pprm accumulate-tough-equations-solutions ('() '() '()) ('()))
    (pprm accumulate-extra-equations-solutions ('() '() '() '()) ())
    (check-equal? (all) '(()()()()())))
   (test-case
    "same-solution?"
    (define S1 (sol))
    (check-true (same-solution? S1 (sol #:E (list (make-equation '(+ 3 x) '(A)))
                                        #:S (list (make-substitution 'y '(+ 3 x) '(B)))
                                        #:T (list (make-equation '(* z 4.) '(C))))))
    (check-false (same-solution? S1 (sol #:E (list (make-equation '(+ 3 x) '(D))))))
    (check-false (same-solution? S1 (sol #:E (list (make-equation '(+ 4 x) '(A))))))
    (check-false (same-solution? S1 (sol #:V (list 'z))))
    (check-false (same-solution? S1 (sol #:S (list (make-substitution 'y '(+ 4 x) '(B))))))
    (check-false (same-solution? S1 (sol #:S (list (make-substitution 'y '(+ 3 x) '(D))))))
    (check-false (same-solution? S1 (sol #:T (list (make-equation '(* z 4.) '(D))))))
    (check-false (same-solution? S1 (sol #:T (list (make-equation '(* z 5.) '(C)))))))
   (test-case
    "same-residual-equations?"
    (define (subsol X) (make-solution X '() '() '()))
    (define E1 (make-equation '(+ x 3) '(A)))
    (define E2 (make-equation '(+ x 3) '(B)))
    (define E3 (make-equation '(+ x 3.) '(A)))
    (define E4 (make-equation '(+ y 3) '(A)))
    (define E5 (make-equation '(* z 2) '(C)))
    (check-true (same-residual-equations? (subsol (list E1)) (subsol (list E3))))
    (check-true (same-residual-equations? (subsol (list E1 E5)) (subsol (list E5 E3))))
    (check-false (same-residual-equations? (subsol (list E1)) (subsol (list E2))))
    (check-false (same-residual-equations? (subsol (list E1)) (subsol (list E4))))
    (check-false (same-residual-equations? (subsol (list E1)) (subsol (list E1 E5)))))
   (test-case
    "same-residual-variables?"
    (define (subsol X) (make-solution '() X '() '()))
    (check-true (same-residual-variables? (subsol (list 'x 'y 'z)) (subsol (list 'x 'z 'y))))
    (check-false (same-residual-variables? (subsol (list 'x 'y 'z)) (subsol (list 'x 'y))))
    (check-false (same-residual-variables? (subsol (list 'x 'y 'z)) (subsol (list 'x 'w 'z)))))
   (test-case
    "same-substitutions?"
    (define S1 (make-substitution 'x 3 '(A)))
    (define S2 (make-substitution 'x 3 '(B)))
    (define S3 (make-substitution 'z '(+ y 3) '(A)))
    (define S4 (make-substitution 'z '(+ y 3.) '(A)))
    (define S5 (make-substitution 'y 3 '(A)))
    (define (subsol X) (make-solution '() '() X '()))
    (check-true (same-substitutions? (subsol (list S1 S3 S5)) (subsol (list S1 S4 S5))))
    (check-false (same-substitutions? (subsol (list S1 S3 S5)) (subsol (list S2 S4 S5))))
    (check-false (same-substitutions? (subsol (list S1 S4)) (subsol (list S2 S5))))
    (check-false (same-substitutions? (subsol (list S1 S4)) (subsol (list S2)))))
   (test-case
    "same-tough-equations?"
    (define (subsol X) (make-solution '() '() '() X))
    (define E1 (make-equation '(+ x 3) '(A)))
    (define E2 (make-equation '(+ x 3) '(B)))
    (define E3 (make-equation '(+ x 3.) '(A)))
    (define E4 (make-equation '(+ y 3) '(A)))
    (define E5 (make-equation '(* z 2) '(C)))
    (check-true (same-tough-equations? (subsol (list E1)) (subsol (list E3))))
    (check-true (same-tough-equations? (subsol (list E1 E5)) (subsol (list E5 E3))))
    (check-false (same-tough-equations? (subsol (list E1)) (subsol (list E2))))
    (check-false (same-tough-equations? (subsol (list E1)) (subsol (list E4))))
    (check-false (same-tough-equations? (subsol (list E1)) (subsol (list E1 E5)))))
   (test-case
    "same-equation?"
    (check-true (same-equation? (make-equation '(+ x 3) '(A))
                                (make-equation '(+ x 3.) '(A))))
    (check-false (same-equation? (make-equation '(+ x 3) '(A))
                                 (make-equation '(+ x 3.) '(B))))
    (check-false (same-equation? (make-equation '(+ x 3) '(A))
                                 (make-equation '(+ y 3) '(A)))))
   (test-case
    "same-expression?"
    (check-true (same-expression? '(+ x 3) '(+ x 3.)))
    (check-true (same-expression? '(+ x 3) '(/ (+ (* 3 x) 9.) 3)))
    (check-false (same-expression? '(+ x 3) '(+ y 3.)))
    (check-false (same-expression? '(+ x 3) '(+ x 2.9))))
   (test-case
    "same-variable?"
    (check-true  (same-variable? 'x 'x))
    (check-false (same-variable? 'x 'y))
    (check-false (same-variable? 'x "x")))
   (test-case
    "same-substitution?"
    (check-true (same-substitution? (make-substitution 'x 3 '(A))
                                    (make-substitution 'x 3 '(A))))
    (check-true (same-substitution? (make-substitution 'x '(+ y 3) '(A))
                                    (make-substitution 'x '(+ y 3.) '(A))))
    (check-false (same-substitution? (make-substitution 'x 3 '(A))
                                     (make-substitution 'x 3 '(B))))
    (check-false (same-substitution? (make-substitution 'x 3 '(A))
                                     (make-substitution 'y 3 '(A))))
    (check-false (same-substitution? (make-substitution 'x 3 '(A))
                                     (make-substitution 'x 4 '(A)))))
   (test-case
    "same-justifications?"
    (check-true (same-justifications? '(A B) '(B A)))
    (check-false (same-justifications? '(A B) '(A)))
    (check-false (same-justifications? '(A B) '(A B C))))
   (test-case
    "same-justification?"
    (check-true (same-justification? 'A 'A))
    (check-false (same-justification? 'A 'B)))
   (test-case
    "equivalent-solution?"
    (define S1 (sol))
    (check-true (equivalent-solutions? S1 (sol #:E (list (make-equation '(+ 3 x) '(A)))
                                        #:S (list (make-substitution 'y '(+ 3 x) '(B)))
                                        #:T (list (make-equation '(* z 4.) '(C))))))
    (check-false (equivalent-solutions? S1 (sol #:E (list (make-equation '(+ 3 x) '(D))))))
    (check-false (equivalent-solutions? S1 (sol #:E (list (make-equation '(+ 4 x) '(A))))))
    (check-false (equivalent-solutions? S1 (sol #:V (list 'z))))
    (check-false (equivalent-solutions? S1 (sol #:S (list (make-substitution 'y '(+ 4 x) '(B))))))
    (check-true  (equivalent-solutions? S1 (sol #:S (list (make-substitution 'y '(+ 3 x) '(D))))))
    (check-false (equivalent-solutions? S1 (sol #:T (list (make-equation '(* z 4.) '(D))))))
    (check-false (equivalent-solutions? S1 (sol #:T (list (make-equation '(* z 5.) '(C)))))))
   (test-case
    "equivalent substitutions?"
    (define S1 (make-substitution 'x 3 '(A)))
    (define S2 (make-substitution 'x 3 '(B)))
    (define S3 (make-substitution 'z '(+ y 3) '(A)))
    (define S4 (make-substitution 'z '(+ y 3.) '(B)))
    (define S5 (make-substitution 'y 3 '(A)))
    (define (subsol X) (make-solution '() '() X '()))
    (check-true (equivalent-substitutions? (subsol (list S1 S3 S5)) (subsol (list S2 S4 S5))))
    (check-false (equivalent-substitutions? (subsol (list S1 S4)) (subsol (list S2 S5))))
    (check-false (equivalent-substitutions? (subsol (list S1 S4)) (subsol (list S2)))))
   (test-case
    "equivalent-substitution?"
    (check-true (equivalent-substitution? (make-substitution 'x 3 '(A))
                                          (make-substitution 'x 3 '(B))))
    (check-true (equivalent-substitution? (make-substitution 'x '(+ y 3) '(A))
                                          (make-substitution 'x '(+ y 3.) '(B))))
    (check-false (equivalent-substitution? (make-substitution 'x 3 '(A))
                                           (make-substitution 'y 3 '(A))))
    (check-false (equivalent-substitution? (make-substitution 'x 3 '(A))
                                           (make-substitution 'x 4 '(A)))))
   (test-case
    "one-of-each"
    (check-equal? (one-of-each '()) '())
    (check-equal? (one-of-each '(())) '())
    (check-equal? (one-of-each '((1 2 3))) '((1) (2) (3)))
    (check-equal? (one-of-each '((1 2) (3 4))) '((1 3) (2 3) (1 4) (2 4)))
    (check-equal? (one-of-each '((1 2) (3) (4))) '((1 3 4) (2 3 4)))
    (check-equal? (one-of-each '((1 2) (3) (4 5))) '((1 3 4) (2 3 4) (1 3 5) (2 3 5))))
   (test-case
    "minimum-length-head"
    (check-equal? (minimum-length-head '()) '())
    (check-equal? (minimum-length-head '(1)) '(1))
    (check-equal? (minimum-length-head '((1) (2))) '((2) (1)))
    (check-equal? (minimum-length-head '((1) (2 3))) '((1)))
    (check-equal? (minimum-length-head '((1) (2 3) (4))) '((1)))
    (check-equal? (minimum-length-head '((1) (4) (2 3))) '((4) (1))))
   (test-case
    "substitution-variable-entry"
    (check-equal? (substitution-variable-entry 'x (make-solution '() '() '() '())) #f)
    (check-equal? (substitution-variable-entry 'x
                                               (make-solution '() '() (list (make-substitution 'x 3 '(A))) '()))
                  (make-substitution 'x 3 '(A)))
    (check-equal? (substitution-variable-entry 'x
                                               (make-solution '() '() (list (make-substitution 'y 3 '(A))) '()))
                   #f))
   ;; ************ actual functionality ************
   (test-case
    "collect-best-solutions"
    (define S1 (sol))
    (check-equal? (collect-best-solutions '()) '())
    (check-equal? (collect-best-solutions (list S1)) (list S1))
    (check-equal? (collect-best-solutions (list S1 S1)) (list S1))
    (define S2 (sol #:E '() #:V '() #:S (cons (make-substitution 'x -3 '(A)) (substitutions S1))))
    (check-equal? (collect-best-solutions (list S1 S2)) (list S1 S2))
    (check-equal? (collect-best-solutions (list S2 S1)) (list S2 S1))
    (define S3 (sol #:E (list (make-equation '(- y 0) '(B)))
                    #:V '(y)
                    #:S (list (make-substitution 'x -3 '(A)))))
    (check-equal? (collect-best-solutions (list S1 S3)) (list S1 S3))
    (check-equal? (collect-best-solutions (list S3 S1)) (list S3 S1))
    (define S4 (sol #:S (list (make-substitution 'y '(+ x 3.) '(D)))))
    (check-equal? (collect-best-solutions (list S1 S4)) (list (sol #:S (list (make-substitution 'y '(+ x 3) '(D)))) S1))
    (check-equal? (collect-best-solutions (list S4 S1)) (list (sol #:S (list (make-substitution 'y '(+ x 3.) '(B)))) S4)))
   (test-case
    "default-fail"
    (define S1 (sol))
    (define-syntax-rule (parm body ...)
      (parameterize ([*complete-solutions* '()][*underdetermined-solutions* '()][*with-residual-equations* '()][*with-tough-equations* '()][*with-extra-equations* '()][*outstanding-contradictions* '()])
        body ...))
    (parm (accumulate-complete-solutions S1)
          (check-equal? (default-fail) (cons 'full-solutions (list S1))))
    (parm (accumulate-underdetermined-solutions S1)
          (check-equal? (default-fail) (cons 'underdetermined (list S1))))
    (parm (accumulate-residual-equation-solutions S1)
          (check-equal? (default-fail) (cons 'parameters-constrained (list S1))))
    (parm (*outstanding-contradictions* 'bad)
          (check-equal? (default-fail) (cons 'contradictions 'bad)))
    (parm (accumulate-tough-equations-solutions S1)
          (check-equal? (default-fail) (cons 'tough-equations (list S1))))
    (parm (accumulate-extra-equations-solutions S1)
          (check-equal? (default-fail) (cons 'extra-equations (list S1))))
    (parm (check-exn #px"How did I get here?"
                     (λ () (default-fail)))))
   (test-case
    "default-succeed"
    (define (fail) 'go-to-next)
    (define (all) (list (*complete-solutions*) (*underdetermined-solutions*) (*with-residual-equations*) (*with-tough-equations*) (*with-extra-equations*)))
    (define-syntax-rule (parm body ...)
      (parameterize ([*complete-solutions* '()][*underdetermined-solutions* '()][*with-residual-equations* '()][*with-tough-equations* '()][*with-extra-equations* '()][*outstanding-contradictions* '()])
        body ...))
    ;; real solution
    (parm (define S (sol #:E '() #:V '() #:T '()))
          (check-equal? (default-succeed S fail) 'go-to-next)
          (check-equal? (all) (list (list S) '() '() '() '())))
    (parm (define S (sol #:E '() #:V '() #:T '()
                         #:S (list (make-substitution 'y '(+ 3 z) '(A))
                                   (make-substitution 'x '(+ 4 z) '(B)))))
          (define S* (sol #:E '() #:V '() #:T '()
                         #:S (list (make-substitution 'x '(+ 4 z) '(B))
                                   (make-substitution 'y '(+ 3 z) '(A)))))
          ;; succeed sorts substitutions
          (check-equal? (default-succeed S fail) 'go-to-next)
          (check-equal? (all) (list (list S*) '() '() '() '())))
    ;; underdetermined solution
    (parm (define S (sol #:E '() #:V '(z) #:T '()))
          (check-equal? (default-succeed S fail) 'go-to-next)
          (check-equal? (all) (list '() (list S) '() '() '())))
    ;; tough equation
    (parm (define S (sol #:E '() #:V '(z)))
          (check-equal? (default-succeed S fail) 'go-to-next)
          (check-equal? (all) (list '() '() '() (list S) '())))
    ;; residual equation
    (parm (define S (sol #:V '() #:T '()))
          (check-equal? (default-succeed S fail) 'go-to-next)
          (check-equal? (all) (list '() '() (list S) '() '())))
    ;; extra equation
    (parm (define S (sol #:V '() #:E '()))
          (check-equal? (default-succeed S fail) 'go-to-next)
          (check-equal? (all) (list '() '() '() '() (list S))))
    ;; rest
    (parm (check-exn #px"How did I get here?" (λ () (default-succeed (sol) fail))))
    (parm (check-exn #px"How did I get here?" (λ () (default-succeed (sol #:V '()) fail)))))
   (test-case
    "test-solver"
    ;;TODO; remove this export?
    (check-equal? (out->string (test-solver (list (make-equation '(+ (* 3 x) 5) '(A))) '(x)))
                  "#|\n(full-solutions (() () (((= x -5/3) (A))) ()))\n|#\n"))
   (test-case
    "solve-equations"
    (define (all) (list (*complete-solutions*) (*underdetermined-solutions*) (*with-residual-equations*) (*with-tough-equations*) (*with-extra-equations*)))
    (parameterize ([*complete-solutions*        'very]
                   [*underdetermined-solutions* 'very]
                   [*with-residual-equations*   'very]
                   [*with-tough-equations*      'very]
                   [*with-extra-equations*      'bad])
      (check-equal? (all) '(very very very very bad))
      (check-equal? (solve-equations (list (make-equation '(+ x 3) '(A))) '(x))
                    '(full-solutions (() () (((= x -3) (A))) ())))
      (check-equal? (all) '(very very very very bad))))
   ;*************************************************************************************************
   (check-equal?
    (solve-equations
     (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(x y))
    '(full-solutions (() () (((= x 2) (A B)) ((= y 1) (A B))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+  x   y   z  1)  (list 'A))
           (make-equation '(+  x   y      2)  (list 'B))
           (make-equation '(+  x          1)  (list 'C)))
     '(x y z))
    '(full-solutions (() () (((= x -1) (C)) ((= y -1) (B C)) ((= z 1) (A B))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x)     y  -5)  (list 'B)))
     '(x y))
    '(contradictions (-2 (A B) ()) (2 (B A) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(-  3 (+ x y))                       (list 'A))
           (make-equation '(-  5 (- x y))                       (list 'B))
           (make-equation '(-  3 (+ (* (sqrt x) z) (square y))) (list 'C)))
     '(x y z))
    '(full-solutions (() () (((= x 4) (A B)) ((= y -1) (A B)) ((= z 1) (A B C))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+ (* (+ a b) (- a c)) c) (list 'A))
           (make-equation '(- 3 (+ a b))             (list 'B)))
     '(a b c))
    '(underdetermined (() (c) (((= a (* 2/3 c)) (A B)) ((= b (+ 3 (* -2/3 c))) (A B))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
           (make-equation '(- 3 (- a c))  (list 'B)))
     '(a b c))
    '(underdetermined (() (c) (((= a (+ 3 c)) (B)) ((= b (+ -3 (* -4/3 c))) (A B))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
           (make-equation '(- 3 (- a b))  (list 'B)))
     '(a b c))
    '(underdetermined
      (()
       (a)
       (((= b (+ -3 a)) (A B))
        ((= c (/ (+ (* 2 (expt a 2)) (* -3 a)) (+ -4 (* 2 a)))) (A B)))
       ())
      (()
       (b)
       (((= a (+ 3 b)) (B))
        ((= c (/ (+ 9 (* 2 (expt b 2)) (* 9 b)) (+ 2 (* 2 b)))) (A B)))
       ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
           (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(x y z))
    '(full-solutions (() () (((= x 2) (A B)) ((= y 1) (A B)) ((= z -1/2) (A B C))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(- 200/3 (/ 1 (+ (/ 1 R1) (/ 1 R2))))  (list 'A))
           (make-equation '(-  1/3 (/ R2 (+ R1 R2)))  (list 'B)))
     '(R1 R2))
    `(full-solutions
      (()
       ()
       (((= R1 0) (A B ,(hypothetical (- quadratic -6 600 0))))
        ((= R2 0) (A B ,(hypothetical (- quadratic -6 600 0)))))
       ())
      (()
       ()
       (((= R1 200) (A B ,(hypothetical (+ quadratic -6 600 0))))
        ((= R2 100) (A B ,(hypothetical (+ quadratic -6 600 0)))))
       ())))

   (check (λ (x y) (lset= equal? x y))
          (solve-equations
           (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
                 (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
           '(R1 R2))
          `(full-solutions
            (()
             ()
             (((= R1 0) (A B ,(hypothetical (- quadratic -6 600 0))))
              ((= R2 0) (A B ,(hypothetical (- quadratic -6 600 0)))))
             ())
            (()
             ()
             (((= R1 0) (A B ,(hypothetical (- quadratic -2 200 0))))
              ((= R2 0) (A B ,(hypothetical (- quadratic -6 600 0)))))
             ())
            (()
             ()
             (((= R1 0) (A B ,(hypothetical (- quadratic -6 600 0))))
              ((= R2 0) (A B ,(hypothetical (- quadratic -2 200 0)))))
             ())
            (()
             ()
             (((= R1 0) (A B ,(hypothetical (- quadratic -2 200 0))))
              ((= R2 0) (A B ,(hypothetical (- quadratic -2 200 0)))))
             ())
            (()
             ()
             (((= R1 200) (A B ,(hypothetical (+ quadratic -2 200 0))))
              ((= R2 100) (A B ,(hypothetical (+ quadratic -2 200 0)))))
             ())
            (()
             ()
             (((= R1 200) (A B ,(hypothetical (+ quadratic -6 600 0))))
              ((= R2 100) (A B ,(hypothetical (+ quadratic -2 200 0)))))
             ())
            (()
             ()
             (((= R1 200) (A B ,(hypothetical (+ quadratic -2 200 0))))
              ((= R2 100) (A B ,(hypothetical (+ quadratic -6 600 0)))))
             ())
            (()
             ()
             (((= R1 200) (A B ,(hypothetical (+ quadratic -6 600 0))))
              ((= R2 100) (A B ,(hypothetical (+ quadratic -6 600 0)))))
             ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(- (expt x 2) 1)  (list 'A))
           (make-equation '(- x 1)  (list 'B)))
     '(x))
    '(full-solutions (() () (((= x 1) (B))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(- (expt x 2) 1)  (list 'A))
           (make-equation '(- x -1)  (list 'B)))
     '(x))
    '(full-solutions (() () (((= x -1) (B))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
           (make-equation '(- (expt y 2) 9) (list 'B))
           (make-equation '(- (- y x) 1) (list 'C)))
     '(x y))
    `(full-solutions
      (()
       ()
       (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
        ((= y 3) (B ,(hypothetical (- quadratic 1 0 -9)))))
       ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
           (make-equation '(- (expt y 2) 9) (list 'B))
           (make-equation '(- (- y x) 2) (list 'C)))
     '(x y))
    `(contradictions
      (7 (B C A ,(hypothetical (- quadratic 1 -9 20))) ())
      (16 (B C A ,(hypothetical (+ quadratic 1 -9 20))) ())
      (2 (C A B ,(hypothetical (- quadratic 1 0 -9))) ())
      (56 (C A B ,(hypothetical (+ quadratic 1 0 -9))) ())
      (-1
       (,(hypothetical (- quadratic 1 0 -9)) B
                                             C
                                             A
                                             ,(hypothetical (- quadratic 1 -5 6)))
       ())
      (-2
       (,(hypothetical (- quadratic 1 0 -9)) B
                                             C
                                             A
                                             ,(hypothetical (+ quadratic 1 -5 6)))
       ())
      (2 (A C B ,(hypothetical (- quadratic 1 0 -9))) ())
      (2 (A C B ,(hypothetical (- quadratic 1 0 -9))) ())
      (-7
       (,(hypothetical (+ quadratic 1 0 -9)) B
                                             C
                                             A
                                             ,(hypothetical (- quadratic 1 -5 6)))
       ())
      (-8
       (,(hypothetical (+ quadratic 1 0 -9)) B
                                             C
                                             A
                                             ,(hypothetical (+ quadratic 1 -5 6)))
       ())
      (56 (A C B ,(hypothetical (+ quadratic 1 0 -9))) ())
      (56 (A C B ,(hypothetical (+ quadratic 1 0 -9))) ())
      (-1
       (,(hypothetical (- quadratic 1 -5 6)) A
                                             C
                                             B
                                             ,(hypothetical (- quadratic 1 0 -9)))
       ())
      (-7
       (,(hypothetical (- quadratic 1 -5 6)) A
                                             C
                                             B
                                             ,(hypothetical (+ quadratic 1 0 -9)))
       ())
      (7 (B C A ,(hypothetical (- quadratic 1 -5 6))) ())
      (-1
       (,(hypothetical (- quadratic 1 -5 6)) A
                                             C
                                             B
                                             ,(hypothetical (- quadratic 1 0 -9)))
       ())
      (-7
       (,(hypothetical (- quadratic 1 -5 6)) A
                                             C
                                             B
                                             ,(hypothetical (+ quadratic 1 0 -9)))
       ())
      (7 (B C A ,(hypothetical (- quadratic 1 -5 6))) ())
      (-2
       (,(hypothetical (+ quadratic 1 -5 6)) A
                                             C
                                             B
                                             ,(hypothetical (- quadratic 1 0 -9)))
       ())
      (-8
       (,(hypothetical (+ quadratic 1 -5 6)) A
                                             C
                                             B
                                             ,(hypothetical (+ quadratic 1 0 -9)))
       ())
      (16 (B C A ,(hypothetical (+ quadratic 1 -5 6))) ())
      (-2
       (,(hypothetical (+ quadratic 1 -5 6)) A
                                             C
                                             B
                                             ,(hypothetical (- quadratic 1 0 -9)))
       ())
      (-8
       (,(hypothetical (+ quadratic 1 -5 6)) A
                                             C
                                             B
                                             ,(hypothetical (+ quadratic 1 0 -9)))
       ())
      (16 (B C A ,(hypothetical (+ quadratic 1 -5 6))) ())))


   (check (λ (x y) (lset= equal? x y))
          (solve-equations
           (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
                 (make-equation '(- (expt y 2) z) (list 'B))
                 (make-equation '(- (- y x) 2) (list 'C)))
           '(x y))
          `(parameters-constrained
            ((((+ 16 (* -1 z)) (B C A ,(hypothetical (- quadratic 1 -5 6))) (z)))
             ()
             (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
              ((= y 4) (A C ,(hypothetical (- quadratic 1 -5 6)))))
             ())
            ((((+ 16 (* -1 z)) (B C A ,(hypothetical (- quadratic 1 -9 20))) (z)))
             ()
             (((= x 2) (A C ,(hypothetical (- quadratic 1 -9 20))))
              ((= y 4) (A C ,(hypothetical (- quadratic 1 -9 20)))))
             ())
            ((((+ 20 z (* 9 (sqrt z))) (A C B ,(hypothetical (- quadratic 1 0 (* -1 z))))
                                       (z)))
             ()
             (((= x (+ -2 (* -1 (sqrt z))))
               (B C ,(hypothetical (- quadratic 1 0 (* -1 z)))))
              ((= y (* -1 (sqrt z))) (B ,(hypothetical (- quadratic 1 0 (* -1 z))))))
             ())
            ((((+ 20 z (* -9 (sqrt z))) (A C B ,(hypothetical (+ quadratic 1 0 (* -1 z))))
                                        (z)))
             ()
             (((= x (+ -2 (sqrt z))) (B C ,(hypothetical (+ quadratic 1 0 (* -1 z)))))
              ((= y (sqrt z)) (B ,(hypothetical (+ quadratic 1 0 (* -1 z))))))
             ())
            ((((+ 25 (* -1 z)) (B C A ,(hypothetical (+ quadratic 1 -9 20))) (z)))
             ()
             (((= x 3) (A C ,(hypothetical (+ quadratic 1 -9 20))))
              ((= y 5) (A C ,(hypothetical (+ quadratic 1 -9 20)))))
             ())
            ((((+ 25 (* -1 z)) (B C A ,(hypothetical (+ quadratic 1 -5 6))) (z)))
             ()
             (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))
              ((= y 5) (A C ,(hypothetical (+ quadratic 1 -5 6)))))
             ())))

   (check (λ (x y) (lset= equal? x y))
          (solve-equations
           (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
                 (make-equation '(- (expt y 2) z) (list 'B))
                 (make-equation '(- (- y x) 2) (list 'C)))
           '(x y z))
          `(full-solutions
            (()
             ()
             (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
              ((= y 4) (A C ,(hypothetical (- quadratic 1 -9 20))))
              ((= z 16) (A B C ,(hypothetical (- quadratic 1 -9 20)))))
             ())
            (()
             ()
             (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
              ((= y 4) (A C ,(hypothetical (- quadratic 1 -5 6))))
              ((= z 16) (A B C ,(hypothetical (- quadratic 1 -9 20)))))
             ())
            (()
             ()
             (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
              ((= y 4) (A C ,(hypothetical (- quadratic 1 -9 20))))
              ((= z 16) (A B C ,(hypothetical (- quadratic 1 -5 6)))))
             ())
            (()
             ()
             (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
              ((= y 4) (A C ,(hypothetical (- quadratic 1 -5 6))))
              ((= z 16) (A B C ,(hypothetical (- quadratic 1 -5 6)))))
             ())
            (()
             ()
             (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))
              ((= y 5) (A C ,(hypothetical (+ quadratic 1 -5 6))))
              ((= z 25) (A B C ,(hypothetical (+ quadratic 1 -5 6)))))
             ())
            (()
             ()
             (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))
              ((= y 5) (A C ,(hypothetical (+ quadratic 1 -9 20))))
              ((= z 25) (A B C ,(hypothetical (+ quadratic 1 -5 6)))))
             ())
            (()
             ()
             (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))
              ((= y 5) (A C ,(hypothetical (+ quadratic 1 -5 6))))
              ((= z 25) (A B C ,(hypothetical (+ quadratic 1 -9 20)))))
             ())
            (()
             ()
             (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))
              ((= y 5) (A C ,(hypothetical (+ quadratic 1 -9 20))))
              ((= z 25) (A B C ,(hypothetical (+ quadratic 1 -9 20)))))
             ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A)))
     '(x))
    `(full-solutions
      (() () (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))) ())
      (() () (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
           (make-equation '(+ (expt x 2) (* -7 x) 10)  (list 'B)))
     '(x))
    `(full-solutions
      (() () (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))) ())
      (() () (((= x 2) (B ,(hypothetical (- quadratic 1 -7 10))))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
           (make-equation '(+ (expt x 2) (* a x) 10)  (list 'B)))
     '(a x))
    `(full-solutions
      (()
       ()
       (((= a -7) (A B ,(hypothetical (- quadratic 1 -5 6))))
        ((= x 2) (A ,(hypothetical (- quadratic 1 -5 6)))))
       ())
      (()
       ()
       (((= a -19/3) (A B ,(hypothetical (+ quadratic 1 -5 6))))
        ((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6)))))
       ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(- 2 (sqrt (+ x 1)))  (list 'A)))
     '(x))
    `(full-solutions (() () (((= x 3) (A))) ())))

   (check-equal?
    (solve-equations
     (list (make-equation '(- 2 (acos (sqrt (+ x 1))))  (list 'A)))
     '(x))
    `(full-solutions (() () (((= x (* -1 (expt (sin 2) 2))) (A))) ())))

   (check-within
    (solve-equations
     (list (make-equation '(+ 1 x (square x))  (list 'A)))
     '(x))
    `(full-solutions
      (()
       ()
       (((= x -.5000000000000001+.8660254037844387i)
         (A ,(hypothetical (- quadratic 1 1 1)))))
       ())
      (()
       ()
       (((= x -1/2-.8660254037844386i)
         (A ,(hypothetical (+ quadratic 1 1 1)))))
       ()))
    5e-16)


   (check-within
    (solve-equations
     (list (make-equation '(+ 1 x (square x))  (list 'A))
           (make-equation '(+ (square y) 3) (list 'B))
           (make-equation '(- (* 2 x) (-  y 1)) (list 'C)))
     '(x y))
    `(full-solutions
      (()
       ()
       (((= x -1/2+.8660254037844388i) (A ,(hypothetical (- quadratic 1 1 1))))
        ((= y +1.7320508075688776i) (B ,(hypothetical (- quadratic 1 0 3)))))
       ())
      (()
       ()
       (((= x -1/2-.8660254037844386i) (A ,(hypothetical (+ quadratic 1 1 1))))
        ((= y -1.7320508075688772i) (B ,(hypothetical (+ quadratic 1 0 3)))))
       ()))
    5e-16)
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))
