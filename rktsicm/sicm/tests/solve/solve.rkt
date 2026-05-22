#lang s-exp "../../generic.rkt"

(require rackunit
         racket/port)

;some examples litered troughout the files of calculus/*
;kernel
(require "../../solve.rkt"
         "../../general/list-utils.rkt"
         "../helper.rkt")

(define-syntax-rule (hypothetical x) (make-hypothetical 'x #f))
;***************************************************************************************************
;* from solve.rkt                                                                                  *
;***************************************************************************************************
;;; For example...

(provide the-tests)
(define the-tests
  (test-suite
   "solve/solve"
   (test-case
    "make-solution"
    (define S (make-solution 'req 'rvar 'subs 'tough))
    (check-equal? (residual-equations S) 'req)
    (check-equal? (residual-variables S) 'rvar)
    (check-equal? (substitutions S) 'subs)
    (check-equal? (tough-equations S) 'tough))
   (test-case
    "premises"
    (check-equal? hypothetical-memory (make-hash))
    (define Q (make-hash))
    (check-equal? (hash-table-intern! Q 'key (λ () 'get-value)) 'get-value)
    (check-equal? Q (make-hash '((key . get-value))))
    ;; TODO not sure whate the hypothetical structure is for, but its field don't seem to be used
    (check-equal? (make-root-premise 'A) (make-hypothetical 'A #f))
    (define A 'A)
    (check-false (root-premise? A))
    (check-false (root-premise-opposite A))
    (define AA (make-root-premise-pair 'A))
    (check-equal? (root-premise? (car AA)) #t)
    (check-equal? (root-premise? (cadr AA)) #t)
    (check-equal? (root-premise-opposite (car AA)) (cadr AA))
    (check-equal? (root-premise-opposite (cadr AA)) (car AA)))
   (test-case
    "make-substitution"
    (define S (make-substitution 'x '(+ 3 y) '(A)))
    (check-equal? (substitution-variable S) 'x)
    (check-equal? (substitution-expression S) '(+ 3 y))
    (check-equal? (substitution-justifications S) '(A))
    (define AA (make-root-premise-pair 'A))
    (check-not-exn (λ () (make-substitution 'x '(+ 3 y) (list (car AA)))))
    (check-exn #px"Aargh-subst" ;; TODO this error message can be better
               (λ () (make-substitution 'x '(+ 3 y) (list (car AA) 'B (cadr AA))))))
   (test-case
    "make-equation"
    (define E (make-equation '(+ 3 x) '(A)))
    (check-equal? (equation-expression E) '(+ 3 x))
    (check-equal? (equation-justifications E) '(A))
    (check-equal? (equation-variables E) '(x))
    (check-equal? (equation-expression (make-equation '(+ x 3 x) '(A))) '(+ 3 (* 2 x)))
    (define AA (make-root-premise-pair 'A))
    (check-not-exn (λ () (make-equation '(+ 3 y) (list (car AA)))))
    (check-exn #px"Aargh-eqn" ;; TODO this error message can be better
               (λ () (make-equation '(+ 3 y) (list (car AA) 'B (cadr AA))))))
   (test-case
    "make-contradiction"
    (define C (make-contradiction '(+ 3 y) '(A B) '(y)))
    (check-equal? (contradiction-expression C) '(+ 3 y))
    (check-equal? (contradiction-justifications C) '(A B))
    (check-equal? (contradiction-variables C) '(y)))
   (test-case
    "occurs?"
    (check-true (occurs? 'x 'x))
    (check-true (occurs? 'x '(+ 3 (* 2 x))))
    (check-false (occurs? 'y '(+ 3 (* 2 x)))))
   (test-case
    "variable-present?"
    (check-not-false  ((variable-present? 'x) (make-equation '(+ 3 x) '(A))))
    (check-false ((variable-present? 'y) (make-equation '(+ 3 x) '(A)))))
   (test-case
    "fewer-variables?"
    (check-true  (fewer-variables? (make-equation '(+ x 3 x) '(A)) (make-equation '(+ x 3 y) '(A))))
    (check-false (fewer-variables? (make-equation '(+ x 3 y) '(A)) (make-equation '(+ x 3 x) '(A)))))
   (test-case
    "correct-substitutions?"
    (check-true (correct-substitutions? (list (make-equation '(+ x      y  3) '(A))
                                              (make-equation '(+ x (* 2 y) 5) '(B)))
                                        (list (make-substitution 'x -1 '(A))
                                              (make-substitution 'y -2 '(B)))))
    (check-false (correct-substitutions? (list (make-equation '(+ x      y  3) '(A))
                                               (make-equation '(+ x (* 2 y) 5) '(B)))
                                         (list (make-substitution 'x 'x '(A))
                                               (make-substitution 'y -2 '(B)))))
    (check-false (correct-substitutions? (list (make-equation '(+ x      y  3) '(A))
                                               (make-equation '(+ x (* 2 y) 5) '(B)))
                                         (list (make-substitution 'x -4 '(A))
                                               (make-substitution 'y -2 '(B))))))
   (test-case
    "contradiction-failure"
    (parameterize ([*outstanding-contradictions* '(first-contradiction)])
      (check-equal? (contradiction-failure '(a-contradiction) (λ () (*outstanding-contradictions*)))
                    '(a-contradiction first-contradiction))))
   (test-case
    "use-new-substitution"
    (define (fail) (*outstanding-contradictions*))
    (check-equal? (use-new-substitution (make-substitution 'x '(+ y 3) '(A))
                                        (list (make-equation '(+ 3 x) '(B)))
                                        (list (make-substitution 'g '(+ 4  x) '(C)))
                                        (list (make-equation '(+ 3 d) '(D)))
                                        vector fail)
                  (vector (list (make-equation '(+ 3 (+ y 3)) '(B A)))
                          (list (make-substitution 'x '(+ y 3) '(A))
                                (make-substitution 'g '(+ 4 (+ y 3)) '(C A)))
                          (list (make-equation '(+ 3 d) '(D)))
                          fail))
    (check-equal? (use-new-substitution (make-substitution 'x '(+ y 3) '(A))
                                        (list (make-equation '(+ 3 x) '(B))
                                              (make-equation '(- 3 (- x y)) '(D)))
                                        (list (make-substitution 'g '(+ 4  x) '(C)))
                                        (list (make-equation '(- 3 (- x y)) '(D)))
                                        vector fail)
                  (vector (list (make-equation '(+ 3 (+ y 3)) '(B A)))
                          (list (make-substitution 'x '(+ y 3) '(A))
                                (make-substitution 'g '(+ 4 (+ y 3)) '(C A)))
                          (list )
                          fail))
    (parameterize ([*outstanding-contradictions* '()])
      (check-equal? (use-new-substitution (make-substitution 'x '(+ y 3) '(A))
                                          (list (make-equation '(+ 3 x) '(B)))
                                          (list (make-substitution 'g '(+ 4  x) '(C)))
                                          (list (make-equation '(- x y) '(D)))
                                          vector fail)
                    (list (make-equation 3 '(D A))))))
   (test-case
    "contradictory-equation?"
    (check-false (contradictory-equation? (make-equation '(+ 4 x) '(A))))
    (check-false (contradictory-equation? (make-equation '(* x 0) '(A))))
    (check-false (contradictory-equation? (make-equation 0 '(A))))
    (check-true  (contradictory-equation? (make-equation 3 '(A))))
    (skip ;; TODO this should flag a contradiction
     (check-true  (contradictory-equation? (make-equation '(+ 3 (cos 3)) '(A))))))
   (test-case
    "flush-tautologies"
    (check-equal? (flush-tautologies '()) '())
    (check-equal? (flush-tautologies (list (make-equation '(* 0 x) '(A)))) '())
    (check-equal? (flush-tautologies (list (make-equation '(* 4 x) '(A))))
                  (list (make-equation '(* 4 x) '(A))))
    (check-equal? (flush-tautologies (list (make-equation '(* 4 x) '(A))
                                           (make-equation 0 '(B))
                                           (make-equation '(* 0 x) '(C))))
                  (list (make-equation '(* 4 x) '(A)))))
   (test-case
    "next-equations"
    (check-equal? (next-equations (make-substitution 'x '(+ y 3) '(A))
                                  (list (make-equation '(+ (* x z) 3) '(B))
                                        (make-equation '(+ (* x x) y) '(C))))
                  (list (make-equation '(+ (* (+ 3 y) z) 3) '(B A))
                        (make-equation '(+ (expt (+ 3 y) 2) y) '(C A))))
    (define AA (make-root-premise-pair 'A))
    (check-equal? (next-equations (make-substitution 'x '(+ y 3) (list (car AA)))
                                  (list (make-equation '(+ (* x z) 3) '(B))
                                        (make-equation '(+ (* x x) y) (list (cadr AA) 'C))))
                  (list (make-equation '(+ (* (+ 3 y) z) 3) (list' B (car AA))))))
   (test-case
    "next-substitutions"
    (check-equal? (next-substitutions (make-substitution 'x '(+ y 3) '(A))
                                      (list (make-substitution 'a '(+ (* x z) 3) '(B))
                                            (make-substitution 'b '(+ (* x x) y) '(C))))
                  (list (make-substitution 'a '(+ (* (+ 3 y) z) 3) '(B A))
                        (make-substitution 'b '(+ (expt (+ 3 y) 2) y) '(C A))))
    (define AA (make-root-premise-pair 'A))
    (check-equal? (next-substitutions (make-substitution 'x '(+ y 3) (list (car AA)))
                                      (list (make-substitution 'a '(+ (* x z) 3) '(B))
                                            (make-substitution 'b '(+ (* x x) y) (list (cadr AA) 'C))))
                  (list (make-substitution 'a '(+ (* (+ 3 y) z) 3) (list' B (car AA))))))
   (test-case
    "allowed-substitution?"
    (check-true (allowed-substitution? '(A) '(B C)))
    (check-true (allowed-substitution? '(A) '(A B C)))
    (define AA (make-root-premise-pair 'A))
    (check-false (allowed-substitution? (list (car AA)) (list (cadr AA) 'B 'C))))
   (test-case
    "isolate-var"
    (define (fail) 'done)
    (check-equal? (isolate-var 'x (make-equation 'x '(A)) vector fail)
                  (vector (make-substitution 'x 0 '(A)) fail))
    ; 3th degree too difficult
    (check-equal? (isolate-var 'x (make-equation '(+ -15 (* 23 x) (* -9 x x) (* x x x)) '(A)) vector fail)
                  'done)
    (check-equal? (car (isolate-var 'x (make-equation '(+ -15 (* 23 x) (* a x x)) '(A)) list fail))
                  (make-substitution 'x
                                     '(/ (+ -23/2 (* 1/2 (sqrt (+ 529 (* 60 a))))) a)
                                     (list 'A (hypothetical (+ quadratic a 23 -15)))))
    )
   (test-case
    "isolatable?"
    (define (fail) 'nee)
    (check-equal? (isolatable? 'x 'x vector fail) (vector 0 fail))
    (check-equal? (isolatable? 'x '(expt x 2) vector fail) (vector 0 fail))
    (let ([ans(isolatable? 'x '(* x 2) vector fail) ])
      (check-equal? (vector-ref ans 0) 0)
      (check-equal? ((vector-ref ans 1))
                    (fail)))
    (let ([ans(isolatable? 'x '(* x (+ x 2)) vector fail) ])
      (check-equal? (vector-ref ans 0) 0)
      (check-equal? (vector-ref ((vector-ref ans 1)) 0) -2))
    (check-equal? (isolatable? 'x '(/ x (+ 3 y)) vector fail) (vector 0 fail))
    ;; algebra problem
    (check-equal? (isolatable? 'x '(+ 3 x) vector fail) (vector -3 fail))
    ;; kernel problem => x will be in kernel-map
    (check-equal? (isolatable? 'x '(+ i (- (* a (- (exp x) 1)))) vector fail)
                  (vector '(- (+ (log a) (* -1 (log (+ a i))))) fail))
    ;;                => x also in analyzed part
    (check-equal? (isolatable? 'x '(+ x (- (* a (- (exp x) 1)))) vector fail)
                  (fail))
    ;; - is not sum? make-equation will take care of this
    (check-equal? (isolatable? 'x '(- 3 x) vector fail)
                  (fail)))
   (test-case
    "fpf-analyze"
    (define rslt (fpf-analyze '(- i (* a (- (exp (/ (- b c) d)) 1))) list))
    (define r2 (list (car rslt) (sort (cadr rslt) symbol<? #:key car)))
    ;; ^^^ order not guaranteed...???
    (check-unique-match? r2
                         (kernel1 kernel2 kernel3)
                         `((+ (* -1 a ,kernel3) a i)
                           ((,kernel1 (+ b (* -1 c)))
                            (,kernel2 (/ ,kernel1 d))
                            (,kernel3 (exp ,kernel2))))))
   (test-case
    "algebra-problem"
    ;; linear
    (check-equal? (algebra-problem 'x '(+ x y) up down) (up '(- y) down))
    ;; quad
    (define (scc a b c) (vector a c))
    (check-equal? (algebra-problem 'x '(+ x y (* z (expt x 2))) scc down)
                  (quadratic-formula 'z 1 'y scc down))
    ;; higher
    (check-equal? (algebra-problem 'x '(+ x y (expt x 5)) up down)
                  (down))
    )
   (test-case
    "kernel-subproblem"
    (define (fail) 'done)
    (check-exn #px"Non-kernel residue"
               (λ () (kernel-subproblem 'x '(+ (* -1 a kernel3) a i) ;; no (known) kernel in analyzed
                                        '((kernel1 x)
                                          (kernel2 (exp kernel1)))
                                        vector fail)))
    (check-equal? (kernel-subproblem 'x '(+ (* -1 a kernel3) kernel2 i) ;; more than one kernel in analyzed
                                        '((kernel1 x)
                                          (kernel2 (exp kernel1))
                                          (kernel3 (+ 3 kernel2)))
                                        vector fail)
                  'done)
    (check-equal? (kernel-subproblem 'x '(+ (* -1 a kernel3) a i)
                                        '((kernel1 (tan x))
                                          (kernel2 (exp kernel1))
                                          (kernel3 (sin kernel2)))
                                        vector fail)
                  (vector '(- (* -1 (atan (log (asin (/ (+ a i) a))) 1))) fail))
    (check-equal? (kernel-subproblem 'x '(+ (* -1 a kernel3) a i)
                                        '((kernel1 (+ x 3))
                                          (kernel2 (+ kernel1 2)) ;; + is not invertible
                                          (kernel3 (sin kernel2)))
                                        vector fail)
                  'done)
    (check-equal? (kernel-subproblem 'x '(+ (* -1 a kernel3) a i)
                                        '((kernel1 (+ x 3))
                                          (kernel2 (exp kernel1))
                                          (kernel3 (sin kernel2)))
                                        vector fail)
                  (vector '(- (+ 3 (* -1 (log (asin (/ (+ a i) a)))))) fail))
    (check-equal? (kernel-subproblem 'x '(+ (* -1 a kernel3) a i)
                                        '((kernel1 (tan x))
                                          (kernel2 (exp kernel1 kernel1));; two arguments to an invertible func (should not be possible)
                                          (kernel3 (sin kernel2)))
                                        vector fail)
                  'done)
    )
   (test-case
    "kernel-operator-spec"
    (for ([f (in-list '(sqrt exp log sin asin cos acos tan atan))]
          [q (in-list (list symb:square symb:log symb:exp symb:asin symb:sin symb:acos symb:cos symb:atan symb:tan))])
      (check-equal? (kernel-operator-spec f) (list f q))
      (check-equal? (kernel-invert (kernel-operator-spec f)) q)))
   (test-case
    "collect-terms"
    (check-exn #px"assertion failed: \\(sum\\? analyzed\\)" (λ () (collect-terms 'x '(- x) 'continuation)))
    (check-equal? (collect-terms 'x '(+ 3 y) vector) #((+ 3 y) 0 0 #hash()))
    (check-equal? (collect-terms 'x '(+ 3 x) vector) #(3 1 0 #hash()))
    ;; TODO? collect-terms expect powers to be in expt form!
    (check-equal? (collect-terms 'x '(+ (* z x x) 3 x y) vector) #((+ 3 y) (+ 1 z) 0 #hash()))
    (check-equal? (collect-terms 'x '(+ (* z (expt x 2)) 3 x y) vector)
                  #((+ 3 y) 1 z #hash()))
    (check-equal? (collect-terms 'x '(+ (* z (expt x 2.0)) 3 (expt x 1.0) (* y (expt x 0.))) vector)
                  #((+ 3 y) 1 z #hash()))
    (check-equal? (collect-terms 'x '(+ (* z (expt x 2.0)) 3 x y (* b (expt x 7) z) (* -1 b (expt x 7) z)) vector)
                  #((+ 3 y) 1 z #hash()))
    ;; other terms are gathered in reverse order at end
    ;; TODO gather higher terms in a way to use with poly:root
    ;; make sure 2.0 is in quad
    (check-equal? (collect-terms 'x '(+ (* z (expt x 2.0)) 3 x y (* 3 (expt x 7) z) (* y (expt x 4) b)) vector)
                  #((+ 3 y) 1 z #hash((7 . (* 3 z))(4 . (* b y)))))
    (check-equal? (collect-terms 'x '(+ (* z (expt x 2)) 3 x y (* 3 (expt x 7)) (* -3 (expt x 4))) vector)
                  #((+ 3 y) 1 z #hash((7 . 3)(4 . -3))))
    (check-equal? (collect-terms 'x '(+ (* z (expt x b)) 3 x y (* 3 (expt x 7)) (* -3 (expt x 4)) (* -3 (expt x 4.0))) vector)
                  #((+ 3 y) 1 0 #hash((b . z)(7 . 3)(4 . -6))))
    ;; these should not happen when indirectly calling collect-terms
    (check-exn #px"not in fpf" (λ () (collect-terms 'x '(+ 1 (sin x)) vector)))
    (check-exn #px"not in fpf" (λ () (collect-terms 'x '(+ 1 (* (sin x) x)) vector)))
    #; ; not an error (but incorrect handling)
    (check-exn #px"not in fpf" (λ () (collect-terms 'x '(+ 1 (* x (sin x))) vector)))
    )
   (test-case
    "positive-power?"
    (check-false (positive-power? '(+ 3 x)))
    (check-false (positive-power? '(expt x y)))
    (check-false (positive-power? '(expt x -1)))
    (check-true  (positive-power? '(expt x 2))))
   (test-case
    "linear-formula"
    (check-equal? (linear-formula 3 'b vector error) (vector '(/ (- b) 3) error)))
   (test-case
    "quadratic-formula"
    (let ([ans1 (quadratic-formula -1. 3. 4. vector error)])
      (check-true (vector? ans1))
      (let ([ans2 ((vector-ref ans1 1))])
        (check-equal? (vector-ref ans1 0) 4.)
        (check-equal? (vector-ref ans2 0) -1.)
        (check-equal? (vector-ref ans1 2) (hypothetical (+ quadratic -1.0 3.0 4.0)))
        (check-equal? (vector-ref ans2 2) (hypothetical (- quadratic -1.0 3.0 4.0)))
        (check-equal? (vector-ref ans2 1) error)))
    (let ([ans1 (quadratic-formula 1. 4. 4. vector error)])
      (check-true (vector? ans1))
      (check-equal? (vector-ref ans1 0) -2.)
      (check-equal? (vector-ref ans1 1) error))
    (let ([ans1 (quadratic-formula 'x 3. 4. vector error)])
      (check-true (vector? ans1))
      (let ([ans2 ((vector-ref ans1 1))])
        (check-equal? (vector-ref ans1 0) '(/ (+ -3.0 (sqrt (- 9.0 (* 16.0 x)))) (* 2 x)))
        (check-equal? (vector-ref ans2 0) '(/ (- -3.0 (sqrt (- 9.0 (* 16.0 x)))) (* 2 x)))
        (check-equal? (vector-ref ans1 2) (hypothetical (+ quadratic x 3.0 4.0)))
        (check-equal? (vector-ref ans2 2) (hypothetical (- quadratic x 3.0 4.0)))
        (check-equal? (vector-ref ans2 1) error))))
   (test-case
    "backsubstitute-substitution"
    (check-equal? (backsubstitute-substitution (make-substitution 'x '(+ 3 y) '(A))
                                               (make-substitution 'z '(+ x y) '(B)))
                  (make-substitution 'z '(+ 3 (* 2 y)) '(B A)))
    (check-equal? (backsubstitute-substitution (make-substitution 'g '(+ 3 y) '(A))
                                               (make-substitution 'z '(+ x y) '(B)))
                  (make-substitution 'z '(+ x y) '(B))))
   (test-case
    "backsubstitute-equation"
    ;; what is the difference with apply-subs-to-equations?
    (check-equal? (backsubstitute-equation (make-substitution 'x '(+ 3 y) '(A))
                                           (make-equation '(+ x z) '(B)))
                  (make-equation '(+ 3 y z) '(B A)))
    (check-equal? (backsubstitute-equation (make-substitution 'g '(+ 3 y) '(A))
                                           (make-equation '(+ x z) '(B)))
                  (make-equation '(+ x z) '(B))))
   (test-case
    "subst(s)->equation(s)"
    (check-equal? (subst->equation (make-substitution 'x '(+ 3 y) '(A)))
                  (make-equation '(- x (+ 3 y)) '(A)))
    (check-equal? (substs->equations (list (make-substitution 'x '(+ 3 y) '(A))
                                           (make-substitution 'z '(+ 3 y) '(B))))
                  (list (make-equation '(- x (+ 3 y)) '(A))
                        (make-equation '(- z (+ 3 y)) '(B)))))
   (test-case
    "apply-substitutions"
    (define E '(+ x y 3))
    (check-equal? (apply-substitutions E '()) '((+ x y 3)))
    (check-equal? (apply-substitutions E (list (make-substitution 'z '(* 2 x) '(A))))
                  '((+ x y 3)))
    (check-equal? (apply-substitutions E (list (make-substitution 'x '(* 2 z) '(A))
                                                        (make-substitution 'y '(* z z) '(B))))
                  '((+ 3 (expt z 2) (* 2 z)) A B))
    (check-equal? (apply-substitutions E (list (make-substitution 'x '(* 2 y) '(A))
                                                        (make-substitution 'y '(* z z) '(B))))
                  '((+ 3 (* 3 (expt z 2))) A B))
    ;; TODO should x be eliminated?
    (check-equal? (apply-substitutions E (list (make-substitution 'x '(* 2 z) '(A))
                                                        (make-substitution 'y '(* z x) '(B))))
                  '((+ 3 (* x z) (* 2 z)) A B)))
   (test-case
    "apply-susbstitutions-to-equation"
    (define E (make-equation '(+ x y 3) '(C)))
    (check-equal? (apply-substitutions-to-equation E '()) (make-equation '(+ x y 3) '(C)))
    (check-equal? (apply-substitutions-to-equation E (list (make-substitution 'z '(* 2 x) '(A))))
                  (make-equation '(+ x y 3) '(C)))
    (check-equal? (apply-substitutions-to-equation E (list (make-substitution 'x '(* 2 z) '(A))
                                                        (make-substitution 'y '(* z z) '(B))))
                  (make-equation '(+ 3 (expt z 2) (* 2 z)) '(B A C)))
    (check-equal? (apply-substitutions-to-equation E (list (make-substitution 'x '(* 2 y) '(A))
                                                        (make-substitution 'y '(* z z) '(B))))
                  (make-equation '(+ 3 (* 3 (expt z 2))) '(B A C)))
    ;; TODO should x be eliminated?
    (check-equal? (apply-substitutions-to-equation E (list (make-substitution 'x '(* 2 z) '(A))
                                                        (make-substitution 'y '(* z x) '(B))))
                  (make-equation '(+ 3 (* x z) (* 2 z)) '(B A C))))
   (test-case
    "max-exponent"
    (define E '(+ 3 x (* y 3 x y) (expt z 3)))
    (check-equal? ((max-exponent E) 'x) 1)
    (check-equal? ((max-exponent E) 'y) 1)
    (check-equal? ((max-exponent (car (standardize-equation E '() '() #f))) 'y) 2)
    (check-equal? ((max-exponent E) 'z) 3)
    (check-equal? ((max-exponent E) 'q) 0)
    (check-equal? ((max-exponent 'g) 'q) 0)
    (check-equal? ((max-exponent '()) 'q) 0)
    "equation-difficulty" ; auto standardize
    (define E0 (make-equation E '(A)))
    (check-equal? (equation-difficulty E0) 6))
   (test-case
    "less-difficult?"
    (check-false (less-difficult? (make-equation '(+ 3 x (* y 3 x y) (expt z 3)) '(A))
                                  (make-equation '(+ 3 x (* y 3 x y)) '(B))))
    (check-true  (less-difficult? (make-equation '(+ 3 x (* y 3 x y) (expt z 3)) '(A))
                                  (make-equation '(+ 3 x (* y 3 x y y y y y y)) '(B)))))
   (test-case
    "lower-order?"
    (check-true  ((lower-order? (make-equation '(+ 3 x (* y 3 x y) (expt z 3)) '(A)))
                  'x 'y))
    (check-false ((lower-order? (make-equation '(+ 3 x (* y 3 x y) (expt z 3)) '(A)))
                  'z 'x)))
   (test-case
    "just-union"
    (check-equal? (just-union '() '()) '())
    (check-equal? (just-union '() '(A)) '(A))
    (check-equal? (just-union '(A) '()) '(A))
    (check-equal? (just-union '(A) '(B)) '(B A))
    (check-equal? (just-union '(A) '(A)) '(A))
    (check-equal? (just-union '(A) '(A B)) '(B A)))
   (test-case
    "differential-operator? D? D2? Dn?"
    (check-false (Dn? 'x))
    (check-false (Dn? '(+ x)))
    (check-false (Dn? '(expt x 2)))
    (check-false (Dn? '(expt D 3)))
    (check-true  (Dn? '((expt D 3) F)))
    
    (check-false (D2? 'x))
    (check-false (D2? '(+ x)))
    (check-false (D2? '(expt x 2)))
    (check-false (D2? '(expt D 3)))
    (check-false (D2? '((expt D 3) F)))
    (check-true  (D2? '((expt D 2) F)))

    (check-false (D? 'x))
    (check-false (D? '(+ x)))
    (check-false (D? '(expt x 2)))
    (check-false (D? '(expt D 3)))
    (check-false (D? '((expt D 1) F))) ;; TODO??
    (check-true  (D? '(D F)))

    (check-false (differential-operator? 'x))
    (check-false (differential-operator? '(+ x)))
    (check-false (differential-operator? '(expt x 2)))
    (check-false (differential-operator? '(expt D 3)))
    (check-true  (differential-operator? '((expt D 1) F)))
    (check-true  (differential-operator? '(D F))))
   (test-case
    "standardize-equation"
    ;; (inexact) numbers
    (check-equal? (standardize-equation 1e-30 '() '() #f) '(0 () ()))
    (check-equal? (standardize-equation '(* 1e-30 x) '() '() #f) '(0 () ()))
    (check-equal? (standardize-equation '(* 4.5 x) '() '() #f) '((* 4.5 x) (x) ()))

    ;; known operators
    (for ([f (in-list '(+ - / * D expt sqrt exp log sin cos))])
      ;; TODO: what about tan, asin, etc...
      (check-equal? (standardize-equation f '() '() #f) `(,f () ())))
    ;; unknown other
    (check-equal? (standardize-equation 'f '() '() #f) '(f (f) ()))

    ;; expressions
    (check-equal? (standardize-equation '(f t) '() '() 't) '((f t) ((f t)) (f)))
    (check-equal? (standardize-equation '(f (up x y)) '(x y) '() '(up x y)) '((f (up x y)) (up f x y) ()))
    (skip ;; TODO : should this work? eq? => equal?
     (let ([t '(up x y)]) (check-equal? (standardize-equation `(f ,t) '(x y) '() t) `((f ,t) (x y (f ,t)) (f)))))
    (check-equal? (standardize-equation '((D f) t) '(x y) '() 't) '(((D f) t) (((D f) t) x y) ((D f))))
    (check-equal? (standardize-equation '(D f) '(x y) '() 't) '((D f) (x y) ((D f))))
    (check-equal? (standardize-equation '(d f) '(x y) '() 't) '((d f) (f d x y) ()))

    ;; dividends: equations are = 0 so numerators are ignored
    (check-equal? (standardize-equation '(/ (+ (f t) 3) (+ x y)) '() '() 't) '((+ 3 (f t)) ((f t)) (f)))
    
    (check-equal? (standardize-equation '(* 4.5 x) '(y) '() #f) '((* 4.5 x) (x y) ()))
    (check-equal? (standardize-equation '(* 4.5 x) '() '(f) #f) '((* 4.5 x) (x) (f)))
    (check-equal? (standardize-equation '(* 4.5 x) '() '() #f vector) (vector '(* 4.5 x) '(x) '()))
    )
   (test-case
    "solve-incremental"
    (check-equal? (solve-incremental (list (make-equation 'x '(A))) '(x))
                  (list '() '() (list (make-substitution 'x 0 '(A))) '()))
    (check-equal? (solve-incremental (list (make-equation 3 '(A))) '(x))
                  '(contradictions (3 (A) ())))
    (check-equal? (solve-incremental (list (make-equation '(+ 1 (expt x 3)) '(A))) '(x))
                  '(failed . #f))
    (let ([ans (solve-incremental (list (make-equation 'x '(A))) '(x) up down)])
      (check-true (vector? ans))
      (check-equal? (vector-ref ans 0) (list '() '() (list (make-substitution 'x 0 '(A))) '()))
      (check-equal? ((vector-ref ans 1)) (down)))
    (check-equal? (solve-incremental (list (make-equation 3 '(A))) '(x) up down)
                  (down))
    ;; TODO this should be solvable (multiple hypotheticals?)
    (check-equal? (solve-incremental (list (make-equation '(+ 1 (expt x 3)) '(A))) '(x) up down)
                  (down))
    (check-equal? (solve-incremental '() '()) '(() () () ()))

    (check-equal? (solve-incremental (list (make-equation 3 '(A))) '())
                  '(failed . #f))
    (check-equal? (solve-incremental '() '(x))
                  '(() (x) () ()))
    (skip ;; TODO - this should fail (contradiction?)
     (solve-incremental (list (make-equation '(+ x -3) '(A))
                              (make-equation '(+ (cos x) -3) '(B)))
                        '(x)))
    (check-equal? (solve-incremental (list (make-equation '(+ x -3) '(A))
                           (make-equation '(+ 4 x) '(B)))
                        '(x))
                  '(contradictions (-7 (A B) ()) (7 (B A) ()))))
   ;**************************************************************************************************
   (check-equal?
    (standardize-equation '(- (* 3 ((D f) t))
                              (+ (* (sqrt x) z (f t))
                                 (g t)
                                 (((expt D 2) g) t)
                                 (square y)))
                          '() '() 't)
    #;'((+ (* -1 z (f t) (sqrt x))
           (* -1 (expt y 2))
           (* 3 ((D f) t))
           (* -1 (g t))
           (* -1 (((expt D 2) g) t)))
        ((((expt D 2) g) t) (g t) ((D f) t) y x (f t) z)
        (((expt D 2) g) g (D f) f))
    ;this looks ok, ... maybe? at least (f t) is put consistently last
    '((+
       (* -1 z (sqrt x) (f t))
       (* -1 (expt y 2))
       (* 3 ((D f) t))
       (* -1 (g t))
       (* -1 (((expt D 2) g) t)))
      ((((expt D 2) g) t) (g t) ((D f) t) y (f t) x z)
      (((expt D 2) g) g (D f) f)))

   ;;; Signs of life.  
   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(x y))
    '(() () (((= y 1) (B A)) ((= x 2) (B A))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(+  x   y   z  1)  (list 'A))
           (make-equation '(+  x   y      2)  (list 'B))
           (make-equation '(+  x          1)  (list 'C)))
     '(x y z))
    '(() () (((= z 1) (A B C)) ((= y -1) (B C)) ((= x -1) (C))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(+  x          1)  (list 'C))
           (make-equation '(+  x   y      2)  (list 'B))
           (make-equation '(+  x   y   z  1)  (list 'A)))
     '(x y z))
    '(() () (((= z 1) (A B C)) ((= y -1) (B C)) ((= x -1) (C))) ()))

   ;;; The following signals a contradiction, as it should:

   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x)     y  -5)  (list 'B)))
     '(x y))
    '(contradictions (-2 (A B) ()) (2 (B A) ())))

   ;;; Some slightly nonlinear systems can be solved:
   (check-equal?
    (solve-incremental
     (list (make-equation '(-  3 (+ x y))  (list 'A))
           (make-equation '(-  5 (- x y))  (list 'B))
           (make-equation '(-  3 (+ (* (sqrt x) z) (square y)))  (list 'C)))
     '(x y z))
    '(() () (((= z 1) (C B A)) ((= y -1) (B A)) ((= x 4) (B A))) ()))

   ;;; Underdetermined systems can be reduced:
   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
           (make-equation '(- 3 (+ a b))  (list 'B)))
     '(a b c))
    '(() (c) (((= b (+ 3 (* -2/3 c))) (A B)) ((= a (* 2/3 c)) (A B))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
           (make-equation '(- 3 (- a c))  (list 'B)))
     '(a b c))
    '(() (c) (((= b (+ -3 (* -4/3 c))) (A B)) ((= a (+ 3 c)) (B))) ()))

   ;;; Even very hard ones are clarified.
   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
           (make-equation '(- 3 (- a b))  (list 'B)))
     '(a b c))
    '(()
      (b)
      (((= c (/ (+ 9 (* 2 (expt b 2)) (* 9 b)) (+ 2 (* 2 b)))) (A B))
       ((= a (+ 3 b)) (B)))
      ()))

   ;;; The following are permutations of the solution sequence
   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
           (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(x y z))
    '(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
           (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(z x y))
    '(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
           (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(y z x))
    ;'(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
    ;previous test seems to sugest that if two equations are equal-difficulty, the order of the
    ;the variables should be followed, this test seems to suggest something else intirely
    '(() () (((= z -1/2) (C B A)) ((= x 2) (B A)) ((= y 1) (B A))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
           (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(y x z))
    ;'(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
    ;previous test seems to sugest that if two equations are equal-difficulty, the order of the
    ;the variables should be followed, this test seems to suggest something else intirely
    '(() () (((= z -1/2) (C B A)) ((= x 2) (B A)) ((= y 1) (B A))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
           (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(z y x))
    ;'(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
    ;previous test seems to sugest that if two equations are equal-difficulty, the order of the
    ;the variables should be followed, this test seems to suggest something else intirely
    '(() () (((= z -1/2) (C B A)) ((= x 2) (B A)) ((= y 1) (B A))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
           (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
           (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(x z y))
    '(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ()))

   ;;; This wins somehow...
   (check-equal?
    (solve-incremental
     (list (make-equation '(- 200/3 (/ 1 (+ (/ 1 R1) (/ 1 R2))))  (list 'A))
           (make-equation '(-  1/3 (/ R2 (+ R1 R2)))  (list 'B)))
     '(R1 R2))
    `(() () (((= R2 100) (A B ,(hypothetical (+ quadratic -6 600 0))))
             ((= R1 200) (A B ,(hypothetical (+ quadratic -6 600 0))))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(- 200/3 (/ 1 (+ (/ 1 R1) (/ 1 R2))))  (list 'A))
           (make-equation '(-  1/3 (/ R2 (+ R1 R2)))  (list 'B)))
     '(R2 R1))
    `(() () (((= R1 200) (A B ,(hypothetical (+ quadratic 3 -600 0))))
             ((= R2 100) (A B ,(hypothetical (+ quadratic 3 -600 0))))) ()))

   (check-equal?
   (solve-incremental
    (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
          (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
    '(R1 R2))
   `(() () (((= R2 100) (B A ,(hypothetical (+ quadratic -2 200 0))))
            ((= R1 200) (B A ,(hypothetical (+ quadratic -2 200 0))))) ()))

   (check-equal?
 (solve-incremental
  (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
        (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
  '(R2 R1))
 `(() () (((= R1 200) (B A ,(hypothetical (+ quadratic -1/2 100 0))))
          ((= R2 100) (B A ,(hypothetical (+ quadratic -1/2 100 0))))) ()))

   ;; how did we avoid the extra root, R2=0 & R1=0, that satisfies the
   ;; given equations but not the original problem?

   ;; Answer: it is here, but we also have a redundant solutions problem!
   (check-equal? (out->string (solve-incremental
                               (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
                                     (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
                               '(R1 R2)
                               (lambda (sol fail) (println sol) (fail))
                               (lambda () 'done)))
                 "(list '() '() (list (list '(= R2 100) (list 'B 'A (hypothetical '(+ quadratic -2 200 0) #f))) (list '(= R1 200) (list 'B 'A (hypothetical '(+ quadratic -2 200 0) #f)))) '())
(list '() '() (list (list '(= R2 0) (list 'B 'A (hypothetical '(- quadratic -2 200 0) #f))) (list '(= R1 0) (list 'B 'A (hypothetical '(- quadratic -2 200 0) #f)))) '())
(list '() '() (list (list '(= R2 100) (list 'B 'A (hypothetical '(+ quadratic -2 200 0) #f))) (list '(= R1 200) (list 'B 'A (hypothetical '(+ quadratic -2 200 0) #f)))) '())
(list '() '() (list (list '(= R2 0) (list 'B 'A (hypothetical '(- quadratic -2 200 0) #f))) (list '(= R1 0) (list 'B 'A (hypothetical '(- quadratic -2 200 0) #f)))) '())
(list '() '() (list (list '(= R2 100) (list 'A 'B (hypothetical '(+ quadratic -6 600 0) #f))) (list '(= R1 200) (list 'A 'B (hypothetical '(+ quadratic -6 600 0) #f)))) '())
(list '() '() (list (list '(= R2 0) (list 'A 'B (hypothetical '(- quadratic -6 600 0) #f))) (list '(= R1 0) (list 'A 'B (hypothetical '(- quadratic -6 600 0) #f)))) '())
")
   
(skip ;;TODO investigate above problem of missing 0 solution (see solve/solve)
   )

   ;;; Now can solve quadratics and does backtracking to find a root
   (check-equal?
    (solve-incremental
     (list (make-equation '(- (expt x 2) 1)  (list 'A))
           (make-equation '(- x 1)  (list 'B)))
     '(x))
    '(() () (((= x 1) (B))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(- (expt x 2) 1)  (list 'A))
           (make-equation '(- x -1)  (list 'B)))
     '(x))
    '(() () (((= x -1) (B))) ()))

   ;;; It doesn't to look at A to get answer, but A constrains the answer.
   (check-equal?
      (solve-incremental
       (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
             (make-equation '(- (expt y 2) 9) (list 'B))
             (make-equation '(- (- y x) 1) (list 'C)))
       '(x y))
      `(() () (((= y 3) (C A ,(hypothetical (- quadratic 1 -5 6))))
               ((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))) ()))

   (check-equal?
    (solve-incremental
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

   ;;; so it knows the outstanding contradictions...

   ;;; Here we are left over with a residual equation in z
   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
           (make-equation '(- (expt y 2) z) (list 'B))
           (make-equation '(- (- y x) 2) (list 'C)))
     '(x y))
    `((((+ 25 (* -1 z)) (B C A ,(hypothetical (+ quadratic 1 -5 6))) (z)))
      ()
      (((= y 5) (C A ,(hypothetical (+ quadratic 1 -5 6))))
       ((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6)))))
      ()))

   ;;; But we can ask to solve for z
   (check-equal?
    (solve-incremental
     (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
           (make-equation '(- (expt y 2) z) (list 'B))
           (make-equation '(- (- y x) 2) (list 'C)))
     '(x y z))
    `(()
      ()
      (((= z 25) (B C A ,(hypothetical (+ quadratic 1 -5 6))))
       ((= y 5) (C A ,(hypothetical (+ quadratic 1 -5 6))))
       ((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6)))))
      ()))

   ;;; Multiple results can be obtained.
   #|
Some things are not right, see solve/solve
(check-equal?
 (solve-incremental
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A)))
  '(x))
 '())

(check-equal?
 (solve-incremental
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(+ (expt x 2) (* -7 x) 10)  (list 'B)))
  '(x))
 '())

(check-equal?
 (solve-incremental
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(+ (expt x 2) (* a x) 10)  (list 'B)))
  '(a x))
 '())
|#

   ;;; perhaps a bit of filtering would help?

   ;;; It now knows about some special functions with inverses
   (check-equal?
    (solve-incremental
     (list (make-equation '(- 2 (sqrt (+ x 1)))  (list 'A)))
     '(x))
    '(() () (((= x 3) (A))) ()))

   (check-equal?
    (solve-incremental
     (list (make-equation '(- 2 (acos (sqrt (+ x 1))))  (list 'A)))
     '(x))
    '(() () (((= x (* -1 (expt (sin 2) 2))) (A))) ())
    #; ;equivalent
    '(() () (((= x (+ -1 (expt (cos 2) 2))) (A))) ()))

   ;;; A real use. Note how dependencies keep track of contributions to solution
   ;TODO
   (let ()
     (define equations
       (list
        (make-equation
         '(+ (* -1/6 eta sr0) (* 1/12 eta sr1) (* -1/2 nu siga0) (* -1/4 nu siga1)
             (* -1/4 nu siga2) (* -1/2 nu siga3) (* -1 nu siga4) (* 1/2 sigd0)
             (* 1/4 sigd1) (* 1/4 sigd2) (* 1/2 sigd3) sigd4)
         (list 'A))
        (make-equation
         '(+ (* -1/4 eta sd1) (* 1/8 eta sr1) (* -1/8 nu siga1) (* 1/8 nu siga2)
             (* 1/8 sigd1) (* -1/8 sigd2))
         (list 'B))
        (make-equation
         '(+ (* -1/4 eta sd1) (* 1/8 eta sr1) (* -1/8 nu siga1) (* 1/8 nu siga2)
             (* 1/8 sigd1) (* -1/8 sigd2))
         (list 'C))
        (make-equation
         '(+ (* -1/4 eta sr1) (* -1/4 nu siga1) (* -1/4 nu siga2) (* -1/2 nu siga3)
             (* 1/4 sigd1) (* 1/4 sigd2) (* 1/2 sigd3))
         (list 'D))
        (make-equation
         '(+ (* -1 eta sd0) (* -1/2 eta sd1) (* -1/2 eta sr0) (* 1/4 eta sr1)
             (* -1/2 nu siga0) (* -1/4 nu siga1) (* 1/4 nu siga2) (* 1/2 sigd0)
             (* 1/4 sigd1) (* -1/4 sigd2))
         (list 'E))
        (make-equation
         '(+ (* -1/8 eta sa0) (* 1/16 eta sd1) (* -1/16 eta sr1) (* 1/16 nu sigd1)
             (* -1/16 nu sigd2) (* -1/16 siga1) (* 1/16 siga2))
         (list 'F))
        (make-equation
         '(+ (* 1/8 eta sa0) (* -1/16 eta sd1) (* 1/16 eta sr1) (* -1/16 nu sigd1)
             (* 1/16 nu sigd2) (* 1/16 siga1) (* -1/16 siga2))
         (list 'G))
        (make-equation
         '(+ (* -3/8 eta sa0) (* -1/2 eta sa1) (* -1/16 eta sd1) (* -3/16 eta sr1)
             (* -1/16 nu sigd1) (* -3/16 nu sigd2) (* -1/4 nu sigd3) (* 1/16 siga1)
             (* 3/16 siga2) (* 1/4 siga3))
         (list 'H))
        (make-equation
         '(+ (* 3/8 eta sa0) (* 1/2 eta sa1) (* 1/16 eta sd1) (* 3/16 eta sr1)
             (* 1/16 nu sigd1) (* 3/16 nu sigd2) (* 1/4 nu sigd3) (* -1/16 siga1)
             (* -3/16 siga2) (* -1/4 siga3))
         (list 'I))
        (make-equation
         '(+ (* -1/4 eta sd0) (* -1/8 eta sd1) (* -1/4 eta sr0) (* 1/8 eta sr1)
             (* -1/4 nu sigd0) (* -1/8 nu sigd1) (* 1/8 nu sigd2) (* 1/4 siga0)
             (* 1/8 siga1) (* -1/8 siga2))
         (list 'J))
        (make-equation
         '(+ (* -1/4 eta sd0) (* -1/8 eta sd1) (* 1/12 eta sr0) (* -1/24 eta sr1)
             (* -1/4 nu sigd0) (* -1/8 nu sigd1) (* -3/8 nu sigd2) (* -1/2 nu sigd3)
             (* -1 nu sigd4) (* 1/4 siga0) (* 1/8 siga1) (* 3/8 siga2) (* 1/2 siga3)
             siga4)
         (list 'K))
        ))
  
     (define unknowns
       '(siga0 siga1 siga2 siga3 siga4 sigd0 sigd1 sigd2 sigd3 sigd4 sa0 sa1 sd0 sd1))

     (define solution (solve-incremental equations unknowns))

     (define textbooksolution
       '(()                                 ; no residuals left
         (sa0 sa1 sd0 sd1 siga3 sigd3)      ; excess variables
         (((= sigd4                         ; substitutions
              (/ (+ (* 3 eta nu sa0)
                    (* 3 eta nu sa1)
                    (* eta nu sr0)
                    (* eta nu sr1)
                    (* 3 eta sd0)
                    (* eta sr0)
                    (* eta sr1))
                 (+ -3 (* 3 (expt nu 2)))))
           (K A J E H D F B))
          ((= sigd2
              (/ (+ (* -2 eta nu sa0)
                    (* -2 eta nu sa1)
                    (* -1 eta nu sr1)
                    (* -1 (expt nu 2) sigd3)
                    (* eta sd1)
                    (* -1 eta sr1)
                    sigd3)
                 (+ -1 (expt nu 2))))
           (H D F B))
          ((= sigd1
              (/ (+ (* -2 eta nu sa1)
                    (* -1 eta nu sd1)
                    (* -1 (expt nu 2) sigd3)
                    (* -1 eta sd1)
                    sigd3)
                 (+ -1 (expt nu 2))))
           (H D F B))
          ((= sigd0
              (/ (+ (* -1 eta nu sa0)
                    (* -1 eta nu sd0)
                    (* -1 eta nu sr0)
                    (* -2 eta sd0)
                    (* -1 eta sr0))
                 (+ -1 (expt nu 2))))
           (J E F B))
          ((= siga4
              (/ (+ (* 3 eta nu sd0)
                    (* eta nu sr0)
                    (* eta nu sr1)
                    (* 3 eta sa0)
                    (* 3 eta sa1)
                    (* eta sr0)
                    (* eta sr1))
                 (+ -3 (* 3 (expt nu 2)))))
           (K A J E H D F B))

          ((= siga2
              (/ (+ (* eta nu sd1)
                    (* -1 eta nu sr1)
                    (* -1 (expt nu 2) siga3)
                    (* -2 eta sa0)
                    (* -2 eta sa1)
                    (* -1 eta sr1)
                    siga3)
                 (+ -1 (expt nu 2))))
           (H D F B))
          ((= siga1
              (/ (+ (* -1 eta nu sd1)
                    (* -1 (expt nu 2) siga3)
                    (* -2 eta sa1)
                    (* -1 eta sd1)
                    siga3)
                 (+ -1 (expt nu 2))))
           (H D F B))
          ((= siga0
              (/ (+ (* -2 eta nu sd0)
                    (* -1 eta nu sr0)
                    (* -1 eta sa0)
                    (* -1 eta sd0)
                    (* -1 eta sr0))
                 (+ -1 (expt nu 2))))
           (J E F B)))
         ())  ; no equations considered tough (no way to isolate)
       )
     ;; bdk ;; order is not preserved but the solution is the same
     (check-equal? (list-ref solution 0) (list-ref textbooksolution 0)) ;; '()
     (check-true (lset= eq? (list-ref solution 1) (list-ref textbooksolution 1)))
     (for ([sub (in-list (list-ref solution 2))])
       (define var (cadar sub))
       (define expr (caddar sub))
       (define just (cadr sub))
       (define tsub (for/first ([t (in-list (list-ref textbooksolution 2))]
                                #:when (eq? var (cadar t))) t))
       (check-not-false tsub (format "No sub for var ~a" var))
       (check-true (lset= eq? just (cadr tsub)) (format "Different justifications for var ~a" var))
       (check-equal? expr (caddar tsub) (format "Different result for var ~a" var)))
     (check-equal? (list-ref solution 3) (list-ref textbooksolution 3)) ;; '()
     ;;; Check
     (check-equal?
      (map (lambda (equation)
             (apply-substitutions-to-equation equation
                                              (substitutions solution)))
           equations)
      '((0 (B F H D K E J A) ())
        (0 (F H D B) ())
        (0 (B F H D C) ())
        (0 (B F H D) ())
        (0 (B F H D J E) ())
        (0 (B H D F) ())
        (0 (B F H D G) ())
        (0 (B F D H) ())
        (0 (B F H D I) ())
        (0 (B F H D E J) ())
        (0 (B F H D A E J K) ())))
  
     )

   ;;; SIMPLE SOLVE
   (check-equal?
    (simple-solve
     (up '(+ (* 3 x)     y  -7)
         '(+ (* 3 x) (- y)  -5))
     '(x y))
    '(*solution* ()
                 ()
                 (((= y 1) (eq:1 eq:0))
                  ((= x 2) (eq:1 eq:0)))
                 ()) )
   (check-equal?
    (simple-solve
     (up '(+ (* 3 x)     y  -7)
         '(+ (* 3 x) (- y)  z))
     '(x y) '(z))
    '(*solution* ()
                 ()
                 (((= y (+ 7/2 (* 1/2 z))) (eq:0 eq:1))
                  ((= x (+ 7/6 (* -1/6 z))) (eq:0 eq:1)))
                 ()))

   (test-case
    "printing 1"
    (define-values (ans str)
      (parameterize ([current-output-port (open-output-string)])
        (values (simple-solve
                 (up '(+ (* 3 (f x))     (g y)  -7)
                     '(+ (* 3 (f x)) (- (g y))  -5))
                 '((f x) (g y))
                 '()
                 #t)
                (begin0 (get-output-string (current-output-port))
                        (close-output-port (current-output-port))))))
    (check-equal? ans
                  '(*solution* ()
                               ()
                               (((= (g y) 1) (eq:1 eq:0))
                                ((= (f x) 2) (eq:1 eq:0)))
                               ()) )
    (define ans2 (cadr (call-with-input-string str read)))
    (check-unique-match? ans2
                         (G439 G440)
                         `(((+ -5 (* 3 ,G439) (* -1 ,G440)) (eq:0) (,G440 ,G439))
                           ((+ -7 (* 3 ,G439) ,G440) (eq:1) (,G440 ,G439)))))

   (test-case
    "printing 2"
    (define-values (ans str)
      (parameterize ([current-output-port (open-output-string)])
        (values (simple-solve
                 (up '(+ (* 3 (f x))     (g y)  (H q))
                     '(+ (* 3 (f x)) (- (g y))  -5))
                 '((f x) (g y))
                 '((H q))
                 #t)
                (begin0 (get-output-string (current-output-port))
                        (close-output-port (current-output-port))))))
    (check-equal? ans
                  '(*solution* ()
                               ()
                               (((= (g y) (+ -5/2 (* -1/2 (H q)))) (eq:1 eq:0))
                                ((= (f x) (+ 5/6 (* -1/6 (H q)))) (eq:1 eq:0)))
                               ()))
    (define ans2 (cadr (call-with-input-string str read)))
    (check-unique-match? ans2
                         (x57 x58 k59)
                         `(((+ -5 (* 3 ,x57) (* -1 ,x58)) (eq:0) (,x58 ,x57))
                           ((+ ,k59 (* 3 ,x57) ,x58) (eq:1) (,x58 ,x57 ,k59)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))
