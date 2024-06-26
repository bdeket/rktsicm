#lang racket/base

(require rackunit
         "../../../main.rkt"
         "../../helper.rkt")

(rename-part 'derivative 'D)

(define Real^2->Real (-> (X Real Real) Real))
(define Real^3->Real (-> (X Real Real Real) Real))

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/diffbug/derivative-test"
   (check-simplified? ((literal-function 'f) 'x) '(f x))
   (check-simplified? ((D (literal-function 'f)) 'x) '((D f) x))
   (check-simplified? ((D (D (literal-function 'f))) 'x) '(((expt D 2) f) x))
   (check-simplified? ((D (compose (literal-function 'f)
                                   (literal-function 'g)))
                       'x)
                      '(* ((D f) (g x)) ((D g) x)))

   ;; Real^2
   (check-simplified? (((partial 0)
                        (literal-function 'f Real^2->Real))
                       'x 'y)
                      '(((partial 0) f) x y))
   (check-simplified? (((partial 1)
                        (literal-function 'f Real^2->Real))
                       'x 'y)
                      '(((partial 1) f) x y))
   (check-simplified? ((D
                        (lambda (x)
                          ((literal-function 'h Real^2->Real)
                           ((literal-function 'f) x)
                           ((literal-function 'g) x))))
                       'x)
                      '(+ (* (((partial 0) h) (f x) (g x)) ((D f) x))
                          (* (((partial 1) h) (f x) (g x)) ((D g) x))))
   
   (test-case
    "foo-square"
    (define (foo x y)
      (square (+ (square x) y)))
    (check-simplified? (((partial 0) foo) 'x 'y) '(+ (* 4 (expt x 3)) (* 4 x y)))
    (check-simplified? (((partial 1) foo) 'x 'y) '(+ (* 2 (expt x 2)) (* 2 y)))
    (check-simplified? (((partial 1) ((partial 0) foo)) 'x 'y) '(* 4 x)))
   
   (check-simplified? ((D (lambda (x) (* x x x))) 'a) '(* 3 (expt a 2)))
   (check-simplified? (((D (lambda (x)
                             (lambda (y z)
                               (* x y z))))
                        2) 3 4) 12)

   ;;Real^3
   (check-simplified? (((D (lambda (x)
                             (lambda (y z)
                               ((literal-function 'f Real^3->Real)
                                x y z))))
                        2) 3 4)
                      '(((partial 0) f) 2 3 4))
   (check-simplified? ((D (lambda (x)
                            (((partial 1) 
                              (literal-function 'f Real^2->Real))
                             x 'v))) 'u)
                      '(((partial 0) ((partial 1) f)) u v))
;;;            Here are some hard problems.
   (check-simplified? (((D (lambda (x)
                             (D
                              (lambda (y)
                                ((literal-function 'f Real^2->Real)
                                 x y)))))
                        'u) 'v)
                      '(((partial 0) ((partial 1) f)) u v))
;;; Eliminating complexities of literal functions.
;; coderef: deferred-extract
   (check-simplified? (((D (lambda (x)
                             (D
                              (lambda (y)
                                (* x y)))))
                        'u) 'v)
                      1)
   (test-case
    "foo-hard"
    (define (foo u v) (+ (* u v v) u))
    (check-simplified? (((D (lambda (x)
                              (D (lambda (y) (foo x y)))))
                         'u) 'v)
                       '(* 2 v)))
   (check-simplified? (((D (lambda (x)
                             (lambda (y) 
                               ((literal-function 'f Real^2->Real)
                                x y))))
                        'u) 'v)
                      '(((partial 0) f) u v))
   (check-simplified? (((lambda (x)
                          (D (lambda (y)
                               ((literal-function 'f Real^2->Real)
                                x y))))
                        'u) 'v)
                      '(((partial 1) f) u v))
;;; Alexey Radul problem!  With Scmutils version of extract.
   (test-case
    "Alexey Radul problem"
    (define (((p x) g) y) (g (+ x y)))
    (define f-hat ((D p) 3))
    (define (cube x) (* x x x))
    (check-simplified? ((f-hat cube) 5) 192)
    (check-simplified? ((f-hat (f-hat cube)) 5) 66)
    (check-simplified? ((f-hat exp) 5) 2980.9579870417283)
    (check-simplified? ((f-hat (f-hat exp)) 5) 59874.14171519782)
    (check-simplified? ((f-hat cube) 'a) '(+ 27 (* 3 (expt a 2)) (* 18 a)))
    (check-simplified? ((f-hat (f-hat cube)) 'a) '(+ 36 (* 6 a)))
    (check-simplified? ((f-hat (literal-function 'g)) 5) '((D g) 8))
    (check-simplified? ((f-hat (f-hat (literal-function 'g))) 'a) '(((expt D 2) g) (+ 6 a))))
   
   (check-simplified? ((Legendre-transform (lambda (x)
                                             (* 'c (square x))))
                       'y)
                      '(/ (* 1/4 (expt y 2)) c))
   (check-simplified? ((Legendre-transform (lambda (x)
                                             (+ (* 'a x) (* 'b (square x)))))
                       'y)
                      '(/ (+ (* 1/4 (expt a 2))
                             (* -1/2 a y)
                             (* 1/4 (expt y 2)))
                          b))
   (check-simplified? ((let ((m 'm) (k 'k) (x 'x))
                         (Legendre-transform
                          (lambda (v)
                            (- (* 1/2 m (square v))
                               (* 1/2 k (square x))))))
                       'p)
                      '(+ (* 1/2 k (expt x 2)) (/ (* 1/2 (expt p 2)) m)))
   (check-simplified? ((let ((m 'm) (k 'k) (x 'x))
                         (Legendre-transform
                          (lambda (v)
                            (- (* 1/2 m (square v))
                               ((literal-function 'V) x)))))
                       'p)
                      '(/ (+ (* m (V x)) (* 1/2 (expt p 2))) m))
   (check-simplified? ((((D (lambda (x)
                              (D (lambda (y)
                                   (D (lambda (z)
                                        (+ (* x y) (* y z))))))))
                         'u) 'v) 'w)
                      0)
   (check-simplified? ((((D (lambda (x)
                              (D (lambda (y)
                                   (D (lambda (z)
                                        (* (* x y) (* y z))))))))
                         'u) 'v) 'w)
                      '(* 2 v))
   (check-simplified? ((((D (lambda (x)
                              (D (lambda (y)
                                   (D (lambda (z)
                                        ((literal-function 'f Real^3->Real)
                                         x y z)))))))
                         'u) 'v) 'w)
                      '(((* (partial 2) (partial 0) (partial 1)) f) u v w))
   (check-simplified? ((((D (lambda (x)
                            (D (lambda (y)
                               (D (lambda (z)
                                  ((literal-function 'f Real^2->Real)
                                   (* x y) (* y z))))))))
                         'u) 'v) 'w)
                      '(+ (* u (expt v 2) (((* (expt (partial 0) 2) (partial 1)) f) (* u v) (* v w)))
                          (* (expt v 2) w (((* (partial 0) (expt (partial 1) 2)) f) (* u v) (* v w)))
                          (* 2 v (((* (partial 0) (partial 1)) f) (* u v) (* v w)))))
   (check-simplified? ((D (lambda (x) (/ (sin x) x))) 'a)
                      #;'(+ (* (cos a) (/ 1 a)) (* -1 (/ (sin a) (square a))))
                      '(/ (+ (* (cos a) (expt a 2))
                             (* -1 a (sin a)))
                          (expt a 3)))
   (check-simplified? ((D (lambda (x) 
                            (/ (- 1 (exp (expt x 2))) x)))
                       'a)
                      '(/ (+ -1
                             (* -2 (expt a 2) (exp (expt a 2)))
                             (exp (expt a 2)))
                          (expt a 2)))
   (test-case
    "Version of Alexey's bug from paper by Manzyuk, et.al."
    ;;; With patch, using with-new-increment
    (define S
      (lambda (u)
        (lambda (f)
          (lambda (x)
            (f (+ x u))))))
    (check-simplified? ((((D S) 0) (literal-function 'f)) 'x)
                       '((D f) x))
    (check-simplified? ((((D S) 0)
                         (((D S) 0)
                          (literal-function 'f))) 'x)
                       '(((expt D 2) f) x))
    (check-simplified? ((((D S) 3)
                         (((D S) 5)
                          (literal-function 'f))) 'x)
                       '(((expt D 2) f) (+ 8 x)))
    ;;; But by defining s-hat rather than creating it twice we get
    ;;; the expected failure.
    (define S-hat ((D S) 3))
    (check-simplified? ((S-hat (literal-function 'f)) 'x)
                       '((D f) (+ 3 x)))
    (check-simplified? ((S-hat (S-hat (literal-function 'f))) 'x)
                       ;;; Without patch ;Value: 0 ;;; Wrong!
                       ;;; With patch -- correct:
                       '(((expt D 2) f) (+ 6 x))))
   (test-case
    "May 2019.  Siskind found a new bug!"
    (define S
      (lambda (u) 
        (lambda (f1)
          (lambda (f2)
            (lambda (x)
              ((f1 f2)
               (+ x u)))))))
    (define d-hat ((D S) 0))
    (check-simplified? (((((D S) 0) 
                          (((D S) 0)
                           identity))
                         exp)
                        1)
                       2.718281828459045)
    
    (check-simplified? (((d-hat (d-hat identity))
                         exp)
                        1)
                       2.718281828459045)

    ;;; FROM diffexamples.scm
    (check-simplified? (((d-hat (d-hat identity))
                         (λ (x) (* x x x)))
                        'a)
                       '(* 6 a))
    (check-simplified? (((d-hat (d-hat identity))
                         (literal-function 'f))
                        'a)
                       '(((expt D 2) f) a))
    (check-simplified? (((((D S) 0)
                          (((D S) 0)
                           identity))
                         (lambda (x) (* x x x x)))
                        'a)
                       '(* 12 (expt a 2)))
    (check-simplified? (((d-hat (d-hat identity))
                         (lambda (x) (* x x x x)))
                        'a)
                       '(* 12 (expt a 2))))
   (test-case
    "Church pairs"
    (define kons
      (lambda (a d)
        (lambda (m)
          (m a d))))
    (define kar
      (lambda (x)
        (x (lambda (a d) a))))
    (define kdr
      (lambda (x)
        (x (lambda (a d) d))))
    (define p1 (kons 'x1 'y1))
    (define p2 (kons 'x2 'y2))
    (define (-p p q)
      (kons (- (kar p) (kar q))
            (- (kdr p) (kdr q))))
    (define (n*p n p)
      (kons (* n (kar p))
            (* n(kdr p))))
    (define (len dp)
      (sqrt (+ (square (kar dp))
               (square (kdr dp)))))
    (check-simplified? (len (-p p2 p1))
                       '(sqrt (+ (square (- x2 x1))
                                 (square (- y2 y1)))))
    
    (define (f z) (len (- (n*p z p2) p1)))
    (check-simplified? (f 'a)
                       '(sqrt (+ (square (- (* a x2) x1)) 
                                 (square (- (* a y2) y1)))))
    (check-simplified? ((D f) 'a)
                       '(* (/ 1
                              (* 2
                                 (sqrt (+ (square (- (* a x2) x1))
                                          (square (- (* a y2) y1))))))
                           (+ (* (* 2 (- (* a x2) x1)) x2)
                              (* (* 2 (- (* a y2) y1)) y2))))
#|
(/ (+ (* a (expt x2 2))
      (* a (expt y2 2))
      (* -1 x1 x2)
      (* -1 y1 y2))
   (sqrt
    (+ (* (expt a 2) (expt x2 2))
       (* (expt a 2) (expt y2 2))
       (* -2 a x1 x2)
       (* -2 a y1 y2)
       (expt x1 2)
       (expt y1 2))))

;;; The correct D!
|#
    (define (g x y) (len (kons x y)))
    (check-simplified? (((partial 0) g) 'a 'b)
                       '(* (/ 1 (* 2 (sqrt (+ (square a) (square b))))) (* 2 a)))
    (check-simplified? (((partial 1) g) 'a 'b)
                       '(* (/ 1 (* 2 (sqrt (+ (square a) (square b))))) (* 2 b)))
    (check-simplified? (((partial 0) ((partial 1) g)) 'a 'b)
                       '(* (* (* -1
                                 (/ 1
                                    (square (* 2 (sqrt (+ (square a) (square b)))))))
                              (* 2 (* (/ 1 (* 2 (sqrt (+ (square a) (square b))))) (* 2 b))))
                           (* 2 a)))
#|
(simplify (((partial 0) ((partial 1) g)) 'a 'b))
(/ (* -4 a b)
   (* (sqrt (+ (expt a 2) (expt b 2)))
      (expt (* 2 (sqrt (+ (expt a 2) (expt b 2))))
            2)))
= (* -1 a b (expt (+ (expt a 2) (expt b 2)) -3/2))
;;; The correct D!
|#
    (check-simplified? (((partial 1) ((partial 0) ((partial 1) g))) 'a 'b)
                       '(/ (+ (* -4 a)
                              (* 48 
                                 a
                                 (expt b 2)
                                 (expt (* 2 (sqrt (+ (expt a 2) (expt b 2)))) -2)))
                           (* (sqrt (+ (expt a 2) (expt b 2)))
                              (expt (* 2 (sqrt (+ (expt a 2) (expt b 2)))) 2))))
    )
   (test-case
    "And yet another Siskind bug now vanquished!"
    ;;; from QOBI on 22 May 2019.  His equation numbers.
    ;;; Equation (9)
    ;;; R->((R->R)->(R->R))
    (define s
      (lambda (u)
        (lambda (f)
          (lambda (x)
            (f (+ x u))))))
    ;;; a sample h:R->R
    (define (cube x) (* x (* x x)))
    ;;; boxed amazing bug
    ;;; boxes
    ;;; R->(box R)
    (define (box x) (lambda (m) (m x)))
    ;;; (box R)->R
    (define (unbox x) (x (lambda (x) x)))
    ;;; Wrap and unwrap a function R->R.
    ;;; same function, just takes boxes as input and returns boxes as output
    ;;; (R->R)->((box R)->(box R))
    (define (wrap f) (lambda (x) (box (f (unbox x)))))
    ;;; inverse of above
    ;;; ((box R)->(box R))->(R->R)
    (define (unwrap f) (lambda (x) (unbox (f (box x)))))
    ;;; Wrap a function (R->R)->(R->R).
    ;;; ((R->R)->(R->R))->(((box R)->(box R))->((box R)->(box R)))
    (define (wrap2 f)
      (lambda (g)
        (lambda (x)
          (box ((f (unwrap g))
                (unbox x))))))
    ;;; Wrap the result of a function R->((R->R)->(R->R)).
    ;;; (R->((R->R)->(R->R)))->(R->(((box R)->(box R))->((box R)->(box R))))
    (define (wrap2-result f)
      (lambda (x)
        (wrap2 (f x))))
    ;;; a wrapped variant of D-hat
    ;;; a wrapped version of equation (11)
    (define wrapped-d-hat ((derivative (wrap2-result s)) 0))
    ;;; a wrapped version of equation (12)
    ;;; These give the wrong answer (0) without double substitution on
    ;;; functions but gives the correct answer with substitution on
    ;;; functions.
    (check-simplified? (unbox ((((derivative (wrap2-result s)) 0) 
                                (((derivative (wrap2-result s)) 0)
                                 (wrap cube)))
                               (box 4)))
                       24)
    (check-simplified? (unbox 
                        ((wrapped-d-hat
                          (wrapped-d-hat (wrap cube)))
                         (box 4)))
                       24))
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))