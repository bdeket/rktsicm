#lang s-exp "../../generic.rkt"

(require rackunit)

(require "../../solve.rkt"
         "../../general/list-utils.rkt")

(define-syntax-rule (hypothetical x) (make-hypothetical 'x #f))
;***************************************************************************************************
;* from solve-utils.rkt                                                                            *
;***************************************************************************************************
;;; Examples of use
(provide the-tests)
(define the-tests
  (test-suite
   "solve/solve-utils"
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
