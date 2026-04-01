#lang racket/base

(require rackunit
         "../../poly/lagrange.rkt"
         "../../kernel/express.rkt"
         "../../simplify/pcf.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "poly/lagrange"
   (test-case
    "vector->vector-constructor"
    (check-equal? (vector->vector-constructor 1) 1)
    (check-equal? (vector->vector-constructor #(1 2)) '(vector 1 2))
    (check-equal? (vector->vector-constructor '(*down* #(1 2))) '(*down* #(1 2))))
   (test-case
    "make-linear-interpolator"
    (check-equal? (expression ((make-linear-interpolator values) 'x0 'x1 'v0 'v1))
                  '(/ (- (* v1 x0) (* v0 x1)) (- x1 x0)))
    (check-equal? (expression ((make-linear-interpolator (λ (x) (case x [(x0) '2][(x1) 5]))) 'x0 'x1 'v0 'v1))
                  '(/ (- (* 2 v1) (* 5 v0)) (- x1 x0)))
    (check-equal? (expression ((make-linear-interpolator (λ (x) (case x [(x0) '2][(x1) 5]))) 'x0 'x1 #(1 2) #(3 2)))
                  '(vector (/ 1 (- x1 x0)) (/ -6 (- x1 x0))))
    )
   (test-case
    "triangle-iterate"
    (check-unique-match? (triangle-iterate '(2 3 5 7) '(11 13 19 23)
                                           (λ (x0 x1 y0 y1) (vector (list x0 y0) (list x1 y1))))
                         (y53)
                         `(let ((,y53 #((3 13) (5 19))))
                            #((2 #((2 #((2 11) (3 13))) (5 ,y53))) (7 #((3 ,y53) (7 #((5 19) (7 23)))))))))
   (test-case
    "lagrange"
    (check-unique-match? (lagrange '(y) '(x))
                         (X Y)
                         `(lambda (,X) (let ([,Y (- ,X x)]) y)))
    (check-unique-match? (lagrange '(3) '(7))
                         (X Y)
                         `(lambda (,X) (let ([,Y (+ -7 ,X)]) 3)))
    (check-unique-match? (lagrange '(y) '(x0 x1))
                         (X Y1 Y2)
                         `(lambda (,X) (let ([,Y1 (- ,X x0)][,Y2 (- ,X x1)]) y)))
    (check-unique-match? (lagrange '(y0 y1) '(x0 x1))
                         (X Y1 Y2)
                         `(lambda (,X) (let ([,Y1 (- ,X x0)][,Y2 (- ,X x1)]) (/ (- (* y1 ,Y1) (* y0 ,Y2)) (- x1 x0)))))
    (check-unique-match? (lagrange '(y1 y2 y3 y4) '(x1 x2 x3 x4))
                         (x47 y48 y49 y50 y51 y52)
                         `(lambda (,x47)
                            (let ((,y48 (- ,x47 x1)) (,y49 (- ,x47 x2)) (,y50 (- ,x47 x3)) (,y51 (- ,x47 x4)))
                              (let ((,y52 (/ (- (* y3 ,y49) (* y2 ,y50)) (- x3 x2))))
                                (/ (- (/ (* (- (/ (* (- (* y4 ,y50) (* y3 ,y51)) ,y49)
                                                  (- x4 x3))
                                               (* ,y52 ,y51))
                                            ,y48)
                                         (- x4 x2))
                                      (/ (* (- (* ,y52 ,y48) (/ (* (- (* y2 ,y48) (* y1 ,y49)) ,y50)
                                                                (- x2 x1)))
                                            ,y51)
                                         (- x3 x1)))
                                   (- x4 x1)))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))