#lang racket/base

(require rackunit
         (submod "../../general/gjs-cselim.rkt" ALL)
         "../../general/list-utils.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "general/gjs-cselim"
   (check-unique-match? (gjs/cselim '(+ (* x 3) (- x y) (* x 3)))
                        (G306)
                        `(let ((,G306 (* x 3)))
                           (+ ,G306 (- x y) ,G306)))
   (check-unique-match? (gjs/cselim
                         '(lambda (x)
                            (/ (+ (* x 3) (- y z) (- x y) (* x 3))
                               (- y z))))
                        (G300 G301)
                        `(let ((,G301 (- y z)))
                           (lambda (x)
                             (let ((,G300 (* x 3)))
                               (/ (+ ,G300 ,G301 (- x y) ,G300)
                                  ,G301)))))
   (check-unique-match? (gjs/cselim
                         '(let ((x 32) (y 5))
                            (+ (* x 3) (- x y) (* x 3))))
                        (G296)
                        `(let ((x 32) (y 5))
                           (let ((,G296 (* x 3)))
                             (+ ,G296 (- x y) ,G296))))
   (check-unique-match? (gjs/cselim
                         '(up
                           (+ (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
                              (/ (* -1/2 (expt dt 3) (expt p_phi_0 2) p_r_0) (* (expt m 3) (expt r_0 4))))
                           (+ (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))
                              (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
                              (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))
                         (fringe-smaller-than? 7))
                        (G44125 G44128)
                        `(let ((,G44125 (expt dt 3)) (,G44128 (* (expt m 3) (expt r_0 4))))
                           (up
                            (+ (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
                               (/ (* -1/2 ,G44125 (expt p_phi_0 2) p_r_0) ,G44128))
                            (+ (/ (* ,G44125 p_phi_0 (expt p_r_0 2)) ,G44128)
                               (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
                               (/ (* -1/3 ,G44125 (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))))
   ;******************************
   (test-case
    "gjs/cselim"
    ;; symbols & quoted items are returned as is
    (check-exn #px"Variable not present" (λ ()(gjs/cselim 'x)))
    (check-equal? (gjs/cselim ''x) ''x)
    (check-equal? (gjs/cselim ''1) ''1)
    (check-equal? (gjs/cselim '(quote 1)) ''1)
    (check-equal? (gjs/cselim '(quote 1 2 3)) '(quote 1 2 3))
    (check-equal? (gjs/cselim '1) 1)
    (check-equal? (gjs/cselim '(x)) '(x))
    ;; internal bindings for "lambda" and "let" are preserved
    (check-equal? (gjs/cselim '(+ (* x y) (lambda (x y) (* x y)))) '(+ (* x y) (lambda (x y) (* x y))))
    (check-equal? (gjs/cselim '(+ (* x y) (λ (x y) (* x y)))) '(+ (* x y) (lambda (x y) (* x y))))
    (check-equal? (gjs/cselim '(+ (* x x) (λ x (* x x)))) '(+ (* x x) (lambda x (* x x))))
    (check-equal? (gjs/cselim '(+ (* x y) (let ((x y)) (* x y)))) '(+ (* x y) (let ((x y)) (* x y))))
    ;; named lets are not implemented
    (check-exn #px"Not implemented" (λ () (gjs/cselim '(+ (* x y) (let lp ((x y)) (* x y))))))
    
    (skip "NEXT TEST ARE INCORRECT but in line with scmutils")
    ;; for... let* let-values ... are not handled correctly!!
    (check-unique-match? (gjs/cselim '(+ (* x y) (for ((x y)) (* x y))))
                         (q)
                         `(let ((,q (* x y)))(+ ,q (for ((x y)) ,q))))
    ;; let with multiple bodies not handled correctly !!
    (check-equal? (gjs/cselim '(+ (* x y) (let () (define x 6) (* x y)))) '(+ (* x y) (define x 6)))
    ;; inner defines are not handled correctly !!
    (check-unique-match? (gjs/cselim '(+ (* x y) (let () (begin (define x 6) (* x y)))))
                         (q)
                         `(let ([,q (* x y)]) (+ ,q (begin (define x 6) ,q)))))
   
   (test-case
    "occurs-in?"
    (check-true (occurs-in? 'x '(list x y)))
    (check-true (occurs-in? 'x '(+ 1 (* y (expt 2 x)))))
    (check-false (occurs-in? 'z '(+ 1 (* y (expt 2 x)))))
    (check-not-false (occurs-in? '(z x y) '(+ 1 (* y (expt 2 x))))))
   (test-case
    "make-let-expression"
    (check-equal? (make-let-expression '((x 1)) '(list y)) '(list y))
    (check-equal? (make-let-expression '((x 1)(z 2)) '(list x y)) '(let ((x 1))(list x y)))
    (check-equal? (make-let-expression '() '(list x y)) '(list x y)))
   (test-case
    "make-canonical-lets"
    (check-equal? (make-canonical-lets '() '(list x y)) '(list x y))
    (check-equal? (make-canonical-lets '((x 1)) '(list y)) '(let ((x 1))(list y)))
    (check-equal? (make-canonical-lets '((x 1)(z 2)) '(list x y)) '(let ((x 1)(z 2))(list x y)))
    (check-equal? (make-canonical-lets '((x y)(y 2)) '(list x y)) '(let ((y 2))(let ((x y)) (list x y))))
    (check-exn #px"variables interdependent" (λ () (make-canonical-lets '((x (* 3 y))(y (+ 2 x))) '(list x y)))))
   
   (test-case
    "expression-recorder"
    (define X (make-expression-recorder #f '()))
    (define a (record-expression! X '(+ x y) '()))
    (check-equal? (variable->expression X a) '(+ x y))
    (define b (record-expression! X (list '+ 'x 'y) '()))
    (check-true (eq? a b))
    (check-equal? (expressions-seen X) `((,a (+ x y))))
    (define Y (make-expression-recorder X '(A B)))
    (define c (record-expression! Y 'A '()))
    (define e (record-expression! Y 'A '()))
    (define f (record-expression! Y 'B '()))
    (check-true (eq? c e))
    (check-equal? (variable->expression Y c) 'A)
    (check-exn #px"Variable not present" (λ () (variable->expression X c)))
    (define d (record-expression! Y 'D '()))
    (check-equal? (variable->expression Y d) 'D)
    (check-equal? (variable->expression X d) 'D)
    (check-equal? (expressions-seen X) `((,a (+ x y))))
    (check-equal? (expressions-seen Y) `((,c A)))
    ;; ignored-variables in record-expression! are 'ignored' ?!?
    (define g (record-expression! Y 'a #f))
    (check-equal? (record-expression! Y 'a '()) g)
    (check-equal? (record-expression! Y 'a 4) g)
    (check-equal? (record-expression! Y 'a +) g)
    (check-equal? (expressions-seen Y) `((,c A)))
    (check-equal? (expressions-seen X) `((,a (+ x y))(,g a))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))