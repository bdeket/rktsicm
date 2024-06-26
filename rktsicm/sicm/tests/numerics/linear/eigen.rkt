#lang s-exp "../../../main.rkt"

(require rackunit
         "../../../numerics/linear/eigen.rkt"
         "../../../numerics/linear/lu.rkt"
         "../../helper.rkt"
         )


(provide the-tests)
(define the-tests
  (test-suite
   "numerics/linear/eigen"
   (test-case
    "1"
    ;;; For example, this system has 3 distinct eigenvalues and
    ;;; corresponding eigenvectors:
    (check-within (matrix->eigenvalues-eigenvectors
                   (matrix-by-rows '(2 -1 0)
                                   '(-1 2 -1)
                                   '(0 -1 2)))
                  '((.585786437626913 #(.5000000000000014 .7071067811865455 .5000000000000014))
                    (2. #(-.7071067811865475 0. .7071067811865475))
                    (3.414213562373087 #(.5000000000000014 -.7071067811865455 .5000000000000014)))
                  1e-15))
   (test-case
    "2"
    ;;; For systems with multiplicity the multiple eigenvectors appear
    ;;; with the multiple root:
    (check-equal? (matrix->eigenvalues-eigenvectors
                   (matrix-by-rows '(0 0 0)
                                   '(0 1 0)
                                   '(0 0 1)))
                  '((0 #(1 0 0)) (1. #(0 0 1) #(0 1 0)))))

   (test-case
    "hyperbolic point"
    ;;; A real example: the standard map at a hyperbolic point.
    (check-within (matrix->eigenvalues-eigenvectors
                   (A^m_n->Mmn
                    (let ((K 1))
                      ((derivative (lambda (v)
                            (let ((x (ref v 0))
                                  (y (ref v 1)))
                              (let ((yp  (+ y (* K (sin x)))))
                                (up (+ x yp) yp)))))
                       (up 0 0)))))
                  '((.38196601125010315 #(-.525731112119133 .8506508083520402))
                    (2.6180339887498967 #(.8506508083520401 .5257311121191331)))
                  1e-12))
   (test-case
    "Pavel's test"
    (check-equal? (matrix->eigenvalues-eigenvectors
                   (matrix-by-rows '(1 1)
                                   '(0 1)))
                  '((1. #(1 0)))))
   (test-case
    "4"
    (check-within (matrix->eigenvalues-eigenvectors
                   (matrix-by-rows '(13 -4 2)
                                   '(-4 13 -2)
                                   '(2 -2 10)))
                  '((9. #(-.4472135954999579 0. .8944271909999159)
                        #(.7071067811865475 .7071067811865475 0.))
                    (17.999999999999954
                     #(.6666666666666701 -.6666666666666624 .33333333333333504)))
                  1e-12))
   (test-case
    "5"
    (check-within (matrix->eigenvalues-eigenvectors
                   (matrix-by-rows '(1 2 3)
                                   '(4 5 6)
                                   '(7 8 9)))
                  '((0.
                     #(.4082482904638631 -.8164965809277261 .4082482904638631))
                    (-1.1168439698070243
                     #(-.7858302387420639 -.08675133925663416 .6123275602288135))
                    (16.116843969807025
                     #(.2319706872462861 .5253220933012344 .8186734993561811)))
                  1e-12))
   (test-case
    "6"
    (check-within (matrix->eigenvalues-eigenvectors
                   (matrix-by-rows '(2 0 0 0)
                                   '(1 2 0 0)
                                   '(0 0 2 0)
                                   '(0 0 0 2)))
                  '((2. #(0 0 0 1) #(0 0 1 0) #(0 1 0 0)))
                  1e-15))
   (test-case
    "7"
    (check-within (matrix->eigenvalues-eigenvectors
                   (matrix-by-rows '(2 0 0 0)
                                   '(1 2 0 0)
                                   '(0 0 2 0)
                                   '(0 0 1 2)))
                  '((2. #(0 0 0 1) #(0 1 0 0)))
                  1e-15))
   (test-case
    "8"
    (define A
      (matrix-by-rows (list 8 0 3/16)
                      (list 0 8 3/16)
                      (list 3/16 3/16 8)))
    ;; TODO BUG
    (check-within (matrix->eigenvalues-eigenvectors A)
                  '((7.7348349570559485) (7.9999999999982006) (8.265165042945835))
                  #;'((7.7348349570559485 #(1 0 1))
                     (7.9999999999982006 #(.7071 1 -.7071))
                     (8.265165042945835 #(.7071 -1 -.7071)))
                  1e-10)
    (check-within (lu-null-space
                   (g:- A
                        (g:* 8 (m:make-identity (m:dimension A)))))
                  '(#(-.7071067811865475 .7071067811865475 0.))
                  1e-15)
    (check-equal? (lu-null-space
                   (g:- A
                        (g:* 7.9999999999982006 (m:make-identity (m:dimension A)))))
                  '())
    (check-within (map car (real-matrix->eigenvalues-eigenvectors A))
                  '(7.7348349570559485 7.9999999999982006 8.265165042945835)
                  1e-10)
    (check-within (real-matrix->eigenvalues-eigenvectors A (* *machine-epsilon* 1e4))
                  '((7.7348349570559485 #(.5 .49999999999999994 -.7071067811865475))
                    (7.9999999999982006 #(-.7071067811865475 .7071067811865475 3.465221769689776e-27))
                    (8.265165042945835 #(-.49999999999999994 -.49999999999999967 -.7071067811865474)))
                  1e-10)
    ;;; Indeed 
    (check-within (/ (* A (vector -1/2 -1/2 (/ (sqrt 2) 2))) 7.7348349570559485)
                  #(-.49999999999994155 -.49999999999994155 .707106781186465)
                  1e-15)
    ;;; And, indeed:
    (heuristic-zero-test-bugger-factor 1e-11)
    (check-within (matrix->eigenvalues-eigenvectors A)
                  '((7.7348349570559485
                     #(-.5000000000008521 -.5000000000008521 .7071067811853424))
                    (7.9999999999982006 #(-.7071067811865475 .7071067811865475 0.))
                    (8.265165042945835
                     #(.49999999999917066 .49999999999917066 .7071067811877204)))
                  1e-10)
    ;;; Problem is roots are not good enuf for null-space test
    (check-simplified? (determinant (- A (* 'lam (m:identity-like A))))
                       '(+ (* -1 (expt lam 3)) (* 24 (expt lam 2)) (* -24567/128 lam) 8183/16))
    (check-equal? ((lambda (lam) 
                     (+ (* -1 (expt lam 3)) (* 24 (expt lam 2)) (* -24567/128 lam) 8183/16))
                   8)
                  0)
    (check-= ((lambda (lam) 
                (+ (* -1 (expt lam 3)) (* 24 (expt lam 2)) (* -24567/128 lam) 8183/16))
              7.9999999999982006)
             -1.1368683772161603e-13
             1e-15)
    ;;; ugh!
    ;;; but I can generally not do better:
    (check-within (zbrent (lambda (lam) 
                       (+ (* -1 (expt lam 3)) (* 24 (expt lam 2)) (* -24567/128 lam) 8183/16))
                     7.9999999999982006
                     8.0003)
                  '(#t 8.000000000001434 3)
                  1e-11))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))