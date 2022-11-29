#lang racket/base

(require rackunit
         "../../../numerics/signals/fft.rkt"
         "../../../kernel/heuristic.rkt"
         )

(define 2pi (* 8 (atan 1 1)))
(define ftsg (make-transform-pair-GJS 16))
(define ftg (cadr ftsg))		; This gets the transform.
(define iftg (caddr ftsg))		; This gets the inverse transform.

(define ftsc (make-transform-pair-CPH 16))
(define ftc (cadr ftsc))		; This gets the transform.
(define iftc (caddr ftsc))		; This gets the inverse transform.

(define sig1 (m-cycles-cos-in-n-samples 2 16))
(define sig2 (m-cycles-sin-in-n-samples 2 16))


(define the-tests
  (test-suite
   "numerics/signals/fft"
   (check-within sig1
                 '(1 .7071067811865476 0 -.7071067811865475 -1. -.7071067811865477 0 .7071067811865474 1. .7071067811865477 0 -.7071067811865467 -1. -.7071067811865471 0 .7071067811865466)
                 1e-15)
   (check-within sig2
                 '(0 .7071067811865476 1 .7071067811865476 0 -.7071067811865476 -1 -.7071067811865476 0 .7071067811865476 1 .7071067811865476 0 -.7071067811865476 -1 -.7071067811865476)
                 1e-15)
   (check-within (ftg sig1)
                 '(0 0 1/2 0 0 0 0 0 0 0 0 0 0 0 1/2 0)
                 1e-15)
   (check-within (ftg sig2)
                 '(0 0 +1/2i 0 0 0 0 0 0 0 0 0 0 0 -1/2i 0)
                 1e-15)
   (check-within (ftc sig1)
                 '(0 0 1/2 0 0 0 0 0 0 0 0 0 0 0 1/2 0)
                 1e-15)
   (check-within (ftc sig2)
                 '(0 0 +1/2i 0 0 0 0 0 0 0 0 0 0 0 -1/2i 0)
                 1e-15)
   #;(test-case
    "General tests of transform"
    (define (fft-test period)
      (let ((data
             (make-initialized-list period
                                    (lambda (i)
                                      (- (random 2.0) 1.0))))
            (gjs (make-transform-pair-GJS period))
            (cph (make-transform-pair-CPH period)))
        (list (reduce + 0
                      (map square
                           (map -
                                ((cadr gjs) data)
                                ((cadr cph) data))))
              (reduce + 0
                      (map square
                           (map -
                                ((caddr gjs) data)
                                ((caddr cph) data))))
              (reduce + 0
                      (map square
                           (map -
                                data
                                ((compose (caddr cph) (cadr cph))
                                 data)))))))
    ;;what is the input?
#|
(6.819447099176773e-32-1.2583544774140102e-31i
 6.4492460489728735e-28+6.518949305520136e-28i
 -1.844996145931605e-28-6.601586323919541e-30i)
|#
    )
;;; General tests of transform.
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))