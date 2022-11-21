#lang racket/base

(require rackunit
         (rename-in (only-in racket/base time) [time rkt:time])
         "../../main.rkt"
         "../helper.rkt"
         )

(define the-tests
  (test-suite
   "calculus/gram-schmidt"
   (test-case
    "Orthonormalizing with respect to the Lorentz metric in 2 dimensions."
    (define-coordinates (up t x) R2-rect)
    (define R2-point ((R2-rect '->point) (up 't0 'x0)))
    (define R2-basis (coordinate-system->basis R2-rect))
    (define ((L2-metric c) u v)
      (+ (* -1 c c (dt u) (dt v))
         (* 1 (dx u) (dx v))))
    (define L2-vector-basis (Gram-Schmidt (basis->vector-basis R2-basis) (L2-metric 'c)))
    (check-simplified? (accumulate pe
                                   (s:foreach (lambda (v)
                                                (pe ((v (literal-manifold-function 'f R2-rect))
                                                      R2-point)))
                                              L2-vector-basis))
                       '((/ (((partial 0) f) (up t0 x0)) c)
                         (((partial 1) f) (up t0 x0))))
    (check-simplified? (accumulate pe
                                   (s:foreach (lambda (omega)
                                                (pe ((omega (literal-vector-field 'v R2-rect))
                                                      R2-point)))
                                    (vector-basis->dual L2-vector-basis R2-rect)))
                       '((* c (v^0 (up t0 x0)))
                         (v^1 (up t0 x0)))))
   (test-case
    "4-dimensional Lorentz metric."
    (define SR R4-rect)
    (define-coordinates (up t x y z) SR)
    (define ((g-Lorentz c) u v)
      (+ (* (dx u) (dx v))
         (* (dy u) (dy v))
         (* (dz u) (dz v))
         (* -1 (square c) (dt u) (dt v))))
    (define SR-basis (coordinate-system->basis SR))
    (define an-event ((SR '->point) (up 't0 'x0 'y0 'z0)))
    (define SR-V (basis->vector-basis SR-basis))
    (define SR-V1 (ultra-flatten (Gram-Schmidt SR-V (g-Lorentz 'c))))
    ;;; SR-V1 is orthogonal
    (check-simplified? (rkt:time (accumulate pe
                                             (for-each (lambda (v1)
                                                         (for-each (lambda (v2)
                                                                     (pe (((g-Lorentz 'c) v1 v2) an-event)))
                                                                   (cdr (memq v1 SR-V1))))
                                                       SR-V1)))
                       '(0 0 0 0 0 0))
    ;;; SR-V1 is normal
    (check-simplified? (rkt:time (accumulate pe
                                             (for-each (lambda (v)
                                                         (pe (((g-Lorentz 'c) v v) an-event)))
                                                       SR-V1)))
                       '(-1 1 1 1))
    (check-simplified? (rkt:time (accumulate pe
                                             (for-each (lambda (v)
                                                         (pe ((v (SR '->coords))
                                                              an-event)))
                                                       SR-V1)))
                       '((up (/ 1 c) 0 0 0)
                         (up 0 1 0 0)
                         (up 0 0 1 0)
                         (up 0 0 0 1))))
   (test-case
    "..."
    (define-coordinates (up x y z) R3-rect)
    (define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (define R3-basis (coordinate-system->basis R3-rect))
    (define ((g3-maker a b c d e f) v1 v2)
      (+ (* a (dx v1) (dx v2))
         (* b (dx v1) (dy v2))
         (* c (dx v1) (dz v2))
         (* b (dx v2) (dy v1))
         (* d (dy v1) (dy v2))
         (* e (dy v1) (dz v2))
         (* c (dx v2) (dz v1))
         (* e (dy v2) (dz v1))
         (* f (dz v1) (dz v2))))
    (define g3 (g3-maker 'a 'b 'c 'd 'e 'f))
    (check-simplified? (accumulate pe
                                   (for-each (lambda (v)
                                               (pe ((v (R3-rect '->coords))
                                                    R3-point)))
                                             (ultra-flatten
                                              (Gram-Schmidt
                                               (basis->vector-basis R3-basis)
                                               g3))))
                       '((up (/ 1 (sqrt a)) 0 0)
                         (up (/ (* -1 b) (sqrt (+ (* (expt a 2) d) (* -1 a (expt b 2)))))
                             (/ a (sqrt (+ (* (expt a 2) d) (* -1 a (expt b 2)))))
                             0)
                         ;;; Third one is something awful...
                         (up
                          (/
                           (+ (* b e) (* -1 c d))
                           (sqrt
                            (+
                             (* (expt a 2) (expt d 2) f)
                             (* -1 (expt a 2) d (expt e 2))
                             (* -2 a (expt b 2) d f)
                             (* a (expt b 2) (expt e 2))
                             (* 2 a b c d e)
                             (* -1 a (expt c 2) (expt d 2))
                             (* (expt b 4) f)
                             (* -2 (expt b 3) c e)
                             (* (expt b 2) (expt c 2) d))))
                          (/
                           (+ (* -1 a e) (* b c))
                           (sqrt
                            (+
                             (* (expt a 2) (expt d 2) f)
                             (* -1 (expt a 2) d (expt e 2))
                             (* -2 a (expt b 2) d f)
                             (* a (expt b 2) (expt e 2))
                             (* 2 a b c d e)
                             (* -1 a (expt c 2) (expt d 2))
                             (* (expt b 4) f)
                             (* -2 (expt b 3) c e)
                             (* (expt b 2) (expt c 2) d))))
                          (/
                           (+ (* a d) (* -1 (expt b 2)))
                           (sqrt
                            (+
                             (* (expt a 2) (expt d 2) f)
                             (* -1 (expt a 2) d (expt e 2))
                             (* -2 a (expt b 2) d f)
                             (* a (expt b 2) (expt e 2))
                             (* 2 a b c d e)
                             (* -1 a (expt c 2) (expt d 2))
                             (* (expt b 4) f)
                             (* -2 (expt b 3) c e)
                             (* (expt b 2) (expt c 2) d))))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))