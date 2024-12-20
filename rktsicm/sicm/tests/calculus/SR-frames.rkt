#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../calculus/SR-frames.rkt"
         "../helper.rkt"
         )

(define :c (*c*))

(provide the-tests)
;; in below use (up...) instead of a fixed #(0 0 0 0)
;; if tests are run multiple times the second wil be always eq? running
;; into problems with ownership (see frame-maker: claim!)
;; this file very seldom throws following error: "stream: reentrant or broken delay"
;; not yet sure what is the cause, for now only triggered when run from
;; command-line, in parallel with other tests
(define the-tests
  (test-suite
   "calculus/SR-frames"
   (test-case
    "Velocity addition formula"
    ;;(symbolic-constants #f)
    ;;(set! *divide-out-terms* #f)
    (define A
      (make-SR-frame 'A the-ether
                     (up 1 0 0)
                     (/ 'va :c)
                     (make-SR-coordinates the-ether
                                          (up 0 0 0 0))))
    (define B
      (make-SR-frame 'B A
                     (up 1 0 0)
                     (/ 'vb :c)
                     (make-SR-coordinates A
                                          (up 0 0 0 0))))
    (check-simplified? (let ((foo ((chart the-ether)
                                   ((point B)
                                    (make-SR-coordinates B
                                                         (up (* :c 'tau) 0 0 0))))))
                         (/ (ref foo 1) (/ (ref foo 0) :c)))
                       `(/ (+ va vb)
                           (+ 1 (* va vb (expt ,:c -2))))))
   (test-case
    "Simple test of reversibility"
    (define A
   (make-SR-frame 'A the-ether (up 1 0 0) 'va/c
                  (make-SR-coordinates the-ether (up 'cta 'xa 'ya 'za))))
    (check-simplified? ((chart A)
                        ((point A)
                         (make-SR-coordinates A (up 'ct 'x 'y 'z))))
                       '(up ct x y z))
    ;;; The ether coordinates of the origin of A relative to "the ether"
    ;;; is
    (define origin-A (coordinate-origin A))
    (check-simplified? (frame-name (frame-owner origin-A))
                       'the-ether)
    (define B (make-SR-frame 'B A (up 1 0 0) 'vba/c
                             (make-SR-coordinates A (up 'ctba 'xba 'yba 'zba))))
    (check-simplified? ((chart B)
                        ((point B)
                         (make-SR-coordinates B
                                              (up 'ct 'x 'y 'z))))
                       '(up ct x y z)))
   (test-case
    "Poincare formula"
    (define A
      (make-SR-frame 'A the-ether (up 1 0 0) 'va/c
                     (make-SR-coordinates the-ether (up 'cta 'xa 'ya 'za))))
    (define B
      (make-SR-frame 'B A (up 1 0 0) 'vba/c
                     (make-SR-coordinates A (up 'ctba 'xba 'yba 'zba))))
    ;;; The ether coordinates of the origin of B relative to "the ether"
    ;;; is
    (define origin-B
      ((chart the-ether)
       ((point A)
        (coordinate-origin B))))
    (check-simplified? origin-B
                       '(up
                         (/ (+ (* cta (sqrt (+ 1 (* -1 (expt va/c 2))))) (* va/c xba) ctba)
                            (sqrt (+ 1 (* -1 (expt va/c 2)))))
                         (/ (+ (* ctba va/c) (* xa (sqrt (+ 1 (* -1 (expt va/c 2))))) xba)
                            (sqrt (+ 1 (* -1 (expt va/c 2)))))
                         (+ ya yba)
                         (+ za zba)))
    (define C
      (make-SR-frame 'C the-ether
                     (up 1 0 0)
                     (add-v/cs 'va/c 'vba/c)
                     origin-B))
    (check-simplified? C 'this-frame)
    ;;; A typical event.
    (define foo
      ((point the-ether)
       (make-SR-coordinates the-ether
                            (up 'ct 'x 'y 'z))))
    (check-simplified? foo '(up ct x y z)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))