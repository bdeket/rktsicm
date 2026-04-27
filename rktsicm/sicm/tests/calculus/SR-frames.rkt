#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../calculus/SR-frames.rkt"
         "../helper+scm.rkt"
         )

(define :c (*c*))

(provide the-tests)
;; in below use (up...) instead of a fixed #(0 0 0 0)
;; if tests are run multiple times the second wil be always eq? running
;; into problems with ownership (see frame-maker: claim!)
;; TODO: this file very seldom throws following error: "stream: reentrant or broken delay"
;; not yet sure what is the cause, for now only triggered when run from
;; command-line, in parallel with other tests
(define the-tests
  (test-suite
   "calculus/SR-frames"
   (test-case
    "SR-coordinates"
    (define X (make-SR-coordinates the-ether (up 0 1 2 3)))
    (check-true (SR-coordinates? X))
    (check-equal? (SR-name X) 'the-ether)
    (check-exn #px"assertion failed: \\(vector\\? 4tuple\\)" (λ () (make-SR-coordinates 'mine 'not-a-4tuple)))
    (check-exn #px"assertion failed: \\(fix:= \\(vector-length 4tuple\\) 4\\)" (λ () (make-SR-coordinates 'mine (up 1 2 3)))))
   (test-case
    "make-SR-frame"
    (define this (make-SR-frame 'this the-ether 'dir 'v/c (up 0 0 0 0)))
    (check-true (frame? this))
    (check-equal? (frame-name this) 'this)
    (check-equal? (ancestor-frame this) the-ether)
    (check-equal? (boost-direction this) 'dir)
    (check-equal? (v/c this) 'v/c)
    (check-equal? (coordinate-origin this) (up 0 0 0 0)))
   (test-case
    "the-ether"
    (check-true (frame? the-ether))
    (define a (make-SR-coordinates the-ether (up 0 0 0 0)))
    (check-true (SR-coordinates? a))
    (check-false (event? a))
    (define b ((coords->event the-ether) a))
    (define c ((event->coords the-ether) b))
    (check-true (event? b))
    (check-true (SR-coordinates? c))
    ;; probably just implementation artifact ...
    (check-true (eq? a b))
    (check-true (eq? a c))
    (let ([not-a-SR-coord 'not-a-coord])
      (claim! not-a-SR-coord the-ether)
      (check-exn #px"assertion failed: \\(SR-coordinates\\? coords\\)"
                 (λ () ((coords->event the-ether) not-a-SR-coord))))
    ;; unreachable: same check in frame-maker
    (check-exn #px"assertion failed: \\(eq\\? \\(frame-owner coords\\) this-frame\\)"
               (λ () ((coords->event the-ether) (make-SR-coordinates 'not-ether (up 0 0 0 0)))))
    ;; unreachable: same check in frame-maker
    (check-exn #px"assertion failed: \\(event\\? event\\)"
               (λ () ((event->coords the-ether) 'not-an-event))))
   
   (test-case
    "coordinates->event"
    (define dir (up 1 0 0)) (define v/c 'v)
    (define org (make-SR-coordinates the-ether (up 0 0 0 0)))
    (define this (make-SR-frame 'this the-ether dir v/c org))
    (define C0 (make-SR-coordinates this (up 1 2 3 4)))
    (define ans '(up (/ (+ 1 (* 2 v)) (sqrt (+ 1 (* -1 (expt v 2)))))
                     (/ (+ 2 v) (sqrt (+ 1 (* -1 (expt v 2)))))
                     3   4))
    (check-simplified? ((coordinates->event the-ether this dir v/c org) C0)
                       ans)
    (check-simplified? ((coords->event this) C0)
                       ans)
    (check-true (event? ((coordinates->event the-ether this dir v/c org) C0)))
    (check-exn #px"assertion failed: \\(eq\\? \\(frame-owner origin\\) ancestor-frame\\)"
               (λ () (coordinates->event the-ether this 'dir 'v/c 'unknown-origin)))
    (check-exn #px"assertion failed: \\(SR-coordinates\\? coords\\)"
               (λ () ((coordinates->event the-ether this (up 1 0 0) 'x org) 'C0))))
   (test-case
    "event->coordinates"
    (define dir (up 1 0 0)) (define v/c 'v)
    (define org (make-SR-coordinates the-ether (up 0 0 0 0)))
    (define this (make-SR-frame 'this the-ether dir v/c org))
    (define E0 (make-event (up 1 2 3 4)))
    (define ans '(up (/ (+ 1 (* -2 v)) (sqrt (+ 1 (* -1 (expt v 2)))))
                     (/ (+ 2 (* -1 v)) (sqrt (+ 1 (* -1 (expt v 2)))))
                     3   4))
    (check-simplified? ((event->coordinates the-ether this dir v/c org) E0)
                       ans)
    (check-simplified? ((event->coords this) E0)
                       ans)
    (check-true (SR-coordinates? ((event->coordinates the-ether this dir v/c org) E0)))
    (check-exn #px"assertion failed: \\(eq\\? \\(frame-owner origin\\) ancestor-frame\\)"
               (λ () (event->coordinates the-ether this 'dir 'v/c 'unknown-origin)))
    (check-exn #px"assertion failed: \\(event\\? event\\)"
               (λ () ((event->coordinates the-ether this (up 1 0 0) 'x org) 'C0))))

   (test-case
    "add-v/cs | velocities"
    (check-equal? (add-v/cs 'v1/c 'v2/c)
                  (/ (+ 'v1/c 'v2/c) (+ 1 (* 'v1/c 'v2/c))))
    (check-equal? (add-velocities 'v1 'v2)
                  (/ (+ 'v1 'v2) (+ 1 (* (/ 'v1 (*c*)) (/ 'v2 (*c*)))))))
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