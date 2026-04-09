#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/frame-maker"
   (test-case
    "frame-maker"
    (define maker (frame-maker 'X 'Y))
    (check-true (procedure? maker))
    (define frame (maker 'this 'father 'para 'meter))
    (check-true (procedure? frame))
    (check-false (frame 'manifold))
    ;; don't test the message passing style if not needed
    (check-equal? (frame-name frame) 'this)
    (check-equal? (ancestor-frame frame) 'father)
    (check-equal? (frame-params frame) '(para meter))
    (check-exn #px"Unknown message: " (λ () (frame 'bad-message))))
   (test-case
    "coords<->event"
    (define frame ((frame-maker (λ x (λ (c) (make-event `(-> ,@x ,c))))
                                (λ x (λ (e) (claim! `(<- ,@x ,e) (cadr x)))))
                   'this 'father 'para 'meter))
    (define coord (up 'x 'y))
    (claim! coord frame)
    (check-equal? (frame-owner coord) frame)
    (check-equal? ((coords->event frame) coord) `(-> father ,frame para meter #(x y)))
    (check-exn #px"assertion failed: \\(eq\\? \\(frame-owner coords\\) this-frame\\)"
               (λ () ((coords->event frame) (up 'x 'y))))
    (define E ((coords->event frame) coord))
    (check-equal? ((event->coords frame) E) `(<- father ,frame para meter ,E))
    (check-exn #px"assertion failed: \\(event\\? event\\)"
               (λ () ((event->coords frame) 'any))))
   (test-case
    "not claming is bad"
    (define frame ((frame-maker (λ x (λ (c) `(-> ,@x ,c)))
                                (λ x (λ (e) `(<- ,@x ,e))))
                   'this 'father 'para 'meter))
    (define coord (claim! (up 'x 'y) frame))
    (check-exn #px"assertion failed: \\(event\\? event\\)" (λ () ((coords->event frame) coord)))
    (define E (make-event 'any))
    (check-exn #px"assertion failed: \\(eq\\? \\(frame-owner coords\\) this-frame\\)" (λ () ((event->coords frame) E))))
   (test-case
    "double claiming is also bad"
    (define C (claim! (up 'x 'y) 'Mr1))
    (check-exn #px"Someone else owns these coords" (λ () (claim! C 'Mr2))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))