#lang racket/base

(require rackunit
         (submod "../../general/stack-queue.rkt" ALL))

(provide the-tests)
(define the-tests
  (test-suite
   "general/stack-queue"
   (test-case
    "make-stack&queue"
    (check-true (stack&queue-empty? (make-stack&queue)))
    (check-equal? (stack&queue-front (make-stack&queue)) '())
    (check-equal? (stack&queue-back (make-stack&queue)) '())
    )
   (test-case
    "pop! empty"
    (check-exn #px"^Empty stack&queue" (λ () (pop! (make-stack&queue)))))
   (test-case
    "pop!-1 push"
    (define stq (make-stack&queue))
    (push! stq 1)
    (check-equal? (pop! stq) 1))
   (test-case
    "pop!-1 end"
    (define stq (make-stack&queue))
    (add-to-end! stq 1)
    (check-equal? (pop! stq) 1))
   (test-case
    "pop!-2 push"
    (define stq (make-stack&queue))
    (push! stq 1)
    (push! stq 2)
    (check-equal? (pop! stq) 2)
    (check-equal? (pop! stq) 1))
   (test-case
    "pop!-2 push"
    (define stq (make-stack&queue))
    (add-to-end! stq 1)
    (add-to-end! stq 2)
    (check-equal? (pop! stq) 1)
    (check-equal? (pop! stq) 2))
   (test-case
    "pop! mixed"
    (define stq (make-stack&queue))
    (push! stq 2)
    (add-to-end! stq 3)
    (push! stq 1)
    (add-to-end! stq 4)
    (check-equal? (pop! stq) 1)
    (check-equal? (pop! stq) 2)
    (check-equal? (pop! stq) 3)
    (check-equal? (pop! stq) 4))
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))