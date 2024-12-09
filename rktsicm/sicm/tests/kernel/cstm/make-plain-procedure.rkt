#lang racket/base

(require rackunit
         "../../../kernel/cstm/arity.rkt"
         (submod "../../../kernel/cstm/make-plain-procedure.rkt" ALL))

(define (check-arity F A)
  (check-equal? (procedure-arity (F A)) A)
  (let loop ([A A])
    (cond
      [(number? A)
       (define ARG (build-list A (λ (i) (random (+ i 1)))))
       (check-equal? (apply (F A) ARG) ARG)]
      [(arity-at-least? A)
       (define ARG (build-list (+ (arity-at-least-value A) (random 5))
                               (λ (i) (random (+ i 1)))))
       (check-equal? (apply (F A) ARG) ARG)]
      [(list? A)
       (map loop A)]
      [else
       (error "check-arity" F A)])))
(define (check-arity-exn F [name 'fct])
  (check-exn (pregexp (format "~a: contract violation\n  expected: procedure-arity\\?\n" name))
             (λ () (F 'wrong))))

(define (std-arity-check F)
  (check-arity F (exact-arity 0))
  (check-arity F (exact-arity 1))
  (check-arity F (exact-arity 2))
  (check-arity F (exact-arity 3))
  (check-arity F (arity-at-least 0))
  (check-arity F (arity-at-least 1))
  (check-arity F (arity-at-least 2))
  (check-arity F (arity-at-least 3))
  (check-arity F '(1 2))
  (check-arity F '(2 5 8))
  (check-arity F (list 2 (arity-at-least 5)))
  (check-arity-exn F))

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/cstm/make-plain-procedure"
   (test-case
    "make-plain-procedure"
    (define (F A) (make-plain-procedure 'fct (λ vs (apply list vs)) A))
    (std-arity-check F))
   (test-case
    "make-plain-procedure-stx"
    (define (F A)
      (make-plain-procedure-stx 'fct
                                (λ (x) #`(#,list #,@x))
                                (λ (x y) #`(#,apply #,list #,@x #,y))
                                A))
    (std-arity-check F))
   (test-case
    "plain-procedure-slct"
    ;; not exported!
    ; default -> make-plain-procedure
    (define (F A) (plain-procedure-slct 'fct (λ v (apply list v)) A))
    (std-arity-check F))
   (test-case
    "make-plain-procedure-slct+"
    ;; not exported!
    (define (F A) (make-plain-procedure-slct+ 'fct
                                              (λ (F)
                                                (F (λ (x y)
                                                     (if y
                                                         #`(#,apply #,list #,@x #,y)
                                                         #`(#,list #,@x)))
                                                   #`(#,make-plain-procedure 'fct
                                                                           (λ x (#,apply #,list x))
                                                                           '#,A)
                                              A))))
    (std-arity-check F))
   (test-case
    "make-plain-procedure-slct"
    (define (F A) (make-plain-procedure-slct 'fct
                                             A
                                             (λ (x)   #`(#,list #,@x))
                                             (λ (x y) #`(#,apply #,list #,@x #,y))
                                             (λ (x)   #`(#,list #,@x))
                                             (λ (x y) #`(#,apply #,list #,@x #,y))))
    ;default -> make-plain-procedure-stx
    (std-arity-check F)
    )))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))