#lang racket/base

(require rackunit
         racket/port
         racket/logging
         "../../simplify/matcher.rkt"
         "../../simplify/rule-simplifier.rkt"
         (only-in "../../rkt/racket-help.rkt" rktsicm-logger)
         "../helper.rkt")

(define (+rule x) (and (list? x)
                        (eq? (car x) '+)
                        (andmap number? (cdr x))
                        (apply + (cdr x))))
(provide the-tests)
(define the-tests
  (test-suite
   "simplify/rule-simplifier"
   (test-case
    "try-rules"
    (check-equal? (try-rules 'expression '()) #f)
    (check-equal? (try-rules '(+ 1 2) (list +rule))
                  3)
    (check-equal? (try-rules '(+ 1 2) (list number? +rule error))
                  3))
   (test-case
    "rule-simplifier"
    (check-equal? ((rule-simplifier 'the-rules) 'expression)
                  'expression)
    (check-equal? ((rule-simplifier (list +rule)) '(+ 3 4))
                  7)
    (check-equal? ((rule-simplifier (list +rule)) '(+ (+ 1 2) 4))
                  7)
    (check-equal? ((rule-simplifier (list +rule)) '(* (+ 1 2) 4))
                  '(* 3 4))
    ;; TODO : ? make it something we can turn on/off?
    (check-equal? (call-with-output-string
                   (λ (out)
                     (with-logging-to-port out
                                           (λ () ((rule-simplifier (list +rule)) '(* (+ 1 2) 4)))
                                           #:logger rktsicm-logger
                                           'debug)))
                  "rktsicm: rule-memoize: not memoizing - #<procedure:simplify-expression>\n"))
   (test-case
    "match:extract-segment"
    (define end '(to end))
    (define begin (append '(be gin ning) end))
    (check-equal? (match:extract-segment (match:make-segment begin '()))
                  '(be gin ning to end))
    (check-equal? (match:extract-segment (match:make-segment begin end))
                  '(be gin ning))
    (check-exn #px"" ;; it should only work when end is eq? !
               (λ () (match:extract-segment (match:make-segment '(be gin ning to end) '(to end))))))
   (test-case
    "rule:make"
    (check-equal? ((rule:make 'pattern vector) 'expression)
                  #f)
    (check-equal? ((rule:make 'pattern vector) 'pattern)
                  #())
    (check-equal? ((rule:make '(? b) vector) 'pattern)
                  #(pattern))
    (check-equal? ((rule:make '(sqrt (expt (? b) 2)) (λ (b) b)) '(sqrt (expt x 2)))
                  'x)
    (check-equal? ((rule:make `(+ (? a ,number?) (?? bs)) (λ (bs a) (apply + a bs))) '(+ 1 2 3))
                  6)
    (check-equal? ((rule:make `(+ (? a ,number?) (?? bs)) (λ (bs a) (apply + a bs))) '(+ one 2 3))
                  #f))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))