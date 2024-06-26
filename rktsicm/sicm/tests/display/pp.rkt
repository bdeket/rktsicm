#lang racket/base

(require rackunit
         racket/port
         "../../display/pp.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "display/pp"
   (test-case
    "wallp-pp"
    (check-equal? (call-with-output-string (λ (out)
                                             (parameterize ([current-output-port out])
                                               (wallp-pp #t 1 2 3))))
                  "1\n2\n3\n")
    (check-equal? (call-with-output-string (λ (out)
                                             (parameterize ([current-output-port out])
                                               (wallp-pp #f 1 2 3))))
                  ""))
   (test-case
    "pp-it"
    (check-equal? (call-with-output-string (λ (out)
                                             (parameterize ([current-output-port out])
                                               (check-equal? (pp-it 1) 1))))
                  "1\n"))
   (test-case
    "watch-it")
   (test-case
    "cpp"
    (check-equal? (call-with-output-string (λ (out)
                                             (parameterize ([current-output-port out])
                                               (cpp 'x))))
                  "#|\nx\n|#\n")
    
    (skip (check-equal? (call-with-output-string (λ (out)
                                             (parameterize ([current-output-port out])
                                               (cpp (lambda (x) x)))))
                  "#|\n(lambda (x) x)\n|#\n")))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))