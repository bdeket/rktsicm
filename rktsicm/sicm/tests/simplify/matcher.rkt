#lang racket/base

(require rackunit
         "../../simplify/matcher.rkt"
         "../helper.rkt")

(define the-tests
  (test-suite
   "simplify/matcher"
   (check-equal? ((match:->combinators '(a ((? b) 2 3) 1 c))
                  '((a (1 2 3) 1 c))
                  '()
                  (lambda (x y) `(succeed ,x ,y)))
                 '(succeed ((b . 1)) ()))
   (check-equal? ((match:->combinators `(a ((? b ,number?) 2 3) 1 c))
                  '((a (1 2 3) 1 c))
                  '()
                  (lambda (x y) `(succeed ,x ,y)))
                 '(succeed ((b . 1)) ()))
   (check-equal? ((match:->combinators `(a ((? b ,symbol?) 2 3) 1 c))
                  '((a (1 2 3) 1 c))
                  '()
                  (lambda (x y) `(succeed ,x ,y)))
                 #f)
   (check-equal? ((match:->combinators '(a ((? b) 2 3) (? b) c))
                  '((a (1 2 3) 2 c))
                  '()
                  (lambda (x y) `(succeed ,x ,y)))
                 #f)
   (check-equal? ((match:->combinators '(a ((? b) 2 3) (? b) c))
                  '((a (1 2 3) 1 c))
                  '()
                  (lambda (x y) `(succeed ,x ,y)))
                 '(succeed ((b . 1)) ()))
   (check-equal? ((match:->combinators '(a (?? x) (?? y) (?? x) c))
                  '((a b b b b b b c))
                  '()
                  (lambda (x y) #f))
                 #f)
   ;;bdk;; the same as above but testing intermediate stages
   (check-equal? (accumulate pp
                             ((match:->combinators '(a (?? x) (?? y) (?? x) c))
                              '((a b b b b b b c))
                              '()
                              (lambda (x y)
                                (pp `(succeed ,x ,y))
                                #f)))
                 '((succeed ((y . #((b b b b b b c) (c))) (x . #((b b b b b b c) (b b b b b b c)))) ())
                   (succeed ((y . #((b b b b b c) (b c))) (x . #((b b b b b b c) (b b b b b c)))) ())
                   (succeed ((y . #((b b b b c) (b b c))) (x . #((b b b b b b c) (b b b b c)))) ())
                   (succeed ((y . #((b b b c) (b b b c))) (x . #((b b b b b b c) (b b b c)))) ())))
   (test-case
    "palindrome"
    (define (palindrome? x)
      ((match:->combinators '((?? x) ($$ x)))
       (list x) '() (lambda (x y) (null? y))))
    (check-true (palindrome? '(a b c c b a)))
    (check-false (palindrome? '(a b c c a b))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))