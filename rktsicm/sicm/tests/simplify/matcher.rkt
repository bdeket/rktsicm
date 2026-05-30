#lang racket/base

(require rackunit
         "../../simplify/matcher.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/matcher"
   (test-case
    "match:predicate"
    (check-equal? ((match:predicate (λ (x) (eq? x 'hoi)))
                   '(hoi how is it)
                   '((data . base))
                   vector)
                  #(((data . base)) (how is it)))
    (check-equal? ((match:predicate (λ (x) (eq? x 'hoi)))
                   '(how is it)
                   '((data . base))
                   vector)
                  #f))
   (test-case
    "match:equal"
    (check-equal? ((match:equal +inf.0) '(+inf.0 2) '((data . base)) vector)
                  #(((data . base)) (2)))
    (check-equal? ((match:equal +inf.0 eq?) '(+inf.0 2) '((data . base)) vector)
                  #(((data . base)) (2)))
    (check-equal? ((match:equal +inf.0 eq?) `(,(/ 1 0.) 2) '((data . base)) vector)
                  #f))
   (test-case
    "match:eqv"
    (check-equal? ((match:eqv +inf.0) '(+inf.0 2) '((data . base)) vector)
                  #(((data . base)) (2)))
    (check-equal? ((match:eqv +inf.0) `(,(/ 1 0.) 2) '((data . base)) vector)
                  #(((data . base)) (2)))
    (check-equal? ((match:eqv +inf.0) `(4 2) '((data . base)) vector)
                  #f))

   (test-case
    "datum=?"
    (check-true  (datum=? +inf.0 (/ 1 0.)))
    (check-true  (datum=? (list +inf.0)  (list (/ 1 0.))))
    (check-false (datum=? (list +inf.0) (/ 1 0.)))
    (check-false (datum=? (list 1 2) (list 1 3))))
   (test-case
    "match:bind | lookup | value"
    (check-equal? (match:bind 'var 'obj '((data . base)))
                  '((var . obj) (data . base)))

    (check-equal? (match:lookup 'var '((a . b)(var . val)(c . d)))
                  '(var . val))
    (check-equal? (match:lookup 'val '((a . b)(var . val)(c . d)))
                  #f)

    (check-equal? (match:value '(var . val))
                  'val))

   (test-case
    "match:element"
    (check-equal? ((match:element 'var) '(val rst) '((data . base)(var . val)) vector)
                  #(((data . base)(var . val)) (rst)))
    (check-equal? ((match:element 'var) '(not-val rst) '((data . base)(var . val)) vector)
                  #f)
    (check-equal? ((match:element 'any) '(new rst) '((data . base)(var . val)) vector)
                  #(((any . new)(data . base)(var . val)) (rst)))
    (check-equal? ((match:element 'var) '() '((data . base)) vector)
                  #f))
   (test-case
    "match:make-segment"
    (define M (match:make-segment 'begin 'end))
    (check-equal? (match:segment-beginning M) 'begin)
    (check-equal? (match:segment-end M) 'end))
   (test-case
    "match:segment"
    (check-equal? ((match:segment 'var) '(val otr rst) `((data . base)(var . ,(match:make-segment '(val otr . ues) 'ues))) vector)
                  #(((data . base) (var . #((val otr . ues) ues))) (rst)))
    (define A '(val otr))
    (check-equal? ((match:segment 'var) A `((data . base)(var . ,(match:make-segment A A))) vector)
                  #(((data . base) (var . #((val otr) (val otr)))) (val otr)))
    (check-equal? ((match:segment 'var) '(val rst) `((data . base)(var . ,(match:make-segment '(val otr . ues) 'ues))) vector)
                  #f)
    (check-equal? ((match:segment 'var) '() `((data . base)(var . ,(match:make-segment '(val otr . ues) 'ues))) vector)
                  #f)
    (check-equal? ((match:segment 'var) '(val otr rst) `((data . base)) vector)
                  `#(((var . ,(match:make-segment '(val otr rst) '(val otr rst))) (data . base)) (val otr rst)))
    (check-equal? ((match:segment 'var) '(val otr rst) `((data . base))
                                        (let ([do #f]) (λ _ (if do (apply vector _) (begin (set! do #t) #f)))))
                  `#(((var . ,(match:make-segment '(val otr rst) '(otr rst))) (data . base)) (otr rst))))
   (test-case
    "match:list"
    (check-equal? ((match:list (match:equal '+) (match:predicate number?) (match:predicate number?))
                   '((+ 2 3)) '((data . base)) vector)
                  #(((data . base)) ()))
    (check-equal? ((match:list (match:equal '+) (match:predicate number?) (match:predicate number?))
                   '((+ 2 3 4)) '((data . base)) vector)
                  #f)
    (check-equal? ((match:list (match:equal '+) (match:predicate number?) (match:predicate number?))
                   '((+ 2 3 . 4)) '((data . base)) vector)
                  #f)
    (check-equal? ((match:list (match:equal '+) (match:predicate number?) (match:predicate number?))
                   '((+ 2)) '((data . base)) vector)
                  #f)
    (check-equal? ((match:list (match:equal '+) (match:predicate number?) (match:predicate number?))
                   '() '((data . base)) vector)
                  #f))
   (test-case
    "match:reverse-segment"
    (check-equal? ((match:reverse-segment 'var) '(not . alist) '((data . base)) vector)
                  #f)
    (check-equal? ((match:reverse-segment 'unknown) '(a list) '((data . base)) vector)
                  #f)
    (check-equal? ((match:reverse-segment 'var) '(a list) `((data . base)(var . ,(match:make-segment '() '()))) vector)
                  #(((data . base) (var . #(() ()))) (a list)))
    (define end '(list))
    (check-equal? ((match:reverse-segment 'var) '(a list) `((data . base)(var . ,(match:make-segment (cons 'a end) end))) vector)
                  #(((data . base) (var . #((a list) (list)))) (list)))
    (check-equal? ((match:reverse-segment 'var) '(list) `((data . base)(var . ,(match:make-segment (cons 'a end) end))) vector)
                  #f)
    (check-equal? ((match:reverse-segment 'var) '() `((data . base)(var . ,(match:make-segment (cons 'a end) end))) vector)
                  #f)

    (check-exn #px"Bad segment--reverse"
               (λ () ((match:reverse-segment 'var) '(a list) `((data . base)(var . ,(match:make-segment '(not) '(here)))) vector))))

   (test-case
    "match:->combinators"
    (check-equal? ((match:->combinators '()) '() '((data . base)) vector)
                  #f)
    (check-equal? ((match:->combinators '()) '(()) '((data . base)) vector)
                  #(((data . base)) ())))
   ;**************************************************************************************************
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