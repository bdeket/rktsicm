#lang racket/base

(require rackunit
         "../../general/list-utils.rkt"
         (submod "../helper.rkt" runner)
         (only-in racket/list shuffle))

(provide the-tests)
(define the-tests
  (test-suite
   "general/list-utils"
   ;; extra items ¿standard? in MIT-scheme
   (test-case
    "reduce/reduce-left"
    (check-true (eq? reduce reduce-left))
    (check-equal? (reduce-left list '() '(1 2 3 4)) '(((1 2) 3) 4))
    (check-equal? (reduce-left / #f '(1 2 3 4)) 1/24)
    (check-equal? (reduce-left / 'ok '()) 'ok))
   (test-case
    "reduce-right"
    (check-equal? (reduce-right list '() '(1 2 3 4)) '(1 (2 (3 4))))
    (check-equal? (reduce-right / #f '(1 2 3 4)) 3/8)
    (check-equal? (reduce-right / 'ok '()) 'ok))
   (test-case
    "sublist"
    (check-equal? (sublist '(0 1 2 3 4 5 6) 2 4) '(2 3))
    (check-equal? (sublist '(0 1 2 3 4 5 6) 1 1) '())
    (check-exn exn:fail? (λ () (sublist '(0 1 2 3 4 5 6) 1 0)))
    (check-exn exn:fail? (λ () (sublist '(0 1 2 3 4 5 6) 1 10)))
    (check-exn exn:fail? (λ () (sublist '(0 1 2 3 4 5 6) -1 2))))
   (test-case
    "last-pair"
    (check-equal? (last-pair '(1 2 3 . 4)) '(3 . 4))
    (check-equal? (last-pair '(1 2 3 4)) '(4)))
   (test-case
    "except-last-pair"
    (check-equal? (except-last-pair '(1 2 3 . 4)) '(1 2))
    (check-equal? (except-last-pair '(1 2 3 4)) '(1 2 3)))
   (test-case
    "lset="
    (check-true  (lset= eq? (list 1 2 3) (list 3 2 1)))
    (check-true  (lset= eq? (list 1 2 3 3) (list 3 2 3 1)))
    (check-true  (lset= eq? (list 1 2 3) (list 3 2 3 1)))
    (check-true  (lset= eq? (list 1 2 3 3) (list 3 2 1)))
    (check-false (lset= eq? (list '(1)) (list (list 1)))))
   (test-case
    "lset-adjoin"
    (check-true (let ([A (list 3)][B '((1)(2)(3))])
                  (andmap eq? (lset-adjoin eq? B A) (cons A B))))
    (check-equal? (lset-adjoin eq? '((1)(2)(3)) (list 3)) '((3)(1)(2)(3)))
    (check-equal? (lset-adjoin equal? '((1)(2)(3)) (list 3)) '((1)(2)(3))))
   (test-case
    "lset-union"
    (check-true (lset= eq? (lset-union eq? '(1 2 3) '(3 4 5))
                       '(1 2 3 4 5)))
    (check-equal?  (lset-union eq? '(1 2 3 3) '(4 4 5 6 6)) '(6 5 4 1 2 3 3))
    (check-equal?  (lset-union eq? '(1 2 3 3) '(4 4 2 5 1 6 3 6)) '(6 5 4 1 2 3 3)))
   (test-case
    "lset-difference"
    (check-equal? (lset-difference eq? (list (vector)) (list (vector)))
                  (list (vector)))
    (check-equal? (lset-difference equal? '(1 2 3 4 5 6) '(3 4 5))
                  '(1 2 6))
    (check-equal? (lset-difference eq? '(1 2 3) '(1 2 3))
                  '())
    (check-equal? (lset-difference eq? '(1 2 3) '())
                  '(1 2 3))
    (check-equal? (lset-difference eq? '() '(1 2 3))
                  '())
    (check-equal?  (lset-difference eq? '(1 2 3 3) '(4 4 5 6 6)) '(1 2 3 3))
    (check-equal?  (lset-difference eq? '(1 2 3 3) '(4 4 2 5 1 6 3 6)) '())
    (check-equal?  (lset-difference eq? '(1 2 3 3) '(4 4 2 5 1 6 6)) '(3 3)))
   (test-case
    "lset-intersection"
    (check-equal? (lset-intersection eq? '(1 2 3 4 5) '(4 5 6))
                  '(4 5))
    (check-equal? (lset-intersection eq? '(1 2 3) '(4 5 6))
                  '())
    (check-equal?  (lset-intersection eq? '(1 2 3 3) '(4 4 5 6 6)) '())
    (check-equal?  (lset-intersection eq? '(1 2 3 3) '(4 4 2 5 1 6 3 6)) '(1 2 3 3))
    (check-equal?  (lset-intersection eq? '(1 2 3 3) '(4 4 2 5 1 6 6)) '(1 2)))
   
   ;; items from original list-utils
   (test-case
    "variable<?"
    (define (build-sym)
      (string->symbol (bytes->string/locale (list->bytes
                                             (build-list (+ 5 (random 5))
                                                         (λ _ (+ 48 (random 60))))))))
    (for ([i (in-range 10)])
      (define A (build-sym)) (define B (build-sym))
      (if (symbol<? A B)
          (check-true  (variable<? A B))
          (check-false (variable<? A B)))))
   (test-case
    "safe-map"
    (check-equal? (safe-map list '(1 2 . 3)) '((1) (2) 3)))
   (test-case
    "count-elements"
    (check-equal? (count-elements number? '(1 2 a "b")) 2))
   (test-case
    "find-first"
    (check-equal? (find-first number? '(a "2" 3 'b 6)) 3)
    (check-equal? (find-first number? '(a "2" 'b)) #f))
   (test-case
    "countsymbols"
    (check-equal? (countsymbols '(a "2" 3 (b) 'q a 6)) 5)
    (check-equal? (countsymbols '(a "2" 3 (b)  q a 6)) 4))
   (test-case
    "butlast"
    (check-equal? (butlast '(1 2 3 4)) '(1 2 3)))
   (test-case
    "last"
    (check-equal? (last '(1 2 3 4)) 4))
   (test-case
    "list-transpose"
    (check-equal? (list-transpose '((1 2)(3 4)(5 6))) '((1 3 5)(2 4 6)))
    (skip (check-equal? (list-transpose '((1 2)(3)(5 6))) '((1 3 5)))))
   (test-case
    "list-index-of"
    (check-equal? (list-index-of  1 '(0 1 2 3 4)) 1)
    (check-exn #px"Not in list" (λ () (list-index-of 'a '(0 1 2 3 4)))))
   (test-case
    "delete-nth"
    (check-equal? (delete-nth 1 '(0 1 2 3 4)) '(0 2 3 4)))
   (test-case
    "list:elementwise"
    (check-equal? ((list:elementwise list) '(1 2 3) '(4 5 6)) '((1 4)(2 5)(3 6))))
   (test-case
    "distinct-pairs"
    (check-equal? (distinct-pairs '()) '())
    (check-equal? (distinct-pairs '(1)) '())
    (check-equal? (distinct-pairs '(1 2)) '((1 2)))
    (check-equal? (distinct-pairs '(1 2 3)) '((1 2)(1 3)(2 3))))
   (test-case
    "map-distinct-pairs"
    (check-equal? (map-distinct-pairs list '()) '())
    (check-equal? (map-distinct-pairs list '(1)) '())
    (check-equal? (map-distinct-pairs list '(1 2)) '((1 2)))
    (check-equal? (map-distinct-pairs list '(1 2 3)) '((1 2)(1 3)(2 3))))
   (test-case
    "for-each-distinct-pair"
    (define (test-it proc lst)
      (define S '())
      (for-each-distinct-pair (λ (a b) (set! S (cons (proc a b) S))) lst)
      (reverse S))
    (check-equal? (test-it list '()) '())
    (check-equal? (test-it list '(1)) '())
    (check-equal? (test-it list '(1 2)) '((1 2)))
    (check-equal? (test-it list '(1 2 3)) '((1 2)(1 3)(2 3))))
   (test-case
    "fringe"
    (check-equal? ((fringe-smaller-than? 3) '())
                  0)
    (check-equal? ((fringe-smaller-than? 100) '(a (b c) d))
                  4)
    (check-equal? ((fringe-smaller-than? 3) '(a (b c) d))
                  #f))
   (test-case
    "split-list"
    (check-equal? (split-list '(1 a 2 b 3 c 4 5 6) number? vector) #((1 2 3 4 5 6) (a b c))))
   (test-case
    "find-infimum"
    (check-equal? (find-infimum (shuffle '(1 2 3 4 5)) <) 1)
    (check-equal? (find-infimum (shuffle '(1 2 3 4 5)) >) 5))
   (test-case
    "subst"
    (check-equal? (subst 'I 1 '(5 1 3 4 1)) '(5 I 3 4 I))
    (check-equal? (subst 'I 1 '(5 1 3 (4 1))) '(5 I 3 (4 I)))
    (check-equal? (subst 'I 6 '(5 1 3 4 1)) '(5 1 3 4 1)))
   (test-case
    "delq-once"
    (check-equal? (delq-once 1 '(5 1 3 4 1)) '(5 3 4 1))
    (check-equal? (delq-once 6 '(5 1 3 4 1)) '(5 1 3 4 1)))
   (test-case
    "cons-if-necessary"
    (define a (list 'a))
    (define d (list 'd))
    (define e (cons a d))
    (check-true  (eq? (cons-if-necessary a d e) e))
    (check-false (eq? (cons-if-necessary (list 'a) d e) e)))
   (test-case
    "substitute-multiple"
    (check-equal? (substitute-multiple '(5 1 3 (4 1)) '((1 I)(5 V)(6 VI))) '(V I 3 (4 I)))
    (define A '(5 1 3 (4 1)))
    (check-true (eq? (cdr (substitute-multiple A '((5 V)(6 VI)))) (cdr A))))
   (test-case
    "map&reduce"
    (check-equal? (map&reduce (lambda (a) `(f ,a)) (lambda (new old) `(c ,new ,old))
                              'null '(1 a 2 b (3 4)))
                  '(c (f (3 4)) (c (f b) (c (f 2) (c (f a) (c (f 1) null))))))
    (check-equal? (map&reduce (lambda (a b) `(f ,a ,b)) (lambda (new old) `(c ,new ,old))
                              'null '(1 2 3) '(a b c))
                  '(c (f 3 c) (c (f 2 b) (c (f 1 a) null))))
    (check-equal? (map&reduce list (lambda (new old) `(c ,new ,old))
                              'null '(1 2 3) '(a b c) '("1" "2" "3"))
                  '(c (3 c "3") (c (2 b "2") (c (1 a "1") null)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))