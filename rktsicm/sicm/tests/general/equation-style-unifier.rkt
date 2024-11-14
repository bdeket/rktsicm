#lang racket/base

(require rackunit
         "../../general/equation-style-unifier.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "general/equation-style-unifier"
   ;; syntax
   ;; the syntax for a patern is made of 'variables'
   ;; a 'variable' of the form (? a) can bind one element
   ;; a 'variable' of the form (?? a) can bind multiple elements (rest-list)
   ;; it can have a third element as guard function: (? a number?)
   (test-case
     "unify:variable?"
     (check-not-false (unify:variable? '(? a)))
     (check-not-false (unify:variable? '(?? a)))
     (check-not-false (unify:variable? `(? a number?)))
     (check-not-false (unify:variable? `(?? a number?)))
     (skip ;; this should NOT be ok, but currently is: no problem if everything is well defined
      (check-false (unify:variable? '(?)))
      (check-false (unify:variable? '(? 1 2)))
      (check-false (unify:variable? `(? 1 ,number? 3))))
     (check-false (unify:variable? '?))
     (check-false (unify:variable? 1))
     (check-false (unify:variable? '()))
     (check-false (unify:variable? '(a ?)))
     )
   (test-case
    "unify:element?"
    (check-true  (unify:element? '(? a)))
    (check-true  (unify:element? `(? a ,number?)))
    (check-false (unify:element? '(?? a))))
   (test-case
    "unify:segment?"
    (check-true  (unify:segment? '(?? a)))
    (check-true  (unify:segment? `(?? a ,number?)))
    (check-false (unify:segment? '(? a))))
   (test-case
    "unify:name"
    (check-equal? (unify:name '(? a)) 'a)
    (check-equal? (unify:name '(?? b)) 'b)
    (check-equal? (unify:name `(? c ,number?)) 'c)
    (check-equal? (unify:name `(?? d ,number?)) 'd))
   (test-case
    "unify:restricted?"
    (check-false (unify:restricted? '(? a)))
    (check-false (unify:restricted? '(?? a)))
    (check-true (unify:restricted? `(? a ,number?)))
    (check-true (unify:restricted? `(?? a ,number?))))
   (test-case
    "unify:restriction"
    (check-equal? (unify:restriction `(? a ,number?)) number?)
    (check-equal? (unify:restriction `(?? a ,list?)) list?))
   (test-case
    "unify:type"
    (check-equal? (unify:type '(? a)) '?)
    (check-equal? (unify:type '(?? a)) '??)
    (check-equal? (unify:type `(? a ,number?)) '?)
    (check-equal? (unify:type `(?? a ,list?)) '??))
   
   ;; dictionary
   ;; the dictionary is an eq-association list of variable names with the found value
   (test-case
    "unify:bind / lookup / content"
    (define dict0 '())
    (define dict1 (unify:bind '(? b) '(? f) dict0))
    (check-equal? dict1 '((b (? f) ?)))
    (define dict2 (unify:bind '(? a) 3 dict1))
    (check-equal? dict2 '((a 3 ?)(b (? f))))
    (define dict3 (unify:bind '(?? e) '(4 5 6) dict2))
    (check-equal? dict3 '((e (4 5 6) ??)(a 3 )(b (? f))))
    (define dict4 (unify:bind '(? f) 2 dict3))
    (check-equal? dict4 '((f 2 ?)(e (4 5 6))(a 3 )(b 2)))
    (check-equal? (unify:lookup '(? a) dict4) '(a 3))
    (check-equal? (unify:lookup '(?? e) dict4) '(e (4 5 6)))
    (check-false (unify:lookup '(? c) dict4))
    (check-false (unify:lookup `(? ,(list 'a)) dict4))
    (check-equal? (unify:content (unify:lookup '(? a) dict4)) 3)
    (check-equal? (unify:content (unify:lookup '(?? b) dict4)) 2))

   ;; unifier
   (test-case
    "unify:occurs-in?"
    (check-true (unify:occurs-in? '(? a) '(+ 4 (? a))))
    (check-true (unify:occurs-in? '(? a) `(+ 4 (? a ,number?))))
    (skip ;; this should probably be an error:
     (check-true (unify:occurs-in? '(? a) `(+ 4 (?? a ,number?)))))
    (check-false (unify:occurs-in?  '(? a) '(+ 4 (? b)))))
   (test-case
    "unify:value"
    (define dict '((a 1)(b 2)(c 3)(d (4 9))))
    (check-equal? (unify:value '(? a) dict) 1)
    (check-equal? (unify:value 7 dict) 7)
    (check-exn #px"should not get here" (λ () (unify:value '(?? a) dict)))
    (check-equal? (unify:value '(?? e) dict) '(?? e))
    (check-equal? (unify:value '((? a)(? b)) dict) '(1 2))
    (check-equal? (unify:value '((?? d)(? a)(? b) 5) dict) '(4 9 1 2 5)))
   (test-case
    "unify:internal"
    (define (->d dct) (map (λ (l) (list (car l) (cadr l))) dct))
    (check-false (unify:internal 1 2 '() ->d))
    (check-equal? (unify:internal '(? a) 2 '() ->d) '((a 2)))
    (check-false (unify:internal '(+ (? a) (? a)) '(+ 2 3) '() ->d))
    (check-equal? (unify:internal '(+ (? a) (? a)) '(+ 2 2) '() ->d) '((a 2)))
    (check-equal? (unify:internal '(+ (?? a)) '(+ 2 2) '() ->d) '((a (2 2))))
    (check-equal? (unify:internal '(+ (?? a)(? b)) '(+ 2 2) '((e 3 ?)) ->d) '((b 2)(a (2))(e 3))))
   (test-case
    "unify"
    (define (->d dct) (map (λ (l) (list (car l) (cadr l))) dct))
    (check-equal? (->d (unify '(+ (?? a)(? b)) '(+ 2 2))) '((b 2)(a (2)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))