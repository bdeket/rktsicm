#lang racket/base

(require (for-syntax racket/base)
         rackunit
         "../../kernel/cstm/ghelper.rkt")

(define (any? _) #t)
(define (nvr? _) #f)

(define-syntax (test-it1 stx)
  (syntax-case stx ()
    [(_ make-generic-operator)
     #'(test-case
       "foo"
       (define (foo-default x y z) 'foo-default)
       (define foo (make-generic-operator 3 'foo foo-default))
       (check-equal? (get-operator-record foo) (operator-record 'foo 3 (tree '() #t foo-default)))
       (check-equal? (get-operator-record 'foo) (operator-record 'foo 3 (tree '() #t foo-default)))
       (check-false (get-operator-record (gensym)))

       (check-equal? (procedure-arity foo) 3)
       (check-equal? (generic-operator-arity foo) 3)
       (check-exn #px"Not an operator:" (λ () (generic-operator-arity (gensym))))

       (check-exn #px"argument arity intersecting with operator arity"
                  (λ () (assign-operation foo (λ (a b) 'a) any? any?)))
       (check-exn #px"argument arity intersecting with operator arity"
                  (λ () (assign-operation foo (λ (a b c) 'a) any? any?)))
       (check-exn #px"argument arity intersecting with handler arity"
                  (λ () (assign-operation foo (λ (a b) 'a) any? any? any?)))
       (check-exn #px"handler is within operator arity"
                  (λ () (assign-operation foo (λ (a [b 1]) 'a) any? #:rest any?)))
       (check-equal? (get-operator-record foo) (operator-record 'foo 3 (tree '() #t foo-default)))

       (define (foo-handler1 a b c) 1)
       (define (a? m) (eq? m 'a))(define (b? m) (eq? m 'b))(define (c? m) (eq? m 'c))
       (assign-operation foo foo-handler1 a? b? #:rest #t)
       (check-equal? (get-operator-record foo)
                     (operator-record 'foo 3 (tree (list (cons a? (tree (list (cons b? (tree '() #t foo-handler1))) #f #f)))
                                                   #t foo-default)))
       (define (foo-handler2 a b c) 2)
       (assign-operation foo foo-handler2 a? c? #:rest #t)
       (check-equal? (get-operator-record foo)
                     (operator-record 'foo 3 (tree (list (cons a? (tree (list (cons c? (tree '() #t foo-handler2))
                                                                              (cons b? (tree '() #t foo-handler1)))
                                                                        #f #f)))
                                                   #t foo-default)))

       (define (foo-handler3 a b c) 3)
       (assign-operation foo foo-handler3 b? c? #:rest #t)
       (check-equal? (get-operator-record foo)
                     (operator-record 'foo 3 (tree (list (cons b? (tree (list (cons c? (tree '() #t foo-handler3)))
                                                                        #f #f))
                                                         (cons a? (tree (list (cons c? (tree '() #t foo-handler2))
                                                                              (cons b? (tree '() #t foo-handler1)))
                                                                        #f #f)))
                                                   #t foo-default)))

       (define (foo-handler4 a b c) 4)
       (assign-operation foo foo-handler4 b? #:rest #t)
       (check-equal? (get-operator-record foo)
                     (operator-record 'foo 3 (tree (list (cons b? (tree (list (cons c? (tree '() #t foo-handler3)))
                                                                        #t foo-handler4))
                                                         (cons a? (tree (list (cons c? (tree '() #t foo-handler2))
                                                                              (cons b? (tree '() #t foo-handler1)))
                                                                        #f #f)))
                                                   #t foo-default)))

       (define (foo-handler5 a b c) 5)
       (assign-operation foo foo-handler5 b? #:rest #t)
       (check-equal? (get-operator-record foo)
                     (operator-record 'foo 3 (tree (list (cons b? (tree (list (cons c? (tree '() #t foo-handler3)))
                                                                        #t foo-handler5))
                                                         (cons a? (tree (list (cons c? (tree '() #t foo-handler2))
                                                                              (cons b? (tree '() #t foo-handler1)))
                                                                        #f #f)))
                                                   #t foo-default)))

       (check-equal? (foo 'a 'b 'b) 1)
       (check-equal? (foo 'a 'c 'c) 2)
       (check-equal? (foo 'b 'c 'c) 3)
       (check-equal? (foo 'b 'b 'b) 5)
       (check-equal? (foo 'c 'c 'c) 'foo-default)
       (check-equal? (foo 'a 'a 'c) 'foo-default)
       (check-equal? (get-operator-record-for foo 'a 'b 'b)
                     `((,a? ,b? (... ...) -> ,foo-handler1)
                       (any/c? (... ...) -> ,foo-default)))
       (check-equal? (get-operator-record-for foo 'a #:defaults? #f)
                     `((,a? ,c? (... ...) -> ,foo-handler2)
                       (,a? ,c? -> ,foo-handler2)
                       (,a? ,b? (... ...) -> ,foo-handler1)
                       (,a? ,b? -> ,foo-handler1)))
       (check-exn #px"get-operator-record-for: contract violation\n  expected: at most 3 arguments for operator foo"
                  (λ () (get-operator-record-for foo 1 2 3 4)))
       )]))
(define-syntax (test-it2 stx)
  (syntax-case stx ()
    [(_ make-generic-operator)
     #'(test-case
   "bar"
   (define bar (make-generic-operator 1 'bar))
   (check-exn #px"Generic operator inapplicable: #<procedure:_bar_>\n function:" (λ () (bar 1)))
   (check-exn #px"make-generic-operator: contract violation\n  expected: procedure that satisfies  arity 1\n  given:"
              (λ () (make-generic-operator 1 'bar (λ () 0))))
   
   (check-equal? (procedure-arity bar) 1)
   (define (ID x) x)
   (assign-operation bar ID)
   (check-equal? (bar 1) 1)
   
   (check-exn #px"assign-operation: contract violation\n  expected: known generic operator\n  given:"
              (λ () (assign-operation (gensym) ID)))
   (check-exn #px"assign-operation:bar: contract violation\n  expected: handler procedure\\?\n  given:"
              (λ () (assign-operation bar 'ID)))
   (check-exn #px"assign-operation:bar: contract violation\n  expected: predicate\\?\n  given:"
              (λ () (assign-operation bar ID 'test?)))

   (check-equal? (get-operator-record-for bar 3 #:defaults? #f) '())
   (check-equal? (get-operator-record-for bar 3 #:defaults? #t) `((any/c? (... ...) -> ,ID)))

   (define (a? a) (equal? a 'a)) (define (b? b) (equal? b 'b)) (define (c? c) (equal? c 'c))
   (assign-operation bar + a?)
   (assign-operation bar - b?)
   (assign-operation bar / c? #:end? #t)

   (check-equal? (get-operator-record-for bar)
                 `((,b? -> ,-) (,a? -> ,+) (,c? -> ,/) (any/c? (... ...) -> ,ID)))
   
   (check-equal? (procedure-arity (make-generic-operator (arity-at-least 2)))
                 (arity-at-least 2))
   (check-equal? (procedure-arity (make-generic-operator (list 0 (arity-at-least 2))))
                 (list 0 (arity-at-least 2)))
   (check-equal? (procedure-arity (make-generic-operator (list 1 2 (arity-at-least 2) 3)))
                 (arity-at-least 1))
   (check-equal? (procedure-arity (make-generic-operator (list 5 4 3)))
                 '(3 4 5))
   (check-exn #px"make-generic-operator: contract violation\n  expected: procedure-arity\\?\n  given:"
              (λ () (make-generic-operator 'false)))
   (check-exn #px"make-generic-operator: contract violation\n  expected: symbol\\?\n  given:"
              (λ () (make-generic-operator 1 1))))]))

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/ghelper-class"
  (let ()
    (local-require "../../kernel/ghelper-class.rkt")
    (test-it1 make-generic-operator)
    (test-it2 make-generic-operator))
  (let ()
    (local-require "../../kernel/ghelper-pro.rkt")
    (test-it1 make-generic-operator)
    (test-it2 make-generic-operator)
    (test-case
     "pro-special"
     (define foo (make-generic-operator 2 'foo))
     (check-equal? (procedure-arity foo) 2)
     (assign-operation foo (λ (a b) (list a b)) (λ (x) (eq? x 'a)) #:rest #t)
     (check-equal? (foo 'a 'whatever) '(a whatever))
     ;; ^^ this is different from ghelper-class (in in line with scmutils): rest args are not checked
     ;; in practice it seems the #:rest is only ever used for the top tree (which doesn't have a pred?)
     ))
  ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))