#lang racket/base

(require "cstm/ghelper.rkt"
         "cstm/arity.rkt"
         "cstm/make-plain-procedure.rkt")

(provide make-generic-operator)

(module+ test (require rackunit))

;;bdk;; start original file

;;;;           Most General Generic-Operator Dispatch

;;; Generic-operator dispatch is implemented here by a discrimination
;;; list, where the arguments passed to the operator are examined by
;;; predicates that are supplied at the point of attachment of a
;;; handler (by ASSIGN-OPERATION alias DEFHANDLER).

;;; To be the correct branch all arguments must be accepted by the
;;; branch predicates, so this makes it necessary to backtrack to find
;;; another branch where the first argument is accepted if the second
;;; argument is rejected.  Here backtracking is implemented using #f
;;; as a failure return, requiring further search.  A success is
;;; consummated by calling the WIN procedure.

;;; The discrimination list has the following structure: it is a
;;; possibly improper alist whose "keys" are the predicates that are
;;; applicable to the first argument.  If a predicate matches the
;;; first argument, the cdr of that alist entry is a discrimination
;;; list for handling the rest of the arguments.  If a discrimination
;;; list is improper, then the cdr at the end of the backbone of the
;;; alist is the default handler to apply (all remaining arguments are
;;; implicitly accepted).


;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
(define (make-generic-operator A [name* #f] [default-operation* #f])

  (define arity
    (if (procedure-arity? A)
        (normalize-arity A)
        (raise-argument-error 'make-generic-operator "procedure-arity?" arity)))

  (define name
    (cond
      [(eq? #f name*) (gensym 'unnamed)]
      [(symbol? name*) name*]
      [else (raise-argument-error 'make-generic-operator "symbol?" name*)]))

  (define default-operation
    (cond
      [(eq? #f default-operation*)
       (define (no-handler . arguments)
         (define s
           (apply
            string-append
            (format "Generic operator inapplicable: ~a\n" operator)
            (format " function: ~a\n" name)
            (for/list ([a (in-list arguments)]
                       [i (in-naturals 1)])
              (format "argument ~a: ~a\n" i a))))
         (error s))
       no-handler]
      [(arity-includes? (procedure-arity default-operation*) arity)
       default-operation*]
      [else
       (raise-argument-error 'make-generic-operator
                             (format "procedure that satisfies  arity ~a" arity)
                             default-operation*)]))

  (define record (make-operator-record name arity))
  (define TREE (operator-record-tree record))

  (define (find-handler arguments)
    (or
     (let loop ([args arguments]
                [tree TREE])
       (cond
         [(null? args) (tree-han tree)]
         [else
          (for/or ([p&b (in-list (tree-branch tree))])
            (define pred? (car p&b))
            (define branch (cdr p&b))
            (or (and (tree-rst? branch) (andmap pred? args) (tree-han branch))
                (and (pred? (car args)) (loop (cdr args) branch))))]))
     (tree-han TREE)))
    
  (define operator
    (procedure-rename
     (make-plain-procedure (λ x (apply (find-handler x) x)) arity)
     (string->symbol (format "_~a_" name))))

  (set-operator-record! operator record)
  (when name* (set-symbol-operator-record! name* record))
  (assign-operation operator default-operation #:rest #t)
  operator)

(module+ test
  (define (any? _) #t)
  (define (nvr? _) #f)
  (require rackunit)
  (define (foo-default x y z) 'foo-default)
  (define foo (make-generic-operator 3 'foo foo-default))
  (check-equal? (get-operator-record foo) (operator-record 'foo 3 (tree '() #t foo-default)))

  (check-equal? (procedure-arity foo) 3)

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

  (define bar (make-generic-operator 1 'bar))
  (check-equal? (procedure-arity bar) 1)
  (assign-operation bar (λ (x) x))

  (check-equal? (procedure-arity (make-generic-operator (arity-at-least 2)))
                (arity-at-least 2))
  (check-equal? (procedure-arity (make-generic-operator (list 0 (arity-at-least 2))))
                (list 0 (arity-at-least 2)))
  (check-equal? (procedure-arity (make-generic-operator (list 1 2 (arity-at-least 2) 3)))
                (arity-at-least 1))
  (check-equal? (procedure-arity (make-generic-operator (list 5 4 3)))
                '(3 4 5))
  )