#lang racket/base

(require (only-in "../rkt/glue.rkt" if default-object default-object?)
         (only-in "../rkt/environment.rkt" generic-environment)
         "cstm/ghelper.rkt"
         "cstm/arity.rkt"
         "cstm/types.rkt"
         "cstm/genenv.rkt")

(provide make-generic-operator)

(module+ test (require rackunit))

;;bdk;; start original file

;;;;           Most General Generic-Operator Dispatch

;;; Generic-operator dispatch is implemented here by a discrimination
;;; list, where the arguments passed to the operator are examined by
;;; predicates that are supplied at the point of attachment of a
;;; handler (by ASSIGN-OPERATION).

;;; To be the correct branch all arguments must be accepted by
;;; the branch predicates, so this makes it necessary to
;;; backtrack to find another branch where the first argument
;;; is accepted if the second argument is rejected.  Here
;;; backtracking is implemented by OR.

;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
(define (make-generic-operator arity [name* #f] [default-operation* #f])

  (unless (procedure-arity? arity)
    (raise-argument-error 'make-generic-operator "procedure-arity?" arity))

  (define name
    (cond
      [(eq? #f name*) (gensym 'unnamed)]
      [(symbol? name*) name*]
      [else (raise-argument-error 'make-generic-operator "symbol?" name*)]))

  (define default-operation
    (cond
      [(eq? #f default-operation*)
       (define (no-handler . arguments)
         (no-way-known operator name arguments))
       no-handler]
      [(arity-includes? (procedure-arity default-operation*) arity)
       default-operation*]
      [else
       (raise-argument-error 'make-generic-operator
                             (format "procedure that satisfies  arity ~a" arity)
                             default-operation*)]))

  (define record (make-operator-record name arity))
  (define TREE (operator-record-tree record))

  (define (find-branch tree arg win)
    (let loop ([trlst (tree-branch tree)])
      (cond
        [(pair? trlst)
         (or (and ((caar trlst) arg) (win (cdar trlst)))
             (loop (cdr trlst)))]
        [(tree-rst? tree) (tree-han tree)]
        [else #f])))
  (define (general-find-handler arguments)
    (let loop ([tree TREE]
               [args arguments])
      (find-branch tree (car args)
                   (if (pair? (cdr args))
                       (λ (branch) (loop branch (cdr args)))
                       tree-han))))
    
  (define operator
    (procedure-rename
     (case arity
       [(1)
        (λ (arg) ((find-branch TREE arg tree-han) arg))]
       [(2)
        (λ (arg1 arg2)
          ((find-branch TREE arg1 (λ (branch) (find-branch branch arg2 tree-han)))
           arg1 arg2))]
       [else
     (λ arguments
       (unless (arity-includes? arity (length arguments))
         (raise-arity-error operator arity arguments))
       (apply (general-find-handler arguments) arguments))])
     (string->symbol (format "_~a_" name))))

  (set-operator-record! operator record)
  (when name* (set-symbol-operator-record! name* record))
  (assign-operation operator default-operation #:rest #t)
  operator)



;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
;;; Failures make it to here.  Time to DWIM, with apologies to Warren
;;; Teitelman.  Can we look at some argument as a default numerical
;;; expression?  I want to get rid of this, since "when in doubt, dike 
;;; it out." -- Greenblatt, but metacirc/prop seems to need this.  
;;; It should be fixed.

(define (no-way-known operator name arguments)
  (let ((new-arguments (map dwim arguments)))
    (if (equal? arguments new-arguments)
        (let ((s (apply
                  string-append
                  (format "Generic operator inapplicable: ~a\n" operator)
                  (format " function: ~a\n" name)
                  (for/list ([a (in-list arguments)]
                             [i (in-naturals 1)])
                    (format "argument ~a: ~a\n" i a)))))
          (error s)))
    (apply operator new-arguments)))

(define (dwim argument)
  (if (pair? argument)
      (cond
        [(memq (car argument) type-tags)
         argument]
        [(memq (car argument) generic-numerical-operators)
         (apply (eval (car argument) generic-environment)
                (cdr argument))]
        [else
         argument])
      argument))

(module+ test
  (define (any? _) #t)
  (define (nvr? _) #f)
  (require rackunit)
  (define (foo-default x y z) 'foo-default)
  (define foo (make-generic-operator 3 'foo foo-default))
  (check-equal? (get-operator-record foo) (operator-record 'foo 3 (tree '() #t foo-default)))

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
  (check-equal? (foo 'b 'any 'any) 5)
  ;; ^^ this is different from ghelper-class (in in line with scim)
  ;; in practice it seems the #:rest is only ever used for the top tree (which doesn't have a pred?)
  (check-equal? (foo 'c 'c 'c) 'foo-default)
  (check-equal? (foo 'a 'a 'c) 'foo-default)
  
  )