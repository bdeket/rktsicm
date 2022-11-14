#lang racket/base

(require (only-in racket/function arity-includes?)
         (only-in "../rkt/racket-help.rkt" rktsicm-logger)
         (only-in "../rkt/environment.rkt" generic-environment)
         "cstm/types.rkt"
         "cstm/genenv.rkt")

(provide make-generic-operator
         assign-operation
         get-operator-record)

(define-logger generics #:parent rktsicm-logger)

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

(define (arity-min ar)
  (cond
    [(integer? ar) ar]
    [(arity-at-least? ar) (arity-at-least-value ar)]
    [else (apply min (map arity-min ar))]))
;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
;branch needs to keep order. Newest checks are added to the front.
;When adding branches, the widest (any?) predicates should be added first,so they will be
;at the end of the branch list
(struct tree (branch rst? han) #:transparent #:mutable)
(define (make-tree [han #f])
  (tree '() #f han))

(define (make-find-in-tree name tree)
  (λ (arguments)
    (or
     (let loop ([args arguments]
                [tree tree])
       (cond
         [(null? args) (tree-han tree)]
         [else
          (for/or ([p&b (in-list (tree-branch tree))])
            (define pred? (car p&b))
            (define branch (cdr p&b))
            (or (and (tree-rst? branch) (andmap pred? args) (tree-han tree))
                (and (pred? (car args)) (loop (cdr args) branch))))]))
     (tree-han tree))))
;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
(struct operator-record (name arity finder tree) #:transparent)
(define (make-operator-record name arity)
  (define tree (make-tree))
  (define find-in-tree (make-find-in-tree name tree))
  (operator-record name arity find-in-tree tree))

(define *generic-operator-table* (make-hasheq))
(define *generic-symbol-operator-table* (make-hasheq))

(define (get-operator-record operator)
  (hash-ref *generic-operator-table* operator
            (λ () (define ans (get-symbol-operator-record operator))
              (when ans (log-generics-warning (format "get-operator-record using symbolic reference: ~a" operator)))
              ans)))
(define (get-symbol-operator-record operator)
  (hash-ref *generic-symbol-operator-table* operator #f))

(define (set-operator-record! operator record)
  (hash-set! *generic-operator-table* operator record))
(define (set-symbol-operator-record! operator record)
  (hash-set! *generic-symbol-operator-table* operator record))

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
    
  (define operator
    (procedure-rename
     (case arity
       [(1)
        (λ (arg)
          (define procedure ((operator-record-finder record) (list arg)))
          (procedure arg))]
       [(2)
        (λ (arg1 arg2)
          (define procedure ((operator-record-finder record) (list arg1 arg2)))
          (procedure arg1 arg2))]
       [else
        (λ arguments
          (unless (arity-includes? arity (length arguments))
            (raise-arity-error operator arity arguments))
          (define procedure ((operator-record-finder record) arguments))
          (apply procedure arguments))])
     (string->symbol (format "_~a_" name))))

  (set-operator-record! operator record)
  (when name* (set-symbol-operator-record! name* record))
  (assign-operation operator default-operation)
  operator)

;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
(define (generic-operator-arity operator)
  (let ([record (get-operator-record operator)])
    (if record
        (operator-record-arity record)
        (error "Not an operator:" operator))))

(define (assign-operation operator handler #:rest [pred? #f] . argument-predicates)
  (let ([record (get-operator-record operator)]
        [arity (length argument-predicates)])
    (if record
        (let ([o-arity (operator-record-arity record)])
          (unless (or (<= arity (arity-min o-arity))
                      (arity-includes? o-arity arity))
            (error (format "Incorrect operator arity: ~a <= (~a -> ~a)"
                           arity operator (operator-record-arity record))))
          (bind-in-tree record
                        argument-predicates
                        pred?
                        handler))
	(error "Assigning a handler to an undefined generic operator"
	       operator)))
  (void))

;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
(define (bind-in-tree record keys rest? handler)
  (let loop ([tree (operator-record-tree record)]
             [keys keys])
    (cond
      [(null? keys)
       (when (equal? (tree-rst? tree) (not rest?)) (set-tree-rst?! tree rest?))
       (when (tree-han tree)
         (log-generics-warning (format "Replacing handler for generic ~a: ~a -> ~a"
                       (operator-record-name record) (tree-han tree) handler)))
       (set-tree-han! tree handler)]
      [(assq (car keys) (tree-branch tree))
       =>
       (λ (stem) (loop (cdr stem) (cdr keys)))]
      [else
       (define sub (make-tree))
       (set-tree-branch! tree (cons (cons (car keys) sub)
                                    (tree-branch tree)))
       (loop sub (cdr keys))])))

;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
;;; Failures make it to here.  Time to DWIM, with apologies to Warren
;;; Teitelman.  Can we look at some argument as a default numerical
;;; expression?  I want to get rid of this, since "when in doubt, dike 
;;; it out." -- Greenblatt, but metacirc/prop seems to need this.  
;;; It should be fixed.

(define (no-way-known operator name arguments)
  (let ([new-arguments (map dwim arguments)])
    (when (equal? arguments new-arguments)
      (define s
        (apply
         string-append
         (format "Generic operator inapplicable: ~a\n" operator)
         (format " function: ~a\n" name)
         (for/list ([a (in-list arguments)]
                    [i (in-naturals 1)])
           (format "argument ~a: ~a\n" i a))))
      (error s))
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
