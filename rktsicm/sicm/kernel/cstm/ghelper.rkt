#lang racket/base

(provide (all-defined-out))

(require (only-in "../../rkt/glue.rkt" if rktsicm-logger default-object default-object?)
         "arity.rkt")

(define-logger generics #:parent rktsicm-logger)

;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
;branch needs to keep order. Newest checks are added to the front.
;When adding branches, the widest (any?) predicates should be added first,so they will be
;at the end of the branch list
(struct tree (branch rst? han) #:transparent #:mutable)
(define (make-tree [han #f])
  (tree '() #f han))

;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
(struct operator-record (name arity tree) #:transparent)
(define (make-operator-record name arity)
  (define tree (make-tree))
  (operator-record name arity tree))

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

(define (get-operator-record-for operator . args)
  (define tree (operator-record-tree (get-operator-record operator)))
  `(,@(let lp ([args args]
               [tree tree])
        (define branch (tree-branch tree))
        (cond
          [(null? branch)
           (list (list '-> (tree-han tree)))]
          [else
           (apply
            append
            (for/list ([p&b (in-list branch)])
              (define pred? (car p&b))
              (define branch (cdr p&b))
              (cond
                [(null? args)
                 `(,@(if (tree-rst? branch)
                         (list (list pred? '... '-> (tree-han branch)))
                         '())
                   ,@(map (λ (l) (cons pred? l)) (lp args branch)))]
                [(pred? (car args))
                 `(,@(if (and (tree-rst? branch) (andmap pred? args))
                         (list (list pred? '... '-> (tree-han branch)))
                         '())
                   ,@(map (λ (l) (cons pred? l)) (lp (cdr args) branch)))]
                [else '()])))]))
    (any/c? ... -> ,(tree-han tree))))

;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
(define (generic-operator-arity operator)
  (let ([record (get-operator-record operator)])
    (if record
        (operator-record-arity record)
        (error "Not an operator:" operator))))

(define (assign-name record) (string->symbol (format "assign-operation:~a" (operator-record-name record))))

(define (assign-operation operator handler #:rest [pred? default-object] #:end? [end? #f]. argument-predicates)
  (if (default-object? pred?) (set! pred? (null? argument-predicates)))
  (let ([record (get-operator-record operator)]
        [arity ((if pred? arity-at-least values)
                (length argument-predicates))]
        [h-arity (and (procedure? handler)(procedure-arity handler))])
    (define o-arity (if record
                        (operator-record-arity record)
                        (raise-argument-error 'assign-operation "known generic operator" operator)))
    (unless h-arity
      (raise-argument-error (assign-name record)
                            "handler procedure?" handler))
    (let ([intersect-ao (arity-intersect arity o-arity)]
          [intersect-ah (arity-intersect arity h-arity)])
      (when (null? (vector-ref intersect-ao 1))
        (raise-argument-error (assign-name record)
                              (format "argument arity intersecting with operator arity <~a>" o-arity)
                              `(,arity <-- (,@argument-predicates ,@(if pred? '(...) '())))))
      (when (null? (vector-ref intersect-ah 1))
        (raise-argument-error (assign-name record)
                              (format "argument arity intersecting with handler arity < ~a >" h-arity)
                              `(,arity <-- (,@argument-predicates ,@(if pred? '(...) '())))))
      (when (null? (vector-ref (arity-intersect (vector-ref intersect-ao 1)
                                                (vector-ref intersect-ah 1))
                               1))
      (raise-argument-error (assign-name record)
                            (format "handler is within operator arity <~a>" o-arity)
                            (list h-arity '<-- handler))))
    (for ([p (in-list argument-predicates)])
      (unless (and (procedure? p) (arity-includes? (procedure-arity p) 1))
        (raise-argument-error (assign-name record) "predicate?" p)))
    (bind-in-tree record
                  argument-predicates
                  pred?
                  handler
                  end?))
  (void))

;***************************************************************************************************
;*                                                                                                 *
;***************************************************************************************************
(define (bind-in-tree record keys rest? handler end?)
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
       (set-tree-branch! tree
                         (if end?
                             `(,@(tree-branch tree)
                               ,(cons (car keys) sub))
                             `(,(cons (car keys) sub)
                               ,@(tree-branch tree))))
       (loop sub (cdr keys))])))