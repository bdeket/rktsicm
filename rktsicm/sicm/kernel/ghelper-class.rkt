#lang racket/base

(require "cstm/ghelper.rkt"
         "cstm/arity.rkt"
         "cstm/make-plain-procedure.rkt")

(provide make-generic-operator)

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
        (raise-argument-error 'make-generic-operator "procedure-arity?" A)))

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
     (make-plain-procedure-stx 'make-generic-operator
                               (λ (xs) #`((#,find-handler #,(cons list xs)) #,@xs))
                               (λ (xs rst) #`(apply (#,find-handler (list* #,@xs #,rst)) #,@xs #,rst))
                               arity)
     (string->symbol (format "_~a_" name))))

  (set-operator-record! operator record)
  (when name* (set-symbol-operator-record! name* record))
  (assign-operation operator default-operation #:rest #t)
  operator)
