#lang racket/base

(provide make-generic-operator
         assign-operation
         make-assign-operations
         get-operator-record
         get-operator-record-for)

(require "ghelper-class.rkt"
         "cstm/ghelper.rkt"
         "../rkt/default-object.rkt")

(define (make-assign-operations name)
  (let ([tmp '()])
    (values (λ (op hndl #:rest [pred? default-object] . argpreds)
              (cond
                [tmp (set! tmp (cons (list* op hndl
                                            (if (default-object? pred?) (null? argpreds) pred?)
                                            argpreds)
                                     tmp))]
                [else (error (format "add: operations for ~a already assigned" name))]))
            (λ ([ignore? #f])
              (cond
                [(list? tmp)
                 (for ([l (in-list (reverse tmp))])
                   (apply assign-operation (car l) (cadr l) #:rest (caddr l) (cdddr l)))
                 (set! tmp #f)]
                [ignore? (void)]
                [else
                 (error (format "assign: operations for ~a already assigned" name))])))))