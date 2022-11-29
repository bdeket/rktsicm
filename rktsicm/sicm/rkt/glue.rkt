#lang racket/base

(provide (all-from-out
          "applyhook.rkt"
          "hashtable.rkt"
          "1d-table.rkt"
          "default-object.rkt"
          "fixnum.rkt"
          "flonum.rkt"
          "if.rkt"
          "int.rkt"
          "racket-help.rkt"
          "undefined.rkt")
         (all-defined-out)
         vector-copy
         (rename-out [mylet let][mylet* let*]))

(require (for-syntax racket/base)
         "applyhook.rkt"
         "hashtable.rkt"
          "1d-table.rkt"
         "default-object.rkt"
         "fixnum.rkt"
         "flonum.rkt"
          "if.rkt"
         "int.rkt"
         "racket-help.rkt"
         "undefined.rkt"
         (submod racket/performance-hint begin-encourage-inline)
         (only-in racket/list take)
         (only-in racket/vector vector-copy vector-map))

(define true #t)
(define false #f)
(define (delete itm lst) (remove* (list itm) lst))
(define (delq itm lst) (remq* (list itm) lst))
(define every andmap) ;; not definedin mitscheme, nor scmutils...
(define any ormap)
(define iota (case-lambda [(e) (for/list ([i (in-range e)]) i)]
                          [(e s)(for/list ([i (in-range s e 1)]) i)]
                          [(e s t)(for/list ([i (in-range s e t)]) i)]))
(define (there-exists? l p?) (ormap p? l))
(define cons* list*)
(define make-initialized-list build-list)
(define generate-list build-list)
(define list-head take)
(define generate-uninterned-symbol gensym)
(define find findf)
(define fold-left foldl)
(define make-initialized-vector build-vector)
(define subvector vector-copy)
(define (subvector-move-left! V vs ve T ts) (vector-copy! T ts V vs ve))
(define (vector-tail v t) (vector-copy v t))
(define string:<? string<?)
(define write-line writeln)
(define pathname? path?)
(define (for-each-vector-element v f) (vector-map f v))
(define 1+ add1)
(define unspecific void)

(define (symbol-upcase sym) (string->symbol (string-upcase (symbol->string sym))))
(define (symbol-downcase sym) (string->symbol (string-downcase (symbol->string sym))))

(define-syntax-rule (define-integrable head body ...)
  (begin-encourage-inline (define head body ...)))

(define-syntax (mylet stx)
  (syntax-case stx ()
    [(_ (v ...) body ...)
     (with-syntax ([(v* ...) (map (λ (x)
                                    (define d (syntax->list x))
                                    (if (null? (cdr d))
                                        (quasisyntax/loc x (#,(car d) (gensym 'undefined)))
                                        x))
                                  (syntax->list #'(v ...)))])
       (syntax/loc stx
         (let (v* ...)
           body ...)))]
    [(_ n (v ...) body ...)
     (with-syntax ([(v* ...) (map (λ (x)
                                    (define d (syntax->list x))
                                    (if (null? (cdr d))
                                        (quasisyntax/loc x (#,(car d) (gensym 'undefined)))
                                        x))
                                  (syntax->list #'(v ...)))])
       (syntax/loc stx
         (let n (v* ...)
           body ...)))]))
(define-syntax (mylet* stx)
  (syntax-case stx ()
    [(_ (v ...) body ...)
     (with-syntax ([(v* ...) (map (λ (x)
                                    (define d (syntax->list x))
                                    (if (null? (cdr d))
                                        (quasisyntax/loc x (#,(car d) (gensym 'undefined)))
                                        x))
                                  (syntax->list #'(v ...)))])
       (syntax/loc stx
         (let* (v* ...)
           body ...)))]
    [(_ n (v ...) body ...)
     (with-syntax ([(v* ...) (map (λ (x)
                                    (define d (syntax->list x))
                                    (if (null? (cdr d))
                                        (quasisyntax/loc x (#,(car d) (gensym 'undefined)))
                                        x))
                                  (syntax->list #'(v ...)))])
       (syntax/loc stx
         (let* n (v* ...)
           body ...)))]))

(define (error:wrong-type-argument val exp proc) (raise-argument-error proc exp val))