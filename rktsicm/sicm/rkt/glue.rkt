#lang racket/base

(provide (all-from-out
          "applyhook.rkt"
          "hashtable.rkt"
          "1d-table.rkt"
          "default-object.rkt"
          "fixnum.rkt"
          "flonum.rkt"
          "gcreclaimed.rkt"
          "if.rkt"
          "int.rkt"
          "racket-help.rkt"
          "undefined.rkt")
         (except-out (all-defined-out) anker)
         vector-copy
         (rename-out [mylet let][mylet* let*][myrandom random]))

(require (for-syntax racket/base)
         "applyhook.rkt"
         "hashtable.rkt"
          "1d-table.rkt"
         "default-object.rkt"
         "fixnum.rkt"
         "flonum.rkt"
         "gcreclaimed.rkt"
          "if.rkt"
         "int.rkt"
         "racket-help.rkt"
         "undefined.rkt"
         (submod racket/performance-hint begin-encourage-inline)
         (only-in racket/list take remove-duplicates)
         (only-in racket/vector vector-copy vector-map)
         (only-in racket/syntax format-symbol)
         )

(define true #t)
(define false #f)
(define number:eqv? eqv?)
(define (delete itm lst [test equal?]) (remove* (list itm) lst equal?))
(define (delq itm lst) (remq* (list itm) lst))
(define every andmap) ;; not definedin mitscheme, nor scmutils...
(define any ormap)
(define iota (case-lambda [(n) (for/list ([i (in-range n)]) i)]
                          [(n s)(for/list ([i (in-range s (+ s n) 1)]) i)]
                          [(n s t)(for/list ([i (in-range s (+ s (* n t)) t)]) i)]))
(define (there-exists? l p?) (ormap p? l))
(define cons* list*)
(define make-initialized-list build-list)
(define list-head take)
(define generate-uninterned-symbol gensym)
(define find findf)
(define fold-left foldl)
(define make-initialized-vector build-vector)
(define subvector vector-copy)
(define (subvector-move-left! V vs ve T ts) (vector-copy! T ts V vs ve))
(define (subvector-fill! V s e v) (for ([i (in-range s e)]) (vector-set! V i v)))
(define (vector-tail v t) (vector-copy v t))
(define (vector-head v t) (vector-copy v 0 t))
(define string:<? string<?)
(define write-line writeln)
(define pathname? path?)
(define (for-each-vector-element v f) (vector-map f v))
(define 1+ add1)
(define unspecific (void))
(define rationalize->exact rationalize)
(define (floor->exact x) (inexact->exact (floor x)))
(define (round->exact x) (inexact->exact (round x)))
(define (myrandom i) (if (flonum? i) (* i (random)) (random i)))

(define (symbol-upcase sym) (string->symbol (string-upcase (symbol->string sym))))
(define (symbol-downcase sym) (string->symbol (string-downcase (symbol->string sym))))
(define (string-head str ind) (substring str 0 ind))
(define (string-tail str ind) (substring str ind))

(define delete-duplicates remove-duplicates)

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

(require (only-in "environment.rkt" system-global-environment extend-environment))
(define-namespace-anchor anker)
(void (extend-environment system-global-environment (namespace-anchor->namespace anker))
      (namespace-undefine-variable! 'anker system-global-environment))
