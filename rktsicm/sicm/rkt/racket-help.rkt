#lang racket/base

(provide (all-defined-out)
         append-map make-list)

(require (for-syntax racket/base)
         racket/list)

(define condition? exn:fail?)


(define-logger rktsicm)
(define (warn . rst)
  (cond
    [(null? rst) (log-rktsicm-warning "warning")]
    [else
     (log-rktsicm-warning (format "~a" (car rst)))
     (for ([p (in-list (cdr rst))]) (log-rktsicm-warning (format " ~a" p)))]))


(define (for-all? lst pred) (andmap pred lst))
(define (delv a lst) (remv* (list a) lst))
(define (symbol . rst) (string->symbol (apply string-append (map (λ (x) (format "~a" x)) rst))))
(define (ignore-errors thunk) (with-handlers ([exn:fail? (λ (e) e)]) (thunk)))
(define (there-exists? lst pred) (for/or ([i (in-list lst)]) (pred i)))
(define-syntax-rule (named-lambda (n args ... . rst) body ...) (let ([n (λ (args ... . rst) body ...)]) n))
