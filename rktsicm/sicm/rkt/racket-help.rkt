#lang racket/base

(provide (all-defined-out)
         append-map make-list)

(require (for-syntax racket/base)
         racket/list)

(define ignored-error (gensym 'ignored-error))
(define (ignored-error? x) (eq? ignored-error x))


(define-logger rktsicm)
(define (warn . rst)
  (cond
    [(null? rst) (log-rktsicm-warning "warning")]
    [else
     (log-rktsicm-warning (car rst))
     (for ([p (in-list (cdr rst))]) (log-rktsicm-warning (format " ~a" p)))]))


(define (for-all? lst pred) (andmap pred lst))
(define (delv a lst) (remv* (list a) lst))
(define (symbol . rst) (string->symbol (apply string-append (map (λ (x) (format "~a" x)) rst))))
(define-syntax-rule (ignore-errors rst ...) (with-handlers ([exn:fail? void]) rst ...))
(define (there-exists? lst pred) (for/or ([i (in-list lst)]) (pred i)))
(define-syntax-rule (named-lambda (n args ... . rst) body ...) (let ([n (λ (args ... . rst) body ...)]) n))
