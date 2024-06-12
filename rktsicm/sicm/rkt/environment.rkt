#lang racket/base

(require (for-syntax racket/base)
         racket/function
         racket/sequence
         (rename-in racket/base [object-name procedure-name]))
(require (only-in "racket-help.rkt" rktsicm-logger))

(provide (all-defined-out) procedure-name)
(define-logger environment #:parent rktsicm-logger)

(define system-global-environment (make-base-namespace))
(define scmutils-base-environment (make-base-namespace))
(define generic-environment       (make-base-namespace))
(define rule-environment          (make-base-namespace))
(define symbolic-environment      (make-base-namespace))
(define numerical-environment     (make-base-namespace))


(define (environment-bound? env sym)
  (and (member sym (namespace-mapped-symbols env)) #t))

(define (environment-assign! env sym v #:bind? [bind? #f])
  (namespace-set-variable-value! sym v bind? env))
(define (environment-define env sym v)
  (environment-assign! env sym v #:bind? #t))

;probalby wrong TODO
(define (lexical-unbound? env sym) (not (environment-bound? env sym)))



(define not-defined (gensym 'not-defined))
(define-syntax-rule (access var env)
  (namespace-variable-value 'var #t
                            (λ ()
                              (log-environment-warning (format "warning: ~a not found in ~a" 'var env))
                              not-defined)
                            env))
(define (make-primitive-procedure var [arity #f])
  (define proc
    (namespace-variable-value var #t
                              (λ () not-defined)
                              (current-namespace)))
  (cond
    [(eq? arity #f)
     (if (eq? proc not-defined)
         (error (format "~a not found in namespace" var))
         proc)]
    [(eq? arity #t)
     (if (eq? proc not-defined)
         #f
         proc)]
    [(eq? proc not-defined)
     (if (= arity -1)
         void
         (λ l (if (= arity (length l)) (void) (error "wrong number of arguments for arity" arity l))))]
    [(= arity -1)
     (if (equal? (procedure-arity proc) (arity-at-least 0))
         proc
         (error "wrong procedure arity" (procedure-arity proc)))]
    [else
     (if (arity-includes? (procedure-arity proc) arity)
         proc
         (error "wrong procedure arity" (procedure-arity proc)))]))

(define (environment-bindings env [indirect? #t])
  (for/list ([sym (sequence-map (λ (s)
                                  (cons s
                                        (with-handlers ([exn:fail:syntax? (λ (e) not-defined)])
                                          (namespace-variable-value s indirect? (λ () not-defined) env))))
                                (namespace-mapped-symbols env))]
             #:unless (eq? not-defined (cdr sym)))
    sym))


(define (extend-top-level-environment env)
  (extend-environment (make-empty-namespace) env))

(define (extend-environment cns env)
  (for* ([s (in-list (namespace-mapped-symbols env))]
         [v (in-value (with-handlers ([exn:fail:syntax? (λ (e) not-defined)])
                        (namespace-variable-value s #t (λ () not-defined) env)))]
         #:unless (eq? v not-defined)
         [w (in-value (with-handlers ([exn:fail:syntax? (λ (e) not-defined)])
                        (namespace-variable-value s #t (λ () not-defined) cns)))]
         #:unless (eq? v w))
    (namespace-set-variable-value! s v #t cns))
  cns)

(define (environment-lookup env sym [not-found #f]) (namespace-variable-value sym #t not-found env))
;(mk extend-top-level-environment)
;(mk bind-condition-handler)
;(mk condition-type:unbound-variable)
;(mk access-condition)
;(mk use-value)
;(mk condition-type:floating-point-underflow)
;(mk bind-default-condition-handler)

(define (object-name object . environments)
  (for*/first ([e (in-list environments)]
               [b (in-list (namespace-mapped-symbols e))]
               [o (in-value (namespace-variable-value b #t (λ () (gensym)) e))]
               #:when (eq? object o))
    b))

(define (nearest-repl/environment) (current-namespace))
(define (environment? env) (namespace? env))
