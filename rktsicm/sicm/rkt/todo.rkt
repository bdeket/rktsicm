#lang racket/base

(require (for-syntax racket/base racket/syntax)
         racket/list
         racket/math
         racket/stream)
(require (only-in "racket-help.rkt" warn))

(provide (all-defined-out))

(define-syntax-rule (==> a b) (define-syntax a (make-rename-transformer #'b)))

;; TODO's find right way to do this

(define-syntax-rule (todo id _ ...) (define (id . rst) (error (format "~a: TODO" 'id))))
(define-syntax (errr stx)
  (syntax-case stx ()
    [(_ id) (syntax/loc #'id (error (format "~a: TODO" 'id)))]))
(define-syntax (todos stx)
  (syntax-case stx ()
    [(todos name cases ...)
     (with-syntax ([(id ...)
                    (apply append
                           (map (λ (x)
                                  (cond
                                    [(and (list? x) (eq? (car x) '#:from))
                                     (cddr x)]
                                    [else (list (car x))]))
                                (syntax->datum #'(cases ...))))])
       (quasisyntax/loc #'name
         (begin
           (module name racket/base
             (provide id ...)
             (define (id . rst) ((λ (sym)#,(syntax/loc stx (error (format "~a: TODO" sym)))) 'id)) ...)
           #,(datum->syntax stx `(require ',#'name)))))]))

(todo set-cdr!)
(todo set-car!)
(todo with-si-units->expression)
(todo unsyntax)
(todo procedure-lambda)
(todo procedure)

(define angular '&angular)
(define *angular* '&angular)
(define symbolic-operators '())

(define (diff-memoize-1arg fct) (warn (format "diff-memoize-1arg: not moizing - ~a" fct)) fct)
(define (diff-memoize-2arg fct) (warn (format "diff-memoize-2arg: not moizing - ~a" fct)) fct)

(todo div-coeff "???")
(todo sub-coeff "???")
(todo expt-coeff "???")

(==> pp println)

(define rationalize->exact inexact->exact)


(==> fluid-let let) ;should be done with parameterize

(==> define-integrable define);realy is define-inline

(define (trace-both f) (printf "not realy tracing ~a\n" f) f)

(==> bkpt error); creates a breakpoint... but what it should do...?
(define (condition-accessor condition-type field-names)
  (λ (continuation restarts . field-names)
    (error "TODO")))
(define condition-type:inapplicable-object (gensym 'condition))
(define (condition? . rst) (error "TODO"))

(define (lexical-unbound? . rst) (error "lexical-unbound?: TODO"))

;; environment stuff
(define not-defined (gensym 'not-defined))

(define-syntax-rule (access var env)
  (namespace-variable-value 'var #t
                            (λ ()
                              (warn (format "warning: ~a not found in ~a\n" 'var env))
                              not-defined)
                            env))


(define (->environment . rst) (make-empty-namespace))
(==> environment-bindings namespace-mapped-symbols)

(todo extend-top-level-environment)
(todo environment-define)
(todo bind-condition-handler)
(todo condition-type:unbound-variable)
(todo access-condition)
(todo use-value)
(todo condition-type:floating-point-underflow)
(todo bind-default-condition-handler)
