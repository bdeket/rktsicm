#lang racket/base

(provide (all-defined-out)
         (all-from-out "generic-extra.rkt"))

(require "generic-extra.rkt"
         "../../parameters.rkt")

;the nesting to make it possible to require litfun went to deep (cyclic dependencies)
;so in the end I opted to make it possible to extend the g:apply function at a later time
;for now, this is only happening in litfun.rkt
(define-values (g:apply install-g:apply-case) 
  (let ([others (make-hash)]
        [notfound (gensym)])

    (define (collapse l)
      (if (null? (cdr l))
          (if (list? (car l))
              (car l)
              (error "g:apply: last argument must be a list"))
          (cons (car l)
                (collapse (cdr l)))))

    (define (install-g:apply-case pred fct)
      (hash-set! others pred fct))
  
    (define  (g:apply f . apply-args)
      (when (null? apply-args) (error "No argument list for G:APPLY"))

      (define args (collapse apply-args))
      
      (cond
        [(procedure? f)
         (apply f args)]
        #;[(applicable-literal? f) ; moved to litfun ... the nesting went too deep :/
           (apply
            (literal-function f
                              (permissive-function-type (length args)))
            args)]
        #|((eq? f second) (apply (access second system-global-environment) args))|#
        [else
         (define ans
           (for/fold ([ans notfound])
                     ([(p t) (in-hash others)]
                      #:when (p f)
                      [v (in-value #t)]
                      #:final v)
             (t f args)))
         (if (eq? ans notfound)
             (generic:apply f args)
             ans)]))

    (values g:apply install-g:apply-case)))

(define (applicable-literal? f)
  (and (symbol? f) (*enable-literal-apply*)))

;;; *enable-literal-apply* is modulated by with-literal-apply-enabled.  
;;; This procedure is defined in extapply.scm.
;;; This feature is used explicitly in ode/interface.scm.
