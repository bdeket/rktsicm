#lang racket/base

(require (for-syntax racket/base racket/syntax)
         (rename-in (except-in racket/base #%top #%app)
                    [compose rkt:compose][object-name rkt:object-name][filter rkt:filter])
         "kernel-intr.rkt"
         (only-in "kernel/extapply.rkt" #%top #%app))

(provide
 ;new top&app
 #%top
 #%app

 (except-out (all-from-out "kernel-intr.rkt") kernel:assign-operations))

(define-syntax (provide-except-renamed-out stx)
  (syntax-case stx ()
    [(_ name itms ...)
     (with-syntax ([(rkt:itms ...) (map (λ (x)(format-id x "rkt:~a" (syntax->datum x))) (syntax->list #'(itms ...)))])
       (syntax/loc stx (provide (except-out (all-from-out name) itms ...)
                                (rename-out [itms rkt:itms] ...))))]))

(provide-except-renamed-out racket/base
                            = < <= > >=
                            + - * /
                            sqrt exp log abs expt gcd
                            sin cos tan asin acos atan
                            make-rectangular make-polar real-part imag-part magnitude angle
                            inexact? zero?
                            apply raise time)

;; set up some environments...
(require (only-in "rkt/environment.rkt" extend-environment
                  scmutils-base-environment numerical-environment generic-environment)
         "kernel/genenv.rkt")
(define-namespace-anchor anker)
(void 'INSTALL-GENERICS-&-SETUP-ENVIRONMENT
      (kernel:assign-operations)
      (extend-environment scmutils-base-environment (namespace-anchor->namespace anker))
      (namespace-undefine-variable! 'anker scmutils-base-environment)
      (namespace-undefine-variable! 'provide-except-renamed-out scmutils-base-environment)
      (extend-environment numerical-environment scmutils-base-environment)
      (generic-environment-maker generic-environment scmutils-base-environment))
