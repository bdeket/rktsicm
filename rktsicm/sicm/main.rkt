#lang racket/base

(require "parameters.rkt"
         (except-in "generic.rkt" ;;some constants overwritten in mechanics/../units/constants
                     :pi   :+pi   :-pi
                     :2pi  :+2pi  :-2pi
                     :pi/2 :+pi/2 :-pi/2
                     :pi/3 :+pi/3 :-pi/3
                     :pi/4 :+pi/4 :-pi/4
                     :pi/6 :+pi/6 :-pi/6)
         "simplify.rkt"
         "numerics.rkt"
         "poly.rkt"
         "solve.rkt"
         "units.rkt"
         (except-in "mechanics.rkt" Lie-derivative);;overwritten in calculus
         "calculus.rkt"
         )

(provide (all-from-out "parameters.rkt"
                       "generic.rkt" ;; provides the bindings making this a lang
                       "simplify.rkt"
                       "numerics.rkt"
                       "poly.rkt"
                       "solve.rkt"
                       "units.rkt"
                       "mechanics.rkt"
                       "calculus.rkt"
                       ))

(require (only-in "rkt/environment.rkt" scmutils-base-environment generic-environment environment-define)
         racket/runtime-path)
(define-runtime-path here ".")
(define (mkpath x) `(file ,(path->string (build-path here x))))
(void 'SETUP-ENVIRONMENT
      (eval `(require ,@(map mkpath
                             (list "display/print.rkt"
                                   "numerics.rkt"
                                   "poly.rkt"
                                   "solve.rkt"
                                   "units.rkt"
                                   )))
            scmutils-base-environment)
      (environment-define scmutils-base-environment 'SI SI) ;; TODO ;; __really__ get rid of the environments (or find a better way)
      ;; SI is loaded in scmutils-base-environment but SBE has a different version of the unit-system struct, so simplify-units doesn't work
      (eval `(require ,@(map mkpath
                             (list "mechanics.rkt"
                                   "calculus.rkt")))
            generic-environment))
