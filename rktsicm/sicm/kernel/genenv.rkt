#lang racket/base

(provide generic-environment-maker)

(require "cstm/genenv.rkt"
         (only-in "../rkt/environment.rkt" extend-environment environment-define
                  scmutils-base-environment generic-environment numerical-environment)
         "generic.rkt"
         "mathutil.rkt")


;;bdk;; start original file

(define (generic-environment-maker new [base scmutils-base-environment])
  (let ((e (extend-environment new base)))
    (let ((d (lambda (name value)
	       (environment-define e name value))))
	(d '*environment* 'generic-environment)

	;; Unary operators from generic.scm

	(d 'type g:type)
	(d 'type-predicate g:type-predicate)
	(d 'arity g:arity)

	(d 'inexact? g:inexact?)

	(d 'zero-like g:zero-like)
	(d 'one-like g:one-like)
	(d 'identity-like g:identity-like)
	
	(d 'zero? g:zero?)
	(d 'one? g:one?)
	(d 'identity? g:identity?)

	(d 'negate g:negate)
	(d 'invert g:invert)

	(d 'square g:square)
	(d 'cube   g:cube)

	(d 'sqrt g:sqrt)

	(d 'exp g:exp)
	(d 'log g:log)

	(d 'exp2  g:exp2)
	(d 'exp10 g:exp10)
	(d 'log2  g:log2)
	(d 'log10 g:log10)

	(d 'sin g:sin)
	(d 'cos g:cos)
	(d 'tan g:tan)
	(d 'cot g:cot)
	(d 'sec g:sec)
	(d 'csc g:csc)

	(d 'asin g:asin)
	(d 'acos g:acos)

	(d 'sinh g:sinh)
	(d 'cosh g:cosh)
	(d 'tanh g:tanh)
	(d 'sech g:sech)
	(d 'csch g:csch)

	(d 'asinh g:asinh)
	(d 'acosh g:acosh)
	(d 'atanh g:atanh)

	(d 'abs g:abs)

	(d 'determinant g:determinant)
	(d 'trace g:trace)
        (d 'transpose g:transpose)
	(d 'dimension g:dimension)

        (d 'solve-linear-left g:solve-linear-left)
        (d 'solve-linear-right g:solve-linear-right)
        (d 'solve-linear g:solve-linear)

	(d 'derivative g:derivative)

	;; Binary (and nary) operators from generic.scm

	(d '= g:=)
	(d '< g:<)
	(d '<= g:<=)
	(d '> g:>)
	(d '>= g:>=)

	(d '+ g:+)
	(d '- g:-)
	(d '* g:*)
	(d '/ g:/)

	(d 'dot-product g:dot-product)
	(d 'cross-product g:cross-product)

	(d 'outer-product g:outer-product)

	(d 'expt g:expt)
	(d 'gcd g:gcd)


        ;; Complex operators from generic.scm

	(d 'make-rectangular g:make-rectangular)
	(d 'make-polar g:make-polar)

	(d 'real-part g:real-part)
	(d 'imag-part g:imag-part)
	(d 'magnitude g:magnitude)
	(d 'angle g:angle)

	(d 'conjugate g:conjugate)


	;; Wierd operators from generic.scm

	(d 'atan g:atan)

	(d 'partial-derivative g:partial-derivative)
	(d 'partial g:partial)

	(d 'apply g:apply)


	;; Compound operators from mathutil.scm

	(d 'arg-scale g:arg-scale)
	(d 'arg-shift g:arg-shift)

	(d 'sigma g:sigma)

        (d 'ref   g:ref)
	(d 'size  g:size)

	(d 'compose g:compose)
	)
    e))

#;;;bdk;; slightly different
(void (generic-environment-maker generic-environment))

;;bdk;; moved to cstm/genenv 1

#|
(let ((numerical-environment
       (extend-top-level-environment generic-environment)))
  (environment-define scmutils-base-environment
		      'numerical-environment
		      numerical-environment)
  (environment-define numerical-environment
		      '*environment*
		      'numerical-environment))
|#

#;
(let ((numerical-environment
       (extend-top-level-environment scmutils-base-environment)))
  (environment-define scmutils-base-environment
		      'numerical-environment
		      numerical-environment)
  (environment-define numerical-environment
		      '*environment*
		      'numerical-environment))