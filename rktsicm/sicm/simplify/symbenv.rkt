#lang racket/base

(provide symbolic-environment-maker)

(require "../rkt/hashtable.rkt"
         "../rkt/environment.rkt"
         "../kernel-intr.rkt"
         )

;;;; Symbolic environment for simplification

(define (symbolic-operator operator-symbol)
  (let ((v (hash-table/get symbolic-operator-table operator-symbol #f)))
    (if v
	v
	(error "Undefined symbolic operator" operator-symbol))))

(define (symbolic-environment-maker [env (make-empty-namespace)][base scmutils-base-environment])
  (let ((e (extend-environment env base)))
    (let ((d
	   (lambda (name value)
	     (environment-define e name value))))

      (d '*environment* 'symbolic-environment)

      ;; Unary operators from generic.scm
      #|
      (d 'type (symbolic-operator 'type))
      (d 'type-predicate (symbolic-operator 'type-predicate))
      (d 'arity (symbolic-operator 'arity))

      (d 'inexact? (symbolic-operator 'inexact?))

      (d 'zero-like (symbolic-operator 'zero-like))
      (d 'one-like (symbolic-operator 'one-like))
      (d 'identity-like (symbolic-operator 'identity-like))
      |#
      (d 'zero? (symbolic-operator 'zero?))
      (d 'one? (symbolic-operator 'one?))
      ;;	(d 'identity? (symbolic-operator 'identity?))

      (d 'negate (symbolic-operator 'negate))
      (d 'invert (symbolic-operator 'invert))

      (d 'square (symbolic-operator 'square))
      (d 'cube   (symbolic-operator 'cube))

      (d 'sqrt (symbolic-operator 'sqrt))

      (d 'exp (symbolic-operator 'exp))
      (d 'log (symbolic-operator 'log))
      #|
      (d 'exp2  (symbolic-operator 'exp2))
      (d 'exp10 (symbolic-operator 'exp10))
      (d 'log2  (symbolic-operator 'log2))
      (d 'log10 (symbolic-operator 'log10))
      |#
      (d 'sin (symbolic-operator 'sin))
      (d 'cos (symbolic-operator 'cos))
      (d 'tan (symbolic-operator 'tan))
      (d 'sec (symbolic-operator 'sec))
      (d 'csc (symbolic-operator 'csc))

      (d 'asin (symbolic-operator 'asin))
      (d 'acos (symbolic-operator 'acos))

      (d 'sinh (symbolic-operator 'sinh))
      (d 'cosh (symbolic-operator 'cosh))
      #|
      (d 'tanh (symbolic-operator 'tanh))
      (d 'sech (symbolic-operator 'sech))
      (d 'csch (symbolic-operator 'csch))
      |#
      (d 'abs (symbolic-operator 'abs))

      ;; (d 'derivative (symbolic-operator 'derivative))
      
      ;; Binary (and nary) operators from generic.scm

      (d 'expt (symbolic-operator 'expt))
      ;; (d 'gcd (symbolic-operator 'gcd))


      ;; Complex operators from generic.scm

      (d 'make-rectangular (symbolic-operator 'make-rectangular))
      (d 'make-polar (symbolic-operator 'make-polar))

      (d 'real-part (symbolic-operator 'real-part))
      (d 'imag-part (symbolic-operator 'imag-part))
      (d 'magnitude (symbolic-operator 'magnitude))
      (d 'angle (symbolic-operator 'angle))

      (d 'conjugate (symbolic-operator 'conjugate))

      (d 'atan (symbolic-operator 'atan))


      (d '= (symbolic-operator '=))

      (d '+ (symbolic-operator '+))

      (d '* (symbolic-operator '*))

      (d '- (symbolic-operator '-))

      (d '/ (symbolic-operator '/))

      )
    e))

;(void (symbolic-environment-maker symbolic-environment))
