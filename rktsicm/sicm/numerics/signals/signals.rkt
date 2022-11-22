#lang racket/base

(provide (except-out (all-defined-out) assign-operation))

(require "../../kernel-intr.rkt"
         "../../general/assert.rkt"
         "fourier.rkt"
         "sigfun.rkt"
         )
(define-values (assign-operation signals:assign-operations)
  (make-assign-operations 'signals))

;;;; Signal Language, ideas stolen from SHE (Siebert/Abelson/Sussman).

;;; Signals are objects that can be specified as time functions or
;;; frequency functions.  

(define (signal? x)
  (and (pair? x)
       (eq? (car x) '*signal*)))

(define (signal:make encoding function)
  (list '*signal* encoding function))

(define (signal:encoding signal)
  (cadr signal))

(define (signal:function signal)
  (caddr signal))


(define (signal:encoded-as-frequency-function? signal)
  (eq? (signal:encoding signal) '*frequency-function*))

(define (signal:encoded-as-time-function? signal)
  (eq? (signal:encoding signal) '*time-function*))

#|
;;;  These take signal functions, as defined in sigfun.scm
;;; A signal function has a span, the domain over which it is nonzero.

(define (sigfun? x)                        ...)

(define (sigfun:make procedure span)       ...)

(define (sigfun:span signal-function)      ...)
(define (sigfun:procedure signal-function) ...)

(define (sigfun:make-span xmin xmax)       ...)
(define (sigfun:min signal-function)       ...)
(define (sigfun:max signal-function)       ...)
|#

;;; To construct a signal, specify a time or frequency signal function.

(define (time-function->signal time-function)
  (assert (sigfun? time-function))
  (signal:make '*time-function* time-function))

(define (frequency-function->signal frequency-function)
  (assert (sigfun? frequency-function))
  (signal:make '*frequency-function* frequency-function))


(define (signal:->function encoding signal)
  (assert (or (eq? encoding '*time-function*)
	      (eq? encoding '*frequency-function*)))
  (assert (signal? signal))
  (cond ((eq? encoding (signal:encoding signal))
	 (signal:function signal))
	((signal:encoded-as-time-function? signal)
	 (Fourier-transform (signal:function signal)))
	((signal:encoded-as-frequency-function? signal)
	 (inverse-Fourier-transform (signal:function signal)))
	(else
	 (error "Unknown signal type" signal))))

(define (signal->frequency-function signal)
  (signal:->function '*frequency-function* signal))

(define (signal->time-function signal)
  (signal:->function '*time-function* signal))

(define ((signal->signal unary-op) sig)
  (assert (signal? sig))
  (signal:make (signal:encoding sig)
	       (unary-op (signal:function sig))))

(define ((signal-domain->signal unary-op encoding) sig)
  (assert (signal? sig))
  (signal:make encoding
	       (unary-op (signal:->function encoding sig))))


(define ((signalXsignal->signal binary-op) sig1 sig2)
  (assert (signal? sig1))
  (assert (signal? sig2))
  (signal:make (signal:encoding sig1)
	       (binary-op
		(signal:function sig1)
		(signal:->function (signal:encoding sig1) sig2))))

(define ((signalXsignal-domain->signal binary-op encoding) sig1 sig2)
  (assert (signal? sig1))
  (assert (signal? sig2))
  (signal:make encoding
	       (binary-op
		(signal:->function encoding sig1)
		(signal:->function encoding sig2))))

(define signal:negate    (signal->signal g:negate))

(define signal:time-invert
  (signal-domain->signal g:invert '*time-function*))

(define signal:frequency-invert
  (signal-domain->signal g:invert '*frequency-function*))

(define signal:+ (signalXsignal->signal g:+))
(define signal:- (signalXsignal->signal g:-))

(define (signal:scale a sig) ((signal->signal (lambda (x) (g:* a x))) sig))

(define (signal:time-shift shift sig)
  (time-function->signal
   (sigfun:shift shift (signal->time-function sig))))

(define (signal:frequency-shift shift sig)
  (frequency-function->signal
   (sigfun:shift shift (signal->frequency-function sig))))

(define signal:time-* (signalXsignal-domain->signal g:* '*time-function*))
(define signal:frequency-convolve signal:time-*)

(define signal:frequency-*
  (signalXsignal-domain->signal g:* '*frequency-function*))
(define signal:time-convolve signal:frequency-*)

(define signal:time-/
  (signalXsignal-domain->signal g:/ '*time-function*))

(define signal:frequency-/
  (signalXsignal-domain->signal g:/ '*frequency-function*))

(define (signal:reverse sig)
  (signal:make (signal:encoding sig)
	       (sigfun:reverse (signal:function sig))))

(define (signal:dual sig)
  (time-function->signal (signal->frequency-function sig)))



(assign-operation 'negate          signal:negate         signal?)

(assign-operation '+               signal:+              signal?  signal?)
(assign-operation '-               signal:-              signal?  signal?)
