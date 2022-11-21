#lang racket/base

(provide (except-out (all-defined-out) assign-operation))

(require "../../kernel-intr.rkt"
         "../../rkt/default-object.rkt"
         "../../general/assert.rkt"
         )
(define-values (assign-operation sigfun:assign-operations)
  (make-assign-operations 'sigfun))

;;; A signal function has a span, the domain over which it may be
;;; nonzero.  The spans are ordered pairs of numbers.

(define (sigfun? x)
  (and (pair? x)
       (eq? (car x) '*signal-function*)))

(define (sigfun:make procedure span)
  (list '*signal-function*
	span
	(lambda (x)
	  (cond ((< x (sigfun:min span)) 0)
		((< x (sigfun:max span))
		 (procedure x))
		(else 0)))))

(define (sigfun:span signal-function)
  (cadr signal-function))

(define (sigfun:procedure signal-function)
  (caddr signal-function))				   


;;; For convenience, the span can be specified as either a single
;;; number, indicating the maxx, or a minimum and a maximum.  I hope
;;; to eventually improve this system to allow non-symmetric spans,
;;; but right now only symmetric spans are allowed.

(define (sigfun:make-span minx [maxx default-object])
  (when (default-object? maxx)
      (begin (set! maxx minx)
	     (set! minx (- maxx))))
  (assert (< minx maxx))
  (cons minx maxx))

(define (sigfun:min span) (car span))
(define (sigfun:max span) (cdr span))


(define ((sigfun:unary-op op) sigfun)
  (sigfun:make (op (sigfun:procedure sigfun))
	       (sigfun:span sigfun)))

#|
;;; Looks wrong...  Must intersect spans
(define ((sigfun:binary-op op) sigfun1 sigfun2)
  (let ((span1 (sigfun:span sigfun1))
	(span2 (sigfun:span sigfun2)))
    (sigfun:make (lambda (x)
		   (op ((sigfun:procedure sigfun1) x)
		       ((sigfun:procedure sigfun2) x)))
		 (sigfun:make-span
		  (min (sigfun:min span1) (sigfun:min span2))
		  (max (sigfun:max span1) (sigfun:max span2))))))
|#

(define ((sigfun:binary-op op) sigfun1 sigfun2)
  (let ((span1 (sigfun:span sigfun1))
	(span2 (sigfun:span sigfun2)))
    (sigfun:make (lambda (x)
		   (op ((sigfun:procedure sigfun1) x)
		       ((sigfun:procedure sigfun2) x)))
		 (sigfun:make-span
		  (max (sigfun:min span1) (sigfun:min span2))
		  (min (sigfun:max span1) (sigfun:max span2))))))
	


(define sigfun:make-rectangular (sigfun:binary-op g:make-rectangular))
(define sigfun:make-polar       (sigfun:binary-op g:make-polar))

(define sigfun:real-part (sigfun:unary-op g:real-part))
(define sigfun:imag-part (sigfun:unary-op g:imag-part))
(define sigfun:magnitude (sigfun:unary-op g:magnitude))
(define sigfun:angle     (sigfun:unary-op g:angle))

(define sigfun:conjugate (sigfun:unary-op g:conjugate))


(define sigfun:negate    (sigfun:unary-op g:negate))
(define sigfun:invert    (sigfun:unary-op g:invert))

(define sigfun:sqrt      (sigfun:unary-op g:sqrt))
(define sigfun:square    (sigfun:unary-op g:square))

(define sigfun:exp       (sigfun:unary-op g:exp))
(define sigfun:log       (sigfun:unary-op g:log))

(define sigfun:sin       (sigfun:unary-op g:sin))
(define sigfun:cos       (sigfun:unary-op g:cos))

(define sigfun:asin      (sigfun:unary-op g:asin))
(define sigfun:acos      (sigfun:unary-op g:acos))

(define sigfun:sinh      (sigfun:unary-op g:sinh))
(define sigfun:cosh      (sigfun:unary-op g:cosh))

(define sigfun:+         (sigfun:binary-op g:+))
(define sigfun:-         (sigfun:binary-op g:-))
(define sigfun:*         (sigfun:binary-op g:*))
(define sigfun:/         (sigfun:binary-op g:/))

(define sigfun:expt      (sigfun:binary-op g:expt))

(define (sigfun:expt2 sigfun n)
  ((sigfun:unary-op (lambda (x) (g:expt x n))) sigfun))

(define (sigfun:expt3 n sigfun)
  ((sigfun:unary-op (lambda (x) (g:expt n x))) sigfun))


(define sigfun:atan2     (sigfun:binary-op g:atan2))
(define sigfun:atan      (sigfun:unary-op g:atan))

(define (sigfun:scale a sigfun)
  ((sigfun:unary-op (lambda (x) (g:* a x))) sigfun))

(define (sigfun:scale2 sigfun a)
  ((sigfun:unary-op (lambda (x) (g:* x a))) sigfun))

(define (sigfun:scale3 a sigfun)
  ((sigfun:unary-op (lambda (x) (g:/ a x))) sigfun))

(define (sigfun:scale4 sigfun a)
  ((sigfun:unary-op (lambda (x) (g:/ x a))) sigfun))


(define (sigfun:shift sigfun shift)
  (sigfun:make (g:arg-shift (sigfun:procedure sigfun) shift)
	       (sigfun:span sigfun)))


(define (sigfun:reverse sigfun)
  (let ((span (sigfun:span sigfun)))
    (sigfun:make (lambda (x) ((sigfun:procedure sigfun) (- x)))
		 (sigfun:make-span (sigfun:max span) (sigfun:min span)))))


(define (sigfun:apply sigfun args)
  (g:apply (sigfun:procedure sigfun) args))
  


(assign-operation generic:negate          sigfun:negate         sigfun?)
(assign-operation generic:invert          sigfun:invert         sigfun?)

(assign-operation generic:sqrt            sigfun:sqrt           sigfun?)
(assign-operation generic:square          sigfun:square         sigfun?)

(assign-operation generic:exp             sigfun:exp            sigfun?)
(assign-operation generic:log             sigfun:log            sigfun?)

(assign-operation generic:sin             sigfun:sin            sigfun?)
(assign-operation generic:cos             sigfun:cos            sigfun?)

(assign-operation generic:asin            sigfun:asin           sigfun?)
(assign-operation generic:acos            sigfun:acos           sigfun?)

(assign-operation generic:sinh            sigfun:sinh           sigfun?)
(assign-operation generic:cosh            sigfun:cosh           sigfun?)

(assign-operation generic:+               sigfun:+              sigfun?  sigfun?)
(assign-operation generic:-               sigfun:-              sigfun?  sigfun?)
(assign-operation generic:*               sigfun:*              sigfun?  sigfun?)
(assign-operation generic:*               sigfun:scale          number?  sigfun?)
(assign-operation generic:*               sigfun:scale2         sigfun?  number?)

(assign-operation generic:/               sigfun:/              sigfun?  sigfun?)
(assign-operation generic:/               sigfun:scale3         number?  sigfun?)
(assign-operation generic:/               sigfun:scale4         sigfun?  number?)

(assign-operation generic:solve-linear-right    sigfun:/              sigfun?  sigfun?)
(assign-operation generic:solve-linear-right    sigfun:scale3         number?  sigfun?)
(assign-operation generic:solve-linear-right    sigfun:scale4         sigfun?  number?)

(assign-operation generic:solve-linear-left  (lambda (x y) (sigfun:/ y x))       sigfun?  sigfun?)
(assign-operation generic:solve-linear-left  (lambda (x y) (sigfun:scale3 y x))  sigfun?  number?)
(assign-operation generic:solve-linear-left  (lambda (x y) (sigfun:scale4 y x))  number?  sigfun?)

(assign-operation generic:solve-linear  (lambda (x y) (sigfun:/ y x))       sigfun?  sigfun?)
(assign-operation generic:solve-linear  (lambda (x y) (sigfun:scale3 y x))  sigfun?  number?)
(assign-operation generic:solve-linear  (lambda (x y) (sigfun:scale4 y x))  number?  sigfun?)

(assign-operation generic:expt            sigfun:expt           sigfun?  sigfun?)
(assign-operation generic:expt            sigfun:expt2          number?  sigfun?)
(assign-operation generic:expt            sigfun:expt3          sigfun?  number?)


(assign-operation generic:make-rectangular sigfun:make-rectangular  sigfun? sigfun?)
(assign-operation generic:make-polar       sigfun:make-polar        sigfun? sigfun?)

(assign-operation generic:real-part        sigfun:real-part      sigfun?)
(assign-operation generic:imag-part        sigfun:imag-part      sigfun?)
(assign-operation generic:magnitude        sigfun:magnitude      sigfun?)
(assign-operation generic:angle            sigfun:angle          sigfun?)

(assign-operation generic:conjugate        sigfun:conjugate      sigfun?)

(assign-operation generic:atan1            sigfun:atan           sigfun?)
(assign-operation generic:atan2            sigfun:atan2          sigfun? sigfun?)

(assign-operation generic:apply            sigfun:apply          sigfun? any?)


#|
;;; See show.scm

(define (sigfun:dB cutoff return sigfun)
  (let ((span (sigfun:span sigfun))
	(epsilon (expt 10 (/ cutoff 20))))
    (sigfun:make (compose (safe-dB epsilon return)
			  (sigfun:procedure sigfun))
		 span)))


(define ((safe-dB epsilon return) x)
  (if (< x epsilon)
      return
      (* 20 (log10 x))))
|#