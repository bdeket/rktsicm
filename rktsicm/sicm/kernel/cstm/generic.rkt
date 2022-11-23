#lang racket/base

(provide (except-out (all-defined-out) define-g)
         make-assign-operations)

(require (for-syntax racket/base
                     racket/syntax)
         (only-in "../../rkt/glue.rkt" default-object default-object? fix:> fix:+)
         "../ghelper.rkt"
         "../numeric.rkt"
         "s-operator.rkt"
         "../../parameters.rkt")

(define-syntax (define-g stx)
  (syntax-case stx ()
    [(_ name expr)
     (with-syntax ([gen_name (format-id #'name "generic:~a" (regexp-replace #px"g:" (format "~a" (syntax->datum #'name)) ""))]
                   [g_name (format-id #'name "g:~a" (regexp-replace #px"g:" (format "~a" (syntax->datum #'name)) ""))])
       (syntax/loc #'name
         (begin
           (define gen_name expr)
           (define g_name gen_name))))]))

;generic:fct are functions that choose procedure based on args using the system in
;            ghelper
;g:fct       are functions on top of this
;for example generic:+ works on 2 arguments g:+ works on n arguments
;for most functions g:fct just references generic:fct

;;;; Primitive Generic Operation Declarations 

;;; Unary Operators 

(define-g g:type (make-generic-operator 1 'type))

(define-g g:type-predicate (make-generic-operator 1 'type-predicate))

(define-g g:arity (make-generic-operator 1 'arity (lambda (x) #f)))


(define-g g:inexact?
  (make-generic-operator 1 'inexact?))

(define-g g:zero-like
  (make-generic-operator 1 'zero-like (lambda (x) :zero)))

(define-g g:one-like
  (make-generic-operator 1 'one-like (lambda (x) :one)))

;;bdk;; from mathutil.scm
(define (g:identity x) x)

(define-g g:identity-like
  (make-generic-operator 1 'identity-like (lambda (x) g:identity)))


;;; Generic tests are conservative.  
;;; They will return #f unless the answer is known true.

(define generic:zero?
  (make-generic-operator 1 'zero? (lambda (x) #f)))

(define (g:zero? x)
  (if (number? x) (exact-zero? x) (generic:zero? x)))


(define generic:one? (make-generic-operator 1 'one? (lambda (x) #f)))

(define (g:one? x)
    (if (number? x) (exact-one? x) (generic:one? x)))


(define-g g:identity? (make-generic-operator 1 'identity? (lambda (x) #f)))

(define-g g:negate (make-generic-operator 1 'negate))

(define-g g:invert (make-generic-operator 1 'invert))

(define-g g:square (make-generic-operator 1 'square (lambda (x) (g:* x x))))

(define-g g:sqrt (make-generic-operator 1 'sqrt))

(define-g g:exp (make-generic-operator 1 'exp))

(define-g g:log (make-generic-operator 1 'log))

(define-g g:sin (make-generic-operator 1 'sin))

(define-g g:cos (make-generic-operator 1 'cos))

(define-g g:asin (make-generic-operator 1 'asin))

(define-g g:acos (make-generic-operator 1 'acos))

(define-g g:sinh (make-generic-operator 1 'sinh))

(define-g g:cosh (make-generic-operator 1 'cosh))

(define-g g:abs (make-generic-operator 1 'abs))

(define-g g:determinant (make-generic-operator 1 'determinant))

(define-g g:trace
  (make-generic-operator 1
                         'trace
                         ;;this probably doesn't really work as expected...
                         (λ (f)
                           (let ([i 0])
                             (λ args
                               (define j i)
                               (set! i (+ i 1))
                               (printf "trace_~a > ~a\n" j (list* f args))
                               (define ans (apply f args))
                               (printf "trace_~a < ~a\n" j ans))))))

;!!!
(define generic:transpose (make-generic-operator 1 'transpose))

(define-g g:dimension
  (make-generic-operator 1
                         'dimension
                         #; ;assign in calculus/manifold
                         (lambda (x)
                           ;;definition in calculus/manifold.scm
                           (coordinate-system-dimension x))))

(define-g g:solve-linear-left
  (make-generic-operator 2 'solve-linear-left))

(define-g g:solve-linear-right
  (make-generic-operator 2 'solve-linear-right))

(define-g g:solve-linear
  (make-generic-operator 2 'solve-linear))

;;bdk;; moved to s-operator

(define generic:partial-derivative
  (make-generic-operator 2 'partial-derivative))

(define g:derivative
  (make-operator
   (lambda (f)
     (generic:partial-derivative f '()))
   'derivative))

(define (g:partial-derivative f . varspecs)
  (generic:partial-derivative f varspecs))

(define (g:partial . varspecs)
  (make-operator
   (lambda (f)
     (generic:partial-derivative f varspecs))
   `(partial ,@varspecs)))

;;; Binary Operators

(define generic:= (make-generic-operator 2 '= (lambda (x y) #f)))

(define (g:=:bin x y)
  (if (and (number? x) (number? y)) (= x y) (generic:= x y)))


(define generic:< (make-generic-operator 2 '< (lambda (x y) #f)))

(define (g:<:bin x y)
  (if (and (number? x) (number? y)) (< x y) (generic:< x y)))


(define generic:<= (make-generic-operator 2 '<= (lambda (x y) #f)))

(define (g:<=:bin x y)
  (if (and (number? x) (number? y)) (<= x y) (generic:<= x y)))


(define generic:> (make-generic-operator 2 '> (lambda (x y) #f)))

(define (g:>:bin x y)
  (if (and (number? x) (number? y)) (> x y) (generic:> x y)))


(define generic:>= (make-generic-operator 2 '>= (lambda (x y) #f)))

(define (g:>=:bin x y)
  (if (and (number? x) (number? y)) (>= x y) (generic:>= x y)))

(define generic:+ (make-generic-operator 2 '+))

(define (g:+:bin x y)
  (cond ((and (number? x) (number? y)) (+ x y))
	((g:zero? x) y)
	((g:zero? y) x)
	(else (generic:+ x y))))


(define generic:- (make-generic-operator 2 '-))

(define (g:-:bin x y)
  (cond ((and (number? x) (number? y)) (- x y))
	((g:zero? y) x)
	((g:zero? x) (g:negate y))
	(else (generic:- x y))))


(define generic:* (make-generic-operator 2 '*))

(define (g:*:bin x y)
  (cond ((and (number? x) (number? y)) (* x y))
	((exact-zero? x) (g:zero-like y))
	((exact-zero? y) (g:zero-like x))
	((g:one? x) y)
	((g:one? y) x)
	(else (generic:* x y))))

;;; In g:*:bin we test for exact (numerical) zero 
;;; because it is possible to produce a wrong-type 
;;; zero here, as follows:

;;;		  |0|             |0|
;;;	  |a b c| |0|   |0|       |0|
;;;	  |d e f| |0| = |0|, not  |0|

;;; We are less worried about the zero? below,
;;; because any invertible matrix is square.


(define generic:/ (make-generic-operator 2 '/))

(define (g:/:bin x y)
  (cond ((and (number? x) (number? y)) (/ x y))
	;; ((g:zero? x) (g:zero-like y))  ; Ancient bug!  No consequence.
	;; ((g:zero? x) x)
	((g:one? y) x)
	(else (generic:/ x y))))

(define generic:expt (make-generic-operator 2 'expt))

(define g:gcd:bin (make-generic-operator 2 'gcd))
(define generic:gcd g:gcd:bin)

(define-g g:dot-product (make-generic-operator 2 'dot-product))
(define-g g:cross-product (make-generic-operator 2 'cross-product))

(define-g g:outer-product (make-generic-operator 2 'outer-product))


;;; Complex Operators

(define-g g:make-rectangular (make-generic-operator 2 'make-rectangular))
(define-g g:make-polar (make-generic-operator 2 'make-polar))

(define-g g:real-part (make-generic-operator 1 'real-part))
(define-g g:imag-part (make-generic-operator 1 'imag-part))

(define-g g:magnitude (make-generic-operator 1 'magnitude))
(define-g g:angle (make-generic-operator 1 'angle))

(define-g g:conjugate (make-generic-operator 1 'conjugate))


;;; Weird operators

(define (g:atan y [x default-object])
  (if (default-object? x) (g:atan1 y) (g:atan2 y x)))

(define-g g:atan1 (make-generic-operator 1 'atan1))
(define-g g:atan2 (make-generic-operator 2 'atan2))

(define generic:apply (make-generic-operator 2 'apply))

;;bdk;; the nesting to make it possible to require litfun went to deep (cyclic dependencies)
;;bdk;; so in the end I opted to make it possible to extend the g:apply function at a later time
;;bdk;; for now, this is only happening in litfun.rkt
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

;;; N-ary Operator extensions

(define (g:= . args)
  (g:=:n args))

(define (g:=:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:=:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:=:bin larg (car args)))))))))


(define (g:< . args)
  (g:<:n args))

(define (g:<:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:<:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:<:bin larg (car args)))))))))

(define (g:<= . args)
  (g:<=:n args))

(define (g:<=:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:<=:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:<=:bin larg (car args)))))))))

(define (g:> . args)
  (g:>:n args))

(define (g:>:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:>:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:>:bin larg (car args)))))))))


(define (g:>= . args)
  (g:>=:n args))

(define (g:>=:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:>=:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:>=:bin larg (car args)))))))))

(define (g:+ . args)
  (g:+:n args))

(define (g:+:n args)
  (cond ((null? args) :zero)
	((null? (cdr args)) (car args))
	(else
	 (let lp ((args (cddr args))
		  (ans (g:+:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (g:+:bin ans (car args))))))))

(define (g:* . args)
  (g:*:n args))

(define (g:*:n args)
  (cond ((null? args) :one)
	((null? (cdr args)) (car args))
	(else
	 (let lp ((args (cddr args))
		  (ans (g:*:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (g:*:bin ans (car args))))))))

(define (g:- . args)
  (g:-:n args))

(define (g:-:n args)
  (cond ((null? args) :zero)
	((null? (cdr args)) (g:negate (car args)))
	(else
	 (g:-:bin (car args)
		  (g:+:n (cdr args))))))

(define (g:/ . args)
  (g:/:n args))

(define (g:/:n args)
  (cond ((null? args) :one)
	((null? (cdr args)) (g:invert (car args)))
	(else
	 (g:/:bin (car args)
		  (g:*:n (cdr args))))))

(define (g:gcd . args)
  (g:gcd:n args))

(define (g:gcd:n args)
  (cond ((null? args) :zero)
	((null? (cdr args)) (car args))
	(else
	 (let lp
	     ((as (cddr args))
	      (ans (g:gcd:bin (car args) (cadr args))))
	   (cond ((null? as) ans)
		 ((g:one? ans) ans)
		 (else
		  (lp (cdr as) (g:gcd:bin ans (car as)))))))))

;;bdk;; was in mathutil.rkt
(define (g:sigma f low high)
  (if (fix:> low high)
      0
      (let lp ((i (fix:+ low 1)) (sum (f low)))
	(if (fix:> i high)
	    sum
	    (lp (fix:+ i 1) (g:+ sum (f i)))))))



; was in simplify/simplify

(define-g g:simplify (make-generic-operator 1 'simplify))

