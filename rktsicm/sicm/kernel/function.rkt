#lang racket/base

(provide (all-defined-out))

(require "../rkt/default-object.rkt"
         "generic.rkt"
         "types.rkt"
         "utils.rkt"
         )

;;;;            Functions

(define (f:type f) function-type-tag)
(define (f:type-predicate f) function-quantity?)

;;; Arity manipulation procedures are in UTILS.SCM
(define (p-rename op fct)
  (define a (regexp-replace #px"#<procedure:(.*)>" (format "~a" op) "\\1"))
  (define b (regexp-replace #px"$g:" a ""))
  (define c (regexp-replace #px"^_(.*)_$" b "\\1"))
  (procedure-rename fct (string->symbol (format "f:~a" c))))

(define ((f:unary operator) f)
  (p-rename operator (compose-bin operator f)))

(define ((f:binary operator) f1 f2)
  (let ((f1 (if (function? f1) f1 (coerce-to-function f1)))
	(f2 (if (function? f2) f2 (coerce-to-function f2))))	
    (let ((a (joint-arity (g:arity f1) (g:arity f2))))
      (unless a
	  (error "Functions have different arities" f1 f2))
      (p-rename operator
       (cond ((equal? a *at-least-zero*)
              (lambda x
                (operator (apply f1 x) (apply f2 x))))
             ((equal? a *exactly-zero*)
              (lambda ()
                (operator (f1) (f2))))
             ((equal? a *at-least-one*)
              (lambda (x . y)
                (operator (apply f1 x y) (apply f2 x y))))
             ((equal? a *exactly-one*)
              (lambda (x)
                (operator (f1 x) (f2 x))))
             ((equal? a *at-least-two*)
              (lambda (x y . z)
                (operator (apply f1 x y z) (apply f2 x y z))))
             ((equal? a *exactly-two*)
              (lambda (x y)
                (operator (f1 x y) (f2 x y))))
             ((equal? a *at-least-three*)
              (lambda (u x y . z)
                (operator (apply f1 u x y z) (apply f2 u x y z))))
             ((equal? a *exactly-three*)
              (lambda (x y z)
                (operator (f1 x y z) (f2 x y z))))
             ((equal? a *one-or-two*)
              (lambda (x [y default-object])
                (if (default-object? y)
                    (operator (f1 x) (f2 x))
                    (operator (f1 x y) (f2 x y)))))
             (else
              (lambda x
                (operator (apply f1 x) (apply f2 x)))))))))

(define ((coerce-to-function g) . x)
  (if (numerical-quantity? g)
      g
      (g:apply g x)))

(define (f:arity f) (procedure-arity f))

(define (f:zero-like f)			;want (zero-like range-element)
  (lambda any (g:zero-like (apply f any))))

(define (f:one-like f)			;want (one-like range-element)
  (lambda any (g:one-like (apply f any))))

(define (f:identity-like f) g:identity)

(assign-operation generic:type            f:type            function?)
(assign-operation generic:type-predicate  f:type-predicate  function?)
(assign-operation generic:arity           f:arity           function?)

(assign-operation generic:inexact?        (f:unary g:inexact?)       function?)

(assign-operation generic:zero-like       f:zero-like                function?)
(assign-operation generic:one-like        f:one-like                 function?)
(assign-operation generic:identity-like   f:identity-like            function?)

;;; The following tests conflict with the conservative theory of
;;; generic predicates in that they return a new procedure with a
;;; deferred test rather than #f, because they cannot know the
;;; result.  Indeed, a user may write (compose zero? f) if necessary.

;;;(assign-operation generic:zero?           (f:unary g:zero?)          function?)
;;;(assign-operation generic:one?            (f:unary g:one?)           function?)
;;;(assign-operation generic:identity?       (f:unary g:identity?)      function?)

(assign-operation generic:negate          (f:unary g:negate)         function?)
(assign-operation generic:invert          (f:unary g:invert)         function?)

(assign-operation generic:sqrt            (f:unary g:sqrt)           function?)
(assign-operation generic:square          (f:unary g:square)         function?)

(assign-operation generic:exp             (f:unary g:exp)            function?)
(assign-operation generic:log             (f:unary g:log)            function?)

(assign-operation generic:sin             (f:unary g:sin)            function?)
(assign-operation generic:cos             (f:unary g:cos)            function?)

(assign-operation generic:asin            (f:unary g:asin)           function?)
(assign-operation generic:acos            (f:unary g:acos)           function?)

(assign-operation generic:sinh            (f:unary g:sinh)           function?)
(assign-operation generic:cosh            (f:unary g:cosh)           function?)

(assign-operation generic:abs             (f:unary g:abs)            function?)

(assign-operation generic:determinant     (f:unary g:determinant)    function?)
(assign-operation generic:trace           (f:unary g:trace)          function?)

;;; Binary operations on functions are a bit weird.  A special predicate
;;; are needed to make the correct coercions possible:

;;; Tests must be conservative.
;;;(assign-operation generic:=               (f:binary g:=)             function? function?)

(assign-operation generic:+                  (f:binary g:+)             function? cofunction?)
(assign-operation generic:+                  (f:binary g:+)             cofunction? function?)
(assign-operation generic:-                  (f:binary g:-)             function? cofunction?)
(assign-operation generic:-                  (f:binary g:-)             cofunction? function?)
(assign-operation generic:*                  (f:binary g:*)             function? cofunction?)
(assign-operation generic:*                  (f:binary g:*)             cofunction? function?)
(assign-operation generic:/                  (f:binary g:/)             function? cofunction?)
(assign-operation generic:/                  (f:binary g:/)             cofunction? function?)

(assign-operation generic:dot-product        (f:binary g:dot-product)   function? cofunction?)
(assign-operation generic:dot-product        (f:binary g:dot-product)   cofunction? function?)

(assign-operation generic:cross-product      (f:binary g:cross-product)   function? cofunction?)
(assign-operation generic:cross-product      (f:binary g:cross-product)   cofunction? function?)

(assign-operation generic:expt               (f:binary g:expt)          function? cofunction?)
(assign-operation generic:expt               (f:binary g:expt)          cofunction? function?)

(assign-operation generic:gcd                (f:binary g:gcd)           function? cofunction?)
(assign-operation generic:gcd                (f:binary g:gcd)           cofunction? function?)

(assign-operation generic:make-rectangular   (f:binary g:make-rectangular)
	                                                        function? cofunction?)
(assign-operation generic:make-rectangular   (f:binary g:make-rectangular)
	                                                        cofunction? function?)

(assign-operation generic:make-polar         (f:binary g:make-polar)    function? cofunction?)
(assign-operation generic:make-polar         (f:binary g:make-polar)    cofunction? function?)

(assign-operation generic:real-part          (f:unary g:real-part)      function?)
(assign-operation generic:imag-part          (f:unary g:imag-part)      function?)
(assign-operation generic:magnitude          (f:unary g:magnitude)      function?)
(assign-operation generic:angle              (f:unary g:angle)          function?)

;(assign-operation generic:conjugate         (f:unary g:conjugate)      function?)

(assign-operation generic:atan1              (f:unary g:atan)           function?)
(assign-operation generic:atan2              (f:binary g:atan)          function? cofunction?)
(assign-operation generic:atan2              (f:binary g:atan)          cofunction? function?)

(assign-operation generic:solve-linear-right (f:binary g:solve-linear-right)  function? cofunction?)
(assign-operation generic:solve-linear-right (f:binary g:solve-linear-right)  cofunction? function?)

(assign-operation generic:solve-linear-left  (f:binary g:solve-linear-left)  cofunction? function?)
(assign-operation generic:solve-linear-left  (f:binary g:solve-linear-left)  function? cofunction?)

(assign-operation generic:solve-linear      (f:binary g:solve-linear)  cofunction? function?)
(assign-operation generic:solve-linear      (f:binary g:solve-linear)  function? cofunction?)

;;; This only makes sense for linear functions...
(define (((f:transpose f) g) a)
  (g (f a)))

(assign-operation generic:transpose          f:transpose                function?)

#|
;;; 

(define (transpose-defining-relation T g a)
  ;; T is a linear transformation T:V -> W
  ;; the transpose of T, T^t:W* -> V* 
  ;; Forall a in V, g in W*,  g:W -> R
  ;; (T^t(g))(a) = g(T(a)).
  (- (((f:transpose T) g) a) (g (T a))))

(let ((DTf
	(let ((T (literal-function 'T (-> (UP Real Real) (UP Real Real Real)))))
	  (let ((DT (D T)))
	    (lambda (s)
	      (lambda (x)
		(* (DT s) x))))))

      (a (up 'a^0 'a^1))
      (g (lambda (w) (* (down 'g_0 'g_1 'g_2) w)))

      (s (up 'x 'y)))
  (pec (transpose-defining-relation (DTf s) g a))
  (((f:transpose (DTf s)) g) a))

#| Result: 0 |#
#|
(+ (* a^0 g_0 (((partial 0) T^0) (up x y)))
   (* a^0 g_1 (((partial 0) T^1) (up x y)))
   (* a^0 g_2 (((partial 0) T^2) (up x y)))
   (* a^1 g_0 (((partial 1) T^0) (up x y)))
   (* a^1 g_1 (((partial 1) T^1) (up x y)))
   (* a^1 g_2 (((partial 1) T^2) (up x y))))
|#

|#
