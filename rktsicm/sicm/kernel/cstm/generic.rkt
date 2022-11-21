#lang racket/base

(provide (except-out (all-defined-out) define-generic))
(require (for-syntax racket/base
                     racket/syntax)
         "../ghelper.rkt"
         "numeric.rkt")

(define-syntax (define-generic stx)
  (syntax-case stx ()
    [(_ name arity) (syntax/loc stx (define-generic name arity #f))]
    [(_ name arity default)
     (with-syntax ([gen_name (format-id #'name "generic:~a" #'name)]
                   [g_name (format-id #'name "g:~a" #'name)])
       (syntax/loc #'name
         (begin
           (define gen_name (make-generic-operator arity 'name default))
           (define g_name gen_name))))]))

;generic:fct are functions that chose procedure based on args using the system in
;            ghelper
;g:fct       are functions on top of this
;for example generic:+ works on 2 arguments g:+ works on n arguments
;for most functions g:fct just references generic:fct

;;; Unary Operators 

(define-generic type 1)
(define-generic type-predicate 1)
(define-generic arity 1 (lambda (x) #f))

(define-generic inexact? 1)

(define-generic zero-like 1 (lambda (x) :zero))
(define-generic one-like 1 (lambda (x) :one))

(define-generic identity-like 1)


;;; Generic tests are conservative.  
;;; They will return #f unless the answer is known true.

;!!!
(define generic:zero? (make-generic-operator 1 'zero? (lambda (x) #f)))
(define (g:zero? x)
  (if (number? x) (exact-zero? x) (generic:zero? x)))

;!!!
(define generic:one? (make-generic-operator 1 'one? (lambda (x) #f)))
(define (g:one? x)
    (if (number? x) (exact-one? x) (generic:one? x)))

(define-generic identity? 1 (lambda (x) #f))

(define-generic negate 1)

(define-generic invert 1)

(define-generic square 1)
(define-generic sqrt 1)
(define-generic exp 1)

(define-generic log 1)

(define-generic sin 1)
(define-generic cos 1)

(define-generic asin 1)
(define-generic acos 1)

(define-generic sinh 1)
(define-generic cosh 1)

(define-generic abs 1)

(define-generic determinant 1)

(define-generic trace 1
  ;;this probably doesn't really work as expected...
  (λ (f)
    (let ([i 0])
      (λ args
        (define j i)
        (set! i (+ i 1))
        (printf "trace_~a > ~a\n" j (list* f args))
        (define ans (apply f args))
        (printf "trace_~a < ~a\n" j ans)))))

;!!!
(define generic:transpose (make-generic-operator 1 'transpose))

(define-generic dimension 1)

(define-generic solve-linear-left 2)

(define-generic solve-linear-right 2)

(define-generic solve-linear 2)

;!!!
(define generic:partial-derivative
  (make-generic-operator 2 'partial-derivative))

;;; Binary Operators

;>!!!
(define generic:= (make-generic-operator 2 '= (lambda (x y) #f)))

(define generic:< (make-generic-operator 2 '< (lambda (x y) #f)))

(define generic:<= (make-generic-operator 2 '<= (lambda (x y) #f)))

(define generic:> (make-generic-operator 2 '> (lambda (x y) #f)))

(define generic:>= (make-generic-operator 2 '>= (lambda (x y) #f)))

(define generic:+ (make-generic-operator 2 '+))

(define generic:- (make-generic-operator 2 '-))

(define generic:* (make-generic-operator 2 '*))

(define generic:/ (make-generic-operator 2 '/))

(define generic:expt (make-generic-operator 2 'expt))
;<!!!

;!!!
(define generic:gcd (make-generic-operator 2 'gcd))

(define-generic dot-product 2)
(define-generic cross-product 2)
(define-generic outer-product 2)


;;; Complex Operators

(define-generic make-rectangular 2)
(define-generic make-polar 2)

(define-generic real-part 1)
(define-generic imag-part 1)

(define-generic magnitude 1)
(define-generic angle 1)

(define-generic conjugate 1)


;;; Weird operators

(define-generic atan1 1)
(define-generic atan2 2)

(define generic:apply (make-generic-operator 2 'apply))

; was in simplify/simplify

(define-generic simplify 1)

