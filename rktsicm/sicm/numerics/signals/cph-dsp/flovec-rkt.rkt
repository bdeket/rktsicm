#lang racket/base

(provide (all-defined-out)
         (all-from-out 'flo:vector))

(require "../../../rkt/fixnum.rkt"
         (except-in racket/flonum ->fl)
         racket/vector
         )

;;;; Floating-Point Vector Utilities
;inside representation
(module flo:vector racket/base
  (require racket/flonum)
  
  (provide flo:vector
           flo:vector-length
           flo:vector-ref
           flo:vector-set!
           flo:vector?
           in-flo:vector
           for/flo:vector
           set-<flo:vector>-l!
           ->flonum)
  
  (define (->flonum x) (if (flonum? x) x (exact->inexact x)))

  (struct <flo:vector> (v [l #:mutable]) #:transparent
    #:methods gen:equal+hash
    [(define equal-proc
       (λ (v1 v2 _)
         (for/and ([i1 (in-flo:vector v1)]
                   [i2 (in-flo:vector v2)])
           (fl= i1 i2))))
     (define hash-proc
       (λ (v _) (equal-hash-code (in-flo:vector v))))
     (define hash2-proc
       (λ (v _) (equal-secondary-hash-code (in-flo:vector v))))])
  (define (flo:vector . vs) (for/flo:vector ([v (in-list vs)]) (->flonum v)))

  (define (flo:vector-length FV)   (<flo:vector>-l FV))
  (define (flo:vector-ref FV n)    (flvector-ref  (<flo:vector>-v FV) n))
  (define (flo:vector-set! FV n v) (flvector-set! (<flo:vector>-v FV) n v))
  (define flo:vector? <flo:vector>?)
  
  (define (in-flo:vector FV [start 0] [end (<flo:vector>-l FV)] [step 1])
    (unless (<= end (<flo:vector>-l FV))
      (raise-argument-error in-flo:vector (format "end-in-range [0;~a]" (<flo:vector>-l FV)) end))
    (in-flvector (<flo:vector>-v FV) start end step))

  (define-syntax-rule (for/flo:vector rst ...)
    (let ([v (for/flvector rst ...)]) (<flo:vector> v (flvector-length v))))
  )
(require 'flo:vector)

;outside access
(define (flonum-vector->vector vector)
  (guarantee-flonum-vector vector 'FLONUM-VECTOR->VECTOR)
  (for/vector ([v (in-flo:vector vector)]) v))

(define (flonum-vector->list vector)
  (guarantee-flonum-vector vector 'FLONUM-VECTOR->LIST)
  (for/list ([v (in-flo:vector vector)]) v))

(define (vector->flonum-vector vector)
  (when (not (vector? vector))
      (raise-argument-error 'VECTOR->FLONUM-VECTOR "vector" vector))
  (for/flo:vector ([i (in-vector vector)]) (->flonum i)))

(define (list->flonum-vector lst)
  (when (not (list? lst))
      (raise-argument-error 'LIST->FLONUM-VECTOR "list" lst))
  (for/flo:vector ([i (in-list lst)]) (->flonum i)))

(define (flo:make-vector n [value 0.0])
  (guarantee-nonnegative-fixnum n 'FLO:MAKE-VECTOR)
  (define flval (->flonum value))
  (for/flo:vector ([i (in-range n)]) flval))

(define (flo:make-initialized-vector n initialization)
  (guarantee-nonnegative-fixnum n 'FLO:MAKE-INITIALIZED-VECTOR)
  (for/flo:vector ([i (in-range n)]) (->flonum (initialization i))))

(define (flo:subvector vector start end)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR)
  (for/flo:vector ([v (in-flo:vector vector start end)]) v))

(define (flo:vector-grow vector length [value 0.0])
  (guarantee-flonum-vector vector 'FLO:VECTOR-GROW)
  (guarantee-nonnegative-fixnum length 'FLO:VECTOR-GROW)
  (define flval (->flonum value))
  (define len (flo:vector-length vector))
  (for/flo:vector ([i (in-range (+ len length))])
    (if (< i len)
        (flo:vector-ref vector i)
        flval)))

(define (flo:subvector-move! source start-source end-source
			     target start-target)
  (guarantee-flonum-subvector source start-source end-source
			      'FLO:SUBVECTOR-MOVE!)
  (guarantee-flonum-vector target 'FLO:SUBVECTOR-MOVE!)
  (guarantee-nonnegative-fixnum start-target 'FLO:SUBVECTOR-MOVE!)
  (let ((end-target (fix:+ start-target (fix:- end-source start-source))))
    (guarantee-flonum-subvector-range target start-target end-target
				      'FLO:SUBVECTOR-MOVE!)
    (cond
      [(eq? source target)
       (define up? (fix:< start-source start-target))
       (for ([v (if up?
                    (in-flo:vector source (- end-source 1) (- start-source 1) -1)
                    (in-flo:vector source start-source end-source))]
             [i (if up?
                    (in-range (- end-target 1) 0 -1)
                    (in-naturals start-target))])
         (flo:vector-set! source i v))]
      [else
       (for ([v (in-flo:vector source start-source end-source)]
             [i (in-naturals start-target)])
         (flo:vector-set! target i v))])))

(define (flo:vector-map vector procedure)
  (guarantee-flonum-vector vector 'FLO:VECTOR-MAP)
  (for/flo:vector ([v (in-flo:vector vector)]) (procedure v)))

(define (flo:vector-map! vector procedure)
  (guarantee-flonum-vector vector 'FLO:VECTOR-MAP!)
  (for ([v (in-flo:vector vector)]
        [i (in-naturals)])
    (flo:vector-set! vector i (procedure v)))
  vector)

(define (flo:vector-for-each vector procedure)
  (guarantee-flonum-vector vector 'FLO:VECTOR-FOR-EACH)
  (for ([v (in-flo:vector vector)])
    (procedure v)))

(define (flo:subvector-for-each vector start end procedure)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR-FOR-EACH)
  (for ([v (in-flo:vector vector start end)])
    (procedure v)))

(define (flo:vector-fill! vector value)
  (guarantee-flonum-vector vector 'FLO:VECTOR-FILL!)
  (define flval (->flonum value))
  (for ([i (in-range (flo:vector-length vector))])
    (flo:vector-set! vector i flval)))

(define (flo:subvector-fill! vector start end value)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR-FILL!)
  (for ([i (in-range start end)])
    (flo:vector-set! vector i (->flonum value))))

#|
(define-syntax ucode-primitive
  (lambda (name arity)
    (make-primitive-procedure name arity)))

(define-syntax ucode-type
  (lambda (name)
    (microcode-type name)))
|#


  (define (flo:set-vector-length! vector n)
    (guarantee-flonum-vector vector 'FLO:SET-VECTOR-LENGTH!)
    (guarantee-nonnegative-fixnum n 'FLO:SET-VECTOR-LENGTH!)
    (when (not (fix:<= n (flo:vector-length vector)))
      (raise-range-error 'FLO:SET-VECTOR-LENGTH!
                         "FLO:VECTOR"
                         "length "
                         n
                         (flonum-vector->vector vector)
                         0 (flo:vector-length vector)))
    (set-<flo:vector>-l! vector n))

(define (guarantee-flonum-vector object procedure)
  (when (not (flo:vector? object))
    (raise-argument-error procedure "flo:vector" object)))

(define (guarantee-nonnegative-fixnum object procedure)
  (when (not (and (fixnum? object) (fix:>= object 0)))
    (raise-argument-error procedure "non-negative fixnum" object)))

(define (guarantee-flonum-subvector v s e procedure)
  (guarantee-flonum-vector v procedure)
  (guarantee-nonnegative-fixnum s procedure)
  (guarantee-nonnegative-fixnum e procedure)
  (guarantee-flonum-subvector-range v s e procedure))

(define (guarantee-flonum-subvector-range v s e procedure)
  (when (not (fix:<= s e))
    (raise-range-error procedure
                       "FLO:VECTOR"
                       "end"
                       e
                       (flonum-vector->vector vector)
                       s (flo:vector-length vector) 0))
  (when (not (fix:<= e (flo:vector-length v)))
    (raise-range-error procedure
                       "FLO:VECTOR"
                       "end"
                       e
                       (flonum-vector->vector vector)
                       0 (flo:vector-length vector))))