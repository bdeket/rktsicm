#lang racket/base

(provide (except-out (all-defined-out) ->fl)
         (all-from-out 'flo:vector))

(require "../../../rkt/fixnum.rkt"
         racket/flonum
         racket/vector
         )

(define ->fl exact->inexact)
;;;; Floating-Point Vector Utilities
;inside representation
(module flo:vector racket/base
  (require racket/vector
           racket/flonum)

  (provide flo:vector
           flo:vector-length
           flo:vector-ref
           flo:vector-set!
           flo:vector?
           in-flo:vector
           for/flo:vector
           set-<flo:vector>-l!)
  
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
  (define (flo:vector . vs) (for/flo:vector ([v (in-list vs)]) (->fl v)))

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
  (for/flo:vector ([i (in-vector vector)]) (->fl i)))

(define (list->flonum-vector lst)
  (when (not (list? lst))
      (raise-argument-error 'LIST->FLONUM-VECTOR "list" lst))
  (for/flo:vector ([i (in-list lst)]) (->fl i)))

(define (flo:make-vector n [value 0.0])
  (guarantee-nonnegative-fixnum n 'FLO:MAKE-VECTOR)
  (define flval (->fl value))
  (for/flo:vector ([i (in-range n)]) flval))

(define (flo:build-vector n initialization)
  (guarantee-nonnegative-fixnum n 'FLO:MAKE-INITIALIZED-VECTOR)
  (for/flo:vector ([i (in-range n)]) (->fl (initialization i))))

(define (flo:subvector vector start end)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR)
  (for/flo:vector ([v (in-flo:vector vector start end)]) v))

(define (flo:vector-grow vector length [value 0.0])
  (guarantee-flonum-vector vector 'FLO:VECTOR-GROW)
  (guarantee-nonnegative-fixnum length 'FLO:VECTOR-GROW)
  (define flval (->fl value))
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
  (define flval (->fl value))
  (for ([i (in-range (flo:vector-length vector))])
    (flo:vector-set! vector i flval)))

(define (flo:subvector-fill! vector start end value)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR-FILL!)
  (for ([i (in-range start end)])
    (flo:vector-set! vector i (->fl value))))

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

#;(module+ test
  (require rackunit)
  (define F (flo:build-vector 5 values))
  (check-equal? (flonum-vector->vector F)
                #(0. 1. 2. 3. 4.))
  (check-equal? (flonum-vector->list F)
                '(0. 1. 2. 3. 4.))
  (check-equal? (vector->flonum-vector #(0 1 2 3 4))
                F)
  (check-equal? (list->flonum-vector '(0 1 2 3 4))
                F)
  (check-equal? (flo:make-vector 2 2)
                (flo:vector 2 2))
  (check-equal? (flo:build-vector 2 (λ (i) (expt (+ i 2) 2)))
                (flo:vector 4 9))
  (check-equal? (flo:subvector F 1 3)
                (flo:build-vector 2 add1))
  (check-equal? (flo:vector-grow F 3 5)
                (flo:vector 0 1 2 3 4 5 5 5))

  (flo:subvector-move! F 0 3 F 1)
  (check-equal? F (flo:vector 0 0 1 2 4))

  (flo:subvector-move! F 2 5 F 0)
  (check-equal? F (flo:vector 1 2 4 2 4))

  (define G (flo:make-vector 8 3))
  (flo:subvector-move! F 0 5 G 2)
  (check-equal? F (flo:vector 1 2 4 2 4))
  (check-equal? G (flo:vector 3 3 1 2 4 2 4 3))

  (check-equal? (flo:vector-map F (λ (x) (fl+ 2. x)))
                (flo:vector 3 4 6 4 6))

  (void (flo:vector-map! F (λ (x) (fl* 2. x))))
  (check-equal? F (flo:vector 2 4 8 4 8))

  (check-equal? (let ([s 0.0]) (flo:vector-for-each F (λ (v) (set! s (+ s v)))) s)
                26.)

  (check-equal? (let ([s 0.0]) (flo:subvector-for-each F 1 3 (λ (v) (set! s (+ s v)))) s)
                12.)

  (flo:vector-fill! F 0)
  (check-equal? F (flo:vector 0 0 0 0 0))

  (flo:subvector-fill! G 3 5 0)
  (check-equal? G (flo:vector 3 3 1 0 0 2 4 3))

  (flo:set-vector-length! G 5)
  (check-equal? G (flo:vector 3 3 1 0 0))
  
  )