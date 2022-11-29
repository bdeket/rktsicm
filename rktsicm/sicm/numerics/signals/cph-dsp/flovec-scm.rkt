#lang racket/base

(provide (all-defined-out) (rename-out [frmargs flo:vector])
         guarantee-flonum-vector
         guarantee-nonnegative-fixnum
         guarantee-flonum-subvector
         guarantee-flonum-subvector-range
         flo:vector-length flo:vector-set! flo:vector-ref)

(require (only-in "../../../rkt/glue.rkt" if default-object default-object? unspecific define-integrable
                  fix:<= fix:= fix:>= fix:< fix:+ fix:- fix:fixnum? flo:flonum?
                  error:wrong-type-argument make-list)
         racket/flonum
         racket/vector
         )

(module flo:vector racket/base
  (provide (all-defined-out))
  (require racket/sequence
           "../../../general/assert.rkt")
  (define (error:bad-range-argument r p) (raise-argument-error p "index" r '??? '??? '???))

  (struct flo:vector (l H) #:mutable
    #:property prop:equal+hash
    (list (λ (A B ->b? b?) (and (= (flo:vector-l A) (flo:vector-l B))
                                (for/and ([i (in-range (flo:vector-l A))])
                                  (= (hash-ref (flo:vector-H A) i 0.)
                                     (hash-ref (flo:vector-H B) i 0.)))))
          (λ (A ->int b?) (equal-hash-code (for/vector ([i (in-range (flo:vector-l A))])
                                             (hash-ref (flo:vector-H A) i 0.))))))

  (define flo:vector-length flo:vector-l)
  (define (flo:vector-ref V i) (hash-ref (flo:vector-H V) i 0.))
  (define (flo:vector-cons n) (flo:vector n (make-hash)))
  (define (flo:vector-set! V i v) (hash-set! (flo:vector-H V) i v))
  (define (frmargs . args)
    (define H (make-hash))
    (for ([i (in-naturals)]
          [f (in-list args)])
      (assert (flonum? f) "flo:vector")
      (hash-set! H i f))
    (flo:vector (hash-count H) H))
  (define (in-flo:vector V)
    (sequence-map (λ (i) (hash-ref (flo:vector-H V) i 0.))
                  (in-range (flo:vector-l V)) )))
(require 'flo:vector)


;;;; Floating-Point Vector Utilities

(define (flonum-vector->vector vector)
  (guarantee-flonum-vector vector 'FLONUM-VECTOR->VECTOR)
  (let ((length (flo:vector-length vector)))
    (let ((result (make-vector length)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i length))
	(vector-set! result i (flo:vector-ref vector i)))
      result)))

(define (flonum-vector->list vector)
  (guarantee-flonum-vector vector 'FLONUM-VECTOR->LIST)
  (let ((length (flo:vector-length vector)))
    (for/list ([v (in-flo:vector vector)]) v)
    #;
    (let ((result (make-list length)))
      (do ((i 0 (fix:+ i 1))
	   (l result (cdr l)))
	  ((fix:= i length))
	(set-car! l (flo:vector-ref vector i)))
      result)))

(define (vector->flonum-vector vector)
  (if (not (vector? vector))
      (error:wrong-type-argument vector "vector" 'VECTOR->FLONUM-VECTOR))
  (let ((length (vector-length vector)))
    (let ((result (flo:vector-cons length)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i length))
	(flo:vector-set! result i (->flonum (vector-ref vector i))))
      result)))

(define (list->flonum-vector list)
  (let ((length (length list)))
    (let ((result (flo:vector-cons length)))
      (do ((i 0 (fix:+ i 1))
	   (l list (cdr l)))
	  ((fix:= i length))
	(flo:vector-set! result i (->flonum (car l))))
      result)))

(define (flo:make-vector n [value default-object])
  (guarantee-nonnegative-fixnum n 'FLO:MAKE-VECTOR)
  (let ((result (flo:vector-cons n)))
    (if (not (or (default-object? value) (not value)))
	(let ((value (->flonum value)))
	  (do ((i 0 (fix:+ i 1)))
	      ((fix:= i n))
	    (flo:vector-set! result i value))))
    result))

(define (flo:make-initialized-vector length initialization)
  (guarantee-nonnegative-fixnum length 'FLO:MAKE-INITIALIZED-VECTOR)
  (let ((vector (flo:vector-cons length)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length))
      (flo:vector-set! vector i (->flonum (initialization i))))
    vector))

(define (flo:subvector vector start end)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR)
  (let ((result (flo:vector-cons (fix:- end start))))
    (do ((i 0 (fix:+ i 1))
	 (j start (fix:+ j 1)))
	((fix:= j end))
      (flo:vector-set! result i (flo:vector-ref vector j)))
    result))

(define (flo:vector-grow vector length [value default-object])
  (guarantee-flonum-vector vector 'FLO:VECTOR-GROW)
  (guarantee-nonnegative-fixnum length 'FLO:VECTOR-GROW)
  (let ((length* (flo:vector-length vector)))
    (if (not (fix:>= length length*))
	(error:bad-range-argument length 'FLO:VECTOR-GROW))
    (let ((result (flo:vector-cons length)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i length*))
	(flo:vector-set! result i (flo:vector-ref vector i)))
      (if (not (or (default-object? value) (not value)))
	  (let ((value (->flonum value)))
	    (do ((i length* (fix:+ i 1)))
		((fix:= i length))
	      (flo:vector-set! result i value))))
      result)))

(define (flo:subvector-move! source start-source end-source
			     target start-target)
  (guarantee-flonum-subvector source start-source end-source
			      'FLO:SUBVECTOR-MOVE!)
  (guarantee-flonum-vector target 'FLO:SUBVECTOR-MOVE!)
  (guarantee-nonnegative-fixnum start-target 'FLO:SUBVECTOR-MOVE!)
  (let ((end-target (fix:+ start-target (fix:- end-source start-source))))
    (guarantee-flonum-subvector-range target start-target end-target
				      'FLO:SUBVECTOR-MOVE!)
    (if (and (eq? source target) (fix:< start-source start-target))
	(let ((limit (fix:- start-source 1)))
	  (do ((scan-source (fix:- end-source 1) (fix:- scan-source 1))
	       (scan-target (fix:- end-target 1) (fix:- scan-target 1)))
	      ((fix:= scan-source limit) unspecific)
	    (flo:vector-set! target
			     scan-target
			     (flo:vector-ref source scan-source))))
	(do ((scan-source start-source (fix:+ scan-source 1))
	     (scan-target start-target (fix:+ scan-target 1)))
	    ((fix:= scan-source end-source) unspecific)
	  (flo:vector-set! target
			   scan-target
			   (flo:vector-ref source scan-source))))))

(define (flo:vector-map vector procedure)
  (guarantee-flonum-vector vector 'FLO:VECTOR-MAP)
  (let ((length (flo:vector-length vector)))
    (let ((result (flo:vector-cons length)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i length))
	(flo:vector-set! result i
			 (->flonum (procedure (flo:vector-ref vector i)))))
      result)))

(define (flo:vector-map! vector procedure)
  (guarantee-flonum-vector vector 'FLO:VECTOR-MAP!)
  (let ((length (flo:vector-length vector)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length))
      (flo:vector-set! vector i
		       (->flonum (procedure (flo:vector-ref vector i))))))
  vector)

(define (flo:vector-for-each vector procedure)
  (guarantee-flonum-vector vector 'FLO:VECTOR-FOR-EACH)
  (let ((length (flo:vector-length vector)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length) unspecific)
      (procedure (flo:vector-ref vector i)))))

(define (flo:subvector-for-each vector start end procedure)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR-FOR-EACH)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end) unspecific)
    (procedure (flo:vector-ref vector i))))

(define (flo:vector-fill! vector value)
  (guarantee-flonum-vector vector 'FLO:VECTOR-FILL!)
  (let ((length (flo:vector-length vector))
	(value (->flonum value)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length) unspecific)
      (flo:vector-set! vector i value))))

(define (flo:subvector-fill! vector start end value)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR-FILL!)
  (let ((value (->flonum value)))
    (do ((i start (fix:+ i 1)))
	((fix:= i end) unspecific)
      (flo:vector-set! vector i value))))

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
  (if (not (fix:<= n (flo:vector-length vector)))
      (error:bad-range-argument n 'FLO:SET-VECTOR-LENGTH!))
  (set-flo:vector-l! vector n)
  #;
  (let ((mask (set-interrupt-enables! interrupt-mask/none)))
    ((make-primitive-procedure 'primitive-object-set! 3)
     ;;(ucode-primitive primitive-object-set! 3)
     vector
     0
     ((make-primitive-procedure 'primitive-object-set-type 2)
      ;;(ucode-primitive primitive-object-set-type 2)
      (microcode-type 'manifest-nm-vector)
      ;;(ucode-type manifest-nm-vector)
      (fix:+ n n)))
    (set-interrupt-enables! mask)
    unspecific))

(define (->flonum x)
  (if (flo:flonum? x)
      x
      (let ((x (exact->inexact x)))
	(if (not (flo:flonum? x))
	    (raise-argument-error '->flonum "number, convertible to flonum" x))
	x)))

(define-integrable (guarantee-flonum-vector object procedure)
  (if (not (flo:vector? object))
      (error:wrong-type-argument object "flonum vector" procedure)))

(define-integrable (guarantee-nonnegative-fixnum object procedure)
  (if (not (and (fix:fixnum? object) (fix:>= object 0)))
      (error:wrong-type-argument object "non-negative fixnum" procedure)))

(define-integrable (guarantee-flonum-subvector v s e procedure)
  (guarantee-flonum-vector v procedure)
  (guarantee-nonnegative-fixnum s procedure)
  (guarantee-nonnegative-fixnum e procedure)
  (guarantee-flonum-subvector-range v s e procedure))

(define-integrable (guarantee-flonum-subvector-range v s e procedure)
  (if (not (fix:<= s e))
      (error:bad-range-argument s procedure))
  (if (not (fix:<= e (flo:vector-length v)))
      (error:bad-range-argument e procedure)))