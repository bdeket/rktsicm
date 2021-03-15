#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         racket/flonum
         "flovec.rkt"
         (only-in "../../../rkt/todo.rkt" todos)
         )

(todos todo
       [#:from "???"
        pathname-default-type
        scode-eval
        system-global-syntax-table
        system-global-environment
        compile-scode
        syntax&integrate])

;;;; FIR Filter Constructors

(define (flo:apply-filter-hn input-data hn)
  (flo:apply-filter input-data
		    (direct-form hn)
		    (fx- (flo:vector-length hn) 1)))

(define (flo:apply-filter input-data filter overlap)
  (let ((length (flo:vector-length input-data)))
    (let ((buffer (flo:make-vector (fx+ length overlap)))
	  (result (flo:make-vector length)))
      (flo:subvector-fill! buffer 0 overlap 0.)
      (do ((i 0 (fx+ i 1)))
	  ((fx= i length))
	(flo:vector-set! buffer (fx+ overlap i)
			 (flo:vector-ref input-data i)))
      (do ((i 0 (fx+ i 1)))
	  ((fx= i length))
	(filter buffer (fx+ overlap i) result i))
      result)))

(define (direct-form hn)
  (let ((m (- (flo:vector-length hn) 1)))
    (if (even? m)
	(direct-form-even hn m)
	(direct-form-odd hn m))))

(define (direct-form-even hn m)
  (let ((m/2 (quotient m 2)))
    (lambda (input input-index output output-index)
      (let ((k (fx- input-index m)))
	(flo:vector-set! output
			 output-index
			 (fl* (flo:vector-ref hn 0)
				(fl+ (flo:vector-ref input input-index)
				       (flo:vector-ref input k))))
	(do ((i 1 (fx+ i 1)))
	    ((fx= i m/2)
	     (flo:vector-set!
	      output
	      output-index
	      (fl+ (flo:vector-ref output output-index)
		     (fl* (flo:vector-ref hn i)
			    (flo:vector-ref input (fx- input-index i))))))
	  (flo:vector-set!
	   output
	   output-index
	   (fl+ (flo:vector-ref output output-index)
		  (fl* (flo:vector-ref hn i)
			 (fl+ (flo:vector-ref input (fx- input-index i))
				(flo:vector-ref input (fx+ k i)))))))))))

(define (direct-form-odd hn m)
  (let ((m+1/2 (quotient (+ m 1) 2)))
    (lambda (input input-index output output-index)
      (let ((k (fx- input-index m)))
	(flo:vector-set! output
			 output-index
			 (fl* (flo:vector-ref hn 0)
				(fl+ (flo:vector-ref input input-index)
				       (flo:vector-ref input k))))
	(do ((i 1 (fx+ i 1)))
	    ((fx= i m+1/2))
	  (flo:vector-set!
	   output
	   output-index
	   (fl+ (flo:vector-ref output output-index)
		  (fl* (flo:vector-ref hn i)
			 (fl+ (flo:vector-ref input (fx- input-index i))
				(flo:vector-ref input (fx+ k i)))))))
	(void)))))

(define (direct-form-file hn filename)
  (call-with-output-file (pathname-default-type filename "scm")
    (lambda (port)
      (write '(DECLARE (USUAL-INTEGRATIONS)) port)
      (newline port)
      (write (direct-form-expression hn) port))))

(define (direct-form-procedure hn)
  (scode-eval (compile-expression (direct-form-expression hn)
				  '((USUAL-INTEGRATIONS))
				  system-global-syntax-table)
	      system-global-environment))

(define (compile-expression s-expression declarations syntax-table)
  (compile-scode (syntax&integrate s-expression declarations syntax-table)))

(define (direct-form-expression hn)
  (let ((m (- (flo:vector-length hn) 1)))
    (if (even? m)
	(direct-form-even-expression hn m)
	(direct-form-odd-expression hn m))))

(define (direct-form-even-expression hn m)
  (let ((apply-hn
	 (lambda (j x)
	   (let ((h (flo:vector-ref hn j)))
	     (if (= h 0)
		 0.
		 `(fl* ,h ,x)))))
	(accumulate
	 (lambda (term accum)
	   (if (eqv? term 0.)
	       accum
	       `(fl+ ,term ,accum))))
	(input-ref
	 (lambda (offset)
	   `(FLO:VECTOR-REF INPUT
			    ,(if (= offset 0)
				 'INPUT-INDEX
				 `(FIX:- INPUT-INDEX ,offset))))))
    (let ((m/2 (quotient m 2)))
      `(LAMBDA (INPUT INPUT-INDEX OUTPUT OUTPUT-INDEX)
	 (FLO:VECTOR-SET!
	  OUTPUT
	  OUTPUT-INDEX
	  ,(let loop
	       ((j 1)
		(accum (apply-hn 0 `(fl+ ,(input-ref 0) ,(input-ref m)))))
	     (if (fx= j m/2)
		 (accumulate (apply-hn j (input-ref j)) accum)
		 (loop (fx+ j 1)
		       (accumulate
			(apply-hn j
				  `(fl+ ,(input-ref j) ,(input-ref (- m j))))
			accum)))))))))

(define (direct-form-odd-expression hn m)
  (let ((apply-hn
	 (lambda (j x)
	   (let ((h (flo:vector-ref hn j)))
	     (if (= h 0)
		 0.
		 `(fl* ,h ,x)))))
	(accumulate
	 (lambda (term accum)
	   (if (eqv? term 0.)
	       accum
	       `(fl+ ,term ,accum))))
	(input-ref
	 (lambda (offset)
	   `(FLO:VECTOR-REF INPUT
			    ,(if (= offset 0)
				 'INPUT-INDEX
				 `(FIX:- INPUT-INDEX ,offset))))))
    (let ((m+1/2 (quotient (+ m 1) 2)))
      `(LAMBDA (INPUT INPUT-INDEX OUTPUT OUTPUT-INDEX)
	 (FLO:VECTOR-SET!
	  OUTPUT
	  OUTPUT-INDEX
	  ,(let loop
	       ((j 1)
		(accum (apply-hn 0 `(fl+ ,(input-ref 0) ,(input-ref m)))))
	     (if (fx= j m+1/2)
		 accum
		 (loop (fx+ j 1)
		       (accumulate
			(apply-hn j
				  `(fl+ ,(input-ref j) ,(input-ref (- m j))))
			accum)))))))))