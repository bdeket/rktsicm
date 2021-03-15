#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         "../../kernel-intr.rkt"
         "singular.rkt"
         )

;;;;  Linear System Solver -- Full-Pivot

;;; 13 August 1990 -- Full pivoting linear-equation solver -- GJS & Jacob

;;; This file contains the following definitions:
;;;
;;;   (full-pivot-solve-linear-system a-matrix b-vector) => x-vector
;;;
;;;   (full-pivot-solve a-matrix b-vector succeed fail)
;;;      succeed = (lambda (x) (assert A*x=b) ...)
;;;      fail = (lambda (dismiss) ...)

;;; There is also a version that works on arrays, rather than matrices
;;;   (full-pivot-solve-internal A b succeed fail)

(define (full-pivot-solve-linear-system A b)
  (full-pivot-solve-internal (matrix->array A) b (lambda (x) x) barf-on-zero-pivot))

(define (full-pivot-solve A b succeed fail)
  (full-pivot-solve-internal (matrix->array A) b succeed fail))

(define (full-pivot-solve-internal A b succeed fail)
  ;; succeed = (lambda (x) ... (assert A*x=b))
  ;; fail    = (lambda (dismiss) ... )
  (let ((n (num-rows A)))
    (if (fx< n 2)
	(let ((a00 (array-ref A 0 0)))
	  (if (< (magnitude a00) *minimum-allowable-full-pivot*)
	      (fail (singular-matrix-error))
	      (succeed (vector (/ (vector-ref b 0) a00)))))
	(full-pivot-find A n
		    (lambda (p ip jp)	; pivot p is in row ip, column jp
		      (let ((nm1 (fx- n 1))
			    (pivot-row (nth-row A ip)))
			(let ((scaled-pivot-row
			       (build-vector nm1
				 (lambda (k)
				   (if (fx< k jp)
				       (/ (vector-ref pivot-row k) p)
				       (/ (vector-ref pivot-row (fx+ k 1)) p)))))
			      (pivot-column
			       (build-vector nm1
				 (lambda (k)
				   (if (fx< k ip)
				       (array-ref A k jp)
				       (array-ref A (fx+ k 1) jp)))))
			      (bp (/ (vector-ref b ip) p)))
			  (full-pivot-solve-internal
			   (generate-array nm1 nm1
			     (lambda (i j)
			       (let ((c (* (vector-ref pivot-column i)
					   (vector-ref scaled-pivot-row j))))
				 (if (fx< i ip)
				     (if (fx< j jp)
					 (- (array-ref A i j) c)
					 (- (array-ref A i (fx+ j 1)) c))
				     (if (fx< j jp)
					 (- (array-ref A (fx+ i 1) j) c)
					 (- (array-ref A (fx+ i 1) (fx+ j 1)) c))))))
			   (build-vector nm1
			     (lambda (i)
			       (let ((c (* bp (vector-ref pivot-column i))))
				 (if (fx< i ip)
				     (- (vector-ref b i) c)
				     (- (vector-ref b (fx+ i 1)) c)))))
			   ;;Continuation of full-pivot-solve-internal
			   (lambda (x)
			     (let ((xip 
				    (let lp ((k 0) (sum 0))
				      (if (fx= k nm1)
					  (- bp sum)
					  (lp (fx+ k 1)
					      (+ sum
						 (* (vector-ref x k)
						    (vector-ref scaled-pivot-row k))))))))
			       (succeed
				(build-vector n
				  (lambda (i)
				    (cond ((fx< i jp) (vector-ref x i))
					  ((fx= i jp) xip)
					  ((fx> i jp) (vector-ref x (fx- i 1)))))))))
			   fail))))
		    fail))))
		
(define (full-pivot-find A n found-pivot fail)
  ;; found  = (lambda (pivot ip jp) ... )
  ;; fail   = (lambda (dismiss) ... )
  (let row-loop ((i 0) (maxabs -1) (maxpiv -1) (imax -1) (jmax -1))
    (if (fx= i n)
	(if (< maxabs *minimum-allowable-full-pivot*)
	    (fail (singular-matrix-error))
	    (found-pivot maxpiv imax jmax))
	(let col-loop ((j 0) (maxabs maxabs) (maxpiv maxpiv) (imax imax) (jmax jmax))
	  (if (fx= j n)
	      (row-loop (fx+ i 1) maxabs maxpiv imax jmax)
	      (let* ((newel (array-ref A i j))
		     (newabs (magnitude newel)))
		(if (> newabs maxabs)
		    (col-loop (fx+ j 1) newabs newel i j)
		    (col-loop (fx+ j 1) maxabs maxpiv imax jmax))))))))

(define *minimum-allowable-full-pivot* 1.0e-30)


