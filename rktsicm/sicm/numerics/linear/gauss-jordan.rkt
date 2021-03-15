#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         racket/vector
         "../../kernel-intr.rkt"
         "singular.rkt"
         )

;;;;  Linear System Solver -- Gauss-Jordan

;;; This file contains the following definitions:
;;;
;;;   (gauss-jordan-solve-linear-system A b) => x-vector
;;;
;;;   (gauss-jordan-invert-and-solve A b succeed fail)
;;;

;;;   (destructive-gauss-jordan-solve-linear-system A b succeed fail)
;;;      succeed = (lambda (x C) (assert A*C=I, A*x=b) ...)
;;;      fail = (lambda (dismiss) ...)

;;; The destructive version works on arrays, not matrices.

;;; Solves an inhomogeneous system of linear equations, A*X=B, returning
;;; the vector X.

(define (gauss-jordan-solve-linear-system A b)
  (let ((A (array-copy (matrix->array A)))	;routine clobbers A and b
	(b (vector-copy b)))
    (destructive-gauss-jordan-solve-linear-system
	   A
	   b
	   (lambda (x Ainv) x)
	   barf-on-zero-pivot)))


(define (gauss-jordan-solve A b succeed fail)
  (let ((A (array-copy (matrix->array A)))	;routine clobbers A and b
	(b (vector-copy b)))
    (destructive-gauss-jordan-solve-linear-system
	   A
	   b
	   (lambda (x Ainv) (succeed x))
	   fail)))

(define (gauss-jordan-invert-and-solve A b succeed fail)
  ;; succeed = (lambda (x C) where x = C*b, C*A = I)
  ;; fail    = (lambda (dismiss) ... )
  (let ((A (array-copy (matrix->array A)))	;routine clobbers A and b
	(b (vector-copy b)))
    (destructive-gauss-jordan-solve-linear-system
	   A
	   b
	   succeed
	   fail)))

(define *minimum-allowable-gj-pivot* 1.0e-30)

;;; Transliterated from Press, Fortran version, p.28., with prejudice.
;;;  Replaces A by A^-1, and b by solution.

(define (destructive-gauss-jordan-solve-linear-system A b succeed fail)
  (let* ((n (num-rows A))
	 (ipiv (make-vector n #f))	;not a legitimate index
	 (indxr (make-vector n 0))
	 (indxc (make-vector n 0)))
    (when (not (fx= n (num-cols A)))
	(error "Non-square matrix -- gj-solve-linear-system" n))
    (when (not (fx= n (vector-length b)))
	(error "Incompatible sizes --  gj-solve-linear-system" n))

    (let iloop ((i 0))			;runs over columns
      (if (fx= i n)
	  'done
	  (let ((big *minimum-allowable-gj-pivot*)
		(irow 0) (icol 0) (pivinv 0))

	    ;; Find the position of the largest (in absolute value) element 
	    ;;  in the matrix that is not in a column from which we
	    ;;  have already picked a pivot.  

	    (let jloop ((j 0))	                        ;runs over rows
	      (if (fx= j n)
		  'done
		  (begin
		    (when (not (vector-ref ipiv j))	;row is free
			(let kloop ((k 0))              ;runs over columns 
			  (if (fx= k n)
			      'done
			      (begin
				(when (not (vector-ref ipiv k))
				    (let ((ajk (magnitude (array-ref A j k))))
				      (when (> ajk big)
					  (begin (set! big ajk)
						 (set! irow j)
						 (set! icol k)))))
				(kloop (fx+ k 1))))))
		    (jloop (fx+ j 1)))))
	    ;(bkpt "gug")
	    (when (= *minimum-allowable-gj-pivot* big) (fail (singular-matrix-error)))
	    (vector-set! ipiv icol #t)
	    ;; Output of jloop (above) is summarized in IROW, ICOL, IPIV
	    ;;(bkpt "pivot found")

	    ;; Pivot element must be on diagonal.
	    ;; The following swaps two rows unless they are already 
	    ;;   the same row.  It will work if the = test is removed.
	    (when (not (fx= irow icol))
		(begin
		  (let lloop ((l 0))
		      (if (fx= l n)
			  'done
			  (let ((dum (array-ref A irow l)))
			    (array-set! A irow l
					(array-ref A icol l))
			    (array-set! A icol l dum)
			    (lloop (fx+ l 1)))))
		  ;;more generally, b can be a matrix
		  ;;if so,replace this loop by one similar to the
		  ;;one above
		  (let ((dum (vector-ref b irow)))
		    (vector-set! b irow (vector-ref b icol))
		    (vector-set! b icol dum))))

	    ;; We remember that we did this swap in information in INDXR and INDXC
	    (vector-set! indxr i irow)
	    (vector-set! indxc i icol)

	    ;;(bkpt "after swap")
	    ;; Scale the icol row by 1/pivot, and set the diag element to 1/pivot.
	    (let ((aii (array-ref A icol icol)))
	      (set! pivinv (invert aii))
	      (array-set! A icol icol 1))
	    (let lloop ((l 0))
	      (if (fx= l n)
		  'done
		  (begin (array-set! A icol l
				     (* (array-ref A icol l) pivinv))
			 (lloop (fx+ l 1)))))
	    ;;more generally, as above....
	    (vector-set! b icol (* (vector-ref b icol) pivinv))

	    ;;for each row, except the pivot row, do row reduction by
	    ;;subtracting the appropriate multiple of the pivot row.
	    (let llloop ((ll 0))
	      (when (fx= ll n)
		  'done
		  (begin
		    (when (not (fx= ll icol))
			(let ((dum (array-ref A ll icol)))
			  (array-set! A ll icol 0)
			  (let lloop ((l 0))
			    (if (fx= l n)
				'done
				(begin
				  (array-set! A ll l
					      (- (array-ref A ll l)
						 (* (array-ref A icol l)
						    dum)))
				  (lloop (fx+ l 1)))))
			  (vector-set! b ll
				       (- (vector-ref b ll)
					  (* (vector-ref b icol)
					     dum)))))
		    (llloop (fx+ ll 1)))))
	    (iloop (fx+ i 1)))))
    ;;end of matrix reduction

    ;;interchange the columns of the matrix, according to the
    ;;permutation specified by INDEXR and INDEXC
    (let lloop ((l (fx- n 1)))
      (if (fx< l 0)
	  'done
	  (let ((kswap (vector-ref indxr l))
		(cswap (vector-ref indxc l)))
	    (when (not (fx= kswap cswap))
		(let kloop ((k 0))
		  (if (fx= k n)
		      'done
		      (let ((dum (array-ref A k kswap)))
			(array-set! A k kswap
				    (array-ref A k cswap))
			(array-set! A k cswap dum)
			(kloop (fx+ k 1))))))
	    (lloop (fx- l 1))))))
  (succeed b A))
