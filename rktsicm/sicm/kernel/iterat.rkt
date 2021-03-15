#lang racket/base

(require racket/fixnum)
(provide (all-defined-out))

;*r* copy/adapted from the scmutils library
;;;; Structure iterators

;;;           Structural Lists

(define generate-list build-list)
(define list:generate build-list)

(define (list-with-substituted-coord lst i x)
  (append (car lst i)
	  (list x)
	  (cdr (cdr lst i))))

;;;           Structural Vectors

(define generate-vector build-vector)

(define ((vector-elementwise f) . vectors)
  (build-vector
    (vector-length (car vectors))
    (lambda (i)
      (apply f
	     (map (lambda (v) (vector-ref v i))
		  vectors)))))

(define (vector-forall p? . vectors)
  (let lp ((i (fx- (vector-length (car vectors)) 1)))
    (cond ((fx= i 0)
	   (apply p? (map (lambda (v) (vector-ref  v i))
			  vectors)))
	  ((apply p? (map (lambda (v) (vector-ref  v i))
			  vectors))
	   (lp (fx- i 1)))
	  (else #f))))

(define (vector-exists p? . vectors)
  (let lp ((i (fx- (vector-length (car vectors)) 1)))
    (cond ((fx= i 0)
	   (apply p? (map (lambda (v) (vector-ref  v i))
			  vectors)))
	  ((apply p? (map (lambda (v) (vector-ref  v i))
			  vectors))
	   #t)
	  (else 
	   (lp (fx- i 1))))))


(define (vector-accumulate acc fun init v)
  (let ((l (vector-length v)))
    (if (fx= l 0)
	init
	(let loop ((i 1)
		   (ans (fun (vector-ref v 0))))
	  (if (fx= i l)
	      ans
	      (loop (fx+ i 1)
		    (acc ans (fun (vector-ref v i)))))))))


(define (vector-with-substituted-coord v i x)
  (build-vector (vector-length v)
    (lambda (j)
      (if (fx= j i)
	  x
	  (vector-ref v j)))))

;;;      Structural 2-dimensional arrays

;;; Structrure procedures -- operate on raw array material

(define (array-ref m i j)
  (vector-ref (vector-ref m i) j))

(define (array-set! m i j v)
  (vector-set! (vector-ref m i) j v))

(define (generate-array rows cols proc)
  (build-vector
   rows
   (lambda (row)
     (build-vector
      cols
      (lambda (col)
	(proc row col))))))
    
(define ((array-elementwise f) . arrays)
  (generate-array
    (num-rows (car arrays))
    (num-cols (car arrays))
    (lambda (i j)
      (apply f
	     (map (lambda (m)
		    (array-ref m i j))
		  arrays)))))

(define (array-copy m)
  (generate-array (num-rows m) (num-cols m)
    (lambda (i j) (array-ref m i j))))


(define (num-rows array)
  (vector-length array))

(define (num-cols array)
  (vector-length (vector-ref array 0)))


(define (nth-row M n) ;return as a vector
  (vector-ref M n))

(define (nth-col M j)
  (generate-vector (num-rows M)
		   (lambda (i)
		     (array-ref M i j))))

(define (array-with-substituted-row A i V)
  (vector-with-substituted-coord A i V))

(define (array-with-substituted-col A k V)
  (generate-array (num-rows A) (num-cols A)
    (lambda (i j)
      (if (fx= j k)
	  (vector-ref V i)
	  (array-ref A i j)))))

(define (array-by-rows M)
  (apply vector (map list->vector M)))

(define (array-by-cols M)
  (transpose-array (array-by-rows M)))

(define (transpose-array M)
  (generate-array (num-cols M) (num-rows M)
    (lambda (i j) (array-ref M j i))))

  