#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         "../../general/assert.rkt"
         "../iterat.rkt"
         "../types.rkt"
         )

(define (m:type m) matrix-type-tag)
(define (m:type-predicate m) matrix-quantity?)

;;; The matrix type is a structural type, providing a means of
;;; combining other objects.  In this system an explicit matrix is a
;;; tagged array.  An array is represented as a scheme vector of rows,
;;; where each row is a scheme vector of elements.

(define (tag-matrix nrows ncols array)
  (list matrix-type-tag (cons nrows ncols) array))

(define (m:num-rows matrix)
  (caadr matrix))

(define (m:num-cols matrix)
  (cdadr matrix))

(define (matrix->array matrix)
  (caddr matrix))

(define (m:dimension mat)
  (assert (matrix? mat) "Not a matrix -- DIMENSION" mat)
  (let ((d (m:num-rows mat)))
    (assert (fx= d (m:num-cols mat))
	    "Not a square matrix -- DIMENSION" mat)
    d))

(define (matrix-size mat)
  (assert (matrix? mat) "Not a matrix -- SIZE" mat)
  (fx* (m:num-rows mat) (m:num-cols mat)))


;;; Single columns or rows are often important.

(define (column-matrix? m)
  (and (matrix? m)
       (fx= (m:num-cols m) 1)))

(define (row-matrix? m)
  (and (matrix? m)
       (fx= (m:num-rows m) 1)))

(define (matrix-ref m i j)
  (vector-ref (vector-ref (matrix->array m) i) j))

(define m:ref matrix-ref)

;;; Submatrices are often used -- here we extract one

(define (m:generate nrows ncols proc)
  (tag-matrix nrows ncols (generate-array nrows ncols proc)))

(define (m:submatrix A lowrow hirow+1 lowcol hicol+1)
  (m:generate (fx- hirow+1 lowrow) (fx- hicol+1 lowcol)
    (lambda (i j)
      (matrix-ref A (fx+ i lowrow) (fx+ j lowcol)))))
