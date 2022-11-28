#lang racket/base

(provide (all-defined-out))

(require (only-in "../../rkt/glue.rkt" if make-initialized-vector)
         (only-in "../../rkt/define.rkt" define default-object?)
         "../../kernel-intr.rkt"
         "svd.rkt"
         )

;;;; Find a least squares solution to Ax=b using SVD.

;;; svd-least-squares finds the least-squares best value for the
;;;   vector x of unknowns satisfying the equations Ax=b.
;;; There are two optional arguments.
;;;   eps is for error control.
;;;     if eps is a number and eps < 1
;;;        then any singular value w_i < eps*max(w) is discarded
;;;     if eps is an integer number and eps > 1 then eps is the
;;;        maximum number of singular values to be used.
;;;     if eps is a procedure,
;;;        then any singular value w_i < eps(w)*max(w) is discarded
;;;     a singular value is discarded by setting it to zero.
;;;   db is a vector of scale factors for each variable.
;;;      Assume the estimated errors of b are db.
;;; svd-least-squares returns a list of (x chi^2 dx), the
;;;  solved-for unknowns, the overall error, and the standard error
;;;  for each unknown.

(define (svd-least-squares A b #:optional eps db)
  (if (default-object? eps) (set! eps 1e-15))
  (let ((bp (if (default-object? db)
		b
		((vector-elementwise /) b db)))
	(Ap (if (default-object? db)
		A
		(m:generate (m:num-rows A) (m:num-cols A)
			    (lambda (i j)
                              (/ (m:ref A i j)
                                 (vector-ref db i)))))))
    (svd Ap
     (lambda (u w-mat v w)
       (let* ((n (vector-length w))
              (inverted-w
               (if (and (number? eps) (integer? eps) (>= eps 1))
                   (make-initialized-vector n
                     (lambda (i) 
                       (let ((wi (vector-ref w i)))
                         (if (< i eps) (/ 1 wi) 0))))
                   (let ((wmin
                          (cond ((number? eps)
                                 (* eps
				    (apply max (vector->list w))))
                                ((procedure? eps)
                                 (* (eps w)
				    (apply max (vector->list w))))
                                (else
                                 (error "Bad cutoff -- SVD" eps)))))
                     (make-initialized-vector n
                       (lambda (i) 
                         (let ((wi (vector-ref w i)))
                           (if (< wi wmin) 0 (/ 1 wi)))))))))
	 ;; Continued

	 ;; Continuation
	 (let ((numw
                (let lp ((i 0))
                  (cond ((= i n) n)
                        ((= (vector-ref w i) 0) i)
                        (else (lp (+ i 1))))))
               (x                       ; The unknown vector
                (matrix*vector v
                               ((vector-elementwise *)
                                (matrix*vector (m:transpose u) bp)
                                inverted-w))))
  
           (let ((chi2                  ; Overall error measure
                  (v:square (vector-vector (matrix*vector A x) b)))
                 (dx^2 
                  (g:sigma (lambda (i) 
                             (scalar*vector (vector-ref inverted-w i)
                                            ((vector-elementwise *)
                                             (m:nth-col v i)
                                             (m:nth-col v i))))
                         0 
                         (- numw 1))))
             (let ((dx                  ; The covariances
                    ((vector-elementwise sqrt) dx^2)))
               (list x dx chi2)))))))))

(define svd-solve-linear-system
  (compose car svd-least-squares))

;;; Tests
#|
(define ((f A B) x)
  (+ (* A (cos x)) (* B (* 2 (sin x)))))

(define (make-random-data A B sigma xs)
  (list->vector
   (map (lambda (x)
	  (+ ((f A B) x) (* sigma (gaussian-random))))
	xs)))

(define (make-design xs)
  (apply matrix-by-rows
	 (map (lambda (x) (list (cos x) (* 2 (sin x)))) xs)))

(define (xs n)
  (map (lambda (i) (* 2pi (/ i n))) (iota n)))

(define (errors sigma var n)
  (v:generate n (lambda (i) (+ sigma (* var (gaussian-random))))))

(define test-xs (xs 100))
(define test-design (make-design test-xs))
(define test-b (make-random-data 1 10 .1 test-xs))

(svd-least-squares test-design test-b 2)
#|
((up 1.0181940511405976 9.992722481700229)
 (up .37606030930863943 .26591479484724945)
 1.1211970577354886)
|#

(svd-least-squares test-design test-b 2 (errors .1 .01 100))
#|
((up 1.0156989475194924 9.987917234874896)
 (up .11796687913681762 .0840107105487443)
 1.1261264142499618)
|#

|#

