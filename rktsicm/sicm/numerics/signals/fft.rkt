#lang racket/base

(provide (except-out (all-defined-out) ->flonum))

(require racket/fixnum
         racket/vector
         "../../kernel-intr.rkt"
         "cph-dsp/fft.rkt"
         "cph-dsp/flovec.rkt"
         )

(define ->flonum exact->inexact)

;;;;    Discrete Fourier Transform -- GJS -- 8 April 1987

;;; Simple FFT for records whose length is a power of 2.
;;;   Data records are represented as lists of (complex) numbers.
;;;   All arithmetic is assumed to be complex generic, 
;;;   unless explicitly noted by "fx" for indices.

#|
(define (make-transform-pair period)
  (define (fft-internal data ws)
    (define (evens list)
      (if (null? list)
          '()
          (cons (car list) (evens (cddr list)))))
    (define (odds list)
      (if (null? list)
          '()
          (cons (cadr list) (odds (cddr list)))))
    (if (null? (cdr data))
	data
	(let ((ews (if (null? (cdr ws)) ws (evens ws))))
	  (let ((even-fft (fft-internal (evens data) ews))
		(odd-fft (map * ws (fft-internal (odds data) ews))))
	    (append (map + even-fft odd-fft)
		    (map - even-fft odd-fft))))))
  (if (power-of-2? period)
      (let* ((w-list (roots-of-unity period))
	     (w*-list (map conjugate w-list))
	     (fperiod (exact->inexact period)))
	(define (ft data)
	  (if (fx= period (length data))
	      (map (lambda (z) (/ z fperiod))
		   (fft-internal data w*-list))
	      (error "Wrong length data record -- FT" period)))
	(define (ift data)
	  (if (fx= period (length data))
	      (fft-internal data w-list)
	      (error "Wrong length data record -- IFT" period)))
	(list period ft ift))
      (error "Period is not a power of 2 -- MAKE-TRANSFORM-PAIR")))
|#

;;; FFTs like to get power-of-2 data points.

(define (power-of-2? n)
  (and (exact? n) (integer? n) (positive? n)
       (let lp ((n n))
	 (or (fx= n 1)
	     (and (even? n)
		  (lp (quotient n 2)))))))


;;; Roots of unity are needed in FFT programs

(define (roots-of-unity n)
  (let ((n/2 (quotient n 2))
	(2pi/n (/ 2pi (exact->inexact n))))
    (let loop ((k 0))
      (if (fx= k n/2)
	  '()
	  (let ((fk (exact->inexact k)))
	    (cons (make-rectangular (cos (* 2pi/n fk))
				    (sin (* 2pi/n fk)))
		  (loop (fx+ 1 k))))))))


;;; Useful for testing FFT programs

(define (m-cycles-cos-in-n-samples m n)
  (let ((w (/ (* 2pi m) n)))
    (build-list
     n
     (lambda (i)
       (cos (* w i))))))

(define (m-cycles-sin-in-n-samples m n)
  (let ((w (/ (* 2pi m) n)))
    (build-list
     n
     (lambda (i)
       (sin (* w i))))))


;;; FFTs grow as A*n*log_2(n)
;;;  We estimate A with the following code:

(define (constant-of-growth n t)
  (/ t (* n (/ (log n) (log 2)))))


(define (make-transform-pair period [version #t])
  (if version
      (make-transform-pair-CPH period)
      (make-transform-pair-GJS period)))

(define (make-transform-pair-GJS period)
  (define (fft-internal data ws)
    (define (evens list)
      (if (null? list)
          '()
          (cons (car list) (evens (cddr list)))))
    (define (odds list)
      (if (null? list)
          '()
          (cons (cadr list) (odds (cddr list)))))
    (if (null? (cdr data))
	data
	(let ((ews (if (null? (cdr ws)) ws (evens ws))))
	  (let ((even-fft (fft-internal (evens data) ews))
		(odd-fft (map * ws (fft-internal (odds data) ews))))
	    (append (map + even-fft odd-fft)
		    (map - even-fft odd-fft))))))
  (if (power-of-2? period)
      (let* ((w-list (roots-of-unity period))
	     (w*-list (map conjugate w-list))
	     (fperiod (exact->inexact period)))
	(define (ft data)
	  (if (fx= period (length data))
	      (map (lambda (z) (/ z fperiod))
		   (fft-internal data w-list))
	      (error "Wrong length data record -- FT" period)))
	(define (ift data)
	  (if (fx= period (length data))
	      (fft-internal data w*-list)
	      (error "Wrong length data record -- IFT" period)))
	(list period ft ift))
      (error "Period is not a power of 2 -- MAKE-TRANSFORM-PAIR")))

(define (transform-pair->period p)
  (car p))

(define (transform-pair->fft p)
  (cadr p))

(define (transform-pair->ift p)
  (caddr p))

(define (make-transform-pair-CPH period)
  (if (power-of-2? period)
      (let* ((fperiod (exact->inexact period)))
	(define (ftkernel data direction scale)
          (fft-check-data-length data period)
          (let ((reals (flo:make-vector period 0.0))
                (imags (flo:make-vector period 0.0)))
	    (define (complex-result)
	      (vector-map (lambda (z) (* scale z))
			  (fft-results->complex
			   (direction reals imags))))
	    (cond ((vector? data)       ;vector of complex numbers
                   (fft-spread-complex-vector data reals imags period)
                   (complex-result))
                  ((list? data)         ;list of complex numbers
                   (fft-spread-complex-list data reals imags)
                   (vector->list
		    (complex-result))))))	  
        (define (ft data)
          (ftkernel data flo:complex-fft (/ 1 fperiod)))
        (define (ift data)
          (ftkernel data flo:complex-inverse-fft fperiod))
	(list period ft ift))
      (error "Period is not a power of 2 -- MAKE-TRANSFORM-PAIR")))

(define (fft-check-data-length data period)
  (when (not
       (fx= period 
              ((cond ((vector? data) vector-length)
                     ((list? data) length)
                     (else (error "Wrong type data -- FT" data)))
               data)))
      (error "Wrong length data record -- FFT" period)))

(define (fft-spread-complex-vector data reals imags period)
  (do ((i 0 (fx+ i 1))) ((fx= i period))
    (flo:vector-set! reals i
                     (->flonum (real-part (vector-ref data i))))
    (flo:vector-set! imags i
                     (->flonum (imag-part (vector-ref data i))))))

(define (fft-spread-complex-list data reals imags)
  (for-each (lambda (z i)
              (flo:vector-set! reals i
                               (->flonum (real-part z)))
              (flo:vector-set! imags i
                               (->flonum (imag-part z))))
            data (build-list (flo:vector-length reals) values)))

#|
;;; For example, we may:

(define 2pi (* 8 (atan 1 1)))

(define ftsg (make-transform-pair-GJS 16))
(define ftg (cadr ftsg))		; This gets the transform.
(define iftg (caddr ftsg))		; This gets the inverse transform.

(define ftsc (make-transform-pair-CPH 16))
(define ftc (cadr ftsc))		; This gets the transform.
(define iftc (caddr ftsc))		; This gets the inverse transform.

(define sig1 (m-cycles-cos-in-n-samples 2 16))
(define sig2 (m-cycles-sin-in-n-samples 2 16))

(cpp (map heuristic-round-complex sig1))
#|
(1
 .7071067811865476
 0
 -.7071067811865475
 -1.
 -.7071067811865477
 0
 .7071067811865474
 1.
 .7071067811865477
 0
 -.7071067811865467
 -1.
 -.7071067811865471
 0
 .7071067811865466)
|#

(cpp (map heuristic-round-complex sig2))
#|
(0 .7071067811865476
   1
   .7071067811865476
   0
   -.7071067811865476
   -1
   -.7071067811865476
   0
   .7071067811865476
   1
   .7071067811865476
   0
   -.7071067811865476
   -1
   -.7071067811865476)
|#

(cpp (map heuristic-round-complex
	  (ftg sig1)))
#| (0 0 1/2 0 0 0 0 0 0 0 0 0 0 0 1/2 0) |#

(cpp (map heuristic-round-complex
	  (ftg sig2)))
#| (0 0 +1/2i 0 0 0 0 0 0 0 0 0 0 0 -1/2i 0) |#

(cpp (map heuristic-round-complex
	  (ftc sig1)))
#| (0 0 1/2 0 0 0 0 0 0 0 0 0 0 0 1/2 0) |#

(cpp (map heuristic-round-complex
	  (ftc sig2)))
#| (0 0 +1/2i 0 0 0 0 0 0 0 0 0 0 0 -1/2i 0) |#




|#

;;; General tests of transform.

#|
(define (fft-test period)
  (let ((data
	 (build-list period
				(lambda (i)
				  (- (random 2.0) 1.0))))
	(gjs (make-transform-pair-GJS period))
	(cph (make-transform-pair-CPH period)))
    (list (reduce + 0
		  (map square
		       (map -
			    ((cadr gjs) data)
			    ((cadr cph) data))))
	  (reduce + 0
		  (map square
		       (map -
			    ((caddr gjs) data)
			    ((caddr cph) data))))
	  (reduce + 0
		  (map square
		       (map -
			    data
			    ((compose (caddr cph) (cadr cph))
			     data)))))))
#|
(6.819447099176773e-32-1.2583544774140102e-31i
 6.4492460489728735e-28+6.518949305520136e-28i
 -1.844996145931605e-28-6.601586323919541e-30i)
|#
|#