#lang racket/base

(provide (all-defined-out))

(require (only-in "../../../rkt/glue.rkt" if
                  fix:= fix:< fix:<= fix:>= fix:+ fix:- fix:lsh
                  flo:= flo:< flo:+ flo:- flo:* flo:/ flo:cos flo:sin flo:atan flo:atan2 flo:log flo:zero?
                  int:->flonum)
         (only-in "../../../rkt/define.rkt" define default-object?)
         "flovec.rkt" (only-in (submod "flovec.rkt" flo:vector) flo:vector-cons)
         )

;;bdk;; start original file

;;;; Fast-Fourier Transform


(define (flo:real-fft reals #:optional n wn-vectors)
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fix:ceiling-lg (flo:vector-length reals))
	     n)))
    (let ((reals (flo:vector-grow reals n 0.))
	  (wn-vectors
	   (if (or (default-object? wn-vectors) (not wn-vectors))
	       (compute-wn-vectors n)
	       wn-vectors)))
      (cons reals
	    (flo:real-fft! reals (car wn-vectors) (cdr wn-vectors))))))

(define (flo:real-fft! reals cosines sines)
  (let ((imags (flo:make-vector (flo:vector-length reals) 0.)))
    (flo:bit-reverse-vector! reals)
    (do-butterflies! reals imags cosines sines)
    imags))

(define (flo:real-inverse-fft reals #:optional n wn-vectors)
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fix:ceiling-lg (flo:vector-length reals))
	     n)))
    (let ((reals (flo:vector-grow reals n 0.))
	  (wn-vectors
	   (if (or (default-object? wn-vectors) (not wn-vectors))
	       (compute-wn-vectors n)
	       wn-vectors)))
      (cons reals
	    (flo:real-inverse-fft! reals wn-vectors)))))

(define (flo:real-inverse-fft! reals #:optional wn-vectors)
  (let ((imags (flo:make-vector (flo:vector-length reals) 0.))
	(wn-vectors
	 (if (or (default-object? wn-vectors) (not wn-vectors))
	     (compute-wn-vectors (flo:vector-length reals))
	     wn-vectors)))
    (flo:inverse-fft-reverse! reals)
    (do-butterflies! reals imags (car wn-vectors) (cdr wn-vectors))
    imags))

(define (flo:complex-fft reals imags #:optional n wn-vectors)
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fix:ceiling-lg (flo:vector-length reals))
	     n)))
    (let ((reals (flo:vector-grow reals n 0.))
	  (imags (flo:vector-grow imags n 0.))
	  (wn-vectors
	   (if (or (default-object? wn-vectors) (not wn-vectors))
	       (compute-wn-vectors n)
	       wn-vectors)))
      (flo:complex-fft! reals imags (car wn-vectors) (cdr wn-vectors))
      (cons reals imags))))

(define (flo:complex-fft! reals imags cosines sines)
  (flo:bit-reverse-vector! reals)
  (flo:bit-reverse-vector! imags)
  (do-butterflies! reals imags cosines sines))

(define (flo:complex-inverse-fft reals imags #:optional n wn-vectors)
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fix:ceiling-lg (flo:vector-length reals))
	     n)))
    (let ((reals (flo:vector-grow reals n 0.))
	  (imags (flo:vector-grow imags n 0.))
	  (wn-vectors
	   (if (or (default-object? wn-vectors) (not wn-vectors))
	       (compute-wn-vectors n)
	       wn-vectors)))
      (flo:complex-inverse-fft! reals imags (car wn-vectors) (cdr wn-vectors))
      (cons reals imags))))

(define (flo:complex-inverse-fft! reals imags cosines sines)
  (flo:inverse-fft-reverse! reals)
  (flo:inverse-fft-reverse! imags)
  (do-butterflies! reals imags cosines sines))

(define (fix:ceiling-lg n)
  (do ((n* 1 (fix:lsh n* 1)))
      ((fix:>= n* n) n*)))

(define (flo:bit-reverse-vector! data)
  (let ((n (flo:vector-length data))
	(temp (flo:vector-cons 1)))
    (let ((n/2 (fix:lsh n -1))
	  (n-1 (fix:- n 1)))
      (do ((i 1 (fix:+ i 1))
	   (j n/2
	      (let loop ((j j) (k n/2))
		(if (fix:<= k j)
		    (loop (fix:- j k) (fix:lsh k -1))
		    (fix:+ j k)))))
	  ((fix:= i n-1))
	(if (fix:< i j)
	    (begin
	      (flo:vector-set! temp 0 (flo:vector-ref data j))
	      (flo:vector-set! data j (flo:vector-ref data i))
	      (flo:vector-set! data i (flo:vector-ref temp 0))))))))

(define (flo:inverse-fft-reverse! data)
  (let ((n (flo:vector-length data)))
    (let ((n/2 (fix:lsh n -1))
	  (n. (int:->flonum n))
	  (temp (flo:vector-cons 1)))
      (flo:vector-set! data 0 (flo:/ (flo:vector-ref data 0) n.))
      (flo:vector-set! data n/2 (flo:/ (flo:vector-ref data n/2) n.))
      (do ((i 1 (fix:+ i 1)))
	  ((fix:= i n/2))
	(flo:vector-set! temp 0 (flo:/ (flo:vector-ref data i) n.))
	(flo:vector-set! data i (flo:/ (flo:vector-ref data (fix:- n i)) n.))
	(flo:vector-set! data (fix:- n i) (flo:vector-ref temp 0)))))
  (flo:bit-reverse-vector! data))

(define compute-wn-vectors
  (let ((-2pi (flo:* -8. (flo:atan2 1. 1.))))
    (lambda (n)
      (let ((base-angle (flo:/ -2pi (int:->flonum n)))
	    (n/2 (fix:lsh n -1)))
	(let ((cosines (flo:vector-cons n/2))
	      (sines (flo:vector-cons n/2)))
	  (flo:vector-set! cosines 0 1.)
	  (flo:vector-set! sines 0 0.)
	  (do ((i 1 (fix:+ i 1))
	       (angle base-angle (flo:+ angle base-angle)))
	      ((fix:= i n/2))
	    (flo:vector-set! cosines i (flo:cos angle))
	    (flo:vector-set! sines i (flo:sin angle)))
	  (cons cosines sines))))))

(define (do-butterflies! reals imags cosines sines)
  (let ((n (flo:vector-length reals))
	(temps (flo:vector-cons 4)))
    (do ((le 2 (fix:lsh le 1))
	 (le1 1 le)
	 (wn-index-delta (fix:lsh n -1) (fix:lsh wn-index-delta -1)))
	((fix:= wn-index-delta 0))
      (do ((i1 0 (fix:+ i1 le)))
	  ((fix:= i1 n))
	(let ((i2 (fix:+ i1 le1)))
	  (flo:vector-set! temps 2 (flo:vector-ref reals i2))
	  (flo:vector-set! temps 3 (flo:vector-ref imags i2))
	  (flo:vector-set! reals i2
			   (flo:- (flo:vector-ref reals i1)
				  (flo:vector-ref temps 2)))
	  (flo:vector-set! imags i2
			   (flo:- (flo:vector-ref imags i1)
				  (flo:vector-ref temps 3)))
	  (flo:vector-set! reals i1
			   (flo:+ (flo:vector-ref reals i1)
				  (flo:vector-ref temps 2)))
	  (flo:vector-set! imags i1
			   (flo:+ (flo:vector-ref imags i1)
				  (flo:vector-ref temps 3)))))
      (do ((j 1 (fix:+ j 1))
	   (wn-index wn-index-delta (fix:+ wn-index wn-index-delta)))
	  ((fix:= j le1))
	(flo:vector-set! temps 0 (flo:vector-ref cosines wn-index))
	(flo:vector-set! temps 1 (flo:vector-ref sines wn-index))
	(do ((i1 j (fix:+ i1 le)))
	    ((fix:>= i1 n))
	  (let ((i2 (fix:+ i1 le1)))
	    (flo:vector-set! temps 2
			     (flo:+ (flo:* (flo:vector-ref reals i2)
					   (flo:vector-ref temps 0))
				    (flo:* (flo:vector-ref imags i2)
					   (flo:vector-ref temps 1))))
	    (flo:vector-set! temps 3
			     (flo:- (flo:* (flo:vector-ref imags i2)
					   (flo:vector-ref temps 0))
				    (flo:* (flo:vector-ref reals i2)
					   (flo:vector-ref temps 1))))
	    (flo:vector-set! reals i2
			     (flo:- (flo:vector-ref reals i1)
				    (flo:vector-ref temps 2)))
	    (flo:vector-set! imags i2
			     (flo:- (flo:vector-ref imags i1)
				    (flo:vector-ref temps 3)))
	    (flo:vector-set! reals i1
			     (flo:+ (flo:vector-ref reals i1)
				    (flo:vector-ref temps 2)))
	    (flo:vector-set! imags i1
			     (flo:+ (flo:vector-ref imags i1)
				    (flo:vector-ref temps 3)))))))))

(define (halve-fft-results! results)
  (flo:set-vector-length! (car results)
			  (fix:lsh (flo:vector-length (car results)) -1))
  (flo:set-vector-length! (cdr results)
			  (fix:lsh (flo:vector-length (cdr results)) -1))
  results)

(define (fft-results->magnitude-squared! results)
  (let ((reals (car results))
	(imags (cdr results)))
    (let ((n (flo:vector-length reals)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(flo:vector-set!
	 reals i
	 (flo:+ (flo:* (flo:vector-ref reals i)
		       (flo:vector-ref reals i))
		(flo:* (flo:vector-ref imags i)
		       (flo:vector-ref imags i))))))
    reals))

(define magnitude-squared->log-magnitude!
  (let ((log-scale-factor (flo:/ 10. (flo:log 10.))))
    (lambda (reals)
      (let ((n (flo:vector-length reals)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (flo:vector-set!
	   reals
	   i
	   (flo:* (flo:log (if (flo:< (flo:vector-ref reals i) 1e-100)
			       1e-100
			       (flo:vector-ref reals i)))
		  log-scale-factor))))
      reals)))

(define (fft-results->angle! results)
  (let ((reals (car results))
	(imags (cdr results)))
    (let ((n (flo:vector-length reals)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(flo:vector-set!
	 reals i
	 (if (and (flo:zero? (flo:vector-ref reals i))
		  (flo:zero? (flo:vector-ref imags i)))
	     0.
	     (flo:atan2 (flo:vector-ref imags i)
                        (flo:vector-ref reals i))))))
    reals))

(define (fft-results->complex results)
  (let ((reals (car results))
	(imags (cdr results)))
    (let ((n (flo:vector-length reals)))
      (let ((result (make-vector n)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (vector-set! result
		       i
		       (if (flo:zero? (flo:vector-ref imags i))
			   (flo:vector-ref reals i)
			   (make-rectangular (flo:vector-ref reals i)
					     (flo:vector-ref imags i)))))
	result))))