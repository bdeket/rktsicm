#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         racket/flonum
         "../../../rkt/default-object.rkt"
         "flovec.rkt"
         )

;;;; Fast-Fourier Transform

(define (flo:real-fft reals [n default-object] [wn-vectors default-object])
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fxceiling-lg (flo:vector-length reals))
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

(define (flo:real-inverse-fft reals [n default-object] [wn-vectors default-object])
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fxceiling-lg (flo:vector-length reals))
	     n)))
    (let ((reals (flo:vector-grow reals n 0.))
	  (wn-vectors
	   (if (or (default-object? wn-vectors) (not wn-vectors))
	       (compute-wn-vectors n)
	       wn-vectors)))
      (cons reals
	    (flo:real-inverse-fft! reals wn-vectors)))))

(define (flo:real-inverse-fft! reals [wn-vectors default-object])
  (let ((imags (flo:make-vector (flo:vector-length reals) 0.))
	(wn-vectors
	 (if (or (default-object? wn-vectors) (not wn-vectors))
	     (compute-wn-vectors (flo:vector-length reals))
	     wn-vectors)))
    (flo:inverse-fft-reverse! reals)
    (do-butterflies! reals imags (car wn-vectors) (cdr wn-vectors))
    imags))

(define (flo:complex-fft reals imags [n default-object] [wn-vectors default-object])
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fxceiling-lg (flo:vector-length reals))
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

(define (flo:complex-inverse-fft reals imags [n default-object] [wn-vectors default-object])
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fxceiling-lg (flo:vector-length reals))
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

(define (fxceiling-lg n)
  (do ((n* 1 (arithmetic-shift n* 1)))
      ((fx>= n* n) n*)))

(define (flo:bit-reverse-vector! data)
  (let ((n (flo:vector-length data))
	(temp (flo:make-vector 1 0.0)))
    (let ((n/2 (arithmetic-shift n -1))
	  (n-1 (fx- n 1)))
      (do ((i 1 (fx+ i 1))
	   (j n/2
	      (let loop ((j j) (k n/2))
		(if (fx<= k j)
		    (loop (fx- j k) (arithmetic-shift k -1))
		    (fx+ j k)))))
	  ((fx= i n-1))
	(when (fx< i j)
	    (begin
	      (flo:vector-set! temp 0 (flo:vector-ref data j))
	      (flo:vector-set! data j (flo:vector-ref data i))
	      (flo:vector-set! data i (flo:vector-ref temp 0))))))))

(define (flo:inverse-fft-reverse! data)
  (let ((n (flo:vector-length data)))
    (let ((n/2 (arithmetic-shift n -1))
	  (n. (exact->inexact n))
	  (temp (flo:make-vector 1 0.0)))
      (flo:vector-set! data 0 (fl/ (flo:vector-ref data 0) n.))
      (flo:vector-set! data n/2 (fl/ (flo:vector-ref data n/2) n.))
      (do ((i 1 (fx+ i 1)))
	  ((fx= i n/2))
	(flo:vector-set! temp 0 (fl/ (flo:vector-ref data i) n.))
	(flo:vector-set! data i (fl/ (flo:vector-ref data (fx- n i)) n.))
	(flo:vector-set! data (fx- n i) (flo:vector-ref temp 0)))))
  (flo:bit-reverse-vector! data))

(define compute-wn-vectors
  (let ((-2pi (fl* -8. (flatan 1.))))
    (lambda (n)
      (let ((base-angle (fl/ -2pi (exact->inexact n)))
	    (n/2 (arithmetic-shift n -1)))
	(let ((cosines (flo:make-vector n/2 0.0))
	      (sines (flo:make-vector n/2)))
	  (flo:vector-set! cosines 0 1.)
	  (flo:vector-set! sines 0 0.)
	  (do ((i 1 (fx+ i 1))
	       (angle base-angle (fl+ angle base-angle)))
	      ((fx= i n/2))
	    (flo:vector-set! cosines i (flcos angle))
	    (flo:vector-set! sines i (flsin angle)))
	  (cons cosines sines))))))

(define (do-butterflies! reals imags cosines sines)
  (let ((n (flo:vector-length reals))
	(temps (flo:make-vector 4)))
    (do ((le 2 (arithmetic-shift le 1))
	 (le1 1 le)
	 (wn-index-delta (arithmetic-shift n -1) (arithmetic-shift wn-index-delta -1)))
	((fx= wn-index-delta 0))
      (do ((i1 0 (fx+ i1 le)))
	  ((fx= i1 n))
	(let ((i2 (fx+ i1 le1)))
	  (flo:vector-set! temps 2 (flo:vector-ref reals i2))
	  (flo:vector-set! temps 3 (flo:vector-ref imags i2))
	  (flo:vector-set! reals i2
			   (fl- (flo:vector-ref reals i1)
				  (flo:vector-ref temps 2)))
	  (flo:vector-set! imags i2
			   (fl- (flo:vector-ref imags i1)
				  (flo:vector-ref temps 3)))
	  (flo:vector-set! reals i1
			   (fl+ (flo:vector-ref reals i1)
				  (flo:vector-ref temps 2)))
	  (flo:vector-set! imags i1
			   (fl+ (flo:vector-ref imags i1)
				  (flo:vector-ref temps 3)))))
      (do ((j 1 (fx+ j 1))
	   (wn-index wn-index-delta (fx+ wn-index wn-index-delta)))
	  ((fx= j le1))
	(flo:vector-set! temps 0 (flo:vector-ref cosines wn-index))
	(flo:vector-set! temps 1 (flo:vector-ref sines wn-index))
	(do ((i1 j (fx+ i1 le)))
	    ((fx>= i1 n))
	  (let ((i2 (fx+ i1 le1)))
	    (flo:vector-set! temps 2
			     (fl+ (fl* (flo:vector-ref reals i2)
					   (flo:vector-ref temps 0))
				    (fl* (flo:vector-ref imags i2)
					   (flo:vector-ref temps 1))))
	    (flo:vector-set! temps 3
			     (fl- (fl* (flo:vector-ref imags i2)
					   (flo:vector-ref temps 0))
				    (fl* (flo:vector-ref reals i2)
					   (flo:vector-ref temps 1))))
	    (flo:vector-set! reals i2
			     (fl- (flo:vector-ref reals i1)
				    (flo:vector-ref temps 2)))
	    (flo:vector-set! imags i2
			     (fl- (flo:vector-ref imags i1)
				    (flo:vector-ref temps 3)))
	    (flo:vector-set! reals i1
			     (fl+ (flo:vector-ref reals i1)
				    (flo:vector-ref temps 2)))
	    (flo:vector-set! imags i1
			     (fl+ (flo:vector-ref imags i1)
				    (flo:vector-ref temps 3)))))))))

(define (halve-fft-results! results)
  (flo:set-vector-length! (car results)
			  (arithmetic-shift (flo:vector-length (car results)) -1))
  (flo:set-vector-length! (cdr results)
			  (arithmetic-shift (flo:vector-length (cdr results)) -1))
  results)

(define (fft-results->magnitude-squared! results)
  (let ((reals (car results))
	(imags (cdr results)))
    (let ((n (flo:vector-length reals)))
      (do ((i 0 (fx+ i 1)))
	  ((fx= i n))
	(flo:vector-set!
	 reals i
	 (fl+ (fl* (flo:vector-ref reals i)
		       (flo:vector-ref reals i))
		(fl* (flo:vector-ref imags i)
		       (flo:vector-ref imags i))))))
    reals))

(define magnitude-squared->log-magnitude!
  (let ((log-scale-factor (fl/ 10. (fllog 10.))))
    (lambda (reals)
      (let ((n (flo:vector-length reals)))
	(do ((i 0 (fx+ i 1)))
	    ((fx= i n))
	  (flo:vector-set!
	   reals
	   i
	   (fl* (fllog (if (fl< (flo:vector-ref reals i) 1e-100)
			       1e-100
			       (flo:vector-ref reals i)))
		  log-scale-factor))))
      reals)))

(define (fft-results->angle! results)
  (let ((reals (car results))
	(imags (cdr results)))
    (let ((n (flo:vector-length reals)))
      (do ((i 0 (fx+ i 1)))
	  ((fx= i n))
	(flo:vector-set!
	 reals i
	 (if (and (fl= 0.0 (flo:vector-ref reals i))
		  (fl= 0.0 (flo:vector-ref imags i)))
	     0.
	     (exact->inexact (atan (flo:vector-ref imags i)
                                   (flo:vector-ref reals i)))))))
    reals))

(define (fft-results->complex results)
  (let ((reals (car results))
	(imags (cdr results)))
    (let ((n (flo:vector-length reals)))
      (let ((result (make-vector n)))
	(do ((i 0 (fx+ i 1)))
	    ((fx= i n))
	  (vector-set! result
		       i
		       (if (fl= 0.0 (flo:vector-ref imags i))
			   (flo:vector-ref reals i)
			   (make-rectangular (flo:vector-ref reals i)
					     (flo:vector-ref imags i)))))
	result))))