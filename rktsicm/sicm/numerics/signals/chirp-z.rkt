#lang racket/base

(provide (all-defined-out))

(require "../../kernel-intr.rkt"
         "fft.rkt"
         )

;;;; Chirp z-transform
;;;   from  Rabiner, Schaffer, Rader, BSTJ, May-June 1969

;;; Gives the z-transform of N samples of data (given as a list) 
;;;   at M points in the z-plane, starting at z=A.  
;;;   Subsequent points are at z=A*W^{-k}, for k=0,1,...,M-1.

;;; The chirp z-transform can be used to develop a segment of the
;;;   spectrum expanded from frequencies f0 to f1 in M steps.

(define (Fourier-expanded M f0 f1 data)
  (let ((A (exp (* 2pi +i f0)))
	(W (exp (* 2pi -i (/ (- f1 f0) M)))))
    (chirp-z M A W data)))

;;; Coded by JW in April 2017.
;;; Minor modification by GJS to remove n^2 list references.

(define (chirp-z M A W data)
  (let ((N (length data)))
    (let ((L
	   (inexact->exact (expt 2
				 (ceiling (/ (log (+ N M -1))
					     (log 2))))))
          (data (list->vector data)))
      (let ((yn
             (generate-list L
               (lambda (n)
                 (if (< n N)
                     (* (expt A (- n))
                        (expt W (/ (* n n) 2))
                        (vector-ref data n))
                     0))))
            (vn
             (generate-list L
               (lambda (n) 
                 (cond ((< n M) 
                        (/ 1 (expt W (/ (* n n) 2))))
                       ((and (<= (+ (- L N) 1) n) (< n L))
                        (/ 1 (expt W (/ (* (- L n) (- L n)) 2))))
                       (else 
                        0))))))
        (let ((fft-transform-pair (make-transform-pair L)))
          (let ((Yr ((transform-pair->fft fft-transform-pair) yn))
                (Vr ((transform-pair->fft fft-transform-pair) vn)))
            (let ((Wr (map * Yr Vr)))
              (let ((gk
                     (list->vector
                      ((transform-pair->ift fft-transform-pair) Wr))))
                (let ((Xk
                       (generate-list M
                         (lambda (k)
                           (* (expt W (/ (* k k) 2))
                              (vector-ref gk k))))))
		  Xk)))))))))

(define (Hanning data)			;data is a list of numbers
  (let ((N (length data)))
    (let lp ((data data) (i 0) (ans '()))
      (if (< i N)
	  (lp (cdr data)
	      (+ i 1)
	      (cons (* (car data)
		       (square (sin (/ (* :pi i)
				       (- N 1)))))
		    ans))
	  (reverse ans)))))

#|
;;; Demonstration of use of Fourier-expanded 
;;; and Hanning window.

(define win (frame 0 1024 -2.0 +2.0 1000 200 1500 100))
(rename-window win "Data: [0,1024]; [-2,2]")

(define win1 (frame 0 1024 0 +1.0 1000 200 1500 350))
(rename-window win1 "DFT: [0,1024]; [0,1]")

(define win2 (frame 0 1024 0 +0.5 1000 200 1500 600))
(rename-window win2 "DFT with Hanning: [0,1024]; [0,0.5]")

(define win3 (frame 0 1000 0 +0.5 1000 200 1500 850))
(rename-window win3 "Fourier expanded: [0,1000]; [0,0.5]")

(define win4 (frame 0 1000 0 0.25 1000 200 1500 1100))
(rename-window win4 "Fourier expanded with Hanning: [0,1000]; [0,0.25]")

(define (clear)
  (graphics-clear win)
  (graphics-clear win1)
  (graphics-clear win3)
  (graphics-clear win2)
  (graphics-clear win4))

(define (close)
  (graphics-close win)
  (graphics-close win1)
  (graphics-close win3)
  (graphics-close win2)
  (graphics-close win4))

(define (plot-data win data)
  (let ((N (length data)))
    (for-each (lambda (n)
		(plot-point win n (ref data n)))
	      (iota N))))

(define data
  (generate-list 1024
		 (lambda (n)
		   (+ (cos (* 8  (/ n 1024) 2pi))
		      (sin (* 12 (/ n 1024) 2pi))))))

(plot-data win data)

(define bar (map magnitude (dft data)))

(plot-data win1 bar)
;;; We see the positive and negative frequency
;;; components displayed and separated.  
;;; Note that the magnitudes of the lines are 0.5.


(define goosh
  (map magnitude
       (dft (Hanning data))))

(plot-data win2 goosh)
;;; The Hanning window broadens the spectral lines, 
;;; as expected (it is an apodized aperture).  It 
;;; halves the peak magnitudes of the lines.


(define foosh
  (map magnitude
       (Fourier-expanded 1000
			 (/ 5 1000)
			 (/ 15 1000)
			 data)))

(plot-data win3 foosh)
;;; The spectrum is expanded to look at the details
;;; of the spectral lines.  The chirp-z also cuts
;;; the magnitude by a factor of 2.  Why?


(define boosh
  (map magnitude
       (Fourier-expanded 1000
			 (/ 5 1000)
			 (/ 15 1000)
			 (Hanning data))))

(plot-data win4 boosh)
;;; The Hanning window suppresses the sidelobes, 
;;; but it decreases the resolution, as expected.
|#

#|
;;; From JW.

(define (apply-hanning data)
  (let ((N (length data)))
    (let lp ((data data) (i 0) (ans '()))
      (if (< i N)
	  (let ((d (car data)))
	    (lp (cdr data)
		(+ i 1)
		(cons (list (car d)
			    (* (cadr d)
			       (square (sin (/ (* pi i)
					       (- N 1))))))
		      ans)))
	  (reverse ans)))))

(define (apply-hanning-0 data)
  (let ((N (length data)))
    (let lp ((data data) (i 0) (ans '()))
      (if (< i N)
	  (let ((d (car data)))
	    (lp (cdr data)
		(+ i 1)
		(cons (* d
			 (square (sin (/ (* pi i)
					 (- N 1)))))
		      ans)))
	  (reverse ans)))))
|#

				   
