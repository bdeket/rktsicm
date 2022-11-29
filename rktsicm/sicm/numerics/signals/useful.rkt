#lang racket/base

(provide (all-defined-out))

(require (only-in "../../rkt/glue.rkt" if round->exact
                  fix:= fix:-)
         (only-in "../../rkt/define.rkt" define default-object?)
         "../../kernel-intr.rkt"
         "../../general/assert.rkt"
         "fourier.rkt"
         "sigfun.rkt"
         )

(define (unit-step #:optional place)
  (if (default-object? place) (set! place 0))
  (define (step t)
    (if (< t place) 0 1))
  step)

(define (unit-boxcar #:optional half-width place)
  (if (default-object? half-width) (set! half-width 1))
  (if (default-object? place) (set! place 0))
  (define (boxcar t)
    (if (< (abs (- t place)) half-width) 1 0))
  boxcar)

#|
;;; sgn is provided somewhere else.

(define (sgn x)
  (if (negative? x)
      -1
      1))

;;; constant is provided somewhere else.

(define ((constant x) y)
  x)
|#

(define (ramp #:optional slope place)
  (if (default-object? slope) (set! slope 1))
  (if (default-object? place) (set! place 0))
  (define (the-ramp t)
    (* slope (- t place)))
  the-ramp)

;;; peak of height 1 at x1; zero and all derivs zero at x0, x2.

(define (general-bump x0 x1 x2)			
  (let* ((a 1)
	 (b (* (square (/ (- x2 x1) (- x1 x0))) a)))
    (define (g x)
      (exp (- (+ (/ a (- x x0)) (/ b (- x2 x))))))
    (let ((g1 (g x1)))
      (define (h x)
	(cond ((<= x x0) 0)
	      ((<= x2 x) 0)
	      (else (/ (g x) g1)))) 
      h)))

(define (sigfun:unit-impulse span)
  (let* ((minx (sigfun:min span))
	 (maxx (sigfun:max span))
	 (period (- maxx minx))
	 (height (/ *nsamples* period)))
    (sigfun:make (circular-interpolate
		  (generate-list *nsamples*
				 (lambda (i)
				   (let ((x (fix:- i (quotient *nsamples* 2))))
				     (if (fix:= 0 x)
					 height
					 0))))
		  span)
		 span)))

(define (sigfun:impulse-train spacing span)
  (let* ((minx (sigfun:min span))
	 (maxx (sigfun:max span))
	 (period (- maxx minx))
	 (height (/ *nsamples* period))
	 (space (* height spacing))
	 (ispace (round->exact space))
         (ns/2 (quotient *nsamples* 2)))
    (assert (= (- space ispace) 0))
    (sigfun:make (circular-interpolate
		  (generate-list *nsamples*
				 (lambda (i)
				   (let ((x (fix:- i ns/2)))
				     (if (fix:= 0 (modulo x ispace))
					 height
					 0))))
		  span)
		 span)))

;;; To specify impulses of given areas at given values in the span.
;;;  specs is a list of lists, each sublist is a position and an area.

(define (sigfun:specified-impulses specs span)
  (let* ((minx (sigfun:min span))
	 (maxx (sigfun:max span))
	 (period (- maxx minx))
         (samples/period-unit (/ *nsamples* period))
         ;; To get unit area in period units
	 (height samples/period-unit) ; of a unit impulse
         (ns/2 (quotient *nsamples* 2))
         (stuff (make-vector *nsamples* 0)))
    (for-each (lambda (spec)
                (let ((x (car spec)) (area (cadr spec)))
                  ;; x is in period units
                  (let ((x-in-samples
                         (round->exact (* x samples/period-unit))))
                    (vector-set! stuff
                                   (+ ns/2 x-in-samples)
                                   (* area height)))))
              specs)
    (sigfun:make (circular-interpolate (vector->list stuff) span)
		 span))  )

#|
;;; For example, suppose we have the impulse-response of an fir filter
;;; with 64 taps, with a cutoff freq of 4096 Hz and a sampling rate of 
;;; 32768 Hz.  (See file fir-filter.scm)

(define paired-coeffs
  (map list
       (iota 64 -32)    ;To center the response on the origin
       ((hanning-filter 4096 32768) 64)))
#| paired-coeffs |#

(set-nsamples! (expt 2 15))
#| done |#

(set! span (sigfun:make-span (- (expt 2 14)) (expt 2 14)))

(define ir-64
  (sigfun:specified-impulses paired-coeffs span))
#| ir-64 |#

(plot-trace 1 ir-64)
#| (1 (-16384. 16384. -.10851661405696131 .4943079647326846)) |#

(plot-trace 2 (Fourier-transform ir-64))
#| (2 (-.5 .5 -.26901825508249205 3.1825251382784794)) |#
|#

(define (evenly-divides? x y)
  (= (* (round (/ y x)) x) y))