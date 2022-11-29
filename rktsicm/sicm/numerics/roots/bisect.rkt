#lang racket/base

(provide (all-defined-out))

(require (only-in "../../rkt/glue.rkt" if fix:+ write-line)
         (only-in "../../rkt/define.rkt" define default-object?)
         "../../kernel-intr.rkt"
         )

;;;; Root finding by successive bisection

;;; Simple bisection search 
;;;   In IEEE 754 binary floating point I think this will always
;;;   converge to full precision, but I have not proved it.  GJS

(define (bisect-1 f x0 x1)
  (let ((fx0 (f x0)) (fx1 (f x1)))
    (if (> (* fx0 fx1) 0.0)
      (error "root not bounded" x0 x1 fx0 fx1))
    (let loop ((x0 x0) (fx0 fx0) (x1 x1) (fx1 fx1))
      (cond ((= fx0 0.0) x0)
	    ((= fx1 0.0) x1)
	    (else
	     (let ((xm (/ (+ x0 x1) 2.0)))
	       (cond ((= x0 xm) x0)
		     ((= x1 xm) x1)
		     (else
		      (let ((fxm (f xm)))
			(if (< (* fx1 fxm) 0.0)
			    (loop xm fxm x1 fx1)
			    (loop x0 fx0 xm fxm)))))))))))
#|
;;; for example, this took 51 evaluations of f.

(define (kepler ecc m)
  (bisect-1
   (lambda (e)
     (write-line e)
     (- e (* ecc (sin e)) m))
   0.0
   2pi))

(kepler .99 .01)
6.283185307179586
0.
3.141592653589793
1.5707963267948966
...
.34227031649176376
.3422703164917861
.3422703164917749
#| .3422703164917749 |#

|#

;;; Simple bisection search 
;;;  terminating when x0 close to x1.

(define (bisect-2 f x0 x1 eps)
  (let loop ((x0 x0) (fx0 (f x0)) (x1 x1) (fx1 (f x1)))
    (if (= fx0 0.0) 
	x0
	(if (= fx1 0.0)
	    x1
	    (if (> (* fx1 fx0) 0.0)
		(error "root not bounded")
		(let ((xm (/ (+ x0 x1) 2.0)))
		  (if (close-enuf? x0 x1 eps)
		      xm
		      (let ((fxm (f xm)))
			(if (< (* fx1 fxm) 0.0)
			    (loop xm fxm x1 fx1)
			    (loop x0 fx0 xm fxm))))))))))
#|
;;; for example

(define (kepler ecc m)
  (bisect-2
   (lambda (e)
     (write-line e)
     (- e (* ecc (sin e)) m))
   0.0
   2pi
   1e-15))

(kepler .99 .01)
6.283185307179586
0.
3.141592653589793
1.5707963267948966
.7853981633974483
;;; Total of 51 lines here
.34227031649171913
.34227031649176376
.3422703164917861
.3422703164917749
;Value: .3422703164917749
|#

;;; Bisection with interpolation

(define (bisect-fp f x0 x1 eps)
  (let loop ((x0 x0) (fx0 (f x0)) (x1 x1) (fx1 (f x1)))
    (if (= fx0 0.0) 
	x0
	(if (= fx1 0.0)
	    x1
	    (if (> (* fx1 fx0) 0.0)
		(error "root not bounded")
		(let ((xm (/ (- (* fx1 x0) (* fx0 x1)) (- fx1 fx0))))
		  (if (close-enuf? x0 x1 eps)
		      xm
		      (let ((fxm (f xm)))
			(if (< (* fx1 fxm) 0.0)
			    (loop xm fxm x1 fx1)
			    (loop x0 fx0 xm fxm))))))))))

#|
;;; for example

(define (kepler ecc m)
  (bisect-fp
   (lambda (e)
     (write-line e)
     (- e (* ecc (sin e)) m))
   0.0
   2pi
   1e-15))

(kepler .99 .01)
6.283185307179586
0.
.01
.01988423649613729
2.9653394755776365e-2
3.9307245802801455e-2
;;; Total of 536 lines here -- ugh!
.3422703164917746
.3422703164917747
.34227031649177475
.3422703164917748
.34227031649177486
.342270316491775
;Value: .342270316491775
|#

;;; Mixed strategy
;;;   for iterations up to *bisect-break* uses midpoint 
;;;   for iterations after *bisect-break* uses linear interpolation

(define *bisect-break* 60)

(define *bisect-wallp* #f)

(define *bisect-error?* #f)

(define (bisect f x0 x1 eps #:optional n-break)
  (let ((n-break (if (default-object? n-break) *bisect-break* n-break)))
    (let loop ((x0 x0) (fx0 (f x0)) (x1 x1) (fx1 (f x1)) (iter 0))
      (if *bisect-wallp* (write-line (list x0 x1)))
      (if (= fx0 0.0) 
          x0
          (if (= fx1 0.0)
              x1
              (if (> (* fx1 fx0) 0.0)
                  (if *bisect-error?*
                      (error "root not bounded")
                      #f)
                  (let ((xm (if (< iter n-break) 
                                (/ (+ x0 x1) 2.)
                                (/ (- (* fx1 x0) (* fx0 x1)) (- fx1 fx0)))))
                    (if (close-enuf? x0 x1 eps)
                        xm
                        (let ((fxm (f xm)))
                          (if (< (* fx1 fxm) 0.0)
                              (loop xm fxm x1 fx1 (fix:+ iter 1))
                              (loop x0 fx0 xm fxm (fix:+ iter 1))))))))))))

#|
;;; for example

(define (kepler ecc m)
  (bisect
   (lambda (e)
     (write-line e)
     (- e (* ecc (sin e)) m))
   0.0
   2pi
   1e-15
   20))

(kepler .99 .01)
6.283185307179586
0.
3.141592653589793
1.5707963267948966
.7853981633974483
.39269908169872414
.19634954084936207
.2945243112740431
.3436116964863836
.3190680038802134
.3313398501832985
.337475773334841
.3405437349106123
.342077715698498
.3428447060924408
.3424612108954694
.3422694632969837
.3423653370962265
.3423174001966051
.3422934317467944
.34228144752188905
.3422754554094364
.3422703164809715
.34227031649177475
.34227031649177553
;Value: .34227031649177553
|#

;;; If we don't know anything, it is usually a good idea to 
;;;   break the interval into dx-sized pieces and look for 
;;;   roots in each interval.

(define (find-a-root f x0 x1 dx eps continue failure)
  (define (find x0 x1)
    (if (> (abs (- x0 x1)) dx)
	(let ((f1 (f x1)) (f0 (f x0)))
	  (if (< (* f0 f1) 0)
	      (continue (bisect f x0 x1 eps))
	      (let ((xm (/ (+ x0 x1) 2)))
		(find x0 xm)
		(find xm x1))))
	failure))
  (find x0 x1))


;;; Collect the roots found.

(define (search-for-roots f x0 x1 eps small)
  (define (find-roots x0 x1)
    (let ((f1 (f x1)) (f0 (f x0)))
      (if (< (abs (- x1 x0)) small)
	  (if (< (* f0 f1) 0)
	      (list (bisect f x0 x1 eps))
	      '())
	  (let ((xm (/ (+ x0 x1) 2)))
	    (append (find-roots x0 xm)
		    (find-roots xm x1))))))
  (find-roots x0 x1))
