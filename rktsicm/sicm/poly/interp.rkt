#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         "../kernel-gnrc.rkt"
         "../general/assert.rkt"
         "../general/list-utils.rkt"
         )

;;; Given a list of distinct abscissas xs = (x1 x2 ... xn) and a list
;;; of ordinates ys = (y1 y2 ... yn), return the Lagrange interpolation
;;; polynomial through the points (x1, y1), (x2, y2), ... (xn, yn).

;;; Edited by GJS 10Jan09

;;; This version, in the file interp.scm, is numerical.  
;;; It is loaded into scmutils-base-environment.
;;; There is also a generic version in the file interp-generic.scm.
(define (lagrange-interpolation-function ys xs)
  (let ((n (length ys)))
    (assert (fx= (length xs) n))
    (define (poly x)
      (reduce + :zero
	      (generate-list n
		(lambda (i)
		  (/ (reduce * :one
		       (generate-list n
		         (lambda (j)
			   (if (fx= j i)
			       (list-ref ys i)
			       (- x (list-ref xs j))))))
		     (let ((xi (list-ref xs i)))
		       (reduce * :one
			 (generate-list n
		           (lambda (j)
			     (cond ((fx< j i) (- (list-ref xs j) xi))
				   ((fx= j i) (expt :-one i))
				   (else    (- xi (list-ref xs j)))))))))))))
    poly))

#|
;;; If run in generic environment we can look at the kind of thing that 
;;; this code does, by partial evaluation... an excellent aid to debugging. 

(print-expression
 ((lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x1))
y1

(print-expression
 ((lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x2))
y2

(print-expression
 ((lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x3))
y3

(print-expression
 ((lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x4))
y4
|#

#|
(pp (cselim
     (expression
      ((lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4))
       'x))))
(let ((V-609 (- x3 x4)) (V-607 (- x2 x3)) (V-606 (* (- x2 x4) -1))
      (V-605 (- x x1)) (V-604 (- x1 x4)) (V-603 (- x1 x3))
      (V-602 (- x1 x2)) (V-601 (- x x2)) (V-599 (- x x3))
      (V-598 (- x x4)))
  (let ((V-608 (* V-601 V-605)) (V-600 (* V-598 V-599)))
    (+ (/ (* V-600 V-601 y1) (* V-602 V-603 V-604))
       (/ (* V-600 y2 V-605) (* V-606 V-607 V-602))
       (/ (* V-608 V-598 y3) (* V-603 V-607 V-609))
       (/ (* V-608 y4 V-599) (* V-606 V-609 V-604)))))
|#












		       
