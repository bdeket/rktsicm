#lang racket/base

(provide (all-defined-out))

(require "../kernel-gnrc.rkt"
         "canonical.rkt"
         "universal.rkt"
         (only-in "../rkt/todo.rkt" todos)
         )

;TODO: was this what was meant?
(define J*-func J-func)

;;;; This is obsoelete.

(define ((dual-canonical? C) s)
  (let ((DC* (flip-indices ((D C) s))))
    ((- J*-func
	(compose (Phi DC*) 
		 J*-func
		 (Phi* DC*)))
     (typical-object s))))

#|
(print-expression
 ((dual-canonical? (F->CT p->r))
  (->H-state 't
	     (coordinate-tuple 'r 'phi)
	     (momentum-tuple 'p_r 'p_phi))))
(down 0 (down 0 0) (up 0 0))

;;; but not all transforms are

(define (a-non-canonical-transform Istate)
  (let ((t (time Istate))
        (theta (coordinate Istate))
	(p (momentum Istate)))
    (let ((x (* p (sin theta)))
	  (p_x (* p (cos theta))))
      (->H-state t x p_x))))

(print-expression
 ((dual-canonical? a-non-canonical-transform)
  (->H-state 't 'theta 'p)))
(down 0 (+ (* -1 p x11190) x11190) (+ (* p x11189) (* -1 x11189)))


(print-expression
 ((dual-canonical? (polar-canonical 'alpha))
  (->H-state 't 'a 'I)))
(down 0 0 0)
|#

#|
(define (Cmix H-state)
  (let ((t (time H-state))
	(q (coordinate H-state))
	(p (momentum H-state)))
    (->H-state t
	       (coordinate-tuple (ref q 0) (- (ref p 1)))
	       (momentum-tuple   (ref p 0) (ref q 1)))))

(define a-state (->H-state 't 
			   (coordinate-tuple 'x 'y)
			   (momentum-tuple 'p_x 'p_y)))
(print-expression
 ((dual-canonical? Cmix)
  a-state))
(down 0 (down 0 0) (up 0 0))

(define (Cmix2 H-state)
  (let ((t (time H-state))
	(q (coordinate H-state))
	(p (momentum H-state)))
    (->H-state t
	       (flip-outer-index p)
	       (- (flip-outer-index q)))))

(print-expression
 ((dual-canonical? Cmix2)
  a-state))
(down 0 (down 0 0) (up 0 0))
|#

#|
(define ((C m0 m1) state)
  (let ((x (coordinate state))
	(p (momentum state)))
    (let ((x0 (ref x 0))
	  (x1 (ref x 1))
	  (p0 (ref p 0))
	  (p1 (ref p 1)))
      (->H-state 
       (time state)
       (coordinate-tuple (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
			 (- x1 x0))
       (momentum-tuple (+ p0 p1)
		       (/ (- (* m0 p1) (* m1 p0))
			  (+ m0 m1)))))))

(define b-state
  (->H-state
   't
   (coordinate-tuple
    (coordinate-tuple 'x_1 'y_1)
    (coordinate-tuple 'x_2 'y_2))
   (momentum-tuple
    (momentum-tuple 'p_x_1 'p_y_1)
    (momentum-tuple 'p_x_2 'p_y_2))))

(print-expression
 ((dual-canonical? (C 'm1 'm2)) b-state))
(down 0 (down (down 0 0) (down 0 0)) (up (up 0 0) (up 0 0)))

|#
