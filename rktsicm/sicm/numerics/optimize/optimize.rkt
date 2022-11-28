#lang racket/base

(provide (all-defined-out))

(require (only-in "../../rkt/glue.rkt" for-all?
                  fix:= fix:+)
         "../../general/list-utils.rkt"
         "../../kernel-intr.rkt"
         "unimin.rkt"
         "multimin.ss"
         )

;;;; Top-level optimization defaults

(define (minimize f lowx highx)
  (brent-min f lowx highx brent-error))
  
(define brent-error 1.0e-5)

;;; f is a function of the parameters.

(define nelder-start-step .01)
(define nelder-epsilon 1.0e-10)
(define nelder-maxiter 1000)

#|
;;; Simple, flat case 

(define (multidimensional-minimize f parameters)
  (let ((f (compose f vector->list)))
    (let ((result
	   (nelder-mead f
			(list->vector parameters)
			nelder-start-step
			nelder-epsilon
			nelder-maxiter)))
      (if (eq? 'ok (car result))
	  (vector->list (caadr result))
	  (error "Minimizer did not converge" result)))))
|#


(define (multidimensional-minimize f parameters)
  (let ((f (compose f (vector->parameters parameters))))
    (let ((result
	   (nelder-mead f
			(parameters->vector parameters)
			nelder-start-step
			nelder-epsilon
			nelder-maxiter)))
      (if (eq? 'ok (car result))
	  ((vector->parameters parameters)
	   (caadr result))
	  (error "Minimizer did not converge" result)))))

(define (parameters->vector p)
  (define (flatten x)
    (cond ((number? x) (list x))
	  ((structure? x)
	   (let ((lst (vector->list (s:->vector x))))
	     (if (for-all? lst number?)
		 lst
		 (append-map flatten lst))))
	  ((list? x)
	   (if (for-all? x number?)
	       x
	       (append-map flatten x)))
	  (else
	   (error "Non-numerical data in optimizer" p x))))
  (if (and (list? p) (for-all? p number?))
      (list->vector p)
      (list->vector (flatten p))))

#|
(parameters->vector
 (list (vector 1 2.3 4)
       (up 3.5 (down 5 1.3) 6)))
;Value: #(1 2.3 4 3.5 5 1.3 6)
|#

(define ((vector->parameters prototype) vect)
  (let ((cur 0))
    (let plp ((proto prototype))
      (cond ((structure? proto)
	     (s:generate (s:length proto)
			 (s:same proto)
			 (lambda (i)
			   (plp (s:ref proto i)))))
	    ((list? proto)
	     (let llp ((proto proto))
	       (if (null? proto)
		   '()
		   (let ((first (plp (car proto))))
		     (cons first (llp (cdr proto)))))))
	    (else
	     (let ((el (vector-ref vect cur)))
	       (set! cur (fix:+ cur 1))
	       el))))))

#|
((vector->parameters
  (list (vector 'a 'b 'c) (up 'd (down 'e 'f) 'g)))
 #(1 2.3 4 3.5 5 1.3 6))
;Value: (#(1 2.3 4) #(3.5 (*down* #(5 1.3)) 6))
|#

#| ;;; Historical nonsense
(define (multidimensional-minimize f x0 cont)
  ;; cont=(lambda (status minimum-point minimum-value) ...)
  (let* ((bundle?
	  (cond ((vector? x0) #f)
		((list-of-vectors? x0) #t)
		(else
		 (error "Bad initial point -- MINIMIZE"
			x0))))
	 (result
	  (nelder-mead (if bundle?
			   (compose f (bundle-vectors (length x0)))
			   f)
		       (if bundle?
			   (flatten-list-of-vectors x0)
			   x0)
		       nelder-start-step
		       nelder-epsilon
		       nelder-maxiter)))
    (cont (eq? 'OK (car result))
	  (if bundle?
	      ((bundle-vectors (length x0)) (caadr result))
	      (caadr result))
	  (cdadr result))))


(define ((bundle-vectors n) qs)
  (let ((dimension (quotient (vector-length qs) n)))
    (let lp ((i 0) (ans '()))
      (if (fix:= i n)
	  (reverse ans)
	  (lp (fix:+ i 1)
	      (cons (subvector qs
			       (fix:* i dimension)
			       (fix:* (fix:+ i 1) dimension))
		    ans))))))

(define (flatten-list-of-vectors l)
  (list->vector (apply append (map vector->list l))))

  (define (list-of-vectors? l)
    (or (null? l)
	(and (pair? l)
	     (vector? (car l))
	     (list-of-vectors? (cdr l)))))
|#

