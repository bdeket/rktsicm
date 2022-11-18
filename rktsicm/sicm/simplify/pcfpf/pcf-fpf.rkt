#lang racket/base

(provide (all-defined-out))

(require "../../rkt/fixnum.rkt"
         "../../general/list-utils.rkt"
         "../../kernel/utils.rkt"
         "../fpf.rkt"
         "pcf.rkt"
         "../sparse.rkt"
         )

;;;; This is the top level of polynomial gcd stuff.

(define *gcd-cut-losses* #f)
;;;(define *gcd-cut-losses* 1000.0)

(define (poly/gcd-classical u v)
  (cond ((explicit-pcf? u)
	 (cond ((explicit-pcf? v)
		(poly/gcd-euclid u v))
	       ((explicit-fpf? v)
		(poly/gcd-euclid u (fpf->pcf v)))
	       (else (error "What do I do here?"))))
	((explicit-fpf? u)
	 (cond ((explicit-pcf? v)
		(pcf->fpf
		 (poly/gcd-euclid (fpf->pcf u) v)))
	       ((explicit-fpf? v)
		(pcf->fpf
		 (poly/gcd-euclid (fpf->pcf u)
				  (fpf->pcf v))))
	       (else (error "What do I do here?"))))))


(define *euclid-breakpoint-arity* 3)

(define (gcd-check-same-arity u v)
  (let ((au (poly:arity u)))
    (when (not (fix:= au (poly:arity v)))
	(error "Unequal arities -- poly:gcd" u v))
    au))
    
(define (gcd-target-type u)
  (cond ((explicit-pcf? u) '*pcf*)
	((explicit-fpf? u) '*fpf*)
	(else
	 (error "Unknown type: gcd-target-type" u))))

(define (poly->sparse p)
  (cond ((explicit-pcf? p) (pcf->sparse p))
	((explicit-fpf? p) (fpf:terms p))
	(else
	 (error "Unknown type: poly->sparse" p))))

(define (sparse->poly s type)
  (cond ((eq? type '*pcf*)
	 (sparse->pcf s))
	((eq? type '*fpf*)
	 (fpf:make s))
	(else
	 (error "Unknown type: sparse->poly" s type))))  

(define (fpf->pcf p)
  (sparse->pcf (fpf:terms p)))

(define (pcf->fpf p)
  (fpf:make (pcf->sparse p)))

(define (pcf->sparse p)
  (let lp ((p p) (arity (poly:arity p)))
    ;;(pp `((p ,p) (arity ,arity)))
    (if (base? p)
	(if (zero? p)
	    '()
	    (list (sparse-term (make-list arity 0) p)))
	(let ((degree (poly:degree p))
	      (c (poly:leading-coefficient p))
	      (r (poly:except-leading-term arity p)))
	  ;;(pp (list degree c r))
	  (sparse-combine-like-terms
	   (append
	    (map (lambda (s-term)
		   (sparse-term (cons degree (sparse-exponents s-term))
				(sparse-coefficient s-term)))
		 (lp c (fix:- arity 1)))
	    (lp r arity)))))))

(define (sparse->pcf s)
  (if (null? s)
      poly:zero
      (let ((v (poly:new-variables (length (sparse-exponents (car s))))))
	(a-reduce poly:+
		  (map (lambda (sterm)
			 (poly:* (sparse-coefficient sterm)
				 (a-reduce poly:*
					   (map poly:expt
						v
						(sparse-exponents sterm)))))
		       s)))))


