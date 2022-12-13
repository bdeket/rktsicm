#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" if iota
                  fix:= fix:+)
         (only-in "../rkt/define.rkt" define default-object?)
         "../general/assert.rkt"
         "../general/list-utils.rkt"
         "../general/permute.rkt"
         "dgutils.rkt"
         "indexed/types.rkt"
         "vector-fields.rkt"
         )

;;bdk;; start original file

;;; Higher rank forms can be constructed from 1forms by wedging them
;;; together.  This antisymmetric tensor product is computed as a
;;; determinant.  The purpose of this is to allow us to use the
;;; construction dx^dy to compute the area described by the vectors
;;; that are given to it.

(define (wedge2 form1 form2)
  (let ((n1 (get-rank form1)) (n2 (get-rank form2)))
    (if (or (zero? n1) (zero? n2))
	(* form1 form2)
	(let ((n (fix:+ n1 n2))
              (k (/ 1 (* (factorial n1) (factorial n2)))))
	  (define (the-wedge . args)
	    (assert (fix:= (length args) n)
		    "Wrong number of args to wedge product")
	    (let ((perms (permutations (iota n))))
	      (g:* k                    ; Error in Singer.
		   (apply g:+
			  (map (lambda (p)
				 (let ((pargs (permute p args)))
				   (let ((order (permutation-interchanges p)))
                                     (define-values (a1 a2) (split-at pargs n1))
				     (g:* (if (even? order) 1 -1)
					  (apply form1 a1)
					  (apply form2 a2)))))
			       perms)))))
	  (procedure->nform-field the-wedge
				  n
				  `(wedge ,(diffop-name form1)
					  ,(diffop-name form2)))))))

(define (wedge . args)
  (reduce-right wedge2 (constant 1) args))

;;; See Spivak p275 v1 "Differential Geometry" to see the correct
;;; definition.  The key is that the wedge of the coordinate basis
;;; forms had better be the volume element.


(define (get-rank op)
  (cond ((operator? op)
	 (let ((a (o:arity op)))
           (unless (exactly-n? a)
             (error (format "Unknown rank operator ~a -> ~a" op a)))
	   a))
	((function? op) 0)
	(else (error "Bad rank " op))))

(define (rank->arity n)
  (exact-arity n))

(define (procedure->nform-field proc n #:optional name)
  (if (default-object? name)
      (set! name 'unnamed-nform-field))
  (if (= n 0)
      (proc)
      (let ((the-field (make-operator proc name wedge (rank->arity n))))
	(declare-argument-types! the-field (make-list n vector-field?))
	the-field)))

#|
(install-coordinates R3-rect (up 'x 'y 'z))

(define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))

(define w (literal-1form-field 'w R3-rect))
(define u (literal-1form-field 'u R3-rect))
(define v (literal-1form-field 'v R3-rect))

(define X (literal-vector-field 'X R3-rect))
(define Y (literal-vector-field 'Y R3-rect))
(define Z (literal-vector-field 'Z R3-rect))
(define W (literal-vector-field 'W R3-rect))

;;; Just checking that everything is working...

(pec ((w X) R3-point))
#| Result:
(+ (* (X^0 (up x0 y0 z0)) (w_0 (up x0 y0 z0)))
   (* (X^1 (up x0 y0 z0)) (w_1 (up x0 y0 z0)))
   (* (X^2 (up x0 y0 z0)) (w_2 (up x0 y0 z0))))
|#

;;; A few theorems

(pec (((- (wedge (wedge w u) v) (wedge w (wedge u v))) X Y Z)
      R3-point))
#| Result:
0
|#

(pec (((- (wedge (+ w u) v) (+ (wedge w v) (wedge u v))) X Y)
      R3-point))
#| Result:
0
|#

;;; Note, a product of forms is their wedge!

(pec (((- (wedge u v) (* u v)) X Y)
      R3-point))
#| Result:
0
|#
|#

#|
(define dx^dy (wedge dx dy))

((dx^dy d/dx d/dy) R3-point)
;Value 1

((dx^dy d/dx d/dx) R3-point)
;Value: 0

((dx^dy d/dy d/dx) R3-point)
;Value: -1
|#

;;; Alternative definition in terms of alternation.

(define (Alt form)
  (let ((n (get-rank form)))
    (if (zero? n)
	form
	(let ()
	  (define (the-alternation . args)
	    (assert (fix:= (length args) n)
		    "Wrong number of args to alternation")
	    (let ((perms (permutations (iota n))))
	      (g:* (/ 1 (factorial n))
		   (apply g:+
			  (map (lambda (p)
				 (let ((pargs (permute p args)))
				   (let ((order (permutation-interchanges p)))
				     (g:* (if (even? order) 1 -1)
					  (apply form pargs)))))
			       perms)))))
	  (procedure->nform-field the-alternation
				  n
				  `(Alt ,(diffop-name form)))))))

(define (tensor-product2 t1 t2)
  (let ((n1 (get-rank t1)) (n2 (get-rank t2)))
    (if (or (zero? n1) (zero? n2))
        (* t1 t2)
        (let ((n (fix:+ n1 n2)))
          (define (the-product . args)
            (assert (fix:= (length args) n)
                    "Wrong number of args to tensor product")
            (define-values (h t) (split-at args n1))
            (* (apply t1 h) (apply t2 t)))
          (procedure->nform-field the-product
                                  n
                                  `(tensor-product ,(diffop-name t1)
                                                   ,(diffop-name t2)))))))

(define (w2 form1 form2)
  (let ((n1 (get-rank form1)) (n2 (get-rank form2)))
    (* (/ (factorial (+ n1 n2))
	  (* (factorial n1) (factorial n2)))
       (Alt (tensor-product2 form1 form2)))))

;;;(define (wedge . args)
;;;  (reduce w2 (constant 1) args))
