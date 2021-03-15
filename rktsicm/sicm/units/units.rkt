#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         "../general/assert.rkt"
         "../kernel/generic.rkt"
         "../kernel/iterat.rkt"
         "../kernel/types.rkt"
         "../kernel/numbers.rkt")

;;;; Manipulation of units

;;; The units of a quantity are represented as a combination of a
;;; (labeled) vector of exponents, one for each base unit in a system,
;;; and a scale factor.  The unit objects, such as meter or joule are
;;; such objects.  Multiplication, division, and exponentiation are
;;; extended to combine units from the same system.

#|
;;; in types.scm

(define unit-type-tag '*unit*)

(define (units? x)
  (or (eq? x '&unitless)
      (and (pair? x)
	   (eq? (car x) unit-type-tag))))
|#

(define &unitless '&unitless)

(define (unitless? x)
  (eq? x &unitless))

(define the-empty-vector (vector))

(define (make-unit system exponents scale-factor)
  (if (or (eq? exponents the-empty-vector)
          (vector-forall zero? exponents))
      (if (equal? scale-factor 1)
          &unitless
          (list unit-type-tag system the-empty-vector scale-factor))
      (list unit-type-tag system exponents scale-factor)))



(define (unit-system u)
  (or (unitless? u) (cadr u)))

(define (unit-exponents u)
  (if (unitless? u) the-empty-vector (caddr u)))

(define (unit-scale u)
  (if (unitless? u) 1 (cadddr u)))

(define (same-dimensions? u1 u2)
  (let ((v1 (unit-exponents u1)) (v2 (unit-exponents u2)))
    (let ((n (vector-length v1)))
      (let lp ((i 0))
        (or (fx= i n)
            (and (n:= (vector-ref v1 i) (vector-ref v2 i))
                 (lp (fx+ i 1))))))))

(define (same-units? u1 u2)
  (assert (and (units? u1) (units? u2)))
  (or (eq? u1 u2)
      (and (eq? (unit-system u1) (unit-system u2))
           (same-dimensions? u1 u2)
           (n:= (unit-scale u1) (unit-scale u2)))))

(define (<-units? u1 u2)
  (assert (and (units? u1) (units? u2)))
  (or (eq? u1 u2)
      (and (eq? (unit-system u1) (unit-system u2))
           (same-dimensions? u1 u2)
           (n:< (unit-scale u1) (unit-scale u2)))))

(define (<=-units? u1 u2)
  (assert (and (units? u1) (units? u2)))
  (or (eq? u1 u2)
      (and (eq? (unit-system u1) (unit-system u2))
           (same-dimensions? u1 u2)
           (or (n:< (unit-scale u1) (unit-scale u2))
               (n:= (unit-scale u1) (unit-scale u2))))))


(define (>-units? u1 u2)
  (assert (and (units? u1) (units? u2)))
  (or (eq? u1 u2)
      (and (eq? (unit-system u1) (unit-system u2))
           (same-dimensions? u1 u2)
           (n:> (unit-scale u1) (unit-scale u2)))))

(define (>=-units? u1 u2)
  (assert (and (units? u1) (units? u2)))
  (or (eq? u1 u2)
      (and (eq? (unit-system u1) (unit-system u2))
           (same-dimensions? u1 u2)
           (or (n:> (unit-scale u1) (unit-scale u2))
               (n:= (unit-scale u1) (unit-scale u2))))))

(define (*units u1 u2)
  (cond ((unitless? u1) u2)
        ((unitless? u2) u1)
        (else (assert (and (units? u1) (units? u2)))
              (assert (and (eq? (unit-system u1) (unit-system u2))))
              (let ((v1 (unit-exponents u1)) (v2 (unit-exponents u2)))
                (cond ((eq? v1 the-empty-vector)
                       (make-unit (unit-system u1)
                                  v2
                                  (n:* (unit-scale u1) (unit-scale u2))))
                      ((eq? v2 the-empty-vector)
                       (make-unit (unit-system u1)
                                  v1
                                  (n:* (unit-scale u1) (unit-scale u2))))
                      (else
                       (make-unit (unit-system u1)
                                  (build-vector (vector-length v1)
                                                (lambda (i)
                                                  (n:+ (vector-ref v1 i)
                                                       (vector-ref v2 i))))
                                  (n:* (unit-scale u1) (unit-scale u2)))))))))

(define (invert-units u)
  (let ((v (unit-exponents u)))
    (if (eq? v the-empty-vector)
        (make-unit (unit-system u)
                   the-empty-vector
                   (n:/ 1 (unit-scale u)))
        (make-unit (unit-system u)
                   (build-vector (vector-length v)
                                 (lambda (i)
                                   (n:* -1 (vector-ref v i))))
                   (n:/ 1 (unit-scale u))))))

(define (/units u1 u2)
  (cond ((unitless? u1)
         (if (unitless? u2)
             &unitless
             (let ((v2 (unit-exponents u2)))
               (make-unit (unit-system u2)
                          (build-vector (vector-length v2)
                                        (lambda (i)
                                          (n:* -1 (vector-ref v2 i))))
                          (n:/ 1 (unit-scale u2))))))
        ((unitless? u2) u1)
        (else (assert (and (eq? (unit-system u1) (unit-system u2))))
              (let ((v1 (unit-exponents u1)) (v2 (unit-exponents u2)))
                (cond ((eq? v1 the-empty-vector)
                       (make-unit (unit-system u1)
                                  (build-vector (vector-length v2)
                                                (lambda (i)
                                                  (n:- 0 (vector-ref v2 i))))
                                  (n:/ (unit-scale u1) (unit-scale u2))))
                      ((eq? v2 the-empty-vector)
                       (make-unit (unit-system u1)
                                  v1
                                  (n:/ (unit-scale u1) (unit-scale u2))))
                      (else
                       (make-unit (unit-system u1)
                                  (build-vector (vector-length v1)
                                                (lambda (i)
                                                  (n:- (vector-ref v1 i)
                                                       (vector-ref v2 i))))
                                  (n:/ (unit-scale u1) (unit-scale u2)))))))))

(define (expt-units u p)
  (cond ((unitless? u) u)
        (else (assert (units? u) "Not a unit -- EXPT")
              (let ((v (unit-exponents u)))
                (make-unit (unit-system u)
                           (build-vector (vector-length v)
                                         (lambda (i)
                                           (n:* p (vector-ref v i))))
                           (n:expt (unit-scale u) p))))))

(assign-operation generic:= same-units? units? units?)
(assign-operation generic:< <-units?    units? units?)
(assign-operation generic:<= <=-units?    units? units?)
(assign-operation generic:> >-units?    units? units?)
(assign-operation generic:>= >=-units?    units? units?)

(assign-operation generic:* *units units? units?)
(assign-operation generic:invert invert-units units?)
(assign-operation generic:/ /units units? units?)
(assign-operation generic:expt expt-units units? number?)

(assign-operation generic:solve-linear-right /units units? units?)
(assign-operation generic:solve-linear-left (lambda (x y) (/units y x)) units? units?)
(assign-operation generic:solve-linear      (lambda (x y) (/units y x)) units? units?)
