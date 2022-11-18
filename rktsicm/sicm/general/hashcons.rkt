#lang racket/base

(provide (all-defined-out))

;;;;         HashCONS
;;;  Apparently invented by Ershov 
;;;   (see CACM 1, 8, August 1958, pp. 3--6)
;;;  Re-introduced by E.Goto in 1974.

;;; Implementation by GJS and Taylor Campbell 2010,
;;;  improved by Taylor Campbell in 2011.
;;;  Further improved by Taylor Campbell(2014), 
;;;   using the new key-ephemeral type.
;;; Updated by GJS (2020).

;;; Given two arguments cons-unique returns a
;;; pair.  If exactly the same two arguments were
;;; previously combined with cons-unique it
;;; returns the same pair it returned the first
;;; time.

;;; HashCONS needs a fancy key-ephemeral hash table!

#;(define (pair-eqv? u v)
  (and (eqv? (car u) (car v))
       (eqv? (cdr u) (cdr v))))

#;(define (pair-eqv-hash-mod key modulus)
  (fix:remainder
   (fix:xor (eqv-hash-mod (car key) modulus)
          (eqv-hash-mod (cdr key) modulus))
   modulus))

#;(define the-cons-table
  ((hash-table-constructor
    (make-hash-table-type
     pair-eqv-hash-mod           ;hash-function
     pair-eqv?                   ;key=?
     #t                          ;rehash-after-gc?
     hash-table-entry-type:key-ephemeral ;entry-type
     ))))

;start new impl.
(define (clean)
  (for* ([x (in-list (hash-keys the-cons-table))]
         [H (in-value (hash-ref the-cons-table x #f))]
         #:when H)
    (when (hash? H)
      (for* ([y (in-list (hash-keys H))]
             [B (in-value (hash-ref H y #f))]
             #:when (and B (not (weak-box-value B))))
        (hash-remove! H y)))
    (when (= (hash-count H) 0)
      (hash-remove! the-cons-table x))))

;TODO
;unlike the original impl. this is potentially litered with empty boxes
;racket hash doesn't really allow to make a hash function with
;a different eqv? implementation I think
;at the moment it's cleaned up after finding 10 gc'ed boxes
(define the-cons-table (make-hasheqv))
(define cons-unique
  (let ([miss 0])
    (λ (x y)
      (define H (hash-ref the-cons-table x #f))
      (cond
        [H
         (define B (hash-ref H y #f))
         (cond
           [(and B (weak-box-value B))]
           [else
            (when B
              (if (< 10 miss)
                  (begin (clean)
                         (set! miss 0))
                  (set! miss (+ miss 1))))
            (define the-cons (cons x y))
            (hash-set! H y (make-weak-box the-cons))
            the-cons])]
        [else
         (define the-cons (cons x y))
         (hash-set! the-cons-table x
                    (make-weak-hasheqv (list (cons y (make-weak-box the-cons)))))
         the-cons]))))
#;(define cons-unique
  (let* ((the-test-pair (cons #f #f)))
    (define (generator) the-test-pair)
    (define (hashcons x y)
      (set-car! the-test-pair x)
      (set-cdr! the-test-pair y)
      (let ((the-canonical-pair
             (hash-table/intern! the-cons-table
                                 the-test-pair
                                 generator)))
        (if (eq? the-canonical-pair the-test-pair)
            ;; Test pair used; make a new one.
            (set! the-test-pair (cons #f #f))
            ;; Clear the test pair.
            (begin (set-car! the-test-pair #f)
                   (set-cdr! the-test-pair #f)))
        the-canonical-pair))
    hashcons))

(define hash-cons cons-unique)

;;; This code copies a list structure, with all
;;;  conses uniquified.

(define (canonical-copy x)
  (define (recurse)
    (cons-unique (canonical-copy (car x))
                 (canonical-copy (cdr x))))
  (if (pair? x)
      (let ((v (hash-ref the-cons-table
                         x
                         #f)))
        (or v (recurse)))
      x))


(define (list-unique . lst)
  (canonical-copy lst))

(define (append-unique l1 l2)
  (if (null? l1)
      l2
      (hash-cons (car l1)
                 (append-unique (cdr l1)
                                l2))))

(define (map-unique p lst)
  (if (pair? lst)
      (hash-cons (p (car lst))
                 (map-unique p (cdr lst)))
      lst))

#|
;;; For example...

(define foo
  '(define (canonical-copy x)
     (if (pair? x)
	 (let ((canonical-pair
		(hash-table/get the-cons-table x #f)))
	   (or canonical-pair
	       (let ((new
		      (cons (canonical-copy (car x))
			    (canonical-copy (cdr x)))))
		 (hash-table/put! the-cons-table new new)
		 new)))
	 x)))

(define bar
  '(define cons-unique
     (let ((the-pair (cons #f #f)))  
       (define (hashcons x y)
	 (set-car! the-pair x)
	 (set-cdr! the-pair y)
	 (let ((canonical-pair
		(hash-table/get the-cons-table the-pair #f)))
	   (or canonical-pair
	       (let ((new the-pair))
		 (hash-table/put! the-cons-table new new)
		 (set! the-pair (cons #f #f))
		 new))))
       hashcons)))

(define cfoo
  (canonical-copy foo))
;Value: cfoo

(eq? cfoo
     (canonical-copy foo))
;Value: #t

(define cbar
  (canonical-copy bar))
;Value: cbar

(define baz
  (caddr (caddr (caddr (caddr (caddr cfoo))))))
;Value: baz

baz
;Value: (hash-table/put! the-cons-table new new)


(define mum
  (caddr
   (caddr (caddr (car (cddddr (caddr (caddr cbar))))))))
;Value: mum

mum
;Value: (hash-table/put! the-cons-table new new)

(eq? baz mum)
;Value: #t
|#

;;; This code is hard to test.  The following
;;; should never return with (lose ...).
#|
(set-gc-notification!)

(let lp ((t1 (tree-copy cfoo)) (t2 (tree-copy cfoo)))
  (let ((c1 (canonical-copy t1)) (c2 (canonical-copy t2)))
    (if (not (and (equal? t1 c1) (equal? t2 c2) (eq? c1 c2)))
	(error `(lose (,(hash c1) ,c1) (,(hash c2) ,c2)))
	(lp (tree-copy c1) t1))))
;GC #6 23:09:39: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188457
;GC #7 23:10:26: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188445
;GC #8 23:11:13: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188385
;GC #9 23:12:01: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188361
;GC #10 23:12:48: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188341
;GC #11 23:13:35: took:   0.20   (0%) CPU,   0.10   (0%) real; free: 307188354
;GC #12 23:14:23: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188405
;GC #13 23:15:10: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188364
;GC #14 23:15:58: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188579
;GC #15 23:16:46: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188396
;GC #16 23:17:34: took:   0.20   (0%) CPU,   0.10   (0%) real; free: 307188391
;GC #17 23:18:22: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188352
;GC #18 23:19:10: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188495
;GC #19 23:19:58: took:   0.10   (0%) CPU,   0.10   (0%) real; free: 307188404
;...
|#