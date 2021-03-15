#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         "../rkt/int.rkt"
         "sets.rkt"
         )

;;; Generates a list of all permutations of a list of distinct elements.

(define (permutations lst)
  (if (null? lst)
      '(())
      (apply append
	     (map (lambda (item)
		    (map (lambda (perm) (cons item perm))
			 (permutations (remove item lst))))
		  lst))))

(define (combinations lst p)
  (cond ((= p 0) '(()))
	((null? lst) '())
	(else (append (map (lambda (rest)
			     (cons (car lst) rest))
			   (combinations (cdr lst) (- p 1)))
		      (combinations (cdr lst) p)))))
#|
(pp (combinations '(a b c d e) 3))
((a b c) 
 (a b d)
 (a b e)
 (a c d)
 (a c e)
 (a d e)
 (b c d)
 (b c e)
 (b d e)
 (c d e))
|#

;;; Returns the number of interchanges required to generate the
;;;  permuted list from the original list.

(define (list-interchanges permuted-list original-list)
  (let lp1 ((plist permuted-list) (count 0))
    (if (null? plist)
	count
	(let ((bigger (cdr (member (car plist) original-list)))
	      (rest (cdr plist)))
	  (let lp2 ((l rest) (increment 0))
	    (if (null? l)
		(lp1 rest
		     (fx+ count increment))
		(lp2 (cdr l)
		     (if (not (member (car l) bigger))
			 (fx+ increment 1)
			 increment))))))))

(define (split-permutations original-list list-of-permutations cont)
  ;; cont = (lambda (even-permutations odd-permutations) ... )
  (let lp ((perms list-of-permutations) (evens '()) (odds '()))
    (if (null? perms)
	(cont evens odds)
	(let ((sig (list-interchanges (car perms) original-list)))
	  (if (even? sig)
	      (lp (cdr perms) (cons (car perms) evens) odds)
	      (lp (cdr perms) evens (cons (car perms) odds)))))))


;;; Returns the number of interchanges required to generate the
;;;  permuted list of numbers from an ordered list.

(define (permutation-interchanges permuted-list)
  (let lp1 ((plist permuted-list) (count 0))
    (if (null? plist)
	count
	(let ((first (car plist))
	      (rest (cdr plist)))
	  (let lp2 ((l rest) (increment 0))
	    (if (null? l)
		(lp1 rest
		     (fx+ count increment))
		(lp2 (cdr l)
		     (if (int:> (car l) first)
			 increment
			 (fx+ increment 1)))))))))

;;; Given a permutation (represented as a list of numbers),
;;;  and a list to be permuted, construct the list so permuted.

(define (permute permutation lst)
  (map (lambda (p)
	 (list-ref lst p))
       permutation))

;;; Given a short list and a comparison function, to sort the list by
;;; the comparison, returning the original list, the sorted list, the
;;; permutation procedure and the inverse permutation procedure
;;; developed by the sort.

(define (sort-and-permute ulist <? cont)
  ;; cont = (lambda (ulist slist perm iperm) ...)
  (let* ((n
	  (length ulist))
	 (lsource
	  (map list ulist (build-list n values)))
	 (ltarget
	  (sort lsource
		(lambda (x y) (<? (car x) (car y)))))
	 (sorted (map car ltarget))
	 (perm (map cadr ltarget))
	 (iperm
	  (build-list n
	    (lambda (i) (list-ref perm i)))))
    (cont ulist
	  sorted
	  (lambda (l) (permute perm l))
	  (lambda (l) (permute iperm l)))))
    
#|
;;; For example

(sort-and-permute '(0 2 0 0 1 2 0 0) <
  (lambda (unsorted sorted permuter unpermuter)
    (list unsorted sorted (permuter unsorted) (unpermuter sorted))))
#|
((0 2 0 0 1 2 0 0)
 (0 0 0 0 0 1 2 2)
 (0 0 0 0 0 1 2 2)
 (0 2 0 0 1 2 0 0))
|#
|#

;;; Sometimes we want to permute some of the elements of a list, as follows:
;;; (subpermute '((1 . 4) (4 . 2) (2 . 3) (3 . 1)) '(a b c d e))
;;; ;Value 6: (a e d b c)

(define (subpermute the-map lst)
  (let* ((n (length lst)))
    (let lp ((i 0) (source lst) (answer '()))
      (if (fx= i n)
	  (reverse answer)
	  (let ((entry (assoc i the-map)))
	    (if (not entry)
		(lp (fx+ i 1)
		    (cdr source)
		    (cons (car source) answer))
		(lp (fx+ i 1)
		    (cdr source)
		    (cons (list-ref lst (cdr entry)) answer))))))))


(define (factorial n)
  (define (f n)
    (if (= n 0)
	1
	(* n (f (- n 1)))))
  (when (and (exact-integer? n) (not (negative? n)))
    (raise-argument-error 'factorial "exact-nonnegative-integer" n))
  (f n))

(define number-of-permutations factorial)

(define (number-of-combinations  n k)
  (int:quotient (factorial n)
		(int:* (factorial (int:- n k))
		       (factorial k))))


(define (permutation-parity permuted-list original-list)
  (if (same-set? permuted-list original-list)
      (if (even? (list-interchanges permuted-list original-list))
	  +1
	  -1)
      0))
