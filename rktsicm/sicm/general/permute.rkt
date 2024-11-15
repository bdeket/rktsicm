#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" fix:= fix:+
                  int:>= int:< int:* int:-
                  delete iota make-initialized-list)
         (only-in "list-utils.rkt" list-index-of)
         "sets.rkt"
         "assert.rkt"
         "equals.rkt"
         )

;;bdk;; start original file


;;; Generates a list of all permutations of a list of distinct elements.

(define (permutations lst)
  (if (null? lst)
      '(())
      (apply append
	     (map (lambda (item)
		    (map (lambda (perm) (cons item perm))
			 (permutations (delete item lst simple:equal?))))
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
	(let ((bigger (cdr (member (car plist) original-list simple:equal?)))
	      (rest (cdr plist)))
	  (let lp2 ((l rest) (increment 0))
	    (if (null? l)
		(lp1 rest
		     (fix:+ count increment))
		(lp2 (cdr l)
		     (if (not (member (car l) bigger simple:equal?))
			 (fix:+ increment 1)
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
		     (fix:+ count increment))
		(lp2 (cdr l)
		     (if (int:>= (car l) first)
			 increment
			 (fix:+ increment 1)))))))))

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
	  (map list ulist (iota n)))
	 (ltarget
	  (sort lsource
		(lambda (x y) (<? (car x) (car y)))))
	 (sorted (map car ltarget))
	 (perm (map cadr ltarget))
	 (iperm
	  (make-initialized-list n
	    (lambda (i) (list-index-of i perm)))))
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
      (if (fix:= i n)
	  (reverse answer)
	  (let ((entry (assv i the-map)))
	    (if (not entry)
		(lp (fix:+ i 1)
		    (cdr source)
		    (cons (car source) answer))
		(lp (fix:+ i 1)
		    (cdr source)
		    (cons (list-ref lst (cdr entry)) answer))))))))


(define (factorial n)
  (if (int:< n 2)
      1
      (int:* n (factorial (int:- n 1)))))

(define number-of-permutations factorial)

#| From Sam Ritchie

(define (number-of-combinations  n k)
  (int:quotient (factorial n)
		(int:* (factorial (int:- n k))
		       (factorial k))))

Because it doesn't cancel out factors, the numbers get huge and the
computation is slow. Also big inputs can exceed recursion depth:

3 error> (number-of-combinations 1000000 12)
;Aborting!: maximum recursion depth exceeded

GJS: Done!  Binomial coefficient was defined in kernel/numeric.scm
|#
;;bdk;; insert 1
(define (exact-quotient n d)
  (define-values (q r) (quotient/remainder n d))
  (assert (= 0 r))
  q)


(define (binomial-coefficient n m)
  (assert (and (exact-integer? n) (exact-integer? m) (<= 0 m n)))
  (let ((d (- n m)))
    (let ((t (max m d)) (s (min m d)))
      (define (lp count prod)
	(if (= count t)
	    (exact-quotient prod (factorial s))
	    (lp (- count 1) (* count prod))))
      (lp n 1))))
;;bdk;; insert 1 end

(define (number-of-combinations n k)
  (cond ((< k 1) 1)
        ((> k n) 0)
        (else
         (binomial-coefficient n k))))
#|
1 ]=> (number-of-combinations 1000000 12)
;Value: 2087489902989715894938746580371577443671966248767470771200000000
;;bdk;; wrong! should be: (calc is correct, comment not)
;;bdk;; 2087537916209397485013453738892186349699824088113864639583250000
|#

(define (permutation-parity permuted-list original-list)
  (if (same-set? permuted-list original-list)
      (if (even? (list-interchanges permuted-list original-list))
	  +1
	  -1)
      0))
