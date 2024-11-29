#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" delete-duplicates find)
         "../kernel-intr.rkt"
         "../general/list-utils.rkt"
         "../general/equals.rkt"
         "../display/pp.rkt"
         "solve.rkt")

;;bdk;; start original file

;;;; Utilities for interpreting solver results

;;; These global variables are used to collect information about
;;; the state of the solver.  They must be initialized for use.

(define *complete-solutions* (make-parameter '()))
(define *underdetermined-solutions* (make-parameter '()))
(define *with-residual-equations* (make-parameter '()))
(define *with-tough-equations* (make-parameter '()))
(define *with-extra-equations* (make-parameter '()))

(define (initialize-solver)
  (*complete-solutions* '())
  (*underdetermined-solutions* '())
  (*with-residual-equations* '())
  (*with-tough-equations* '())
  (*with-extra-equations* '()))


;;; To collect all solutions of a set of equations.  
;;; The initialization of the globals is locally done 
;;; by fluid binding.

(define (solve-equations equations unknowns)
  (parameterize ([*complete-solutions* '()]
                 [*underdetermined-solutions* '()]
                 [*with-residual-equations* '()]
                 [*with-tough-equations* '()]
                 [*with-extra-equations* '()])
    (solve-incremental equations
                       unknowns
                       default-succeed
                       default-fail)))

;;; This default success and failure continuation pair collects and
;;; classifies all of the results (in global variables, ugh!) and
;;; returns them at the ultimate failure time.  Note that these global
;;; variables must be initialized at the call to incremental-solve
;;; (see solver-test, below for an example of how to use).

(define (default-succeed result fail)
  (let ((result
	 (make-solution
	  (residual-equations result)
	  (residual-variables result)
	  (sort (map (lambda (s)
		       (make-substitution
			(substitution-variable s)
			(substitution-expression s)
			(sort (substitution-justifications s)
			      expr:<)))
		     (substitutions result))
		(lambda (s1 s2)
		  (expr:< (substitution-variable s1)
			  (substitution-variable s2))))
	  (tough-equations result))))
    (cond ((and (null? (residual-variables result))
                (null? (residual-equations result))
		(null? (tough-equations result)))
	   (accumulate-complete-solutions result))
	  ((and (not (null? (residual-variables result)))
		(null? (residual-equations result))
		(null? (tough-equations result)))
	   (accumulate-underdetermined-solutions result))
	  ((and (not (null? (residual-variables result)))
		(null? (residual-equations result))
                (not (null? (tough-equations result))))
	   (accumulate-tough-equations-solutions result))
	  ((and (null? (residual-variables result))
		(not (null? (residual-equations result)))
		(null? (tough-equations result)))
	   (accumulate-residual-equation-solutions result))
	  ((and (null? (residual-variables result))
		(null? (residual-equations result))
		(not (null? (tough-equations result))))
	   (accumulate-extra-equations-solutions result))
	  (else (error "How did I get here?"))))
  (fail))

(define (default-fail)
  (cond ((not (null? (*complete-solutions*)))
	 `(full-solutions
	   ,@(collect-best-solutions (*complete-solutions*))))
	((not (null? (*underdetermined-solutions*)))
	 `(underdetermined
	   ,@(collect-best-solutions (*underdetermined-solutions*))))
	((not (null? (*with-residual-equations*)))
	 `(parameters-constrained
	   ,@(collect-best-solutions (*with-residual-equations*))))
	((not (null? (*outstanding-contradictions*)))
	 `(contradictions ,@(*outstanding-contradictions*)))
	((not (null? (*with-tough-equations*)))
	 `(tough-equations
	   ,@(collect-best-solutions (*with-tough-equations*))))
	((not (null? (*with-extra-equations*)))
	 `(extra-equations
	   ,@(collect-best-solutions (*with-extra-equations*))))
	(else
	  (error "How did I get here?"))))

(define (accumulate-complete-solutions result)
  (*complete-solutions*
   (lset-adjoin same-solution?
                (*complete-solutions*)
                result)))

(define (accumulate-underdetermined-solutions result)
  (*underdetermined-solutions*
   (lset-adjoin same-solution?
                (*underdetermined-solutions*)
                result)))

(define (accumulate-residual-equation-solutions result)
  (*with-residual-equations*
	(lset-adjoin same-solution?
		     (*with-residual-equations*)
		     result)))

(define (accumulate-tough-equations-solutions result)
  (*with-tough-equations*
   (lset-adjoin same-solution?
                (*with-tough-equations*)
                result)))

(define (accumulate-extra-equations-solutions result)
  (*with-extra-equations*
   (lset-adjoin same-solution?
                (*with-extra-equations*)
                result)))

(define (same-solution? sol1 sol2)
  (and (same-residual-equations? sol1 sol2)
       (same-residual-variables? sol1 sol2)
       (same-substitutions? sol1 sol2)
       (same-tough-equations? sol1 sol2)))

(define (same-residual-equations? sol1 sol2)
  (lset= same-equation?
	 (residual-equations sol1)
	 (residual-equations sol2)))

(define (same-residual-variables? sol1 sol2)
  (lset= same-variable?
	 (residual-variables sol1)
	 (residual-variables sol2)))

(define (same-substitutions? sol1 sol2)
  (lset= same-substitution?
	 (substitutions sol1)
	 (substitutions sol2)))

(define (same-tough-equations? sol1 sol2)
  (lset= same-equation?
	 (tough-equations sol1)
	 (tough-equations sol2)))

(define (same-equation? eq1 eq2)
  (and (same-expression? (equation-expression eq1)
			 (equation-expression eq2))
       (same-justifications? (equation-justifications eq1)
			     (equation-justifications eq2))))

(define (same-expression? e1 e2)
  (let ((d (s:simplify (symb:- e1 e2))))
    (and (number? d) (~0? d))))

(define (same-variable? v1 v2) (simple:equal? v1 v2))

(define (same-substitution? s1 s2)
  (and (same-variable? (substitution-variable s1)
		       (substitution-variable s2))
       (same-expression? (substitution-expression s1)
			 (substitution-expression s2))
       (same-justifications? (substitution-justifications s1)
			     (substitution-justifications s2))))

(define (same-justifications? js1 js2)
  (lset= same-justification? js1 js2))

(define (same-justification? j1 j2) (simple:equal? j1 j2))

(define (equivalent-solutions? sol1 sol2)
  (and (same-residual-equations? sol1 sol2)
       (same-residual-variables? sol1 sol2)
       (same-tough-equations? sol1 sol2)
       (equivalent-substitutions? sol1 sol2)))

(define (equivalent-substitutions? sol1 sol2)
  (lset= equivalent-substitution?
	 (substitutions sol1)
	 (substitutions sol2)))

(define (equivalent-substitution? s1 s2)
  (and (same-variable? (substitution-variable s1)
		       (substitution-variable s2))
       (same-expression? (substitution-expression s1)
			 (substitution-expression s2))))

(define (one-of-each lists)
  (cond ((null? lists) '())
	((null? (cdr lists)) (map list (car lists)))
	(else
	 (let ((heads (car lists))
	       (tails (one-of-each (cdr lists))))
	   (append-map (lambda (t)
			 (map (lambda (h)
				(cons h t))
			      heads))
		       tails)))))

(define (minimum-length-head lst)
  (if (null? lst)
      lst
      (let lp ((lst (cdr lst)) (result (list (car lst))))
	(cond ((null? lst) result)
	      ((= (length (car lst)) (length (car result)))
	       (lp (cdr lst) (cons (car lst) result)))
	      (else result)))))

(define (substitution-variable-entry var solution)
  (find (lambda (subst)
	  (simple:equal? var (substitution-variable subst)))
	(substitutions solution)))

(define (collect-best-solutions solutions)
  ;;assumption: in each solution substitutions are sorted by varname
  (let lp ((sols solutions) (winners '()))
    (if (null? sols)
        winners
        (let* ((sol1 (car sols))
               (equivalents
                (cons sol1
                      (filter (lambda (sol)
                                (equivalent-solutions? sol1 sol))
                              (cdr sols))))
	       (subs  (substitutions sol1))
	       (vars (map substitution-variable subs))
	       (exprs (map substitution-expression subs))
	       )
	  (let ((req (residual-equations sol1))
		(rev (residual-variables sol1))
		(teq (tough-equations sol1))
		(justs
		 (one-of-each
		  (map (lambda (sub)
			 (let ((var (substitution-variable sub)))
			   (minimum-length-head
			    (delete-duplicates
			     (sort 
			      (map (lambda (solution)
				     (substitution-justifications
				      (substitution-variable-entry var solution)))
				   equivalents)
			      (lambda (j1 j2)
				(< (length j1) (length j2))))))))
		       subs))))
	    (lp (lset-difference simple:equal? sols equivalents)
		(lset-union simple:equal?
                            (map (lambda (just)
                                   (make-solution req
                                                  rev 
                                                  (map (lambda (var expr j)
                                                         (make-substitution var
                                                                            expr
                                                                            j))
                                                       vars exprs just)
                                                  teq))
                                 justs)
                            winners)))))))

;;; Examples of use

(define (test-solver equations unknowns)
  (cpp (solve-equations equations unknowns)))

#|
(test-solver
 (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
       (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
 '(x y))
#|
(full-solutions (() () (((= x 2) (A B)) ((= y 1) (A B))) ()))
|#

(test-solver
 (list (make-equation '(+  x   y   z  1)  (list 'A))
       (make-equation '(+  x   y      2)  (list 'B))
       (make-equation '(+  x          1)  (list 'C)))
 '(x y z))
#|
(full-solutions (() () (((= x -1) (C)) ((= y -1) (B C)) ((= z 1) (A B))) ()))
|#

(test-solver
 (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
       (make-equation '(+ (* 3 x)     y  -5)  (list 'B)))
 '(x y))
#|
(contradictions (-2 (A B) ()) (2 (B A) ()))
|#

(test-solver
 (list (make-equation '(-  3 (+ x y))  (list 'A))
       (make-equation '(-  5 (- x y))  (list 'B))
       (make-equation '(-  3 (+ (* (sqrt x) z) (square y)))  (list 'C)))
 '(x y z))
#|
(full-solutions (() () (((= x 4) (A B)) ((= y -1) (A B)) ((= z 1) (A B C))) ()))
|#

(test-solver
 (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
       (make-equation '(- 3 (+ a b))  (list 'B)))
 '(a b c))
#|
(underdetermined (() (c) (((= a (* 2/3 c)) (A B)) ((= b (+ 3 (* -2/3 c))) (A B))) ()))
|#

(test-solver
 (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
       (make-equation '(- 3 (- a c))  (list 'B)))
 '(a b c))
#|
(underdetermined (() (c) (((= a (+ 3 c)) (B)) ((= b (+ -3 (* -4/3 c))) (A B))) ()))
|#

(test-solver
 (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
       (make-equation '(- 3 (- a b))  (list 'B)))
 '(a b c))
#|
(underdetermined
 (()
  (a)
  (((= b (+ -3 a)) (A B))
   ((= c (/ (+ (* 2 (expt a 2)) (* -3 a)) (+ -4 (* 2 a)))) (A B)))
  ())
 (()
  (b)
  (((= a (+ 3 b)) (B))
   ((= c (/ (+ 9 (* 2 (expt b 2)) (* 9 b)) (+ 2 (* 2 b)))) (A B)))
  ()))
|#

(test-solver
 (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
       (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
       (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
 '(x y z))
#|
(full-solutions (() () (((= x 2) (A B)) ((= y 1) (A B)) ((= z -1/2) (A B C))) ()))
|#

(test-solver
 (list (make-equation '(- 200/3 (/ 1 (+ (/ 1 R1) (/ 1 R2))))  (list 'A))
       (make-equation '(-  1/3 (/ R2 (+ R1 R2)))  (list 'B)))
 '(R1 R2))
#|
(full-solutions
 (()
  ()
  (((= R1 0) (A B (hypothetical (- quadratic -6 600 0))))
   ((= R2 0) (A B (hypothetical (- quadratic -6 600 0)))))
  ())
 (()
  ()
  (((= R1 200) (A B (hypothetical (+ quadratic -6 600 0))))
   ((= R2 100) (A B (hypothetical (+ quadratic -6 600 0)))))
  ()))
|#

(test-solver
 (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
       (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
 '(R1 R2))
#|
(full-solutions
 (()
  ()
  (((= R1 0) (A B (hypothetical (- quadratic -6 600 0))))
   ((= R2 0) (A B (hypothetical (- quadratic -6 600 0)))))
  ())
 (()
  ()
  (((= R1 0) (A B (hypothetical (- quadratic -2 200 0))))
   ((= R2 0) (A B (hypothetical (- quadratic -6 600 0)))))
  ())
 (()
  ()
  (((= R1 0) (A B (hypothetical (- quadratic -6 600 0))))
   ((= R2 0) (A B (hypothetical (- quadratic -2 200 0)))))
  ())
 (()
  ()
  (((= R1 0) (A B (hypothetical (- quadratic -2 200 0))))
   ((= R2 0) (A B (hypothetical (- quadratic -2 200 0)))))
  ())
 (()
  ()
  (((= R1 200) (A B (hypothetical (+ quadratic -2 200 0))))
   ((= R2 100) (A B (hypothetical (+ quadratic -2 200 0)))))
  ())
 (()
  ()
  (((= R1 200) (A B (hypothetical (+ quadratic -6 600 0))))
   ((= R2 100) (A B (hypothetical (+ quadratic -2 200 0)))))
  ())
 (()
  ()
  (((= R1 200) (A B (hypothetical (+ quadratic -2 200 0))))
   ((= R2 100) (A B (hypothetical (+ quadratic -6 600 0)))))
  ())
 (()
  ()
  (((= R1 200) (A B (hypothetical (+ quadratic -6 600 0))))
   ((= R2 100) (A B (hypothetical (+ quadratic -6 600 0)))))
  ()))
|#

;;;***
(test-solver
 (list (make-equation '(- (expt x 2) 1)  (list 'A))
       (make-equation '(- x 1)  (list 'B)))
 '(x))
#|
(full-solutions (() () (((= x 1) (B))) ()))
|#

(test-solver
 (list (make-equation '(- (expt x 2) 1)  (list 'A))
       (make-equation '(- x -1)  (list 'B)))
 '(x))
#|
(full-solutions (() () (((= x -1) (B))) ()))
|#

(test-solver
 (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
       (make-equation '(- (expt y 2) 9) (list 'B))
       (make-equation '(- (- y x) 1) (list 'C)))
 '(x y))
#|
(full-solutions
 (()
  ()
  (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))
   ((= y 3) (B (hypothetical (- quadratic 1 0 -9)))))
  ()))
|#

(test-solver
 (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
       (make-equation '(- (expt y 2) 9) (list 'B))
       (make-equation '(- (- y x) 2) (list 'C)))
 '(x y))
#|
(contradictions
 (7 (B C A (hypothetical (- quadratic 1 -9 20))) ())
 (16 (B C A (hypothetical (+ quadratic 1 -9 20))) ())
 (2 (C A B (hypothetical (- quadratic 1 0 -9))) ())
 (56 (C A B (hypothetical (+ quadratic 1 0 -9))) ())
 (-1
  ((hypothetical (- quadratic 1 0 -9)) B
                                       C
                                       A
                                       (hypothetical (- quadratic 1 -5 6)))
  ())
 (-2
  ((hypothetical (- quadratic 1 0 -9)) B
                                       C
                                       A
                                       (hypothetical (+ quadratic 1 -5 6)))
  ())
 (2 (A C B (hypothetical (- quadratic 1 0 -9))) ())
 (2 (A C B (hypothetical (- quadratic 1 0 -9))) ())
 (-7
  ((hypothetical (+ quadratic 1 0 -9)) B
                                       C
                                       A
                                       (hypothetical (- quadratic 1 -5 6)))
  ())
 (-8
  ((hypothetical (+ quadratic 1 0 -9)) B
                                       C
                                       A
                                       (hypothetical (+ quadratic 1 -5 6)))
  ())
 (56 (A C B (hypothetical (+ quadratic 1 0 -9))) ())
 (56 (A C B (hypothetical (+ quadratic 1 0 -9))) ())
 (-1
  ((hypothetical (- quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (- quadratic 1 0 -9)))
  ())
 (-7
  ((hypothetical (- quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (+ quadratic 1 0 -9)))
  ())
 (7 (B C A (hypothetical (- quadratic 1 -5 6))) ())
 (-1
  ((hypothetical (- quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (- quadratic 1 0 -9)))
  ())
 (-7
  ((hypothetical (- quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (+ quadratic 1 0 -9)))
  ())
 (7 (B C A (hypothetical (- quadratic 1 -5 6))) ())
 (-2
  ((hypothetical (+ quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (- quadratic 1 0 -9)))
  ())
 (-8
  ((hypothetical (+ quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (+ quadratic 1 0 -9)))
  ())
 (16 (B C A (hypothetical (+ quadratic 1 -5 6))) ())
 (-2
  ((hypothetical (+ quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (- quadratic 1 0 -9)))
  ())
 (-8
  ((hypothetical (+ quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (+ quadratic 1 0 -9)))
  ())
 (16 (B C A (hypothetical (+ quadratic 1 -5 6))) ()))
|#

(test-solver
 (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
       (make-equation '(- (expt y 2) z) (list 'B))
       (make-equation '(- (- y x) 2) (list 'C)))
 '(x y))
#|
(parameters-constrained
 ((((+ 16 (* -1 z)) (B C A (hypothetical (- quadratic 1 -5 6))) (z)))
  ()
  (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))
   ((= y 4) (A C (hypothetical (- quadratic 1 -5 6)))))
  ())
 ((((+ 16 (* -1 z)) (B C A (hypothetical (- quadratic 1 -9 20))) (z)))
  ()
  (((= x 2) (A C (hypothetical (- quadratic 1 -9 20))))
   ((= y 4) (A C (hypothetical (- quadratic 1 -9 20)))))
  ())
 ((((+ 20 z (* 9 (sqrt z))) (A C B (hypothetical (- quadratic 1 0 (* -1 z))))
                            (z)))
  ()
  (((= x (+ -2 (* -1 (sqrt z))))
    (B C (hypothetical (- quadratic 1 0 (* -1 z)))))
   ((= y (* -1 (sqrt z))) (B (hypothetical (- quadratic 1 0 (* -1 z))))))
  ())
 ((((+ 20 z (* -9 (sqrt z))) (A C B (hypothetical (+ quadratic 1 0 (* -1 z))))
                             (z)))
  ()
  (((= x (+ -2 (sqrt z))) (B C (hypothetical (+ quadratic 1 0 (* -1 z)))))
   ((= y (sqrt z)) (B (hypothetical (+ quadratic 1 0 (* -1 z))))))
  ())
 ((((+ 25 (* -1 z)) (B C A (hypothetical (+ quadratic 1 -9 20))) (z)))
  ()
  (((= x 3) (A C (hypothetical (+ quadratic 1 -9 20))))
   ((= y 5) (A C (hypothetical (+ quadratic 1 -9 20)))))
  ())
 ((((+ 25 (* -1 z)) (B C A (hypothetical (+ quadratic 1 -5 6))) (z)))
  ()
  (((= x 3) (A (hypothetical (+ quadratic 1 -5 6))))
   ((= y 5) (A C (hypothetical (+ quadratic 1 -5 6)))))
  ()))
|#

(test-solver
 (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
       (make-equation '(- (expt y 2) z) (list 'B))
       (make-equation '(- (- y x) 2) (list 'C)))
 '(x y z))
#|
(full-solutions
 (()
  ()
  (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))
   ((= y 4) (A C (hypothetical (- quadratic 1 -9 20))))
   ((= z 16) (A B C (hypothetical (- quadratic 1 -9 20)))))
  ())
 (()
  ()
  (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))
   ((= y 4) (A C (hypothetical (- quadratic 1 -5 6))))
   ((= z 16) (A B C (hypothetical (- quadratic 1 -9 20)))))
  ())
 (()
  ()
  (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))
   ((= y 4) (A C (hypothetical (- quadratic 1 -9 20))))
   ((= z 16) (A B C (hypothetical (- quadratic 1 -5 6)))))
  ())
 (()
  ()
  (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))
   ((= y 4) (A C (hypothetical (- quadratic 1 -5 6))))
   ((= z 16) (A B C (hypothetical (- quadratic 1 -5 6)))))
  ())
 (()
  ()
  (((= x 3) (A (hypothetical (+ quadratic 1 -5 6))))
   ((= y 5) (A C (hypothetical (+ quadratic 1 -5 6))))
   ((= z 25) (A B C (hypothetical (+ quadratic 1 -5 6)))))
  ())
 (()
  ()
  (((= x 3) (A (hypothetical (+ quadratic 1 -5 6))))
   ((= y 5) (A C (hypothetical (+ quadratic 1 -9 20))))
   ((= z 25) (A B C (hypothetical (+ quadratic 1 -5 6)))))
  ())
 (()
  ()
  (((= x 3) (A (hypothetical (+ quadratic 1 -5 6))))
   ((= y 5) (A C (hypothetical (+ quadratic 1 -5 6))))
   ((= z 25) (A B C (hypothetical (+ quadratic 1 -9 20)))))
  ())
 (()
  ()
  (((= x 3) (A (hypothetical (+ quadratic 1 -5 6))))
   ((= y 5) (A C (hypothetical (+ quadratic 1 -9 20))))
   ((= z 25) (A B C (hypothetical (+ quadratic 1 -9 20)))))
  ()))
|#

(test-solver
 (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A)))
 '(x))
#|
(full-solutions
 (() () (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))) ())
 (() () (((= x 3) (A (hypothetical (+ quadratic 1 -5 6))))) ()))
|#

(test-solver
 (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
       (make-equation '(+ (expt x 2) (* -7 x) 10)  (list 'B)))
 '(x))
#|
(full-solutions
 (() () (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))) ())
 (() () (((= x 2) (B (hypothetical (- quadratic 1 -7 10))))) ()))
|#

(test-solver
 (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
       (make-equation '(+ (expt x 2) (* a x) 10)  (list 'B)))
 '(a x))
#|
(full-solutions
 (()
  ()
  (((= a -7) (A B (hypothetical (- quadratic 1 -5 6))))
   ((= x 2) (A (hypothetical (- quadratic 1 -5 6)))))
  ())
 (()
  ()
  (((= a -19/3) (A B (hypothetical (+ quadratic 1 -5 6))))
   ((= x 3) (A (hypothetical (+ quadratic 1 -5 6)))))
  ()))
|#

(test-solver
 (list (make-equation '(- 2 (sqrt (+ x 1)))  (list 'A)))
 '(x))
#|
(full-solutions (() () (((= x 3) (A))) ()))
|#

(test-solver
 (list (make-equation '(- 2 (acos (sqrt (+ x 1))))  (list 'A)))
 '(x))
#|
(full-solutions (() () (((= x (* -1 (expt (sin 2) 2))) (A))) ()))
|#

(test-solver
 (list (make-equation '(+ 1 x (square x))  (list 'A)))
 '(x))
#|
(full-solutions
 (()
  ()
  (((= x -.5000000000000001+.8660254037844387i)
    (A (hypothetical (- quadratic 1 1 1)))))
  ())
 (()
  ()
  (((= x -1/2-.8660254037844386i)
    (A (hypothetical (+ quadratic 1 1 1)))))
  ()))
|#


(test-solver
 (list (make-equation '(+ 1 x (square x))  (list 'A))
       (make-equation '(+ (square y) 3) (list 'B))
       (make-equation '(- (* 2 x) (-  y 1)) (list 'C)))
 '(x y))
#|
(full-solutions
 (()
  ()
  (((= x -1/2+.8660254037844388i) (A (hypothetical (- quadratic 1 1 1))))
   ((= y +1.7320508075688776i) (B (hypothetical (- quadratic 1 0 3)))))
  ())
 (()
  ()
  (((= x -1/2-.8660254037844386i) (A (hypothetical (+ quadratic 1 1 1))))
   ((= y -1.7320508075688772i) (B (hypothetical (+ quadratic 1 0 3)))))
  ()))
|#

|#

