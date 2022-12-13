#lang racket/base

(provide (all-defined-out)
         append-map
         make-list
         take drop split-at
         partition
         )

(require (only-in "../rkt/glue.rkt" if
                  fix:+ fix:= fix:- int:> int:+
                  cons* there-exists? every)
         "../rkt/define.rkt"
         racket/list)

;;bdk;; start original file

;;;; List utilities


(define (variable<? x y)
  (symbol<? x y))

#; ;;bdk;; this changes order, I don't thrust this: (* A B C) <> (* C (* B A)) for matrices...
(define (reduce fct in lst)
  (cond
    [(null? lst) in]
    [else
     (let loop ([a (car lst)][lst (cdr lst)])
       (cond
         [(null? lst) a]
         [else (loop (fct (car lst) a) (cdr lst))]))]))
;(reduce list '() '(1 2 3 4)) => '(4 (3 (2 1)))
(define (reduce-left fct in lst)
  (cond
    [(null? lst) in]
    [else
     (let loop ([a (car lst)][lst (cdr lst)])
       (cond
         [(null? lst) a]
         [else (loop (fct a (car lst)) (cdr lst))]))]))
(define reduce reduce-left)
;(reduce-left list '() '(1 2 3 4)) => '(((1 2) 3) 4)

(define (reduce-right fct in lst)
  (cond
    [(null? lst) in]
    [else
     (let loop ([a (car lst)][lst (cdr lst)])
       (cond
         [(null? lst) a]
         [else
          (fct a (loop (car lst)(cdr lst)))]))]))
;(reduce-right list '() '(1 2 3 4)) => (1 (2 (3 4)))

(define (sublist lst start end) (take (drop lst start) (- end start)))

;;; Ok to pass it an improper list
(define (safe-map f pairs)
  (cond ((null? pairs) '())
	((pair? pairs)
	 (cons (f (car pairs))
	       (safe-map f (cdr pairs))))
	(else (f pairs))))

(define (count-elements p? l)
  (let loop ((count 0) (l l))
    (cond ((null? l) count)
          ((p? (car l)) (loop (fix:+ count 1) (cdr l)))
          (else (loop count (cdr l))))))

(define (find-first pred lst)
  (cond ((null? lst) #f)
	((pred (car lst)) (car lst))
	(else (find-first pred (cdr lst)))))

(define (countsymbols exp)
  (cond ((pair? exp)
	 (fix:+ (countsymbols (car exp))
		(countsymbols (cdr exp))))
	((symbol? exp) 1)
	(else 0)))

(define (butlast l)
  (if (null? (cdr l)) 
      '()
      (cons (car l)
            (butlast (cdr l)))))

(define (last-pair lst)
  (if (and (pair? lst) (pair? (cdr lst)))
      (last-pair (cdr lst))
      lst))

(define (except-last-pair lst)
  (if (and (pair? lst) (pair? (cdr lst)))
      (cons (car lst) (except-last-pair (cdr lst)))
      '()))

(define (lset-adjoin iseq? lst itm)
  (if (member itm lst iseq?)
      lst
      (cons itm lst)))

(define (lset= comp? s1 s2)
  (define (inner s1 s2)
    (cond
      [(and (null? s1) (null? s2)) #t]
      [(or (null? s1) (null? s2)) #f]
      [else
       (define cur (car s1))
       (define-values (eqs ots)
         (partition (λ (x) (comp? cur x)) s2))
       (cond
         [(null? eqs) #f]
         [else
          (inner (cdr s1)
                 (append (cdr eqs) ots))])]))
  (inner s1 s2))

(define (lset-union comp? s1 s2)
  (cond
    [(empty? s2) s1]
    [(empty? s1) s2]
    [else
     (let inner ([s2 s2]
                 [rst s1])
    (cond
      [(empty? s2) rst]
      [(member (car s2) rst comp?)
       (inner (cdr s2) rst)]
      [else
       (inner (cdr s2) (cons (car s2) rst))]))]))

(define (lset-difference comp? s1 s2)
  (let lp ([s1 s1])
    (cond
      [(null? s1) s1]
      [(member (car s1) s2 comp?) (lp (cdr s1))]
      [else (cons (car s1) (lp (cdr s1)))])))

(define (lset-intersection comp? s1 s2)
  (remove* (remove* s2 s1 comp?) s1 comp?))

(define (last l)
  (car (last-pair l)))

(define (list-transpose l)
  (apply map list l))

(define (list-index-of x lst)
  (cond ((null? lst)
	 (error "Not in list -- LIST-INDEX-OF" x))
	((equal? x (car lst)) 0)
	(else (fix:+ (list-index-of x (cdr lst)) 1))))

(define (delete-nth n list)
  (if (fix:= n 0)
      (cdr list)
      (cons (car list)
	    (delete-nth (fix:- n 1) (cdr list)))))

(define ((list:elementwise proc) . lists)
  (apply map proc lists))

;;; MAP-DISTINCT-PAIRS APPLYs a procedure, F, to every distinct pair
;;; of values chosen from the list, M, producing a list of the
;;; results.

(define (map-distinct-pairs f lst)
  (map (lambda (p) (apply f p))
       (distinct-pairs lst)))

(define (distinct-pairs lst)
  (if (null? lst)
      '()
      (let ((f (car lst))
	    (r (distinct-pairs (cdr lst))))
	(let loop ((left (cdr lst)))
	  (if (null? left)
	      r
	      (cons (list f (car left))
		    (loop (cdr left))))))))

(define (for-each-distinct-pair proc list)
  (if (not (null? list))
      (let loop ((first (car list)) (rest (cdr list)))
	(for-each (lambda (other-element)
		    (proc first other-element))
		  rest)
	(if (not (null? rest))
	    (loop (car rest) (cdr rest))))))


(define ((fringe-smaller-than? n) expr)
  (define (walk expr count next)
    (cond ((int:> count n) #f)
	  ((pair? expr)
	   (walk (car expr) count
		 (lambda (count)
		   (walk (cdr expr) count next))))
	  ((null? expr)
	   (next count))
	  (else
	   (next (int:+ count 1)))))
  (walk expr 0 (lambda (count) count)))

#|
((fringe-smaller-than? 3) '())
;Value: 0

((fringe-smaller-than? 100) '(a (b c) d))
;Value: 4

((fringe-smaller-than? 3) '(a (b c) d))
;Value: #f
|#

(define (split-list list predicate recvr)
  (let split ((list list)
	      (recvr recvr))
    (if (not (pair? list))
	(recvr '() '())
	(split (cdr list)
	       (lambda (win lose)
		 (if (predicate (car list))
		     (recvr (cons (car list) win)
			    lose)
		     (recvr win
			    (cons (car list) lose))))))))

(define (find-infimum list predicate)
  (if (null? list)
      (error "find-infimum: empty list" list))
  (let loop ((current (car list))
	     (left (cdr list)))
    (cond ((null? left)
	   current)
	  ((predicate (car left) current)
	   (loop (car left) (cdr left)))
	  (else
	   (loop current (cdr left))))))

(define (subst new old where)
  (cond ((eq? where old)
	 new)
	((not (pair? where))
	 where)
	(else
	 (cons (subst new old (car where))
	       (subst new old (cdr where))))))

(define (delq-once element list)
  (cond ((null? list)
	 '())
	((eq? (car list) element)
	 (cdr list))
	(else
	 (cons (car list)
	       (delq-once element (cdr list))))))
#|
;;; Grrrr dumb!
(define (substitute-multiple expression dictionary)
  (define (walk e)
    (if (pair? e)
	(cons (walk (car e)) (walk (cdr e)))
	(let ((v (assoc e dictionary)))
	  (if v
	      (cadr v)
	      e))))
  (walk expression))
|#

(define (substitute-multiple expression dictionary)
  (define (walk e)
    (let ((v (assoc e dictionary)))
      (if v
          (cadr v)
          (if (pair? e)
              (cons (walk (car e)) (walk (cdr e)))
              e))))
  (walk expression))

;;;; Mapping and reducing

;; Important: All of these are iterative, so they won't run out of stack!

(define (map&reduce procedure combiner null-value list1 #:optional list2 . lists)
  ;; (reduce combiner null-value (map procedure list1 list2 . lists))
  (cond ((default-object? list2)
	 (let loop ((result null-value)
		    (l list1))
	   (if (null? l)
	       result
	       (loop (combiner (procedure (car l))
			       result)
		     (cdr l)))))
	((null? lists)
	 (let loop ((result null-value)
		    (l1 list1)
		    (l2 list2))
	   (if (or (null? l1) (null? l2))
	       result
	       (loop (combiner (procedure (car l1) (car l2))
			       result)
		     (cdr l1)
		     (cdr l2)))))
	(else
	 (let loop ((result null-value)
		    (l (cons* list1 list2 lists)))
	   (if (there-exists? l null?)
	       result
	       (loop (combiner (apply procedure (map car l))
			       result)
		     (map cdr l)))))))

(define (%append x y)
  (if (null? x)
      y
      (%reverse! (%reverse x '()) y)))
  
(define (%reverse! l #:optional tail)
  (error "TODO")#;
  (let loop ((current l)
             (new-cdr (if (default-object? tail)
                          '()
                          tail)))
    (if (pair? current)
        (let ((next (cdr current)))
          (set-cdr! current new-cdr)
          (loop next current))
        (begin
          (if (not (null? current))
              (error "%REVERSE!: Argument not a list" l))
          new-cdr))))

(define (%reverse ol #:optional tail)
  (let loop ((l ol)
             (accum (if (default-object? tail)
                        '()
                        tail)))
    (cond ((pair? l)
	   (loop (cdr l)
		 (cons (car l) accum)))
	  ((null? l)
	   accum)
	  (else
	   (error "%REVERSE: Argument not a list" ol)))))  

(define (%map f ol1 #| #:optional ol2 . rest |#)
  ;; Important: The circular list hack for multi-argument
  ;; map does not work here.
  (error "TODO")#;
  (cond ((default-object? l2)
	 (%map-1 f ol1))
	((null? rest)
	 (%map-2 f ol1 ol2))
	(else
	 (let outer ((result '())
		     (ls (reverse (%map-1 reverse (cons* ol1 ol2 rest)))))
	   (cond ((pair? (car ls))
		  (let inner ((args (list (caar ls)))
			      (next (list (cdar ls)))
			      (rest (cdr ls)))
		    (cond ((null? rest)
			   (outer (cons (apply f args) result)
				  (reverse! next)))
			  ((not (pair? (car rest)))
			   (error "%map: Arguments have different lengths"
				  (cons* ol1 ol2 rest)))
			  (else
			   (inner (cons (caar rest) args)
				  (cons (cdar rest) next)
				  (cdr rest))))))
		 ((there-exists? ls (lambda (x) (not (null? x))))
		  (error "%map:Arguments have different lengths"))
		 (else
		  result))))))

(define-integrable (%map-1 f ol)
  (let loop ((result '()) (l1 (reverse ol)))
    (cond ((pair? l1)
	   (loop (cons (f (car l1)) result)
		 (cdr l1)))
	  ((null? l1)
	   result)
	  (else
	   (error "%map: Argument not a list" ol)))))      

(define-integrable (%map-2 f ol1 ol2)
  (let loop ((result '())
	     (l1 (reverse ol1))
	     (l2 (reverse ol2)))
    (cond ((and (pair? l1) (pair? l2))
	   (loop (cons (f (car l1) (car l2)) result)
		 (cdr l1)
		 (cdr l2)))
	  ((and (null? l1) (null? l2))
	   result)
	  (else
	   (error "%map: Arguments have different lengths"
		  ol1 ol2)))))
