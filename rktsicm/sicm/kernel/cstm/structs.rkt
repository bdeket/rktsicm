#lang s-exp "../extapply.rkt"

(provide (all-defined-out))
(require (only-in "../../rkt/glue.rkt" any
                  fix:= fix:+)
         "express.rkt"
         "../types.rkt"
         "../utils.rkt"
         "../iterat.rkt"
         "generic.rkt"
         "vectors.rkt"
         "matrices.rkt"
         )

;;; Structures are primitive tensor-like objects.  They are
;;; represented as recursive combinations of down vectors and up
;;; vectors, useful for dealing with derivatives of things with
;;; structured inputs and outputs.


#| in TYPES.SCM

(define (up? x)
  ;;(and (pair? x) (eq? (car x) up-type-tag))
  (vector? x))

(define (down? x)
  (and (pair? x)
       (eq? (car x) down-type-tag)))

(define (structure? x)
  (or (up? x) (down? x)))


(define (abstract-structure? x)
  (or (abstract-up? x) (abstract-down? x)))
|#

(define (s:type v)
  (cond ((up? v) up-type-tag)
	((down? v) down-type-tag)
	((abstract-up? v) up-type-tag)
	((abstract-down? v) down-type-tag)
	(else
	 (error "Bad structure -- S:TYPE" v))))

(define (sc:type-predicate v) up-quantity?)
(define (sr:type-predicate v) down-quantity?)


(define (vector->up v)
  ;;(list up-type-tag v)
  v)

(define (vector->down v)
  (list down-type-tag v))

(define (literal-up name size)
  (s:generate size 'up
	      (lambda (i)
		(string->symbol
		 (string-append (symbol->string name)
				"^"
				(number->string i))))))

(define (literal-down name size)
  (s:generate size 'down
	      (lambda (i)
		(string->symbol
		 (string-append (symbol->string name)
				"_"
				(number->string i))))))

(define (s:structure up/down v)
  (case up/down
    ((up contravariant vector up)
     (vector->up v))
    ((down covariant covector down)
     (vector->down v))
    (else
     (error "Bad up/down spec -- S:STRUCTURE"
	    up/down v))))

(define (up->vector v)
  ;;(cadr v)
  v)

(define (down->vector v)
  (cadr v))

(define (s:->vector v)
  (cond ((up? v) (up->vector v))
	((down? v) (down->vector v))
	(else
	 (error "Bad structure -- S:->VECTOR" v))))

(define (up . args)
  (vector->up (list->vector args)))

(define (down . args)
  (vector->down (list->vector args)))


(define (s:opposite v)
  (cond ((up? v) 'down)
	((down? v) 'up)
	(else
	 (error "Bad structure -- S:OPPOSITE" v))))

(define (s:same v)
  (cond ((up? v) 'up)
	((down? v) 'down)
	(else
	 (error "Bad structure -- S:SAME" v))))

(define (s:length v)
  (if (structure? v)
      (vector-length (s:->vector v))
      1))


(define (s:ref v i)
  (if (structure? v)
      (vector-ref (s:->vector v) i)
      (if (fix:= i 0)
	  v
	  (error "Bad structure -- S:REF" v i))))

(define (s:with-substituted-coord v i xi)
  (if (structure? v)
      (s:structure (s:same v)
                   (vector-with-substituted-coord
                    (s:->vector v)
                    i xi))
      (if (fix:= i 0)
          xi
          (error "Bad structure -- S:WITH-SUBSTITUTED-COORD" v i xi))))

(define (s:subst struct newval . chain)
  (s:subst-internal struct newval chain))

(define (s:subst-internal struct newval chain)
  (let lp ((chain chain) (struct struct))
    (if (null? chain)
        newval
        (s:generate (s:length struct)
                    (s:same struct)
                    (lambda (i)
                      (if (fix:= i (car chain))
                          (lp (cdr chain) (s:ref struct i))
                          (s:ref struct i)))))))

(define (s:generate n up/down proc)
  (s:structure up/down (v:generate n proc)))

(define (s:forall p s)
  (let ((n (s:length s)))
    (let lp ((i 1) (ans (p (s:ref s 0))))
      (cond ((fix:= i n) ans)
            ((not ans) ans)
            (else
             (lp (fix:+ i 1) (p (s:ref s i))))))))  

(define (s:select . selectors)
  (let lp ((selectors selectors)
           (ans g:identity)) 
    (if (null? selectors)
        ans
        (lp (cdr selectors)
            (compose (lambda (s)
                       (s:ref s (car selectors)))
                     ans)))))


(define (s:map-chain proc s)
  (define (walk s rev-chain)
    (if (structure? s)
        (s:generate (s:length s)
                    (s:same s)
                    (lambda (i)
                      (walk (s:ref s i)
                            (cons i rev-chain))))
        (proc s (reverse rev-chain))))
  (walk s '()))


;;; S:FRINGE recursively traverses a structure, making up a list of
;;; the terminal elements.

(define (s:fringe s)
  (define (walk s ans)
    (if (structure? s)
        (let ((n (s:length s)))
          (let lp ((i 0) (ans ans))
            (if (fix:= i n)
                ans
                (lp (fix:+ i 1)
                    (walk (s:ref s i) ans)))))
        (cons s ans)))
  (walk s '()))

(define (s:foreach proc s)
  (define (walk s)
    (if (structure? s)
        (let ((n (s:length s)))
          (let lp ((i 0))
            (if (fix:= i n)
                'done
                (begin (walk (s:ref s i))
                       (lp (fix:+ i 1))))))
        (proc s)))
  (walk s))

;;; The following mappers only make sense if, when there is more than
;;; one structure they are all isomorphic.

(define (s:map/r proc . structures)
  (s:map/r/l proc structures))

(define (s:map/r/l proc structures)
  (s:map/l (lambda elements
             (if (structure? (car elements))
                 (s:map/r/l proc elements)
                 (apply proc elements)))
           structures))

(define (s:map proc . structures)
  (s:map/l proc structures))

(define (s:map/l proc structures)
  (if (structure? (car structures))
      (s:generate (s:length (car structures))
                  (s:same (car structures))
                  (lambda (i)
                    (apply proc
                           (map (lambda (s) (s:ref s i))
                                structures))))
      (apply proc structures)))
(define ((s:elementwise proc) . structures)
  (s:map/l proc structures))

(define structure:elementwise s:elementwise)

;;; Is there a part of thing that the predicate is true of?

(define (rexists pred thing)
  (let tlp ((thing thing))
    (cond ((pred thing) #t)
          ((vector? thing)
           (let ((n (vector-length thing)))
             (let lp ((i 0))
               (cond ((fix:= i n) #f)
                     ((tlp (vector-ref thing i)))
                     (else (lp (fix:+ i 1)))))))
          ((structure? thing)
           (let ((n (s:length thing)))
             (let lp ((i 0))
               (cond ((fix:= i n) #f)
                     ((tlp (s:ref thing i)) #t)
                     (else (lp (fix:+ i 1)))))))
          ((matrix? thing)
           (tlp (matrix->array thing)))
          ((pair? thing)
           (cond ((memq (car thing) type-tags)
                  (let ((v (get-property thing 'expression)))
                    (if (not v)
                        #f
                        (tlp v))))
                 ((list? thing)
                  (any tlp thing))
                 (else
                  (or (tlp (car thing))
                      (tlp (cdr thing))))))
          (else #f))))


(define (list->up-structure lst)
  (vector->up
   (list->vector (map matrix->structure lst))))

(define (matrix->structure mat)
  (cond ((down? mat) mat)
        ((up? mat) mat)
        ((matrix? mat)
         (s:generate (m:num-cols mat) 'down
                     (lambda (j)
                       (s:generate (m:num-rows mat) 'up
                                   (lambda (i)
                                     (matrix-ref mat i j))))))
        (else mat)))

(define (up-structure->list s)
  (vector->list (up->vector s)))
