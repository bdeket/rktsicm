#lang racket/base

(provide (all-defined-out))

(require racket/syntax
         "../rkt/racket-help.rkt"
         )

(define *suppressed-argument-list* (make-parameter '()))

(define *suppressed-argument-list-counter* (make-parameter 0))

(define *rename-list* (make-parameter #hash()))

(define (suppress-arguments arguments)
  (let ((n (+ (length (*suppressed-argument-list*)) 1)))
    (*suppressed-argument-list-counter*
     (+ (*suppressed-argument-list-counter*) 1))
    (*suppressed-argument-list*
     (cons (cons arguments
                 (format-symbol "args.~a"
                                (*suppressed-argument-list-counter*)))
           (*suppressed-argument-list*)))
    n))

(define (show-suppressed-arguments)
  (map (lambda (al)
             `(,(cdr al) = ,@(car al)))
           (*suppressed-argument-list*)))

(define (clear-arguments)
  (*suppressed-argument-list* '())
  (*suppressed-argument-list-counter* 0)
  (*rename-list* #hash())
  0)

(define (arg-suppressor expression)
  (if (pair? expression)
      (let ((v (assoc (cdr expression) (*suppressed-argument-list*))))
        (if v
            (list (arg-suppressor (car expression)) (cdr v))
            (cons (arg-suppressor (car expression))
                  (arg-suppressor (cdr expression)))))
      expression))

(define (arg-suppressor+ expression)
  (if (pair? expression)
      (let ((v (assoc (cdr expression) (*suppressed-argument-list*))))
        (if v
            (arg-suppressor+ (car expression))
            (cons (arg-suppressor+ (car expression))
                  (arg-suppressor+ (cdr expression)))))
      expression))

(define (rename-part arg rep) (*rename-list* (hash-set (*rename-list*) arg rep)))

(define rename-expression
  (let ([g (gensym)])
    (λ (expr)
      (define rep (hash-ref (*rename-list*) expr g))
      (cond
        [(eq? rep g)
         (if (pair? expr)
             (map rename-expression expr)
             expr)]
        [else rep]))))

#|
;;; For example

(let ((t 't) (xy (up 'x 'y)) (uv (up 'r 's)))
  (* (((partial 2) Hp) (up t uv (- (((partial 2) F1) t xy uv))))
     (((partial 2) ((partial 1) F1)) 't xy uv)))
#|
(down
 (+
  (*
   (((partial 1 0) ((partial 2 1) F1)) t (up x y) (up r s))
   (((partial 2 1) Hp)
    (up t
	(up r s)
	(down (* -1 (((partial 2 0) F1) t (up x y) (up r s)))
	      (* -1 (((partial 2 1) F1) t (up x y) (up r s)))))))
  ...mess...)
 ...mess...)
|#

;;; We choose arguments to suppress:

(suppress-arguments '((up t
			  (up r s)
			  (down (* -1 (((partial 2 0) F1) t (up x y) (up r s)))
				(* -1 (((partial 2 1) F1) t (up x y) (up r s)))))))
#| 1 |#

(suppress-arguments '(t (up x y) (up r s)))
#| 2 |#


;;; Now look at the pretty result:

(let ((t 't) (xy (up 'x 'y)) (uv (up 'r 's)))
  (* (((partial 2) Hp) (up t uv (- (((partial 2) F1) t xy uv))))
     (((partial 2) ((partial 1) F1)) 't xy uv)))
#|
(down
 (+ (* (((partial 2 0) Hp) args.1) (((partial 1 0) ((partial 2 0) F1)) args.2))
    (* (((partial 2 1) Hp) args.1) (((partial 1 0) ((partial 2 1) F1)) args.2)))
 (+ (* (((partial 2 0) Hp) args.1) (((partial 1 1) ((partial 2 0) F1)) args.2))
    (* (((partial 2 1) Hp) args.1) (((partial 1 1) ((partial 2 1) F1)) args.2))))
|#

(show-suppressed-arguments)
((args.2 = t (up x y) (up r s))
 (args.1 = (up t
	       (up r s)
	       (down (* -1 (((partial 2 0) F1) t (up x y) (up r s)))
		     (* -1 (((partial 2 1) F1) t (up x y) (up r s)))))))
|#
