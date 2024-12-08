#lang racket/base

(provide samritchie-memoizer clear-memoizer-tables hash-memoize-1arg linear-memoize
         (except-out (struct-out memoizer) memoizer-of make-memoizer) memoizer-org the-memoizers
         weak-find-equal-args? weak-find-eqv-args? weak-find-eq-args?)
(module+ ALL (provide (all-from-out (submod ".."))))

(require "../rkt/glue.rkt"
         (only-in "../rkt/define.rkt" define default-object?)
         "weak.rkt"
         "eq-properties.rkt"
         )

(struct memoizer (fun of type max-table-size info reset) #:constructor-name make-memoizer)
(define (memoizer-org M) (weak-box-value (memoizer-of M) gc-reclaimed-object))

(define (add-memoizer! M F info reset [type #f][T -1])
  (define M* (procedure-reduce-arity-mask
              (if (apply-hook? F)
                  (make-apply-hook M (apply-hook-extra F))
                  M)
              (procedure-arity-mask F)
              (string->symbol (format "mem:~a" (or (object-name F) "")))))
  (eq-clone! F M*)
  (define MS (make-memoizer M* (make-weak-box F) type T info reset))
  (hash-set! (hash-ref! (car *memoizers*) F (make-hasheq)) type MS)
  (hash-set! (cdr *memoizers*) M* MS)
  M*)

(define (the-memoizers) (hash-values (cdr *memoizers*)))
;;bdk;; start original file

;;;; Memoizers


;;; A weak alist of memoized functions.

(define *memoizers* (cons (make-ephemeron-hasheq) (make-ephemeron-hasheq)))

;;brm;;(define (memoizer-gc-daemon)
;;brm;;  (set! *memoizers*
;;brm;;        (clean-weak-alist *memoizers*))
;;brm;;  'done)

;;brm;;(add-gc-daemon! memoizer-gc-daemon)

;;;(define *auditing-memoizers* #f)
(define *auditing-memoizers* #t)

;;brm;;(define (show-memoizer-statistics)
;;brm;;  (for-each (lambda (m)
;;brm;;              (let ((f (weak-car m)) (s ((cadr (weak-cdr m)))))
;;brm;;                (if (not (gc-reclaimed-object? f))
;;brm;;                    (pp `(,(car s)
;;brm;;                          ,(cadr s)
;;brm;;                          ,(function-expression (cadddr (weak-cdr m))
;;brm;;                                                f))))))
;;brm;;            *memoizers*)
;;brm;;  'done)

;;brm;;(define (function-expression f memo-f)
;;brm;;  (or (object-name memo-f
;;brm;;                   generic-environment
;;brm;;                   rule-environment
;;brm;;                   numerical-environment
;;brm;;                   scmutils-base-environment)
;;brm;;      (procedure-name f)))

  
(define (clear-memoizer-tables)
  (for ([M (in-hash-values (cdr *memoizers*))]
        #:unless (gc-reclaimed-object? (memoizer-fun M)) )
    ((memoizer-reset M)))
  'done)

;;; Single-argument linear memoizer.  Can use weak alists for
;;; single-argument keys.

(define (linear-memoize-1arg f #:optional max-table-size finder)
  (let ((max-table-size			;set to 0 for no limit
	 (if (default-object? max-table-size)
	     12
	     max-table-size))
	(finder
	 (if (default-object? finder) weak-find-equal? finder))
        (table '())
        (memo-hits 0)
        (memo-misses 0))
    (let ((info
           (lambda ()
             (list memo-hits memo-misses table)))
          (reset
           (lambda ()
             (set! memo-hits 0)
             (set! memo-misses 0)
             (set! table '())))
          (memo-f
           (lambda (x)
             (let ((seen (finder x table)))
               (if seen
                   (begin (if *auditing-memoizers*
                            (set! memo-hits (int:+ memo-hits 1)))
                          seen)
                   (let ((ans (f x)))
                     (if *auditing-memoizers*
                       (set! memo-misses (int:+ memo-misses 1)))
                     (set! table
                           (purge-list (cons (weak-cons x ans) table)
                                       max-table-size))
                     ans))))))
      (add-memoizer! memo-f f info reset 'lin-mem-1arg max-table-size))))

;;; A general linear-time memoizer for functions.  In this case the
;;; arg lists are ALWAYS unprotected, so we cannot use weak pairs in
;;; the alist structure here.  However, we can use weak lists as
;;; arglists.

(define (linear-memoize f #:optional max-table-size finder)
  (let ((max-table-size			;set to 0 for no limit
	 (if (default-object? max-table-size)
	     12
	     max-table-size))
	(finder
	 (if (default-object? finder) weak-find-equal-args? finder))
        (table '())
        (memo-hits 0)
        (memo-misses 0))
    (let ((info
           (lambda ()
             (list memo-hits memo-misses table)))
          (reset
           (lambda ()
             (set! memo-hits 0)
             (set! memo-misses 0)
             (set! table '())))
          (memo-f
           (lambda x
             (let ((seen (finder x table)))
               (if seen
                   (begin (if *auditing-memoizers*
                            (set! memo-hits (int:+ memo-hits 1)))
                          seen)
                   (let ((ans (apply f x)))
                     (if *auditing-memoizers*
                       (set! memo-misses (int:+ memo-misses 1)))
                     (set! table
                           (purge-list (cons (cons (list->weak-list x)
                                                   ans)
                                             table)
                                       max-table-size))
                     ans))))))
      (add-memoizer! memo-f f info reset 'lin-mem max-table-size))))

;;; Equality of arguments in argument lists or weak argument lists.

(define (same-args? same?)
  (define (safe-car x)
    (if (weak-pair? x) (weak-car x) (car x)))
  (define (safe-cdr x)
    (if (weak-pair? x) (weak-cdr x) (cdr x)))
  (define (the-test args1 args2)
    (cond ((null? args1)
           (cond ((null? args2) #t)
                 (else #f)))
          ((null? args2) #f)
          ((same? (safe-car args1) (safe-car args2))
           (the-test (safe-cdr args1) (safe-cdr args2)))
          (else #f)))
  the-test)

(define equal-args? (same-args? equal?))

(define eqv-args? (same-args? eqv?))

(define eq-args? (same-args? eq?))


(define weak-find-equal-args? (weak-finder equal-args?))

(define weak-find-eqv-args? (weak-finder eqv-args?))

(define weak-find-eq-args? (weak-finder eq-args?))

;;; The following memoizers use hash tables

(define *not-seen* (list (gensym '*not-seen*)))


;;; Single argument hash memoizer.  Can use weak table here.

(define (hash-memoize-1arg f)
  (let ((table) (memo-hits) (memo-misses))
    (let ((info
           (lambda ()
             (list memo-hits memo-misses table)))
          (reset
           (lambda ()
             (set! memo-hits 0)
             (set! memo-misses 0)
             (set! table (make-weak-hash))))
          (memo-f
           (lambda (x)
             (let ((seen (hash-table/get table x *not-seen*)))
               (if (not (eq? seen *not-seen*))
                   (begin (if *auditing-memoizers*
                            (set! memo-hits (int:+ memo-hits 1)))
                          seen)
                   (let ((ans (f x)))
                     (if *auditing-memoizers*
                       (set! memo-misses (int:+ memo-misses 1)))
                     (hash-table/put! table x ans)
                     ans))))))
      (reset)
      (add-memoizer! memo-f f info reset 'hash-mem-1arg))))

;;; A general hash memoizer for functions.  In this case the arg lists
;;; are ALWAYS unprotected, so we cannot use a weak table here.

(define (hash-memoize f)
  (let ((table) (memo-hits) (memo-misses))
    (let ((info
           (lambda ()
             (list memo-hits memo-misses table)))
          (reset
           (lambda ()
             (set! memo-hits 0)
             (set! memo-misses 0)
             (set! table (make-hash))))
          (memo-f
           (lambda x
             (let ((seen (hash-table/get table x *not-seen*)))
               (if (not (eq? seen *not-seen*))
                   (begin (if *auditing-memoizers*
                            (set! memo-hits (int:+ memo-hits 1)))
                          seen)
                   (let ((ans (apply f x)))
                     (if *auditing-memoizers*
                       (set! memo-misses (int:+ memo-misses 1)))
                     (hash-table/put! table x ans)
                     ans))))))
      (reset)
      (add-memoizer! memo-f f info reset 'hash-mem))))


;;; To install and remove memoizers on named procedures

;;brm;;(define (memoize-procedure! name #:optional memo-type environment)
;;brm;;  (assert (symbol? name))
;;brm;;  (if (default-object? environment)
;;brm;;      (set! environment (nearest-repl/environment))
;;brm;;      (assert (environment? environment)))
;;brm;;  (assert (environment-bound? environment name))
;;brm;;  (if (default-object? memo-type)
;;brm;;      (set! memo-type 'linear)
;;brm;;      (assert (memq memo-type '(linear hash))))
;;brm;;  (if (environment-bound? environment '*memoized-procedures*)
;;brm;;      (if (assq name (eval '*memoized-procedures* environment))
;;brm;;          (begin (warn name "rememoizing!")
;;brm;;                 (unmemoize-procedure! name environment))))
;;brm;;  (let ((proc (eval name environment)))
;;brm;;    (assert (procedure? proc))
;;brm;;    (let ((arity (procedure-arity proc)))
;;brm;;      (let ((memoized-procedure
;;brm;;             (cond ((and (fix:= (car arity) 0) ;(0 . 0)
;;brm;;                         (fix:= (cdr arity) 0))
;;brm;;                    (let ((ran? #f) (value))
;;brm;;                      (lambda ()
;;brm;;                        (if ran?
;;brm;;                            value
;;brm;;                            (begin
;;brm;;                              (set! value (proc))
;;brm;;                              (set! ran? #t)
;;brm;;                              value)))))
;;brm;;                   ((and (fix:= (car arity) 1) ;(1 . 1)
;;brm;;                         (fix:= (cdr arity) 1))
;;brm;;                    (case memo-type
;;brm;;                      ((linear) (linear-memoize-1arg proc))
;;brm;;                      ((hash) (hash-memoize-1arg proc))))
;;brm;;                   (else
;;brm;;                    (case memo-type
;;brm;;                      ((linear) (linear-memoize proc))
;;brm;;                      ((hash) (hash-memoize proc)))))))	
;;brm;;        (if (not (environment-bound? environment
;;brm;;                                     '*memoized-procedures*))
;;brm;;            (environment-define environment
;;brm;;                                '*memoized-procedures*
;;brm;;                                (list (cons name proc)))
;;brm;;            (environment-assign! environment
;;brm;;                                 '*memoized-procedures*
;;brm;;                                 (cons (cons name proc)
;;brm;;                                       (eval '*memoized-procedures*
;;brm;;                                             environment))))
;;brm;;        (environment-assign! environment
;;brm;;                             name
;;brm;;                             memoized-procedure)
;;brm;;        'done))))

;;brm;;(define (unmemoize-procedure! name #:optional environment)
;;brm;;  (assert (symbol? name))
;;brm;;  (if (default-object? environment)
;;brm;;      (set! environment (nearest-repl/environment))
;;brm;;      (assert (environment? environment)))
;;brm;;  (assert (environment-bound? environment name))
;;brm;;  (assert (environment-bound? environment '*memoized-procedures*))
;;brm;;  (let ((vcell (assq name (eval '*memoized-procedures* environment))))
;;brm;;    (assert vcell)
;;brm;;    (environment-assign! environment (car vcell) (cdr vcell))
;;brm;;    (environment-assign! environment
;;brm;;                         '*memoized-procedures*
;;brm;;                         (delete vcell
;;brm;;                                 (eval '*memoized-procedures* environment)))
;;brm;;    'done!))

;;; Added by GJS on 11 Nov 2021, I was frustrated that my memoizers never
;;; seemed to help much...

(define (n-dimensional-table)

  (define (make-table)
    (make-1d-table))

  (define the-table (make-table))

  (define *not-found* (list '*not-found*))

  (define (not-found? x)
    (eq? *not-found* x))

  ;; No keys in key list can go away while in these procedures.
  (define (fetch keys)
    (let lp ((keys keys) (table the-table))
      (if (null? keys)
          table
          (if (1d-table? table)
              (let ((v
                     (1d-table/get table
                                   (car keys)
                                   *not-found*)))
                (if (eq? v *not-found*)
                    v
                    (lp (cdr keys) v)))
              *not-found*))))

  (define (store! value keys)      
    (let lp ((keys keys) (table the-table))
      (if (null? (cdr keys))
          (if (1d-table? table)
              (1d-table/put! table (car keys) value)
              (begin (error "hit end") 'avoid-tail-here))
          (let ((v
                 (1d-table/get table
                               (car keys)
                               *not-found*)))
            (if (eq? v *not-found*)
                (let ((new (make-table)))
                  (1d-table/put! table (car keys) new)
                  (lp (cdr keys) new))
                (lp (cdr keys) v))))))

  (define (me m)
    (case m
      ((fetch) fetch)
      ((store!) store!)
      ((not-found?) not-found?)
      ((table) the-table)
      (else
       (error "Unknown message: n-dimensional-table" m))))
  me)

;;; Added by GJS on 14 Nov 2021, I was frustrated that my memoizers never
;;; seemed to help much...  This one kills GC with ephemerons!

;; (define (n-dimensional-table)

;;   (define (make-table)
;;     ;;  (make-strong-eq-hash-table)
;;     ;;  (make-key-weak-eq-hash-table)
;;     ;;  (make-key-ephemeral-eq-hash-table)
;;     ((hash-table-constructor
;;       (make-hash-table-type eq?-hash-mod eq? #t
;;                             hash-table-entry-type:key&datum-weak)))
;;     )

;;   (define the-table (make-table))

;;   (define *not-found* (list '*not-found*))

;;   (define (not-found? x)
;;     (eq? *not-found* x))

;;   ;; No keys in key list can go away while in these procedures.
;;   (define (fetch keys)
;;     (let lp ((keys keys) (table the-table))
;;       (if (null? keys)
;;           table
;;           (if (hash-table? table)
;;               (let ((v
;;                      (hash-table-ref/default table
;;                                              (car keys)
;;                                              *not-found*)))
;;                 (if (eq? v *not-found*)
;;                     v
;;                     (lp (cdr keys) v)))
;;               *not-found*))))

;;   (define (store! value keys)
;;     (let lp ((keys keys) (table the-table))
;;       (if (null? (cdr keys))
;;           (if (hash-table? table)
;;               (hash-table-set! table (car keys) value)
;;               (begin (bkpt "hit end") 'avoid-tail-here))
;;           (let ((v
;;                  (hash-table-ref/default table
;;                                          (car keys)
;;                                          *not-found*)))
;;             (if (eq? v *not-found*)
;;                 (let ((new (make-table)))
;;                   (hash-table-set! table (car keys) new)
;;                   (lp (cdr keys) new))
;;                 (lp (cdr keys) v))))))

;;   (define (me m)
;;     (case m
;;       ((fetch) fetch)
;;       ((store!) store!)
;;       ((not-found?) not-found?)
;;       ((table) the-table)
;;       (else
;;        (error "Unknown message: n-dimensional-table" m))))
;;   me)

(define (memoize-multi-arg-eq procedure)
  (let* ((memory) (lookup) (not-found?) (store!)
         (hits) (misses))

    (define (info)
      (list hits misses memory))

    (define (reset)
      (set! memory (n-dimensional-table))
      (set! lookup (memory 'fetch))
      (set! not-found? (memory 'not-found?))
      (set! store! (memory 'store!))
      (set! hits 0)
      (set! misses 0))

    (define (the-memoized-procedure . args)
      (let ((seen ((memory 'fetch) args)))
        (if (not-found? seen)
            (let ((val (apply procedure args)))
              (store! val args)
              (set! misses (+ misses 1))
              val)
            (begin
              (set! hits (+ hits 1))
              seen))))

    (define (me m)
      (case m
        ((the-memoized-procedure) the-memoized-procedure)
        ((statistics info) (info))
        ((reset) (reset))
        (else (error "unknown message: memoize" m))))

    (set! the-memoized-procedure
          (add-memoizer! the-memoized-procedure procedure info reset 'mem-mul-arg-eq))
    (reset)
    me))

;;; Ignoring statistics stuff
(define (simple-memoize-multi-arg-eq f)
  ((memoize-multi-arg-eq f) 'the-memoized-procedure))

#|
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define foo (memoize-multi-arg-eq fib))

(define fib (foo 'the-memoized-procedure))

(fib 20)
;Value: 6765

(fib 35)
;Value: 9227465

(foo 'statistics)
;Value: (34 36)


;;; now for a 2-argument case

(define (ffib n m)
  (if (< n 2)
      (if (< m 2)
          (+ n m)
          (+ (ffib n (- m 1))
             (ffib n (- m 2))))
      (+ (ffib (- n 1) m)
         (ffib (- n 2) m))))

(ffib 18 18)
;Value: 21607408

(show-time (lambda () (ffib 18 18)))
;process time: 15200 (15200 RUN + 0 GC); real time: 15196
;Value: 21607408

(define ffib (simple-memoize-multi-arg-eq ffib))
;Value: ffib

(show-time (lambda () (ffib 18 18)))
;process time: 0 (0 RUN + 0 GC); real time: 2
;Value: 21607408
|#

;;; For scmutils

(define (make-scmutils-memoizer)
  (let ((memoizers (car *memoizers*))
        (nope (gensym 'not-seen)))
    (define (make-memoizer f)
      (let ((seen (hash-ref (hash-ref memoizers f #hash()) 'mem-mul-arg-eq nope)))
        (if (eq? seen nope)
            (simple-memoize-multi-arg-eq f)
            (memoizer-fun seen))))
    make-memoizer))

(define scmutils-memoize-multi-arg-eq
  (make-scmutils-memoizer))
  
;; (define (scmutils-memoize-multi-arg-eq f)
;;   (let ((mf (simple-memoize-multi-arg-eq f)))
;;     (if (apply-hook? f)                 
;;         (set! mf (make-apply-hook mf (apply-hook-extra f))))
;;     (eq-clone! f mf)
;;     mf))

;; (define (samritchie-memoizer f)
;;   (if *samritchie-memoizing*
;;       (let ((mf 
;;              (scmutils-memoize-multi-arg-eq
;;               (compose scmutils-memoize-multi-arg-eq f))))
;;         (if (apply-hook? f)                 
;;             (set! mf (make-apply-hook mf (apply-hook-extra f))))
;;         (eq-clone! f mf)
;;         mf)
;;       f))

;;; Single layer mechanism.

(define (samritchie-memoizer f)
  (if *samritchie-memoizing*
      (let ((mf (scmutils-memoize-multi-arg-eq f)))
        mf)
      f))

(define *samritchie-memoizing* #t)
