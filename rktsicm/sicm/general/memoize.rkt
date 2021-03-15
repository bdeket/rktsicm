#lang racket/base

(provide (except-out (all-defined-out)))

(require "../rkt/undefined.rkt"
         "../rkt/int.rkt"
         (only-in "../rkt/racket-help.rkt" warn)
         "../rkt/environment.rkt"
         (only-in "../rkt/todo.rkt" pp)
         "assert.rkt"
         "weak.rkt"
         "../kernel/express.rkt"
         )

;;;; Memoizers

;;; A weak alist of memoized functions.

(struct memoizer (memo f max-table-size info reset) #:transparent)

(define *memoizers* '())

(define (add-memoizer! M)
  (set! *memoizers* (cons M *memoizers*)))

(define (memoizer-gc-daemon)
  (set! *memoizers*
        (clean-weak-alist *memoizers*))
  'done)

;(add-gc-daemon! memoizer-gc-daemon)


;;;(define *auditing-memoizers* #f)
(define *auditing-memoizers* #t)

(define (show-memoizer-statistics)
  (for-each (lambda (m)
              (let ((f (weak-car m)) (s ((cadr (weak-cdr m)))))
                (when f
                  (pp (list (car s)
                            (cadr s)
                            (function-expression (cadddr (weak-cdr m))
                                                 f))))))
            *memoizers*)
  'done)

(define (function-expression f memo-f)
  (or (object-name memo-f
                   generic-environment
                   rule-environment
                   numerical-environment
                   scmutils-base-environment)
      (procedure-name f)))

  
(define (clear-memoizer-tables)
  (for-each (lambda (m)
              (let ((f (weak-car m)))
                (when f ((caddr (weak-cdr m))))))
            *memoizers*)
  'done)


;;; Single-argument linear memoizer.  Can use weak alists for
;;; single-argument keys.


(define (linear-memoize-1arg f [max-table-size 12] ;0 for no ;limit
                             [finder weak-find-equal?])
  (let ((table '())
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
                   (begin (when *auditing-memoizers*
                            (set! memo-hits (int:+ memo-hits 1)))
                          seen)
                   (let ((ans (f x)))
                     (when *auditing-memoizers*
                       (set! memo-misses (int:+ memo-misses 1)))
                     (set! table
                           (purge-list (cons (weak-cons x ans) table)
                                       max-table-size))
                     ans))))))
      (set! *memoizers*
            (cons (weak-cons memo-f
                             (list max-table-size info reset f))
                  *memoizers*))
      memo-f)))


;;; A general linear-time memoizer for functions.  In this case the
;;; arg lists are ALWAYS unprotected, so we cannot use weak pairs in
;;; the alist structure here.  However, we can use weak lists as
;;; arglists.

(define (linear-memoize f [max-table-size 12] ;0 for no limit
                        [finder weak-find-equal-args?])
  (let ((table '())
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
                   (begin (when *auditing-memoizers*
                            (set! memo-hits (int:+ memo-hits 1)))
                          seen)
                   (let ((ans (apply f x)))
                     (when *auditing-memoizers*
                       (set! memo-misses (int:+ memo-misses 1)))
                     (set! table
                           (purge-list (cons (cons (list->weak-list x)
                                                   ans)
                                             table)
                                       max-table-size))
                     ans))))))
      (set! *memoizers*
            (cons (weak-cons memo-f
                             (list max-table-size info reset f))
                  *memoizers*))
      memo-f)))


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

(define *not-seen* (gensym '*not-seen*))


;;; Single argument hash memoizer.  Can use weak table here.

(define (hash-memoize-1arg f)
  (let ((table undefined-value)
        (memo-hits undefined-value)
        (memo-misses undefined-value))
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
             (let ((seen (hash-ref table x *not-seen*)))
               (if (not (eq? seen *not-seen*))
                   (begin (when *auditing-memoizers*
                            (set! memo-hits (int:+ memo-hits 1)))
                          seen)
                   (let ((ans (f x)))
                     (when *auditing-memoizers*
                       (set! memo-misses (int:+ memo-misses 1)))
                     (hash-set! table x ans)
                     ans))))))
      (reset)
      (set! *memoizers*
            (cons (weak-cons memo-f (list -1 info reset f))
                  *memoizers*))
      memo-f)))


;;; A general hash memoizer for functions.  In this case the arg lists
;;; are ALWAYS unprotected, so we cannot use a weak table here.

(define (hash-memoize f)
  (let ((table undefined-value)
        (memo-hits undefined-value)
        (memo-misses undefined-value))
    (let ((info
           (lambda ()
             (list memo-hits memo-misses table)))
          (reset
           (lambda ()
             (set! memo-hits 0)
             (set! memo-misses 0)
             (set! table ((make-hash)))))
          (memo-f
           (lambda x
             (let ((seen (hash-ref table x *not-seen*)))
               (if (not (eq? seen *not-seen*))
                   (begin (when *auditing-memoizers*
                            (set! memo-hits (int:+ memo-hits 1)))
                          seen)
                   (let ((ans (apply f x)))
                     (when *auditing-memoizers*
                       (set! memo-misses (int:+ memo-misses 1)))
                     (hash-set! table x ans)
                     ans))))))
      (reset)
      (set! *memoizers*
            (cons (weak-cons memo-f (list -1 info reset f))
                  *memoizers*))
      memo-f)))



;;; To install and remove memoizers on named procedures

(define (memoize-procedure! name [memo-type 'linear] [environment (current-namespace)])
  (assert (symbol? name))
  (assert (environment-bound? environment name))
  (assert (memq memo-type '(linear hash)))
  (when (environment-bound? environment '*memoized-procedures*)
    (when (assq name (eval '*memoized-procedures* environment))
      (begin (warn name "rememoizing!")
             (unmemoize-procedure! name environment))))
  (let ((proc (eval name environment)))
    (assert (procedure? proc))
    (let ((arity (procedure-arity proc)))
      (let ((memoized-procedure
             (cond ((equal? arity '(0 . 0))
                    (let ((ran? #f) (value undefined-value))
                      (lambda ()
                        (if ran?
                            value
                            (begin
                              (set! value (proc))
                              (set! ran? #t)
                              value)))))
                   ((equal? arity '(1 . 1))
                    (case memo-type
                      ((linear) (linear-memoize-1arg proc))
                      ((hash) (hash-memoize-1arg proc))))
                   (else
                    (case memo-type
                      ((linear) (linear-memoize proc))
                      ((hash) (hash-memoize proc)))))))	
        (if (not (environment-bound? environment
                                     '*memoized-procedures*))
            (environment-assign! environment
                                 '*memoized-procedures*
                                 (list (cons name proc)))
            (environment-assign! environment
                                 '*memoized-procedures*
                                 (cons (cons name proc)
                                       (eval '*memoized-procedures*
                                             environment))))
        (environment-assign! environment
                             name
                             memoized-procedure)
        'done))))

(define (unmemoize-procedure! name [environment (current-namespace)])
  (assert (symbol? name))
  (assert (environment-bound? environment name))
  (assert (environment-bound? environment '*memoized-procedures*))
  (let ((vcell (assq name (eval '*memoized-procedures* environment))))
    (assert vcell)
    (environment-assign! environment (car vcell) (cdr vcell))
    (environment-assign! environment
                         '*memoized-procedures*
                         (remove vcell
                                 (eval '*memoized-procedures* environment)))
    'done!))
