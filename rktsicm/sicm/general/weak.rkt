#lang racket/base

(provide (all-defined-out)
         (rename-out [ALT-CLEAN-WEAK-ALIST clean-weak-alist])
         gc-reclaimed-object?)

(require "../rkt/fixnum.rkt"
         "../rkt/if.rkt"
         "../rkt/gcreclaimed.rkt")

(define (ALT-CLEAN-WEAK-ALIST W) (purge-list W 0))

;; set-car! is only used to invalidate pairs -> this is solved in purge-list by rechecking
(define set-car! void)

(struct weak-pair (car cdr) #:transparent #:mutable)
(define (weak-cons car cdr) (weak-pair (make-weak-box car) cdr))
(define (weak-car W) (weak-box-value (weak-pair-car W) gc-reclaimed-object))
(define (weak-cdr W) (weak-pair-cdr W))
(define (weak-set-cdr! W v) (set-weak-pair-cdr! W v))
(define (weak-pair/car? W)
  (not (gc-reclaimed-object? (weak-car W))))

(define (list->weak-list lst)
  (if (pair? lst)
      (weak-cons (car lst) (list->weak-list (cdr lst)))
      lst))
(define (weak-list-intact? L)
  (if (weak-pair? L)
      (and (weak-pair/car? L) (weak-list-intact? (weak-cdr L)))
      #t))

;;bdk;; start original file

;;;; Weak list utilities


;;; Looks for obj in a weak list.

(define (get-weak-member obj weak-list)
  (if (null? weak-list)
      #f
      (let ((a (weak-car weak-list)))
        (if (equal? obj a)
            a
            (get-weak-member obj (weak-cdr weak-list))))))


;;; Looks for obj as the key in a weak alist.
;;;  The weak alist has a backbone that is a strong list
;;;    with weak pair entries.

(define (weak-find obj weak-alist)
  (if (null? weak-alist)
      #f
      (let ((pair (car weak-alist)))
        (if pair                        ;not dead pair
            (let ((a (weak-car pair)))
              (if (gc-reclaimed-object? a)
                  (begin (set-car! weak-alist #f)
                         #f)            ;kill this pair
                  (if (equal? obj a)
                      a
                      (weak-find obj
                                 (cdr weak-alist)))))
            (weak-find obj (cdr weak-alist))))))

(define (weak-length weak-list)
  (if (weak-pair? weak-list)
      (fix:+ (weak-length (weak-cdr weak-list)) 1)
      0))

;;; Weak-alist searches.  These scan a weak alist for an object,
;;; returning the associated value if found.  They also clean up the
;;; alist by clobbering out value cells that have lost their key.
;;; These also work for strong alists, but strong alists are not
;;; modified.

(define (weak-finder same?)
  (define (the-finder obj weak-alist)
    (if (null? weak-alist)
        #f
        (let ((pair (car weak-alist)))
          (cond ((weak-pair? pair)
                 (let ((a (weak-car pair)))
                   (if (gc-reclaimed-object? a)
                       (begin (set-car! weak-alist #f)
                              #f)
                       (if (same? obj a)
                           (weak-cdr pair)
                           (the-finder obj
                                       (cdr weak-alist))))))
                ((pair? pair)
                 (let ((a (car pair)))
                   (if (same? obj a)
                       (cdr pair)
                       (the-finder obj (cdr weak-alist)))))
                (else
                 (the-finder obj (cdr weak-alist)))))))
  the-finder)

(define weak-find-equal? (weak-finder equal?))
(define weak-find-eqv? (weak-finder eqv?))
(define weak-find-eq? (weak-finder eq?))

;;; The following clips out dead linkages that have been clobbered by
;;; a weak finder (above).  It also limits the size of the alist to
;;; the maximum size specified, by chopping off the tail.  max-size
;;; must be a positive integer larger than 1.

(define (purge-list lst max-size)
  ;;bdk;; purge-list is only called from within memoize, it is always called with
  ;;bdk;; a (Listof A=(U B=(weak-cons Any Any) C=(cons (weak-list ...) Any))
  ;;bdk;; weak-finder should set-car! to #f any weak link that is gc-ed, but
  ;;bdk;; this only works for A of type B. The other is treated as normal pair
  ;;bdk;; and never set-car!ed to #f so purge can not remove it
  ;;bdk;; instead of relaying on weak-finder to invalidate gc-ed items
  ;;bdk;; purge will retraverse everything
  (let loop ([lst lst][max-size (if (<= max-size 0) +inf.0 max-size)])
    (cond
      [(or (null? lst) (<= max-size 0)) '()]
      [else
       (define A (car lst))
       ((if (if (weak-pair? A)
                (weak-pair/car? A) ;; type B
                (weak-list-intact? (car A))) ;; type C
            (λ (l) (cons A l)) values )
        (loop (cdr lst) (- max-size 1)))])))

;;; Weak list cleanups

(define (clean-weak-list weak-list)
  (let clean-head ((this weak-list))
    (if (weak-pair? this)
        (let ((next (weak-cdr this)))
          (if (gc-reclaimed-object? (weak-car this))
              (clean-head next)
              (begin
                (let clean-tail ((this next) (prev this))
                  (if (weak-pair? this)
                      (let ((next (weak-cdr this)))
                        (if (gc-reclaimed-object? (weak-car this))
                            (begin
                              (weak-set-cdr! prev next)
                              (clean-tail next prev))
                            (clean-tail next this)))))
                this)))
        this)))

;;brm;;(define (clean-weak-alist weak-alist)
;;brm;;  (clean-alist weak-alist
;;brm;;               (lambda (p)
;;brm;;                 (if (not (weak-pair? p))
;;brm;;                     (raise-argument-error 'clean-weak-alist "weak-alist" weak-alist))
;;brm;;                 (not (gc-reclaimed-object? (weak-car p))))))
;;brm;;
;;brm;;(define (clean-subtable-alist alist)
;;brm;;  (clean-alist alist
;;brm;;               (lambda (p)
;;brm;;                 (if (not (pair? p))
;;brm;;                     (raise-argument-error 'clean-subtable-alist "weak-alist" alist))
;;brm;;                 (clean-expression-table (cdr p)))))
;;brm;;
;;brm;;(define (clean-alist alist clean-association)
;;brm;;  (let clean-head ((this alist))
;;brm;;    (if (pair? this)
;;brm;;        (let ((next (cdr this)))
;;brm;;          (if (clean-association (car this))
;;brm;;              (begin
;;brm;;                (let clean-tail ((this next) (prev this))
;;brm;;                  (if (pair? this)
;;brm;;                      (let ((next (cdr this)))
;;brm;;                        (if (clean-association (car this))
;;brm;;                            (clean-tail next this)
;;brm;;                            (begin
;;brm;;                              (set-cdr! prev next)
;;brm;;                              (clean-tail next prev))))))
;;brm;;                this)
;;brm;;              (clean-head next)))
;;brm;;        this)))
