#lang racket/base

(provide (all-defined-out))

(require racket/vector
         racket/port
         "../rkt/default-object.rkt"
         (only-in "../rkt/racket-help.rkt" warn)
         "../rkt/undefined.rkt"
         "../general/eq-properties.rkt"
         (only-in "../rkt/todo.rkt" todos)
         )

(todos todo
       [#:from "???"
        unmap-reference-trap
        make-unassigned-reference-trap
        bkpt
        ]
       )

;; not found in scmutils or mit-scheme ...
(module extra racket/base
  (require "../rkt/undefined.rkt")

  (provide symbol-hash string-hash object-hash valid-hash-number? object-unhash)
  (define symhsh (make-hash))
  (define strhsh (make-hash))
  (define objhsh (make-hash))
  (define hshs (list symhsh strhsh objhsh))
  (define (symbol-hash sym) (let ([n (equal-hash-code sym)])(hash-set! symhsh n (make-weak-box sym))n))
  (define (string-hash str) (let ([n (equal-hash-code str)])(hash-set! strhsh n (make-weak-box str))n))
  (define (object-hash obj) (let ([n (equal-hash-code obj)])(hash-set! objhsh n (make-weak-box obj))n))
  (define (valid-hash-number? n) (and (ormap (λ (H) (hash-ref H n #f)) hshs) #t))
  (define (object-unhash n)
    (for*/first ([H (in-list hshs)]
                 [v (in-value (hash-ref H n #f))]
                 #:when v)
      (weak-box-value v undefined-value)))
  )
(require 'extra)
;; end

;;; -*- Scheme -*-

;;; $Header: utils.scm,v 1.13 90/08/23 02:07:44 GMT jinx Exp $

;;;; Control utilities

;;; Allowing callees to default correctly.

(define make-default-object
  ;; Cacheing kludge
  (let ((result '()))
    (define (make-default-object)
      (when (null? result)
	  (set! result
		(cons
		 (unmap-reference-trap (make-unassigned-reference-trap))
		 '())))
      (car result))
    make-default-object))

;;; Keyword parameter passing.

;; This should include an option that disables "Unknown option" errors
;; and passes left over parameters along.

(define (get-options all-options option-names recvr)
  (define (pass paired-options)
    (let loop ((names (reverse option-names))
	       (parameters '()))
      (cond ((null? names)
	     (apply recvr parameters))
	    ((assq (car names) paired-options)
	     =>
	     (lambda (pair)
	       (loop (cdr names)
		     (cons (cdr pair) parameters))))
	    (else
	     (loop (cdr names)
		   (cons (make-default-object) parameters))))))	  

  (let loop ((options all-options)
	     (acc '()))
    (cond ((null? options)
	   (pass acc))
	  ((not (memq (car options) option-names))
	   (error "get-options: Unknown option" (car options)))
	  ((null? (cdr options))
	   (error "get-options: No value" (car options)))
	  (else
	   (loop (cddr options)
		 (cons (cons (car options)
			     (cadr options))
		       acc))))))

;;;; Higher order utilities

(define make-reducer
  (let ()
    (define (construct-reducer binop direction min-args max-args null-value)
      (let ((reducer-maker
	     (if (eq? direction 'RIGHT)
                 (procedure-rename (λ (handle-last)
                                     (define (reducer next rest)
                                       (if (null? rest)
                                           (handle-last next)
                                           (binop next
                                                  (reducer (car rest) (cdr rest)))))
                                     reducer)
                                   'make-right-reducer)
                 (procedure-rename (λ (handle-last)
                                     (lambda (next rest)
                                       (let loop ((accum (handle-last next))
                                                  (rest rest))
                                         (if (null? rest)
                                             accum
                                             (loop (binop accum (car rest))
                                                   (cdr rest))))))
                                   'make-left-reducer)))
	    (handle1
	     (cond ((or (zero? max-args)
			(< max-args min-args))
		    (lambda (x) x))
		   ((eq? direction 'RIGHT)
		    (lambda (x)
		      (binop x null-value)))
		   (else
		    (lambda (x)
		      (binop null-value x))))))
	(case min-args
	  ((2)
	   (let ((process (reducer-maker handle1)))
	     (lambda (all)
	       (if (or (null? all) (null? (cdr all)))
		   (error "reducer: Too few arguments" all)
		   (process (car all) (cdr all))))))
	  ((1)
	   (if (= max-args 2)
	       (let ((process (reducer-maker handle1)))
		 (lambda (all)
		   (if (null? all)
		       (error "reducer: Too few arguments" all)
		       (process (car all) (cdr all)))))
	       (let ((process (reducer-maker (lambda (x) x))))
		 (lambda (all)
		   (cond ((null? all)
			  (error "reducer: Too few arguments" all))
			 ((null? (cdr all))
			  (handle1 (car all)))
			 (else
			  (process (car all) (cdr all))))))))
	  ((0)
	   (if (= max-args 2)
	       (let ((process (reducer-maker handle1)))
		 (lambda (all)
		   (if (null? all)
		       null-value
		       (process (car all) (cdr all)))))
	       (let ((process (reducer-maker (lambda (x) x))))
		 (lambda (all)
		   (cond ((null? all)
			  null-value)
			 ((null? (cdr all))
			  (handle1 (car all)))
			 (else
			  (process (car all) (cdr all))))))))
	  (else
	   (error "make-tail-collector: Inconsistency" min-args)))))

    (procedure-rename
     (λ (binop . options)
       (get-options
        options
        '(DIRECTION MIN-ARGS MAX-ARGS-USING-NULL-VALUE NULL-VALUE)
        (lambda (direction min-args max-args null-value)
          (let* ((min-args
                  (if (default-object? min-args)
                      2
                      min-args))
                 (max-args
                  (cond ((not (default-object? max-args))
                         max-args)
                        ((default-object? null-value)
                         0)
                        (else
                         2))))
            ;; Paranoia check.
            (cond ((or (not (integer? min-args))
                       (not (<= 0 min-args 2)))
                   (error "make-reducer: Bad min-args" min-args))
                  ((or (not (integer? max-args))
                       (not (<= 0 max-args 2)))
                   (error "make-reducer: Bad max-args-using-null-value"
                          max-args))
                  ((default-object? null-value)
                   (when (or (= min-args 0)
                           (>= max-args min-args))
                       (error "make-reducer: required null-value not supplied"
                              `((MIN-ARGS ,min-args)
                                (MAX-ARGS-USING-NULL-VALUE ,max-args)))))
                  ((< max-args min-args)
                   (error "make-reducer: null-value meaningless"
                          `((NULL-VALUE ,null-value)
                            (MIN-ARGS ,min-args)
                            (MAX-ARGS-USING-NULL-VALUE ,max-args)))))

            (construct-reducer binop
                               (if (default-object? direction)
                                   'LEFT
                                   direction)
                               min-args
                               max-args
                               (if (default-object? null-value)
                                   (make-default-object)
                                   null-value))))))
     'make-reducer)))

;;;; Random utilities: association tables (eq? based)
(define (find-next-prime number)
  (let loop ((primes 
	      '(1009 2003 4001 8009 16001 32003 64007
		     128021 256019 512009 1024021)))
    (cond ((null? primes)
	   (error "find-next-prime: number too large" number))
	  ((< number (car primes))
	   (car primes))
	  (else (loop (cdr primes))))))

(define (size->table-size size)
 (find-next-prime (quotient (* 5 size) 4)))

(define (%table/make size)
  (make-vector size '()))

(define (table/make size)
  (%table/make (size->table-size size)))

(define (table/index table object)
  (modulo
   (cond ((exact-integer? object)
	  object)
	 ((symbol? object)
	  (symbol-hash object))
	 ((string? object)
	  (string-hash object))
	 (else
	  (object-hash object)))
   (vector-length table)))

(define (table/association table key)
  (let ((bucket (vector-ref table (table/index table key))))
    (and (not (null? bucket))
	 (let ((place (assq key bucket)))
	   (and (pair? place)
		(cdr place))))))

(define (table/associate! table key value)
  (let* ((index (table/index table key))
	 (bucket (vector-ref table index))
	 (place (assq key bucket)))
    (if (not place)
	(begin
	  (vector-set! table index (cons (cons key value) bucket))
	  #t)
	(begin
          (vector-set! table index (assq-set bucket key value))
	  #f))))

;;;; Growing association tables

(struct <table> (size limit entries table) #:transparent #:mutable)
(define (%table+/make [size #f] [limit #f] [entries #f] [table #f])
  (<table> size limit entries table))
(define table+/size    <table>-size)
(define table+/limit   <table>-limit)
(define table+/entries <table>-entries)
(define table+/table   <table>-table)
(define set-table+/size!    set-<table>-size!)
(define set-table+/limit!   set-<table>-limit!)
(define set-table+/entries! set-<table>-entries!)
(define set-table+/table!   set-<table>-table!)

#;(define-structure (table+ (conc-name table+/)
			  (constructor %table+/make))
  (size false read-only false)
  (limit false read-only false)
  (entries false read-only false)
  (table false read-only false))

(define (table+-size->limit size)
  (quotient (* 4 size) 5))

(define (table+/make [size 100])
  (let* ((size
	  ;; Bug in sf 4.8
	  (let ((input-size size))
	    (size->table-size input-size)))
	 (table (%table/make size)))
    (%table+/make size
		  (table+-size->limit size)
		  0
		  table)))

(define (table+/association table+ key)
  (table/association (table+/table table+) key))

(define (table+/associate! table+ key value)
  (when (table/associate! (table+/table table+) key value)
      (let ((entries (+ 1 (table+/entries table+))))
	(set-table+/entries! table+ entries)
	(when (> entries (table+/limit table+))
	    (table+/grow! table+)))))

(define (table+/grow! table+)
  (let* ((next-size (find-next-prime (table+/size table+)))
	 (new-table (%table/make next-size)))
    (vector-map
     (lambda (bucket)
       (for-each (lambda (pair)
		   (table/associate! new-table (car pair) (cdr pair)))
		 bucket))
     (table+/table table+))
    (set-table+/limit! table+ (table+-size->limit next-size))
    (set-table+/table! table+ new-table)
    (set-table+/size! table+ next-size)))

;;;; I/O utilities

(define (warning . arguments)
  (apply warn arguments)
  (warning/stop-hook))

(define (warning/stop-hook/bkpt)
  (bkpt "Breakpoint at warning"))

(define (warning/stop-hook/default)
  (void))

(define warning/stop-hook
  warning/stop-hook/default)

(define message-tag
  (list '*MESSAGE-NOISE*))

(define (message/noise noise)
  (cons message-tag noise))

(define (message/noise? noise)
  (and (pair? noise)
       (eq? (car noise) message-tag)))

(define (message/pluralize string number [suffix default-object])
  (let ((result
	 (if (= number 1)
	     string
	     (string-append string "s"))))
    (message/noise
     (if (default-object? suffix)
	 result
	 (string-append result suffix)))))

(define (message string . values)
  (newline)
  (write-string string)
  (for-each (lambda (value)
	      (write-char #\space)
	      (if (message/noise? value)
		  (write-string (cdr value))
		  (write value)))
	    values))
