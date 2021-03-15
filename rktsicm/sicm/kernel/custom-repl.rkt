#lang racket/base

(provide (all-defined-out))

(require (only-in "../general/logic-utils.rkt" disjunction)
         (only-in "../rkt/undefined.rkt" undefined-value?)
         (only-in "../rkt/environment.rkt" access ->environment scmutils-base-environment)
         (only-in "../rkt/todo.rkt" todo pp)
         (only-in "types.rkt" operator?)
         )

(todo hook/repl-eval)
(todo hook/repl-write)
(todo cmdl/port)
(todo prepare-for-printing)
(todo simplify)
(todo *last-expression-printed*)
(todo unsimplifiable?)
(todo record?)
(todo record-type-name)
(todo record-type-description)
(todo record-type-descriptor)
(todo record-description)
(todo port/operation)
(todo internal-show-expression)

#|
;;; I don't trust this!

(define saved-repl-eval
  (access default/repl-eval
	  (->environment '(runtime rep))))

(define (scmutils/repl-eval s-expression environment repl)
  ;; Reset the differential tag count each time we start a new eval.
  ;; See kernel/deriv.scm.
  (set! (access differential-tag-count scmutils-base-environment) 0)
  (saved-repl-eval s-expression environment repl))

(set! hook/repl-eval scmutils/repl-eval)
|#

(define saved-repl-eval
  (access default/repl-eval
	  (->environment '(runtime rep))))

(define (scmutils/repl-eval s-expression environment repl)
  ((access clear-notes! scmutils-base-environment))
  (saved-repl-eval s-expression environment repl))

(set! hook/repl-eval scmutils/repl-eval)


(define saved-repl-write
  (access default/repl-write
	  (->environment '(runtime rep))))

(define (scmutils/repl-write object s-expression repl)
  (let* ((port (cmdl/port repl))
	 (edwin? (edwin-port? port)))
    (define (maybe-message val)
      (when edwin? (edwin/transcript-write val #f)))
    (define (simplifiable object)
      (prepare-for-printing object simplify)
      (let ((val (*last-expression-printed*)))
	(if ((disjunction symbol? number?) val)
	    (print-unsimplifiable val)
	    (begin (display "#|\n" port)
		   (println val port)
		   (display "|#\n" port)
		   (maybe-message val)))))
    (define (print-unsimplifiable object)
      (display "#| " port)
      (write object port)
      (display " |#\n" port)
      (maybe-message object))
    (cond ((unsimplifiable? object)
	   (if (undefined-value? object)
	       (begin (newline port)
                      (display ";No return value." port)
		      (maybe-message object))
	       (print-unsimplifiable object)))
	  ((or (symbol? object)
	       (list? object)
	       (vector? object)
	       (procedure? object))
	   (simplifiable object))
	  ((record? object)
	   (simplifiable
	    `(*record*
	      ,(record-type-name (record-type-descriptor object))
	      ,@(record-description object))))
	  (else (print-unsimplifiable object)))))

(define (start-scmutils-print)
  (set! hook/repl-write scmutils/repl-write))

(define (stop-scmutils-print)
  (set! hook/repl-write saved-repl-write))


(define edwin/write-result
  (access operation/write-result (->environment '(edwin inferior-repl))))

(define edwin/transcript-write
  (access transcript-write (->environment '(edwin inferior-repl))))

(define (edwin-port? port)
  (eq? (port/operation port 'write-result)
       edwin/write-result))


(define (display-expression)
  (if (or (undefined-value? (*last-expression-printed*))
	  (and (procedure? (*last-expression-printed*))
	       (not (operator? (*last-expression-printed*)))))
      (*last-expression-printed*)
      (internal-show-expression (*last-expression-printed*))))

(define de display-expression)
