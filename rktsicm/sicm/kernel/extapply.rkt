#lang racket/base

(provide (all-defined-out)
         #%top
         (rename-out [myapp #%app])
         *enable-literal-apply*
         *enable-generic-apply*)

(require (for-syntax racket/base)
         (only-in "../rkt/todo.rkt" bind-condition-handler use-value condition-type:floating-point-underflow)
         "../parameters.rkt"
         "cstm/generic.rkt")

;;;; Scheme evaluator extensions for algebra.

;;; Extension of Scheme for application of non-procedures in operator position.

#; ;parameter moved to paramters
(define *enable-generic-apply* true)

(define-syntax-rule (myapp f args ...)
  (if (and (not (procedure? f)) (*enable-generic-apply*))
      (#%app g:apply f (list args ...))
      (#%app f args ...)))

#|
(define inapplicable-object/operator
  (condition-accessor condition-type:inapplicable-object 'DATUM))

(define (apply-extension-init)
  (bind-default-condition-handler
   (list condition-type:inapplicable-object)
   (lambda (condition)
     (if *enable-generic-apply*
       (use-value
        (lambda args
          (g:apply (inapplicable-object/operator condition) args)))))))

(define (once-only! thunk name)
  (if (lexical-unbound? system-global-environment name)
      (begin (thunk)
	     ;; Create NAME in SGE
	     (eval `(define ,name #t) system-global-environment)
	     'done)
      'already-done))

(once-only! apply-extension-init 'apply-extension-init)
|#

#|
;;; Example of the way it should not be done!

(define (apply-extension-init)
  (bind-default-condition-handler
   (list condition-type:inapplicable-object)
   (lambda (condition)
     (if *enable-generic-apply*
	 ((stack-frame->continuation
	   (stack-frame/next
	    (stack-frame/next
	     (stack-frame/next
	      (continuation->stack-frame
	       (condition/continuation condition))))))
	  (lambda args
	    (g:apply (inapplicable-object/operator condition) args)))))))
|#


;;; Extension of Scheme for self-evaluating unbound variables

;this works... maybe: #%top needs to be (re)exported...
(module for-top racket/base
  (provide (rename-out [#%topp #%top]))
  (require (for-syntax racket/base)
           racket/stxparam)
  (define-syntax-parameter #%topp (syntax-rules () [(_ . id) (#%top . id)])))

(require racket/stxparam
         'for-top)
(define-syntax with-self-evaluating-unbound-variables
  (syntax-rules ()
      [(_ thunk)
       (syntax-parameterize ([#%top (syntax-rules () [(_ . id) 'id])])
         (thunk))]))

#;
(define (with-self-evaluating-unbound-variables thunk)
  (bind-condition-handler
      (list condition-type:unbound-variable)
      (lambda (condition)
	(let ((variable-name
	       (access-condition condition 'location)))
	  (use-value variable-name)))
    thunk))

#|
(pe (with-self-evaluating-unbound-variables
     (lambda ()
       (+ a 1))))
(+ 1 a)
|#

;;; Extension of Scheme for allowing symbolic literals to be applied

;;; *enable-generic-apply* is tested in applicable-literal?, used in
;;; g:apply.  See generic.scm.

#; ;moved to parameter
(define *enable-literal-apply* #f)

(define (with-literal-apply-enabled thunk)
  (parameterize ([*enable-literal-apply* #t])
    (thunk)))

#|
(pe (+ (f 'a) 3))
;Unbound variable: f

(pe (with-literal-apply-enabled
	(lambda ()
	  (+ (f 'a) 3))))
;Unbound variable: f

(pe (with-self-evaluating-unbound-variables
     (lambda ()
       (+ (f 'a) 3)) ))
;Application of a number not allowed f ((a))

(pe (with-literal-apply-enabled
	(lambda ()
	  (with-self-evaluating-unbound-variables
	   (lambda ()
	     (+ (f 'a) 3)) ))))
(+ 3 (f a))
|#

;;; (define *numbers-are-constant-functions* #f) in numbers.scm.  If
;;; this is set to #t then numbers are applied as constant functions.


;;; This allows literal functions to be reconstructed.

(define (with-literal-reconstruction-enabled thunk)
  (parameterize ([*literal-reconstruction* #t])
    (thunk)))


;;; Sometimes this saves the butt of a number jockey.

(define (with-underflow->zero thunk)
  (bind-condition-handler
      (list condition-type:floating-point-underflow)
      (lambda (condition)
	(use-value 0.))
    thunk))
