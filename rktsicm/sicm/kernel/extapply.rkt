#lang racket/base

(provide (all-defined-out)
         #%top
         (rename-out [myapp #%app]
                     [g:apply apply])
         *enable-literal-apply*
         *enable-generic-apply*
         (except-out (all-from-out racket/base) #%app apply))

(require (for-syntax racket/base)
         (only-in "../rkt/todo.rkt" bind-condition-handler use-value condition-type:floating-point-underflow)
         "../parameters.rkt"
         "cstm/generic.rkt")

;;;; Scheme evaluator extensions for algebra.

;;; Extension of Scheme for application of non-procedures in operator position.

#; ;parameter moved to paramters
(define *enable-generic-apply* true)

; closer to the original, but contracts (like on map) make this fail => the applicable structure is
;    never called as function. so no real gain over impl. below
#; ; Worse: this is at least 10x slower than the other option.
(define-syntax-rule (myapp f args ...)
  (with-handlers ([exn:fail:contract:arity? raise]
                  [(λ (e)
                     (and exn:fail:contract?)
                     (regexp-match #px"^application: not a procedure;\n expected a procedure that can be applied to arguments\n" (exn-message e)))
                   (λ (e)
                     (#%app g:apply f (list args ...)))])
    (#%app f args ...)))

(define-syntax (myapp stx)
  (syntax-case stx ()
    [(myapp f args ...)
     (quasisyntax/loc
         stx
       (if (and (not (procedure? f)) (*enable-generic-apply*))
           #,(syntax/loc stx (#%app g:apply f (list args ...)))
           #,(syntax/loc stx (#%app f args ...))))]))

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
