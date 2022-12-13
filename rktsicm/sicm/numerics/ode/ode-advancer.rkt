#lang racket/base

(provide (all-defined-out))

(require (only-in "../../rkt/glue.rkt" if write-line symbol-upcase symbol-downcase)
         (only-in "../../rkt/define.rkt" define)
         (only-in "../../rkt/todo.rkt" pp)
         "be.rkt"
         "bulirsch-stoer.rkt"
         "gear.rkt"
         "qc.rkt"
         )


;;bdk;; start original file


;;; Default settings

(define *ode-integration-method* 'BULIRSCH-STOER)

;;; (set! *ode-integration-method* 'QCRK4)
;;; (set! *ode-integration-method* 'BULIRSCH-STOER)
;;; (set! *ode-integration-method* 'QCCTRAP2)
;;; (set! *ode-integration-method* 'GEAR)

(define *first-step-scale* 1.0)

(define *corrector-convergence-margin* 1.0e-1)

(define *progress-monitor* #f)

(define *last-state*)

;;; ode-advancer returns a procedure of the form
;;; (lambda (state dt continue) ...)
;;;   where
;;;     continue=(lambda (new-state dt-obtained dt-suggested) ...)

#|
(pe ((ode-advancer
      (lambda (s) (up 1 (ref s 1)))
      1.e-12
      2)
     (up 0 1)
     1.0
     list))
((up 1. 2.718281828459047) 1 1.5)
|#

(define (ode-advancer sysder local-error-tolerance dimension)
  (case (symbol-downcase *ode-integration-method*)
    ((bulirsch-stoer)
     (bs-advancer sysder local-error-tolerance dimension))
    ((qcrk4)
     (qcrk4-advancer sysder local-error-tolerance))
    ((qcctrap2)
     (qc-ctrap-advancer sysder local-error-tolerance))
    ((qcceuler)
     (qc-ceuler-advancer sysder local-error-tolerance))
    ((explicit-gear)
     ;; actually sysder here is f&df
     (gear-advancer sysder local-error-tolerance dimension))
    (else
     (write-line `(methods: bulirsch-stoer qcrk4 qcctrap2 qcceuler explicit-gear))
     (error "Unknown ode integrator" *ode-integration-method*))))

(define (set-ode-integration-method! method)
  (case method
    ((BULIRSCH-STOER bulirsch-stoer Bulirsch-Stoer)
     (set! *ode-integration-method* 'bulirsch-stoer))
    ((QCRK4 qcrk4)
     (set! *ode-integration-method* 'qcrk4))
    ((QCCTRAP2 qcctrap2)
     (set! *ode-integration-method* 'qcctrap2))
    ((QCCEULER qcceuler)
     (set! *ode-integration-method* 'qcceuler))
    ((Gear Explicit-Gear gear explicit-gear GEAR)
     ;; actually sysder here is f&df
     (set! *ode-integration-method* 'explicit-gear))
    (else
     (write-line
      `(available methods: bulirsch-stoer qcrk4 qcctrap2 qcceuler explicit-gear))
     (display
      "Note: for x' = f(x), Gear needs f&df, all others need only f.")
     (newline)
     `(currently: ,*ode-integration-method*))))

(define (advance-monitor ns step-achieved step-suggested cont)
  (if *progress-monitor* (pp `(,ns ,step-achieved ,step-suggested)))
  (set! *last-state* ns)
  (cont))

(define (final-step-monitor ns step-achieved step-suggested)
  (if *progress-monitor* (pp `(,ns ,step-achieved ,step-suggested)))
  (set! *last-state* ns)
  ns)

(define (bs-advancer sysder local-error-tolerance dimension)
  (bulirsch-stoer-lisptran		;integrator
   (system-derivative->lisptran-derivative sysder)
   dimension
   local-error-tolerance))

(define (qcrk4-advancer sysder local-error-tolerance)
  ((quality-control rk4 4)
   sysder	
   local-error-tolerance))

(define (qc-ctrap-advancer sysder local-error-tolerance)
  ((quality-control c-trapezoid 2)
   sysder			
   local-error-tolerance 
   (* *corrector-convergence-margin*
      local-error-tolerance)))

(define (qc-ceuler-advancer sysder local-error-tolerance)
  ((quality-control c-euler 1)
   sysder			
   local-error-tolerance 
   (* *corrector-convergence-margin*
      local-error-tolerance)))

(define (gear-advancer f&df local-error-tolerance dimension)
  (gear-stepper-generator
   f&df
   dimension
   local-error-tolerance))

(define (gear? method)
  (memq method '(Gear Explicit-Gear gear explicit-gear GEAR)))