#lang racket/base

(provide (all-defined-out)
         rule:make)

(require (for-syntax racket/base
                     (only-in "rule-syntax.rkt" rule:compile))
         "rule-syntax.rkt"
         ;"unifier-rule-simplifier.rkt";marked experiment
         "rule-simplifier.rkt"
         )

;next two need to come from one of the rule-simplifiers
;(define rule:make void)

(define-syntax (rule-system stx)
  (syntax-case stx ()
    [(_ rules ...)
     (with-syntax ([compiled-rules
                    (datum->syntax stx
                                   (cons list
                                         (map rule:compile (syntax->datum #'(rules ...)))))])
       #'(rule-simplifier compiled-rules))]))

#;(define-syntax rule-system
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(rule-simplifier (list ,@(map rule:compile (cdr form)))))))