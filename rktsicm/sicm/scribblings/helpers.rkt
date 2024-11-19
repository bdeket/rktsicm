#lang racket/base

(require (for-syntax racket/base)
         (only-in scribble/manual defthing* defproc*))
(provide (all-defined-out))

(define-syntax-rule (deftype* type [itm ...])
  (defthing* [[itm type] ...]))
(define-syntax-rule (deftempproc* itm ...)
  (defproc* [[(itm [??? any?] (... ...)) '???] ...]))
(define-syntax (defsameproc* stx)
  (syntax-case stx ()
    [(_ [type rslt itms] ...)
     (with-syntax ([(procs ...)
                    (apply
                     append
                     (map (λ (t r is)
                            (map (λ (i) (list (cons i t) r)) (syntax->list is)))
                          (syntax->list #'(type ...)) (syntax->list #'(rslt ...)) (syntax->list #'(itms ...))))])
       #'(defproc* [procs ...]))]))