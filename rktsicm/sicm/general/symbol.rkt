#lang racket/base

(provide sym^)

(define (sym^ sym)
  (string->symbol (string-upcase (symbol->string sym))))
