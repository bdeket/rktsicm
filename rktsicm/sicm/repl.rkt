#lang racket/base

(require "display.rkt"
         "general/notes.rkt")

(provide show-notes)

(define sicm-print
  (let ([P (current-print)])
    (λ (rslt)
      (unless (void? rslt)
        (begin0 (P (simplify rslt)) (clear-notes!))))))
(current-print sicm-print)

(define (sicm-prompt-read)
  (printf (if (null? *last-notes*) "> " "?> "))
  (let ([in ((current-get-interaction-input-port))])
    ((current-read-interaction) (object-name in) in)))

(current-prompt-read sicm-prompt-read)