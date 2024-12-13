#lang s-exp "../../generic.rkt"

(provide (all-defined-out))

(require "../../rkt/fixnum.rkt"
         "../../kernel-gnrc.rkt")

;;bdk;; insert 1
(define (c:generate n type proc)
  (if (fix:= n 1)
      (proc 0)
      (s:generate n type proc)))

(define (c:lookup m struct)
  (if (structure? struct)
      (assq m
            (vector->list
             (if (up? struct)
                 (up->vector struct)
                 (down->vector struct))))
      struct))
;;bdk;; insert 1 end
