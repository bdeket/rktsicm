#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" if)
         (only-in "../rkt/define.rkt" define)
         (only-in "../rkt/todo.rkt" pp)
         "list-utils.rkt"
         "eq-properties.rkt")

;;bdk;; insert 1
;;; Programs may leave notes here

(define *taking-notes* #t)
(define *showing-notes* #f)

(define *notes* '())

(define (note-that! note)
  (and note                             ;fail if note is #f
       (begin
         (if *showing-notes*
             (display-note note))
         (if *taking-notes*
             (begin 
               (set! *notes* (lset-adjoin equal? *notes* note))
               'noted)
             'ignored))))

(define (clear-notes!)
  (set! *last-notes* *notes*)
  (set! *notes* '()))

(define (display-note note)
  (display "#| ")
  (newline)
  (pp note)
  (display "|#")
  (newline))

(define *last-notes*)
(define *last-notes-shown*)

(define (show-notes)
  (set! *last-notes-shown* *last-notes*)
  (newline)
  (display "#| ")
  (for-each (lambda (note)
              (newline)
              (pp note)
              (let ((sig (eq-get note 'rules)))
                (if sig (pp sig))))
            *last-notes*)
  (display "|#"))
;;bdk;; insert 1 end
