#lang racket/base

(require rackunit
         racket/port
         "../../general/notes.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "general/notes"
   (test-case
    "note-that!"
    (clear-notes!)
    (note-that! '(test))
    (check-equal? *notes* '((test)))
    (note-that! '(test))
    (check-equal? *notes* '((test)))
    (note-that! '(else))
    (check-unique-match? '((test)(else))
                         ()
                         (list-no-order '(test) '(else))))
   (test-case
    "clear-notes!"
    (note-that! '(test))(note-that! '(else))
    (clear-notes!)
    (check-equal? *notes* '()))
   (test-case
    "display-note"
    (check-equal? (call-with-output-string (λ (out)
                                             (parameterize ([current-output-port out])
                                               (display-note '(test)))))
                  "#| \n'(test)\n|#\n"))
   (test-case
    "show-notes"
    (define note 'test)
    (local-require (only-in "../../general/eq-properties.rkt" eq-adjoin!))
    (eq-adjoin! note 'rules 'rule-22)
    (note-that! note)
    (clear-notes!)
    (check-equal? (call-with-output-string (λ (out)
                                             (parameterize ([current-output-port out])
                                               (show-notes))))
                  "\n#| \n'test\n'(rule-22)\n|#"))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))