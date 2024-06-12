#lang racket/base

(require rackunit
         (submod "../../general/eq-properties.rkt" ALL))

(define the-tests
  (test-suite
   "general/eq-properties"
   ;; assoc/assq - helpers
   (test-case
    "assoc-del"
    (check-equal? (assoc-del 1 '((1 . 2)(3 . 4))) '((3 . 4)))
    (check-equal? (assoc-del 1 '((1 . 2)(3 . 4)) =) '((3 . 4)))
    (check-equal? (assoc-del (list 4) '(((4) . 2)(3 . 4))) '((3 . 4)))
    (check-equal? (assoc-del 1 '((1 . 2)(3 . 4)(5 6)(1 8)(7 9))) '((3 . 4)(5 6)(7 9))))
   (test-case
    "assq-del"
    (check-equal? (assq-del 1 '((1 . 2)(3 . 4))) '((3 . 4)))
    (check-equal? (assq-del (list 4) '(((4) . 2)(3 . 4))) '(((4) . 2)(3 . 4)))
    (check-equal? (assq-del 1 '((1 . 2)(3 . 4)(5 6)(1 8)(7 9))) '((3 . 4)(5 6)(7 9))))
   (test-case
    "assoc-del*"
    (check-equal? (assoc-del* (list 1 3) '((1 . 2)(8 . 9)(3 . 4)))   '((8 . 9)))
    (check-equal? (assoc-del* (list 1 3) '((1 . 2)(8 . 9)(3 . 4)) =) '((8 . 9)))
    (check-equal? (assoc-del* (list (list 4) 8) '(((4) . 2)(8 . 9)(3 . 4))) '((3 . 4)))
    (check-equal? (assoc-del* (list 1 5) '((1 . 2)(3 . 4)(5 6)(1 8)(7 9))) '((3 . 4)(7 9))))
   (test-case
    "assq-del*"
    (check-equal? (assq-del* (list 1 3) '((1 . 2)(8 . 9)(3 . 4)))   '((8 . 9)))
    (check-equal? (assq-del* (list (list 4) 8) '(((4) . 2)(8 . 9)(3 . 4))) '(((4) . 2)(3 . 4)))
    (check-equal? (assq-del* (list 1 5) '((1 . 2)(3 . 4)(5 6)(1 8)(7 9))) '((3 . 4)(7 9))))
   (test-case
    "assoc-set"
    (check-equal? (assoc-set '() 1 2) '((1 . 2)))
    (check-equal? (assoc-set '((1 . 2)) 1 3) '((1 . 3)))
    (check-equal? (assoc-set '((1 . 2)((4) . 2)) (list 4) 3) '((1 . 2)((4) . 3)))
    (check-equal? (assoc-set '((1 . 2)((4) . 2)) (list 4) 3 eq?) '(((4) . 3)(1 . 2)((4) . 2))))
   (test-case
    "assq-set"
    (check-equal? (assq-set '() 1 2) '((1 . 2)))
    (check-equal? (assq-set '((1 . 2)) 1 3) '((1 . 3)))
    (check-equal? (assq-set '((1 . 2)((4) . 2)) (list 4) 3) '(((4) . 3)(1 . 2)((4) . 2))))

   ;; the table
   (test-case
    "eq-put! & get"
    (collect-garbage)(collect-garbage)(collect-garbage)
    (define A (list 4))
    (define B A)
    (check-equal? (hash-count eq-properties) 0)
    (eq-put! A 'waar #t)
    (check-equal? (hash-count eq-properties) 1)
    (check-equal? (eq-get A 'waar) #t)
    (check-equal? (eq-get B 'waar) #t)
    (eq-put! B 'waar 'daar)
    (check-equal? (eq-get A 'waar) 'daar)
    (check-equal? (eq-get B 'waar) 'daar)
    (set! A #f)
    (collect-garbage)(collect-garbage)(collect-garbage)
    (check-equal? (hash-count eq-properties) 1)
    (set! B #f)
    (collect-garbage)(collect-garbage)(collect-garbage)
    (check-equal? (hash-count eq-properties) 0)
    )
   (test-case
    "eq-plist"
    (define A (list 4))
    (eq-put! A 'waar #t)
    (check-equal? (eq-plist A) (cons A '((waar . #t))))
    (eq-put! A 'daar 6)
    (check-equal? (eq-plist A) (cons A '((daar . 6)(waar . #t))))
    )
   (test-case
    "eq-rem!"
    (define A (list 4))
    (eq-put! A 'waar #t)
    (eq-put! A 'daar 6)
    (eq-rem! A 'waar 'daar)
    (check-equal? (eq-plist A) (cons A '())))
   (test-case
    "eq-adjoin! & delete!"
    (define A (list 4))
    (eq-put! A 'waar '(1 2 3))
    (eq-adjoin! A 'waar 2)
    (check-equal? (eq-get A 'waar) '(1 2 3))
    (eq-adjoin! A 'waar 9)
    (check-equal? (eq-get A 'waar) '(9 1 2 3))
    (eq-delete! A 'waar 1)
    (check-equal? (eq-get A 'waar) '(9 2 3))
    (eq-delete! A 'waar 1)
    (check-equal? (eq-get A 'waar) '(9 2 3)))
   (test-case
    "eq-clone!"
    (define A (list 4))
    (define B (list 6))
    (eq-put! A 'waar #t)
    (eq-clone! A B)
    (check-equal? (cdr (eq-plist A)) (cdr (eq-plist B))))
   (test-case
    "eq-label!"
    (define A (list 4))
    (eq-label! A 'waar #t 'daar '(1 2 3))
    (check-equal? (eq-get A 'waar) #t)
    (check-equal? (eq-get A 'daar) '(1 2 3))
    (check-equal? (eq-get A 'othr) #f))
   (test-case
    "eq-path"
    (check-equal? ((eq-path 'A) #f) #f)
    (check-equal? ((eq-path 'A) 'B) 'B)
    (define A (list 0))
    (check-equal? ((eq-path '(waar)) A) #f)
    (eq-put! A 'waar 'daar)
    (check-equal? ((eq-path '(waar)) A) 'daar)
    (define B (list 1))
    (eq-put! A '->B B)
    (eq-put! B 'iets 4)
    (check-equal? ((eq-path '(iets ->B)) A) 4)
    (define C (list 2))
    (eq-put! B '->C C)
    (eq-put! C 'iets 5)
    (check-equal? ((eq-path '(->C ->B)) A) C)
    (check-equal? ((eq-path '(iets ->C ->B)) A) 5))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))