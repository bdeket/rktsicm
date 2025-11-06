#lang racket/base

(require rackunit
         "../../kernel/express.rkt"
         "../../kernel/types.rkt"
         "../../parameters.rkt"
         (only-in "../../rkt/glue.rkt" undefined-value default-object)
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/express"
   ;; ==== cstm ====
   (test-case
    "general accessors"
    (check-equal? (operator '(matrix-by-rows (5 6 7) (2 3 8))) 'matrix-by-rows)
    (check-equal? (operands '(matrix-by-rows (5 6 7) (2 3 8))) '((5 6 7) (2 3 8)))
    (check-equal? (first-operand '(matrix-by-rows (5 6 7) (2 3 8))) '(5 6 7))
    (check-equal? (second-operand '(matrix-by-rows (5 6 7) (2 3 8))) '(2 3 8))
    (check-equal? (rest-operands '(matrix-by-rows (5 6 7) (2 3 8))) '((2 3 8))))
   (test-case
    "abstract-quantity-properties"
    (define M (make-numerical-literal 'm))
    (check-equal? ((has-property? 'expression) M) 'm)
    (check-equal? ((has-property? 'expression) 'S) 'S)
    (check-equal? (get-property M 'expression) 'm)
    (check-equal? (get-property 'S 'expression) 'S)
    (check-equal? ((has-property? 'other) M) #f)
    (check-equal? (get-property M 'other 'default) 'default)
    (check-exn #px"Symbols have only EXPRESSION" (λ ()((has-property? 'other) 'S)))
    (check-equal? (get-property 'S 'other 'default) 'default)
    (add-property! M 'other #t)
    (check-equal? ((has-property? 'other) M) #t)
    (check-equal? (get-property M 'other 'default) #t)
    (check-true ((has-property? 'real) (make-real-literal 'M)))
    (check-exn #px"Malformed abstract-quantity"
               (λ () ((has-property? 'expression) '(ver keerd))))
    (check-exn #px"Bad abstract quantity"
               (λ () ((has-property? 'expression) 3)))
    (check-exn  #px"Bad abstract quantity"
               (λ () (get-property 3 'expression)))
    (check-exn  #px"Bad abstract quantity"
               (λ () (add-property! 3 'expression 3))))
   (test-case
    "literals"
    (check-equal? (expression (make-numerical-literal 'f)) 'f)
    (check-equal? (expression (make-real-literal 'f)) 'f)
    (check-equal? (get-property (make-real-literal 'f) 'real) #t)
    (check-equal? (expression (make-combination number-type-tag 'sin '(3))) '(sin 3))
    (check-equal? (expression-of (make-numerical-literal 'f)) 'f)
    (check-exn #px"No expression for abstract quantity" (λ () (expression-of '(f . #hash()))))
    (check-equal? (expression-of 'f) 'f)
    (check-exn #px"Bad abstract quantity" (λ () (expression-of 3))))
   (test-case
    "substitute"
    (check-equal? (substitute 'new 'old '(let ([old 4]) (+ 4 old)))
                  '(let ([new 4]) (+ 4 new)))
    (check-equal? (substitute 'new 'old '(*down* . #(2 3 old 8)))
                  '(*down* . #(2 3 new 8)))
    (check-equal? (substitute 'new '(+ 4 5) '(+ 4 5))
                  'new)
    (define O 'old)
    (check-equal? (substitute 'old 'old '(let ([old 4]) (+ 4 old)))
                  '(let ([old 4]) (+ 4 old))))
   (test-case
    "makers"
    (check-true (down-maker? '(down 2 5)))
    (check-true (up-maker? '(up 2 5)))
    (check-true (vector-maker? '(vector 2 5)))
    (check-true (quaternion-maker? '(quaternion i j 2 5)))
    (check-true (matrix-by-rows-maker? '(matrix-by-rows '(2 5) '(6 8))))
    (check-true (matrix-by-columns-maker? '(matrix-by-cols '(2 5) '(6 8))))
    (check-true (matrix-maker? '(matrix-by-rows '(2 5) '(6 8))))
    (check-true (matrix-maker? '(matrix-by-cols '(2 5) '(6 8))))
    (for ([v (in-list '(vector quaternion down up matrix-by-rows matrix-by-cols))])
      (check-not-false (compound-data-constructor? `(,v ...)))))
   (test-case
    "expression"
    (local-require "../../kernel-intr.rkt"
                   "../../units.rkt")
    ;; number
    ;;;; flonum
    (parameterize ([heuristic-number-canonicalizer #f])
      (check-equal? (expression 3.1) 3.1))
    (parameterize ([heuristic-number-canonicalizer (λ (x) 'drie)])
      (check-equal? (expression 3.1) 'drie))
    ;;;; normal
    (check-equal? (expression 3) 3)
    ;; symbol
    (check-equal? (expression 'alpha) 'alpha)
    ;; null
    (check-equal? (expression '()) '())
    ;; differential
    (check-equal? (expression (expression (terms->differential (list (make-differential-term '(0 0) 1)))))
                  ;; = '(*diff* ((0 0) 1))
                  '(make-differential-quantity (list (make-differential-term '(0 0) 1))))
    ;; down
    (check-equal? (expression (down 1 2 3)) '(down 1 2 3))
    ;; up
    (check-equal? (expression (up 1 2 3)) '(up 1 2 3))
    ;; quaternion
    (check-equal? (expression (quaternion 'a 'b 3 4)) '(quaternion a b 3 4))
    ;; matrix
    (check-equal? (expression (m:generate 3 3 +)) '(matrix-by-rows (list 0 1 2) (list 1 2 3) (list 2 3 4)))
    ;; literal-number
    (check-equal? (expression (make-real-literal 'f)) 'f)
    ;; unit
    (check-equal? (expression &foot) '(& 0.3048 &meter))
    ;; pair
    ;;;; ???
    (check-equal? (expression '(??? . 3)) '(??? . 3))
    ;;;; abstract-type
    (check-equal? (expression (abstract-matrix 'M)) 'M)
    ;;;; list
    (check-equal? (expression (list (abstract-down 'D) (abstract-up 'U))) '(D U))
    ;; abstract-fun?
    (check-equal? (expression (literal-function 'f)) 'f)
    ;; operator?
    (check-equal? (expression (o:+ o:identity o:identity)) '(+ identity identity))
    ;; procedure?
    (check-equal? (expression +) '+) ;; + from racket/base
    ;; undefined-value
    (check-equal? (expression undefined-value) '*undefined-value*)
    ;; bool
    (check-equal? (expression #t) 'true)
    (check-equal? (expression #f) 'false)
    ;; else
    (check-exn #px"Bad expression" (λ () (expression (exn "any" (current-continuation-marks))))))
   (test-case
    "procedure-expression / object-name / procedure-name"
    (local-require "../../rkt/environment.rkt"
                   "../../general/eq-properties.rkt"
                   racket/flonum)
    (check-equal? (procedure-expression +) '+)
    (check-equal? (procedure-expression +) (object-name + generic-environment))
    (check-equal? (object-name + rule-environment) '+)
    (check-equal? (procedure-name +) '+)
    (eq-put! + 'function-name 'addition)
    (check-equal? (procedure-expression +) 'addition)
    (check-equal? (procedure-expression flvector) 'flvector)
    (check-equal? (procedure-expression 'does-not-exist) '???))
   (test-case
    "generate-list-of-symbols"
    (check-equal? (generate-list-of-symbols 'sin 3) '(sin.0 sin.1 sin.2)))
   (test-case
    "variables-in"
    (check-equal? (variables-in `(,sin (,+ 2 (,* a b) 3 c))) '(c b a))
    (check-equal? (variables-in `(sin (+ 2 (* a b) 3 (* c a)))) '(c b a * + sin)))
   (test-case
    "pair-up"
    (check-equal? (pair-up '(a b c) '(1 2 3) '((d 4))) '((a 1) (b 2) (c 3) (d 4)))
    (check-equal? (pair-up '() '() '((d 4))) '((d 4)))
    (check-exn #px"Too few vals -- PAIR-UP" (λ () (pair-up '(a) '() '((d 4)))))
    (check-exn #px"Too many vals -- PAIR-UP" (λ () (pair-up '() '(1) '((d 4))))))
   (test-case
    "expression-walker"
    (check-equal? ((expression-walker `((+ ,+))) '(+ 4 5)) 9)
    (check-exn #px"Unknown expression type -- EXPRESSION-WALK"
               (λ () ((expression-walker `((+ ,+))) '(+ #(4) #(5))))))
   (test-case
    "expr:< expr:="
    (check-true (expr:< '() '(any)))
    (check-false (expr:< '() '()))

    (define (check-two a lst)
      (for ([e2 (in-list lst)])
        (check-true (expr:< a e2)))
      (for ([e1 (in-list lst)])
        (check-false (expr:< e1 a))))

    (check-two '() (list 1 'een "een" '(1) #(1)))
    (check-two 1 (list 2 'twee "twee" '(2) #(2)))
    (check-two 'a (list 'b "b" '(2) #(2)))
    (check-two "a" (list "b" '(2) #(2)))
    (check-two '(1 1) (list '(1 0 0) '(2 2) '(1 2)  #(2)))
    (check-two #(1 1) (list #(1 0 0) #(2 2) #(1 2)))
    (check-false (expr:< #(1 2) #(1 2)))
    
    ;; !?! if it is not part of the standard set it can only be compared with
    ;; something outside of the standard set...
    (check-false (expr:< undefined-value 1))
    (check-false (expr:< 1 undefined-value))
    (check-false (expr:< #(2) undefined-value))
    (check-false (expr:< undefined-value #(2)))

    (if (< (equal-hash-code default-object) (equal-hash-code undefined-value))
        (check-true (expr:< default-object undefined-value))
        (check-true (expr:< undefined-value default-object)))
    
    
    (check-true (expr:= '(+ 4 5) '(+ 4 5))))
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))