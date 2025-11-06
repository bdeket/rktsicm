#lang racket/base

(require rackunit
         "../../kernel/litfun.rkt"
         "../../kernel/cstm/arity.rkt"
         "../../kernel/function.rkt"
         (only-in "../../kernel/structs.rkt" up down)
         (only-in "../../kernel/express.rkt" expression)
         "../../kernel/types.rkt"
         "../helper.rkt"
         )

(function:assign-operations)
(provide the-tests)
(define the-tests
  (test-suite
   "kernel/litfun"
   ;; TYPES
   (test-case
    "types - X"
    (check-equal? (X Real) Real)
    (check-equal? (X Real Real) `(X ,Real ,Real))
    (check-exn #px"Null type argument -- X" (λ () (X)))

    (check-equal? (^ Real 3) (X Real Real Real)))
   (test-case
    "types - UP"
    (check-equal? (UP Real) Real)
    (check-equal? (UP Real Real) `(UP ,Real ,Real))
    (check-exn #px"Null type argument -- UP" (λ () (UP))))
   (test-case
    "types - DOWN"
    (check-equal? (DOWN Real) Real)
    (check-equal? (DOWN Real Real) `(DOWN ,Real ,Real))
    (check-exn #px"Null type argument -- DOWN" (λ () (DOWN))))
   (test-case
    "types - X*"
    (check-equal? (X* Real) `(X* ,Real))
    (check-equal? (X* Real Real) `(X* ,Real ,Real))
    (check-equal? (X* Complex 2 Real) `(X* ,Complex ,Complex ,Real))
    (check-equal? (X* Real 2) `(X ,Real ,Real))
    (check-exn #px"Bad type arguments" (λ () (X* 3 Real)))
    (check-exn #px"Bad type arguments" (λ () (X* Real 0)))
    (check-exn #px"Null type argument 'X\\*" (λ () (X*))))
   (test-case
    "types - UP*"
    (check-equal? (UP* Real) `(UP* ,Real))
    (check-equal? (UP* Real Real) `(UP* ,Real ,Real))
    (check-exn #px"Null type argument 'UP\\*" (λ () (UP*))))
   (test-case
    "types - DOWN*"
    (check-equal? (DOWN* Real) `(DOWN* ,Real))
    (check-equal? (DOWN* Real Real) `(DOWN* ,Real ,Real))
    (check-exn #px"Null type argument 'DOWN\\*" (λ () (DOWN*))))
   (test-case
    "types - ->"
    (check-equal? (-> (UP* Real) (X Any)) `(-> (UP* ,Real) ,Any))
    (check-equal? (default-function-type 1) `(-> ,Real ,Real))
    (check-equal? (default-function-type 3 Complex) `(-> (X ,Complex ,Complex ,Complex) ,Complex))
    (check-equal? (permissive-function-type 2) `(-> (X ,Any ,Any) ,Real))
    (check-equal? (permissive-function-type 1) `(-> ,Any ,Real))

    (check-equal? (Lagrangian) `(-> (UP ,Real (UP* ,Real) (UP* ,Real)) Real))
    (check-equal? (Lagrangian 1) `(-> (UP ,Real ,Real ,Real) Real))
    (check-equal? (Hamiltonian) `(-> (UP ,Real (UP* ,Real) (DOWN* ,Real)) Real))
    (check-equal? (Hamiltonian 2) `(-> (UP ,Real (UP ,Real ,Real) (DOWN ,Real ,Real)) Real)))
   (test-case
    "type/domain/range"
    (check-equal? (type->domain (Lagrangian 1)) (UP Real Real Real))
    (check-equal? (type->domain (default-function-type 2)) (X Real Real))
    (check-equal? (type->range-type (Lagrangian 1)) Real)
    (check-equal? (type->domain-types (Lagrangian 1)) (list (UP Real Real Real)))
    (check-equal? (type->domain-types (default-function-type 2)) (list Real Real))
    (check-equal? (type->arity (default-function-type 2)) (length->exact-arity 2))
    (check-equal? (type->arity (-> (X* Complex Real 2 Complex) (UP Real Real)))
                  ;; TODO: can this be (arity-at-least 3) ?
                  (arity-at-least 0)))
   (test-case
    "type-expression->predicate - simple types"
    (check-true  ((type-expression->predicate Real) 3))
    ;; TODO: Real and Complex both are just numerical-quantity?
    (check-true  ((type-expression->predicate Complex) 3+i))
    (check-true  ((type-expression->predicate Complex) 'a))
    (check-true  ((type-expression->predicate Any) "string"))
    (check-false ((type-expression->predicate Real) #(1 3)))
    (check-false ((type-expression->predicate Complex) #(3)))
    (check-exn #px"Unknown primitive type" (λ () ((type-expression->predicate 'something-stupid) 3))))
   (test-case
    "type-expression->predicate - structure types"
    (check-true  ((type-expression->predicate (X Real Real)) #(3 4)))
    (check-false ((type-expression->predicate (X Real Real)) #(3)))
    (check-true  ((type-expression->predicate (UP Any)) (up "string")))
    (check-false ((type-expression->predicate (UP Any Real)) (up "string" "één")))
    (check-true  ((type-expression->predicate (DOWN (UP Real Real) Real)) (down (up 1 2) 3)))
    (check-false ((type-expression->predicate (DOWN (UP Real))) #(down (up 1 2) 3 4)))
    ;; TODO: is this right?
    (check-true  ((type-expression->predicate (DOWN (UP Real))) 1))
    (check-true  ((type-expression->predicate (X* Real)) 1))
    (check-true  ((type-expression->predicate (X* Real)) #(1 2 3 4 5 6)))
    (check-false ((type-expression->predicate (X* Real Complex)) 1))
    (check-true  ((type-expression->predicate (X* Real Complex)) #(1)))
    (check-true  ((type-expression->predicate (X* Real Complex)) #(1 2)))
    (check-true  ((type-expression->predicate (X* Real Complex)) #(1 2 3)))
    (check-false ((type-expression->predicate (X* Real)) (vector 1 2 3 4 (up 5) 6)))
    (check-false ((type-expression->predicate (X* Real)) +))
    (check-true  ((type-expression->predicate (UP* Real 2 (DOWN* Real))) (up 1 2 (down 3 4 5 6 7))))
    (check-true  ((type-expression->predicate (UP* (DOWN* Real))) (up (down 2 3) (down 3 4 5 6 7))))
    (check-false ((type-expression->predicate (UP* (DOWN* Real))) (up (down 2 3) (down 3 4 5 6 7) (up 1))))
    (check-true  ((type-expression->predicate (UP* (DOWN* Real))) 1))
    (check-false ((type-expression->predicate (DOWN* Real)) (up 3 4)))
    (check-false ((type-expression->predicate (DOWN* Real)) "(down 3 4)")))
   (test-case
    "type-expression->predicate - rest"
    (check-true ((type-expression->predicate (-> Real Real)) +))
    ;; TODO: arity is not checked, can it be at this point?
    (check-false ((type-expression->predicate (-> Real Real)) 2))
    (check-exn #px"Unknown type combinator" (λ () (type-expression->predicate '(not-correct Real)))))
   (test-case
    "type-expression->type-tag"
    (check-equal? (type-expression->type-tag Real)                (abstract-type-tag *number*))
    (check-equal? (type-expression->type-tag Complex)             (abstract-type-tag *number*))
    (check-equal? (type-expression->type-tag (X Real Complex))    (abstract-type-tag *vector*))
    (check-equal? (type-expression->type-tag (UP Real Real))      (abstract-type-tag *up*))
    (check-equal? (type-expression->type-tag (DOWN Complex Real)) (abstract-type-tag *down*))
    (check-equal? (type-expression->type-tag (X* Complex))        (abstract-type-tag *vector*))
    (check-equal? (type-expression->type-tag (UP* Real))          (abstract-type-tag *up*))
    (check-equal? (type-expression->type-tag (DOWN* Real))        (abstract-type-tag *down*))
    (check-equal? (type-expression->type-tag (-> Real Complex))   (abstract-type-tag *function*))
    (check-exn #px"Unknown type combinator" (λ () (type-expression->type-tag '(something-stupid))))
    (check-exn #px"Unknown primitive type" (λ () (type-expression->type-tag 'something-stupid))))
   
   (test-case
    "typed-function"
    (check-false (f:domain-types +))
    (check-false (f:range-type +))
    (define +:typed (typed-function (λ (a b) (+ a b)) Real (list Real Real)))
    (check-equal? (f:domain-types +:typed) (list Real Real))
    (check-equal? (f:range-type +:typed) Real)
    (check-true (typed-function? +:typed))
    (check-false (abstract-function? +:typed))
    (check-exn #px"Inconsistent arity -- TYPED-FUNCTION"
               (λ () (typed-function (λ (a b) (+ a b)) Real (list Real))))
    (check-exn #px"I cannot handle this arity -- TYPED-FUNCTION"
               (λ () (typed-function + Real (list Real)))))
   (test-case
    "literal-function"
    (define F (literal-function 'f))
    (check-false (typed-function? F))
    (check-not-false (abstract-function? F))
    (check-true (literal-function? F))
    (check-equal? (f:domain-types F) (list Real))
    (check-equal? (f:domain-types (literal-function 'f (-> (X Real Real) Real))) (list Real Real))
    ;; TODO: this does not seem correct (see also litfun tests)
    (check-equal? (f:domain-types (literal-function 'f (-> (X* Real Real) Real))) (list (X* Real Real)))
    (check-equal? (f:range-type F) Real)
    (check-equal? (f:expression F) 'f)

    (check-equal? (f:range-type (literal-function F (-> Real Complex))) Complex)
    (check-equal? (procedure-arity (literal-function F (-> Real (-> Real Real)))) *exactly-one*)
    ;; TODO: I think the literal function should be limited to symbolic expressions
    ;; (literal-function "todo") ;=> is a valid literal-function
    ;; if the range is something else, it will fail
    (check-exn #px"Cannot handle this function expression: LITERAL-FUNCTION"
               (λ () (literal-function "F" (-> Real (UP Real Real)))))
    (check-true (up?   (literal-function 'f (-> Real (UP Real Real)))))
    (check-true (down? (literal-function 'f (-> Real (DOWN Real Real)))))
    (define U (literal-function 'f (-> Real (UP Complex Real))))
    (check-equal? (expression ((vector-ref U 0) 't)) '(f^0 t))
    (check-equal? (f:domain-types (vector-ref U 0)) (list Real))
    (check-equal? (f:range-type (vector-ref U 0)) Complex)
    ;; this fails with the error from type-expression->type-tag, but not with the
    ;; error from literal-function
    (check-exn #px"" (λ () (literal-function 'f `(-> ,Real 'something-stupid)))))
   (test-case
    "litfun / apply"
    (check-equal? (expression ((literal-function 'f (-> `(X) Real))))
                  '(f))
    (check-equal? (expression ((literal-function 'f (-> (X Real Real) Real)) 'x 'y))
                  '(f x y))
    (check-equal? (expression ((literal-function 'f (-> (X Real Real Real) Real)) 'x 'y 'z))
                  '(f x y z))
    (check-equal? (expression ((literal-function 'f (-> (X* Real 4) Real)) 'x 'y 'z 'a))
                  '(f x y z a))
    (check-equal? (expression (((literal-function 'f (-> (X* Real 4) (-> Real Real))) 'x 'y 'z 'a) 't))
                  '((f x y z a) t))
    (check-exn #px"Wrong type argument -- LITERAL-FUNCTION"
               (λ () ((literal-function 'f (-> (X Real Real) Real)) 'x (up 'y))))
    ;; TODO what is X* ? it seems vector, but X works for individual elements...
    (skip ;; The X* really seems broken, It now behaves like vectorof, but I think it should work
     ;; like listof. Also (X) should be valid, for 0-arg functions
     ;; creating a rest-arg function is now not possible
     ))
   ;; Tests for litderiv in "diffbug"

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))