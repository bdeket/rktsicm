#lang racket/base

(require rackunit
         "../../kernel/generic.rkt"
         "../../kernel/ghelper.rkt"
         "../../parameters.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/generic"
   ;; CSTM
   (test-case
    "arity"
    (check-equal? (g:arity "anything") #f))
   (test-case
    "zero/one"
    (check-equal? (g:zero-like "anything") 0)
    (check-equal? (g:one-like "anything") 1)
    (check-equal? (generic:zero? 0) #f) ;; no generics loaded
    (check-equal? (g:zero? 0) #t)
    (check-equal? (g:zero? 0.0) #f)
    (check-equal? (g:zero? "zero") #f)
    (check-equal? (generic:one? 1) #f) ;; no generics loaded
    (check-equal? (g:one? 1) #t)
    (check-equal? (g:one? 1.0) #f)
    (check-equal? (g:one? "one") #f))
   (test-case
    "identity"
    (let ([x (gensym)]) (check-equal? (g:identity x) x))
    (check-equal? (g:identity? g:identity) #f) ;; no generics loaded
    (let ([x (gensym)]) (check-equal? ((g:identity-like "anything") x) x)))
   (test-case
    "square"
    (check-equal? (g:square 2) 4))
   (test-case
    "trace"
    (define G (g:trace +))
    (check-equal? (G 5 6) 11)
    (check-equal? (out->string (let ([G (g:trace +)]) (G 4) (G 5)))
                  "trace_0 > (#<procedure:+> 4)\ntrace_0 < 4\ntrace_1 > (#<procedure:+> 5)\ntrace_1 < 5\n"))
   (test-case
    "derivatives"
    (assign-operation generic:partial-derivative (λ lst (cons 'p_d lst)) #:end? #t)
    (check-equal? (g:partial-derivative 'f 'arg1 'arg2) '(p_d f (arg1 arg2)))
    (check-equal? (g:derivative 'f) '(p_d f ()))
    (check-equal? ((g:partial 'arg1 'arg2) 'f) '(p_d f (arg1 arg2))))
   (test-case
    "compare"
    (check-equal? (generic:= 1 1) #f);; no generics loaded
    (check-equal? (g:=:bin 1 1) #t)
    (check-equal? (g:=:bin 'a 'a) #f)
    
    (check-equal? (generic:< 1 2) #f);; no generics loaded
    (check-equal? (g:<:bin 1 2) #t)
    (check-equal? (g:<:bin 'a 'b) #f)
    (check-equal? (g:<:bin 'b 'a) #f)
    
    (check-equal? (generic:<= 1 1) #f);; no generics loaded
    (check-equal? (g:<=:bin 1 1) #t)
    (check-equal? (g:<=:bin 'a 'a) #f)
    
    (check-equal? (generic:> 2 1) #f);; no generics loaded
    (check-equal? (g:>:bin 2 1) #t)
    (check-equal? (g:>:bin 'a 'b) #f)
    (check-equal? (g:>:bin 'b 'a) #f)
    
    (check-equal? (generic:>= 1 1) #f);; no generics loaded
    (check-equal? (g:>=:bin 1 1) #t)
    (check-equal? (g:>=:bin 'a 'a) #f))
   (test-case
    "compare-n"
    (check-equal? (g:=:n '()) #t)
    (check-equal? (g:=:n '(any)) #t)
    (check-equal? (g:=:n '(1 1 1 1)) #t)
    (check-equal? (g:=:n '(1 1 2 1)) #f)
    (check-equal? (g:=) #t)
    (check-equal? (g:= 1 1 2 1) #f)
    
    (check-equal? (g:<:n '()) #t)
    (check-equal? (g:<:n '(any)) #t)
    (check-equal? (g:<:n '(1 2 3 4)) #t)
    (check-equal? (g:<:n '(1 2 2 3)) #f)
    (check-equal? (g:<) #t)
    (check-equal? (g:< 1 2 2 3) #f)
    
    (check-equal? (g:<=:n '()) #t)
    (check-equal? (g:<=:n '(any)) #t)
    (check-equal? (g:<=:n '(1 2 2 4)) #t)
    (check-equal? (g:<=:n '(1 2 1 3)) #f)
    (check-equal? (g:<=) #t)
    (check-equal? (g:<= 1 2 1 4) #f)
    
    (check-equal? (g:>:n '()) #t)
    (check-equal? (g:>:n '(any)) #t)
    (check-equal? (g:>:n '(4 3 2 1)) #t)
    (check-equal? (g:>:n '(4 2 2 1)) #f)
    (check-equal? (g:>) #t)
    (check-equal? (g:> 4 2 2 1) #f)
    
    (check-equal? (g:>=:n '()) #t)
    (check-equal? (g:>=:n '(any)) #t)
    (check-equal? (g:>=:n '(4 2 2 1)) #t)
    (check-equal? (g:>=:n '(4 2 3 1)) #f)
    (check-equal? (g:>=) #t)
    (check-equal? (g:>= 4 2 3 1) #f))
   (test-case
    "+"
    (check-equal? (g:+:bin 1 2) 3)
    (check-equal? (g:+:bin 'a 0) 'a)
    (check-equal? (g:+:bin 0 'a) 'a)
    (check-exn #px"Generic operator inapplicable:" (λ () (g:+:bin 'a 'b)))
    (assign-operation generic:+ (λ lst (cons '+ lst)) #:end? #t)
    (check-equal? (g:+:bin 'a 'b) '(+ a b))
    (check-equal? (g:+:n '()) 0)
    (check-equal? (g:+:n '(any)) 'any)
    (check-equal? (g:+:n '(2 2 2 2)) 8)
    (check-equal? (g:+) 0)
    (check-equal? (g:+ 2 2 2 2) 8))
   (test-case
    "-"
    (check-equal? (g:-:bin 1 2) -1)
    (check-equal? (g:-:bin 'a 0) 'a)
    (assign-operation g:negate (λ lst (cons '-- lst)) #:end? #t)
    (check-equal? (g:-:bin 0 'a) '(-- a))
    (check-exn #px"Generic operator inapplicable:" (λ () (g:-:bin 'a 'b)))
    (assign-operation generic:- (λ lst (cons '- lst)) #:end? #t)
    (check-equal? (g:-:bin 'a 'b) '(- a b))
    (check-equal? (g:-:n '()) 0)
    (check-equal? (g:-:n '(any)) '(-- any))
    (check-equal? (g:-:n '(2 2 2 2)) -4)
    (check-equal? (g:-) 0)
    (check-equal? (g:- 2 2 2 2) -4))
   (test-case
    "*"
    (check-equal? (g:*:bin 1 2) 2)
    (check-equal? (g:*:bin 'a 0) 0)
    (check-equal? (g:*:bin 0 'a) 0)
    (check-equal? (g:*:bin 'a 1) 'a)
    (check-equal? (g:*:bin 1 'a) 'a)
    (check-exn #px"Generic operator inapplicable:" (λ () (g:*:bin 'a 'b)))
    (assign-operation generic:* (λ lst (cons '* lst)) #:end? #t)
    (check-equal? (g:*:bin 'a 'b) '(* a b))
    (check-equal? (g:*:n '()) 1)
    (check-equal? (g:*:n '(any)) 'any)
    (check-equal? (g:*:n '(2 2 2 2)) 16)
    (check-equal? (g:*) 1)
    (check-equal? (g:* 2 2 2 2) 16))
   (test-case
    "/"
    (check-equal? (g:/:bin 1 2) 1/2)
    (check-equal? (g:/:bin 'a 1) 'a)
    ;; todo why can this not be the same for the bin-case as for the n-case
    (skip (check-equal? (g:/:bin 1 'a) '(invert a)))
    (check-exn #px"Generic operator inapplicable:" (λ () (g:/:bin 'a 'b)))
    (assign-operation generic:/ (λ lst (cons '/ lst)) #:end? #t)
    (check-equal? (g:/:bin 'a 'b) '(/ a b))
    (check-equal? (g:/:n '()) 1)
    (assign-operation generic:invert (λ lst (cons '// lst)) #:end? #t)
    (check-equal? (g:/:n '(any)) '(// any))
    (check-equal? (g:/:n '(2 2 2 2)) 1/4)
    (check-equal? (g:/) 1)
    (check-equal? (g:/ 2 2 2 2) 1/4))
   (test-case
    "atan"
    (assign-operation g:atan1 (λ lst (cons 'atan1 lst)) #:end? #t)
    (assign-operation g:atan2 (λ lst (cons 'atan2 lst)) #:end? #t)
    (check-equal? (g:atan 'y 'x) `(atan2 y x))
    (check-equal? (g:atan 'y) `(atan1 y)))
   (test-case
    "g:apply"
    (check-exn #px"No argument list for G:APPLY" (λ () (g:apply +)))
    (check-exn #px"g:apply: last argument must be a list" (λ () (g:apply + 1)))
    (check-equal? (g:apply + '()) 0)
    (check-exn #px"Generic operator inapplicable: #<procedure:_apply_>\\\n function: apply\\\nargument 1: f\\\nargument 2: \\(1\\)\\\n"
               (λ () (g:apply 'f 1 '())))
    (install-g:apply-case (λ (x) (eq? x 'f)) (λ (g args) `(g:apply -> (,g ,@args))))
    (assign-operation generic:apply (λ (g args) `(generic:apply -> (,g ,@args))) #:end? #t)
    (check-equal? (g:apply 'f 1 '()) '(g:apply -> (f 1)))
    (check-equal? (g:apply 'F 1 '()) '(generic:apply -> (F 1)))
    (skip ;;TODO: why is the install:g-apply-case needed. Can't we use the generic:apply system?
     ))
   (test-case
    "applicable-literal?"
    (check-equal? (applicable-literal? 1) #f)
    (check-equal? (applicable-literal? 's) #f)
    (parameterize ([*enable-literal-apply* #t])
      (check-equal? (applicable-literal? 1) #f)
      (check-equal? (applicable-literal? 's) #t)))
   (test-case
    "gcd"
    (check-equal? (g:gcd:n '()) 0)
    (check-equal? (g:gcd:n '(any)) 'any)
    (assign-operation g:gcd:bin gcd #:end? #t) ;; use rackets gcd for testing
    (check-equal? (g:gcd:n '(3 6)) 3)
    (check-equal? (g:gcd:n '(3 6 9)) 3)
    (check-equal? (g:gcd:n '(3 4 9)) 1)
    (check-equal? (g:gcd) 0)
    (check-equal? (g:gcd 3 4 9) 1))
   (test-case
    "sigma"
    (check-equal? (g:sigma g:identity 3 8) (+ 3 4 5 6 7 8))
    (check-equal? (g:sigma (λ (x) (expt x 2)) 1 3) (+ (* 1 1) (* 2 2) (* 3 3)))
    (check-equal? (g:sigma g:identity 3 1) 0))
   (test-case
    "simplify"
    (check-equal? (out->string (g:simplify 'any)) "!! not simplified !!\n")
    (check-equal? (g:simplify 'any) 'any))
   ;; MAIN
   (test-case
    "transpose"
    (local-require "../../kernel/structs.rkt")
    (structs:assign-operations)
    (assign-operation generic:transpose (λ lst (cons 'trans lst)) #:end? #t)
    (check-equal? (g:transpose 'a) '(trans a))
    (check-equal? (g:transpose #(1) #(1)) (up (up (down 1)))))
   (test-case
    "expt"
    (check-equal? (g:expt 2 3) 8)
    (check-equal? (g:expt 1 'a) 1)
    (check-equal? (g:expt 'a 0) 1)
    (check-equal? (g:expt 'a 1) 'a)
    (check-exn #px"Generic operator inapplicable:" (λ () (generic:expt 'a 'b)))
    (assign-operation generic:expt (λ lst (cons 'expt lst)) #:end? #t)
    (check-equal? (g:expt 'a 'b) '(expt a b)))
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))