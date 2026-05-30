#lang racket/base

(require rackunit
         "../../simplify/rule-syntax.rkt"
         "../../kernel.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/rule-syntax"
   (test-case
    "rule"
    (check-equal? (rule:pattern    '(pat pred skel)) 'pat)
    (check-equal? (rule:predicate  '(pat pred skel)) 'pred)
    (check-equal? (rule:consequent '(pat csq))       'csq)
    (check-equal? (rule:skeleton   '(pat pred skel)) 'skel))
   (test-case
    "match:element"
    (check-true  (match:element? '(? any)))
    (check-false (match:element? '(?? any)))
    (check-false (match:element? 'any))
    (check-true  (match:segment? '(?? any)))
    (check-false (match:segment? '(?  any)))
    (check-false (match:segment? 'any))
    (check-true  (match:restricted? '(? any all)))
    (check-false (match:restricted? '(? any)))
    (check-equal? (match:restriction '(? any all)) 'all)
    (check-true  (match:reverse-segment? '($$ any)))
    (check-false (match:reverse-segment? '(?? any)))
    (check-equal? (match:variable-name '(?  name)) 'name)
    (check-equal? (match:variable-name '(?? name)) 'name)
    (check-equal? (match:variable-name '($$ name)) 'name))
   (test-case
    "predicate:compile"
    (check-equal? (predicate:compile 'pred?) 'pred?))
   (test-case
    "skel:element"
    (check-true  (skel:constant? 'any))
    (check-false (skel:constant? (cons 'anything 'pair)))
    (check-true  (skel:element? '(: element)))
    (check-false (skel:element? '(:: element)))
    (check-false (skel:element? 'any))
    (check-equal? (skel:element-expression '(: element)) 'element)
    (check-true  (skel:segment? '(:: element)))
    (check-false (skel:segment? '(:  element)))
    (check-false (skel:segment? 'any))
    (check-equal? (skel:segment-expression '(:: element)) 'element))
   (test-case
    "pattern:compile"
    (check-equal? (pattern:compile 'any) '`any)
    (check-equal? (pattern:compile '(? name)) '`(? name))
    (check-equal? (pattern:compile '(? name restr)) '`(? name ,restr))
    (check-equal? (pattern:compile '(?? name)) '`(?? name))
    (check-equal? (pattern:compile '($$ name)) '`($$ name))
    (check-equal? (pattern:compile '((? n0) (? n2 r) ($$ n1))) '`((? n0) (? n2 ,r) ($$ n1)))
    ;; TODO: this is never used ... - allow it? (0 can never be a pattern, only an element)
    (check-equal? (pattern:compile '((? n0) (? n2 r) . 0)) '`((? n0) (? n2 ,r) . 0)))
   (test-case
    "pattern:vars"
    (check-equal? (pattern:vars 'any) '())
    (check-equal? (pattern:vars '(? name)) '(name))
    (check-equal? (pattern:vars '(? name restr)) '(name))
    (check-equal? (pattern:vars '(?? name)) '(name))
    (check-equal? (pattern:vars '($$ name)) '(name))
    (check-equal? (pattern:vars '((? n0) (? n2 r) ($$ n1))) '(n1 n2 n0))
    ;; TODO: this is never used ... - allow it? (0 can never be a pattern, only an element)
    (check-equal? (pattern:vars '((? n0) (? n2 r) . 0)) '(n2 n0)))
   (test-case
    "skel:compile"
    (check-equal? (skel:compile 'any) '`any)
    (check-equal? (skel:compile '(: el)) '`,el)
    (check-equal? (skel:compile '(:: el)) '`,@el)
    (check-equal? (skel:compile '((: el0) (:: el1) (: el2))) '`(,el0 ,@el1 ,el2))
    ;; TODO: this is never used ... - allow it? (0 can never be a pattern, only an element)
    (check-equal? (skel:compile '((: el0) (:: el1) . 0)) '`(,el0 ,@el1 . 0)))
   (test-case
    "rule:compile"
    (check-equal? (rule:compile '( (exp (* (? n integer?) (log (? x))))
                                   (< n x)
                                   (expt (: x) (: n)) ))
                  '(rule:make `(exp (* (? n ,integer?) (log (? x))))
                              (lambda (x n) (let ((predicate-value (< n x)))
                                              (and predicate-value `(expt ,x ,n))))))
    (check-equal? (rule:compile `( (exp (* (? n integer?) (log (? x))))
                                   none
                                   (expt (: x) (: n)) ))
                  '(rule:make `(exp (* (? n ,integer?) (log (? x))))
                              (lambda (x n) `(expt ,x ,n))))

    (check-equal? (rule:compile '( (exp (* (? n integer?) (log (? x))))
                                   `(expt ,x ,n) ))
                  '(rule:make `(exp (* (? n ,integer?) (log (? x))))
                              (lambda (x n) `(expt ,x ,n))))
    (check-exn #px"Badly-formed rule"
               (λ () (rule:compile '(not-enough-arguments))))
    (check-exn #px"Badly-formed rule"
               (λ () (rule:compile '(way to many arguments)))))
   ;; TODO use syntax-parse
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))