#lang racket/base

(require racket/file
         racket/format
         racket/port
         diff-merge
         threading)

(define scmutil-old1 "../../racket/test/sicm/scmutils-20200810_bld/")
(define scmutil-cur "../../racket/test/sicm/scmutils-20220518/")
(define scmutil-dir (make-parameter scmutil-cur))
(define rktsicm-dir "./rktsicm/sicm/")

;***************************************************************************************************
(define scm-lhs difference-old-lhs)
(define scm-len difference-old-len)
(define (->scmFile f) (string-append (scmutil-dir) f ".scm"))
(define rkt-lhs difference-new-lhs)
(define rkt-len difference-new-len)
(define (->rktFile f) (string-append rktsicm-dir f ".rkt"))

(define (->scm-lines scm)
  (~> (for/list ([l (in-list (file->lines scm))]
                 [i (in-naturals 1)])
        (list l scm i))
      (skip-scm-license _ scm)
      scm-remove-declare
      scm-clean-optional
      scm-clean-rest
      trim-whitespace))
(define (->rkt-lines rkt)
  (~> (for/list ([l (in-list (file->lines rkt))]
                 [i (in-naturals 1)])
        (list l rkt i))
      (skip-rkt-langprovreq _ rkt)
      insert-rkt-part
      skip-inserts-rkt
      rkt-unparameterize
      trim-whitespace))

(define (make-diff scmL rktL) (diff-indices (map car scmL) (map car rktL)))

(define (scm-diff file)
  (make-diff (->scm-lines (->scmFile file))
             (->rkt-lines (->rktFile file))))

(define (show-diff scmL rktL [indices (make-diff scmL rktL)])
  (define H (make-hash))
  (define (get f i)
    (cond [(hash-ref H f #f) => (λ (l) (list-ref l (- i 1)))]
          [else
           (define l (file->lines f))
           (hash-set! H f l)
           (list-ref l (- i 1))]))
  (define (write-part @ l L)
    (define Li (list-ref L (min @ (- (length L) 1))))
    (displayln (format "--- ~a ---  ~a+>~a" (cadr Li) (caddr Li) l))
    (for ([i (in-range @ (+ @ l))])
      (define itm (list-ref L i))
      (displayln (format "~a: ~a" (~r (caddr itm) #:min-width 4)
                         (get (cadr itm) (caddr itm))))))
  (for ([i (in-list indices)])
    (write-part (scm-lhs i) (scm-len i) scmL)
    (write-part (rkt-lhs i) (rkt-len i) rktL)
    (displayln "===================================================")
    (displayln "")))

(define (scm-show-diff file) (show-diff (->scm-lines (->scmFile file))
                                        (->rkt-lines (->rktFile file))))

;***************************************************************************************************
(define ((clean-smthng px alt) lines)
  (for/list ([l (in-list lines)])
    (cons (regexp-replace* px (car l) alt)
          (cdr l))))

;***************************************************************************************************
(define SCM-LIC
  '("#| -*- Scheme -*-"
    ""
    "Copyright (c) 1987, 1988, 1989, 1990, 1991, 1995, 1997, 1998,"
    "              1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,"
    "              2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,"
    "              2015, 2016, 2017, 2018, 2019, 2020"
    "            Massachusetts Institute of Technology"
    ""
    "This file is part of MIT scmutils."
    ""
    "MIT scmutils is free software; you can redistribute it and/or modify"
    "it under the terms of the GNU General Public License as published by"
    "the Free Software Foundation; either version 2 of the License, or (at"
    "your option) any later version."
    ""
    "MIT scmutils is distributed in the hope that it will be useful, but"
    "WITHOUT ANY WARRANTY; without even the implied warranty of"
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU"
    "General Public License for more details."
    ""
    "You should have received a copy of the GNU General Public License"
    "along with MIT scmutils; if not, write to the Free Software"
    "Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,"
    "USA."
    ""
    "|#"))
(define (skip-scm-license lines [name #f])
  (let lp ([l0 lines]
           [l1 SCM-LIC])
    (cond
      [(null? l0) (error "incomplete license" name)]
      [(null? l1) l0]
      [(string=? (caar l0) (car l1)) (lp (cdr l0) (cdr l1))]
      [else (error (format "~a difference in license:" name) (cons (caar l0) (car l1)))])))

(define (scm-remove-declare lines)
  (let lp ([l0 lines]
           [open 0]
           [in #f])
    (cond
      [(null? l0) '()]
      [(and in (= open 0)) l0]
      [(regexp-match #px"^\\(declare \\(usual-integrations" (caar l0))
       (lp (cdr l0)
           (- (length (regexp-match* #px"\\(" (caar l0)))
              (length (regexp-match* #px"\\)" (caar l0))))
           #t)]
      [in
       (lp (cdr l0)
           (+ open (length (regexp-match* #px"\\(" (caar l0)))
              (- (length (regexp-match* #px"\\)" (caar l0)))))
           in)]
      [else (cons (car l0) (lp (cdr l0) open in))])))

(define scm-clean-optional (clean-smthng #px"#!optional" "#:optional"))
(define scm-clean-rest (clean-smthng #px"#!rest" "."))


;***************************************************************************************************
(define RKT-START ";;bdk;; start original file")
(define known-parameters '(*suppressed-argument-list*
                           *suppressed-argument-list-counter*
                           *rename-list*
                           *time-upper-limit*
                           *active-tags*
                           *known-reals*
                           heuristic-zero-test-bugger-factor
                           singular-matrix-error
                           *quadrature-neighborhood-width*
                           *enable-literal-apply*
                           *literal-reconstruction*
                           enable-constructor-simplifications?
                           incremental-simplifier
                           *enable-generic-apply*
                           *c*
                           euclid-wallp?
                           *heuristic-sparse-gcd-enabled*
                           *complete-solutions*
                           *underdetermined-solutions*
                           *with-residual-equations*
                           *with-tough-equations*
                           *with-extra-equations*
                           *outstanding-contradictions*
                           *numerical-constants*
                           ))

(define (skip-rkt-langprovreq lines [name #f])
  (let lp ([l0 lines])
    (cond
      [(null? l0) (error "Start line missing" name)]
      [(string=? (caar l0) RKT-START) (cdr l0)]
      [else (lp (cdr l0))])))

(define (insert-rkt-part lines)
  (let lp ([l0 lines])
    (cond
      [(null? l0) '()]
      [(regexp-match #px"^;;bdk;; moved to (.+) (\\d+)" (caar l0))
       =>
       (λ (x)
         (define-values (d _0 _1) (split-path (cadar l0)))
         (define f (string-append (path->string d) (cadr x) ".rkt"))
         (lp (append (get-rkt-insert f (caddr x))
                     (cdr l0))))]
      [else (cons (car l0) (lp (cdr l0)))])))

(define (rkt-unparameterize lines)
  (define (no-make-pm l)
    (define in (regexp-match #px"\\(make-parameter.*" (car l)))
    (cond
      [in
       (define full (substring (format "~v" (call-with-input-string (car in) read)) 1))
       (define new (regexp-replace #px".*\\(make-parameter (.*)\\)" full "\\1"))
       (cons (regexp-replace (regexp-quote full) (car l) new)
             (cdr l))]
      [else l]))
  (map no-make-pm
       ((clean-smthng #px"parameterize" "fluid-let")
        (for/fold ([lines lines])
                  ([p (in-list known-parameters)])
          ((clean-smthng (regexp-quote (format "(~a)" p)) (format "~a" p))
           lines)))))

(define (skip-inserts-rkt lines)
  (let lp ([l0 lines]
           [cur #f]
           [file #f]
           [cont (λ (l0) (error))])
    (cond
      [(and (null? l0) cur)
       (error 'skip-inserts-rkt (format "missing end for ~a in ~a" cur file))]
      [(null? l0) '()]
      [(regexp-match #px"^;;bdk;; insert (\\d+)($| : )" (caar l0))
       =>
       (λ (x) (lp (cdr l0) (cadr x) (cadr l0) (λ (l0) (lp l0 cur file cont))))]
      [(regexp-match #px"^;;bdk;; insert (\\d+) end" (caar l0))
       =>
       (λ (x) (if (equal? cur (cadr x))
                  (cont (cdr l0))
                  (error 'skip-inserts-rkt (format "not in ~a (but ~a) for ~a" (cadr x) cur file))))]
      [cur (lp (cdr l0) cur file cont)]
      [else (cons (car l0) (lp (cdr l0) cur file cont))])))

(define (get-rkt-insert file ctr)
  (define lines (for/list ([l (in-list (file->lines file))]
                           [i (in-naturals 1)])
                  (list l file i)))
  (let lp ([l0 lines]
           [in #f])
    (cond
      [(null? l0) (error 'get-rkt-insert (format "missing ~a for ~a in ~a" (if in "end" "start") ctr file))]
      [(let ([a (regexp-match #px"^;;bdk;; insert (\\d+)($| : )" (caar l0))])
         (and a (string=? (cadr a) ctr)))
       (lp (cdr l0) #t)]
      [(let ([a (regexp-match #px"^;;bdk;; insert (\\d+) end" (caar l0))])
         (and a (string=? (cadr a) ctr)))
       '()]
      [in (cons (car l0) (lp (cdr l0) in))]
      [else (lp (cdr l0) in)])))

(define (trim-whitespace lines)
  (for/list ([l (in-list lines)])
    (cons (regexp-replace #px"\\s*$" (regexp-replace #px"^\\s*" (car l) "") "")
          (cdr l))))

;***************************************************************************************************
;#;
(~> (for*/list ([d (in-directory rktsicm-dir)]
                [s0 (in-value (path->string d))]
                #:when (regexp-match #px"\\.(rkt|scm)$" s0)
                #:when (regexp-match #px"\\\\" s0)
                #:unless (or (regexp-match #px"/lang\\\\" s0)
                             (regexp-match #px"/rkt\\\\" s0)
                             (regexp-match #px"/tests\\\\" s0)
                             (regexp-match #px"/rkt\\\\" s0)
                             (regexp-match #px"/calculus\\\\manifold\\\\" s0)
                             (regexp-match #px"/calculus\\\\indexed\\\\" s0)
                             (regexp-match #px"/kernel\\\\cstm\\\\" s0)
                             (regexp-match #px"/kernel\\\\todo\\\\" s0)
                             (regexp-match #px"/simplify\\\\pcfpf\\\\" s0))
                [file (in-value (regexp-replace #px"\\.rkt$" (regexp-replace rktsicm-dir s0 "") ""))]
                [s1 (in-value (regexp-replace #px"rkt$" (regexp-replace rktsicm-dir s0 (scmutil-dir)) "scm"))]
                #:when (if (file-exists? s1) #t
                           (begin
                             (when (member RKT-START (file->lines d) string=?)
                               (error "Non scm file has start line" d))
                             (displayln (format "could not find: ~a" s1)) #f))
                [ds (in-value (make-diff (->scm-lines s1) (->rkt-lines s0)))]
                #:unless (null? ds))
      ;(displayln (format "~a difference(s) found in ~a" (length ds) file))
      (cons (length ds) file)
      )
    ;(sort _ < #:key car)
    (map (λ (x) (displayln (format "~a difference(s) found in ~a" (car x) (cdr x)))) _)
    (void))


