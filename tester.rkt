#lang racket/base

(require racket/match
         rackunit)

;; run all tests in dir
;; using jobs
;;

;; extra result types


;***************************************************************************************************
;* Some params
;***************************************************************************************************
(define quiet? #f)
(define quiet-program? #f)
(define timeout +inf.0)

(define jobs 10) ; 0 mean "default"

;***************************************************************************************************
;* FROM compiler/comands/test
;; heavily butchered
;***************************************************************************************************
(struct summary (succes failed error timeout skip total time label body-res))
(define current-label (make-parameter "???"))

(define SUM (summary "SXS" "FAIL" "ERR" "TOUT" "SKIP" "TOT" "TIME" "NAME" "BODY"))
(define (make-summary #:sxs  [s 0]
                      #:fail [f 0]
                      #:err  [e 0]
                      #:tout [t 0]
                      #:skip [k 0]
                      #:time [T 0]
                      #:lbl  [l (current-label)]
                      #:bres [b #f])
  (summary s f e t k (+ s f e t k) T l b))
(define (summary+ #:lbl lbl . res)
  (make-summary #:sxs  (apply + (map summary-succes res))
                #:fail (apply + (map summary-failed res))
                #:err  (apply + (map summary-error res))
                #:tout (apply + (map summary-timeout res))
                #:skip (apply + (map summary-skip res))
                #:time (apply + (map summary-time res))
                #:lbl lbl
                #:bres res))

(define-syntax-rule (with-summary label . body)
  (call-with-summary label (lambda () . body)))

(define (call-with-summary label thunk)
  (define res
    ;; Produces either a summary or a list of summary:
    (parameterize ([current-label label])
      (thunk)))
  (if (summary? res)
      res
      (apply summary+ #:lbl (current-label) res)))

(define (display-summary top)
  (define files
    (let flatten ([sum top])
      (match sum
        [(list sum ...)
         (append-map flatten sum)]
        [(summary succes failed error timeout skip total time `(file ,p) body)
         (list sum)]
        [(summary succes failed error timeout skip total time label body)
         (flatten body)]
        [(? void?)
         empty])))
  (define sfiles (sort files string<=? #:key (λ (s) (path->string (cadr (summary-label s))))))
  (define total (apply summary+ #:lbl "TOTAL" sfiles))
  (define sfiles+ (cons total sfiles))
  (define data (map (λ (f) (cons f (max (string-length
                                         (number->string
                                          (apply max 0 (map f sfiles+))))
                                        (string-length (f SUM)))))
                    (list summary-total summary-failed summary-error summary-timeout summary-skip
                          (λ (x) (let ([t (summary-time x)])(if (number? t) (/ (round (/ t 30000.)) 2) t))))))
  (define (1line sum)
    (displayln
     (apply string-append
            (add-between `(,@(map (λ (f) (~a #:min-width (cdr f)
                                             #:align 'right
                                             (let ([a ((car f) sum)])
                                               (if (eqv? a 0) "" a))))
                                  data)
                           ,(let ([lbl (summary-label sum)])(if (list? lbl) (path->string (cadr lbl)) lbl)))
                         " "))))
  (1line SUM)
  (displayln "--------------------------------------------------------------")
  (for ([f (in-list sfiles)])
    (1line f))
  (displayln "--------------------------------------------------------------")
  (1line SUM)
  (1line total))

;; ------------------------------------------------------------------------------------------------
(require racket/match
         racket/format
         racket/list
         racket/port
         racket/path
         racket/file
         compiler/find-exe
         raco/command-name
         racket/system
         raco/testing
         setup/collects
         setup/getinfo
         compiler/module-suffix)

(define rx:default-suffixes (get-module-suffix-regexp))
;; For any other file suffix, a `test-command-line-arguments`
;; entry is required in "info.rkt".

(define test-exe-name (string->symbol (short-program+command-name)))

;; Stub for running a test in a process:
(module process racket/base
  (require raco/testing
           racket/file
           (submod "rktsicm/sicm/tests/helper.rkt" runner))
  ;; Arguments include a temp file to hold test results, the module path to run,
  ;; and the `dynamic-require` second argument. See the 'process case of
  ;; dynamic-require-elsewhere.
  (define argv (current-command-line-arguments))
  (define result-file (vector-ref argv 0))
  (define test-module (read (open-input-string (vector-ref argv 1))))
  (define test-invocation-path (bytes->path (read (open-input-string (vector-ref argv 2)))))

  (current-test-invocation-directory test-invocation-path)

  ;; In case PLTUSERHOME is set, make sure relevant
  ;; directories exist:
  (make-directory* (find-system-path 'doc-dir))

  (define the-tests (and (pair? test-module)
                         (eq? (car test-module) 'submod)
                         (dynamic-require (cadr test-module) 'the-tests (λ () #f))))

  (define ans
    (cond
      [the-tests
       (my-test-runner the-tests)]
      [else
       (parameterize ([current-command-line-arguments #()])
         (dynamic-require test-module 0)
         ((executable-yield-handler) 0))
       (define ans (test-report #:display? #f #:exit? #f))
       (vector (- (cdr ans) (car ans)) (car ans) 0 0 0)]))

  (when (< 0 (+ (vector-ref ans 1) (vector-ref ans 2)))
    (printf " > ~a FAILURES\n" (+ (vector-ref ans 1) (vector-ref ans 2))))
  (call-with-output-file* result-file #:exists 'truncate (λ (o) (write ans o)))
  
  (exit 0))

;; Run each test in its own place or process, and collect both test
;; results and whether any output went to stderr.
(define (dynamic-require-elsewhere p #:id id)
  (define c (make-custodian))
  (define timeout? #f)
  (define time (let ([start (current-milliseconds)])
                 (λ () (- (current-milliseconds) start))))
  (with-handlers ([exn:fail? (lambda (exn)
                               (custodian-shutdown-all c)
                               (unless quiet?
                                 (eprintf "~a: ~a\n" 
                                          (extract-file-name p)
                                          (exn-message exn)))
                               (if timeout?
                                   (make-summary #:tout 1 #:time (time))
                                   (make-summary #:fail 1 #:time (time))))])
    (define stdout (if quiet-program?
                       (open-output-nowhere)
                       (open-output-bytes)))
    (define stderr stdout)
    (define stdin (open-input-bytes #""))

    (unless quiet?
      (flush-output stdout))
      
    (define tmp-file (make-temporary-file))
    (define ps
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr]
                     [current-subprocess-custodian-mode 'kill]
                     [current-custodian c]
                     [current-environment-variables (environment-variables-copy
                                                     (current-environment-variables))])
        (define rel-path
          (let ([T (current-test-invocation-directory)])
            (let lp ([S (current-directory)][i 0])
              (define-values (A B C) (split-path S))
              (if (equal? A T)
                  (let lp ([i i]) (if (< 0 i) (string-append "../" (lp (- i 1))) "../tester.rkt"))
                  (lp A (+ i 1))))))
        (process*/ports
         stdout
         stdin
         stderr
         (find-exe)
         "-l"
         "racket/base"
         "-e"
         (format "(dynamic-require '(submod ~s process) #f)" rel-path)
         tmp-file
         (format "~s" (normalize-module-path p))
         (format "~s" (path->bytes (current-test-invocation-directory))))))
    (define proc (list-ref ps 4))

    (unless (sync/timeout timeout (thread (lambda () (proc 'wait))))
      (set! timeout? #t)
      (error test-exe-name "timeout after ~a seconds" timeout))

    (define results
      (with-handlers ([exn:fail:read? (lambda () #f)])
        (call-with-input-file* tmp-file read)))
    (define result-code (proc 'exit-code))
    (define test-results 
      (and (vector? results)
           (= 5 (vector-length results))
           (for/and ([v (in-vector results)]) (exact-nonnegative-integer? v))
           results))
      
    (unless (zero? result-code)
      (error test-exe-name "non-zero exit: ~e" result-code))
    (define ans
      (cond
        [test-results
         (make-summary #:sxs  (vector-ref test-results 0)
                       #:fail (vector-ref test-results 1)
                       #:err  (vector-ref test-results 2)
                       #:tout (vector-ref test-results 3)
                       #:skip (vector-ref test-results 4)
                       #:time (time))]
        [else
         (make-summary #:sxs 1 #:time (time))]))

    ;; cleanup
    (delete-file tmp-file)
    (close-output-port stdout)
    (unless quiet?
      ;; in lock, so printouts are not interleaved
      (printf "raco test: ~a ~s\n" id (normalize-module-path p))
      (flush-output))
    (define outputb (get-output-bytes stdout))
    (unless quiet-program?
      (display outputb))
    (call-with-output-file #:exists 'append
      (build-path (current-test-invocation-directory) "testresults.bak")
      (λ (out)
        (fprintf out "raco test: ~a ~s\n" id (normalize-module-path p))
        (display outputb out)))

    ans))

(define (extract-file-name p)
  (cond
    [(and (pair? p) (eq? 'submod (car p)))
     (cadr p)]
    [else p]))

;; Like `map`, but allows `run-one-test`s in parallel while starting
;; tasks in the order that a plain `map` would run them. The #:sema
;; argument everywhere makes tests start in a deterministic order
;; and keeps a filesystem traversal from getting far ahead of the
;; test runs.
(define (map/parallel f l #:sema continue-sema)
  (cond
    [(jobs . <= . 1) (map (lambda (v) (f v #:sema continue-sema)) l)]
    [else
     (struct task (th result-box))
     (define ts
       (for/list ([i (in-list l)])
         (define b (box #f))
         (define c-sema (make-semaphore))
         (define t (thread
                    (lambda ()
                      (set-box! b (with-handlers ([exn? values])
                                    (f i #:sema c-sema)))
                      ;; If no parallel task was ever created,
                      ;; count that as progress to the parent
                      ;; thread:
                      (semaphore-post c-sema))))
         (sync c-sema)
         (task t b)))
     (semaphore-post continue-sema)
     (for-each sync (map task-th ts))
     (for/list ([t (in-list ts)])
       (define v (unbox (task-result-box t)))
       (if (exn? v)
           (raise v)
           v))]))

(define (normalize-module-path p)
  (cond
    [(path? p) (path->string p)]
    [(and (pair? p) (eq? 'submod (car p)))
     (list* 'submod (normalize-module-path (cadr p)) (cddr p))]
    [else p]))

(define task-sema (make-semaphore jobs))
(define ids (for/list ([i (in-range jobs)]) i))
(define ids-lock (make-semaphore 1))

;; Perform test of one module (in parallel, as allowed by
;; `task-sema`):
(define (test-module p mod #:sema continue-sema)
  (call-with-semaphore
   task-sema ; limits parallelism
   (lambda ()
     (semaphore-post continue-sema) ; allow next to try to start
     (define id
       (call-with-semaphore
        ids-lock
        (lambda ()
          (define id (car ids))
          (set! ids (cdr ids))
          id)))
     (begin0
       (dynamic-require-elsewhere mod #:id (format " ~a" id))
       (call-with-semaphore
        ids-lock
        (lambda ()
          (set! ids (cons id ids))))))))

;; Perform all tests in path `e`:
(define (test-files p #:sema continue-sema)
  (cond
    [(directory-exists? p)
     (define dir-p (path->directory-path p))
     (check-info dir-p)
     (if (omit-path? dir-p)
         (make-summary #:lbl #f #:bres '())
         (with-summary
          `(directory ,p)
          (map/parallel
           (λ (dp #:sema s)
             (test-files (build-path p dp)
                         #:sema s))
           (directory-list p)
           #:sema continue-sema)))]
    [(and (or (and (regexp-match? rx:default-suffixes p)
                   (not (regexp-match? #rx"^[.]" (file-name-from-path p))))
              (include-path? p #:check-info? #t))
          (not (omit-path? p #:check-info? #t)))
     (parameterize ([current-directory (let-values ([(base name dir?) (split-path p)])
                                         (if (path? base)
                                             base
                                             (current-directory)))])
       (define file-name (file-name-from-path p))
       (define mod `(submod ,file-name test))
       (with-summary
        `(file ,p)
        (cond
           [(with-handlers ([exn:fail?
                             (lambda (exn)
                               ;; If there's an error, then try running
                               ;; this submodule to let the error show.
                               ;; Log a warning, just in case.
                               (log-warning "submodule load failed: ~s"
                                            (exn-message exn))
                               '())])
              (module-declared? mod #t))
            (test-module p mod #:sema continue-sema)]
           [else
            (test-module p file-name #:sema continue-sema)])))]
    [else (make-summary #:lbl #f #:bres '())]))

;; --------------------------------------------------
;; Reading "info.rkt" files

(define omit-paths (make-hash))
(define include-paths (make-hash))

(define collects-cache (make-hash))
(define info-done (make-hash))

(define (check-dir-info p)
  (define-values (base name dir?) (split-path p))
  (define dir (normalize-info-path
               (if dir?
                   p
                   (if (path? base)
                       (path->complete-path base)
                       (current-directory)))))

  (unless (hash-ref info-done dir #f)
    (hash-set! info-done dir #t)
    (define info (get-info/full dir))
    (when info
      (define (bad what v)
        (log-error "bad `~a' in \"info.rkt\": ~e" what v))
      
      (define (get-members table what all-ok?)
        (define v (info what (lambda () '())))
        (cond
          [(and all-ok? (eq? v 'all))
           (hash-set! table dir #t)]
          [(list? v)
           (for ([i (in-list v)])
             (cond
               [(path-string? i)
                (define p (normalize-info-path (path->complete-path i dir)))
                (define dp (if (directory-exists? p)
                               (path->directory-path p)
                               p))
                (hash-set! table dp #t)]
               [(regexp? i)
                (for ([f (in-directory dir)]
                      #:when (regexp-match i (path->string f)))
                  (hash-set! table f #t))]
               [else
                (bad what v)]))]
          [else (bad what v)]))
      (get-members omit-paths 'test-omit-paths #t)
      (get-members include-paths 'test-include-paths #t)
      )))

(define (check-info p)
  (check-dir-info p)
  ;; Check enclosing collection
  (define-values (base name dir?) (split-path p))
  (define c (if dir?
                #f
                (path->collects-relative p #:cache collects-cache)))
  (when (list? c)
    (let loop ([dir (if (path? base)
                        (path->complete-path base)
                        (current-directory))]
               [subpath (apply build-path (map bytes->path (reverse (cdr (reverse (cdr c))))))])
      (check-dir-info dir)
      (define-values (next-subpath subpath-name subpath-dir?) (split-path subpath))
      (define-values (next-dir dir-name dir-dir?) (split-path dir))
      (when (path? next-subpath)
        (loop next-dir next-subpath)))))

(define (normalize-info-path p)
  (simplify-path (path->complete-path p) #f))

(define (make-omit-path? omit-paths)
  (define (omit-path? p #:check-info? [check-info? #f])
    (when check-info? (check-info p))
    (let ([p (normalize-info-path p)])
      (or (hash-ref omit-paths p #f)
          (let-values ([(base name dir?) (split-path p)])
            (and (path? base)
                 (omit-path? base))))))
  omit-path?)

(define omit-path? (make-omit-path? omit-paths))
(define include-path? (make-omit-path? include-paths))

(module+ main
  (let ()
    (current-test-invocation-directory (current-directory))

    (define top (test-files (string->path "./rktsicm/sicm") #:sema (make-semaphore)))
    (call-with-output-file "./testrun.txt" #:exists 'replace (λ (out) (parameterize ([current-output-port out]) (display-summary (list top)))))
    ;(display-summary (list top))
    ))
