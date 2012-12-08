;; Berkeley STK Test Framework Additions
;; by Pierre Karashchuk

;; To use, save code into some file, say "tests-named.scm".
;; then load the file: (load "tests-named.scm")

;; ------------------------

;; What can you do with it?

;; add tests to test database:

;; (add-tests-named <name> <test1> <test2> ...)
;; where <name> is some symbol, string, or number which names this group of tests.
;; and <test1>,<test2>, ... are of the format (<expression> <expected result>)
;;
;; NOTE: If a test group with name <name> has already been added, add-tests-named will neither
;; override nor modify it. Rather, it will do nothing.

;; then run the tests:
;;
;; (run-tests-named <name>)
;; where <name> is a name of some group of tests added earlier
;;
;; (run-tests-named <name1> <name2> ...)
;; will run group of tests with <name1> or <name2> and so on...
;;
;; (run-tests-named)
;; will run ALL groups of tests added

;; if you wish to clear tests
;; (need to clear to re-add tests with same name):
;;
;; (clear-tests-name <name1> <name2> ...)
;; will clear tests groups with names <name1>, <name2>, etc...
;;
;; (clear-test-cases)
;; will clear all test cases.

;; sample usage:
;; > (add-tests-named 'arithmetic-test
;;     ((+ 1 2) 3)
;;     ((* 3 5) 15)
;;     ((/ 6 2) (/ 12 4))
;;     ((+ 1 1) 5)
;;     )
;; => arithmetic-test

;; > (run-tests-named 'arithmetic-test)
;; (TESTS NAMED: arithmetic-test)
;; (----- Running 4 tests... Failure output below. -----)
;; ((+ 1 1)     Expected: 5     Actual: 2)
;; (----- Done.  1 failed test(s).  0 error(s). -----)
;;
;; (good: 3     failed: 1   errors: 0)


;; -----------------------------------------


;; use for Stk scheme
(define (to-string . args)
  (define (one-string x)
    (cond ((symbol? x) (symbol->string x))
          ((number? x) (number->string x))
          ((string? x) x)
          (else x)))
  (apply string-append (map one-string args)))

(define-macro (add-tests-named . tests)
  (let ((name (gensym)))
    `(let ((,name ,(car tests)))
      (if (member (to-string ,name) (map car *test-groups*))
     (show (list "WARNING: duplicate test group" ,name "detected. Ignoring it."))
     (begin
       (add-tests-named-helper ,name (quote ,(cdr tests)))
       ,name)))))

(define-macro (add-test-cases . tests)
  (cons 'begin
	(map (lambda (a) (cons 'add-test-case a))
	     tests)))

;; use for MIT scheme
;; (define to-string string)
;; (define-syntax catch
;;   (syntax-rules ()
;;     ((catch expr)
;;      (condition? (ignore-errors (lambda () expr))))))

;; (define-syntax add-tests-named
;;   (syntax-rules ()
;;     ((add-tests-named name (expr val) ...)
;;      (if (member (to-string name) (map car *test-groups*))
;;          (show (list "WARNING: duplicate test group" name "detected. Ignoring it."))
;;          (begin
;;            (add-tests-named-helper name '((expr val) ...))
;;            name)))))

;; (define-syntax add-test-cases
;;   (syntax-rules ()
;;     ((add-test-cases (name expr val))
;;      (add-test-case name val expr))
;;     ((add-test-cases (name expr val) t ...)
;;      (begin (add-test-case name val expr)
;;             (add-test-cases t ...)))))



(define (good-test-name? name)
  (or (symbol? name) (number? name) (string? name)))

(define *test-groups* '())

(define (clear-test-cases)
  (set! *xx-test-case-store* '())
  (set! *test-groups* '()))

(define (clear-tests test-names)
  (set! *xx-test-case-store*
        (filter (lambda (x) (not (member (car x) test-names)))
                *xx-test-case-store*)))

(define (clear-tests-named . names)
  (let ((namestrs (map to-string names)))
    (define (clear groups)
      (cond ((empty? groups) '())
            ((member (caar groups) namestrs)
             (clear-tests (map car (cdar groups)))
             (clear (cdr groups)))
            (else (cons (car groups)
                        (clear (cdr groups))))))
    (set! *test-groups* (clear *test-groups*))
    'ok))

(define (add-test-case-quoted name expected expr)
  (if (not (good-test-name? name))
      (show (list "WARNING: badly named test case:" name
                  "Ignoring it."))
      (let ((namestr (to-string name)))
        (if (assoc namestr *xx-test-case-store*)
            (show (list  "WARNING: duplicate test case" namestr "detected."
                         "Ignoring it."))
            (set! *xx-test-case-store*
                  (append *xx-test-case-store*
                          (list (list namestr expected expr))))))))

(define (add-tests-named-helper name tests)
  (define (format-tests i tests)
    (cond ((empty? tests) '())
          (else (let ((t (car tests)))
                  (cons (list (to-string name "-" i) (cadr t) (car t))
                        (format-tests (+ i 1) (cdr tests)))))))
  (let ((formatted-tests (format-tests 0 tests)))
    (for-each (lambda (x) (apply add-test-case-quoted x))
              formatted-tests)
    (set! *test-groups*
          (cons (cons (to-string name) formatted-tests)
                *test-groups*))
    ))


(define (run-single-case case)
  (let* ((get-name car)
         (get-expected cadr)
         (get-expression caddr)

         (expected (eval (get-expected case)))
         (actual '()))
    (cond
     ((catch (set! actual (eval (get-expression case))))
      (show (list (get-expression case)
                  "\t Expected:" expected
                  "\tActual:" "ERROR!!!"))
      (list 1 0)) ;; 1 error, 0 failures
     ((not (equal? actual expected))
      (show (list (get-expression case)
                  "\tExpected:" expected
                  "\tActual:" actual))
      (list 0 1)) ;; 0 errors, 1 failure
     (else (list 0 0)) ;; 0 errors, 0 failures
     ) ) )


(define (run-test-group-named name)
  (let ((test-cases-to-run '())
        (err-fail (list 0 0)))

    (let ((a (assoc (to-string name) *test-groups*)))
      (if a (set! test-cases-to-run (cdr a))))

    (show (list "TESTS NAMED:" name))
    (show (list "----- Running" (length test-cases-to-run)
                "tests... Failure output below. -----"))
    (set! err-fail
          (reduce (lambda (a b) (map + a b))
                  (map run-single-case test-cases-to-run)))
    (show (list "----- Done. "
                (car err-fail) "failed test(s). "
                (cadr err-fail) "error(s). -----" ))
    (newline)
    (cons (length test-cases-to-run) err-fail)
    ))

(define (run-tests-named . names)
  (if (empty? names)
      (apply run-tests-named
             (reverse (map car *test-groups*)))
      (let ((count-err-fail
             (reduce (lambda (a b) (map + a b))
                     (map run-test-group-named names))))
        (show (list "good:" (- (car count-err-fail)
                               (cadr count-err-fail)
                               (caddr count-err-fail))
                    "\tfailed:" (caddr count-err-fail)
                    "\terrors:" (cadr count-err-fail))))))



(define (run-test-cases2 . input)
  (let ((test-cases-to-run '())
        (get-name car)
        (get-expected cadr)
        (get-function caaddr)
        (get-expression caddr)
        (err-fail (list 0 0))
        )

    (define (string-starts-with? start full)
      (equal? (substring full 0 (string-length start))
              start))

    (cond ((null? input)
           (set! test-cases-to-run *xx-test-case-store*))
          ((procedure? (car input))
           (set! test-cases-to-run
                 (filter (lambda (case)
                           (and (list? (get-expression case))
                                (equal? (eval (get-function case))
                                        (car input))))
                         *xx-test-case-store*) )
           )
          (else  ; check names
           (let ((argstr (if (good-test-name? (car input))
                             (to-string (car input))
                             (error "Bad input to run-test-cases:"
                                    "not a procedure, symbol, number, or string!"))))
             (set! test-cases-to-run
                   (filter (lambda (case)
                             (string-starts-with? argstr (get-name case)))
                           *xx-test-case-store*) ) )) )

    (show (list "----- Running"
                (length test-cases-to-run)
                "tests... Failure output below. -----"))
    (set! err-fail
          (reduce (lambda (a b) (map + a b))
                  (map run-single-case test-cases-to-run)))
    (show (list "----- Done. "
                (car err-fail) "failed test(s). "
                (cadr err-fail) "error(s). -----" ))
    (newline)
    ))

