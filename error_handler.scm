;; Proper error handling in STk!
;;
;; (with-error-handler error-handler . expressions)
;; Will run expressions sequentially, until it encounters the first error.
;; At the first error, it will run error-handler on the error string.

;; There is a practical example in project3_gameloop.scm

;; Some simple example uses:

;; > (with-error-handler (lambda (e)
;;                            (display e))
;;                          (display "Nice message =D\n")
;;                          (/ 1 0))
;; WILL DISPLAY (no real return value):
;;  Nice message =D
;;  *** Error:
;;   /: not a valid number 0

;; > (with-error-handler (lambda (e)
;;                       100)
;;                     (/ 1 0))
;; => 100

;; > (with-error-handler (lambda (e)
;;                       100)
;;                     (* 1 2))
;; => 2


;;in case I mess something up (but this doesn't really happen anymore)
(define *old-error* error)
(define *old-report-error* report-error)

;; this took forever to figure out...
(define-macro (with-error-handler error-handler . body)
  (let ((result (gensym)))
    `(fluid-let ((error error)
                 (report-error report-error))
       (let ((,result (make-undefined))
             (args->string (lambda (args)
                             (if (and (not (null? args))
                                      (string-find? "~" (car args)))
                                 (apply format #f args)
                                 (accumulate (lambda (x y)
                                               (string-append x " " y))
                                             ""
                                             (map & args))))))
         (call/cc
          (lambda (c)
            (set! error
                  (lambda args
                    (let ((str (args->string args)))
                      (set! ,result (,error-handler str))
                      (c))))
            (set! report-error error)
            (set! ,result ,(cons 'begin body))))
         ,result))))
