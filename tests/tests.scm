;;
;; Define some test functions
;;

(define tests 0)
(define failed 0)

(define fail (lambda (code expected)
  (set! tests (+ 1 tests))
  (set! failed (+ 1 failed))
  (display (string-append "FAILED: " (->string code) " != " (->string expected) "\n"))))

(define success (lambda (code expected)
  (set! tests (+ 1 tests))
  (display (string-append "OK: " (->string code) " == " (->string expected) "\n"))))

(define test (lambda (code expected)
  (if (not (eq? (eval code) expected))
    (fail code expected)
    (success code expected))))

;;
;; Perform actual tests
;;

(test (quote (+ 1 2 3 4)) 10)
(test (quote (* 1 2 3 4)) 24)
