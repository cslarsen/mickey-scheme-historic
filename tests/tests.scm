;;
;; Define some test functions
;;

(display "Setting variables\n")

(define tests 0)
(define failed 0)

(display "Creating 'fail'\n")

(define fail (lambda (code expected)
  (begin
;    (define tests (+ 1 tests))
;    (define failed (+ 1 failed))
    (display (string-append "FAILED: " (->string code) " != " (->string expected) "\n")))))

(display "Creating 'success'\n")

(define success (lambda (code expected)
  (begin
    ;(define tests (+ 1 tests))
    (display (string-append "OK: " (->string code) " == " (->string expected) "\n")))))

(display "Creating 'test'\n")

(define test-true (lambda (code expected)
  (begin
    (if (not (eq? (eval code) expected))
      (fail code expected)
      (success code expected)))))

;;
;; Perform actual tests
;;

(display "Running tests\n")

(test-true (quote (+ 1 2 3 4)) 10)
(test-true (quote (* 1 2 3 4)) 24)
