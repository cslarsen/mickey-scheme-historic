;;
;; Define some test functions
;;

(define tests 0)
(define failed 0)

(define fail (lambda (code expected)
    (set! tests (+ 1 tests)) // should be set! to get to global definitions
    (set! failed (+ 1 failed))
    (display (string-append "FAILED: " (->string code) " != " (->string expected) "\n"))))

(display "Creating 'success'\n")

(define success (lambda (code expected)
  (begin
    (set! tests (+ 1 tests))
    (display (string-append "OK: " (->string code) " == " (->string expected) "\n")))))

(display "Creating 'test'\n")

(define test-true (lambda (code expected)
  (begin
    (if (not (eq? (eval code) expected))
      (fail code expected)
      (success code expected)))))

(define test-results (lambda (__dummy__)
  (display 
    (string-append
      "Tests : " (->string tests) "\n"
      "OK    : " (->string (- tests failed)) "\n"
      "FAIL  : " (->string failed) "\n"))))

;;
;; Perform actual tests
;;

(display "Running tests\n")

(test-true (quote (+ 1 2 3 4)) 10)
(test-true (quote (* 1 2 3 4)) 24)

(test-results (quote __dummy__))
