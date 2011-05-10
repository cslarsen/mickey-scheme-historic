;; Library for performing tests

(define tests 0)
(define test+1
  (lambda (_foo)
    (set! tests (+ tests 1))))

(define failed 0)
(define fail+1
  (lambda (_foo)
    (set! failed (+ failed 1))))

(define fail (lambda (code expected actual)
  (begin
    (test+1 (quote _))
    (fail+1 (quote _))
    (display tests) (display " FAIL: ")
    (display code) (display " != ") (display expected) (newline)
    (display "  Actual result: '") (display actual) (display "'") (newline))))

(define success (lambda (code expected)
  (begin
    (test+1 (quote _))
    (display tests) (display " OK: ")
    (display code) (display " == ") (display expected) (newline))))

(define (test-eq code expected)
    (if (not (eq? (eval code)
                  (eval expected)))
        (fail code expected (eval code))
        (success code expected)))

(define results (lambda ()
    (display 
      (string-append
        (number->string (- tests failed)) " / " (number->string tests) " tests OK, "
          (number->string failed) " failed\n"))))


