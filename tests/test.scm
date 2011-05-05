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
    (display (string-append
      (->string tests) " FAIL: "
        (->string code) " != " (->string expected) "\n"
        "  Actual result: '" (->string actual) "'\n")))))

(define success (lambda (code expected)
  (begin
    (test+1 (quote _))
    (display (string-append
      (->string tests) " OK: "
        (->string code) " == " (->string expected) "\n")))))

(define (test-eq code expected)
    (if (not (eq? (eval code)
                  (eval expected)))
        (fail code expected (eval code))
        (success code expected)))

(define results (lambda ()
    (display 
      (string-append
        (->string (- tests failed)) " / " (->string tests) " tests OK, "
          (->string failed) " failed\n"))))


