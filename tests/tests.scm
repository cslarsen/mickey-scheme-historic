;;
;; Define some test functions
;;


(define tests 0)
(define test+1
  (lambda (_foo)
    (set! tests (+ tests 1))))

(define failed 0)
(define fail+1
  (lambda (_foo)
    (set! failed (+ failed 1))))

(define fail (lambda (code expected)
  (begin
    (test+1 (quote _))
    (fail+1 (quote _))
    (display (string-append
      (->string tests) " FAIL: "
        (->string code) " != " (->string expected) "\n")))))

(define success (lambda (code expected)
  (begin
    (test+1 (quote _))
    (display (string-append
      (->string tests) " OK: "
        (->string code) " == " (->string expected) "\n")))))

(define test-eq (lambda (code expected)
  (begin
    (if
      (not (eq? (eval code) expected))
        (fail code expected)
        (success code expected)))))

(define results (lambda (__dummy__)
  (begin
    (display 
      (string-append
        (->string (- tests failed)) " / " (->string tests) " tests OK, "
          (->string failed) " failed\n")))));

;;
;; Perform actual tests
;;

(display "Tests\n")
(test-eq (quote (+ 1 2 3 4)) 10)
(test-eq (quote (* 1 2 3 4)) 24)
(test-eq (quote (- 1 2 3 4)) -8)

(display "\nResults\n")
(results (quote __dummy__))

(display "\n")
