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

(define test-eq (lambda (code expected)
  (begin
    (if
      (not (eq? (eval code) (eval expected)))
        (fail code expected (eval code))
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
(test-eq (quote (string-append "a" "b" "b" "a")) "abba")
(test-eq (quote (->string 12345)) "12345")
(test-eq (quote (->string #f)) "#f")
(test-eq (quote (->string #t)) "#t")
(test-eq (quote (->string (eq? 1 2))) "#f")
(test-eq (quote (->string (eq? 3 (+ 1 2)))) "#t")
(test-eq (quote (->string (eq? 3 (+ 1 2 3)))) "#f")
(test-eq (quote (->string (list 1 2 (list 3 4)))) "(1 2 (3 4))")
(test-eq (quote (atom? (quote abba))) #t)

(display "\nResults\n")
(results (quote __dummy__))

(display "\n")
