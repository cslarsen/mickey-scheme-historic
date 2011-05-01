;; Fibonacci sequence, slow, recursive version

(define fib
  (lambda (n)
    (if (= n 0) 0
      (if (= n 1) 1
        (+ (fib (- n 1))
           (fib (- n 2)))))))

(define calc
  (lambda (n)
    (display (string-append
      "fib(" (->string n) ") = " (->string (fib n)) "\n"))))

(calc 0)
(calc 1)
(calc 2)
(calc 3)
(calc 4)
(calc 5)
(calc 6)
(calc 7)
(calc 8)
(calc 9)
(calc 10)
