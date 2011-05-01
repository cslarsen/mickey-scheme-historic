
; Let's make simple function
(define multiply
  (lambda (x y)
    (* x y)))

; Now curry it
(define times3
  (multiply 3))

; ... and try it!
(define test
  (lambda (x)
    (display (string-append
      (->string x) "*3 = " (->string (times3 x)) "\n"))))

(display "Testing currying:\n")
(test 1)
(test 2)
(test 3)
(test 4)
(test 5)
(test 6)
