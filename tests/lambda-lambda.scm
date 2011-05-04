; Make a function that multiplies any number with `x`
(define (make-mul-fun x)
  (lambda (any-number)
    (* x any-number)))

; Test it
(display (string-append "Should be 30: " (->string ((make-mul-fun 3) 10)) "\n"))
(display (string-append "Should be 20: " (->string ((make-mul-fun 2) 10)) "\n"))
(display (string-append "Should be 30: " (->string ((make-mul-fun 10) 3)) "\n"))

(define mul3 (make-mul-fun 3))
(define mul7 (make-mul-fun 7))

(display (string-append "Should be 18: " (->string (mul3 6)) "\n"))
(display (string-append "Should be 77: " (->string (mul7 11)) "\n"))
