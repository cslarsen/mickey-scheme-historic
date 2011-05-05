(define (square n)
  (* n n))

(define (distance x y)
  (sqrt (+ (* x x) (* y y))))

(display (string-append
  "12^2 = " (->string (square 12)) "\n")) ; should equal 144

(display (string-append
  "sqrt(5^2 + 4^2) = " (->string (distance 5 4)) "\n")) ; should equal 6.40312...