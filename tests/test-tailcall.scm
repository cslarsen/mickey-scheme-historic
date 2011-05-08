(define (my-loop body count)
  (if (> count 0)
    (begin
      (body)
      (my-loop body (- count 1)))
    0))

(define (do-something)
  (display ""))

(display "Let's try recursing 30000 times\n")
(my-loop do-something 30000)
(display "Well, that worked out alright!\n")
