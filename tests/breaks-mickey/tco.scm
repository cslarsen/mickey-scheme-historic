;; Tail call elimination
(define (loop-forever)
  (display ".") (loop-forever))

(display "We're not going to loop forever in a tail-call\n\n")

(display "Proper Schemes will eliminate the tail call,\n")
(display "so if you get a segfault below, your scheme is not scheme.\n")
(display "Ready, set, go!\n")

(loop-forever)

(display "This line should never execute\n")
