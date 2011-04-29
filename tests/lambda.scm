; simple definition
(define square
  (lambda (x)
    (* x x)))

; print some squares
(display
  (string-append
    "12*12 = " (->string (square 12)) "\n"
    " 6*6  =  " (->string (square 6)) "\n"
    " 3*3  =  " (->string (square 3)) "\n"))
