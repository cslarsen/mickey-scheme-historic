; Test simple math expressions, as well as implicit (begin ...)

; Simple math
(display (string-append
  "3*4*5*6 = " (->string (* 3 4 5 6)) "\n"))

; Another expression
(display (string-append
  "(1+2+3)*5 = " (->string (* (+ 1 2 3) 5)) "\n"))
