;; Testing simple recursion loop

(define (loop n)
  (display (string-append (number->string n) " "))
  (if (= n 1)
      (display "\n") ; stop
      (loop (- n 1))))

(display "Printing numbers from 200 to 1\n")
(loop 200)
(display "It works! Done\n")
