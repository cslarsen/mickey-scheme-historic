;; Testing simple recursion loop

(define (loop n)
  (display (string-append (number->string n) " "))
  (if (= n 1)
      (display "\n") ; stop
      (loop (- n 1))))

(display "Printing numbers from 100 to 1\n")
(loop 200) ; current Mickey Scheme chokes at 100 recursions...
(display "It works! Done\n")
