;; Testing simple recursion loop

(define (loop n)
  (display (string-append (->string n) " "))
  (if (= n 1)
      (display "\n") ; stop
      (loop (- n 1))))

(display "Printing numbers from 100 to 1\n")
(loop 100)
(display "Done\n")
