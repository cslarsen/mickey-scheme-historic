;; Testing simple recursion loop

(define loop
  (lambda (n)
    (display (string-append "n=" (->string n) "\n"))
    (if (< n 2) n
        (loop (- n 1)))))

(display "Printing numbers 1 to 100\n")
(loop 100)
(display "Done\n")
