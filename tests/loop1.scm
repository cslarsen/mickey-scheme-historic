;; Testing simple recursion loop

(define loop
  (lambda (n)
    (display (string-append "n=" (->string n) "\n"))
    (if (< n 2) n
        (loop (- n 1)))))

(loop 3)
