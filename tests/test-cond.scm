(define (test-cond x y)
  (cond ((> x y) (quote greater))
        ((< x y) (quote less))
        ( else   (quote equal))))

(define (test x y)
  (display (string-append
    (->string x) " and " (->string y) ": "
    (->string (test-cond x y)) "\n")))

(test 3 2)
(test 2 3)
(test 2 2)
