;; R7RS support for (case)

(display
  (case (* 2 3)
    ((2 3 5 7) 'prime)
    ((1 4 6 8 9) 'composite))) ; ==> composite
(newline)

(display
  (case (car '(c d))
    ((a) 'a)
    ((b) 'b))) ; ==> unspecified
(newline)

(display
  (case (car '(c d))
    ((a e i o u) 'vowel)
    ((w y) 'semivowel)
    (else => (lambda (x) (list x x x))))) ; ==> (c c c)
(newline)
