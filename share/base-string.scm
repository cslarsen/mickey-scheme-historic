;; This file is part of Mickey Scheme (R7RS)
;; by Christian Stigen Larsen

#|
 | R7RS string-map
 |
 | Explicitly require at least one string.
 |#
(define (string-map proc first-string . remaining-strings)
  (let*
    ((output  '())
     (input   (cons first-string remaining-strings))
     (args    (length input))
     (strings (map string->list input)))
    (let loop
      ((chars (map car strings))
       (rest  (map cdr strings)))
      (if (= args (length chars))
        (begin
          (set! output (cons (apply proc chars) output))
          (if (> (apply * (map length rest)) 0)
            (loop 
              (map car rest)
              (map cdr rest))))))
    (list->string (reverse output))))

#|
 | R7RS string-for-each
 |
 | If n strings are given as input, then `proc` must take n parameters.
 |
 | Explicitly requires at least one string.
 |#
(define (string-for-each proc first-string . remaining-strings)
  (let*
    ((input   (cons first-string remaining-strings))
     (args    (length input))
     (strings (map string->list input)))
    (let loop
      ((chars (map car strings))
       (rest  (map cdr strings)))
      (if (= args (length chars))
        (begin
          (if (> (apply * (map length rest)) 0)
            (begin
              (apply proc chars)
              (loop 
                (map car rest)
                (map cdr rest)))))))))
