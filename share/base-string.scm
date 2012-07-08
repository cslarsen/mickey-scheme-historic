;; Mickey Scheme

#|
 | R7RS string-map
 |#
(define (string-map proc . strings)
  (let
    ((output  '())
     (args    (length strings))
     (strings (map string->list strings)))
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
