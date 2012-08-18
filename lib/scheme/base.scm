#|
 | Mickey Scheme R7RS base library
 |
 | Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
 | http://csl.sublevel3.org                              _
 |                                                        \
 | Distributed under the LGPL 2.1; see LICENSE            /\
 | Please post bugfixes and suggestions to the author.   /  \_
 |
 |#

#|
 | Example: (when #t (display "hey\n") 123) prints "hey\n" and returns 123.
 |#
(define-syntax when
  (syntax-rules ()
    ((when test expr ...)
     (if test (begin expr ...)))))

#|
 | Example: (unless #f 123) returns 123
 |#
(define-syntax unless
  (syntax-rules ()
    ((unless test expr ...)
     (if (not test) (begin expr ...)))))

(define (caar s) (car (car s)))
(define (cadr s) (car (cdr s)))
(define (cdar s) (cdr (car s)))
(define (cddr s) (cdr (cdr s)))
(define (caaar s) (car (car (car s))))
(define (caadr s) (car (car (cdr s))))
(define (cadar s) (car (cdr (car s))))
(define (caddr s) (car (cdr (cdr s))))
(define (cdaar s) (cdr (car (car s))))
(define (cdadr s) (cdr (car (cdr s))))
(define (cddar s) (cdr (cdr (car s))))
(define (cdddr s) (cdr (cdr (cdr s))))
(define (caaaar s) (car (car (car (car s)))))
(define (caaadr s) (car (car (car (cdr s)))))
(define (caadar s) (car (car (cdr (car s)))))
(define (caaddr s) (car (car (cdr (cdr s)))))
(define (cadaar s) (car (cdr (car (car s)))))
(define (cadadr s) (car (cdr (car (cdr s)))))
(define (caddar s) (car (cdr (cdr (car s)))))
(define (cadddr s) (car (cdr (cdr (cdr s)))))
(define (cdaaar s) (cdr (car (car (car s)))))
(define (cdaadr s) (cdr (car (car (cdr s)))))
(define (cdadar s) (cdr (car (cdr (car s)))))
(define (cdaddr s) (cdr (car (cdr (cdr s)))))
(define (cddaar s) (cdr (cdr (car (car s)))))
(define (cddadr s) (cdr (cdr (car (cdr s)))))
(define (cdddar s) (cdr (cdr (cdr (car s)))))
(define (cddddr s) (cdr (cdr (cdr (cdr s)))))

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

#|
 | This is a naive, straight-forward implementation.
 |
 | TODO: for-each is supposed to handle circular lists,
 |       as long as not all of them are circular.
 |
 |       This implementation will NOT handle that situation,
 |       as it will go into an infinite loop, instead of
 |       raising an error.
 |
 |       I think it basically means that we have to check
 |       for circularity on each input.
 |#
(define (for-each procedure list1 . etc)
  (let*
    ((lists (cons list1 etc))
     (count (length lists)))
    (let loop
      ((arguments (map car lists))
       (remaining (map cdr lists)))
      ;;
      ;; terminate when the shortest list is finished
      (if (= (length arguments) count)
        (begin
          ;; call procedure with input parameters
          (apply procedure arguments)

          ;; ... and keep going
          (loop (map car remaining)
                (map cdr remaining)))))))

;; Code taken from R7RS draft
(define (values . things)
  (call-with-current-continuation
    (lambda (cont) (apply cont things))))
