;; Function currying in Scheme

; Let's make simple function

(define (multiply x y)
  (* x y))

; We USED to be able to do the following in Mickey Scheme:
;
; (define times3
;   (multiply 3))
;
; But after we implicitly wrapped the define <body>
; in (begin <body>) it is no longer possible.  Besides,
; it's not standard Scheme, and when we get support for
; macros we can create macros that will automatically
; allow for currying anywa (ahh... macros)
;
; So, let's do it explicitly instead, taken from
; http://www.engr.uconn.edu/~jeffm/Papers/curry.html

(define (curry function . arguments)
  (lambda x
    (apply function (append arguments x))))

(define times3
  (curry multiply 3))

; Why is the above (curry multiply 3) not completely lame?
; Because you could potentially use n-arity higher order
; functions, for instance in map, without explicitly writing
; lambda.

; ... and try it!
(define test
  (lambda (x)
    (display (string-append
      (->string x) "*3 = " (->string (times3 x)) "\n"))))

(display "Testing currying:\n")
(test 1)
(test 2)
(test 3)
(test 4)
(test 5)
(test 6)
