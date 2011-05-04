;; Test of different variadic lambda forms

(define no-args
  (lambda ()
    (display "no-args\n")))

(define one-arg
  (lambda (x)
    (display (string-append
      "one-arg, with x='" x "'\n"))))

(define one-arg-rest
  (lambda (x . rest)
    (display (string-append
      "one-arg-rest with x='" x "' and rest='" (->string rest) "'\n"))))

(define two-arg-rest
  (lambda (x y . rest)
    (display (string-append
      "two-arg-rest with x='" x "' y='" y "' and rest='" (->string rest) "'\n"))))

(define rest-arg
  (lambda rest
    (display (string-append
      "rest with rest='" (->string rest) "'\n"))))

(no-args)
(one-arg "Uno!")

(one-arg-rest "Uno" "dos!")
(one-arg-rest "Uno" "dos" "tres!")
(one-arg-rest "Uno" "dos" "tres" "cuatro!")

(two-arg-rest "Uno" "dos!")
(two-arg-rest "Uno" "dos" "tres!")
(two-arg-rest "Uno" "dos" "tres" "cuatro!")

(rest-arg "Uno!")
(rest-arg "Uno" "dos!")
(rest-arg "Uno" "dos" "tres!")
(rest-arg) ; zero

(display "Fin!\n")
