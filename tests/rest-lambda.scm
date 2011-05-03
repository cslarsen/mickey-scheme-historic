(define eat-all-params
  (lambda all-params
    (display (string-append
      "Here is a list: " (->string all-params) "\n"))))

(eat-all-params "one")
(eat-all-params "one" "two")
(eat-all-params "one" "two" "three")
