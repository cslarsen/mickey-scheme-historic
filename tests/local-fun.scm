;; Test local function definitions

(define (outer o)
  (define (inner1 n)
    (display (string-append "Inner1: " n "\n")))
  (define (inner2 n)
    (display (string-append "Inner2: " n "\n")))
  (inner1 (string-append o "Inner1"))
  (inner2 (string-append o "Inner2"))
  (inner1 (string-append o "both!"))
  (inner2 (string-append o "both1")))

(outer "Hello, ")

;; This should NOT work
(inner1 "*If you see this, there is an error!*")
