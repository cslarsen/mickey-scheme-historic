;; test syntax-rules with several patterns
;; from http://www.shido.info/lisp/scheme_syntax_e.html
;;

(define-syntax incf
  (syntax-rules ()
    ((_ x) (begin (set! x (+ x 1)) x))     ;; increment by one
    ((_ x i) (begin (set! x (+ x i)) x)))) ;; increment by i

(let ((i 0) (j 0))
  (incf i)
  (incf j 3)
  (display (list 'i '= i))
  (newline)
  (display (list 'j '= j))
  (newline))
; (i = 1)
; (j = 3)
; returns unspecified value

; Mickey bugs and returns
; (i = 1)
; (j = 1)