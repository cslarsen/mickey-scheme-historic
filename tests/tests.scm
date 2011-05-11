;;
;; Define some test functions
;;

(load "complex.scm")
(load "test.scm")

;;
;; Perform actual tests
;;

(display "Tests\n")
(test-eq (quote (+ 1 2 3 4)) 10)
(test-eq (quote (* 1 2 3 4)) 24)
(test-eq (quote (- 1 2 3 4)) -8)
(test-eq (quote (string-append "a" "b" "b" "a")) "abba")
(test-eq (quote (number->string 12345)) "12345")
(test-eq (quote (boolean->string #f)) "#f")
(test-eq (quote (boolean->string #t)) "#t")
(test-eq (quote (boolean->string (eq? 1 2))) "#f")
(test-eq (quote (boolean->string (eq? 3 (+ 1 2)))) "#t")
(test-eq (quote (boolean->string (eq? 3 (+ 1 2 3)))) "#f")
(test-eq (quote (list->string (list 1 2 (list 3 4)))) "(1 2 (3 4))")
(test-eq (quote (apply + (list 1 2 3))) 6)
(test-eq (quote (apply + (quote (1 2 3)))) 6)
(test-eq (quote (apply + (list 1 2 3 (* 5 6)))) 36)
(test-eq (quote (apply + (list 1 2 3 (* 5 5)))) 31)
(test-eq (quote (complex->string
                  (*complex (make-complex 2 3)
                            (make-complex 5 7)))) "-11 + 29i")
(test-eq (quote (- 0.5 -1.5)) 2)
(test-eq (quote (- 0.5 -10.5)) 11)
(test-eq (quote (- 1 2 3)) -4)
(test-eq (quote (- 1)) -1)
(test-eq (quote (- 2)) -2)
(test-eq (quote (- -2)) 2)

;; (if t a1 a2)
(test-eq (quote (if (> 4 2) 11 22)) 11)
(test-eq (quote (if (> 3 2) 11 22)) 11)
(test-eq (quote (if (> 2 2) 11 22)) 22)
(test-eq (quote (if (> 1 2) 11 22)) 22)

;; (if t a1)
(test-eq (quote (if (> 3 2) 11)) 11)
(test-eq (quote (if (< 1 2) 11)) 11)

;; (reverse ...)
(test-eq (quote (reverse (list 1 2 3 4))) (list 4 3 2 1))
(test-eq (quote (reverse (list 1 2 3))) (list 3 2 1))
(test-eq (quote (reverse (list 1 2))) (list 2 1))
(test-eq (quote (reverse (list 1))) (list 1))

;; (abs <int>)
(test-eq (quote (abs -2)) 2)
(test-eq (quote (abs -1)) 1)
(test-eq (quote (abs 0)) 0)
(test-eq (quote (abs 1)) 1)
(test-eq (quote (abs 2)) 2)
;; (abs <float>)
(test-eq (quote (abs -2.1)) 2.1)
(test-eq (quote (abs -1.1)) 1.1)
(test-eq (quote (abs 0.1)) 0.1)
(test-eq (quote (abs -0.1)) 0.1)
(test-eq (quote (abs 1.1)) 1.1)
(test-eq (quote (abs 2.1)) 2.1)

;; assq
; TODO: Have to make test-eq a macro
;(test-eq (quote (assq (quote two) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (quote (list (quote two) 2)))
;(test-eq (quote (assq (quote one) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (quote (list (quote one) 1)))
;(test-eq (quote (assq (quote three) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (quote (list (quote three) 3)))
;(test-eq (quote (assq (quote threee) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) #f)

;; eqv? tests from R7RS, section 6.1
(test-eq (quote (eqv? (quote a) (quote a))) #t) ;; (eqv? 'a 'a)
(test-eq (quote (eqv? (quote a) (quote b))) #f)
(test-eq (quote (eqv? 2 2)) #t)
(test-eq (quote (eqv? 2 1)) #f)
(test-eq (quote (eqv? (list) (list))) #t) ;; (eqv? '() '())
(test-eq (quote (eqv? 100000000 100000000)) #t)
(test-eq (quote (eqv? (cons 1 2) (cons 1 2))) #f)
(test-eq (quote (eqv? (lambda () 1) (lambda () 2))) #f)
(test-eq (quote (eqv? #f (quote nil))) #f)
(test-eq (quote (let ((p (lambda (x) x)))
                  (eqv? p p))) #t)

;; unspecified tests for eqv
; (eqv? "" "")
; (eqv? '#() '#())
; (eqv? (lambda (x) x) (lambda (x) x))
; (eqv? (lambda (x) x) (lambda (y) y))

(display "\nResults\n")
(results)

(display "\n")
