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
;(test-eq (quote (boolean->string #f)) "#f")
;(test-eq (quote (boolean->string #t)) "#t")
;(test-eq (quote (boolean->string (eq? 1 2))) "#f")
;(test-eq (quote (boolean->string (eq? 3 (+ 1 2)))) "#t")
;(test-eq (quote (boolean->string (eq? 3 (+ 1 2 3)))) "#f")
(test-eq (quote (list 1 2 (list 3 4))) (list 1 2 (list 3 4)))
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
(test-eq (quote (assq (quote three) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (list (quote three) 3))
(test-eq (quote (assq (quote two) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (list (quote two) 2))
(test-eq (quote (assq (quote threee) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) #f)

;; even, odd
(test-eq (quote (even? 0)) #t) ;; correct in MIT Scheme
(test-eq (quote (even? 1)) #f)
(test-eq (quote (even? 2)) #t)
(test-eq (quote (even? 3)) #f)
(test-eq (quote (even? 4)) #t)
(test-eq (quote (even? 9)) #f)
(test-eq (quote (even? 100)) #t)
(test-eq (quote (even? 1000)) #t)
(test-eq (quote (odd? 0)) #f) ;; correct in MIT Scheme
(test-eq (quote (odd? 1)) #t)
(test-eq (quote (odd? 2)) #f)
(test-eq (quote (odd? 3)) #t)
(test-eq (quote (odd? 4)) #f)
(test-eq (quote (odd? 9)) #t)
(test-eq (quote (odd? 100)) #f)
(test-eq (quote (odd? 1000)) #f)

;; positive, negative
(test-eq (quote (negative? 0)) #f)
(test-eq (quote (negative? 1)) #f)
(test-eq (quote (negative? 2)) #f)
(test-eq (quote (negative? 3)) #f)
(test-eq (quote (negative? -1)) #t)
(test-eq (quote (negative? -2)) #t)
(test-eq (quote (negative? -3)) #t)
;;
(test-eq (quote (positive? 0)) #f)
(test-eq (quote (positive? 1)) #t)
(test-eq (quote (positive? 2)) #t)
(test-eq (quote (positive? 3)) #t)
(test-eq (quote (positive? -1)) #f)
(test-eq (quote (positive? -2)) #f)
(test-eq (quote (positive? -3)) #f)

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

;; round
(test-eq (quote (round 1)) 1)
(test-eq (quote (round 2)) 2)
(test-eq (quote (round 0.9)) 1.0)
(test-eq (quote (round 1.0)) 1.0)
(test-eq (quote (round 1.1)) 1.0)
(test-eq (quote (round 1.2)) 1.0)
(test-eq (quote (round 1.3)) 1.0)
(test-eq (quote (round 1.4)) 1.0)
(test-eq (quote (round 1.5)) 2.0)
(test-eq (quote (round 1.6)) 2.0)
(test-eq (quote (round 2.49)) 2.0)
(test-eq (quote (round 2.50)) 3.0) ;; NOTE: Chicken and MIT Scheme reports 2! IEEE-754 magic or?
(test-eq (quote (round 2.51)) 3.0)

;; truncate
(test-eq (quote (truncate 1.1)) 1.0)
(test-eq (quote (truncate 1.4)) 1.0)
(test-eq (quote (truncate 1.5)) 1.0)
(test-eq (quote (truncate 1.6)) 1.0)
(test-eq (quote (truncate 1.9)) 1.0)
(test-eq (quote (truncate 1)) 1)
(test-eq (quote (truncate 2)) 2)

(display "\nResults\n")
(results)

(display "\n")
