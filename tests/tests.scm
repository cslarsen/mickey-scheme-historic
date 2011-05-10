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
(test-eq (quote (atom? (quote abba))) #t)
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

;; alist?
(test-eq (quote (alist? (list (list (quote a) 1) (list (quote b) 2) (list (quote c) 3)))) #t)
(test-eq (quote (alist? (list))) #t) ; empty lists are alist too
(test-eq (quote (alist? (list (list)))) #f) ; but inner-list must have at least one element
(test-eq (quote (alist? (list (list 1)))) #t) ; but inner-list must have at least one element
(test-eq (quote (alist? (list (list 1 2 3)))) #t) ; can have more too, it seems
(test-eq (quote (alist? (list (list (quote a) 1 2) (list (quote b) 2) (list (quote c) 3)))) #t)
(test-eq (quote (alist? (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) #t)

;; assq
; TODO: Have to make test-eq a macro
;(test-eq (quote (assq (quote two) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (quote (list (quote two) 2)))
;(test-eq (quote (assq (quote one) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (quote (list (quote one) 1)))
;(test-eq (quote (assq (quote three) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (quote (list (quote three) 3)))
;(test-eq (quote (assq (quote threee) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) #f)

(display "\nResults\n")
(results)

(display "\n")
