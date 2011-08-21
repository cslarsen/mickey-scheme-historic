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

;; min
(test-eq (quote (min 1 2 3)) 1)
(test-eq (quote (min 4 2 3)) 2)
(test-eq (quote (min 4.2 2 3)) 2)
(test-eq (quote (min 4.2 2.3 3)) 2.3)
(test-eq (quote (min 4.2)) 4.2)

;; max
(test-eq (quote (max 1 2 3)) 3)
(test-eq (quote (max 4 2 3)) 4)
(test-eq (quote (max 4.2 2 3)) 4.2)
(test-eq (quote (max 4.2 2.3 3)) 4.2)
(test-eq (quote (max 4.2)) 4.2)

;; expt
(test-eq (quote (expt 2 0)) 1)
(test-eq (quote (expt 2 1)) 2)
(test-eq (quote (expt 2 2)) 4)
(test-eq (quote (expt 2 3)) 8)
(test-eq (quote (expt 3 3)) 27)

;; char-whitespace
(test-eq (quote (char-whitespace? #\a)) #f)
(test-eq (quote (char-whitespace? #\b)) #f)
(test-eq (quote (char-whitespace? #\c)) #f)
; TODO: Test for #\tab and friends

;; modulo
(test-eq (quote (modulo 10 6)) 4)
(test-eq (quote (modulo 10 5)) 0)
(test-eq (quote (modulo 10 4)) 2)
(test-eq (quote (modulo 10 3)) 1)
(test-eq (quote (modulo 10 2)) 0)
(test-eq (quote (modulo 10 1)) 0)
; TODO: Test negative modulo, (modulo 10 -3)

;; char functions
(test-eq (quote (char->integer #\a)) 97)
(test-eq (quote (char->integer #\b)) 98)
(test-eq (quote (char->integer #\A)) 65)
;;
(test-eq (quote (char-alphabetic? #\a)) #t)
(test-eq (quote (char-alphabetic? #\A)) #t)
(test-eq (quote (char-alphabetic? #\2)) #f)
(test-eq (quote (char-alphabetic? #\8)) #f)
;;
(test-eq (quote (char-lower-case? #\8)) #f)
(test-eq (quote (char-lower-case? #\Z)) #f)
(test-eq (quote (char-lower-case? #\A)) #f)
(test-eq (quote (char-lower-case? #\H)) #f)
(test-eq (quote (char-lower-case? #\z)) #t)
(test-eq (quote (char-lower-case? #\a)) #t)
(test-eq (quote (char-lower-case? #\h)) #t)
;;
(test-eq (quote (char-upper-case? #\8)) #f)
(test-eq (quote (char-upper-case? #\Z)) #t)
(test-eq (quote (char-upper-case? #\A)) #t)
(test-eq (quote (char-upper-case? #\H)) #t)
(test-eq (quote (char-upper-case? #\z)) #f)
(test-eq (quote (char-upper-case? #\a)) #f)
(test-eq (quote (char-upper-case? #\h)) #f)
;;
(test-eq (quote (char-numeric? #\8)) #t)
(test-eq (quote (char-numeric? #\Z)) #f)
(test-eq (quote (char-numeric? #\A)) #f)
(test-eq (quote (char-numeric? #\a)) #f)
(test-eq (quote (char-numeric? #\0)) #t)
(test-eq (quote (char-numeric? #\1)) #t)
(test-eq (quote (char-numeric? #\9)) #t)
;;
(test-eq (quote (char-<=? #\9 #\0)) #f)
(test-eq (quote (char-<=? #\3 #\5)) #t)
(test-eq (quote (char-<=? #\a #\z)) #t)
(test-eq (quote (char-<=? #\a #\a)) #t)
(test-eq (quote (char-<=? #\h #\e)) #f)
(test-eq (quote (char-<=? #\r #\w)) #t)
;;
(test-eq (quote (char->=? #\9 #\0)) #t)
(test-eq (quote (char->=? #\3 #\5)) #f)
(test-eq (quote (char->=? #\a #\z)) #f)
(test-eq (quote (char->=? #\a #\a)) #t)
(test-eq (quote (char->=? #\h #\e)) #t)
(test-eq (quote (char->=? #\r #\w)) #f)
;;
(test-eq (quote (char->? #\9 #\0)) #t)
(test-eq (quote (char->? #\3 #\5)) #f)
(test-eq (quote (char->? #\a #\a)) #f)
(test-eq (quote (char->? #\h #\e)) #t)
(test-eq (quote (char->? #\r #\w)) #f)
;;
(test-eq (quote (char-=? #\9 #\0)) #f)
(test-eq (quote (char-=? #\3 #\5)) #f)
(test-eq (quote (char-=? #\a #\a)) #t)
(test-eq (quote (char-=? #\4 #\4)) #t)
(test-eq (quote (char-=? #\h #\e)) #f)
(test-eq (quote (char-=? #\r #\w)) #f)

;; conversion
(test-eq (quote (integer->char 65)) (quote #\A))
(test-eq (quote (integer->char 97)) (quote #\a))
(test-eq (quote (list->string (list #\a #\b #\c))) "abc")
(test-eq (quote (list->string (list #\a #\b #\b #\A))) "abbA")

(test-eq (quote (list-tail (list 1 2 3) 0)) (list 1 2 3))
(test-eq (quote (list-tail (list 1 2 3) 1)) (list 2 3))
(test-eq (quote (list-tail (list 1 2 3) 2)) (list 3))
(test-eq (quote (list-tail (list 1 2 3) 3)) (list))

;; member
(test-eq (quote (member 10 (list 1 2 3))) #f)
(test-eq (quote (member 10 (list 10 20 30))) (list 10 20 30))
(test-eq (quote (member 20 (list 10 20 30))) (list 20 30))
(test-eq (quote (member 20 (list 10 20 30 (quote bee) (quote cee)))) (list 20 30 (quote bee) (quote cee)))
(test-eq (quote (member 30 (list 10 20 30))) (list 30))
(test-eq (quote (member 40 (list 10 20 30))) #f)

;; memv (TODO: insert eqv? specific check)
(test-eq (quote (memv 10 (list 1 2 3))) #f)
(test-eq (quote (memv 10 (list 10 20 30))) (list 10 20 30))
(test-eq (quote (memv 20 (list 10 20 30))) (list 20 30))
(test-eq (quote (memv 20 (list 10 20 30 (quote bee) (quote cee)))) (list 20 30 (quote bee) (quote cee)))
(test-eq (quote (memv 30 (list 10 20 30))) (list 30))
(test-eq (quote (memv 40 (list 10 20 30))) #f)

;; memq (TODO: insert eq? specific check)
(test-eq (quote (memq 10 (list 1 2 3))) #f)
(test-eq (quote (memq 10 (list 10 20 30))) (list 10 20 30))
(test-eq (quote (memq 20 (list 10 20 30))) (list 20 30))
(test-eq (quote (memq 20 (list 10 20 30 (quote bee) (quote cee)))) (list 20 30 (quote bee) (quote cee)))
(test-eq (quote (memq 30 (list 10 20 30))) (list 30))
(test-eq (quote (memq 40 (list 10 20 30))) #f)

;; gcd, lcm
(test-eq (quote (gcd 10 2)) 2)
(test-eq (quote (gcd 10 3)) 1)
(test-eq (quote (gcd 10 5)) 5)
(test-eq (quote (gcd 5 10)) 5)
(test-eq (quote (gcd 1230 4560)) 30)
(test-eq (quote (lcm 4 6)) 12)
(test-eq (quote (lcm 6 4)) 12)

;; list-ref
(test-eq (quote (list-ref (list 1 2 3) 2)) 3)
(test-eq (quote (list-ref (list 1 2 3) 1)) 2)
(test-eq (quote (list-ref (list 1 2 3) 0)) 1)
(test-eq (quote (list-ref (list 1 2 3 4) 0)) 1)
(test-eq (quote (list-ref (list 1 2 3 4) 3)) 4)

;; string conversion
(test-eq (quote (string->list "hey")) (list #\h #\e #\y))
(test-eq (quote (string->symbol "hey")) (quote hey))
(test-eq (quote (string->number "abba")) #f)
(test-eq (quote (string->number "123")) 123)
(test-eq (quote (string->number "456")) 456)
(test-eq (quote (string->number "1.2")) 1.2)
(test-eq (quote (string->number "1.5")) 1.5)
(test-eq (quote (string-length "abba")) 4)
(test-eq (quote (string-length "abb")) 3)
(test-eq (quote (string-length "ab")) 2)
(test-eq (quote (string-length "a")) 1)
(test-eq (quote (string-length "")) 0)
;;
(test-eq (quote (substring "Hello!" 0 0)) "")
(test-eq (quote (substring "Hello!" 0 1)) "H")
(test-eq (quote (substring "Hello!" 0 3)) "Hel")
(test-eq (quote (substring "Hello!" 1 3)) "ell")
(test-eq (quote (substring "Hello!" 1 3)) "ell")
(test-eq (quote (substring "Hello!" 1 4)) "ello")
(test-eq (quote (substring "Hello!" 2 4)) "llo!")
;;
(test-eq (quote (string=? "hey" "hey")) #t)
(test-eq (quote (string=? "hey" "heya")) #f)
(test-eq (quote (string<=? "hey" "heya")) #t)
;;
(test-eq (quote (string-ref "hey" 0)) #\h)
(test-eq (quote (string-ref "hey" 1)) #\e)
(test-eq (quote (string-ref "hey" 2)) #\y)

;; append tests
(test-eq (quote (append (list) 1)) (1))
(test-eq (quote (append (list) 1)) (1))
(test-eq (quote (append (list) (list 1 2))) (list 1 2))
(test-eq (quote (append (list) (list 1 2))) (list 1 2))
(test-eq (quote (append (list 1))) (list 1))
(test-eq (quote (append (list 1))) (list 1))
(test-eq (quote (append (list 1) 2)) (cons 1 2))
(test-eq (quote (append (list 1) (list 3))) (list 1 3))
(test-eq (quote (append (list 1) (list 3 4))) (list 1 3 4))
(test-eq (quote (append (list 1) (list 3 4) 5)) (cons 1 (cons 3 (cons 4 5))))
(test-eq (quote (append (append (list 1 2 3) (list 4)) (list 5 6) 7)) (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 7)))))))

;; Regression test: append should not mutate
(define a (list 1))
(append a 2)
(append a 3)
(test-eq (quote a) (list 1))

;; Quoting
(test-eq (quote 'hey) (quote hey))
(test-eq (quote 'hey) 'hey)
(test-eq (quote ''hey) ''hey)
;
(test-eq (quote (append '(1 2 3) 4)) (cons 1 (cons 2 (cons 3 4))))
(test-eq (quote (apply + '(1 2 3))) 6)
(test-eq '(apply + '(1 2 3)) 6) ; is double-quote ok?
;
(test-eq (quote (quasiquote (1 2 (unquote (+ 3 4))))) '(1 2 7))
(test-eq (quote `(1 2 ,(+ 3 4) 3 y)) '(1 2 7 3 y))

;; Module math
(test-eq '(ceiling 3.0) 3.0)
(test-eq '(ceiling 3.1) 4.0)
(test-eq '(ceiling 3.4) 4.0)
(test-eq '(ceiling 3.5) 4.0)
(test-eq '(ceiling 3.6) 4.0)
;
(test-eq '(floor 3.0) 3.0)
(test-eq '(floor 3.1) 3.0)
(test-eq '(floor 3.4) 3.0)
(test-eq '(floor 3.5) 3.0)
(test-eq '(floor 3.6) 3.0)
(test-eq '(floor 3.9) 3.0)
(test-eq '(floor 3.999) 3.0)
;
(test-eq '(sqrt 3.999) 1.99975)
(test-eq '(sqrt 4.0) 2.0)
(test-eq '(sqrt -4.0) (sqrt -1)) ; nan
;
(test-eq '(exp 1) 2.71828)
(test-eq '(exp 2) 7.38906)

(display "\nResults\n")
(results)

(display "\n")
