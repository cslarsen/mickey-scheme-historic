;; Mickey Scheme
;;
;; Test code from SICP, taken from
;; http://mitpress.mit.edu/sicp/code/index.html

(load "test.scm")

(display "Tests from\n")
(display "Structure and Interpretation of Comptuter Programs (SICP)\n\n")

;; ---

(display "SICP SECTION 1.1.1\n")

(test-eq (quote 486) 486)
(test-eq (quote (+ 137 349)) 486)
(test-eq (quote (- 1000 334)) 666)
(test-eq (quote (* 5 99)) 495)
(test-eq (quote (/ 10 5)) 2)
(test-eq (quote (+ 2.7 10)) 12.7)
(test-eq (quote (+ 21 35 12 7)) 75)
(test-eq (quote (* 25 4 12)) 1200)
(test-eq (quote (+ (* 3 5) (- 10 6))) 19)
(test-eq (quote (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))) 57)
(test-eq (quote 
 (+ (* 3
       (+ (* 2 4)
          (+ 3 5)))
    (+ (- 10 7)
       6)))
  57) ; expected

;; ---

(display "\nSICP SECTION 1.1.2\n")

(define size 2)
(test-eq (quote size) 2)
(test-eq (quote (* 5 size)) 10)

(define pi 3.14159)
(define radius 10)
(test-eq (quote (* pi (* radius radius))) 314.158997)

(define circumference (* 2 pi radius))
(test-eq (quote circumference) 62.831802)

;; ---

(display "\nSICP SECTION 1.1.3\n")

(test-eq (quote (* (+ 2 (* 4 6)))) 26)
(test-eq (quote (+ 3 5 7)) 15)

;; ---

(display "\nSICP SECTION 1.1.4\n")

(define (square x) (* x x))
(test-eq (quote (square 21)) 441)
(test-eq (quote (square (+ 2 5))) 49)
(test-eq (quote (square (square 3))) 81)

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(test-eq (quote (sum-of-squares 3 4)) 25)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(test-eq (quote (f 5)) 136)

;; ---

(display "\nSICP SECTION 1.1.5\n")

(test-eq (quote (f 5)) 136)
(test-eq (quote (sum-of-squares (+ 5 1) (* 5 2))) 136)
(test-eq (quote (+ (square 6) (square 10))) 136)
(test-eq (quote (+ (* 6 6) (* 10 10))) 136)
(test-eq (quote (+ 36 100)) 136)

(test-eq (quote (f 5)) 136)
(test-eq (quote (sum-of-squares (+ 5 1) (* 5 2))) 136)
(test-eq (quote (+    (square (+ 5 1))      (square (* 5 2))  )) 136)
(test-eq (quote (+    (* (+ 5 1) (+ 5 1))   (* (* 5 2) (* 5 2)))) 136)
(test-eq (quote (+         (* 6 6)             (* 10 10))) 136)
(test-eq (quote (+           36                   100)) 136)
(test-eq (quote                     136) 136)

(display "\nRESULTS\n")
(results)
