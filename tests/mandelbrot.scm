;; The Mandelbrot set in ASCII
;;
;; Calculated in a straight-forward manner,
;; without any sort of optimizations whatsoever.
;;
;; This program thus serves as a good yardstick
;; for benchmarking the natural performance of various
;; Schemes.
;;
;; Note that this version is tailored for Mickey Schee,
;; meaning that it actually uses a subset of RnRS Scheme.
;;
;; It's been verified to work with Chicken Scheme.
;;
;; Made by Christian Stigen Larsen in 2011
;; Placed in the public domain.

;; Number of iterations, increase for more accurate
;; answer at a cost of slower computation
(define cutoff-steps 20)

;; Output screen sizes
(define screen-columns 78)
(define screen-rows    25)

;; Characters used for drawing
(define dot-char   "*")
(define space-char " ")

(load "complex.scm")

(define <=
  (lambda (x y)
    (or (= x y)
        (< x y))))

(define 1-
  (lambda (n)
    (- n 1)))

; C_{n+1} = C_{n}^2  + c, C_{0} = c
(define mandelbrot?-iter
  (lambda (z c nmax)
    (begin
      (if (zero? nmax)
        (> (complex-mag z) threshold)
        (if (> (complex-mag z) threshold)
          #t
          (mandelbrot?-iter (+complex c (*complex z z))
                            c (1- nmax)))))))

(define mandelbrot?
  (lambda (z)
    (mandelbrot?-iter (make-complex 0.0 0.0) z cutoff-steps)))

(define plot
  (lambda (x y)
    (display (string-append
      (if (mandelbrot? (make-complex x y))
          space-char dot-char)))))

;; Main

(define threshold 2.0) ;; should be fixed

(define x-start -2.0)
(define x-stop   1.0)
(define x-step   (/ (- x-stop x-start) (+ 0.5 (- screen-columns 1))))

(define y-start -1.0)
(define y-stop   1.0)
(define y-step   (/ (- y-stop y-start) (- screen-rows 1)))

(define x-loop
  (lambda (x y)
    (if (<= x x-stop)
      (begin
        (plot x y)
        (x-loop (+ x x-step) y))
      0)))

(define y-loop
  (lambda (y)
    (if (<= y y-stop)
      (begin
        (x-loop x-start y)
        (display "\n")
        (y-loop (+ y y-step)))
        0)))

(y-loop y-start)
