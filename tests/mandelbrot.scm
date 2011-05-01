(load "complex.scm")

(define zero?
  (lambda (p)
    (= p 0)))

(define 1-
  (lambda (n)
    (- n 1)))

; C_{n+1} = C_{n}^2  + c, C_{0} = c
(define mandelbrot-iter
  (lambda (c nmax)
    (begin
      (if (zero? nmax)
       c
       (mandelbrot-iter (+complex (*complex c c) c)
                        (1- nmax))))))

(define mandelbrot?
  (lambda (z)
    (> (complex-mag (mandelbrot-iter z cutoff-steps))
       threshold)))

(define plot
  (lambda (x y)
    (display (string-append
      (if (mandelbrot? (make-complex x y))
          "*" " ")))))

;; Main

(define threshold 999999999999.9)
(define cutoff-steps 13)

(define screen-columns 78)
(define screen-rows 22)

(define x-start -2.0)
(define x-stop   1.0)
(define x-step   (/ (- x-stop x-start) screen-columns))

(define y-start -1.0)
(define y-stop   1.0)
(define y-step   (/ (- y-stop y-start) screen-rows))

(define x-loop
  (lambda (x y)
    (if (< x x-stop)
      (begin
        (plot x y)
        (x-loop (+ x x-step) y))
      0)))

(define y-loop
  (lambda (y)
    (if (< y y-stop)
      (begin
        (x-loop x-start y)
        (display "\n")
        (y-loop (+ y y-step)))
        0)))

(y-loop y-start)
