;; The Mandelbrot, printed in ASCII
;; Straight-forward version, with no optimizations

;; Functions for COMPLEX NUMBERS

(define make-z
  (lambda (re im)
    (cons re im)))

(define re
  (lambda (z)
    (car z)))

(define im
  (lambda (z)
    (cdr z)))

(define z->string
  (lambda (c)
    (string-append
      (->string (re c)) " + "
      (->string (im c)) "i")))

(define +z
  (lambda (a b)
    (make-z
      (+ (re a) (re b))
      (+ (im a) (im b)))))

(define -z
  (lambda (a b)
    (make-z
      (- (re a) (re b))
      (- (im a) (im b)))))

(define *z
  ; (a + ib) * (c + id) = (ac + i(ad + ibc) - bd)
  ;                     = (ac - bd) + i(ad + bc)
  (lambda (a b)
    (make-z
      (- (* (re a) (re b))  ; ac
         (* (im a) (im b))) ; bd
      (+ (* (re a) (im b))     ; ad
         (* (im a) (re b)))))) ; bc

