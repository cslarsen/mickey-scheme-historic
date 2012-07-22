;; Mickey Scheme
;; Assumed definitions: car, cdr
;;
;; TODO: Create a routine to generate these
;;       ahead of runtime.

(define (caar s) (car (car s)))
(define (cadr s) (car (cdr s)))
(define (cdar s) (cdr (car s)))
(define (cddr s) (cdr (cdr s)))
(define (caaar s) (car (car (car s))))
(define (caadr s) (car (car (cdr s))))
(define (cadar s) (car (cdr (car s))))
(define (caddr s) (car (cdr (cdr s))))
(define (cdaar s) (cdr (car (car s))))
(define (cdadr s) (cdr (car (cdr s))))
(define (cddar s) (cdr (cdr (car s))))
(define (cdddr s) (cdr (cdr (cdr s))))
(define (caaaar s) (car (car (car (car s)))))
(define (caaadr s) (car (car (car (cdr s)))))
(define (caadar s) (car (car (cdr (car s)))))
(define (caaddr s) (car (car (cdr (cdr s)))))
(define (cadaar s) (car (cdr (car (car s)))))
(define (cadadr s) (car (cdr (car (cdr s)))))
(define (caddar s) (car (cdr (cdr (car s)))))
(define (cadddr s) (car (cdr (cdr (cdr s)))))
(define (cdaaar s) (cdr (car (car (car s)))))
(define (cdaadr s) (cdr (car (car (cdr s)))))
(define (cdadar s) (cdr (car (cdr (car s)))))
(define (cdaddr s) (cdr (car (cdr (cdr s)))))
(define (cddaar s) (cdr (cdr (car (car s)))))
(define (cddadr s) (cdr (cdr (car (cdr s)))))
(define (cdddar s) (cdr (cdr (cdr (car s)))))
(define (cddddr s) (cdr (cdr (cdr (cdr s)))))
