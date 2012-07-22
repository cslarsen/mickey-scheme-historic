(define-syntax when
  (syntax-rules ()
    ((when test expr ...)
     (if test (begin expr ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test expr ...)
     (if (not test) (begin expr ...)))))
