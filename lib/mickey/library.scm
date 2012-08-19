#|

Library: (mickey library)

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1

|#

(import (scheme base)
        (mickey dynamic-library))

(define *handle* #f)
(define *file* #f)

;; Usage: (open-library file . options) where `options` are the same as for
;; (dlopen) in (mickey dynamic-library).
;;
(define (open-library filename . options)
  (set! *file* filename)
  (set! *handle* (apply dlopen (cons *file* options)))

  (if (not *handle*)
    (error (string-append
      "Could not dlopen " *file* ": " (dlerror))))

  *handle*)

;; Usage: (bind-procedure "some_c_function")
;;
;; Returns closure of function in library.
;;
(define (bind-procedure name)
  (let
    ((r (dlsym *handle* name)))
    (if (not r)
      (error (string-append
        "Could not dlsym " name " in " *file* ": "
        (dlerror))))
    r))

;; Usage: (bind-syntax "some_c_function")
;;
;; Returns syntactic closure of function in library.
;;
(define (bind-syntax name)
  (lambda (name)
    (let
      ((r (dlsym-syntax *handle* name)))
      (if (not r)
        (error (string-append
          "Could not dlsym " name " in " *file* ": "
          (dlerror))))
      r)))
