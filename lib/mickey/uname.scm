#|

Library: (mickey uname)

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1

|#

(import (only (scheme base)
              define if not error let)
        (mickey dynamic-library))

(define file "lib/mickey/libmickey-uname.so")

(define handle (dlopen file) 'lazy)
(if (not handle)
  (error (string-append "Could not dlopen " file)))

(define (bind-procedure name)
  (let
    ((r (dlsym handle name)))
    (if (not r)
      (error (string-append "Could not dlsym " name " in " file)))
    r))

(define uname bind-procedure "proc_uname"))
