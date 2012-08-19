#|

Library: (mickey uname)

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1

|#

(import (only (scheme base) define)
        (mickey library))

(open-library "lib/unix/libunix-uname.so" 'lazy)

(define uname
  (bind-procedure "proc_uname"))
