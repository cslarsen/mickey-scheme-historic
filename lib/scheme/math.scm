#|
 | Mickey Scheme R7RS character library
 |
 | Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
 | http://csl.sublevel3.org                              _
 |                                                        \
 | Distributed under the LGPL 2.1; see LICENSE            /\
 | Please post bugfixes and suggestions to the author.   /  \_
 |
 |#

(import (only (scheme base) define)
        (mickey library))

(open-library "lib/scheme/libscheme-math.so" 'lazy)

(define exp     (bind-procedure "proc_exp"))
(define log     (bind-procedure "proc_log"))
(define sin     (bind-procedure "proc_sin"))
(define cos     (bind-procedure "proc_cos"))
(define tan     (bind-procedure "proc_tan"))
(define asin    (bind-procedure "proc_asin"))
(define acos    (bind-procedure "proc_acos"))
(define atan    (bind-procedure "proc_atan"))
(define sqrt    (bind-procedure "proc_sqrt"))
(define ceiling (bind-procedure "proc_ceil"))
(define floor   (bind-procedure "proc_floor"))
