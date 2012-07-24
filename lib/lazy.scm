#|
 | Mickey Scheme R7RS lazy evaluation library
 |
 | Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
 | http://csl.sublevel3.org                              _
 |                                                        \
 | Distributed under the modified BSD license.            /\
 | Please post bugfixes and suggestions to the author.   /  \_
 |
 |#

#|
 | Lazy evaluation with memoization.
 |#
(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (let
       ((computed? #f)
        (memoized '()))
       (lambda ()
         (if computed? memoized
           (begin
             (set! memoized expression)
             (set! computed? #t)
             memoized)))))))

#|
 | Force a previously delayed computation.
 |
 | After forcing the first time, the computer
 | value is memoized, and thus not recomputed
 | every time.
 |#
(define (force promise)
  (promise))
