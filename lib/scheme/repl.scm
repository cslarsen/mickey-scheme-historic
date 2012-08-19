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

(import (only (scheme base)
              define let if not string-append error)
        (mickey dynamic-library))

(define file "lib/scheme/libscheme-repl.so")

(define handle
  (let ((handle (dlopen file 'lazy)))

    (if (not handle)
        (error (string-append
          "Could not dlopen " file ": " (dlerror))))

    handle))

(define (bind-procedure name)
  (let ((proc (dlsym handle name)))

    (if (not proc)
        (error (string-append
          "Could not dlsym " name " in " file ": " (dlerror))))

    proc))

(define interaction_environment (bind-procedure "proc_interaction_environment"))
