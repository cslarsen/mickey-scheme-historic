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

(define file "lib/scheme/libscheme-process-context.so")

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

(define command-line (bind-procedure "proc_command_line"))
(define exit (bind-procedure "exit"))
(define get-environment-variable (bind-procedure "proc_get_environment_variable"))
(define get-environment-variables (bind-procedure "proc_get_environment_variables"))
