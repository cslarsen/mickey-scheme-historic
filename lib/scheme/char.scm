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

(define file "lib/scheme/libscheme-char.so")

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

(define char-alphabetic? (bind-procedure "proc_char_alphabeticp"))
(define char-downcase    (bind-procedure "proc_char_downcase"))
(define char-lower-case? (bind-procedure "proc_char_lowercasep"))
(define char-numeric?    (bind-procedure "proc_char_numericp"))
(define char-upcase      (bind-procedure "proc_char_upcase"))
(define char-upper-case? (bind-procedure "proc_char_uppercasep"))
(define char-whitespace? (bind-procedure "proc_char_whitespacep"))

#|
 | From R7RS draft 6:
 |
 | "[...] applies the unicode simple case-folding algorithm to
 |  its argument and returns the result."
 |
 | See the Unicode Standard Annex #29 at
 | http://unicode.org/reports/tr29/ for details.
 |
 | Note that THIS implementation is CHEATING, and does not
 | employ the correct algorithm.
 |#
(define (char-foldcase char)
  ; simply call `char-downcase`
  ; note that this might not be entirely correct
  (char-downcase char))
