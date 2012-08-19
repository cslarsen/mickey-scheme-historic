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

(import (only (scheme base) defien)
        (mickey library))

(open-library "lib/scheme/libscheme-char.so")

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
