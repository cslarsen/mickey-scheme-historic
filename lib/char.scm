#|
 | Mickey Scheme R7RS character library
 |
 | Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
 | http://csl.sublevel3.org                              _
 |                                                        \
 | Distributed under the modified BSD license.            /\
 | Please post bugfixes and suggestions to the author.   /  \_
 |                                                          
 |#

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
