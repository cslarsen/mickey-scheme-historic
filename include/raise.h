/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the modified BSD license.            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdexcept>

/*
 * Raise error.  If you have defined NO_EXCEPTIONS,
 * the error will be reported and the process aborted.
 *
 * If NO_EXCEPTIONS has not been defined, an exception
 * will be raised.
 */
void raise(const std::exception& e);
