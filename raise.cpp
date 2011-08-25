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

#include "repl.h"
#include "raise.h"

#ifdef NO_EXCEPTIONS
jmp_buf catch_point;
#endif

void raise(const std::exception& e)
{
#ifdef NO_EXCEPTIONS
  printf("Exception: %s\n", e.what());
  longjmp(catch_point, 1);
#else
  throw e;
#endif
}
