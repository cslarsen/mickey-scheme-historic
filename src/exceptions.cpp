/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include "repl.h"
#include "exceptions.h"

#ifdef NO_EXCEPTIONS
jmp_buf catch_point;
std::runtime_error __exception("Unknown exception");
#endif

void raise(const std::exception& e)
{
#ifdef NO_EXCEPTIONS
  __exception = std::runtime_error(e.what());
  longjmp(catch_point, 1);
#else
  // we cannot simply throw e, because then
  // stack unrolling will cause the original e to
  // be popped -- therefore we rethrow by copying
  throw std::runtime_error(e.what());
#endif
}
