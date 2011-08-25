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

void raise(const std::exception& e)
{
#ifdef NO_EXCEPTIONS
  printf("Exception raised: %s\n", e.what());

  /*
   * TODO: Longjump back to REPL here, or error catcher.
   */
  if ( jmpbuf_repl != NULL ) {
    longjmp(*jmpbuf_repl, 1);
    return;
  }

  exit(1);
#else
  throw e;
#endif
}
