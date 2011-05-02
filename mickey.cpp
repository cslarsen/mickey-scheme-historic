/*
 * Mickey Scheme -- an incomplete, toy-implementation of Lisp.
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org
 * 
 * Distributed under the modified BSD license.             \\
 * Please post bugfixes or suggestions to the author.      /\\
 *                                                        /  \\
 */

#include "repl.h"
#include "parser.h"
#include "eval.h"
#include "print.h"
#include "primitives.h"
#include "heap.h"
#include "backtrace.h"

int main(int argc, char** argv)
{
  #ifdef BOEHM_GC
  GC_INIT();
  #endif

  if ( argc == 1 )
    return repl();

  for ( int n=1; n<argc; ++n )
    if ( argv[n][0] != '-' ) {
      try {
        environment_t *env = new environment_t();
        load_default_defs(env);
        defun_load(cons(string(argv[n])), env);
      } catch (const std::exception& e) {
        fprintf(stderr, "Error: %s\n", e.what());
        backtrace();
        backtrace_clear();
        return 1;
      }
    }

  return 0;
}
