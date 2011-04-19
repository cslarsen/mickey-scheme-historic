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
#include "file_io.h"

static bool verbose = false;

void print_program(FILE *f)
{
  try {
    program_t *p = parse(slurp(f).c_str());
    cons_t *r = eval(p->root);

    if ( verbose )
      printf("eval returned %s\n", sprint(r).c_str());
  }
  catch ( const std::exception& e ) {
    fprintf(stderr, "%s\n", e.what());
    exit(1);
  }
}

int main(int argc, char** argv)
{
  if ( argc == 1 )
    return repl(eval);

  for ( int n=1; n<argc; ++n ) {
    if ( argv[n][0] != '-' )
      print_program(open_file(argv[n]));
  }

  return 0;
}
