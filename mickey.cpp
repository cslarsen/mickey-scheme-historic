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
#include "primitives.h"

static bool verbose = false;

void print_program(FILE *f)
{
  try {
    program_t *p = parse(slurp(f).c_str(), NULL);
    load_default_defs(p->globals);

    // When reading from disk, we implicitly wrap it all in (begin ...)
    cons_t *begin = p->globals->lookup("begin");
    p->root = list(begin, p->root);

    cons_t *r = eval(p);

    if ( verbose ) {
      std::string s = sprint(r);
      printf("eval returned %s\n", s.empty() ? "<nothing>" : s.c_str());
    }
  }
  catch ( const std::exception& e ) {
    fprintf(stderr, "%s\n", e.what());
    exit(1);
  }
}

int main(int argc, char** argv)
{
  if ( argc == 1 )
    return repl();

  for ( int n=1; n<argc; ++n ) {
    if ( argv[n][0] != '-' )
      print_program(open_file(argv[n]));
  }

  return 0;
}
