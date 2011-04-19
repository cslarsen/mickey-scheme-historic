#include "repl.h"
#include "parser.h"
#include "eval.h"
#include "print.h"
#include "file_io.h"

int main(int argc, char** argv)
{
  if ( argc == 1 )
    return repl();

  for ( int n=1; n<argc; ++n ) {
    if ( argv[n][0] != '-' ) {
      program_t *p = parse(slurp(open_file(argv[n])).c_str());
      printf("%s\n", sprint(eval(p->root)).c_str());
    }
  }

  return 0;
}
