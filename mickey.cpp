#include "repl.h"
#include "parser.h"
#include "eval.h"
#include "print.h"
#include "file_io.h"

void print_program(FILE *f)
{
  try {
    program_t *p = parse(slurp(f).c_str());
    printf("%s\n", print(eval(p->root)).c_str());
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
