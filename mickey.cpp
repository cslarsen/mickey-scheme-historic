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
      printf("%s\n", sprint(eval(parse(slurp(open_file(argv[n])).c_str()))).c_str());
    }
  }

  return 0;
}
