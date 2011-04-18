#include <string>
#include "file_io.h"

std::string slurp(FILE *f)
{
  std::string r;

  for ( int c; (c = fgetc(f)) != EOF; )
    r += c;

  return r;
}
