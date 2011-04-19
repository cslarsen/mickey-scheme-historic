#include <stdexcept>
#include "apply.h"

cons_t* apply(lambda_t f, cons_t *args)
{
  // TODO: Add environment argument
  return f? f(args) : NULL;
}
