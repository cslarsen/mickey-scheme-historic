#include <stdexcept>
#include "apply.h"

/*
 * TODO: Add environment argument, and do this
 *        - eval returns expr + environment
 *        - apply gobbles expr + environment, returns environment
 *        - eval gobbles up this new environment, etc
 */
cons_t* apply(lambda_t f, cons_t *args, environment_t *env)
{
  // TODO: Add environment argument
  return f? f(args, env) : NULL;
}
