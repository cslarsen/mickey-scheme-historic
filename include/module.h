#ifndef INC_MICKEY_MODULE_H
#define INC_MICKEY_MODULE_H

#include "cons.h"

struct named_function_t {
  const char* name;
  lambda_t function;
};

void import(environment_t*, named_function_t*);

#endif
