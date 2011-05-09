#include <math.h>
#include "module.h"
#include "module_math.h"
#include "primops.h"
#include "assertions.h"

cons_t* proc_exp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));
  return decimal(exp(number_to_double(car(p))));
}

cons_t* proc_log(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));
  return decimal(log(number_to_double(car(p))));
}

named_function_t exports_math[] = {
  {"exp", proc_exp},
  {"log", proc_log},
  {NULL, NULL}
};
