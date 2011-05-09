#include <math.h>
#include "module.h"
#include "module_math.h"
#include "primops.h"
#include "assertions.h"

#define MAKE_PROC(name, math_fun)                       \
  cons_t* name(cons_t* p, environment_t*)               \
  {                                                     \
    assert_length(p, 1);                                \
    assert_number(car(p));                              \
    return decimal(math_fun(number_to_double(car(p)))); \
  }

MAKE_PROC(proc_exp, exp);
MAKE_PROC(proc_log, log);
MAKE_PROC(proc_sin, sin);
MAKE_PROC(proc_cos, cos);
MAKE_PROC(proc_tan, tan);
MAKE_PROC(proc_asin, asin);
MAKE_PROC(proc_acos, acos);
MAKE_PROC(proc_sqrt, sqrt);

cons_t* proc_atan(cons_t* p, environment_t*)
{
  if ( length(p) == 1 ) {
    assert_number(car(p));
    return decimal(atan(number_to_double(car(p))));
  } else if ( length(p) == 2 ) {
    throw std::runtime_error("Two-argument atan is not (yet) supported");
  } else
    throw std::runtime_error("Function atan requires one or two arguments");
}

named_function_t exports_math[] = {
  {"exp", proc_exp},
  {"log", proc_log},
  {"sin", proc_sin},
  {"cos", proc_cos},
  {"tan", proc_tan},
  {"asin", proc_asin},
  {"acos", proc_acos},
  {"atan", proc_atan},
  {"sqrt", proc_sqrt},
  {NULL, NULL}
};
