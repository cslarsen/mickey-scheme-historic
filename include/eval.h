#include "cons.h"
#include "parser.h"

cons_t* eval(cons_t* p, environment_t* env);
cons_t* eval(program_t* p);
