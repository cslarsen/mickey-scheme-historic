#ifndef INC_MICKEY_PRINT_H
#define INC_MICKEY_PRINT_H

#include <string>
#include "cons.h"
#include "parser.h"
#include "primops.h"

#define SPRINT(x) { printf(#x " = '%s'\n", sprint(x).c_str()); }
#define  PRINT(x) { printf(#x " = '%s'\n", print(x).c_str()); }

std::string sprint(cons_t* p);
std::string sprint(program_t* p);

// print data values "as-is", i.e. do not escape stuff
std::string print(cons_t* p);
std::string print(program_t* p);

#endif
