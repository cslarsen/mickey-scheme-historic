#ifndef INC_MICKEY_PRINT_H
#define INC_MICKEY_PRINT_H

#include <string>
#include "cons.h"
#include "parser.h"
#include "primops.h"

std::string sprint(cons_t* p);
std::string sprint(program_t* p);

#endif
