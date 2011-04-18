#ifndef INC_MICKEY_PARSER_H
#define INC_MICKEY_PARSER_H

#include "cons.h"
#include "primops.h"

cons_t* parse_list();
cons_t* parse(const char *program);

#endif
