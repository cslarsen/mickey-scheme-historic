#ifndef INC_MICKEY_PARSER_H
#define INC_MICKEY_PARSER_H

#include "cons.h"
#include "primops.h"

struct program_t {
  environment_t *globals;
  cons_t *root;
};

program_t* parse(const char *program);

#endif
