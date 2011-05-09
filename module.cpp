#include "module.h"

void import(environment_t *e, named_function_t *p)
{
  while ( p->name!=NULL && p->function!=NULL ) {
    e->defun(p->name, p->function);
    ++p;
  }
}
