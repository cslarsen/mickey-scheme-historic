#ifndef INC_MICKEY_PRIMOPS_H
#define INC_MICKEY_PRIMOPS_H

#include "cons.h"
#include "tokenizer.h"

type_t type_of(cons_t* p);

cons_t* cons(cons_t* head, cons_t* tail = NULL);
cons_t* list(cons_t* head, cons_t* tail = NULL);
cons_t* append(cons_t *h, cons_t *t);
cons_t* symbol(const char* s, environment_t *env);
cons_t* integer(int n);

cons_t* car(cons_t* p);
cons_t* cdr(cons_t* p);
cons_t* caar(cons_t* p);
cons_t* cadr(cons_t* p);
cons_t* cddr(cons_t* p);
cons_t* cdar(cons_t* p);

bool symbolp(cons_t* p);
bool atomp(cons_t* p);
bool integerp(cons_t* p);
bool nullp(cons_t* p);
bool pairp(cons_t* p);

#endif
