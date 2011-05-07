#ifndef INC_MICKEY_PRIMOPS_H
#define INC_MICKEY_PRIMOPS_H

#include "cons.h"
#include "tokenizer.h"

type_t type_of(cons_t* p);
size_t length(cons_t*);

cons_t* cons(cons_t* head, cons_t* tail = NULL);
cons_t* list(cons_t* head, cons_t* tail = NULL);
cons_t* append(cons_t*, cons_t*);
cons_t* append_non_mutable(cons_t*, cons_t*);
cons_t* symbol(const char*, environment_t*);
cons_t* integer(int);
cons_t* boolean(bool);
cons_t* character(char);
cons_t* decimal(float);
cons_t* string(const char*);
cons_t* closure(lambda_t, environment_t*);
cons_t* nil();

cons_t* car(cons_t*);
cons_t* cdr(cons_t*);
cons_t* caar(cons_t*);
cons_t* cadr(cons_t*);
cons_t* cddr(cons_t*);
cons_t* cdar(cons_t*);

bool symbolp(cons_t*);
bool atomp(cons_t*);
bool integerp(cons_t*);
bool decimalp(cons_t*);
bool nullp(cons_t*);
bool pairp(cons_t*);
bool listp(cons_t*);
bool stringp(cons_t*);
bool closurep(cons_t*);
bool booleanp(cons_t*);
bool vectorp(cons_t*);
bool charp(cons_t*);
bool numberp(cons_t*);
bool eqp(cons_t*, cons_t*);
bool equalp(cons_t*, cons_t*);

bool not_p(cons_t*);
bool and_p(cons_t*);
bool or_p(cons_t*);
bool xor_p(cons_t*);

#endif
