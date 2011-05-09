/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the modified BSD license.            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#ifndef INC_MICKEY_PRIMOPS_H
#define INC_MICKEY_PRIMOPS_H

#include "cons.h"
#include "tokenizer.h"

type_t type_of(const cons_t* p);
size_t length(const cons_t*);

cons_t* cons(const cons_t* head, const cons_t* tail = NULL);
cons_t* list(const cons_t* head, const cons_t* tail = NULL);
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

cons_t* car(const cons_t*);
cons_t* cdr(const cons_t*);
cons_t* caar(const cons_t*);
cons_t* cadr(const cons_t*);
cons_t* cddr(const cons_t*);
cons_t* cdar(const cons_t*);
cons_t* cadar(const cons_t*);

bool symbolp(const cons_t*);
bool atomp(const cons_t*);
bool integerp(const cons_t*);
bool decimalp(const cons_t*);
bool nullp(const cons_t*);
bool pairp(const cons_t*);
bool listp(const cons_t*);
bool stringp(const cons_t*);
bool closurep(const cons_t*);
bool booleanp(const cons_t*);
bool vectorp(const cons_t*);
bool charp(const cons_t*);
bool numberp(const cons_t*);
bool eqp(const cons_t*, const cons_t*);
bool equalp(const cons_t*, const cons_t*);

bool not_p(const cons_t*);
bool and_p(const cons_t*);
bool or_p(const cons_t*);
bool xor_p(const cons_t*);

double number_to_double(const cons_t*);
float number_to_float(const cons_t*);
bool iswhole(float);

#endif
