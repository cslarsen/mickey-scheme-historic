#include "cons.h"
#include "module.h"

/*
 * Primitives
 *
 * TODO: Create FFI and expose functions so they
 *       can be used to IMPLEMENT primitives like
 *       the ones below by calling foreign C functions.
 */

extern named_function_t exports_base[];

closure_t* lookup_closure(symbol_t*, environment_t*);
void defun(symbol_t*, lambda_t, environment_t*);
void load_default_defs(environment_t*);

cons_t* proc_version(cons_t*, environment_t*);
cons_t* proc_print(cons_t*, environment_t*);
cons_t* proc_newline(cons_t*, environment_t*);
cons_t* proc_strcat(cons_t*, environment_t*);
cons_t* proc_addf(cons_t*, environment_t*);
cons_t* proc_add(cons_t*, environment_t*);
cons_t* proc_sub(cons_t*, environment_t*);
cons_t* proc_mul(cons_t*, environment_t*);
cons_t* proc_less(cons_t*, environment_t*);
cons_t* proc_greater(cons_t*, environment_t*);
cons_t* proc_div(cons_t*, environment_t*);
cons_t* proc_divf(cons_t*, environment_t*);
cons_t *proc_sqrt(cons_t*, environment_t*);
cons_t* proc_to_string(cons_t*, environment_t*);
cons_t* proc_list(cons_t*, environment_t*);
cons_t* proc_define(cons_t*, environment_t*);
cons_t* proc_load(cons_t*, environment_t*);
cons_t* proc_debug(cons_t*, environment_t*);
cons_t* proc_exit(cons_t*, environment_t*);
cons_t* proc_let(cons_t*, environment_t*);
cons_t* proc_letstar(cons_t*, environment_t*);

// cons and friends
cons_t* proc_cons(cons_t*, environment_t*);
cons_t* proc_car(cons_t*, environment_t*);
cons_t* proc_cdr(cons_t*, environment_t*);
cons_t* proc_caar(cons_t*, environment_t*);
cons_t* proc_cadr(cons_t*, environment_t*);
cons_t* proc_cdar(cons_t*, environment_t*);
cons_t* proc_cddr(cons_t*, environment_t*);
cons_t* proc_append(cons_t*, environment_t*);

// predicates
cons_t* proc_atomp(cons_t*, environment_t*);
cons_t* proc_symbolp(cons_t*, environment_t*);
cons_t* proc_integerp(cons_t*, environment_t*);
cons_t* proc_decimalp(cons_t*, environment_t*);
cons_t* proc_nullp(cons_t*, environment_t*);
cons_t* proc_pairp(cons_t*, environment_t*);
cons_t* proc_listp(cons_t*, environment_t*);
cons_t* proc_procedurep(cons_t*, environment_t*);
cons_t* proc_booleanp(cons_t*, environment_t*);
cons_t* proc_vectorp(cons_t*, environment_t*);
cons_t* proc_charp(cons_t*, environment_t*);
cons_t* proc_zerop(cons_t*, environment_t*);

cons_t* proc_length(cons_t*, environment_t*);
cons_t* proc_eqp(cons_t*, environment_t*);
cons_t* proc_equalp(cons_t*, environment_t*);
cons_t* proc_eqintp(cons_t*, environment_t*);

// bool ops
cons_t* proc_not(cons_t*, environment_t*);
cons_t* proc_and(cons_t*, environment_t*);
cons_t* proc_or(cons_t*, environment_t*);
cons_t* proc_xor(cons_t*, environment_t*);

cons_t* proc_type_of(cons_t*, environment_t*);
cons_t* proc_closure_source(cons_t*, environment_t*);
cons_t* proc_backtrace(cons_t*, environment_t*);
cons_t* proc_cond(cons_t*, environment_t*);
cons_t* proc_number_to_string(cons_t*, environment_t*);
cons_t* proc_reverse(cons_t*, environment_t*);
cons_t* proc_set_car(cons_t*, environment_t*);
cons_t* proc_set_cdr(cons_t*, environment_t*);
cons_t* proc_file_existsp(cons_t*, environment_t*);
cons_t* proc_abs(cons_t* p, environment_t*);
