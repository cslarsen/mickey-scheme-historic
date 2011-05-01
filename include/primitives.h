#include "cons.h"

/*
 * Primitives
 *
 * TODO: Create FFI and expose functions so they
 *       can be used to IMPLEMENT primitives like
 *       the ones below by calling foreign C functions.
 */

closure_t* lookup_closure(symbol_t*, environment_t*);
void defun(symbol_t*, lambda_t, environment_t*);
void load_default_defs(environment_t*);

cons_t* defun_version(cons_t*, environment_t*);
cons_t* defun_print(cons_t*, environment_t*);
cons_t* defun_strcat(cons_t*, environment_t*);
cons_t* defun_addf(cons_t*, environment_t*);
cons_t* defun_add(cons_t*, environment_t*);
cons_t* defun_sub(cons_t*, environment_t*);
cons_t* defun_mul(cons_t*, environment_t*);
cons_t* defun_less(cons_t*, environment_t*);
cons_t* defun_greater(cons_t*, environment_t*);
cons_t* defun_div(cons_t*, environment_t*);
cons_t* defun_divf(cons_t*, environment_t*);
cons_t *defun_sqrt(cons_t*, environment_t*);
cons_t* defun_to_string(cons_t*, environment_t*);
cons_t* defun_list(cons_t*, environment_t*);
cons_t* defun_define(cons_t*, environment_t*);
cons_t* defun_load(cons_t*, environment_t*);
cons_t* defun_debug(cons_t*, environment_t*);
cons_t* defun_exit(cons_t*, environment_t*);

// cons and friends
cons_t* defun_cons(cons_t*, environment_t*);
cons_t* defun_car(cons_t*, environment_t*);
cons_t* defun_cdr(cons_t*, environment_t*);
cons_t* defun_caar(cons_t*, environment_t*);
cons_t* defun_cadr(cons_t*, environment_t*);
cons_t* defun_cdar(cons_t*, environment_t*);
cons_t* defun_cddr(cons_t*, environment_t*);
cons_t* defun_append(cons_t*, environment_t*);

// predicates
cons_t* defun_atomp(cons_t*, environment_t*);
cons_t* defun_symbolp(cons_t*, environment_t*);
cons_t* defun_integerp(cons_t*, environment_t*);
cons_t* defun_decimalp(cons_t*, environment_t*);
cons_t* defun_nullp(cons_t*, environment_t*);
cons_t* defun_pairp(cons_t*, environment_t*);
cons_t* defun_listp(cons_t*, environment_t*);
cons_t* defun_procedurep(cons_t*, environment_t*);
cons_t* defun_booleanp(cons_t*, environment_t*);
cons_t* defun_vectorp(cons_t*, environment_t*);
cons_t* defun_charp(cons_t*, environment_t*);
cons_t* defun_zerop(cons_t*, environment_t*);

cons_t* defun_length(cons_t*, environment_t*);
cons_t* defun_eqp(cons_t*, environment_t*);
cons_t* defun_eqintp(cons_t*, environment_t*);

// bool ops
cons_t* defun_not(cons_t*, environment_t*);
cons_t* defun_and(cons_t*, environment_t*);
cons_t* defun_or(cons_t*, environment_t*);
cons_t* defun_xor(cons_t*, environment_t*);
