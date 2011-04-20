#include "cons.h"

/*
 * Primitives
 *
 * TODO: Create FFI and expose functions so they
 *       can be used to IMPLEMENT primitives like
 *       the ones below by calling foreign C functions.
 */

lambda_t lookup_lambda(symbol_t*);
void defun(symbol_t*, lambda_t);
void load_default_defs(environment_t*);

cons_t* defun_print(cons_t*);
cons_t* defun_strcat(cons_t*);
cons_t* defun_add(cons_t*);
cons_t* defun_mul(cons_t*);
cons_t* defun_begin(cons_t*);
cons_t* defun_to_string(cons_t*);
