#include "cons.h"

/*
 * Primitives
 *
 * TODO: Create FFI and expose functions so they
 *       can be used to IMPLEMENT primitives like
 *       the ones below by calling foreign C functions.
 */

cons_t* defun_print(cons_t*);
cons_t* defun_strcat(cons_t*);
cons_t* defun_add(cons_t*);
cons_t* defun_mul(cons_t*);
cons_t* defun_begin(cons_t*);
