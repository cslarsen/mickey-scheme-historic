#include "cons.h"

void set_backtrace(bool on_off);
void backtrace_push(cons_t* expr);
void backtrace_pop();
void backtrace_clear();
void backtrace();
