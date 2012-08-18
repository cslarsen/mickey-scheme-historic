/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include <dlfcn.h>
#include "util.h"
#include "cons.h"
#include "primops.h"
#include "module_mickey_dynamic_library.h"
#include "assertions.h"
#include "exceptions.h"

/*
 * Define an arbitrary type tag to use to discern
 * between different pointers.
 */
#define TYPE_TAG "dynamic-shared-library-handle"

/*
 * Symbols we will use for the RTLD_* mode flags.
 */
#define SYMBOL_RTLD_LAZY "lazy"
#define SYMBOL_RTLD_NOW "now"
#define SYMBOL_RTLD_GLOBAL "global"
#define SYMBOL_RTLD_LOCAL "local"

named_function_t exports_mickey_dynamic_library[] = {
  {"dlclose", proc_dlclose, false},
  {"dlerror", proc_dlerror, false},
  {"dlopen", proc_dlopen, false},
  {"dlsym", proc_dlsym, false},
  {"dlsym-syntax", proc_dlsym_syntax, false},
  {NULL, NULL, false}};

cons_t* proc_dlclose(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(TYPE_TAG, car(p));
  void *handle = car(p)->pointer->value;
  return boolean(dlclose(handle) == 0);
}

cons_t* proc_dlerror(cons_t*, environment_t*)
{
  const char* s = dlerror();
  return string(s!=NULL? s : "");
}

/*
 * Signature: (dlopen <filename> <mode options> ...)
 *
 * Options must be symbols with names lazy now global local.  These
 * correspond to the RTLD_LAZY, RTLD_NOW, RTLD_GLOBAL and RTLD_LOCAL mode
 * options.  If you specify several options, they will be bitwise OR'ed
 * together.
 *
 */
cons_t* proc_dlopen(cons_t* p, environment_t*)
{
  assert_length_min(p, 1);
  assert_type(STRING, car(p));

  const char* file = car(p)->string;
  int mode = 0;

  // build mode
  for ( cons_t *m = cdr(p); !nullp(m); m = cdr(m) ) {
    assert_type(SYMBOL, car(m));
    std::string n = symbol_name(car(m));

    if ( n == SYMBOL_RTLD_LAZY )        mode |= RTLD_LAZY;
    else if ( n == SYMBOL_RTLD_NOW )    mode |= RTLD_NOW;
    else if ( n == SYMBOL_RTLD_GLOBAL ) mode |= RTLD_GLOBAL;
    else if ( n == SYMBOL_RTLD_LOCAL )  mode |= RTLD_LOCAL;
    else {
      raise(runtime_exception(format(
        "Unknown dlopen mode parameter %s --- "
        "available modes are %s %s %s %s", n.c_str(),
          SYMBOL_RTLD_LAZY, SYMBOL_RTLD_NOW,
          SYMBOL_RTLD_GLOBAL, SYMBOL_RTLD_LOCAL)));
    }
  }

  void *h = dlopen(file, mode);

  return h!=NULL?
    pointer(new pointer_t(TYPE_TAG, h)) :
    boolean(false);
}

static lambda_t dlsym_helper(cons_t* p)
{
  assert_length(p, 2, 3);
  assert_pointer(TYPE_TAG, car(p));
  assert_type(STRING, cadr(p));

  pointer_t *handle = car(p)->pointer;
  const char* name = cadr(p)->string;

  return reinterpret_cast<lambda_t>(dlsym(handle->value, name));
}

/*
 * Takes a handle, a function name and an optional environment
 * (default is current environment) and returns a closure
 * of the given function and environment, or #f if operation
 * failed.
 *
 * (dlsym <handle> <name> <environment>?)
 *
 */
cons_t* proc_dlsym(cons_t* p, environment_t* current)
{
  environment_t *e = current;

  if ( length(p) == 3 ) {
    assert_type(ENVIRONMENT, caddr(p));
    e = caddr(p)->environment;
  }

  lambda_t f = dlsym_helper(p);
  return f != NULL ? closure(f, e) : boolean(false);
}

/*
 * Same as dlsym, but returns a syntactic closure, meaning that function
 * arguments are not evaluated before invocation.
 */
cons_t* proc_dlsym_syntax(cons_t* p, environment_t* current)
{
  environment_t *e = current;

  if ( length(p) == 3 ) {
    assert_type(ENVIRONMENT, caddr(p));
    e = caddr(p)->environment;
  }

  lambda_t f = dlsym_helper(p);
  return f != NULL ? closure(f, e, true) : boolean(false);
}
