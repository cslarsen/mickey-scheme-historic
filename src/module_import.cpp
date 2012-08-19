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

#include "cons.h" // to_s(cons_t*)
#include "primops.h"
#include "module.h"
#include "module_import.h"
#include "module_base.h"
#include "module_math.h"
#include "module_mickey_dynamic_library.h"
#include "assertions.h"
#include "exceptions.h"
#include "print.h"
#include "options.h"
#include "eval.h"
#include "file_io.h"

/*
 * Library exports
 */
named_function_t exports_import[] = {
  {"import", proc_import, true},
  {NULL, NULL, false}
};

/*
 * Use heuristics to find file in our library.
 */
static std::string library_file(const std::string& basename)
{
  const std::string library_path = global_opts.lib_path;

  /*
   * TODO: Add more search heuristics here to find a file
   *       in library.
   */

  // first try library path
  std::string file = library_path + "/" + basename;

  // secondly, try include path
  if ( !file_exists(file.c_str()) )
    file = std::string(global_opts.include_path) + "/" + basename;

  // no cigar
  if ( !file_exists(file.c_str()) )
    raise(runtime_exception("Cannot find library file: " + file));

  return file;
}

/*
 * Read, parse and evaluate SCHEME SOURCE FILE in target environment.
 */
static void import(environment_t *target, const std::string& filename)
{
  if ( global_opts.verbose )
    fprintf(stderr, "Loading file %s\n", filename.c_str());

  program_t *p = parse(slurp(open_file(filename)), target);
  eval(cons(symbol("begin", p->globals), p->root), p->globals);
}

static void import_scheme_file(environment_t *r, const char* file)
{
  import(r, exports_import); // add (import)
  import(r, library_file(file));
}

static environment_t* import_library(const std::string& name)
{
  environment_t* r = null_environment();

  if ( global_opts.verbose )
    fprintf(stderr, "Import library %s\n", name.c_str());

  if ( name == "(scheme base)" ) {
    import(r, exports_base, name); // Precompiled C code
    import(r, library_file("scheme/base.scm")); // Scheme source code
  }

  else if ( name == "(scheme math)" )
    import(r, exports_math, name);

  else if ( name == "(scheme char)" )
    import_scheme_file(r, "scheme/char.scm");

  else if ( name == "(scheme lazy)" )
    import_scheme_file(r, "scheme/lazy.scm");

  else if ( name == "(scheme write)" )
    import_scheme_file(r, "scheme/write.scm");

  else if ( name == "(scheme load)" )
    import_scheme_file(r, "scheme/load.scm");

  else if ( name == "(scheme repl)" )
    import_scheme_file(r, "scheme/repl.scm");

  else if ( name == "(scheme process-context)" )
    import_scheme_file(r, "scheme/process-context.scm");

  else if ( name == "(mickey environment)" )
    import_scheme_file(r, "mickey/environment.scm");

  else if ( name == "(mickey uname)" )
    import_scheme_file(r, "mickey/uname.scm");

  else if ( name == "(mickey misc)" )
    import_scheme_file(r, "mickey/misc.scm");

  else if ( name == "(mickey library)" )
    import_scheme_file(r, "mickey/library.scm");

  else if ( name == "(mickey dynamic-library)" )
    import(r, exports_mickey_dynamic_library, name);

  else
    raise(runtime_exception("Unknown library: " + name));

  return r;
}

static environment_t* rename(environment_t* e, cons_t* ids)
{
  assert_type(PAIR, ids);

  // build a new environment and return it
  environment_t *r = null_environment();

  // TODO: Below code runs in slow O(n^2) time
  for ( dict_t::const_iterator i = e->symbols.begin();
        i != e->symbols.end(); ++i )
  {
    std::string name = (*i).first;

    // find new name
    for ( cons_t *id = ids; !nullp(id); id = cdr(id) ) {
      assert_type(PAIR, car(id));
      assert_type(SYMBOL, caar(id));
      assert_type(SYMBOL, cadar(id));
      if ( symbol_name(caar(id)) == name ) {
        name = symbol_name(cadar(id));
        break;
      }
    }

    r->symbols[name] = (*i).second;
  }

  return r;
}

static environment_t* prefix(environment_t* e, cons_t* identifier)
{
  assert_type(SYMBOL, identifier);

  // build a new environment and return it
  environment_t *r = null_environment();

  for ( dict_t::const_iterator i = e->symbols.begin();
        i != e->symbols.end(); ++i )
  {
    const std::string prefix = symbol_name(identifier);
    const std::string name = (*i).first;
    r->symbols[prefix + name] = (*i).second;
  }

  return r;
}

static environment_t* only(environment_t* e, cons_t* ids)
{
  assert_type(PAIR, ids);

  // build a new environment and return it
  environment_t *r = null_environment();

  for ( dict_t::const_iterator i = e->symbols.begin();
        i != e->symbols.end(); ++i )
  {
    std::string name = (*i).first;

    // only import specified names
    // TODO: Fix slow O(n^2) algo below
    for ( cons_t *id = ids; !nullp(id); id = cdr(id) ) {
      assert_type(SYMBOL, car(id));

      if ( symbol_name(car(id)) == name ) {
        r->symbols[name] = (*i).second;
        break;
      }
    }
  }

  return r;
}

static environment_t* except(environment_t* e,  cons_t* ids)
{
  assert_type(PAIR, ids);

  // build a new environment and return it
  environment_t *r = null_environment();

  for ( dict_t::const_iterator i = e->symbols.begin();
        i != e->symbols.end(); ++i )
  {
    std::string name = (*i).first;

    // do not import specified name
    // TODO: Fix slow O(n^2) algo below
    for ( cons_t *id = ids; !nullp(id); id = cdr(id) ) {
      assert_type(SYMBOL, car(id));

      if ( symbol_name(car(id)) == name )
        goto DO_NOT_IMPORT;
    }

    r->symbols[name] = (*i).second;

DO_NOT_IMPORT:
    continue;
  }

  return r;
}

static environment_t* import_set(cons_t* p)
{
  std::string s = symbol_name(car(p));

  /*
   * Each import set can be either of:
   */

  // (rename <import set> (<identifier1> <identifier2>) ...)
  if ( s == "rename" )
    return rename(import_set(cadr(p)), cddr(p));

  // (prefix <import set> <identifier>)
  else if ( s == "prefix" )
    return prefix(import_set(cadr(p)), caddr(p));

  // (only <import set> <identifier> ...)
  else if ( s == "only" )
    return only(import_set(cadr(p)), cddr(p));

  // (except <import set> <identifier> ...)
  else if ( s == "except" )
    return except(import_set(cadr(p)), cddr(p));

  // <library name>
  else if ( !s.empty() )
    return import_library(sprint(p));

  raise(runtime_exception("Unknown import set: " + sprint(p)));
  return NULL;
}

cons_t* proc_import(cons_t* p, environment_t* e)
{
  assert_length_min(p, 1);
  assert_type(PAIR, car(p));

  /*
   * Handle all import sets in (import <import set> ...)
   */
  for ( ; !nullp(p); p = cdr(p) ) {
    environment_t *impenv = import_set(car(p));

    /*
     * Now we need to bring the imported environment to the environment,
     * so that the new definitions are available there.
     *
     * We do this by copying the definitions.
     */
    merge(e, impenv);

    /*
     * But we also need to connect the lower level imported environment to
     * definitions found in its outer environment.
     *
     * This is because the exported functions in impenv must be able to see
     * definitions in the toplevel, controlling, environment.
     *
     * Consider the (mickey environment) module, which has a "syntactic"
     * procedure bound?.
     *
     * If we (import (scheme write)) then we get the procedure display.  But
     * if we now (import (mickey environment)) and call (bound? display)
     * then bound? will not be able to see any definition of display, and
     * will wrongly return #f.
     *
     * Note that I'm not entirely certain that this is the correct way of
     * handling things, since closures must be evaluated in the environment
     * they were defined in.
     *
     * TODO: Think hard about this and write some tests.
     *
     * Note that this behaviour might be different for libraries that are
     * imported as scheme source code.  They must be first evaluated in
     * their own closed environment (to bind definitions) before being
     * connected to the outer one.
     *
     * I think what we need is a global pointer to the ACTUAL top-level
     * environment.
     *
     */
    impenv->outer = e;
  }

  /*
   * TODO: Should we return the final environment,
   *       so we can easily run cond-expand on it?
   */
  return nil();
}
