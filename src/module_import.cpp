/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the modified BSD license.            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include "cons.h" // to_s(cons_t*)
#include "primops.h"
#include "module.h"
#include "module_import.h"
#include "module_load.h"
#include "module_base.h"
#include "module_math.h"
#include "module_char.h"
#include "module_write.h"
#include "module_process_context.h"
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
  {"import", proc_import},
  {NULL, NULL}
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
 * Read, parse and evaluate source file in target environment.
 */
static void import(environment_t *target, const std::string& filename)
{
  if ( global_opts.verbose )
    fprintf(stderr, "Loading file %s\n", filename.c_str());

  program_t *p = parse(slurp(open_file(filename)), target);
  eval(cons(symbol("begin", p->globals), p->root), p->globals);
  //merge(target, p->globals); // TODO: Needed? Really shouldn't be
}

static environment_t* import_library(const std::string& name)
{
  environment_t* r = null_environment();

  if ( global_opts.verbose )
    fprintf(stderr, "Import library %s\n", name.c_str());

  if ( name == "(scheme base)" ) {
    import(r, exports_base, name); // Precompiled C code
    import(r, library_file("base.scm")); // Scheme source code
  }

  else if ( name == "(scheme math)" )
    import(r, exports_math, name);

  else if ( name == "(scheme char)" ) {
    import(r, exports_char, name);
    import(r, library_file("char.scm"));
  }

  else if ( name == "(scheme lazy)" )
    import(r, library_file("lazy.scm"));

  else if ( name == "(scheme write)" )
    import(r, exports_write, name);

  else if ( name == "(scheme load)" )
    import(r, exports_load, name);

  else if ( name == "(scheme process-context)" )
    import(r, exports_process_context, name);

  else
    raise(runtime_exception("Unknown library: " + name));

  return r;
}

static environment_t* rename(environment_t* /*importset*/, cons_t* /*identifiers*/)
{
  raise(runtime_exception("import rename not supported"));
  return NULL;
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

static environment_t* only(environment_t* /*importset*/, cons_t* /*identifiers*/)
{
  raise(runtime_exception("import only not supported"));
  return NULL;
}

static environment_t* except(environment_t* /*importset*/, cons_t* /*identifiers*/)
{
  raise(runtime_exception("import except not supported"));
  return NULL;
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
  assert_length_min(p, 2);
  assert_type(PAIR, cadr(p));

  /*
   * Handle all import sets in (import <import set> ...)
   */
  for ( p = cdr(p); !nullp(p); p = cdr(p) )
    merge(e, import_set(car(p)));

  /*
   * TODO: Should we return the final environment,
   *       so we can easily run cond-expand on it?
   */
  return nil();
}
