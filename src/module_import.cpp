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

#include "cons.h" // to_s(cons_t*)
#include "primops.h"
#include "module.h"
#include "module_import.h"
#include "module_base.h"
#include "module_math.h"
#include "module_write.h"
#include "module_process_context.h"
#include "assertions.h"
#include "exceptions.h"
#include "print.h"
#include "options.h"

cons_t* proc_import(cons_t* p, environment_t* e)
{
  assert_length_min(p, 2);
  assert_type(PAIR, cadr(p));

  /*
   * TODO: (import) is not fully supported yet
   */

  cons_t *spec = cadr(p);

  if ( symbol_name(car(spec)) == "scheme" ) {
    std::string mod = symbol_name(cadr(spec));
    const char *lib_path = global_opts.lib_path;

    if ( mod == "base" ) {
      // library is split into C-lib and Scheme-lib
      import(e, exports_base);
      load(e, lib_path, "base.scm");
    } else if ( mod == "math" ) {
      import(e, exports_math);
    } else if ( mod == "char" ) {
      load(e, lib_path, "char.scm");
    } else if ( mod == "lazy" ) {
      load(e, lib_path, "lazy.scm");
    } else if ( mod == "write" ) {
      import(e, exports_write);
    } else if ( mod == "process-context" ) {
      import(e, exports_process_context);
    } else {
      raise(runtime_exception("Unknown library: " + sprint(car(p))));
      return boolean(false);
    }
  }

  // TODO: Correct ret val
  return boolean(true);
}

/*
 * We typically name Mickey-specific function with
 * a colon prefix, as in ":bound?".
 */
named_function_t exports_import[] = {
  {"import", proc_import},
  {NULL, NULL} /* terminate with null */
};
