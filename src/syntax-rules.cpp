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

#include "cons.h"
#include "primops.h"
#include "syntax-rules.h"
#include "print.h"
#include "exceptions.h"
#include "arguments.h"

extern "C" { // because I don't like name-mangling

/*
 * See R7RS 4.3.2 Pattern Language for examples on how to match
 * a syntax-rules pattern with code.
 *
 * NOTE: We do NOT support the full pattern language yet.  We only
 *       support simple patterns like (<pattern>) and (<pattern> ...)
 *       and the same for templates.
 *
 *       There is also a small problem with the environments.  We should
 *       capture free variables from the environment where the macro was
 *       DEFINED, and reuse that... Currently, we don't do that.
 *
 */
static bool syntax_match(cons_t *pat, cons_t *c, dict_t& map)
{
  bool variadic = has_rest_args(pat);
  bool prev_dot = false;

  while ( !nullp(pat) ) {
    cons_t *l = car(pat); // left; pattern
    cons_t *r = car(c); // right; code

    // Match REST of symbols (TODO: Fix this, it's not very correct)
    if ( symbol_name(l) == "..." ) {
      map["..."] = nullp(c) ? nil() : c;
      return true;
    }

    // Variadic function; collect rest of arguments
    if ( variadic && prev_dot ) {
      // give it a default value if not set
      map[symbol_name(l)] = nullp(c) ? list(NULL) : c;
      return true;
    }

    // detect dot in (arg1 arg2 . argN)
    prev_dot = (symbol_name(l) == ".") ? true : false;

    if ( prev_dot ) {
      /*
       * Skip dot symbol in pattern.
       *
       * This is a special case, which I don't think I'm
       * handling correctly (elsewhere in the system).
       *
       * If we have to variadic macros with one calling
       * the other with the variadic function, we get
       * the extra dot in the input arguments, so normally
       * input args would be "(a b c)" but here it will be
       * "(a b . c)".
       *
       * TODO: I guess the CORRECT way of handling this is
       *       to expand a function call "(foo arg1 arg2 . etc)"
       *       into a dot-free argument list.  Find out.
       *
       * For now, just skip ahead on c as well.
       */
      if ( symbol_name(r) == "." )
        c = cdr(c);

      pat = cdr(pat);
      continue;
    }

    if ( symbol_name(l) == "_" ) {
      // "_" = match anything, so skip both pattern and args
      c = cdr(c);
      pat = cdr(pat);
      continue;
    }

    // Pattern is too long; no match
    if ( nullp(c) )
      return false;

    // store in match symbol table
    map[symbol_name(l)] = r;
    c = cdr(c);
    pat = cdr(pat);
  }

  // Check that pattern was fully matched
  return nullp(pat);
}

static cons_t* syntax_replace(dict_t &map, cons_t* p)
{
  cons_t *start = p;

  while ( !nullp(p) ) {
    if ( listp(car(p)) )
      p->car = syntax_replace(map, car(p));
    else if ( symbolp(car(p)) && map.count(car(p)->symbol->name()) ) {
      const std::string& name = car(p)->symbol->name();
      cons_t *repl = map[name];

      /*
       * If "...", then splice into p, so that
       * "a b c" does not expand into "(a b c)".
       */
      if ( name != "..." )
        p->car = repl;
      else {
        if ( repl == NULL ) {
          p->car = nil();
          continue;
        }
        else {
          p->car = repl->car;
          p->cdr = repl->cdr;
        }

        /*
         * We can't do `p = cdr(p)` after we've
         * replaced p->cdr, so we just stop here
         * after expanding "...".  This is not an
         * entirely correct implementation of
         * macros, but good enough for now.
         */
        break;
      }
    }

    p = cdr(p);
  }

  return start;
}

cons_t* syntax_expand(cons_t *macro, cons_t *code, environment_t*)
{
  cons_t *rules = macro->syntax->transformer;

  // Go through all rules and find a match
  for ( cons_t *p = cdar(rules); !nullp(p); p = cdr(p) ) {
    cons_t *pattern = caar(p);
    cons_t *expansion = cadar(p);

    dict_t map;

    // mark "..." as '() for cases where it's empty, like `(when #t)`
    map["..."] = NULL;

    if ( syntax_match(pattern, code, map) )
      return syntax_replace(map, deep_copy(expansion));
  }

  /*
   * No match.  This is an error.
   */
  raise(std::runtime_error(
    "Macro invocation did not match any patterns: "
      + sprint(code)));
  return nil();
}

cons_t* make_syntax(cons_t* body, environment_t* e)
{
  syntax_t *s = new syntax_t();
  s->transformer = body;
  s->environment = e;

  cons_t *r = new cons_t();
  r->type = SYNTAX;
  r->syntax = s;
  return r;
}

} // extern "C"
