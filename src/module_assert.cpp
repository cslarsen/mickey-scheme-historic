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

#include <math.h>
#include "module.h"
#include "module_assert.h"
#include "primops.h"
#include "assertions.h"
#include "exceptions.h"
#include "types.h"
#include "print.h"

/*
 *    (assert-length <length> <pair>)
 * OR (assert-length <min> <max> <pair>)
 */
cons_t* proc_assert_length(cons_t* p, environment_t*)
{
  assert_length(p, 2, 3);

  // (assert-length <length> <pair>)
  if ( length(p) == 2 ) {
    assert_type(INTEGER, car(p));
    assert_type(PAIR, cadr(p));

    const size_t e = car(p)->integer;
    const size_t l = length(const_cast<cons_t*>(cadr(p)));

    if ( e != l )
      raise(std::runtime_error(format(
        "assert-length expected list of length %lu to be %lu",
          l, e)));

    return boolean(true);
  }

  // (assert-length <min> <max> <pair>)
  assert_type(INTEGER, car(p));
  assert_type(INTEGER, cadr(p));
  assert_type(PAIR, caddr(p));

  const size_t min = car(p)->integer;
  const size_t max = cadr(p)->integer;
  const size_t l = length(const_cast<cons_t*>(caddr(p)));

  if ( l<min || l>max )
    raise(std::runtime_error(format(
      "assert-length expected list of length %lu to be between %lu and %lu",
        l, min, max)));

  return boolean(true);
}

// (assert-type <type-as-symbol> <expression>)
cons_t* proc_assert_type(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(SYMBOL, car(p));

  const enum type_t expected = to_type_t(car(p)->symbol->name().c_str());
  const enum type_t actual   = type_of(cadr(p));

  if ( expected != actual )
    raise(std::runtime_error(format(
      "assert-type expected type %s but got %s for: %s",
        to_s(expected).c_str(),
        to_s(actual).c_str(),
        sprint(cadr(p)).c_str())));

  return boolean(true);
}

// (assert-number <expression>)
cons_t* proc_assert_number(cons_t* p, environment_t*)
{
  assert_length(p, 1);

  if ( !numberp(car(p)) )
    raise(std::runtime_error(format(
      "assert-number expected a number for: %s",
        sprint(car(p)).c_str())));

  return boolean(true);
}

// (assert-length-min <length> <pair>)
cons_t* proc_assert_length_min(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(INTEGER, car(p));
  assert_type(PAIR, cadr(p));

  const size_t min = car(p)->integer;
  const size_t l = length(const_cast<cons_t*>(cadr(p)));

  if ( l<min )
    raise(std::runtime_error(format(
      "assert-length-min expected list of length %lu to be minimum %lu",
        l, min)));
  
  return boolean(true);
}

named_function_t exports_assert[] = {
  {"assert-length",proc_assert_length},
  {"assert-length-min", proc_assert_length_min},
  {"assert-type", proc_assert_type},
  {"assert-number", proc_assert_number},
  {NULL, NULL}
};
