/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include <stdio.h>
#include "test.h"

static int tests=0, good=0;

void reset_tests()
{
  tests = good = 0;
}

static void ok(const char* s)
{
  printf("%d/%d OK:   %s\n", ++good, ++tests, s);
}

static void fail(const char* s)
{
  printf("%d/%d FAIL: %s\n", good, ++tests, s);
}

void test(bool result, const char* s)
{
  (result ? ok : fail)(s) ;
}

void results()
{
  printf("\n");

  printf("%d/%d (%.2f%%) tests OK\n",
    good, tests, 100.0f*good/tests);

  printf("%d/%d (%.2f%%) tests FAILED\n",
    tests-good, tests, 100.0f*(tests-good)/tests);
}

void test_streq(const std::string& code, const std::string& actual, const std::string& expected)
{
  test(actual == expected, (code + " == \"" + expected + "\"").c_str());

  if ( actual != expected ) {
    printf("  expected: `%s`\n", expected.c_str());
    printf("  actual  : `%s`\n", actual.c_str());
  }
}
