#include <stdio.h>
#include "test.h"

static int tests=0, good=0;

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
