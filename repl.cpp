#include <stdio.h>
#include "cons.h"
#include "util.h"
#include "repl.h"
#include "tests.h"
#include "parser.h"
#include "print.h"

int repl(cons_t* (*eval)(cons_t* p))
{
  printf("Type :QUIT to quit\n");
  printf("Type :TEST to run tests\n");

  int no=0;
  char buf[1024];

  for(;;) {
    buf[0] = '\0';

    printf("%d> ", no++);
    fflush(stdout);

    if ( fgets(buf, sizeof(buf)-1, stdin) == NULL ) {
      printf("\n");
      break;
    }

    trimr(buf);

    if ( buf[0] == '\0' )
      continue;

    if ( toupper(buf) == ":QUIT" ) break;
    if ( toupper(buf) == ":TEST" ) run_tests();

    try {
      if ( eval == NULL )
        printf("%s\n", sprint(parse(buf)).c_str());
      else {
        std::string s = print(eval(parse(buf)->root));
        if ( !s.empty() )
          printf("%s\n", s.c_str());
      }
    }
    catch(const std::exception& e) {
      fprintf(stderr, "%s\n", e.what());
    }
  }

  return 0;
}
